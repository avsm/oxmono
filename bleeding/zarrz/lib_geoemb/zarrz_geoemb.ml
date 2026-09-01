(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unknown members are collected in document order. [Jsont.json_mems]
   would fold them into a JSON object, which loses the distinction
   between a member list and a value. *)
let mem_list_mems =
  let dec_empty () = [] in
  let dec_add meta name v mems = ((name, meta), v) :: mems in
  let dec_finish _meta mems = List.rev mems in
  let enc f l acc = List.fold_left (fun acc ((n, m), v) -> f m n v acc) acc l in
  Jsont.Object.Mems.map ~kind:"unknown" ~dec_empty ~dec_add ~dec_finish
    ~enc:{ Jsont.Object.Mems.enc } Jsont.json

let mems_equal (a : Jsont.mem list) b =
  List.equal
    (fun ((n0, _), v0) ((n1, _), v1) ->
      String.equal n0 n1 && Jsont.Json.equal v0 v1)
    a b

let pp_of_jsont t ppf v =
  match Jsont.Json.encode t v with
  | Ok j -> Jsont.Json.pp ppf j
  | Error m -> Format.fprintf ppf "<unencodable: %s>" m

(* The schema states its lower bounds on decoded values only. Encoding
   a value built by hand does not recheck them. *)

let min_int_jsont ~kind ~(min : int) =
  Jsont.map ~kind
    ~dec:(fun v ->
      if v >= min then v
      else
        Jsont.Error.msgf Jsont.Meta.none "expected %s >= %d, found %d" kind min
          v)
    ~enc:Fun.id Jsont.int

let positive_number_jsont ~kind =
  Jsont.map ~kind
    ~dec:(fun v ->
      if v > 0. then v
      else Jsont.Error.msgf Jsont.Meta.none "expected %s > 0, found %g" kind v)
    ~enc:Fun.id Jsont.number

let non_empty_list_jsont ~kind t =
  Jsont.map ~kind
    ~dec:(function
      | [] -> Jsont.Error.msgf Jsont.Meta.none "expected at least one %s" kind
      | l -> l)
    ~enc:Fun.id (Jsont.list t)

module Convention = struct
  type t = {
    uuid : string option;
    name : string option;
    description : string option;
    spec_url : string option;
    schema_url : string option;
    unknown : Jsont.mem list;
  }

  let geoemb_uuid = "61c12cc5-0e28-4056-999a-480cf3fb7e4c"
  let geoemb_name = "geoemb:"

  let geoemb_description =
    "Geoembeddings convention for geospatial embedding arrays with model \
     provenance"

  let geoemb_spec_url =
    "https://github.com/geo-embeddings/embeddings-zarr-convention/blob/v1/\
     README.md"

  let geoemb_schema_url =
    "https://raw.githubusercontent.com/geo-embeddings/embeddings-zarr-\
     convention/refs/tags/v1/schema.json"

  let v ?uuid ?name ?description ?spec_url ?schema_url () =
    { uuid; name; description; spec_url; schema_url; unknown = [] }

  let geoemb =
    v ~uuid:geoemb_uuid ~name:geoemb_name ~description:geoemb_description
      ~spec_url:geoemb_spec_url ~schema_url:geoemb_schema_url ()

  let is_geoemb c =
    match c.uuid with
    | Some u -> String.equal u geoemb_uuid
    | None -> (
        match c.name with
        | Some n -> String.equal n geoemb_name
        | None -> false)

  let find ~uuid l =
    List.find_opt
      (fun c ->
        match c.uuid with Some u -> String.equal u uuid | None -> false)
      l

  let jsont =
    Jsont.Object.map ~kind:"conventionMetadata"
      (fun uuid name description spec_url schema_url unknown ->
        { uuid; name; description; spec_url; schema_url; unknown })
    |> Jsont.Object.opt_mem "uuid" Jsont.string ~enc:(fun c -> c.uuid)
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun c -> c.name)
    |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun c ->
           c.description)
    |> Jsont.Object.opt_mem "spec_url" Jsont.string ~enc:(fun c -> c.spec_url)
    |> Jsont.Object.opt_mem "schema_url" Jsont.string ~enc:(fun c ->
           c.schema_url)
    |> Jsont.Object.keep_unknown mem_list_mems ~enc:(fun c -> c.unknown)
    |> Jsont.Object.finish

  let equal a b =
    a.uuid = b.uuid && a.name = b.name && a.description = b.description
    && a.spec_url = b.spec_url
    && a.schema_url = b.schema_url
    && mems_equal a.unknown b.unknown

  let pp = pp_of_jsont jsont
end

module Chip_layout = struct
  type layout = Regular_grid | Irregular

  type t = {
    layout_type : layout;
    chip_size : int * int;
    stride : (int * int) option;
    grid_id : string option;
    grid_definition : string option;
    unknown : Jsont.mem list;
  }

  let v ?stride ?grid_id ?grid_definition ~layout_type ~chip_size () =
    { layout_type; chip_size; stride; grid_id; grid_definition; unknown = [] }

  let effective_stride c =
    match c.stride with Some s -> s | None -> c.chip_size

  let layout_jsont =
    Jsont.enum ~kind:"layout_type"
      [ ("regular_grid", Regular_grid); ("irregular", Irregular) ]

  (* Both member arrays are pinned to two positive integers by the
     schema, so a pair is exact. *)
  let pair_jsont ~kind =
    Jsont.t2 ~kind
      ~dec:(fun a b -> (a, b))
      ~enc:(fun (a, b) i -> if i = 0 then a else b)
      (min_int_jsont ~kind:(kind ^ " element") ~min:1)

  let jsont =
    Jsont.Object.map ~kind:"chipLayoutObject"
      (fun layout_type chip_size stride grid_id grid_definition unknown ->
        { layout_type; chip_size; stride; grid_id; grid_definition; unknown })
    |> Jsont.Object.mem "layout_type" layout_jsont ~enc:(fun c -> c.layout_type)
    |> Jsont.Object.mem "chip_size"
         (pair_jsont ~kind:"chip_size")
         ~enc:(fun c -> c.chip_size)
    |> Jsont.Object.opt_mem "stride"
         (pair_jsont ~kind:"stride")
         ~enc:(fun c -> c.stride)
    |> Jsont.Object.opt_mem "grid_id" Jsont.string ~enc:(fun c -> c.grid_id)
    |> Jsont.Object.opt_mem "grid_definition" Jsont.string ~enc:(fun c ->
           c.grid_definition)
    |> Jsont.Object.keep_unknown mem_list_mems ~enc:(fun c -> c.unknown)
    |> Jsont.Object.finish

  let equal a b =
    a.layout_type = b.layout_type
    && a.chip_size = b.chip_size
    && a.stride = b.stride && a.grid_id = b.grid_id
    && a.grid_definition = b.grid_definition
    && mems_equal a.unknown b.unknown

  let pp = pp_of_jsont jsont
end

module Quantization = struct
  module Scale = struct
    type nodata = Number of float | String of string

    type scalar = { scale : float; offset : float; unknown : Jsont.mem list }

    type array_ref = {
      array_name : string;
      nodata : nodata option;
      unknown : Jsont.mem list;
    }

    type t = Scalar of scalar | Array of array_ref

    let scalar ?(offset = 0.) scale = Scalar { scale; offset; unknown = [] }

    let array_ref ?nodata array_name =
      Array { array_name; nodata; unknown = [] }

    (* [Jsont.any] picks the branch from the constructor on encode, so
       the mismatched arm of each map is unreachable. *)
    let nodata_number_jsont =
      Jsont.map ~kind:"nodata"
        ~dec:(fun v -> Number v)
        ~enc:(function
          | Number v -> v
          | String _ ->
              Jsont.Error.msg Jsont.Meta.none "nodata is not a number")
        Jsont.number

    let nodata_string_jsont =
      Jsont.map ~kind:"nodata"
        ~dec:(fun s -> String s)
        ~enc:(function
          | String s -> s
          | Number _ ->
              Jsont.Error.msg Jsont.Meta.none "nodata is not a string")
        Jsont.string

    let nodata_jsont =
      Jsont.any ~kind:"nodata" ~dec_number:nodata_number_jsont
        ~dec_string:nodata_string_jsont
        ~enc:(function
          | Number _ -> nodata_number_jsont
          | String _ -> nodata_string_jsont)
        ()

    let scalar_jsont =
      Jsont.Object.map ~kind:"scalar scale" (fun scale offset unknown ->
          { scale; offset; unknown })
      |> Jsont.Object.mem "scale" Jsont.number ~enc:(fun s -> s.scale)
      |> Jsont.Object.mem "offset" Jsont.number ~dec_absent:(fun () -> 0.)
           ~enc:(fun s -> s.offset)
      |> Jsont.Object.keep_unknown mem_list_mems
           ~enc:(fun (s : scalar) -> s.unknown)
      |> Jsont.Object.finish

    let array_jsont =
      Jsont.Object.map ~kind:"array scale" (fun array_name nodata unknown ->
          { array_name; nodata; unknown })
      |> Jsont.Object.mem "array_name" Jsont.string ~enc:(fun s -> s.array_name)
      |> Jsont.Object.opt_mem "nodata" nodata_jsont ~enc:(fun s -> s.nodata)
      |> Jsont.Object.keep_unknown mem_list_mems ~enc:(fun s -> s.unknown)
      |> Jsont.Object.finish

    let jsont =
      let scalar =
        Jsont.Object.Case.map "scalar" scalar_jsont ~dec:(fun s -> Scalar s)
      in
      let array =
        Jsont.Object.Case.map "array" array_jsont ~dec:(fun a -> Array a)
      in
      let enc_case = function
        | Scalar s -> Jsont.Object.Case.value scalar s
        | Array a -> Jsont.Object.Case.value array a
      in
      let cases = Jsont.Object.Case.[ make scalar; make array ] in
      Jsont.Object.map ~kind:"scaleObject" Fun.id
      |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
      |> Jsont.Object.finish

    let equal a b =
      match (a, b) with
      | Scalar x, Scalar y ->
          Float.equal x.scale y.scale
          && Float.equal x.offset y.offset
          && mems_equal x.unknown y.unknown
      | Array x, Array y ->
          String.equal x.array_name y.array_name
          && x.nodata = y.nodata
          && mems_equal x.unknown y.unknown
      | Scalar _, Array _ | Array _, Scalar _ -> false

    let pp = pp_of_jsont jsont
  end

  type t = {
    method_ : string;
    original_dtype : string;
    quantized_dtype : string option;
    scale : Scale.t option;
    link : string option;
    unknown : Jsont.mem list;
  }

  let v ?quantized_dtype ?scale ?link ~method_ ~original_dtype () =
    { method_; original_dtype; quantized_dtype; scale; link; unknown = [] }

  let jsont =
    Jsont.Object.map ~kind:"quantizationObject"
      (fun method_ original_dtype quantized_dtype scale link unknown ->
        { method_; original_dtype; quantized_dtype; scale; link; unknown })
    |> Jsont.Object.mem "method" Jsont.string ~enc:(fun q -> q.method_)
    |> Jsont.Object.mem "original_dtype" Jsont.string ~enc:(fun q ->
           q.original_dtype)
    |> Jsont.Object.opt_mem "quantized_dtype" Jsont.string ~enc:(fun q ->
           q.quantized_dtype)
    |> Jsont.Object.opt_mem "scale" Scale.jsont ~enc:(fun q -> q.scale)
    |> Jsont.Object.opt_mem "link" Jsont.string ~enc:(fun q -> q.link)
    |> Jsont.Object.keep_unknown mem_list_mems ~enc:(fun q -> q.unknown)
    |> Jsont.Object.finish

  let equal a b =
    String.equal a.method_ b.method_
    && String.equal a.original_dtype b.original_dtype
    && a.quantized_dtype = b.quantized_dtype
    && Option.equal Scale.equal a.scale b.scale
    && a.link = b.link
    && mems_equal a.unknown b.unknown

  let pp = pp_of_jsont jsont
end

type kind = Pixel | Chip
type spatial_layout = Utm_zones | Global

type t = {
  conventions : Convention.t list;
  kind : kind;
  dimensions : int;
  model : string;
  source_data : string list;
  data_type : string;
  gsd : float option;
  chip_layout : Chip_layout.t option;
  quantization : Quantization.t option;
  spatial_layout : spatial_layout option;
  build_version : string option;
  benchmark : string list option;
  unknown : Jsont.mem list;
}

let kind_jsont =
  Jsont.enum ~kind:"geoemb:type" [ ("pixel", Pixel); ("chip", Chip) ]

let spatial_layout_jsont =
  Jsont.enum ~kind:"geoemb:spatial_layout"
    [ ("utm_zones", Utm_zones); ("global", Global) ]

(* The schema's [contains] on zarr_conventions. Without it this codec
   would accept the attributes of any other convention that happens to
   carry the geoemb members. *)
let check_registered conventions =
  if List.exists Convention.is_geoemb conventions then conventions
  else
    Jsont.Error.msgf Jsont.Meta.none
      "zarr_conventions has no entry for the geoemb convention (uuid %s or \
       name %S)"
      Convention.geoemb_uuid Convention.geoemb_name

(* The schema's if/then: a chip store must say how it was chipped. *)
let check_chip_layout kind chip_layout =
  match (kind, chip_layout) with
  | Chip, None ->
      Jsont.Error.msg Jsont.Meta.none
        "geoemb:chip_layout is required when geoemb:type is \"chip\""
  | (Chip | Pixel), _ -> chip_layout

let jsont =
  Jsont.Object.map ~kind:"geoemb attributes"
    (fun conventions kind dimensions model source_data data_type gsd chip_layout
         quantization spatial_layout build_version benchmark unknown ->
      {
        conventions = check_registered conventions;
        kind;
        dimensions;
        model;
        source_data;
        data_type;
        gsd;
        chip_layout = check_chip_layout kind chip_layout;
        quantization;
        spatial_layout;
        build_version;
        benchmark;
        unknown;
      })
  |> Jsont.Object.mem "zarr_conventions"
       (Jsont.list Convention.jsont)
       ~enc:(fun t -> t.conventions)
  |> Jsont.Object.mem "geoemb:type" kind_jsont ~enc:(fun t -> t.kind)
  |> Jsont.Object.mem "geoemb:dimensions"
       (min_int_jsont ~kind:"geoemb:dimensions" ~min:1)
       ~enc:(fun t -> t.dimensions)
  |> Jsont.Object.mem "geoemb:model" Jsont.string ~enc:(fun t -> t.model)
  |> Jsont.Object.mem "geoemb:source_data"
       (non_empty_list_jsont ~kind:"geoemb:source_data" Jsont.string)
       ~enc:(fun t -> t.source_data)
  |> Jsont.Object.mem "geoemb:data_type" Jsont.string ~enc:(fun t ->
         t.data_type)
  |> Jsont.Object.opt_mem "geoemb:gsd"
       (positive_number_jsont ~kind:"geoemb:gsd")
       ~enc:(fun t -> t.gsd)
  |> Jsont.Object.opt_mem "geoemb:chip_layout" Chip_layout.jsont ~enc:(fun t ->
         t.chip_layout)
  |> Jsont.Object.opt_mem "geoemb:quantization" Quantization.jsont
       ~enc:(fun t -> t.quantization)
  |> Jsont.Object.opt_mem "geoemb:spatial_layout" spatial_layout_jsont
       ~enc:(fun t -> t.spatial_layout)
  |> Jsont.Object.opt_mem "geoemb:build_version" Jsont.string ~enc:(fun t ->
         t.build_version)
  |> Jsont.Object.opt_mem "geoemb:benchmark" (Jsont.list Jsont.string)
       ~enc:(fun t -> t.benchmark)
  |> Jsont.Object.keep_unknown mem_list_mems ~enc:(fun t -> t.unknown)
  |> Jsont.Object.finish

let of_json j = Jsont.Json.decode jsont j

let to_json t =
  match Jsont.Json.encode jsont t with
  | Ok j -> j
  | Error m -> invalid_arg ("Zarrz_geoemb.to_json: " ^ m)

let equal a b =
  List.equal Convention.equal a.conventions b.conventions
  && a.kind = b.kind
  && a.dimensions = b.dimensions
  && String.equal a.model b.model
  && List.equal String.equal a.source_data b.source_data
  && String.equal a.data_type b.data_type
  && Option.equal Float.equal a.gsd b.gsd
  && Option.equal Chip_layout.equal a.chip_layout b.chip_layout
  && Option.equal Quantization.equal a.quantization b.quantization
  && a.spatial_layout = b.spatial_layout
  && a.build_version = b.build_version
  && Option.equal (List.equal String.equal) a.benchmark b.benchmark
  && mems_equal a.unknown b.unknown

let pp = pp_of_jsont jsont

(* A probe over generic JSON. It must not decode the whole object: a
   caller uses it to skip attributes that belong to other conventions. *)
let string_mem n mems =
  match Jsont.Json.find_mem n mems with
  | Some (_, Jsont.String (s, _)) -> Some s
  | Some _ | None -> None

let is_geoemb_entry j =
  match j with
  | Jsont.Object (mems, _) -> (
      match string_mem "uuid" mems with
      | Some u -> String.equal u Convention.geoemb_uuid
      | None -> (
          match string_mem "name" mems with
          | Some n -> String.equal n Convention.geoemb_name
          | None -> false))
  | _ -> false

let is_geoemb_attributes j =
  match j with
  | Jsont.Object (mems, _) -> (
      match Jsont.Json.find_mem "zarr_conventions" mems with
      | Some (_, Jsont.Array (l, _)) -> List.exists is_geoemb_entry l
      | Some _ | None -> false)
  | _ -> false

let of_attributes j =
  if is_geoemb_attributes j then Some (of_json j) else None
