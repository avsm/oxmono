(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type array_meta = {
  shape : int array;
  data_type : Ext.t;
  chunk_grid : Ext.t;
  chunk_key_encoding : Ext.t;
  fill_value : Jsont.json;
  codecs : Ext.t list;
  attributes : Jsont.json option;
  dimension_names : string option list option;
  storage_transformers : Ext.t list;
  unknown : Jsont.mem list;
}

type group_meta = {
  group_attributes : Jsont.json option;
  group_unknown : Jsont.mem list;
}

let zarr_format_jsont =
  Jsont.map ~kind:"zarr_format"
    ~dec:(fun v ->
      if v = 3 then ()
      else
        Jsont.Error.msgf Jsont.Meta.none "expected zarr_format 3, found %d" v)
    ~enc:(fun () -> 3)
    Jsont.int

let node_type_jsont expect =
  Jsont.map ~kind:"node_type"
    ~dec:(fun v ->
      if String.equal v expect then ()
      else
        Jsont.Error.msgf Jsont.Meta.none "expected node_type %S, found %S"
          expect v)
    ~enc:(fun () -> expect)
    Jsont.string

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

(* zarr-python <= 3.1.3 writes a null "consolidated_metadata" member into
   group metadata. It carries no information, so drop it rather than
   fail the must_understand check below. *)
let drop_consolidated_null mems =
  List.filter
    (fun ((n, _), v) ->
      match v with
      | Jsont.Null _ -> not (String.equal n "consolidated_metadata")
      | _ -> true)
    mems

(* A non-object member, or an object without an explicit
   "must_understand": false, must be understood. *)
let must_understand j =
  match j with
  | Jsont.Object (mems, _) -> (
      match Jsont.Json.find_mem "must_understand" mems with
      | Some (_, Jsont.Bool (b, _)) -> b
      | Some _ | None -> true)
  | _ -> true

let check_unknown mems =
  let mems = drop_consolidated_null mems in
  List.iter
    (fun ((n, meta), v) ->
      if must_understand v then
        Jsont.Error.msgf meta
          "unknown member %S must be understood but is not supported" n)
    mems;
  mems

let attributes_mem ~enc map =
  Jsont.Object.mem "attributes" (Jsont.option Jsont.json_object)
    ~dec_absent:None ~enc ~enc_omit:Option.is_none map

let array_jsont =
  Jsont.Object.map ~kind:"ArrayMetadata"
    (fun () () shape data_type chunk_grid chunk_key_encoding fill_value codecs
         attributes storage_transformers dimension_names unknown ->
      {
        shape;
        data_type;
        chunk_grid;
        chunk_key_encoding;
        fill_value;
        codecs;
        attributes;
        dimension_names;
        storage_transformers;
        unknown = check_unknown unknown;
      })
  |> Jsont.Object.mem "zarr_format" zarr_format_jsont ~enc:(fun _ -> ())
  |> Jsont.Object.mem "node_type" (node_type_jsont "array") ~enc:(fun _ -> ())
  |> Jsont.Object.mem "shape" (Jsont.array Jsont.int) ~enc:(fun m -> m.shape)
  |> Jsont.Object.mem "data_type" Ext.jsont ~enc:(fun m -> m.data_type)
  |> Jsont.Object.mem "chunk_grid" Ext.jsont ~enc:(fun m -> m.chunk_grid)
  |> Jsont.Object.mem "chunk_key_encoding" Ext.jsont ~enc:(fun m ->
         m.chunk_key_encoding)
  |> Jsont.Object.mem "fill_value" Jsont.json ~enc:(fun m -> m.fill_value)
  |> Jsont.Object.mem "codecs" (Jsont.list Ext.jsont) ~enc:(fun m -> m.codecs)
  |> attributes_mem ~enc:(fun m -> m.attributes)
  |> Jsont.Object.mem "storage_transformers" (Jsont.list Ext.jsont)
       ~dec_absent:[]
       ~enc:(fun m -> m.storage_transformers)
       ~enc_omit:(function [] -> true | _ -> false)
  |> Jsont.Object.mem "dimension_names"
       (Jsont.option (Jsont.list (Jsont.option Jsont.string)))
       ~dec_absent:None
       ~enc:(fun m -> m.dimension_names)
       ~enc_omit:Option.is_none
  |> Jsont.Object.keep_unknown mem_list_mems ~enc:(fun m -> m.unknown)
  |> Jsont.Object.finish

let group_jsont =
  Jsont.Object.map ~kind:"GroupMetadata"
    (fun () () group_attributes group_unknown ->
      { group_attributes; group_unknown = check_unknown group_unknown })
  |> Jsont.Object.mem "zarr_format" zarr_format_jsont ~enc:(fun _ -> ())
  |> Jsont.Object.mem "node_type" (node_type_jsont "group") ~enc:(fun _ -> ())
  |> attributes_mem ~enc:(fun m -> m.group_attributes)
  |> Jsont.Object.keep_unknown mem_list_mems ~enc:(fun m -> m.group_unknown)
  |> Jsont.Object.finish

let array_of_json j = Jsont.Json.decode array_jsont j
let group_of_json j = Jsont.Json.decode group_jsont j

let encode t v =
  match Jsont.Json.encode t v with
  | Ok j -> j
  | Error m -> Error.raise_ (Error.Metadata m)

let array_to_json m = encode array_jsont m
let group_to_json m = encode group_jsont m
