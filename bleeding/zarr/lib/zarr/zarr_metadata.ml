(** {1 Zarr v3 Metadata}

    Parsing and serialisation of [zarr.json] metadata documents
    for arrays and groups. See Zarr v3.1 spec, "Metadata" section. *)

(** Array metadata record. *)
type array_metadata = {
  zarr_format : int;
  shape : int array;
  data_type : Zarr_dtype.t;
  chunk_grid : Chunk_grid.t;
  chunk_key_encoding : Chunk_key.encoding;
  fill_value : Zarr_fill.t;
  codecs : Zarr_codec.codec_spec list;
  dimension_names : string option array option;
  attributes : Jsont.json option;
}

(** Group metadata record. *)
type group_metadata = {
  zarr_format : int;
  attributes : Jsont.json option;
}

(** {2 Validation} *)

(** [validate_node_name name] checks Zarr v3 node naming rules.
    @raise Invalid_argument if the name violates naming rules. *)
let validate_node_name name =
  if name = "" then invalid_arg "node name must not be empty"
  else if String.contains name '/' then invalid_arg "node name must not contain '/'"
  else if name = "." || name = ".." then invalid_arg "node name must not be '.' or '..'"
  else if String.length name >= 2 && name.[0] = '_' && name.[1] = '_' then
    invalid_arg "node name must not start with '__'"

(** {2 Array metadata} *)

let array_of_json json_str =
  let json = Json_util.of_string json_str in
  let zarr_format = Json_util.(member "zarr_format" json |> to_int_exn) in
  if zarr_format <> 3 then
    failwith ("zarr_format must be 3, got " ^ string_of_int zarr_format);
  let node_type = Json_util.(member "node_type" json |> to_string_exn) in
  if node_type <> "array" then failwith "node_type must be 'array'";
  let shape = Json_util.(member "shape" json |> to_list_exn)
              |> List.map Json_util.to_int_exn |> Array.of_list in
  let dtype_str = Json_util.(member "data_type" json |> to_string_exn) in
  let data_type = match Zarr_dtype.of_string dtype_str with
    | Some d -> d | None -> failwith ("unsupported dtype: " ^ dtype_str) in
  let chunk_grid = Chunk_grid.of_json (Json_util.member "chunk_grid" json) in
  let chunk_key_encoding = Chunk_key.of_json (Json_util.member "chunk_key_encoding" json) in
  let fill_value = Zarr_fill.of_json data_type (Json_util.member "fill_value" json) in
  let codecs = Zarr_codec.specs_of_json (Json_util.(member "codecs" json |> to_list_exn)) in
  let has_a2b = List.exists (function
    | Zarr_codec.Bytes _ | Sharding _ | Extension _ -> true | _ -> false
  ) codecs in
  if not has_a2b then failwith "codecs must contain an array->bytes codec";
  let dim_json = Json_util.member "dimension_names" json in
  let dimension_names = match dim_json with
    | Jsont.Null _ -> None
    | Jsont.Array (names, _) ->
      Some (Array.of_list (List.map (function
        | Jsont.String (s, _) -> Some s | _ -> None) names))
    | _ -> None in
  let attr_json = Json_util.member "attributes" json in
  let attributes = if Json_util.is_null attr_json then None else Some attr_json in
  { zarr_format; shape; data_type; chunk_grid; chunk_key_encoding;
    fill_value; codecs; dimension_names; attributes }

let array_to_json m =
  let dim_names = match m.dimension_names with
    | None -> Json_util.null
    | Some names -> Json_util.arr (Array.to_list (Array.map (function
        | None -> Json_util.null | Some s -> Json_util.str s) names)) in
  Json_util.(obj [
    mem "zarr_format" (int m.zarr_format);
    mem "node_type" (str "array");
    mem "shape" (arr (Array.to_list (Array.map (fun i -> int i) m.shape)));
    mem "data_type" (str (Zarr_dtype.to_string m.data_type));
    mem "chunk_grid" (Chunk_grid.to_json m.chunk_grid);
    mem "chunk_key_encoding" (Chunk_key.to_json m.chunk_key_encoding);
    mem "fill_value" (Zarr_fill.to_json m.fill_value);
    mem "codecs" (arr (Zarr_codec.specs_to_json m.codecs));
    mem "dimension_names" dim_names;
    mem "attributes" (Option.value ~default:null m.attributes);
  ]) |> Json_util.to_string_pretty

let create_array_metadata ~shape ~chunks ~dtype
    ?(fill_value = Zarr_fill.default dtype)
    ?(codecs = [Zarr_codec.Bytes { endian = Some Little }])
    ?dimension_names ?attributes () =
  { zarr_format = 3; shape; data_type = dtype;
    chunk_grid = Regular { chunk_shape = chunks };
    chunk_key_encoding = Default { separator = Slash };
    fill_value; codecs; dimension_names; attributes }

(** {2 Group metadata} *)

let group_of_json json_str =
  let json = Json_util.of_string json_str in
  let zf = Json_util.(member "zarr_format" json |> to_int_exn) in
  if zf <> 3 then failwith ("zarr_format must be 3, got " ^ string_of_int zf);
  let nt = Json_util.(member "node_type" json |> to_string_exn) in
  if nt <> "group" then failwith "node_type must be 'group'";
  let attr_json = Json_util.member "attributes" json in
  let attributes = if Json_util.is_null attr_json then None else Some attr_json in
  { zarr_format = zf; attributes }

let group_to_json m =
  Json_util.(obj [mem "zarr_format" (int m.zarr_format);
          mem "node_type" (str "group");
          mem "attributes" (Option.value ~default:null m.attributes)])
  |> Json_util.to_string_pretty

let create_group_metadata ?attributes () =
  { zarr_format = 3; attributes }
