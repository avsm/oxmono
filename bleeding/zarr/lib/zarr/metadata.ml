(** Metadata parsing and serialization for Zarr v3 *)

(* Create alias for fill_value module before opening Ztypes *)
module FV_mod = Fill_value

open Ztypes

module E = Ztypes.Endianness
module S = Ztypes.Separator

(** Parse array metadata from JSON string *)
let array_of_json json_str =
  let ( let* ) = Result.bind in
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in

    (* zarr_format must be 3 *)
    let zarr_format = json |> member "zarr_format" |> to_int in
    let* () =
      if zarr_format <> 3 then
        Error (`Invalid_metadata (Printf.sprintf "zarr_format must be 3, got %d" zarr_format))
      else Ok ()
    in

    (* node_type must be "array" *)
    let node_type_str = json |> member "node_type" |> to_string in
    let* () =
      if node_type_str <> "array" then
        Error (`Invalid_metadata "node_type must be 'array' for array metadata")
      else Ok ()
    in

    (* shape *)
    let shape = json |> member "shape" |> to_list |> List.map to_int |> Array.of_list in

    (* data_type *)
    let dtype_str = json |> member "data_type" |> to_string in
    let* data_type = Data_type.of_string dtype_str in

    (* chunk_grid *)
    let* chunk_grid = Chunk_grid.of_json (json |> member "chunk_grid") in

    (* chunk_key_encoding *)
    let* chunk_key_encoding = Chunk_key.of_json (json |> member "chunk_key_encoding") in

    (* fill_value *)
    let* fill_value = FV_mod.of_json data_type (json |> member "fill_value") in

    (* codecs *)
    let* codecs = Codec.specs_of_json (json |> member "codecs" |> to_list) in

    (* Verify codec chain has array->bytes (Extension codecs are tolerated
       since we can't know their class at parse time) *)
    let has_array_to_bytes = List.exists (function
      | Bytes _ | Sharding _ | Extension _ -> true
      | _ -> false
    ) codecs in
    let* () =
      if not has_array_to_bytes then
        Error (`Invalid_metadata "codecs must contain an array->bytes codec (bytes or sharding_indexed)")
      else Ok ()
    in

    (* dimension_names (optional) *)
    let dimension_names =
      match json |> member "dimension_names" with
      | `Null -> None
      | `List names ->
        Some (Array.of_list (List.map (function
          | `Null -> None
          | `String s -> Some s
          | _ -> None
        ) names))
      | _ -> None
    in

    (* attributes (optional) *)
    let attributes =
      match json |> member "attributes" with
      | `Null -> None
      | attrs -> Some attrs
    in

    Ok {
      zarr_format;
      node_type = `Array;
      shape;
      data_type;
      chunk_grid;
      chunk_key_encoding;
      fill_value;
      codecs;
      dimension_names;
      attributes;
    }
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (`Invalid_metadata ("JSON type error: " ^ msg))
  | Yojson.Json_error msg ->
    Error (`Invalid_metadata ("JSON parse error: " ^ msg))

(** Convert array metadata to JSON string *)
let array_to_json meta =
  let dimension_names_json = match meta.dimension_names with
    | None -> `Null
    | Some names -> `List (Array.to_list (Array.map (function
        | None -> `Null
        | Some s -> `String s
      ) names))
  in

  let json = `Assoc [
    ("zarr_format", `Int meta.zarr_format);
    ("node_type", `String "array");
    ("shape", `List (Array.to_list (Array.map (fun i -> `Int i) meta.shape)));
    ("data_type", `String (Data_type.to_string meta.data_type));
    ("chunk_grid", Chunk_grid.to_json meta.chunk_grid);
    ("chunk_key_encoding", Chunk_key.to_json meta.chunk_key_encoding);
    ("fill_value", FV_mod.to_json meta.data_type meta.fill_value);
    ("codecs", `List (Codec.specs_to_json meta.codecs));
    ("dimension_names", dimension_names_json);
    ("attributes", Option.value ~default:`Null meta.attributes);
  ] in
  Yojson.Safe.pretty_to_string json

(** Parse group metadata from JSON string *)
let group_of_json json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in

    (* zarr_format must be 3 *)
    let zarr_format = json |> member "zarr_format" |> to_int in
    if zarr_format <> 3 then
      Error (`Invalid_metadata (Printf.sprintf "zarr_format must be 3, got %d" zarr_format))
    else

    (* node_type must be "group" *)
    let node_type_str = json |> member "node_type" |> to_string in
    if node_type_str <> "group" then
      Error (`Invalid_metadata "node_type must be 'group' for group metadata")
    else

    (* attributes (optional) *)
    let attributes =
      match json |> member "attributes" with
      | `Null -> None
      | attrs -> Some attrs
    in

    Ok {
      zarr_format;
      node_type = `Group;
      attributes;
    }
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (`Invalid_metadata ("JSON type error: " ^ msg))
  | Yojson.Json_error msg ->
    Error (`Invalid_metadata ("JSON parse error: " ^ msg))

(** Convert group metadata to JSON string *)
let group_to_json meta =
  let json = `Assoc [
    ("zarr_format", `Int meta.zarr_format);
    ("node_type", `String "group");
    ("attributes", Option.value ~default:`Null meta.attributes);
  ] in
  Yojson.Safe.pretty_to_string json

(** Create default array metadata *)
let create_array_metadata
    ~shape
    ~chunks
    ~dtype
    ?(fill_value = FV_mod.default dtype)
    ?(codecs = [Bytes { endian = Some E.Little }])
    ?(dimension_names = None)
    ?(attributes = None)
    () =
  {
    zarr_format = 3;
    node_type = `Array;
    shape;
    data_type = dtype;
    chunk_grid = Regular { chunk_shape = chunks };
    chunk_key_encoding = Default { separator = S.Slash };
    fill_value;
    codecs;
    dimension_names;
    attributes;
  }

(** Create default group metadata *)
let create_group_metadata ?(attributes = None) () =
  {
    zarr_format = 3;
    node_type = `Group;
    attributes;
  }
