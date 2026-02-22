(** Chunk key encoding for Zarr v3 *)

open Ztypes

module S = Ztypes.Separator

(** Convert separator to character *)
let separator_char = function
  | S.Slash -> '/'
  | S.Dot -> '.'

(** Convert separator to string *)
let separator_string = function
  | S.Slash -> "/"
  | S.Dot -> "."

(** Encode chunk coordinates to a key path *)
let encode encoding coords =
  match encoding with
  | Default { separator } ->
    let sep = separator_string separator in
    let parts = Array.to_list (Array.map string_of_int coords) in
    "c" ^ sep ^ String.concat sep parts
  | V2 { separator } ->
    let sep = separator_string separator in
    let parts = Array.to_list (Array.map string_of_int coords) in
    String.concat sep parts

(** Decode a key path to chunk coordinates *)
let decode encoding key =
  match encoding with
  | Default { separator } ->
    let sep = separator_char separator in
    (* Key should start with "c" followed by separator *)
    if String.length key < 2 || key.[0] <> 'c' || key.[1] <> sep then
      None
    else
      let rest = String.sub key 2 (String.length key - 2) in
      let parts = String.split_on_char sep rest in
      (try
         Some (Array.of_list (List.map int_of_string parts))
       with Failure _ -> None)
  | V2 { separator } ->
    let sep = separator_char separator in
    let parts = String.split_on_char sep key in
    (try
       Some (Array.of_list (List.map int_of_string parts))
     with Failure _ -> None)

(** Get the full path for a chunk key relative to array path *)
let full_path array_path encoding coords =
  let chunk_key = encode encoding coords in
  if array_path = "" || array_path = "/" then
    chunk_key
  else if String.get array_path (String.length array_path - 1) = '/' then
    array_path ^ chunk_key
  else
    array_path ^ "/" ^ chunk_key

(** Parse chunk key encoding from JSON *)
let of_json json =
  let open Yojson.Safe.Util in
  let name = json |> member "name" |> to_string in
  let config = json |> member "configuration" in
  let sep = config |> member "separator" |> to_string in
  let separator = match sep with
    | "/" -> S.Slash
    | "." -> S.Dot
    | _ -> S.Slash  (* Default to slash *)
  in
  match name with
  | "default" -> Ok (Default { separator })
  | "v2" -> Ok (V2 { separator })
  | _ -> Error (`Invalid_metadata ("unsupported chunk key encoding: " ^ name))

(** Convert chunk key encoding to JSON *)
let to_json = function
  | Default { separator } ->
    `Assoc [
      ("name", `String "default");
      ("configuration", `Assoc [
        ("separator", `String (separator_string separator))
      ])
    ]
  | V2 { separator } ->
    `Assoc [
      ("name", `String "v2");
      ("configuration", `Assoc [
        ("separator", `String (separator_string separator))
      ])
    ]

(** Get the metadata file path for a node *)
let metadata_path path =
  if path = "" || path = "/" then
    "zarr.json"
  else if String.get path (String.length path - 1) = '/' then
    path ^ "zarr.json"
  else
    path ^ "/zarr.json"
