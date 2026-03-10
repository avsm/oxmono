(** {1 Chunk Key Encoding}

    Maps chunk grid coordinates to storage keys.
    See Zarr v3.1 spec, "Chunk Key Encoding" section.

    - Default encoding: ["c/0/1/2"] or ["c.0.1.2"]
    - V2 encoding: ["0.1.2"] or ["0/1/2"] (legacy compatibility) *)

type encoding =
  | Default of { separator : Zarr_codec.separator }
  | V2 of { separator : Zarr_codec.separator }

let sep_char : Zarr_codec.separator -> char = function Slash -> '/' | Dot -> '.'
let sep_str : Zarr_codec.separator -> string = function Slash -> "/" | Dot -> "."

(** [encode encoding coords] converts chunk coordinates to a key string. *)
let encode encoding coords =
  let n = Array.length coords in
  if n = 0 then
    match encoding with Default _ -> "c" | V2 _ -> "0"
  else
    match encoding with
    | Default { separator } ->
      let s = sep_str separator in
      let buf = Buffer.create (n * 4 + 2) in
      Buffer.add_char buf 'c';
      for i = 0 to n - 1 do
        Buffer.add_string buf s;
        Buffer.add_string buf (string_of_int coords.(i))
      done;
      Buffer.contents buf
    | V2 { separator } ->
      let s = sep_str separator in
      let buf = Buffer.create (n * 4) in
      for i = 0 to n - 1 do
        if i > 0 then Buffer.add_string buf s;
        Buffer.add_string buf (string_of_int coords.(i))
      done;
      Buffer.contents buf

(** [decode encoding key] parses a key string to chunk coordinates. *)
let decode encoding key =
  match encoding with
  | Default { separator } ->
    let sep = sep_char separator in
    if String.length key < 2 || key.[0] <> 'c' || key.[1] <> sep then
      None
    else
      let rest = String.sub key 2 (String.length key - 2) in
      (try Some (Array.of_list (List.map int_of_string
                                  (String.split_on_char sep rest)))
       with Failure _ -> None)
  | V2 { separator } ->
    let sep = sep_char separator in
    (try Some (Array.of_list (List.map int_of_string
                                (String.split_on_char sep key)))
     with Failure _ -> None)

(** [full_path array_path encoding coords] returns the complete storage key
    for a chunk, combining the array path and chunk key. *)
let full_path array_path encoding coords =
  let ck = encode encoding coords in
  if array_path = "" || array_path = "/" then ck
  else if array_path.[String.length array_path - 1] = '/' then
    array_path ^ ck
  else
    array_path ^ "/" ^ ck

(** [metadata_path path] returns the storage key for [zarr.json] at
    the given node path. *)
let metadata_path path =
  if path = "" || path = "/" then "zarr.json"
  else if path.[String.length path - 1] = '/' then path ^ "zarr.json"
  else path ^ "/zarr.json"

(** {2 JSON serialization} *)

let of_json json =
  let name = Json_util.(member "name" json |> to_string_exn) in
  let config = Json_util.member "configuration" json in
  let sep = match Json_util.(member "separator" config |> to_string_exn) with
    | "." -> Zarr_codec.Dot | _ -> Slash in
  match name with
  | "default" -> Default { separator = sep }
  | "v2" -> V2 { separator = sep }
  | _ -> failwith ("unsupported chunk key encoding: " ^ name)

let to_json = function
  | Default { separator } ->
    Json_util.(obj [mem "name" (str "default");
         mem "configuration" (obj [
           mem "separator" (str (sep_str separator))])])
  | V2 { separator } ->
    Json_util.(obj [mem "name" (str "v2");
         mem "configuration" (obj [
           mem "separator" (str (sep_str separator))])])
