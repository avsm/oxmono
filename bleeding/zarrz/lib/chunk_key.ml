(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Default of { separator : char } | V2 of { separator : char }

let default = Default { separator = '/' }
let v2 = V2 { separator = '.' }

let separator_of_config ~default mems =
  match Jsont.Json.find_mem "separator" mems with
  | None -> Ok default
  | Some (_, Jsont.String ("/", _)) -> Ok '/'
  | Some (_, Jsont.String (".", _)) -> Ok '.'
  | Some (_, Jsont.String (s, _)) ->
      Error (Printf.sprintf "chunk key encoding: separator %S is not / or ." s)
  | Some _ -> Error "chunk key encoding: separator must be a string"

let config_separator ~default e =
  match e.Ext.config with
  | None -> Ok default
  | Some (Jsont.Object (mems, _)) -> (
      match
        List.find_opt (fun ((n, _), _) -> not (String.equal n "separator")) mems
      with
      | Some ((n, _), _) ->
          Error
            (Printf.sprintf
               "chunk key encoding: unknown configuration member %S" n)
      | None -> separator_of_config ~default mems)
  | Some _ -> Error "chunk key encoding: configuration must be an object"

let of_ext e =
  match e.Ext.name with
  | "default" ->
      Result.map
        (fun separator -> Default { separator })
        (config_separator ~default:'/' e)
  | "v2" ->
      Result.map
        (fun separator -> V2 { separator })
        (config_separator ~default:'.' e)
  | n -> Error (Printf.sprintf "chunk key encoding: unsupported name %S" n)

let to_ext t =
  let name, separator =
    match t with
    | Default { separator } -> ("default", separator)
    | V2 { separator } -> ("v2", separator)
  in
  let config =
    Jsont.Json.object'
      [
        ( Jsont.Json.name "separator",
          Jsont.Json.string (String.make 1 separator) );
      ]
  in
  Ext.v name ~config

let join sep i =
  let b = Buffer.create 32 in
  Array.iteri
    (fun d x ->
      if x < 0 then invalid_arg "Chunk_key.encode: negative chunk index";
      if d > 0 then Buffer.add_char b sep;
      Buffer.add_string b (string_of_int x))
    i;
  Buffer.contents b

let encode t i =
  match t with
  | Default { separator } ->
      if Array.length i = 0 then "c"
      else "c" ^ String.make 1 separator ^ join separator i
  | V2 { separator } -> if Array.length i = 0 then "0" else join separator i

let strip_root path =
  if String.length path > 0 && path.[0] = '/' then
    String.sub path 1 (String.length path - 1)
  else path

let data_key ~path k =
  let path = strip_root path in
  if String.equal path "" then k else path ^ "/" ^ k

let meta_key ~path =
  let path = strip_root path in
  if String.equal path "" then "zarr.json" else path ^ "/zarr.json"
