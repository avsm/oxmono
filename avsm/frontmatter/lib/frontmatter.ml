(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type yaml = Yamlrw.value

type t = {
  yaml : yaml;
  body : string;
  fname : string option;
}

let yaml { yaml; _ } = yaml
let body { body; _ } = body
let fname { fname; _ } = fname

let error_with_fname fname msg =
  let prefix = Option.fold ~none:"" ~some:(fun f -> f ^ ": ") fname in
  Error (prefix ^ msg)

(** Parse Jekyll-style date prefix from filename.
    Handles: 2025-01-15-slug.md or just slug.md *)
let parse_date_prefix s =
  let len = String.length s in
  if len >= 11 then
    try
      let year = int_of_string (String.sub s 0 4) in
      let month = int_of_string (String.sub s 5 2) in
      let day = int_of_string (String.sub s 8 2) in
      if s.[4] = '-' && s.[7] = '-' && s.[10] = '-' then
        match Ptime.of_date (year, month, day) with
        | Some date -> Some (date, String.sub s 11 (len - 11))
        | None -> None
      else None
    with _ -> None
  else None

(** Normalize a slug to match Jekyll's slug_of_string behavior:
    map all non-alphanumeric characters to hyphens and lowercase. *)
let normalize_slug s =
  let mapped = String.map (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' -> c
    | _ -> '-'
  ) s in
  String.lowercase_ascii mapped

let slug_of_fname fname =
  let basename = Filename.basename fname in
  let no_ext = Filename.chop_extension basename in
  match parse_date_prefix no_ext with
  | Some (date, slug) -> Ok (normalize_slug slug, Some date)
  | None -> Ok (normalize_slug no_ext, None)

(** Parse frontmatter using yamlrw's streaming parser.
    Uses multi-document support to find the document boundary,
    then extracts the body from the byte position. *)
let of_string ?fname content =
  (* Check for opening delimiter *)
  let content_trimmed = String.trim content in
  if not (String.length content_trimmed >= 3 && String.sub content_trimmed 0 3 = "---") then
    error_with_fname fname "Content does not start with '---' frontmatter delimiter"
  else
    let parser = Yamlrw.Parser.of_string content in
    let end_pos = ref 0 in
    (* Wrap parser to track Document_end position *)
    let next_with_tracking () =
      match Yamlrw.Parser.next parser with
      | None -> None
      | Some ev as result ->
        (match ev.event with
         | Yamlrw.Event.Document_end _ ->
           end_pos := ev.span.stop.Yamlrw.Position.index
         | _ -> ());
        result
    in
    try
      let yaml = Yamlrw.Loader.value_of_parser next_with_tracking in
      let body_start = !end_pos in
      (* Skip leading newline after document end marker *)
      let body_start =
        if body_start < String.length content && content.[body_start] = '\n'
        then body_start + 1
        else body_start
      in
      let body = String.sub content body_start (String.length content - body_start) in
      Ok { yaml; body; fname }
    with Yamlrw.Yamlrw_error e ->
      error_with_fname fname ("YAML parse error: " ^ Yamlrw.Error.to_string e)

let of_string_exn ?fname content =
  match of_string ?fname content with
  | Ok t -> t
  | Error msg -> failwith msg

let find key { yaml; _ } =
  match yaml with
  | `O fields -> List.assoc_opt key fields
  | _ -> None

let find_string key t =
  Option.bind (find key t) (function `String s -> Some s | _ -> None)

let find_strings key t =
  find key t
  |> Option.map (function
       | `A items -> List.filter_map (function `String s -> Some s | _ -> None) items
       | _ -> [])
  |> Option.value ~default:[]

let find_bool key t =
  Option.bind (find key t) (function `Bool b -> Some b | _ -> None)

let find_int key t =
  Option.bind (find key t) (function
    | `Float f when Float.is_integer f -> Some (int_of_float f)
    | _ -> None)

let find_float key t =
  Option.bind (find key t) (function `Float f -> Some f | _ -> None)

let decode jsont { yaml; _ } = Yamlt.decode_value jsont yaml

let decode_exn jsont t =
  match decode jsont t with
  | Ok v -> v
  | Error msg -> failwith msg
