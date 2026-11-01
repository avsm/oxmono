(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Vcard_result

type entry = Position of int * int | Separator of string
type t = { default_separator : string option; entries : entry list }

let index s =
  if s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s then
    int_of_string_opt s
  else None

let entry_of_string s =
  if String.starts_with ~prefix:"s," s then
    Ok (Separator (Vcard_text.unescape (String.sub s 2 (String.length s - 2))))
  else
    match List.map index (String.split_on_char ',' s) with
    | [ Some a ] -> Ok (Position (a, 0))
    | [ Some a; Some b ] -> Ok (Position (a, b))
    | _ -> error "%S is not a JSCOMPS entry" s

let of_string s =
  match Vcard_text.split ';' s with
  | [] | [ _ ] ->
      error "a JSCOMPS value holds at least one entry after the first"
  | first :: rest ->
      let* default_separator =
        if first = "" then Ok None
        else
          match entry_of_string first with
          | Ok (Separator sep) -> Ok (Some sep)
          | _ -> error "the first JSCOMPS entry %S is not a separator" first
      in
      let rec entries acc = function
        | [] -> Ok (List.rev acc)
        | e :: es ->
            let* e = entry_of_string e in
            entries (e :: acc) es
      in
      let* entries = entries [] rest in
      Ok { default_separator; entries }

let entry_to_string = function
  | Position (i, 0) -> string_of_int i
  | Position (i, j) -> Printf.sprintf "%d,%d" i j
  | Separator s -> "s," ^ Vcard_text.escape_component s

let to_string t =
  let first =
    match t.default_separator with
    | None -> ""
    | Some s -> entry_to_string (Separator s)
  in
  String.concat ";" (first :: List.map entry_to_string t.entries)

let equal_entry a b =
  match (a, b) with
  | Position (i, j), Position (i', j') -> i = i' && j = j'
  | Separator a, Separator b -> String.equal a b
  | _ -> false

let equal a b =
  Option.equal String.equal a.default_separator b.default_separator
  && List.equal equal_entry a.entries b.entries

let pp ppf t = Format.pp_print_string ppf (to_string t)
