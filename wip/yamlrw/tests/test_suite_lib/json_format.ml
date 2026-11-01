(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Format Value.t as JSON matching yaml-test-suite expected format *)

open Yamlrw

let escape_string s =
  let buf = Buffer.create (String.length s * 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\x08' -> Buffer.add_string buf "\\b"
      | '\x0c' -> Buffer.add_string buf "\\f"
      | c when Char.code c < 32 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let rec format_value ?(indent = 0) (v : Value.t) =
  let spaces n = String.make n ' ' in
  match v with
  | `Null -> "null"
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `Float f ->
      if Float.is_nan f then "null" (* JSON doesn't support NaN *)
      else if f = Float.infinity || f = Float.neg_infinity then "null"
        (* JSON doesn't support Inf *)
      else if Float.is_integer f && Float.abs f < 1e15 then
        Printf.sprintf "%.0f" f
      else
        (* Try to match yaml-test-suite's number formatting *)
        let s = Printf.sprintf "%g" f in
        (* Ensure we have a decimal point for floats *)
        if
          String.contains s '.' || String.contains s 'e'
          || String.contains s 'E'
        then s
        else s ^ ".0"
  | `String s -> escape_string s
  | `A [] -> "[]"
  | `A items ->
      let inner_indent = indent + 2 in
      let formatted_items =
        List.map
          (fun item ->
            spaces inner_indent ^ format_value ~indent:inner_indent item)
          items
      in
      "[\n" ^ String.concat ",\n" formatted_items ^ "\n" ^ spaces indent ^ "]"
  | `O [] -> "{}"
  | `O pairs ->
      let inner_indent = indent + 2 in
      let formatted_pairs =
        List.map
          (fun (k, v) ->
            let key = escape_string k in
            let value = format_value ~indent:inner_indent v in
            spaces inner_indent ^ key ^ ": " ^ value)
          pairs
      in
      "{\n" ^ String.concat ",\n" formatted_pairs ^ "\n" ^ spaces indent ^ "}"

let to_json (v : Value.t) : string = format_value v

(* Format multiple documents (for multi-doc YAML) *)
let documents_to_json (docs : Value.t list) : string =
  String.concat "\n" (List.map to_json docs)
