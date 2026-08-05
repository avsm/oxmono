(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Format parser events as tree notation compatible with yaml-test-suite *)

open Yamlrw

let escape_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      match c with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\x00' -> Buffer.add_string buf "\\0"
      | '\x07' -> Buffer.add_string buf "\\a"
      | '\x08' -> Buffer.add_string buf "\\b"
      | '\x0b' -> Buffer.add_string buf "\\v"
      | '\x0c' -> Buffer.add_string buf "\\f"
      | '\x1b' -> Buffer.add_string buf "\\e"
      | '\xa0' -> Buffer.add_string buf "\\_"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let style_char = function
  | `Plain -> ':'
  | `Single_quoted -> '\''
  | `Double_quoted -> '"'
  | `Literal -> '|'
  | `Folded -> '>'
  | `Any -> ':'

let format_event { Event.event; span = _span } =
  match event with
  | Event.Stream_start _ -> "+STR"
  | Event.Stream_end -> "-STR"
  | Event.Document_start { implicit; _ } ->
      if implicit then "+DOC" else "+DOC ---"
  | Event.Document_end { implicit } -> if implicit then "-DOC" else "-DOC ..."
  | Event.Mapping_start { anchor; tag; style; _ } ->
      let anchor_str = match anchor with Some a -> " &" ^ a | None -> "" in
      let tag_str = match tag with Some t -> " <" ^ t ^ ">" | None -> "" in
      let flow_str = match style with `Flow -> " {}" | _ -> "" in
      Printf.sprintf "+MAP%s%s%s" flow_str anchor_str tag_str
  | Event.Mapping_end -> "-MAP"
  | Event.Sequence_start { anchor; tag; style; _ } ->
      let anchor_str = match anchor with Some a -> " &" ^ a | None -> "" in
      let tag_str = match tag with Some t -> " <" ^ t ^ ">" | None -> "" in
      let flow_str = match style with `Flow -> " []" | _ -> "" in
      Printf.sprintf "+SEQ%s%s%s" flow_str anchor_str tag_str
  | Event.Sequence_end -> "-SEQ"
  | Event.Scalar { anchor; tag; value; style; _ } ->
      let anchor_str = match anchor with Some a -> " &" ^ a | None -> "" in
      let tag_str = match tag with Some t -> " <" ^ t ^ ">" | None -> "" in
      let style_c = style_char style in
      Printf.sprintf "=VAL%s%s %c%s" anchor_str tag_str style_c
        (escape_string value)
  | Event.Alias { anchor } -> Printf.sprintf "=ALI *%s" anchor

let of_spanned_events events =
  let buf = Buffer.create 256 in
  List.iter
    (fun (e : Event.spanned) ->
      let line = format_event e in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n')
    events;
  Buffer.contents buf
