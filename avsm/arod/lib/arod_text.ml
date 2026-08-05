(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Plaintext extraction helpers for HTML content. *)

let strip_html s =
  let buf = Buffer.create (String.length s) in
  let in_tag = ref false in
  String.iter (fun c ->
    if c = '<' then in_tag := true
    else if c = '>' then in_tag := false
    else if not !in_tag then Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let collapse_whitespace s =
  let buf = Buffer.create (String.length s) in
  let in_ws = ref false in
  String.iter (fun c ->
    if c = ' ' || c = '\n' || c = '\r' || c = '\t' then begin
      if not !in_ws then Buffer.add_char buf ' ';
      in_ws := true
    end else begin
      in_ws := false;
      Buffer.add_char buf c
    end
  ) s;
  Buffer.contents buf

let truncate n s =
  if String.length s <= n then s
  else String.sub s 0 n ^ "\xe2\x80\xa6"

let plain_summary ?(max_len=150) html =
  let plain = strip_html html in
  let trimmed = String.trim (collapse_whitespace plain) in
  if String.length trimmed > 0 then
    Some (truncate max_len trimmed)
  else
    None
