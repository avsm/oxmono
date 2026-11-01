(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let on_matrix_0_1 = String.starts_with ~prefix:"0.1." Matrix_version.v
let discard_if b = if on_matrix_0_1 && b then Alcobar.bad_test ()

(* VS16 is EF B8 8F in UTF-8. *)
let has_vs16 c =
  let rec from i =
    i + 3 <= String.length c
    && (String.sub c i 3 = "\xef\xb8\x8f" || from (i + 1))
  in
  from 0

(* Whether a VS16 in [s] follows a character that is no emoji. *)
let selects_nothing s =
  let rec from i prev =
    i < String.length s
    &&
    let d = String.get_utf_8_uchar s i in
    let u = Uchar.utf_decode_uchar d in
    (Uchar.to_int u = 0xfe0f && not (Uucp.Emoji.is_emoji prev))
    || from (i + Uchar.utf_decode_length d) u
  in
  from 0 Uchar.min

(* matrix 0.1.0 draws a cluster with a VS16 two cells wide, whatever console
   counts, and widens the cell of a spacing mark a VS16 follows. *)
let discard_stray_vs16 s =
  discard_if
    (List.exists
       (fun c -> has_vs16 c && Width.string_width c <> 2)
       (Width.clusters s)
    || selects_nothing s)

let is_printable_ascii c = c >= ' ' && c <= '~'

let discard_dropped_mark s =
  let rec from i =
    if i >= String.length s then false
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      i > 0
      && is_printable_ascii s.[i - 1]
      && n > 1
      && Width.string_width (String.sub s i n) = 0
      || from (i + n)
  in
  discard_if (from 0)

let is_newer_than_16 u =
  match Uucp.Age.age u with
  | `Version (major, _) -> major > 16
  | `Unassigned -> false

let discard_newer_unicode s =
  let rec from i =
    i < String.length s
    &&
    let d = String.get_utf_8_uchar s i in
    is_newer_than_16 (Uchar.utf_decode_uchar d)
    || from (i + Uchar.utf_decode_length d)
  in
  discard_if (from 0)

(* The cells of the first character of [c] with any, and of its widest
   one that is not a spacing mark after the first. *)
let first_and_widest c =
  let rec go i first widest =
    if i >= String.length c then (first, widest)
    else
      let d = String.get_utf_8_uchar c i in
      let u = Uchar.utf_decode_uchar d in
      let w = max 0 (Uucp.Break.tty_width_hint u) in
      let widest =
        if i > 0 && Uucp.Gc.general_category u = `Mc then widest
        else max widest w
      in
      go (i + Uchar.utf_decode_length d) (if first = 0 then w else first) widest
  in
  go 0 0 0

let discard_wide_after_first s =
  discard_if
    (List.exists
       (fun c ->
         let first, widest = first_and_widest c in
         widest > first)
       (Width.clusters s))
