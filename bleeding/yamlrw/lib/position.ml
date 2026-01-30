(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Position tracking for source locations *)

type t = {
  index : int;  (** Byte offset from start *)
  line : int;  (** 1-indexed line number *)
  column : int;  (** 1-indexed column number *)
}

let initial = { index = 0; line = 1; column = 1 }
let advance_byte t = { t with index = t.index + 1; column = t.column + 1 }
let advance_line t = { index = t.index + 1; line = t.line + 1; column = 1 }
let advance_char c t = if c = '\n' then advance_line t else advance_byte t

let advance_utf8 uchar t =
  let len = Uchar.utf_8_byte_length uchar in
  let code = Uchar.to_int uchar in
  if code = 0x0A (* LF *) then
    { index = t.index + len; line = t.line + 1; column = 1 }
  else { t with index = t.index + len; column = t.column + 1 }

let advance_bytes n t = { t with index = t.index + n; column = t.column + n }
let pp fmt t = Format.fprintf fmt "line %d, column %d" t.line t.column
let to_string t = Format.asprintf "%a" pp t
let compare a b = Int.compare a.index b.index
let equal a b = a.index = b.index
