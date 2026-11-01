(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* TEMPORARY. Each block below works around a defect of matrix 0.1.0's
   emulator that invariant-hq/mosaic has already fixed, so that the tests
   read the screen a terminal shows. Delete this module when console moves
   to matrix 0.2.0.

   The behaviour each block reproduces is matrix's, ISC License, Copyright
   (c) 2025, Thibaut Mattio. *)

(* invariant-hq/mosaic@ee777c47, "Make streaming UTF-8 decoding
   split-invariant": Ansi.Parser decodes a character fed a byte at a time
   with the bytes of an earlier one. Feed a valid multi-byte character whole,
   at its last byte. *)
let scan p s ~at d i f =
  let last = at + Uchar.utf_decode_length d - 1 in
  let b = Bytes.unsafe_of_string s in
  if Uchar.utf_decode_is_valid d && last > at then
    begin if last = i then Ansi.Parser.feed p b ~off:at ~len:(last - at + 1) f
    end
  else Ansi.Parser.feed p b ~off:i ~len:1 f

(* invariant-hq/mosaic@2dd1c517, "VT100 semantics for LF and BS": BS erases
   the cell it moves onto. Upstream makes it the cursor movement of CUB 1. *)
let backspace s =
  if not (String.contains s '\b') then s
  else String.concat "\027[D" (String.split_on_char '\b' s)
