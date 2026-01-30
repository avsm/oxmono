(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Character encoding detection and handling *)

type t = [ `Utf8 | `Utf16be | `Utf16le | `Utf32be | `Utf32le ]

let to_string = function
  | `Utf8 -> "UTF-8"
  | `Utf16be -> "UTF-16BE"
  | `Utf16le -> "UTF-16LE"
  | `Utf32be -> "UTF-32BE"
  | `Utf32le -> "UTF-32LE"

let pp fmt t = Format.pp_print_string fmt (to_string t)

(** Detect encoding from BOM or first bytes. Returns (encoding, bom_length) *)
let detect s =
  let len = String.length s in
  if len = 0 then (`Utf8, 0)
  else
    let b0 = Char.code s.[0] in
    let b1 = if len > 1 then Char.code s.[1] else 0 in
    let b2 = if len > 2 then Char.code s.[2] else 0 in
    let b3 = if len > 3 then Char.code s.[3] else 0 in
    match (b0, b1, b2, b3) with
    (* BOM patterns *)
    | 0xEF, 0xBB, 0xBF, _ -> (`Utf8, 3)
    | 0xFE, 0xFF, _, _ -> (`Utf16be, 2)
    | 0xFF, 0xFE, 0x00, 0x00 -> (`Utf32le, 4)
    | 0xFF, 0xFE, _, _ -> (`Utf16le, 2)
    | 0x00, 0x00, 0xFE, 0xFF -> (`Utf32be, 4)
    (* Content pattern detection (no BOM) *)
    | 0x00, 0x00, 0x00, b3 when b3 <> 0x00 -> (`Utf32be, 0)
    | b0, 0x00, 0x00, 0x00 when b0 <> 0x00 -> (`Utf32le, 0)
    | 0x00, b1, _, _ when b1 <> 0x00 -> (`Utf16be, 0)
    | b0, 0x00, _, _ when b0 <> 0x00 -> (`Utf16le, 0)
    | _ -> (`Utf8, 0)

let equal a b = a = b
