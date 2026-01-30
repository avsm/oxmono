(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Character encoding detection and handling *)

type t = [ `Utf8 | `Utf16be | `Utf16le | `Utf32be | `Utf32le ]

val to_string : t -> string
(** Convert encoding to string representation *)

val pp : Format.formatter -> t -> unit
(** Pretty-print an encoding *)

val detect : string -> t * int
(** Detect encoding from BOM or first bytes. Returns (encoding, bom_length) *)

val equal : t -> t -> bool
(** Test equality of two encodings *)
