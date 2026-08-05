(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Character classification for YAML parsing *)

val is_break : char -> bool
(** Line break characters (\n or \r) *)

val is_blank : char -> bool
(** Blank (space or tab) *)

val is_whitespace : char -> bool
(** Whitespace (break or blank) *)

val is_digit : char -> bool
(** Decimal digit *)

val is_hex : char -> bool
(** Hexadecimal digit *)

val is_alpha : char -> bool
(** Alphabetic character *)

val is_alnum : char -> bool
(** Alphanumeric character *)

val is_indicator : char -> bool
(** YAML indicator characters *)

val is_flow_indicator : char -> bool
(** Flow context indicator characters (comma and brackets) *)
