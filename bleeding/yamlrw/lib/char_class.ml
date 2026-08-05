(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Character classification for YAML parsing *)

(** Line break characters *)
let is_break c = c = '\n' || c = '\r'

(** Blank (space or tab) *)
let is_blank c = c = ' ' || c = '\t'

(** Whitespace (break or blank) *)
let is_whitespace c = is_break c || is_blank c

(** Decimal digit *)
let is_digit c = c >= '0' && c <= '9'

(** Hexadecimal digit *)
let is_hex c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

(** Alphabetic character *)
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

(** Alphanumeric character *)
let is_alnum c = is_alpha c || is_digit c

(** YAML indicator characters *)
let is_indicator c =
  match c with
  | '-' | '?' | ':' | ',' | '[' | ']' | '{' | '}' | '#' | '&' | '*' | '!' | '|'
  | '>' | '\'' | '"' | '%' | '@' | '`' ->
      true
  | _ -> false

(** Flow context indicator characters *)
let is_flow_indicator c =
  match c with ',' | '[' | ']' | '{' | '}' -> true | _ -> false
