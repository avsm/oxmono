(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** TOML parsing and encoding error types *)

(** Location in the input *)
type location = {
  line : int;
  column : int;
  file : string option;
}

let pp_location fmt loc =
  match loc.file with
  | Some f -> Format.fprintf fmt "%s:%d:%d" f loc.line loc.column
  | None -> Format.fprintf fmt "line %d, column %d" loc.line loc.column

(** Lexer errors - low-level tokenization issues *)
type lexer_error =
  | Invalid_utf8
  | Incomplete_utf8
  | Invalid_escape of char
  | Incomplete_escape of string  (** e.g., "\\x", "\\u", "\\U" *)
  | Invalid_unicode_escape of string
  | Invalid_unicode_codepoint of int
  | Surrogate_codepoint of int
  | Bare_carriage_return
  | Control_character of int
  | Unterminated_string
  | Unterminated_comment
  | Too_many_quotes
  | Newline_in_string
  | Unexpected_character of char
  | Unexpected_eof

let pp_lexer_error fmt = function
  | Invalid_utf8 -> Format.fprintf fmt "invalid UTF-8 sequence"
  | Incomplete_utf8 -> Format.fprintf fmt "incomplete UTF-8 sequence"
  | Invalid_escape c -> Format.fprintf fmt "invalid escape sequence: \\%c" c
  | Incomplete_escape s -> Format.fprintf fmt "incomplete %s escape sequence" s
  | Invalid_unicode_escape s -> Format.fprintf fmt "invalid %s escape sequence" s
  | Invalid_unicode_codepoint cp -> Format.fprintf fmt "invalid Unicode codepoint: U+%X" cp
  | Surrogate_codepoint cp -> Format.fprintf fmt "surrogate codepoint not allowed: U+%04X" cp
  | Bare_carriage_return -> Format.fprintf fmt "bare carriage return not allowed"
  | Control_character cp -> Format.fprintf fmt "control character U+%04X not allowed" cp
  | Unterminated_string -> Format.fprintf fmt "unterminated string"
  | Unterminated_comment -> Format.fprintf fmt "unterminated comment"
  | Too_many_quotes -> Format.fprintf fmt "too many consecutive quotes"
  | Newline_in_string -> Format.fprintf fmt "newline not allowed in basic string"
  | Unexpected_character c -> Format.fprintf fmt "unexpected character '%c'" c
  | Unexpected_eof -> Format.fprintf fmt "unexpected end of input"

(** Number parsing errors *)
type number_error =
  | Leading_zero
  | Leading_underscore
  | Trailing_underscore
  | Double_underscore
  | Underscore_not_between_digits
  | Underscore_after_exponent
  | Missing_digit
  | Missing_digit_after_sign
  | Missing_digit_after_decimal
  | Missing_digit_after_exponent
  | Invalid_hex_digit
  | Invalid_octal_digit
  | Invalid_binary_digit

let pp_number_error fmt = function
  | Leading_zero -> Format.fprintf fmt "leading zeros not allowed"
  | Leading_underscore -> Format.fprintf fmt "leading underscore not allowed"
  | Trailing_underscore -> Format.fprintf fmt "trailing underscore not allowed"
  | Double_underscore -> Format.fprintf fmt "double underscore not allowed"
  | Underscore_not_between_digits -> Format.fprintf fmt "underscore must be between digits"
  | Underscore_after_exponent -> Format.fprintf fmt "underscore cannot follow exponent"
  | Missing_digit -> Format.fprintf fmt "expected digit"
  | Missing_digit_after_sign -> Format.fprintf fmt "expected digit after sign"
  | Missing_digit_after_decimal -> Format.fprintf fmt "expected digit after decimal point"
  | Missing_digit_after_exponent -> Format.fprintf fmt "expected digit after exponent"
  | Invalid_hex_digit -> Format.fprintf fmt "invalid hexadecimal digit"
  | Invalid_octal_digit -> Format.fprintf fmt "invalid octal digit"
  | Invalid_binary_digit -> Format.fprintf fmt "invalid binary digit"

(** DateTime parsing errors *)
type datetime_error =
  | Invalid_month of int
  | Invalid_day of int * int  (** day, month *)
  | Invalid_hour of int
  | Invalid_minute of int
  | Invalid_second of int
  | Invalid_timezone_offset_hour of int
  | Invalid_timezone_offset_minute of int
  | Invalid_format of string  (** expected format description *)

let pp_datetime_error fmt = function
  | Invalid_month m -> Format.fprintf fmt "invalid month: %d" m
  | Invalid_day (d, m) -> Format.fprintf fmt "invalid day %d for month %d" d m
  | Invalid_hour h -> Format.fprintf fmt "invalid hour: %d" h
  | Invalid_minute m -> Format.fprintf fmt "invalid minute: %d" m
  | Invalid_second s -> Format.fprintf fmt "invalid second: %d" s
  | Invalid_timezone_offset_hour h -> Format.fprintf fmt "invalid timezone offset hour: %d" h
  | Invalid_timezone_offset_minute m -> Format.fprintf fmt "invalid timezone offset minute: %d" m
  | Invalid_format desc -> Format.fprintf fmt "invalid %s format" desc

(** Semantic/table structure errors *)
type semantic_error =
  | Duplicate_key of string
  | Table_already_defined of string
  | Cannot_redefine_table_as_value of string
  | Cannot_redefine_array_as_value of string
  | Cannot_use_value_as_table of string
  | Cannot_extend_inline_table of string
  | Cannot_extend_closed_table of string
  | Cannot_extend_array_of_tables of string
  | Cannot_convert_table_to_array of string
  | Cannot_convert_array_to_table of string
  | Table_has_content of string
  | Conflicting_keys
  | Empty_key
  | Multiline_key

let pp_semantic_error fmt = function
  | Duplicate_key k -> Format.fprintf fmt "duplicate key: %s" k
  | Table_already_defined k -> Format.fprintf fmt "table '%s' already defined" k
  | Cannot_redefine_table_as_value k -> Format.fprintf fmt "cannot redefine table '%s' as a value" k
  | Cannot_redefine_array_as_value k -> Format.fprintf fmt "cannot redefine array of tables '%s' as a value" k
  | Cannot_use_value_as_table k -> Format.fprintf fmt "cannot use value '%s' as a table" k
  | Cannot_extend_inline_table k -> Format.fprintf fmt "cannot extend inline table '%s'" k
  | Cannot_extend_closed_table k -> Format.fprintf fmt "cannot extend table '%s' using dotted keys" k
  | Cannot_extend_array_of_tables k -> Format.fprintf fmt "cannot extend array of tables '%s' using dotted keys" k
  | Cannot_convert_table_to_array k -> Format.fprintf fmt "cannot define '%s' as array of tables; already defined as table" k
  | Cannot_convert_array_to_table k -> Format.fprintf fmt "cannot define '%s' as table; already defined as array of tables" k
  | Table_has_content k -> Format.fprintf fmt "cannot define '%s' as array of tables; already has content" k
  | Conflicting_keys -> Format.fprintf fmt "conflicting keys in inline table"
  | Empty_key -> Format.fprintf fmt "empty key"
  | Multiline_key -> Format.fprintf fmt "multiline strings are not allowed as keys"

(** Syntax errors *)
type syntax_error =
  | Expected of string
  | Invalid_table_header
  | Invalid_array_of_tables_header
  | Unexpected_token of string
  | Unexpected_bare_key of string

let pp_syntax_error fmt = function
  | Expected s -> Format.fprintf fmt "expected %s" s
  | Invalid_table_header -> Format.fprintf fmt "invalid table header syntax"
  | Invalid_array_of_tables_header -> Format.fprintf fmt "invalid array of tables syntax"
  | Unexpected_token s -> Format.fprintf fmt "unexpected token: %s" s
  | Unexpected_bare_key k -> Format.fprintf fmt "unexpected bare key '%s' as value" k

(** Encoding errors *)
type encode_error =
  | Cannot_encode_inline_table
  | Not_a_table

let pp_encode_error fmt = function
  | Cannot_encode_inline_table -> Format.fprintf fmt "cannot encode table inline without inline flag"
  | Not_a_table -> Format.fprintf fmt "top-level TOML must be a table"

(** All error kinds *)
type kind =
  | Lexer of lexer_error
  | Number of number_error
  | Datetime of datetime_error
  | Semantic of semantic_error
  | Syntax of syntax_error
  | Encode of encode_error

let pp_kind fmt = function
  | Lexer e -> pp_lexer_error fmt e
  | Number e -> pp_number_error fmt e
  | Datetime e -> pp_datetime_error fmt e
  | Semantic e -> pp_semantic_error fmt e
  | Syntax e -> pp_syntax_error fmt e
  | Encode e -> pp_encode_error fmt e

(** Full error with location *)
type t = {
  kind : kind;
  location : location option;
}

let make ?location kind = { kind; location }

let pp fmt t =
  match t.location with
  | Some loc -> Format.fprintf fmt "%a: %a" pp_location loc pp_kind t.kind
  | None -> pp_kind fmt t.kind

let to_string t =
  Format.asprintf "%a" pp t

(** Exception for TOML errors *)
exception Error of t

let () = Printexc.register_printer (function
  | Error e -> Some (Format.asprintf "Tomlt.Error: %a" pp e)
  | _ -> None)

(** Raise a TOML error *)
let raise_error ?location kind =
  raise (Error { kind; location })

let raise_lexer ?location e = raise_error ?location (Lexer e)
let raise_number ?location e = raise_error ?location (Number e)
let raise_datetime ?location e = raise_error ?location (Datetime e)
let raise_semantic ?location e = raise_error ?location (Semantic e)
let raise_syntax ?location e = raise_error ?location (Syntax e)
let raise_encode ?location e = raise_error ?location (Encode e)

(** Create location from line and column *)
let loc ?file ~line ~column () = { line; column; file }
