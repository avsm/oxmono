(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* TOML error helpers. Extends [Loc.Error] with typed TOML error kinds and
   raising helpers used by the lexer, parser and encoder. *)

(* Typed error payloads *)

type lexer_error =
  | Invalid_utf8
  | Incomplete_utf8
  | Invalid_escape of char
  | Incomplete_escape of string
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
  | Invalid_utf8 -> Fmt.pf fmt "invalid UTF-8 sequence"
  | Incomplete_utf8 -> Fmt.pf fmt "incomplete UTF-8 sequence"
  | Invalid_escape c -> Fmt.pf fmt "invalid escape sequence: \\%c" c
  | Incomplete_escape s -> Fmt.pf fmt "incomplete %s escape sequence" s
  | Invalid_unicode_escape s -> Fmt.pf fmt "invalid %s escape sequence" s
  | Invalid_unicode_codepoint cp ->
      Fmt.pf fmt "invalid Unicode codepoint: U+%X" cp
  | Surrogate_codepoint cp ->
      Fmt.pf fmt "surrogate codepoint not allowed: U+%04X" cp
  | Bare_carriage_return -> Fmt.pf fmt "bare carriage return not allowed"
  | Control_character cp -> Fmt.pf fmt "control character U+%04X not allowed" cp
  | Unterminated_string -> Fmt.pf fmt "unterminated string"
  | Unterminated_comment -> Fmt.pf fmt "unterminated comment"
  | Too_many_quotes -> Fmt.pf fmt "too many consecutive quotes"
  | Newline_in_string -> Fmt.pf fmt "newline not allowed in basic string"
  | Unexpected_character c -> Fmt.pf fmt "unexpected character '%c'" c
  | Unexpected_eof -> Fmt.pf fmt "unexpected end of input"

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
  | Leading_zero -> Fmt.pf fmt "leading zeros not allowed"
  | Leading_underscore -> Fmt.pf fmt "leading underscore not allowed"
  | Trailing_underscore -> Fmt.pf fmt "trailing underscore not allowed"
  | Double_underscore -> Fmt.pf fmt "double underscore not allowed"
  | Underscore_not_between_digits ->
      Fmt.pf fmt "underscore must be between digits"
  | Underscore_after_exponent -> Fmt.pf fmt "underscore cannot follow exponent"
  | Missing_digit -> Fmt.pf fmt "expected digit"
  | Missing_digit_after_sign -> Fmt.pf fmt "expected digit after sign"
  | Missing_digit_after_decimal ->
      Fmt.pf fmt "expected digit after decimal point"
  | Missing_digit_after_exponent -> Fmt.pf fmt "expected digit after exponent"
  | Invalid_hex_digit -> Fmt.pf fmt "invalid hexadecimal digit"
  | Invalid_octal_digit -> Fmt.pf fmt "invalid octal digit"
  | Invalid_binary_digit -> Fmt.pf fmt "invalid binary digit"

type datetime_error =
  | Month of int
  | Day of int * int
  | Hour of int
  | Minute of int
  | Second of int
  | Timezone_offset_hour of int
  | Timezone_offset_minute of int
  | Format of string

let pp_datetime_error fmt = function
  | Month m -> Fmt.pf fmt "invalid month: %d" m
  | Day (d, m) -> Fmt.pf fmt "invalid day %d for month %d" d m
  | Hour h -> Fmt.pf fmt "invalid hour: %d" h
  | Minute m -> Fmt.pf fmt "invalid minute: %d" m
  | Second s -> Fmt.pf fmt "invalid second: %d" s
  | Timezone_offset_hour h -> Fmt.pf fmt "invalid timezone offset hour: %d" h
  | Timezone_offset_minute m ->
      Fmt.pf fmt "invalid timezone offset minute: %d" m
  | Format desc -> Fmt.pf fmt "invalid %s format" desc

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
  | Duplicate_key k -> Fmt.pf fmt "duplicate key: %s" k
  | Table_already_defined k -> Fmt.pf fmt "table '%s' already defined" k
  | Cannot_redefine_table_as_value k ->
      Fmt.pf fmt "cannot redefine table '%s' as a value" k
  | Cannot_redefine_array_as_value k ->
      Fmt.pf fmt "cannot redefine array of tables '%s' as a value" k
  | Cannot_use_value_as_table k ->
      Fmt.pf fmt "cannot use value '%s' as a table" k
  | Cannot_extend_inline_table k ->
      Fmt.pf fmt "cannot extend inline table '%s'" k
  | Cannot_extend_closed_table k ->
      Fmt.pf fmt "cannot extend table '%s' using dotted keys" k
  | Cannot_extend_array_of_tables k ->
      Fmt.pf fmt "cannot extend array of tables '%s' using dotted keys" k
  | Cannot_convert_table_to_array k ->
      Fmt.pf fmt
        "cannot define '%s' as array of tables; already defined as table" k
  | Cannot_convert_array_to_table k ->
      Fmt.pf fmt
        "cannot define '%s' as table; already defined as array of tables" k
  | Table_has_content k ->
      Fmt.pf fmt "cannot define '%s' as array of tables; already has content" k
  | Conflicting_keys -> Fmt.pf fmt "conflicting keys in inline table"
  | Empty_key -> Fmt.pf fmt "empty key"
  | Multiline_key -> Fmt.pf fmt "multiline strings are not allowed as keys"

type syntax_error =
  | Expected of string
  | Invalid_table_header
  | Invalid_array_of_tables_header
  | Unexpected_token of string
  | Unexpected_bare_key of string

let pp_syntax_error fmt = function
  | Expected s -> Fmt.pf fmt "expected %s" s
  | Invalid_table_header -> Fmt.pf fmt "invalid table header syntax"
  | Invalid_array_of_tables_header ->
      Fmt.pf fmt "invalid array of tables syntax"
  | Unexpected_token s -> Fmt.pf fmt "unexpected token: %s" s
  | Unexpected_bare_key k -> Fmt.pf fmt "unexpected bare key '%s' as value" k

type encode_error = Cannot_encode_inline_table | Not_a_table

let pp_encode_error fmt = function
  | Cannot_encode_inline_table ->
      Fmt.pf fmt "cannot encode table inline without inline flag"
  | Not_a_table -> Fmt.pf fmt "top-level TOML must be a table"

(* Error kinds: extend the shared extensible variant. *)

type kind = Loc.Error.kind = ..

type Loc.Error.kind +=
  | Lexer of lexer_error
  | Number of number_error
  | Datetime of datetime_error
  | Semantic of semantic_error
  | Syntax of syntax_error
  | Encode of encode_error
  | Sort_mismatch of { exp : Sort.t; fnd : Sort.t }
  | Kinded_sort_mismatch of { exp : string; fnd : Sort.t }

let pp_code ppf s = Fmt.(styled `Bold string) ppf s

let () =
  Loc.Error.register_kind_printer @@ function
  | Lexer e -> Some (fun ppf -> pp_lexer_error ppf e)
  | Number e -> Some (fun ppf -> pp_number_error ppf e)
  | Datetime e -> Some (fun ppf -> pp_datetime_error ppf e)
  | Semantic e -> Some (fun ppf -> pp_semantic_error ppf e)
  | Syntax e -> Some (fun ppf -> pp_syntax_error ppf e)
  | Encode e -> Some (fun ppf -> pp_encode_error ppf e)
  | Sort_mismatch { exp; fnd } ->
      Some
        (fun ppf ->
          Fmt.pf ppf "Expected %a but found %a" Sort.pp exp Sort.pp fnd)
  | Kinded_sort_mismatch { exp; fnd } ->
      Some
        (fun ppf ->
          Fmt.pf ppf "Expected %a but found %a" pp_code exp Sort.pp fnd)
  | _ -> None

(* Re-exports: thin aliases over Loc.Error. *)

type t = Loc.Error.t = { ctx : Loc.Context.t; meta : Loc.Meta.t; kind : kind }

let string_of_kind = Loc.Error.string_of_kind
let v = Loc.Error.v
let msg = Loc.Error.msg
let raise = Loc.Error.raise
let fail = Loc.Error.fail
let failf = Loc.Error.failf
let expected = Loc.Error.expected
let push_array = Loc.Error.push_array
let push_object = Loc.Error.push_object
let adjust_context = Loc.Error.adjust_context
let pp = Loc.Error.pp
let to_string = Loc.Error.to_string
let pp_label = Loc.Error.pp_label

(* Raising helpers - one per payload category. *)

let raise_lexer ~meta e = raise ~ctx:Loc.Context.empty ~meta (Lexer e)
let raise_number ~meta e = raise ~ctx:Loc.Context.empty ~meta (Number e)
let raise_datetime ~meta e = raise ~ctx:Loc.Context.empty ~meta (Datetime e)
let raise_semantic ~meta e = raise ~ctx:Loc.Context.empty ~meta (Semantic e)
let raise_syntax ~meta e = raise ~ctx:Loc.Context.empty ~meta (Syntax e)
let raise_encode ~meta e = raise ~ctx:Loc.Context.empty ~meta (Encode e)

(* Shape-error helpers following the skill's standard menu. *)

let sort meta ~exp ~fnd =
  raise ~ctx:Loc.Context.empty ~meta (Sort_mismatch { exp; fnd })

let kinded_sort meta ~exp ~fnd =
  raise ~ctx:Loc.Context.empty ~meta (Kinded_sort_mismatch { exp; fnd })

let missing_mems meta ~kinded_sort:ks ~exp ~fnd:_ =
  match exp with
  | [ n ] -> failf meta "Missing member %a in %a" pp_code n pp_code ks
  | ns ->
      failf meta "@[<v1>Missing members in %a:@,%a@]" pp_code ks
        Fmt.(list ~sep:cut (fun ppf n -> pf ppf "%a" pp_code n))
        ns

let unexpected_mems meta ~kinded_sort:ks ~exp:_ ~fnd =
  match fnd with
  | [ (u, _) ] -> failf meta "Unexpected member %a for %a" pp_code u pp_code ks
  | us ->
      failf meta "@[<v1>Unexpected members for %a:@,%a@]" pp_code ks
        Fmt.(list ~sep:cut (fun ppf (n, _) -> pf ppf "%a" pp_code n))
        us

let unexpected_case_tag meta ~kinded_sort:ks ~mem_name ~exp ~fnd =
  failf meta "Unexpected %a value in %a: %a. Must be one of: %a" pp_code
    mem_name pp_code ks pp_code fnd
    Fmt.(list ~sep:(any ", ") pp_code)
    exp

let index_out_of_range meta ~n ~len =
  failf meta "Index %d out of range [0;%d]" n (len - 1)

let number_range meta ~kind n =
  failf meta "Number %g not in %a range" n pp_code kind

let parse_string_number meta ~kind s =
  failf meta "String %a does not parse to %a value" pp_code s pp_code kind

let integer_range meta ~kind n =
  failf meta "Integer %d not in %a range" n pp_code kind

let no_decoder meta ~kind = failf meta "No decoder for %a" pp_code kind
let no_encoder meta ~kind = failf meta "No encoder for %a" pp_code kind

let decode_todo meta ~kind_opt =
  if kind_opt = "" then failf meta "TODO: decode"
  else failf meta "TODO: decode %a" pp_code kind_opt

let encode_todo meta ~kind_opt =
  if kind_opt = "" then failf meta "TODO: encode"
  else failf meta "TODO: encode %a" pp_code kind_opt
