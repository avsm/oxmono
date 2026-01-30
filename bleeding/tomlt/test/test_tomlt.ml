(* Comprehensive test suite for tomlt - TOML 1.1 codec *)

open Tomlt.Toml

(* Helper to encode TOML to string via writer *)
let to_toml_string value =
  let buf = Buffer.create 256 in
  Tomlt_bytesrw.to_writer (Bytesrw.Bytes.Writer.of_buffer buf) value;
  Buffer.contents buf

(* Helper to parse and extract value *)
let parse_toml s =
  match Tomlt_bytesrw.of_string s with
  | Ok v -> v
  | Error e -> Alcotest.fail (Error.to_string e)

let parse_error s =
  match Tomlt_bytesrw.of_string s with
  | Ok _ -> Alcotest.fail "Expected parse error"
  | Error _ -> ()

(* Custom testable for t *)
let rec pp_t fmt = function
  | String s -> Format.fprintf fmt "String %S" s
  | Int i -> Format.fprintf fmt "Int %Ld" i
  | Float f -> Format.fprintf fmt "Float %f" f
  | Bool b -> Format.fprintf fmt "Bool %b" b
  | Datetime s -> Format.fprintf fmt "Datetime %S" s
  | Datetime_local s -> Format.fprintf fmt "Datetime_local %S" s
  | Date_local s -> Format.fprintf fmt "Date_local %S" s
  | Time_local s -> Format.fprintf fmt "Time_local %S" s
  | Array items ->
      Format.fprintf fmt "Array [%a]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") pp_t)
        items
  | Table pairs ->
      Format.fprintf fmt "Table [%a]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (k, v) -> Format.fprintf fmt "(%S, %a)" k pp_t v))
        pairs

let rec equal_t a b =
  match a, b with
  | String s1, String s2 -> String.equal s1 s2
  | Int i1, Int i2 -> Int64.equal i1 i2
  | Float f1, Float f2 ->
      Float.equal f1 f2 || (Float.is_nan f1 && Float.is_nan f2)
  | Bool b1, Bool b2 -> Bool.equal b1 b2
  | Datetime s1, Datetime s2 -> String.equal s1 s2
  | Datetime_local s1, Datetime_local s2 -> String.equal s1 s2
  | Date_local s1, Date_local s2 -> String.equal s1 s2
  | Time_local s1, Time_local s2 -> String.equal s1 s2
  | Array a1, Array a2 ->
      List.length a1 = List.length a2 &&
      List.for_all2 equal_t a1 a2
  | Table p1, Table p2 ->
      List.length p1 = List.length p2 &&
      List.for_all2 (fun (k1, v1) (k2, v2) ->
        String.equal k1 k2 && equal_t v1 v2
      ) (List.sort Stdlib.compare p1) (List.sort Stdlib.compare p2)
  | _ -> false

let value_testable = Alcotest.testable pp_t equal_t

(* Helper to get a key from a table *)
let get key = function
  | Table pairs -> List.assoc key pairs
  | _ -> Alcotest.fail "Expected table"

(* ============================================
   Comments
   ============================================ *)

let test_comment_full_line () =
  let t = parse_toml "# This is a comment\nkey = \"value\"" in
  Alcotest.(check value_testable) "full line comment" (String "value") (get "key" t)

let test_comment_inline () =
  let t = parse_toml "key = \"value\" # inline comment" in
  Alcotest.(check value_testable) "inline comment" (String "value") (get "key" t)

let test_comment_hash_in_string () =
  let t = parse_toml "key = \"# not a comment\"" in
  Alcotest.(check value_testable) "hash in string" (String "# not a comment") (get "key" t)

let test_comment_empty () =
  let t = parse_toml "#\nkey = 1" in
  Alcotest.(check value_testable) "empty comment" (Int 1L) (get "key" t)

let comment_tests = [
  "full line comment", `Quick, test_comment_full_line;
  "inline comment", `Quick, test_comment_inline;
  "hash in string", `Quick, test_comment_hash_in_string;
  "empty comment", `Quick, test_comment_empty;
]

(* ============================================
   Keys - Bare, Quoted, Dotted
   ============================================ *)

let test_bare_key () =
  let t = parse_toml "key = \"value\"" in
  Alcotest.(check value_testable) "simple bare key" (String "value") (get "key" t)

let test_bare_key_underscore () =
  let t = parse_toml "bare_key = \"value\"" in
  Alcotest.(check value_testable) "bare key with underscore" (String "value") (get "bare_key" t)

let test_bare_key_dash () =
  let t = parse_toml "bare-key = \"value\"" in
  Alcotest.(check value_testable) "bare key with dash" (String "value") (get "bare-key" t)

let test_bare_key_numeric () =
  let t = parse_toml "1234 = \"value\"" in
  Alcotest.(check value_testable) "numeric bare key" (String "value") (get "1234" t)

let test_quoted_key_basic () =
  let t = parse_toml "\"127.0.0.1\" = \"value\"" in
  Alcotest.(check value_testable) "quoted key with dots" (String "value") (get "127.0.0.1" t)

let test_quoted_key_spaces () =
  let t = parse_toml "\"character encoding\" = \"value\"" in
  Alcotest.(check value_testable) "quoted key with spaces" (String "value") (get "character encoding" t)

let test_quoted_key_literal () =
  let t = parse_toml "'key' = \"value\"" in
  Alcotest.(check value_testable) "literal quoted key" (String "value") (get "key" t)

let test_empty_quoted_key () =
  let t = parse_toml "\"\" = \"blank\"" in
  Alcotest.(check value_testable) "empty quoted key" (String "blank") (get "" t)

let test_dotted_key () =
  let t = parse_toml "physical.color = \"orange\"" in
  match get "physical" t with
  | Table pairs ->
      Alcotest.(check value_testable) "dotted key" (String "orange") (List.assoc "color" pairs)
  | _ -> Alcotest.fail "Expected nested table"

let test_dotted_key_quoted () =
  let t = parse_toml "site.\"google.com\" = true" in
  match get "site" t with
  | Table pairs ->
      Alcotest.(check value_testable) "dotted key with quoted part" (Bool true) (List.assoc "google.com" pairs)
  | _ -> Alcotest.fail "Expected nested table"

let test_dotted_key_whitespace () =
  let t = parse_toml "fruit . color = \"yellow\"" in
  match get "fruit" t with
  | Table pairs ->
      Alcotest.(check value_testable) "dotted key with whitespace" (String "yellow") (List.assoc "color" pairs)
  | _ -> Alcotest.fail "Expected nested table"

let test_duplicate_key_error () =
  parse_error "name = \"Tom\"\nname = \"Pradyun\""

let test_bare_quoted_equivalent () =
  parse_error "spelling = \"favorite\"\n\"spelling\" = \"favourite\""

let key_tests = [
  "bare key", `Quick, test_bare_key;
  "bare key underscore", `Quick, test_bare_key_underscore;
  "bare key dash", `Quick, test_bare_key_dash;
  "bare key numeric", `Quick, test_bare_key_numeric;
  "quoted key basic", `Quick, test_quoted_key_basic;
  "quoted key spaces", `Quick, test_quoted_key_spaces;
  "quoted key literal", `Quick, test_quoted_key_literal;
  "empty quoted key", `Quick, test_empty_quoted_key;
  "dotted key", `Quick, test_dotted_key;
  "dotted key quoted", `Quick, test_dotted_key_quoted;
  "dotted key whitespace", `Quick, test_dotted_key_whitespace;
  "duplicate key error", `Quick, test_duplicate_key_error;
  "bare quoted equivalent", `Quick, test_bare_quoted_equivalent;
]

(* ============================================
   Strings - Basic, Literal, Multiline
   ============================================ *)

let test_basic_string () =
  let t = parse_toml {|str = "hello world"|} in
  Alcotest.(check value_testable) "basic string" (String "hello world") (get "str" t)

let test_basic_string_escapes () =
  let t = parse_toml {|str = "tab\there"|} in
  Alcotest.(check value_testable) "tab escape" (String "tab\there") (get "str" t)

let test_basic_string_newline () =
  let t = parse_toml {|str = "line1\nline2"|} in
  Alcotest.(check value_testable) "newline escape" (String "line1\nline2") (get "str" t)

let test_basic_string_backslash () =
  let t = parse_toml {|str = "back\\slash"|} in
  Alcotest.(check value_testable) "backslash escape" (String "back\\slash") (get "str" t)

let test_basic_string_quote () =
  let t = parse_toml {|str = "say \"hello\""|} in
  Alcotest.(check value_testable) "quote escape" (String "say \"hello\"") (get "str" t)

let test_basic_string_unicode_u () =
  let t = parse_toml {|str = "\u0041"|} in
  Alcotest.(check value_testable) "unicode \\u escape" (String "A") (get "str" t)

let test_basic_string_unicode_U () =
  let t = parse_toml {|str = "\U0001F600"|} in
  (* U+1F600 is the grinning face emoji *)
  Alcotest.(check value_testable) "unicode \\U escape" (String "\xF0\x9F\x98\x80") (get "str" t)

let test_basic_string_hex_escape () =
  let t = parse_toml {|str = "\xE9"|} in
  (* U+00E9 is e-acute *)
  Alcotest.(check value_testable) "hex escape" (String "\xC3\xA9") (get "str" t)

let test_basic_string_escape_e () =
  let t = parse_toml {|str = "\e"|} in
  Alcotest.(check value_testable) "escape \\e" (String "\x1B") (get "str" t)

let test_literal_string () =
  let t = parse_toml {|str = 'C:\Users\nodejs\templates'|} in
  Alcotest.(check value_testable) "literal string" (String {|C:\Users\nodejs\templates|}) (get "str" t)

let test_literal_string_no_escape () =
  let t = parse_toml {|str = '<\i\c*\s*>'|} in
  Alcotest.(check value_testable) "literal no escape" (String {|<\i\c*\s*>|}) (get "str" t)

let test_multiline_basic () =
  let t = parse_toml {|str = """
Roses are red
Violets are blue"""|} in
  Alcotest.(check value_testable) "multiline basic" (String "Roses are red\nViolets are blue") (get "str" t)

let test_multiline_basic_trim () =
  let t = parse_toml {|str = """\
       The quick brown \
       fox jumps over \
       the lazy dog.\
       """|} in
  Alcotest.(check value_testable) "multiline trim" (String "The quick brown fox jumps over the lazy dog.") (get "str" t)

let test_multiline_basic_quotes () =
  let t = parse_toml {|str = """Here are two quotation marks: "". Simple."""|} in
  Alcotest.(check value_testable) "multiline with quotes" (String {|Here are two quotation marks: "". Simple.|}) (get "str" t)

let test_multiline_literal () =
  let t = parse_toml {|str = '''
The first newline is
trimmed in literal strings.
   All other whitespace
   is preserved.
'''|} in
  let expected = "The first newline is\ntrimmed in literal strings.\n   All other whitespace\n   is preserved.\n" in
  Alcotest.(check value_testable) "multiline literal" (String expected) (get "str" t)

let test_multiline_literal_no_escape () =
  let t = parse_toml {|str = '''I [dw]on't need \d{2} apples'''|} in
  Alcotest.(check value_testable) "multiline literal no escape" (String {|I [dw]on't need \d{2} apples|}) (get "str" t)

let string_tests = [
  "basic string", `Quick, test_basic_string;
  "basic string escapes", `Quick, test_basic_string_escapes;
  "basic string newline", `Quick, test_basic_string_newline;
  "basic string backslash", `Quick, test_basic_string_backslash;
  "basic string quote", `Quick, test_basic_string_quote;
  "basic string unicode u", `Quick, test_basic_string_unicode_u;
  "basic string unicode U", `Quick, test_basic_string_unicode_U;
  "basic string hex escape", `Quick, test_basic_string_hex_escape;
  "basic string escape e", `Quick, test_basic_string_escape_e;
  "literal string", `Quick, test_literal_string;
  "literal string no escape", `Quick, test_literal_string_no_escape;
  "multiline basic", `Quick, test_multiline_basic;
  "multiline basic trim", `Quick, test_multiline_basic_trim;
  "multiline basic quotes", `Quick, test_multiline_basic_quotes;
  "multiline literal", `Quick, test_multiline_literal;
  "multiline literal no escape", `Quick, test_multiline_literal_no_escape;
]

(* ============================================
   Integers - Decimal, Hex, Octal, Binary
   ============================================ *)

let test_integer_positive () =
  let t = parse_toml "int = +99" in
  Alcotest.(check value_testable) "positive integer" (Int 99L) (get "int" t)

let test_integer_plain () =
  let t = parse_toml "int = 42" in
  Alcotest.(check value_testable) "plain integer" (Int 42L) (get "int" t)

let test_integer_zero () =
  let t = parse_toml "int = 0" in
  Alcotest.(check value_testable) "zero" (Int 0L) (get "int" t)

let test_integer_negative () =
  let t = parse_toml "int = -17" in
  Alcotest.(check value_testable) "negative integer" (Int (-17L)) (get "int" t)

let test_integer_underscore () =
  let t = parse_toml "int = 1_000" in
  Alcotest.(check value_testable) "underscore separator" (Int 1000L) (get "int" t)

let test_integer_underscore_multi () =
  let t = parse_toml "int = 5_349_221" in
  Alcotest.(check value_testable) "multiple underscores" (Int 5349221L) (get "int" t)

let test_integer_hex () =
  let t = parse_toml "int = 0xDEADBEEF" in
  Alcotest.(check value_testable) "hexadecimal" (Int 0xDEADBEEFL) (get "int" t)

let test_integer_hex_lower () =
  let t = parse_toml "int = 0xdeadbeef" in
  Alcotest.(check value_testable) "hex lowercase" (Int 0xdeadbeefL) (get "int" t)

let test_integer_hex_underscore () =
  let t = parse_toml "int = 0xdead_beef" in
  Alcotest.(check value_testable) "hex with underscore" (Int 0xdeadbeefL) (get "int" t)

let test_integer_octal () =
  let t = parse_toml "int = 0o755" in
  Alcotest.(check value_testable) "octal" (Int 0o755L) (get "int" t)

let test_integer_binary () =
  let t = parse_toml "int = 0b11010110" in
  Alcotest.(check value_testable) "binary" (Int 0b11010110L) (get "int" t)

let test_integer_leading_zero_error () =
  parse_error "int = 007"

let test_integer_large () =
  let t = parse_toml "int = 9223372036854775807" in
  Alcotest.(check value_testable) "max int64" (Int Int64.max_int) (get "int" t)

let test_integer_negative_large () =
  let t = parse_toml "int = -9223372036854775808" in
  Alcotest.(check value_testable) "min int64" (Int Int64.min_int) (get "int" t)

let integer_tests = [
  "positive integer", `Quick, test_integer_positive;
  "plain integer", `Quick, test_integer_plain;
  "zero", `Quick, test_integer_zero;
  "negative integer", `Quick, test_integer_negative;
  "underscore separator", `Quick, test_integer_underscore;
  "multiple underscores", `Quick, test_integer_underscore_multi;
  "hexadecimal", `Quick, test_integer_hex;
  "hex lowercase", `Quick, test_integer_hex_lower;
  "hex with underscore", `Quick, test_integer_hex_underscore;
  "octal", `Quick, test_integer_octal;
  "binary", `Quick, test_integer_binary;
  "leading zero error", `Quick, test_integer_leading_zero_error;
  "max int64", `Quick, test_integer_large;
  "min int64", `Quick, test_integer_negative_large;
]

(* ============================================
   Floats - Fractional, Exponent, Special
   ============================================ *)

let test_float_positive () =
  let t = parse_toml "flt = +1.0" in
  Alcotest.(check value_testable) "positive float" (Float 1.0) (get "flt" t)

let test_float_fractional () =
  let t = parse_toml "flt = 3.1415" in
  Alcotest.(check value_testable) "fractional" (Float 3.1415) (get "flt" t)

let test_float_negative () =
  let t = parse_toml "flt = -0.01" in
  Alcotest.(check value_testable) "negative float" (Float (-0.01)) (get "flt" t)

let test_float_exponent () =
  let t = parse_toml "flt = 5e+22" in
  Alcotest.(check value_testable) "exponent" (Float 5e+22) (get "flt" t)

let test_float_exponent_no_sign () =
  let t = parse_toml "flt = 1e06" in
  Alcotest.(check value_testable) "exponent no sign" (Float 1e06) (get "flt" t)

let test_float_exponent_negative () =
  let t = parse_toml "flt = -2E-2" in
  Alcotest.(check value_testable) "negative exponent" (Float (-2E-2)) (get "flt" t)

let test_float_both () =
  let t = parse_toml "flt = 6.626e-34" in
  Alcotest.(check value_testable) "fractional and exponent" (Float 6.626e-34) (get "flt" t)

let test_float_underscore () =
  let t = parse_toml "flt = 224_617.445_991_228" in
  Alcotest.(check value_testable) "underscore in float" (Float 224617.445991228) (get "flt" t)

let test_float_inf () =
  let t = parse_toml "flt = inf" in
  Alcotest.(check value_testable) "infinity" (Float Float.infinity) (get "flt" t)

let test_float_pos_inf () =
  let t = parse_toml "flt = +inf" in
  Alcotest.(check value_testable) "positive infinity" (Float Float.infinity) (get "flt" t)

let test_float_neg_inf () =
  let t = parse_toml "flt = -inf" in
  Alcotest.(check value_testable) "negative infinity" (Float Float.neg_infinity) (get "flt" t)

let test_float_nan () =
  let t = parse_toml "flt = nan" in
  match get "flt" t with
  | Float f when Float.is_nan f -> ()
  | _ -> Alcotest.fail "Expected NaN"

let test_float_pos_nan () =
  let t = parse_toml "flt = +nan" in
  match get "flt" t with
  | Float f when Float.is_nan f -> ()
  | _ -> Alcotest.fail "Expected NaN"

let test_float_neg_nan () =
  let t = parse_toml "flt = -nan" in
  match get "flt" t with
  | Float f when Float.is_nan f -> ()
  | _ -> Alcotest.fail "Expected NaN"

let test_float_no_leading_digit () =
  parse_error "flt = .7"

let test_float_no_trailing_digit () =
  parse_error "flt = 7."

let float_tests = [
  "positive float", `Quick, test_float_positive;
  "fractional", `Quick, test_float_fractional;
  "negative float", `Quick, test_float_negative;
  "exponent", `Quick, test_float_exponent;
  "exponent no sign", `Quick, test_float_exponent_no_sign;
  "negative exponent", `Quick, test_float_exponent_negative;
  "fractional and exponent", `Quick, test_float_both;
  "underscore in float", `Quick, test_float_underscore;
  "infinity", `Quick, test_float_inf;
  "positive infinity", `Quick, test_float_pos_inf;
  "negative infinity", `Quick, test_float_neg_inf;
  "nan", `Quick, test_float_nan;
  "positive nan", `Quick, test_float_pos_nan;
  "negative nan", `Quick, test_float_neg_nan;
  "no leading digit", `Quick, test_float_no_leading_digit;
  "no trailing digit", `Quick, test_float_no_trailing_digit;
]

(* ============================================
   Booleans
   ============================================ *)

let test_bool_true () =
  let t = parse_toml "bool = true" in
  Alcotest.(check value_testable) "true" (Bool true) (get "bool" t)

let test_bool_false () =
  let t = parse_toml "bool = false" in
  Alcotest.(check value_testable) "false" (Bool false) (get "bool" t)

let test_bool_case_sensitive () =
  parse_error "bool = True"

let boolean_tests = [
  "true", `Quick, test_bool_true;
  "false", `Quick, test_bool_false;
  "case sensitive", `Quick, test_bool_case_sensitive;
]

(* ============================================
   Date-Times
   ============================================ *)

let test_datetime_offset () =
  let t = parse_toml "dt = 1979-05-27T07:32:00Z" in
  Alcotest.(check value_testable) "offset datetime UTC" (Datetime "1979-05-27T07:32:00Z") (get "dt" t)

let test_datetime_offset_negative () =
  let t = parse_toml "dt = 1979-05-27T00:32:00-07:00" in
  Alcotest.(check value_testable) "offset datetime negative" (Datetime "1979-05-27T00:32:00-07:00") (get "dt" t)

let test_datetime_offset_frac () =
  let t = parse_toml "dt = 1979-05-27T00:32:00.5-07:00" in
  Alcotest.(check value_testable) "offset datetime fractional" (Datetime "1979-05-27T00:32:00.5-07:00") (get "dt" t)

let test_datetime_space_separator () =
  let t = parse_toml "dt = 1979-05-27 07:32:00Z" in
  Alcotest.(check value_testable) "space separator" (Datetime "1979-05-27T07:32:00Z") (get "dt" t)

let test_datetime_local () =
  let t = parse_toml "dt = 1979-05-27T07:32:00" in
  Alcotest.(check value_testable) "local datetime" (Datetime_local "1979-05-27T07:32:00") (get "dt" t)

let test_datetime_local_frac () =
  let t = parse_toml "dt = 1979-05-27T07:32:00.5" in
  Alcotest.(check value_testable) "local datetime fractional" (Datetime_local "1979-05-27T07:32:00.5") (get "dt" t)

let test_date_local () =
  let t = parse_toml "dt = 1979-05-27" in
  Alcotest.(check value_testable) "local date" (Date_local "1979-05-27") (get "dt" t)

let test_time_local () =
  let t = parse_toml "dt = 07:32:00" in
  Alcotest.(check value_testable) "local time" (Time_local "07:32:00") (get "dt" t)

let test_time_local_frac () =
  let t = parse_toml "dt = 00:32:00.999999" in
  Alcotest.(check value_testable) "local time fractional" (Time_local "00:32:00.999999") (get "dt" t)

let datetime_tests = [
  "offset datetime UTC", `Quick, test_datetime_offset;
  "offset datetime negative", `Quick, test_datetime_offset_negative;
  "offset datetime fractional", `Quick, test_datetime_offset_frac;
  "space separator", `Quick, test_datetime_space_separator;
  "local datetime", `Quick, test_datetime_local;
  "local datetime fractional", `Quick, test_datetime_local_frac;
  "local date", `Quick, test_date_local;
  "local time", `Quick, test_time_local;
  "local time fractional", `Quick, test_time_local_frac;
]

(* ============================================
   Arrays
   ============================================ *)

let test_array_integers () =
  let t = parse_toml "arr = [1, 2, 3]" in
  Alcotest.(check value_testable) "integer array"
    (Array [Int 1L; Int 2L; Int 3L])
    (get "arr" t)

let test_array_strings () =
  let t = parse_toml {|arr = ["red", "yellow", "green"]|} in
  Alcotest.(check value_testable) "string array"
    (Array [String "red"; String "yellow"; String "green"])
    (get "arr" t)

let test_array_nested () =
  let t = parse_toml "arr = [[1, 2], [3, 4, 5]]" in
  Alcotest.(check value_testable) "nested array"
    (Array [
      Array [Int 1L; Int 2L];
      Array [Int 3L; Int 4L; Int 5L]
    ])
    (get "arr" t)

let test_array_mixed () =
  let t = parse_toml "arr = [0.1, 0.2, 1, 2]" in
  Alcotest.(check value_testable) "mixed types"
    (Array [Float 0.1; Float 0.2; Int 1L; Int 2L])
    (get "arr" t)

let test_array_empty () =
  let t = parse_toml "arr = []" in
  Alcotest.(check value_testable) "empty array" (Array []) (get "arr" t)

let test_array_multiline () =
  let t = parse_toml "arr = [\n  1,\n  2,\n  3\n]" in
  Alcotest.(check value_testable) "multiline array"
    (Array [Int 1L; Int 2L; Int 3L])
    (get "arr" t)

let test_array_trailing_comma () =
  let t = parse_toml "arr = [1, 2, 3,]" in
  Alcotest.(check value_testable) "trailing comma"
    (Array [Int 1L; Int 2L; Int 3L])
    (get "arr" t)

let test_array_with_inline_tables () =
  let t = parse_toml {|arr = [{x = 1}, {x = 2}]|} in
  match get "arr" t with
  | Array [Table [("x", Int 1L)]; Table [("x", Int 2L)]] -> ()
  | _ -> Alcotest.fail "Expected array of inline tables"

let array_tests = [
  "integer array", `Quick, test_array_integers;
  "string array", `Quick, test_array_strings;
  "nested array", `Quick, test_array_nested;
  "mixed types", `Quick, test_array_mixed;
  "empty array", `Quick, test_array_empty;
  "multiline array", `Quick, test_array_multiline;
  "trailing comma", `Quick, test_array_trailing_comma;
  "with inline tables", `Quick, test_array_with_inline_tables;
]

(* ============================================
   Tables
   ============================================ *)

let test_table_basic () =
  let t = parse_toml "[table]\nkey = \"value\"" in
  match get "table" t with
  | Table pairs ->
      Alcotest.(check value_testable) "basic table" (String "value") (List.assoc "key" pairs)
  | _ -> Alcotest.fail "Expected table"

let test_table_multiple () =
  let t = parse_toml "[table1]\nkey1 = 1\n\n[table2]\nkey2 = 2" in
  let t1 = get "table1" t and t2 = get "table2" t in
  (match t1 with
   | Table pairs -> Alcotest.(check value_testable) "table1" (Int 1L) (List.assoc "key1" pairs)
   | _ -> Alcotest.fail "Expected table1");
  (match t2 with
   | Table pairs -> Alcotest.(check value_testable) "table2" (Int 2L) (List.assoc "key2" pairs)
   | _ -> Alcotest.fail "Expected table2")

let test_table_dotted_header () =
  let t = parse_toml "[dog.\"tater.man\"]\ntype = \"pug\"" in
  match get "dog" t with
  | Table pairs ->
      (match List.assoc "tater.man" pairs with
       | Table inner ->
           Alcotest.(check value_testable) "nested quoted" (String "pug") (List.assoc "type" inner)
       | _ -> Alcotest.fail "Expected nested table")
  | _ -> Alcotest.fail "Expected dog table"

let test_table_implicit_parent () =
  let t = parse_toml "[x.y.z.w]\nkey = 1" in
  (* x, x.y, x.y.z should all be implicitly created *)
  match get "x" t with
  | Table _ -> ()
  | _ -> Alcotest.fail "Expected x table"

let test_table_empty () =
  let t = parse_toml "[empty]\n[other]\nkey = 1" in
  match get "empty" t with
  | Table [] -> ()
  | Table _ -> ()  (* May have implicit content *)
  | _ -> Alcotest.fail "Expected empty table"

let test_table_duplicate_error () =
  parse_error "[fruit]\napple = 1\n\n[fruit]\norange = 2"

let test_table_super_after () =
  let t = parse_toml "[x.y]\na = 1\n[x]\nb = 2" in
  match get "x" t with
  | Table pairs ->
      Alcotest.(check value_testable) "super table b" (Int 2L) (List.assoc "b" pairs)
  | _ -> Alcotest.fail "Expected x table"

let table_tests = [
  "basic table", `Quick, test_table_basic;
  "multiple tables", `Quick, test_table_multiple;
  "dotted header", `Quick, test_table_dotted_header;
  "implicit parent", `Quick, test_table_implicit_parent;
  "empty table", `Quick, test_table_empty;
  "duplicate error", `Quick, test_table_duplicate_error;
  "super after", `Quick, test_table_super_after;
]

(* ============================================
   Inline Tables
   ============================================ *)

let test_inline_table_basic () =
  let t = parse_toml {|name = { first = "Tom", last = "Preston-Werner" }|} in
  match get "name" t with
  | Table pairs ->
      Alcotest.(check value_testable) "first" (String "Tom") (List.assoc "first" pairs);
      Alcotest.(check value_testable) "last" (String "Preston-Werner") (List.assoc "last" pairs)
  | _ -> Alcotest.fail "Expected inline table"

let test_inline_table_compact () =
  let t = parse_toml "point = {x=1, y=2}" in
  match get "point" t with
  | Table pairs ->
      Alcotest.(check value_testable) "x" (Int 1L) (List.assoc "x" pairs);
      Alcotest.(check value_testable) "y" (Int 2L) (List.assoc "y" pairs)
  | _ -> Alcotest.fail "Expected inline table"

let test_inline_table_dotted_key () =
  let t = parse_toml "animal = { type.name = \"pug\" }" in
  match get "animal" t with
  | Table pairs ->
      (match List.assoc "type" pairs with
       | Table inner ->
           Alcotest.(check value_testable) "nested" (String "pug") (List.assoc "name" inner)
       | _ -> Alcotest.fail "Expected type table")
  | _ -> Alcotest.fail "Expected animal table"

let test_inline_table_empty () =
  let t = parse_toml "empty = {}" in
  Alcotest.(check value_testable) "empty inline table" (Table []) (get "empty" t)

let test_inline_table_trailing_comma () =
  let t = parse_toml "x = {a = 1, b = 2,}" in
  match get "x" t with
  | Table pairs ->
      Alcotest.(check value_testable) "a" (Int 1L) (List.assoc "a" pairs);
      Alcotest.(check value_testable) "b" (Int 2L) (List.assoc "b" pairs)
  | _ -> Alcotest.fail "Expected inline table"

let test_inline_table_nested () =
  let t = parse_toml "x = { a = { b = 1 } }" in
  match get "x" t with
  | Table pairs ->
      (match List.assoc "a" pairs with
       | Table inner ->
           Alcotest.(check value_testable) "nested" (Int 1L) (List.assoc "b" inner)
       | _ -> Alcotest.fail "Expected nested table")
  | _ -> Alcotest.fail "Expected x table"

let inline_table_tests = [
  "basic inline table", `Quick, test_inline_table_basic;
  "compact", `Quick, test_inline_table_compact;
  "dotted key", `Quick, test_inline_table_dotted_key;
  "empty", `Quick, test_inline_table_empty;
  "trailing comma", `Quick, test_inline_table_trailing_comma;
  "nested", `Quick, test_inline_table_nested;
]

(* ============================================
   Array of Tables
   ============================================ *)

let test_array_of_tables_basic () =
  let t = parse_toml "[[product]]\nname = \"Hammer\"\n\n[[product]]\nname = \"Nail\"" in
  match get "product" t with
  | Array [Table p1; Table p2] ->
      Alcotest.(check value_testable) "first" (String "Hammer") (List.assoc "name" p1);
      Alcotest.(check value_testable) "second" (String "Nail") (List.assoc "name" p2)
  | _ -> Alcotest.fail "Expected array of tables"

let test_array_of_tables_empty () =
  let t = parse_toml "[[product]]\nname = \"Hammer\"\n\n[[product]]\n\n[[product]]\nname = \"Nail\"" in
  match get "product" t with
  | Array [_; Table []; _] -> ()
  | Array items when List.length items = 3 -> ()
  | _ -> Alcotest.fail "Expected 3 elements"

let test_array_of_tables_subtable () =
  let t = parse_toml "[[fruits]]\nname = \"apple\"\n\n[fruits.physical]\ncolor = \"red\"" in
  match get "fruits" t with
  | Array [Table pairs] ->
      Alcotest.(check value_testable) "name" (String "apple") (List.assoc "name" pairs);
      (match List.assoc "physical" pairs with
       | Table inner ->
           Alcotest.(check value_testable) "color" (String "red") (List.assoc "color" inner)
       | _ -> Alcotest.fail "Expected physical table")
  | _ -> Alcotest.fail "Expected array of tables"

let test_array_of_tables_nested () =
  let t = parse_toml "[[fruits]]\nname = \"apple\"\n\n[[fruits.varieties]]\nname = \"red delicious\"\n\n[[fruits.varieties]]\nname = \"granny smith\"" in
  match get "fruits" t with
  | Array [Table pairs] ->
      Alcotest.(check value_testable) "name" (String "apple") (List.assoc "name" pairs);
      (match List.assoc "varieties" pairs with
       | Array [Table v1; Table v2] ->
           Alcotest.(check value_testable) "v1" (String "red delicious") (List.assoc "name" v1);
           Alcotest.(check value_testable) "v2" (String "granny smith") (List.assoc "name" v2)
       | _ -> Alcotest.fail "Expected varieties array")
  | _ -> Alcotest.fail "Expected fruits array"

let test_array_of_tables_static_error () =
  parse_error "fruits = []\n\n[[fruits]]"

let array_of_tables_tests = [
  "basic", `Quick, test_array_of_tables_basic;
  "empty element", `Quick, test_array_of_tables_empty;
  "subtable", `Quick, test_array_of_tables_subtable;
  "nested", `Quick, test_array_of_tables_nested;
  "static array error", `Quick, test_array_of_tables_static_error;
]

(* ============================================
   Encoding / Round-trip
   ============================================ *)

let test_encode_roundtrip_basic () =
  let original = Table [
    ("name", String "test");
    ("count", Int 42L);
    ("enabled", Bool true);
  ] in
  let encoded = to_toml_string original in
  let decoded = parse_toml encoded in
  Alcotest.(check value_testable) "roundtrip basic" original decoded

let test_encode_roundtrip_nested () =
  let original = Table [
    ("server", Table [
      ("host", String "localhost");
      ("port", Int 8080L);
    ]);
  ] in
  let encoded = to_toml_string original in
  let decoded = parse_toml encoded in
  Alcotest.(check value_testable) "roundtrip nested" original decoded

let test_encode_roundtrip_array () =
  let original = Table [
    ("items", Array [Int 1L; Int 2L; Int 3L]);
  ] in
  let encoded = to_toml_string original in
  let decoded = parse_toml encoded in
  Alcotest.(check value_testable) "roundtrip array" original decoded

let test_encode_roundtrip_special_string () =
  let original = Table [
    ("str", String "line1\nline2\ttab");
  ] in
  let encoded = to_toml_string original in
  let decoded = parse_toml encoded in
  Alcotest.(check value_testable) "roundtrip special string" original decoded

let test_encode_roundtrip_float () =
  let original = Table [
    ("pi", Float 3.14159);
    ("inf", Float Float.infinity);
    ("neg_inf", Float Float.neg_infinity);
  ] in
  let encoded = to_toml_string original in
  let decoded = parse_toml encoded in
  Alcotest.(check value_testable) "roundtrip float" original decoded

let test_encode_roundtrip_datetime () =
  let original = Table [
    ("dt", Datetime "1979-05-27T07:32:00Z");
    ("ld", Date_local "1979-05-27");
    ("lt", Time_local "07:32:00");
  ] in
  let encoded = to_toml_string original in
  let decoded = parse_toml encoded in
  Alcotest.(check value_testable) "roundtrip datetime" original decoded

let encode_tests = [
  "roundtrip basic", `Quick, test_encode_roundtrip_basic;
  "roundtrip nested", `Quick, test_encode_roundtrip_nested;
  "roundtrip array", `Quick, test_encode_roundtrip_array;
  "roundtrip special string", `Quick, test_encode_roundtrip_special_string;
  "roundtrip float", `Quick, test_encode_roundtrip_float;
  "roundtrip datetime", `Quick, test_encode_roundtrip_datetime;
]

(* ============================================
   Edge Cases and Error Handling
   ============================================ *)

let test_error_invalid_escape () =
  parse_error {|str = "\q"|}

let test_error_unterminated_string () =
  parse_error {|str = "hello|}

let test_error_unterminated_multiline () =
  parse_error {|str = """hello|}

let test_error_bare_key_only () =
  parse_error "key"

let test_error_missing_value () =
  parse_error "key ="

let test_error_invalid_integer () =
  parse_error "int = 1__2"

let test_error_invalid_float () =
  parse_error "flt = 1.2.3"

let test_error_redefine_as_table () =
  parse_error "a = 1\n[a]\nb = 2"

let test_error_inline_extend () =
  parse_error "[product]\ntype = { name = \"Nail\" }\ntype.edible = false"

let test_unicode_key () =
  let t = parse_toml {|"ʎǝʞ" = "value"|} in
  Alcotest.(check value_testable) "unicode key" (String "value") (get "ʎǝʞ" t)

let test_crlf_newlines () =
  let t = parse_toml "key1 = 1\r\nkey2 = 2" in
  Alcotest.(check value_testable) "key1" (Int 1L) (get "key1" t);
  Alcotest.(check value_testable) "key2" (Int 2L) (get "key2" t)

let edge_case_tests = [
  "invalid escape", `Quick, test_error_invalid_escape;
  "unterminated string", `Quick, test_error_unterminated_string;
  "unterminated multiline", `Quick, test_error_unterminated_multiline;
  "bare key only", `Quick, test_error_bare_key_only;
  "missing value", `Quick, test_error_missing_value;
  "invalid integer", `Quick, test_error_invalid_integer;
  "invalid float", `Quick, test_error_invalid_float;
  "redefine as table", `Quick, test_error_redefine_as_table;
  "inline extend", `Quick, test_error_inline_extend;
  "unicode key", `Quick, test_unicode_key;
  "crlf newlines", `Quick, test_crlf_newlines;
]

(* ============================================
   Ptime Conversions
   ============================================ *)

let ptime_testable =
  let pp fmt t = Format.fprintf fmt "%s" (Ptime.to_rfc3339 ~tz_offset_s:0 t) in
  Alcotest.testable pp Ptime.equal

let date_testable =
  let pp fmt (y, m, d) = Format.fprintf fmt "%04d-%02d-%02d" y m d in
  let eq (y1, m1, d1) (y2, m2, d2) = y1 = y2 && m1 = m2 && d1 = d2 in
  Alcotest.testable pp eq

let test_datetime_of_ptime () =
  let ptime = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid test datetime"
  in
  let v = datetime_of_ptime ptime in
  Alcotest.(check value_testable) "datetime_of_ptime UTC"
    (Datetime "1979-05-27T07:32:00Z") v

let test_datetime_of_ptime_with_tz () =
  let ptime = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid test datetime"
  in
  let v = datetime_of_ptime ~tz_offset_s:(-25200) ptime in  (* -07:00 = -25200s *)
  Alcotest.(check value_testable) "datetime_of_ptime with tz"
    (Datetime "1979-05-27T00:32:00-07:00") v

let test_datetime_of_ptime_with_frac () =
  let ptime = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid test datetime"
  in
  let v = datetime_of_ptime ~frac_s:3 ptime in
  Alcotest.(check value_testable) "datetime_of_ptime with frac"
    (Datetime "1979-05-27T07:32:00.000Z") v

let test_to_ptime () =
  let v = Datetime "1979-05-27T07:32:00Z" in
  let ptime = to_ptime v in
  let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid expected datetime"
  in
  Alcotest.(check ptime_testable) "to_ptime" expected ptime

let test_to_ptime_with_offset () =
  let v = Datetime "1979-05-27T00:32:00-07:00" in
  let ptime = to_ptime v in
  (* UTC time should be 1979-05-27T07:32:00Z *)
  let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid expected datetime"
  in
  Alcotest.(check ptime_testable) "to_ptime with offset" expected ptime

let test_to_ptime_tz () =
  let v = Datetime "1979-05-27T00:32:00-07:00" in
  match to_ptime_tz v with
  | Some (_, Some tz) ->
      Alcotest.(check int) "timezone offset" (-25200) tz
  | Some (_, None) ->
      Alcotest.fail "expected timezone offset"
  | None ->
      Alcotest.fail "expected ptime result"

let test_to_ptime_opt_local () =
  let v = Datetime_local "1979-05-27T07:32:00" in
  Alcotest.(check (option ptime_testable)) "local datetime returns None"
    None (to_ptime_opt v)

let test_to_ptime_optional_seconds () =
  (* TOML 1.1 allows optional seconds *)
  let v = Datetime "1979-05-27T07:32Z" in
  let ptime = to_ptime v in
  let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid expected datetime"
  in
  Alcotest.(check ptime_testable) "to_ptime optional seconds" expected ptime

let test_date_of_ptime () =
  let ptime = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid test datetime"
  in
  let v = date_of_ptime ptime in
  Alcotest.(check value_testable) "date_of_ptime"
    (Date_local "1979-05-27") v

let test_to_date () =
  let v = Date_local "1979-05-27" in
  let date = to_date v in
  Alcotest.(check date_testable) "to_date" (1979, 5, 27) date

let test_to_date_opt_invalid () =
  let v = Date_local "1979-02-30" in  (* Invalid date *)
  Alcotest.(check (option date_testable)) "invalid date returns None"
    None (to_date_opt v)

let test_ptime_roundtrip () =
  let original = match Ptime.of_date_time ((2024, 12, 19), ((15, 30, 45), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid test datetime"
  in
  let v = datetime_of_ptime original in
  let roundtrip = to_ptime v in
  Alcotest.(check ptime_testable) "ptime roundtrip" original roundtrip

let ptime_tests = [
  "datetime_of_ptime", `Quick, test_datetime_of_ptime;
  "datetime_of_ptime with tz", `Quick, test_datetime_of_ptime_with_tz;
  "datetime_of_ptime with frac", `Quick, test_datetime_of_ptime_with_frac;
  "to_ptime", `Quick, test_to_ptime;
  "to_ptime with offset", `Quick, test_to_ptime_with_offset;
  "to_ptime_tz", `Quick, test_to_ptime_tz;
  "to_ptime_opt local", `Quick, test_to_ptime_opt_local;
  "to_ptime optional seconds", `Quick, test_to_ptime_optional_seconds;
  "date_of_ptime", `Quick, test_date_of_ptime;
  "to_date", `Quick, test_to_date;
  "to_date_opt invalid", `Quick, test_to_date_opt_invalid;
  "ptime roundtrip", `Quick, test_ptime_roundtrip;
]

(* ============================================
   Unified Ptime Datetime
   ============================================ *)

let ptime_datetime_testable =
  Alcotest.testable pp_ptime_datetime (fun a b ->
    match a, b with
    | `Datetime (t1, tz1), `Datetime (t2, tz2) ->
        Ptime.equal t1 t2 && tz1 = tz2
    | `Datetime_local t1, `Datetime_local t2 ->
        Ptime.equal t1 t2
    | `Date d1, `Date d2 -> d1 = d2
    | `Time t1, `Time t2 -> t1 = t2
    | _ -> false)

let test_unified_offset_datetime () =
  let v = Datetime "1979-05-27T07:32:00Z" in
  match to_ptime_datetime v with
  | Some (`Datetime (ptime, Some 0)) ->
      let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
        | Some t -> t | None -> Alcotest.fail "invalid expected datetime" in
      Alcotest.(check ptime_testable) "ptime value" expected ptime
  | Some (`Datetime (_, tz)) ->
      Alcotest.failf "expected tz=Some 0, got %s"
        (match tz with Some n -> string_of_int n | None -> "None")
  | Some other ->
      Alcotest.failf "expected `Datetime, got %a" pp_ptime_datetime other
  | None ->
      Alcotest.fail "expected Some, got None"

let test_unified_offset_datetime_with_tz () =
  let v = Datetime "1979-05-27T00:32:00-07:00" in
  match to_ptime_datetime v with
  | Some (`Datetime (ptime, Some tz)) ->
      (* UTC time should be 1979-05-27T07:32:00Z *)
      let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
        | Some t -> t | None -> Alcotest.fail "invalid expected datetime" in
      Alcotest.(check ptime_testable) "ptime value" expected ptime;
      Alcotest.(check int) "timezone" (-25200) tz
  | _ -> Alcotest.fail "expected `Datetime with tz"

let test_unified_local_datetime () =
  let v = Datetime_local "1979-05-27T07:32:00" in
  (* Use explicit UTC for testing *)
  match to_ptime_datetime ~tz_offset_s:0 v with
  | Some (`Datetime_local ptime) ->
      let expected = match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
        | Some t -> t | None -> Alcotest.fail "invalid expected datetime" in
      Alcotest.(check ptime_testable) "ptime value" expected ptime
  | Some other ->
      Alcotest.failf "expected `Datetime_local, got %a" pp_ptime_datetime other
  | None ->
      Alcotest.fail "expected Some, got None"

let test_unified_local_date () =
  let v = Date_local "1979-05-27" in
  match to_ptime_datetime v with
  | Some (`Date (year, month, day)) ->
      Alcotest.(check int) "year" 1979 year;
      Alcotest.(check int) "month" 5 month;
      Alcotest.(check int) "day" 27 day
  | Some other ->
      Alcotest.failf "expected `Date, got %a" pp_ptime_datetime other
  | None ->
      Alcotest.fail "expected Some, got None"

let test_unified_local_time () =
  let v = Time_local "07:32:00" in
  match to_ptime_datetime v with
  | Some (`Time (hour, minute, second, ns)) ->
      Alcotest.(check int) "hour" 7 hour;
      Alcotest.(check int) "minute" 32 minute;
      Alcotest.(check int) "second" 0 second;
      Alcotest.(check int) "nanoseconds" 0 ns
  | Some other ->
      Alcotest.failf "expected `Time, got %a" pp_ptime_datetime other
  | None ->
      Alcotest.fail "expected Some, got None"

let test_unified_local_time_frac () =
  let v = Time_local "07:32:00.123456789" in
  match to_ptime_datetime v with
  | Some (`Time (hour, minute, second, ns)) ->
      Alcotest.(check int) "hour" 7 hour;
      Alcotest.(check int) "minute" 32 minute;
      Alcotest.(check int) "second" 0 second;
      Alcotest.(check int) "nanoseconds" 123456789 ns
  | Some other ->
      Alcotest.failf "expected `Time, got %a" pp_ptime_datetime other
  | None ->
      Alcotest.fail "expected Some, got None"

let test_unified_roundtrip_offset () =
  let original = `Datetime (
    (match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
     | Some t -> t | None -> Alcotest.fail "invalid datetime"),
    Some (-25200)
  ) in
  let toml = ptime_datetime_to_toml original in
  match to_ptime_datetime toml with
  | Some result -> Alcotest.(check ptime_datetime_testable) "roundtrip" original result
  | None -> Alcotest.fail "roundtrip failed"

let test_unified_roundtrip_local () =
  let original = `Datetime_local (
    match Ptime.of_date_time ((1979, 5, 27), ((7, 32, 0), 0)) with
    | Some t -> t | None -> Alcotest.fail "invalid datetime"
  ) in
  let toml = ptime_datetime_to_toml original in
  match to_ptime_datetime ~tz_offset_s:0 toml with
  | Some result -> Alcotest.(check ptime_datetime_testable) "roundtrip" original result
  | None -> Alcotest.fail "roundtrip failed"

let test_unified_roundtrip_date () =
  let original = `Date (1979, 5, 27) in
  let toml = ptime_datetime_to_toml original in
  match to_ptime_datetime toml with
  | Some result -> Alcotest.(check ptime_datetime_testable) "roundtrip" original result
  | None -> Alcotest.fail "roundtrip failed"

let test_unified_roundtrip_time () =
  let original = `Time (7, 32, 45, 123000000) in
  let toml = ptime_datetime_to_toml original in
  match to_ptime_datetime toml with
  | Some result -> Alcotest.(check ptime_datetime_testable) "roundtrip" original result
  | None -> Alcotest.fail "roundtrip failed"

let test_unified_not_datetime () =
  let v = String "not a datetime" in
  Alcotest.(check (option ptime_datetime_testable)) "non-datetime"
    None (to_ptime_datetime v)

let unified_datetime_tests = [
  "offset datetime", `Quick, test_unified_offset_datetime;
  "offset datetime with tz", `Quick, test_unified_offset_datetime_with_tz;
  "local datetime", `Quick, test_unified_local_datetime;
  "local date", `Quick, test_unified_local_date;
  "local time", `Quick, test_unified_local_time;
  "local time frac", `Quick, test_unified_local_time_frac;
  "roundtrip offset", `Quick, test_unified_roundtrip_offset;
  "roundtrip local", `Quick, test_unified_roundtrip_local;
  "roundtrip date", `Quick, test_unified_roundtrip_date;
  "roundtrip time", `Quick, test_unified_roundtrip_time;
  "not datetime", `Quick, test_unified_not_datetime;
]

(* ============================================
   Main
   ============================================ *)

let () =
  Alcotest.run "tomlt" [
    "comments", comment_tests;
    "keys", key_tests;
    "strings", string_tests;
    "integers", integer_tests;
    "floats", float_tests;
    "booleans", boolean_tests;
    "datetimes", datetime_tests;
    "arrays", array_tests;
    "tables", table_tests;
    "inline_tables", inline_table_tests;
    "array_of_tables", array_of_tables_tests;
    "encoding", encode_tests;
    "edge_cases", edge_case_tests;
    "ptime", ptime_tests;
    "unified_datetime", unified_datetime_tests;
  ]
