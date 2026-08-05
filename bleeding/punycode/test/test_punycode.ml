(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Comprehensive tests for Punycode (RFC 3492) implementation *)

open Alcotest
module Punycode = Punycode
module Punycode_idna = Punycode_idna

(* Helper to convert hex code points to Uchar array *)
let codepoints_of_hex_list hex_list =
  Array.of_list (List.map Uchar.of_int hex_list)

(* Helper to convert string to code points *)
let codepoints_of_string s =
  let acc = ref [] in
  let i = ref 0 in
  while !i < String.length s do
    let dec = String.get_utf_8_uchar s !i in
    acc := Uchar.utf_decode_uchar dec :: !acc;
    i := !i + Uchar.utf_decode_length dec
  done;
  Array.of_list (List.rev !acc)

(* Test result helper *)
let check_encode_ok expected input =
  try
    let result = Punycode.encode input in
    check string "encode" expected result
  with Punycode.Error e ->
    fail (Format.asprintf "encode failed: %a" Punycode.pp_error_reason e)

let check_decode_ok expected input =
  try
    let result = Punycode.decode input in
    let expected_arr = codepoints_of_hex_list expected in
    check int "length" (Array.length expected_arr) (Array.length result);
    Array.iteri
      (fun i u ->
        check int
          (Printf.sprintf "char %d" i)
          (Uchar.to_int expected_arr.(i))
          (Uchar.to_int u))
      result
  with Punycode.Error e ->
    fail (Format.asprintf "decode failed: %a" Punycode.pp_error_reason e)

let check_utf8_roundtrip s =
  try
    let encoded = Punycode.encode_utf8 s in
    let decoded = Punycode.decode_utf8 encoded in
    check string "roundtrip" s decoded
  with Punycode.Error e ->
    fail (Format.asprintf "roundtrip failed: %a" Punycode.pp_error_reason e)

(* RFC 3492 Section 7.1 Test Vectors *)

(* (A) Arabic (Egyptian) *)
let arabic_codepoints =
  [
    0x0644;
    0x064A;
    0x0647;
    0x0645;
    0x0627;
    0x0628;
    0x062A;
    0x0643;
    0x0644;
    0x0645;
    0x0648;
    0x0634;
    0x0639;
    0x0631;
    0x0628;
    0x064A;
    0x061F;
  ]

let arabic_punycode = "egbpdaj6bu4bxfgehfvwxn"

(* (B) Chinese (simplified) *)
let chinese_simplified_codepoints =
  [ 0x4ED6; 0x4EEC; 0x4E3A; 0x4EC0; 0x4E48; 0x4E0D; 0x8BF4; 0x4E2D; 0x6587 ]

let chinese_simplified_punycode = "ihqwcrb4cv8a8dqg056pqjye"

(* (C) Chinese (traditional) *)
let chinese_traditional_codepoints =
  [ 0x4ED6; 0x5011; 0x7232; 0x4EC0; 0x9EBD; 0x4E0D; 0x8AAA; 0x4E2D; 0x6587 ]

let chinese_traditional_punycode = "ihqwctvzc91f659drss3x8bo0yb"

(* (D) Czech *)
let czech_codepoints =
  [
    0x0050;
    0x0072;
    0x006F;
    0x010D;
    0x0070;
    0x0072;
    0x006F;
    0x0073;
    0x0074;
    0x011B;
    0x006E;
    0x0065;
    0x006D;
    0x006C;
    0x0075;
    0x0076;
    0x00ED;
    0x010D;
    0x0065;
    0x0073;
    0x006B;
    0x0079;
  ]

let czech_punycode = "Proprostnemluvesky-uyb24dma41a"

(* (E) Hebrew *)
let hebrew_codepoints =
  [
    0x05DC;
    0x05DE;
    0x05D4;
    0x05D4;
    0x05DD;
    0x05E4;
    0x05E9;
    0x05D5;
    0x05D8;
    0x05DC;
    0x05D0;
    0x05DE;
    0x05D3;
    0x05D1;
    0x05E8;
    0x05D9;
    0x05DD;
    0x05E2;
    0x05D1;
    0x05E8;
    0x05D9;
    0x05EA;
  ]

let hebrew_punycode = "4dbcagdahymbxekheh6e0a7fei0b"

(* (F) Hindi (Devanagari) *)
let hindi_codepoints =
  [
    0x092F;
    0x0939;
    0x0932;
    0x094B;
    0x0917;
    0x0939;
    0x093F;
    0x0928;
    0x094D;
    0x0926;
    0x0940;
    0x0915;
    0x094D;
    0x092F;
    0x094B;
    0x0902;
    0x0928;
    0x0939;
    0x0940;
    0x0902;
    0x092C;
    0x094B;
    0x0932;
    0x0938;
    0x0915;
    0x0924;
    0x0947;
    0x0939;
    0x0948;
    0x0902;
  ]

let hindi_punycode = "i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd"

(* (G) Japanese (kanji and hiragana) *)
let japanese_codepoints =
  [
    0x306A;
    0x305C;
    0x307F;
    0x3093;
    0x306A;
    0x65E5;
    0x672C;
    0x8A9E;
    0x3092;
    0x8A71;
    0x3057;
    0x3066;
    0x304F;
    0x308C;
    0x306A;
    0x3044;
    0x306E;
    0x304B;
  ]

let japanese_punycode = "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa"

(* (H) Korean (Hangul syllables) *)
let korean_codepoints =
  [
    0xC138;
    0xACC4;
    0xC758;
    0xBAA8;
    0xB4E0;
    0xC0AC;
    0xB78C;
    0xB4E4;
    0xC774;
    0xD55C;
    0xAD6D;
    0xC5B4;
    0xB97C;
    0xC774;
    0xD574;
    0xD55C;
    0xB2E4;
    0xBA74;
    0xC5BC;
    0xB9C8;
    0xB098;
    0xC88B;
    0xC744;
    0xAE4C;
  ]

let korean_punycode =
  "989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5jpsd879ccm6fea98c"

(* (I) Russian (Cyrillic) *)
let russian_codepoints =
  [
    0x043F;
    0x043E;
    0x0447;
    0x0435;
    0x043C;
    0x0443;
    0x0436;
    0x0435;
    0x043E;
    0x043D;
    0x0438;
    0x043D;
    0x0435;
    0x0433;
    0x043E;
    0x0432;
    0x043E;
    0x0440;
    0x044F;
    0x0442;
    0x043F;
    0x043E;
    0x0440;
    0x0443;
    0x0441;
    0x0441;
    0x043A;
    0x0438;
  ]

let russian_punycode = "b1abfaaepdrnnbgefbadotcwatmq2g4l"

(* (J) Spanish *)
let spanish_codepoints =
  [
    0x0050;
    0x006F;
    0x0072;
    0x0071;
    0x0075;
    0x00E9;
    0x006E;
    0x006F;
    0x0070;
    0x0075;
    0x0065;
    0x0064;
    0x0065;
    0x006E;
    0x0073;
    0x0069;
    0x006D;
    0x0070;
    0x006C;
    0x0065;
    0x006D;
    0x0065;
    0x006E;
    0x0074;
    0x0065;
    0x0068;
    0x0061;
    0x0062;
    0x006C;
    0x0061;
    0x0072;
    0x0065;
    0x006E;
    0x0045;
    0x0073;
    0x0070;
    0x0061;
    0x00F1;
    0x006F;
    0x006C;
  ]

let spanish_punycode = "PorqunopuedensimplementehablarenEspaol-fmd56a"

(* (K) Vietnamese *)
let vietnamese_codepoints =
  [
    0x0054;
    0x1EA1;
    0x0069;
    0x0073;
    0x0061;
    0x006F;
    0x0068;
    0x1ECD;
    0x006B;
    0x0068;
    0x00F4;
    0x006E;
    0x0067;
    0x0074;
    0x0068;
    0x1EC3;
    0x0063;
    0x0068;
    0x1EC9;
    0x006E;
    0x00F3;
    0x0069;
    0x0074;
    0x0069;
    0x1EBF;
    0x006E;
    0x0067;
    0x0056;
    0x0069;
    0x1EC7;
    0x0074;
  ]

let vietnamese_punycode = "TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g"

(* (L) 3年B組金八先生 - Japanese with ASCII *)
let example_l_codepoints =
  [ 0x0033; 0x5E74; 0x0042; 0x7D44; 0x91D1; 0x516B; 0x5148; 0x751F ]

let example_l_punycode = "3B-ww4c5e180e575a65lsy2b"

(* (M) 安室奈美恵-with-SUPER-MONKEYS *)
let example_m_codepoints =
  [
    0x5B89;
    0x5BA4;
    0x5948;
    0x7F8E;
    0x6075;
    0x002D;
    0x0077;
    0x0069;
    0x0074;
    0x0068;
    0x002D;
    0x0053;
    0x0055;
    0x0050;
    0x0045;
    0x0052;
    0x002D;
    0x004D;
    0x004F;
    0x004E;
    0x004B;
    0x0045;
    0x0059;
    0x0053;
  ]

let example_m_punycode = "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n"

(* (N) Hello-Another-Way-それぞれの場所 *)
let example_n_codepoints =
  [
    0x0048;
    0x0065;
    0x006C;
    0x006C;
    0x006F;
    0x002D;
    0x0041;
    0x006E;
    0x006F;
    0x0074;
    0x0068;
    0x0065;
    0x0072;
    0x002D;
    0x0057;
    0x0061;
    0x0079;
    0x002D;
    0x305D;
    0x308C;
    0x305E;
    0x308C;
    0x306E;
    0x5834;
    0x6240;
  ]

let example_n_punycode = "Hello-Another-Way--fc4qua05auwb3674vfr0b"

(* (O) ひとつ屋根の下2 *)
let example_o_codepoints =
  [ 0x3072; 0x3068; 0x3064; 0x5C4B; 0x6839; 0x306E; 0x4E0B; 0x0032 ]

let example_o_punycode = "2-u9tlzr9756bt3uc0v"

(* (P) MaijでKoiする5秒前 *)
let example_p_codepoints =
  [
    0x004D;
    0x0061;
    0x006A;
    0x0069;
    0x3067;
    0x004B;
    0x006F;
    0x0069;
    0x3059;
    0x308B;
    0x0035;
    0x79D2;
    0x524D;
  ]

let example_p_punycode = "MajiKoi5-783gue6qz075azm5e"

(* (Q) パフィーdeルンバ *)
let example_q_codepoints =
  [ 0x30D1; 0x30D5; 0x30A3; 0x30FC; 0x0064; 0x0065; 0x30EB; 0x30F3; 0x30D0 ]

let example_q_punycode = "de-jg4avhby1noc0d"

(* (R) そのスピードで *)
let example_r_codepoints =
  [ 0x305D; 0x306E; 0x30B9; 0x30D4; 0x30FC; 0x30C9; 0x3067 ]

let example_r_punycode = "d9juau41awczczp"

(* (S) -> $1.00 <- (pure ASCII) *)
let example_s_codepoints =
  [
    0x002D;
    0x003E;
    0x0020;
    0x0024;
    0x0031;
    0x002E;
    0x0030;
    0x0030;
    0x0020;
    0x003C;
    0x002D;
  ]

let example_s_punycode = "-> $1.00 <--"

(* Test functions *)

let test_decode_arabic () = check_decode_ok arabic_codepoints arabic_punycode

let test_decode_chinese_simplified () =
  check_decode_ok chinese_simplified_codepoints chinese_simplified_punycode

let test_decode_chinese_traditional () =
  check_decode_ok chinese_traditional_codepoints chinese_traditional_punycode

let test_decode_hebrew () = check_decode_ok hebrew_codepoints hebrew_punycode
let test_decode_hindi () = check_decode_ok hindi_codepoints hindi_punycode

let test_decode_japanese () =
  check_decode_ok japanese_codepoints japanese_punycode

let test_decode_korean () = check_decode_ok korean_codepoints korean_punycode

let test_decode_example_l () =
  check_decode_ok example_l_codepoints example_l_punycode

let test_decode_example_m () =
  check_decode_ok example_m_codepoints example_m_punycode

let test_decode_example_n () =
  check_decode_ok example_n_codepoints example_n_punycode

let test_decode_example_o () =
  check_decode_ok example_o_codepoints example_o_punycode

let test_decode_example_q () =
  check_decode_ok example_q_codepoints example_q_punycode

let test_decode_example_r () =
  check_decode_ok example_r_codepoints example_r_punycode

let test_decode_czech () = check_decode_ok czech_codepoints czech_punycode

let test_decode_russian () =
  check_decode_ok russian_codepoints (String.lowercase_ascii russian_punycode)

let test_decode_spanish () = check_decode_ok spanish_codepoints spanish_punycode

let test_decode_vietnamese () =
  check_decode_ok vietnamese_codepoints vietnamese_punycode

let test_decode_example_p () =
  check_decode_ok example_p_codepoints example_p_punycode

let test_decode_example_s () =
  check_decode_ok example_s_codepoints example_s_punycode

let test_encode_arabic () =
  check_encode_ok arabic_punycode (codepoints_of_hex_list arabic_codepoints)

let test_encode_chinese_simplified () =
  check_encode_ok chinese_simplified_punycode
    (codepoints_of_hex_list chinese_simplified_codepoints)

let test_encode_chinese_traditional () =
  check_encode_ok chinese_traditional_punycode
    (codepoints_of_hex_list chinese_traditional_codepoints)

let test_encode_hebrew () =
  check_encode_ok hebrew_punycode (codepoints_of_hex_list hebrew_codepoints)

let test_encode_hindi () =
  check_encode_ok hindi_punycode (codepoints_of_hex_list hindi_codepoints)

let test_encode_japanese () =
  check_encode_ok japanese_punycode (codepoints_of_hex_list japanese_codepoints)

let test_encode_korean () =
  check_encode_ok korean_punycode (codepoints_of_hex_list korean_codepoints)

let test_encode_example_l () =
  check_encode_ok
    (String.lowercase_ascii example_l_punycode)
    (codepoints_of_hex_list example_l_codepoints)

let test_encode_example_m () =
  check_encode_ok
    (String.lowercase_ascii example_m_punycode)
    (codepoints_of_hex_list example_m_codepoints)

let test_encode_example_n () =
  check_encode_ok
    (String.lowercase_ascii example_n_punycode)
    (codepoints_of_hex_list example_n_codepoints)

let test_encode_example_o () =
  check_encode_ok
    (String.lowercase_ascii example_o_punycode)
    (codepoints_of_hex_list example_o_codepoints)

let test_encode_example_q () =
  check_encode_ok example_q_punycode
    (codepoints_of_hex_list example_q_codepoints)

let test_encode_example_r () =
  check_encode_ok example_r_punycode
    (codepoints_of_hex_list example_r_codepoints)

(* UTF-8 roundtrip tests *)
let test_utf8_roundtrip_german () = check_utf8_roundtrip "münchen"
let test_utf8_roundtrip_chinese () = check_utf8_roundtrip "中文"
let test_utf8_roundtrip_japanese () = check_utf8_roundtrip "日本語"
let test_utf8_roundtrip_arabic () = check_utf8_roundtrip "العربية"
let test_utf8_roundtrip_russian () = check_utf8_roundtrip "русский"
let test_utf8_roundtrip_greek () = check_utf8_roundtrip "ελληνικά"
let test_utf8_roundtrip_korean () = check_utf8_roundtrip "한국어"
let test_utf8_roundtrip_emoji () = check_utf8_roundtrip "hello👋world"

(* Label encoding tests *)
let test_label_encode_ascii () =
  try
    let result = Punycode.encode_label "example" in
    check string "ascii passthrough" "example" result
  with Punycode.Error e ->
    fail (Format.asprintf "encode_label failed: %a" Punycode.pp_error_reason e)

let test_label_encode_german () =
  try
    let result = Punycode.encode_label "münchen" in
    check string "german label" "xn--mnchen-3ya" result
  with Punycode.Error e ->
    fail (Format.asprintf "encode_label failed: %a" Punycode.pp_error_reason e)

let test_label_decode_german () =
  try
    let result = Punycode.decode_label "xn--mnchen-3ya" in
    check string "german decode" "münchen" result
  with Punycode.Error e ->
    fail (Format.asprintf "decode_label failed: %a" Punycode.pp_error_reason e)

(* IDNA tests *)
let test_idna_to_ascii_simple () =
  try
    let result = Punycode_idna.to_ascii "münchen.example.com" in
    check string "idna to_ascii" "xn--mnchen-3ya.example.com" result
  with Punycode_idna.Error e ->
    fail (Format.asprintf "to_ascii failed: %a" Punycode_idna.pp_error_reason e)

let test_idna_to_unicode_simple () =
  try
    let result = Punycode_idna.to_unicode "xn--mnchen-3ya.example.com" in
    check string "idna to_unicode" "münchen.example.com" result
  with Punycode_idna.Error e ->
    fail (Format.asprintf "to_unicode failed: %a" Punycode_idna.pp_error_reason e)

let test_idna_roundtrip () =
  let original = "münchen.example.com" in
  try
    let ascii = Punycode_idna.to_ascii original in
    let unicode = Punycode_idna.to_unicode ascii in
    check string "idna roundtrip" original unicode
  with Punycode_idna.Error e ->
    fail (Format.asprintf "roundtrip failed: %a" Punycode_idna.pp_error_reason e)

let test_idna_all_ascii () =
  try
    let result = Punycode_idna.to_ascii "www.example.com" in
    check string "all ascii passthrough" "www.example.com" result
  with Punycode_idna.Error e ->
    fail (Format.asprintf "to_ascii failed: %a" Punycode_idna.pp_error_reason e)

let test_idna_mixed_labels () =
  try
    let result = Punycode_idna.to_ascii "日本語.example.com" in
    (* Check that result starts with xn-- and ends with .example.com *)
    check bool "has ace prefix" true (Punycode.has_ace_prefix result);
    check bool "ends with example.com" true
      (String.length result > 12
      && String.sub result (String.length result - 12) 12 = ".example.com")
  with Punycode_idna.Error e ->
    fail (Format.asprintf "to_ascii failed: %a" Punycode_idna.pp_error_reason e)

(* Case annotation tests *)
let test_case_annotation_decode () =
  (* RFC example: uppercase letters indicate case flags *)
  try
    let codepoints, case_flags =
      Punycode.decode_with_case "MajiKoi5-783gue6qz075azm5e"
    in
    check int "codepoints length"
      (List.length example_p_codepoints)
      (Array.length codepoints);
    check int "case_flags length" (Array.length codepoints)
      (Array.length case_flags);
    (* M should be uppercase *)
    check bool "M uppercase" true (case_flags.(0) = Punycode.Uppercase);
    (* a should be lowercase *)
    check bool "a lowercase" true (case_flags.(1) = Punycode.Lowercase)
  with Punycode.Error e ->
    fail (Format.asprintf "decode_with_case failed: %a" Punycode.pp_error_reason e)

let test_case_annotation_encode () =
  let codepoints = codepoints_of_hex_list [ 0x0061; 0x0062; 0x0063 ] in
  (* "abc" *)
  let case_flags =
    [| Punycode.Uppercase; Punycode.Lowercase; Punycode.Uppercase |]
  in
  try
    let result = Punycode.encode_with_case codepoints case_flags in
    (* Should encode as "AbC-" (basic code points with case annotation) *)
    check string "case encoded" "AbC-" result
  with Punycode.Error e ->
    fail (Format.asprintf "encode_with_case failed: %a" Punycode.pp_error_reason e)

(* Edge case tests *)
let test_empty_input () =
  try
    let result = Punycode.encode [||] in
    check string "empty encode" "" result
  with Punycode.Error _ -> fail "empty encode should succeed"

let test_empty_decode () =
  try
    let result = Punycode.decode "" in
    check int "empty decode length" 0 (Array.length result)
  with Punycode.Error _ -> fail "empty decode should succeed"

let test_pure_ascii () =
  let input = codepoints_of_string "hello" in
  try
    let result = Punycode.encode input in
    check string "pure ascii" "hello-" result
  with Punycode.Error e ->
    fail (Format.asprintf "encode failed: %a" Punycode.pp_error_reason e)

let test_invalid_digit () =
  try
    ignore (Punycode.decode "hello!");
    fail "should fail on invalid digit"
  with
  | Punycode.Error (Punycode.Invalid_digit _) -> ()
  | Punycode.Error e ->
      fail (Format.asprintf "wrong error type: %a" Punycode.pp_error_reason e)

let test_label_too_long () =
  let long_label = String.make 100 'a' in
  try
    ignore (Punycode.encode_label long_label);
    fail "should fail on long label"
  with
  | Punycode.Error (Punycode.Label_too_long _) -> ()
  | Punycode.Error e ->
      fail (Format.asprintf "wrong error type: %a" Punycode.pp_error_reason e)

let test_empty_label () =
  try
    ignore (Punycode.encode_label "");
    fail "should fail on empty label"
  with
  | Punycode.Error Punycode.Empty_label -> ()
  | Punycode.Error e ->
      fail (Format.asprintf "wrong error type: %a" Punycode.pp_error_reason e)

(* Validation tests *)
let test_is_basic () =
  check bool "space is basic" true (Punycode.is_basic (Uchar.of_int 0x20));
  check bool "A is basic" true (Punycode.is_basic (Uchar.of_int 0x41));
  check bool "DEL is basic" true (Punycode.is_basic (Uchar.of_int 0x7F));
  check bool "0x80 not basic" false (Punycode.is_basic (Uchar.of_int 0x80));
  check bool "ü not basic" false (Punycode.is_basic (Uchar.of_int 0xFC))

let test_is_ascii_string () =
  check bool "ascii string" true (Punycode.is_ascii_string "hello");
  check bool "non-ascii string" false (Punycode.is_ascii_string "héllo");
  check bool "empty string" true (Punycode.is_ascii_string "")

let test_has_ace_prefix () =
  check bool "has xn--" true (Punycode.has_ace_prefix "xn--mnchen-3ya");
  check bool "has XN--" true (Punycode.has_ace_prefix "XN--mnchen-3ya");
  check bool "no prefix" false (Punycode.has_ace_prefix "example");
  check bool "too short" false (Punycode.has_ace_prefix "xn-")

(* Test suites *)
let decode_tests =
  [
    ("Arabic", `Quick, test_decode_arabic);
    ("Chinese simplified", `Quick, test_decode_chinese_simplified);
    ("Chinese traditional", `Quick, test_decode_chinese_traditional);
    ("Czech", `Quick, test_decode_czech);
    ("Hebrew", `Quick, test_decode_hebrew);
    ("Hindi", `Quick, test_decode_hindi);
    ("Japanese", `Quick, test_decode_japanese);
    ("Korean", `Quick, test_decode_korean);
    ("Russian", `Quick, test_decode_russian);
    ("Spanish", `Quick, test_decode_spanish);
    ("Vietnamese", `Quick, test_decode_vietnamese);
    ("Example L (mixed)", `Quick, test_decode_example_l);
    ("Example M (mixed)", `Quick, test_decode_example_m);
    ("Example N (mixed)", `Quick, test_decode_example_n);
    ("Example O (mixed)", `Quick, test_decode_example_o);
    ("Example P (mixed)", `Quick, test_decode_example_p);
    ("Example Q (mixed)", `Quick, test_decode_example_q);
    ("Example R", `Quick, test_decode_example_r);
    ("Example S (ASCII)", `Quick, test_decode_example_s);
  ]

let encode_tests =
  [
    ("Arabic", `Quick, test_encode_arabic);
    ("Chinese simplified", `Quick, test_encode_chinese_simplified);
    ("Chinese traditional", `Quick, test_encode_chinese_traditional);
    ("Hebrew", `Quick, test_encode_hebrew);
    ("Hindi", `Quick, test_encode_hindi);
    ("Japanese", `Quick, test_encode_japanese);
    ("Korean", `Quick, test_encode_korean);
    ("Example L (mixed)", `Quick, test_encode_example_l);
    ("Example M (mixed)", `Quick, test_encode_example_m);
    ("Example N (mixed)", `Quick, test_encode_example_n);
    ("Example O (mixed)", `Quick, test_encode_example_o);
    ("Example Q (mixed)", `Quick, test_encode_example_q);
    ("Example R", `Quick, test_encode_example_r);
  ]

let utf8_tests =
  [
    ("German roundtrip", `Quick, test_utf8_roundtrip_german);
    ("Chinese roundtrip", `Quick, test_utf8_roundtrip_chinese);
    ("Japanese roundtrip", `Quick, test_utf8_roundtrip_japanese);
    ("Arabic roundtrip", `Quick, test_utf8_roundtrip_arabic);
    ("Russian roundtrip", `Quick, test_utf8_roundtrip_russian);
    ("Greek roundtrip", `Quick, test_utf8_roundtrip_greek);
    ("Korean roundtrip", `Quick, test_utf8_roundtrip_korean);
    ("Emoji roundtrip", `Quick, test_utf8_roundtrip_emoji);
  ]

let label_tests =
  [
    ("ASCII passthrough", `Quick, test_label_encode_ascii);
    ("German encode", `Quick, test_label_encode_german);
    ("German decode", `Quick, test_label_decode_german);
  ]

let idna_tests =
  [
    ("to_ascii simple", `Quick, test_idna_to_ascii_simple);
    ("to_unicode simple", `Quick, test_idna_to_unicode_simple);
    ("roundtrip", `Quick, test_idna_roundtrip);
    ("all ASCII", `Quick, test_idna_all_ascii);
    ("mixed labels", `Quick, test_idna_mixed_labels);
  ]

let case_tests =
  [
    ("decode with case", `Quick, test_case_annotation_decode);
    ("encode with case", `Quick, test_case_annotation_encode);
  ]

let edge_case_tests =
  [
    ("empty encode", `Quick, test_empty_input);
    ("empty decode", `Quick, test_empty_decode);
    ("pure ASCII", `Quick, test_pure_ascii);
    ("invalid digit", `Quick, test_invalid_digit);
    ("label too long", `Quick, test_label_too_long);
    ("empty label", `Quick, test_empty_label);
  ]

let validation_tests =
  [
    ("is_basic", `Quick, test_is_basic);
    ("is_ascii_string", `Quick, test_is_ascii_string);
    ("has_ace_prefix", `Quick, test_has_ace_prefix);
  ]

let () =
  run "Punycode"
    [
      ("decode RFC vectors", decode_tests);
      ("encode RFC vectors", encode_tests);
      ("UTF-8 roundtrip", utf8_tests);
      ("label operations", label_tests);
      ("IDNA operations", idna_tests);
      ("case annotation", case_tests);
      ("edge cases", edge_case_tests);
      ("validation", validation_tests);
    ]
