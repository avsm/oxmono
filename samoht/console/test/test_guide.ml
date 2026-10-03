(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let check_parts ~branch ~last ~pipe ~space guide =
  Alcotest.(check string) "branch" branch (Guide.branch guide);
  Alcotest.(check string) "last" last (Guide.last guide);
  Alcotest.(check string) "pipe" pipe (Guide.pipe guide);
  Alcotest.(check string) "space" space (Guide.space guide)

let test_presets () =
  check_parts ~branch:"+-- " ~last:"+-- " ~pipe:"|   " ~space:"    " Guide.ascii;
  check_parts ~branch:"├── " ~last:"└── " ~pipe:"│   " ~space:"    "
    Guide.unicode

let test_v_preserves_arbitrary_glyphs () =
  let guide = Guide.v ~branch:"" ~last:"\000\n" ~pipe:"🧪" ~space:"\027[31m" in
  check_parts ~branch:"" ~last:"\000\n" ~pipe:"🧪" ~space:"\027[31m" guide

let test_equal_checks_every_part () =
  let base = Guide.v ~branch:"b" ~last:"l" ~pipe:"p" ~space:"s" in
  let same = Guide.v ~branch:"b" ~last:"l" ~pipe:"p" ~space:"s" in
  Alcotest.(check bool) "same parts" true (Guide.equal base same);
  List.iter
    (fun changed ->
      Alcotest.(check bool) "one changed part" false (Guide.equal base changed))
    [
      Guide.v ~branch:"B" ~last:"l" ~pipe:"p" ~space:"s";
      Guide.v ~branch:"b" ~last:"L" ~pipe:"p" ~space:"s";
      Guide.v ~branch:"b" ~last:"l" ~pipe:"P" ~space:"s";
      Guide.v ~branch:"b" ~last:"l" ~pipe:"p" ~space:"S";
    ]

let test_pp_escapes_control_text () =
  let guide = Guide.v ~branch:"\n" ~last:"\000" ~pipe:"|" ~space:" " in
  Alcotest.(check string)
    "diagnostic form"
    "{ branch = \"\\n\"; last = \"\\000\"; pipe = \"|\"; space = \" \" }"
    (Fmt.str "%a" Guide.pp guide)

let suite =
  ( "guide",
    [
      Alcotest.test_case "presets" `Quick test_presets;
      Alcotest.test_case "v preserves arbitrary glyphs" `Quick
        test_v_preserves_arbitrary_glyphs;
      Alcotest.test_case "equal checks every part" `Quick
        test_equal_checks_every_part;
      Alcotest.test_case "pp escapes control text" `Quick
        test_pp_escapes_control_text;
    ] )
