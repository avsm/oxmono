(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_plain () =
  let t = Rule.v ~glyph:"-" ~width:5 () in
  Alcotest.(check string) "unlabelled" "-----" (Rule.to_string t)

let test_label () =
  let label = Span.styled Style.bold "Build" in
  Alcotest.(check string)
    "center" "-- Build --"
    (Rule.to_string (Rule.v ~glyph:"-" ~label ~width:11 ()));
  Alcotest.(check string)
    "left" "Build -----"
    (Rule.to_string (Rule.v ~glyph:"-" ~align:`Left ~label ~width:11 ()));
  Alcotest.(check string)
    "right" "----- Build"
    (Rule.to_string (Rule.v ~glyph:"-" ~align:`Right ~label ~width:11 ()))

let test_invalid () =
  Alcotest.check_raises "negative width"
    (Invalid_argument "Console.Rule.v: negative width") (fun () ->
      ignore (Rule.v ~width:(-1) ()));
  Alcotest.check_raises "wide glyph"
    (Invalid_argument "Console.Rule.v: glyph must occupy one cell") (fun () ->
      ignore (Rule.v ~glyph:"--" ~width:10 ()));
  Alcotest.check_raises "unsafe glyph"
    (Invalid_argument "Console.Rule.v: control character in glyph") (fun () ->
      ignore (Rule.v ~glyph:"\027" ~width:10 ()));
  Alcotest.check_raises "long label"
    (Invalid_argument "Console.Rule.v: label wider than rule") (fun () ->
      ignore (Rule.v ~label:(Span.text "long") ~width:3 ()))

let suite =
  ( "rule",
    [
      Alcotest.test_case "plain" `Quick test_plain;
      Alcotest.test_case "label" `Quick test_label;
      Alcotest.test_case "invalid" `Quick test_invalid;
    ] )
