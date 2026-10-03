(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_ascii () =
  let border = Border.ascii in
  Alcotest.(check string) "ascii top_left" "+" (Border.chars border).top_left

let test_unicode () =
  let border = Border.rounded in
  Alcotest.(check string) "rounded top_left" "╭" (Border.chars border).top_left

let suite =
  ( "border",
    [
      Alcotest.test_case "ascii" `Quick test_ascii;
      Alcotest.test_case "unicode" `Quick test_unicode;
    ] )
