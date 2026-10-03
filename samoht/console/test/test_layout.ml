(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let check = Alcotest.(check string)

(* Two equal-height blocks join with a one-column gutter; the left column is
   padded to its widest line ("bb" -> 2) so the right column starts flush. *)
let test_basic () =
  check "two columns, default gutter" "a  1\nbb 2"
    (Layout.hcat [ "a\nbb"; "1\n2" ])

(* The default gutter is a single space. *)
let test_gutter () =
  check "custom gutter widens the gap" "a   b"
    (Layout.hcat ~gutter:3 [ "a"; "b" ])

let test_gutter_zero () =
  check "zero gutter butts columns together" "a1\nb2"
    (Layout.hcat ~gutter:0 [ "a\nb"; "1\n2" ])

(* A shorter right block is padded with blank lines at the bottom by default.
   Rows where the right cell is blank stop at the last non-empty column, so they
   carry no trailing spaces. *)
let test_ragged_top () =
  check "shorter block padded at the bottom" "a X\nb\nc"
    (Layout.hcat [ "a\nb\nc"; "X" ])

(* [`Bottom] aligns the shorter block against the bottom edge instead. *)
let test_ragged_bottom () =
  check "shorter block padded at the top" "a\nb\nc X"
    (Layout.hcat ~align:`Bottom [ "a\nb\nc"; "X" ])

(* ANSI escapes count as zero width, so the coloured cell is treated as 2 wide
   ("hi") and the gutter lands in the same place as for plain text -- the escape
   bytes do not push the right column out. *)
let test_ansi_width () =
  check "ANSI codes do not inflate the column" "\027[31mhi\027[0m 1\nx  2"
    (Layout.hcat [ "\027[31mhi\027[0m\nx"; "1\n2" ])

(* A middle block shorter than its neighbours still reserves its column width on
   the blank row, so the right column stays aligned. *)
let test_three_columns () =
  check "interior column keeps its width when blank" "a M 1\nb   2"
    (Layout.hcat [ "a\nb"; "M"; "1\n2" ])

let test_empty () = check "empty list is the empty string" "" (Layout.hcat [])

let test_single () =
  check "single block is returned unchanged" "a\nbb" (Layout.hcat [ "a\nbb" ])

(* Ragged-width left block: every left line is padded to the block's widest line
   (14) so the right column starts at the same screen column on every row. This
   is the whole reason to use hcat over a naive [a ^ " " ^ b], which would leave
   the right column jagged. *)
let test_ragged_width () =
  let expected = "wide line here A\n" ^ "x" ^ String.make 14 ' ' ^ "B" in
  check "narrow rows padded out to the block width" expected
    (Layout.hcat [ "wide line here\nx"; "A\nB" ])

(* hcat_anim samples each animated block at the current time and joins the
   frames, so the joined row changes as its inputs do. Here the left block
   widens from "a" to "bb" at t=1, and the static right block stays put. *)
let test_hcat_anim () =
  let left = Anim.v (fun ~elapsed -> if elapsed < 1. then "a" else "bb") in
  let right = Anim.const "Z" in
  let a = Layout.hcat_anim [ left; right ] in
  check "frame before the change" "a Z" (Anim.frame a ~elapsed:0.5);
  check "frame after the change" "bb Z" (Anim.frame a ~elapsed:1.5)

let test_vcat () =
  check "adjacent blocks" "a\nb\nc" (Layout.vcat [ "a\nb"; "c" ]);
  check "one blank gutter line" "a\n\nb" (Layout.vcat ~gutter:1 [ "a"; "b" ]);
  Alcotest.check_raises "negative gutter"
    (Invalid_argument "Console.Layout.vcat: negative gutter") (fun () ->
      ignore (Layout.vcat ~gutter:(-1) [ "a" ]))

let test_vcat_anim () =
  let top = Anim.v (fun ~elapsed -> if elapsed < 1. then "a" else "A") in
  let a = Layout.vcat_anim [ top; Anim.const "b" ] in
  check "first frame" "a\nb" (Anim.frame a ~elapsed:0.);
  check "later frame" "A\nb" (Anim.frame a ~elapsed:2.)

let suite =
  ( "layout",
    [
      Alcotest.test_case "basic" `Quick test_basic;
      Alcotest.test_case "gutter" `Quick test_gutter;
      Alcotest.test_case "gutter zero" `Quick test_gutter_zero;
      Alcotest.test_case "ragged top" `Quick test_ragged_top;
      Alcotest.test_case "ragged bottom" `Quick test_ragged_bottom;
      Alcotest.test_case "ansi width" `Quick test_ansi_width;
      Alcotest.test_case "three columns" `Quick test_three_columns;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "single" `Quick test_single;
      Alcotest.test_case "ragged width" `Quick test_ragged_width;
      Alcotest.test_case "hcat_anim" `Quick test_hcat_anim;
      Alcotest.test_case "vcat" `Quick test_vcat;
      Alcotest.test_case "vcat_anim" `Quick test_vcat_anim;
    ] )
