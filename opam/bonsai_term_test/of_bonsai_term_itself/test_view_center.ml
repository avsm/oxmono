open! Core
open Bonsai_term

(* NOTE this test suite is an examplte test suite that demonstrates an off-by-one bug in
   [View.center ~within content] where the output width of [View.center] could be less
   than within.width if the parity of [View.width content - within.width] is odd. *)

let dimensions width height = { Dimensions.width; height }
let box width height = View.transparent_rectangle ~width ~height

let test ~(here : [%call_pos]) ~within content =
  let centered = View.center ~within content in
  let%tydi { width; height } = View.dimensions centered in
  Expect_test_helpers_core.require ~here (width >= within.width);
  Expect_test_helpers_core.require ~here (height >= within.height);
  Expect_test_helpers_core.require ~here (width >= View.width content);
  Expect_test_helpers_core.require ~here (height >= View.height content)
;;

let%expect_test "A couple of specific test cases that fail" =
  test ~within:(dimensions 2 2) (box 2 2);
  [%expect {| |}];
  test ~within:(dimensions 2 2) (box 1 2);
  [%expect {| |}];
  test ~within:(dimensions 2 2) (box 2 1);
  [%expect {| |}];
  test ~within:(dimensions 2 2) (box 1 1);
  [%expect {| |}]
;;

let%expect_test "Multiple test cases" =
  let boxes = [ box 10 10; box 11 11; box 1 1; box 1 2; box 2 1; box 0 0 ] in
  let dimensions =
    [ dimensions 1 1; dimensions 100 100; dimensions 20 21; dimensions 21 20 ]
  in
  let test_cases =
    let%map.List box = boxes
    and dimensions in
    ~content:box, ~within:dimensions
  in
  List.iter test_cases ~f:(fun (~content, ~within) -> test ~within content);
  [%expect {| |}]
;;
