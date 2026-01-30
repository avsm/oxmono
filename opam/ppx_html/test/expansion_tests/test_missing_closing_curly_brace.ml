open! Core
open Test_utils

let%expect_test "Test nice error message on missing curly brace" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|
    <div>

    %{ Vdom.Node.text "hi"

  |});
  [%expect {| ("Missing curly brace for interpolated OCaml") |}]
;;

let%expect_test "Test nice error message on missing brace with nesting" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {x|
    <div>

    %{ Vdom.Node.text [%string {| %{"hi"} |}]</div>

  |x});
  [%expect {| ("Missing curly brace for interpolated OCaml") |}]
;;
