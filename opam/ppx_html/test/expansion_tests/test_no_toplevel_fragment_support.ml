open! Core
open Test_utils

let%expect_test "Implicit top-level fragments are not supported" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|%{foo} %{foo}|});
  [%expect
    {| ("ppx_html expects to return a single html element, but found 2 top-level elements.") |}]
;;

let%expect_test "Implicit top-level fragments are not supported - multiple divs" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<div></div><div></div>|});
  [%expect
    {| ("ppx_html expects to return a single html element, but found 2 top-level elements.") |}]
;;
