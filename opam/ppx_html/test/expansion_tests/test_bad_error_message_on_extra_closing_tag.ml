open! Core
open Test_utils

let%expect_test "Extra top-level unopened HTML tags." =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<div></div></div>|});
  [%expect {| ("This closing tag was never opened.") |}]
;;
