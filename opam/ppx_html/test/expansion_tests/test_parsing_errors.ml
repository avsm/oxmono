open! Core
open Test_utils

let%expect_test "Misparse - due to mismatch in tags" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {| <h1>cool</h2> |});
  [%expect {| ("Expected closing tag </h1>, but got </h2>.") |}]
;;

let%expect_test "Misparse - due to mismatch in tags" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {| <h1>cool</> |});
  [%expect {| ("Expected </h1>, but got an empty closing tag (</>).") |}]
;;

let%expect_test "No closing tag." =
  Expect_test_helpers_core.require_does_raise (fun () -> test {| <div> |});
  [%expect {| ("No closing tag, but expected one for 'div'") |}]
;;

let%expect_test "Misparsed OCaml expression" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|

  <div>

       %{

       let x = = = = = = = 1 in
       ()

       }


  </div>


  |});
  [%expect
    {| ("Failed to parse OCaml expression inside of HTML.\nFile \"_none_\", line 6, characters 15-16:\n                                                 Error: Syntax error\n") |}]
;;
