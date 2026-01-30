open! Core
open Test_utils

let%expect_test "Error message that we show when someone forgets to wrap nested \
                 arguments with parens"
  =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<Foo.f ~child1:<div></div>></>|});
  [%expect
    {| ("Error. Expected an OCaml interpolation (e.g. %{}) or HTML element here (e.g. (<></>)) (HINT: Did you forget to wrap the element in parentheses?)") |}]
;;
