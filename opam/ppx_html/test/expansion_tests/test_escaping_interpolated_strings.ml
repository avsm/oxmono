open! Core
open Test_utils

let%expect_test "Misparsing closing parens" =
  (* NOTE: This is a regression test against a situation where the "}" ended up closing
     the intepolated Ocaml early... *)
  test {|<div>%{Vdom.Node.text "}"}</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [(Vdom.Node.text "}" : _)]
    |}]
;;

let%expect_test "Nested ppx_html inside of itself." =
  test {|<div>%{[%html {xx|<div></div>|xx}]}</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [([%html {xx|<div></div>|xx}] : _)]
    |}]
;;
