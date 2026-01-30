open! Core
open Test_utils

let%expect_test "Parsing unparenthesized ocaml expression" =
  test {|<div>%{x : Vdom.Node.t}</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [((x : Vdom.Node.t) : _)]
    |}]
;;

let%expect_test "Parsing parenthesized ocaml expression" =
  test {|<div>%{(x : Vdom.Node.t)}</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [((x : Vdom.Node.t) : _)]
    |}]
;;
