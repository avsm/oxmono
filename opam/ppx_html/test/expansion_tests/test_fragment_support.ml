open! Core
open Test_utils

let%expect_test "Fragment support" =
  test {|<></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.Primitives.fragment []
    |}]
;;

let%expect_test "Fragment with attributes results in an error." =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|< key="foo" on_click=%{foo}></>|});
  [%expect
    {| ("Expected a valid HTML tag, but instead found whitespace. No whitespace is allowed here..  ") |}]
;;

let%expect_test "Fragment support - does not get confused when interpolated tags are \
                 also used"
  =
  test {|<><%{EXPR}><><div></div></></><%{EXPR2}></></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.Primitives.fragment
      [EXPR [Html_syntax.Node.Primitives.fragment [Html_syntax.Node.div []]];
      EXPR2 []]
    |}]
;;
