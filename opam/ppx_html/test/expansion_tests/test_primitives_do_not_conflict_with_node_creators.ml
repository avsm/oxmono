open! Core
open Test_utils

let%expect_test "Primitives do not conflict with node creators in SVG" =
  test {|<text>I am a text primitive inside of a text tag</text>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.text
      [Html_syntax.Node.Primitives.text
         "I am a text primitive inside of a text tag"]
    |}]
;;
