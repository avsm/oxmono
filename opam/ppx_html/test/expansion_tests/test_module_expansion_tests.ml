open! Core
open Test_utils

let%expect_test "Module expansions - attribute value" =
  test
    {|<div width=%{1#Int}>
  </div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.width)[@merlin.focus ]) (Int.to_string 1) :
             Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.width)[@merlin.focus ]) (Int.to_string 1) :
    -|         Virtual_dom.Vdom.Attr.t)] []
    +|  ~attrs:[((Html_syntax.Attr.width)[@merlin.focus ]) (Int.to_string 1)] []
    |}]
;;

let%expect_test "Module expansions - node" =
  test
    {|<div>
    %{x#Foo}
  </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [(Html_syntax.Node.Primitives.text (Foo.to_string x) : _)]
    |}]
;;

let%expect_test "Module expansions - attribute" =
  test
    {|<div %{attr#Foo}>
  </div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div ~attrs:[(Foo.to_attr attr : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
    -|Html_syntax.Node.div ~attrs:[(Foo.to_attr attr : Virtual_dom.Vdom.Attr.t)] []
    +|Html_syntax.Node.div ~attrs:[Foo.to_attr attr] []
    |}]
;;
