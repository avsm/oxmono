open! Core
open Test_utils

let%expect_test "Style" =
  test
    {|
    <div style="background-color: tomato"> </div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[([%css "background-color: tomato;"] : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.Primitives.text " "]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[([%css "background-color: tomato;"] : Virtual_dom.Vdom.Attr.t)]
    +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ])
    +|            "background-color: tomato"]
        [Html_syntax.Node.Primitives.text " "]
    |}]
;;

let%expect_test "Many styles" =
  test
    {|
    <div style="background-color: tomato; background-color: red"> </div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[([%css "background-color: tomato; background-color: red;"] :
             Virtual_dom.Vdom.Attr.t)] [Html_syntax.Node.Primitives.text " "]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[([%css "background-color: tomato; background-color: red;"] :
    -|         Virtual_dom.Vdom.Attr.t)] [Html_syntax.Node.Primitives.text " "]
    +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ])
    +|            "background-color: tomato; background-color: red"]
    +|  [Html_syntax.Node.Primitives.text " "]
    |}]
;;

let%expect_test "Interpolation within styles" =
  (* NOTE: We want to make ppx_css do this. *)
  test
    {|
    <div style="background-color: tomato; background-color: %{color}"> </div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[([%css "background-color: tomato; background-color: %{(color)};"] :
             Virtual_dom.Vdom.Attr.t)] [Html_syntax.Node.Primitives.text " "]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[([%css "background-color: tomato; background-color: %{(color)};"] :
    -|         Virtual_dom.Vdom.Attr.t)] [Html_syntax.Node.Primitives.text " "]
    +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ])
    +|            ([%string
    +|               "background-color: tomato; background-color: %{(color)}"])]
    +|  [Html_syntax.Node.Primitives.text " "]
    |}]
;;

let%expect_test "Whole-sale interpolation does not call ppx_css" =
  (* NOTE: We do not want to make ppx_css do this. *)
  test
    {|
    <div style=%{Css_gen.foo}> </div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.style Css_gen.foo : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.Primitives.text " "]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(Html_syntax.Attr.style Css_gen.foo : Virtual_dom.Vdom.Attr.t)]
    +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ]) Css_gen.foo]
        [Html_syntax.Node.Primitives.text " "]
    |}]
;;

let%expect_test "PPX CSS's interpolation syntax" =
  test
    {|
    <div style="background-color: %{color#Color}"> </div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[([%css "background-color: %{(color)#Color};"] : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.Primitives.text " "]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[([%css "background-color: %{(color)#Color};"] : Virtual_dom.Vdom.Attr.t)]
    +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ])
    +|            ([%string "background-color: %{(color)#Color}"])]
        [Html_syntax.Node.Primitives.text " "]
    |}]
;;
