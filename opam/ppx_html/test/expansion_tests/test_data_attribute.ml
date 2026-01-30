open! Core
open Test_utils

let%expect_test "data-test attribute" =
  test {|<div data-test=foo></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
                 "foo" : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
    +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
    -|             "foo" : Virtual_dom.Vdom.Attr.t)] []
    +|            "foo"] []
    |}]
;;

let%expect_test "data-test attribute with interpolation" =
  test {|<div data-test=%{foo}></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
                 foo : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
    -|             foo : Virtual_dom.Vdom.Attr.t)] []
    +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
    +|            foo] []
    |}];
  test {|<div data-test="hi__%{foo}"></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
                 ([%string "hi__%{(foo)}"]) : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
    +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-test")[@merlin.focus ])
    -|             ([%string "hi__%{(foo)}"]) : Virtual_dom.Vdom.Attr.t)] []
    +|            ([%string "hi__%{(foo)}"])] []
    |}]
;;

let%expect_test "other kinds of data-* attributes" =
  test {|<div data-columns=foo></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.Primitives.create "data-columns")
                 [@merlin.focus ]) "foo" : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-columns")
    +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-columns")
    -|             [@merlin.focus ]) "foo" : Virtual_dom.Vdom.Attr.t)] []
    +|            [@merlin.focus ]) "foo"] []
    |}];
  test {|<div data-rows=foo></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.Primitives.create "data-rows")[@merlin.focus ])
                 "foo" : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-rows")[@merlin.focus ])
    +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-rows")[@merlin.focus ])
    -|             "foo" : Virtual_dom.Vdom.Attr.t)] []
    +|            "foo"] []
    |}]
;;

let%expect_test "other kinds of foo-* attributes" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<div foo-columns=foo></div>|});
  [%expect
    {| ("ppx_html attribute keys cannot have hyphens (with the exception of data-* attributes)") |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<div foo-test=foo></div>|});
  [%expect
    {| ("ppx_html attribute keys cannot have hyphens (with the exception of data-* attributes)") |}]
;;
