open! Core
open Test_utils

let%expect_test "basic expansion" =
  test {|<div tailwind="bg-white"></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t)] []
    +|  ~attrs:[((Html_syntax.Attr.tailwind)[@merlin.focus ]) "bg-white"] []
    |}]
;;

let%expect_test "multiple classes" =
  test {|<div tailwind="bg-white bg-black foo bar baz"></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[([%tailwind "bg-white bg-black foo bar baz"] : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[([%tailwind "bg-white bg-black foo bar baz"] : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|  ~attrs:[((Html_syntax.Attr.tailwind)[@merlin.focus ])
    +|            "bg-white bg-black foo bar baz"] []
    |}]
;;

let%expect_test "multiple tailwind attrs" =
  test {|<div tailwind="bg-white" tailwind="bg-white foo bar baz"></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t);
             ([%tailwind "bg-white foo bar baz"] : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t);
    -|         ([%tailwind "bg-white foo bar baz"] : Virtual_dom.Vdom.Attr.t)] []
    +|  ~attrs:[((Html_syntax.Attr.tailwind)[@merlin.focus ]) "bg-white";
    +|         ((Html_syntax.Attr.tailwind)[@merlin.focus ]) "bg-white foo bar baz"]
    +|  []
    |}]
;;

let%expect_test "Invalid position" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<div tailwind=%{foo}></div>|});
  [%expect
    {| ("Error: Tailwind support for dynamic classes is not supported. Tailwind only supports string literals.") |}]
;;

let%expect_test "No quotes" =
  test {|<div tailwind=bg-white></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t)] []
    +|  ~attrs:[((Html_syntax.Attr.tailwind)[@merlin.focus ]) "bg-white"] []
    |}]
;;
