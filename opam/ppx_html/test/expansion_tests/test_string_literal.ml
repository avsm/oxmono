open! Core
open Test_utils

let%expect_test "Node interpolation with a string literal" =
  test {|<div>%{" "}</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [(Html_syntax.Node.Primitives.text ((" ")[@merlin.focus ]) : _)]
    |}];
  test {|<div> I am a %{" "}%{"string literal!"} </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text " I am a ";
      (Html_syntax.Node.Primitives.text ((" ")[@merlin.focus ]) : _);
      (Html_syntax.Node.Primitives.text (("string literal!")[@merlin.focus ]) :
      _);
      Html_syntax.Node.Primitives.text " "]
    |}]
;;

let%expect_test "The string literal interpolation only happens in a node context and not \
                 in the other contexts"
  =
  test {|<div %{"attr"} src=%{"attr value"}> <%{"tag value"}></> </div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[("attr" : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.src)[@merlin.focus ]) "attr value" : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.Primitives.text " ";
      "tag value" [];
      Html_syntax.Node.Primitives.text " "]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[("attr" : Virtual_dom.Vdom.Attr.t);
    -|         (((Html_syntax.Attr.src)[@merlin.focus ]) "attr value" : Virtual_dom.Vdom.Attr.t)]
    +|  ~attrs:["attr"; ((Html_syntax.Attr.src)[@merlin.focus ]) "attr value"]
        [Html_syntax.Node.Primitives.text " ";
        "tag value" [];
        Html_syntax.Node.Primitives.text " "]
    |}]
;;

let%expect_test "The interpolation does not happen if a modul is provided" =
  (* NOTE: this is weird. This is always a mistake, but at least this expansion will
     result in a nice error message. *)
  test {|<div> %{"constant"#Modul} </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text " ";
      (Html_syntax.Node.Primitives.text (Modul.to_string "constant") : _);
      Html_syntax.Node.Primitives.text " "]
    |}]
;;
