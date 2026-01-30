open! Core
open Test_utils

let%expect_test "Hello world!" =
  test {|<h1>Hello World!</h1>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.h1 [Html_syntax.Node.Primitives.text "Hello World!"]
    |}]
;;

let%expect_test "Nesting" =
  test
    {|
  <p> Capybaras are <strong>cool</strong></p>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.p
      [Html_syntax.Node.Primitives.text " Capybaras are ";
      Html_syntax.Node.strong [Html_syntax.Node.Primitives.text "cool"]]
    |}]
;;

let%expect_test "HTML Custom elements" =
  (* NOTE: This test only shows current behavior and is not necesarily a bug/something we
     should fix. A potential scenario is that for custom elements like there (i.e. ones
     that are kebab-case we could use Vdom.Node.create "custom-element" instead.). Also
     unsure if this should be supported as it's also supported via the interpolation
     syntax.

     https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_custom_elements *)
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|
    <custom-element></custom-element>
  |});
  [%expect {| ("Expected closing '>' to terminate element \"custom\", but found '-'") |}];
  (* This is a current possible workaround in this rare situation. *)
  test
    {|
    <%{Vdom.Node.create "custom-element"}></>
  |};
  [%expect
    {xxx|
    same output between ppx_html and ppx_html_kernel

    Vdom.Node.create "custom-element" []
    |xxx}]
;;

let%expect_test "Element tag interpolation" =
  test
    {|
    <%{EXPR}></>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    EXPR []
    |}];
  test
    {|
    <%{EXPR}/>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    EXPR ()
    |}]
;;

let%expect_test "Attributes" =
  test
    {|
    <h2 class_=menu-add-card-header height="20" on_click=%{fun _ -> Effect.print_s [%message "hello"]}></h2>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.h2
      ~attrs:[(((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-header" :
             Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.height)[@merlin.focus ]) "20" : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.on_click)[@merlin.focus ])
                (fun _ -> Effect.print_s ([%message "hello"])) : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.h2
    -|  ~attrs:[(((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-header" :
    -|         Virtual_dom.Vdom.Attr.t);
    -|         (((Html_syntax.Attr.height)[@merlin.focus ]) "20" : Virtual_dom.Vdom.Attr.t);
    -|         (((Html_syntax.Attr.on_click)[@merlin.focus ])
    +|  ~attrs:[((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-header";
    +|         ((Html_syntax.Attr.height)[@merlin.focus ]) "20";
    +|         ((Html_syntax.Attr.on_click)[@merlin.focus ])
    -|            (fun _ -> Effect.print_s ([%message "hello"])) : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|           (fun _ -> Effect.print_s ([%message "hello"]))] []
    |}];
  [%expect {| |}]
;;

let%expect_test "Attributes and element tag interpolation" =
  test
    {|
    <%{ELEMENT_EXPR} %{ATTRIBUTE_EXPR} foo=%{DUMMY_ATTR_EXPR}></>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    ELEMENT_EXPR
      ~attrs:[(ATTRIBUTE_EXPR : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.foo)[@merlin.focus ]) DUMMY_ATTR_EXPR :
             Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      ELEMENT_EXPR
    -|  ~attrs:[(ATTRIBUTE_EXPR : Virtual_dom.Vdom.Attr.t);
    +|  ~attrs:[ATTRIBUTE_EXPR;
    -|         (((Html_syntax.Attr.foo)[@merlin.focus ]) DUMMY_ATTR_EXPR :
    -|         Virtual_dom.Vdom.Attr.t)] []
    +|         ((Html_syntax.Attr.foo)[@merlin.focus ]) DUMMY_ATTR_EXPR] []
    |}]
;;

let%expect_test "Key-based attribute interpolation" =
  (* NOTE: This currently only demonstrates existing behavior of a feature we may want to
     support in the future. *)
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|
    <div %{Virtual_dom_svg.Attr.href}="google.com"></div>
  |});
  [%expect {| ("Expected closing '>' to terminate element \"div\", but found '='") |}];
  test
    {|
    <%{EXPR}></>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    EXPR []
    |}]
;;

let%expect_test "Empty node" =
  test {||};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.Primitives.none
    |}]
;;

let%expect_test "JSX fragment" =
  test {|<></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.Primitives.fragment []
    |}]
;;

let%expect_test "Interpolation with no parsing context" =
  test {|%{foo}|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    (foo : _)
    |}];
  (* NOTE: Wow! It using fragment implicitly here is really cool! *)
  Expect_test_helpers_core.require_does_raise (fun () -> test {|%{foo} %{bar}|});
  [%expect
    {| ("ppx_html expects to return a single html element, but found 2 top-level elements.") |}]
;;

let%expect_test "Many classes used at once" =
  (* NOTE: I think this is fine. My gut reaction was that it should probably use
     [Vdom.Attr.classes] instead, but I think that using [Vdom.Attr.class] is the same
     behavior.

     After double-double checking, it seems like it's not exactly 1:1 with what we do in
     [virtual_dom]. In virtual_dom, using [Virtual_dom.Attr.classes] attempts to combine
     the classes, which I think is maybe something we should attempt here, although I
     still probably need to think more about this.
  *)
  test {|<div class="foo bar baz"></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.classes ["foo"; "bar"; "baz"] : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(Html_syntax.Attr.classes ["foo"; "bar"; "baz"] : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|  ~attrs:[((Html_syntax.Attr.class_)[@merlin.focus ]) "foo bar baz"] []
    |}]
;;

let%expect_test "classes with substitutions" =
  (* NOTE: I think this is fine. My gut reaction was that it should probably use
     [Vdom.Attr.classes] instead, but I think that using [Vdom.Attr.class] is the same
     behavior.

     After double-double checking, it seems like it's not exactly 1:1 with what we do in
     [virtual_dom]. In virtual_dom, using [Virtual_dom.Attr.classes] attempts to combine
     the classes, which I think is maybe something we should attempt here, although I
     still probably need to think more about this.
  *)
  test {|<div class="foo-%{"bar"}-baz fizz %{"other"}"></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.classes
                 [[%string "foo-%{(\"bar\")}-baz"]; "fizz"; ("other" : string)] :
             Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(Html_syntax.Attr.classes
    -|             [[%string "foo-%{(\"bar\")}-baz"]; "fizz"; ("other" : string)] :
    -|         Virtual_dom.Vdom.Attr.t)] []
    +|  ~attrs:[((Html_syntax.Attr.class_)[@merlin.focus ])
    +|            ([%string "foo-%{(\"bar\")}-baz fizz %{(\"other\")}"])] []
    |}]
;;

let%expect_test "Complex-ish test case" =
  test
    {| <div class="menu-add-card" on_click=%{fun _ -> Effect.print_s [%message "capybaras are cool"] }>
          %{title}
          <span class_=pill class_=menu-add-card-verb>
            %{Vdom.Node.text verb}
          </span>
          <span class_=menu-add-card-text>
            %{Vdom.Node.text text}
          </span>
          %{help}
        </div>
        |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.classes ["menu-add-card"] : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.on_click)[@merlin.focus ])
                (fun _ -> Effect.print_s ([%message "capybaras are cool"])) :
             Virtual_dom.Vdom.Attr.t)]
      [(title : _);
      Html_syntax.Node.span
        ~attrs:[(((Html_syntax.Attr.class_)[@merlin.focus ]) "pill" : Virtual_dom.Vdom.Attr.t);
               (((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-verb" :
               Virtual_dom.Vdom.Attr.t)] [(Vdom.Node.text verb : _)];
      Html_syntax.Node.span
        ~attrs:[(((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-text" :
               Virtual_dom.Vdom.Attr.t)] [(Vdom.Node.text text : _)];
      (help : _)]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(Html_syntax.Attr.classes ["menu-add-card"] : Virtual_dom.Vdom.Attr.t);
    +|  ~attrs:[((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card";
    -|         (((Html_syntax.Attr.on_click)[@merlin.focus ])
    +|         ((Html_syntax.Attr.on_click)[@merlin.focus ])
    -|            (fun _ -> Effect.print_s ([%message "capybaras are cool"])) :
    -|         Virtual_dom.Vdom.Attr.t)]
    +|           (fun _ -> Effect.print_s ([%message "capybaras are cool"]))]
    -|  [(title : _);
    -|  Html_syntax.Node.span
    -|    ~attrs:[(((Html_syntax.Attr.class_)[@merlin.focus ]) "pill" : Virtual_dom.Vdom.Attr.t);
    +|  [(title : _);
    +|  Html_syntax.Node.span
    +|    ~attrs:[((Html_syntax.Attr.class_)[@merlin.focus ]) "pill";
    -|           (((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-verb" :
    -|           Virtual_dom.Vdom.Attr.t)] [(Vdom.Node.text verb : _)];
    +|           ((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-verb"]
    +|    [(Vdom.Node.text verb : _)];
    -|  Html_syntax.Node.span
    -|    ~attrs:[(((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-text" :
    -|           Virtual_dom.Vdom.Attr.t)] [(Vdom.Node.text text : _)];
    +|  Html_syntax.Node.span
    +|    ~attrs:[((Html_syntax.Attr.class_)[@merlin.focus ]) "menu-add-card-text"]
    +|    [(Vdom.Node.text text : _)];
        (help : _)]
    |}]
;;

let%expect_test "Attrs that are OCaml keywords are special cased" =
  test
    {|
    <div class="class" for="for" type="type" open
    ></div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.classes ["class"] : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.for_)[@merlin.focus ]) "for" : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.type_)[@merlin.focus ]) "type" : Virtual_dom.Vdom.Attr.t);
             (Html_syntax.Attr.open_ : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(Html_syntax.Attr.classes ["class"] : Virtual_dom.Vdom.Attr.t);
    +|  ~attrs:[((Html_syntax.Attr.class_)[@merlin.focus ]) "class";
    -|         (((Html_syntax.Attr.for_)[@merlin.focus ]) "for" : Virtual_dom.Vdom.Attr.t);
    +|         ((Html_syntax.Attr.for_)[@merlin.focus ]) "for";
    -|         (((Html_syntax.Attr.type_)[@merlin.focus ]) "type" : Virtual_dom.Vdom.Attr.t);
    -|         (Html_syntax.Attr.open_ : Virtual_dom.Vdom.Attr.t)] []
    +|         ((Html_syntax.Attr.type_)[@merlin.focus ]) "type";
    +|         Html_syntax.Attr.open_] []
    |}]
;;

let%expect_test "Nodes that are OCaml keywords are not handled." =
  (* NOTE: This one is doubly weird as the type signature does not expect a Vdom.Node.t
     list so maybe it shouldn't be specialcased as it won't quite work. *)
  test
    {|
    <lazy></lazy>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.lazy_ []
    |}]
;;

let%expect_test "Disabled attribute" =
  test
    {|
    <div disabled></div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.disabled : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
    -|Html_syntax.Node.div
    -|  ~attrs:[(Html_syntax.Attr.disabled : Virtual_dom.Vdom.Attr.t)] []
    +|Html_syntax.Node.div ~attrs:[Html_syntax.Attr.disabled] []
    |}]
;;

let%expect_test "className is not special handled" =
  test
    {|
    <div className="foo"></div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.className)[@merlin.focus ]) "foo" : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.className)[@merlin.focus ]) "foo" : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|  ~attrs:[((Html_syntax.Attr.className)[@merlin.focus ]) "foo"] []
    |}]
;;

let%expect_test "Duplicate attribute names." =
  test
    {|
    <div a="1" a="2"></div>
  |};
  (* How these are handled is deferred to the implementation of [?attrs] *)
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.a)[@merlin.focus ]) "1" : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.a)[@merlin.focus ]) "2" : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.a)[@merlin.focus ]) "1" : Virtual_dom.Vdom.Attr.t);
    -|         (((Html_syntax.Attr.a)[@merlin.focus ]) "2" : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|  ~attrs:[((Html_syntax.Attr.a)[@merlin.focus ]) "1";
    +|         ((Html_syntax.Attr.a)[@merlin.focus ]) "2"] []
    |}];
  test
    {|
    <div class="foo" class="bar"></div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.classes ["foo"] : Virtual_dom.Vdom.Attr.t);
             (Html_syntax.Attr.classes ["bar"] : Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(Html_syntax.Attr.classes ["foo"] : Virtual_dom.Vdom.Attr.t);
    -|         (Html_syntax.Attr.classes ["bar"] : Virtual_dom.Vdom.Attr.t)] []
    +|  ~attrs:[((Html_syntax.Attr.class_)[@merlin.focus ]) "foo";
    +|         ((Html_syntax.Attr.class_)[@merlin.focus ]) "bar"] []
    |}]
;;

let%expect_test "Escaping of attribute strings" =
  test
    {|
    <div no_quotes=1 with_quotes="2"></div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.no_quotes)[@merlin.focus ]) "1" : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.with_quotes)[@merlin.focus ]) "2" : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.no_quotes)[@merlin.focus ]) "1" : Virtual_dom.Vdom.Attr.t);
    +|  ~attrs:[((Html_syntax.Attr.no_quotes)[@merlin.focus ]) "1";
    -|         (((Html_syntax.Attr.with_quotes)[@merlin.focus ]) "2" : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|         ((Html_syntax.Attr.with_quotes)[@merlin.focus ]) "2"] []
    |}];
  (* This one handles escaped "\"" correctly. *)
  test
    {|
     <div two="\"\"" one="\""></div>
     |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.two)[@merlin.focus ]) "\"\"" : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.one)[@merlin.focus ]) "\"" : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.two)[@merlin.focus ]) "\"\"" : Virtual_dom.Vdom.Attr.t);
    -|         (((Html_syntax.Attr.one)[@merlin.focus ]) "\"" : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|  ~attrs:[((Html_syntax.Attr.two)[@merlin.focus ]) "\"\"";
    +|         ((Html_syntax.Attr.one)[@merlin.focus ]) "\""] []
    |}]
;;

let%expect_test "ppx_html inside of ppx_html" =
  test
    {|<div no_quotes=1 with_quotes="2"> %{[%html{x|<p>hello</p>|x}]}</div>
|};
  (* NOTE: This is a limitation of the test harness, and not the actual PPX. In ppx-land
     this test should fully expand, and can be tested in a different way. This test only
     runs a single invocation of the PPX. *)
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[(((Html_syntax.Attr.no_quotes)[@merlin.focus ]) "1" : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.with_quotes)[@merlin.focus ]) "2" : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.Primitives.text " "; ([%html {x|<p>hello</p>|x}] : _)]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
    -|  ~attrs:[(((Html_syntax.Attr.no_quotes)[@merlin.focus ]) "1" : Virtual_dom.Vdom.Attr.t);
    +|  ~attrs:[((Html_syntax.Attr.no_quotes)[@merlin.focus ]) "1";
    -|         (((Html_syntax.Attr.with_quotes)[@merlin.focus ]) "2" : Virtual_dom.Vdom.Attr.t)]
    +|         ((Html_syntax.Attr.with_quotes)[@merlin.focus ]) "2"]
        [Html_syntax.Node.Primitives.text " "; ([%html {x|<p>hello</p>|x}] : _)]
    |}]
;;

let%expect_test "Childless HTML Tags" =
  test
    {|
    <div>
      Hello
      <br/>
      World!
      <input type="checkbox"/>
      <img src="./img.png"/>
      <hr />
    </div>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text " Hello ";
      Html_syntax.Node.br ();
      Html_syntax.Node.Primitives.text " World! ";
      Html_syntax.Node.input
        ~attrs:[(((Html_syntax.Attr.type_)[@merlin.focus ]) "checkbox" :
               Virtual_dom.Vdom.Attr.t)] ();
      Html_syntax.Node.img
        ~attrs:[(((Html_syntax.Attr.src)[@merlin.focus ]) "./img.png" : Virtual_dom.Vdom.Attr.t)]
        ();
      Html_syntax.Node.hr ()]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div
        [Html_syntax.Node.Primitives.text " Hello ";
        Html_syntax.Node.br ();
        Html_syntax.Node.Primitives.text " World! ";
        Html_syntax.Node.input
    -|    ~attrs:[(((Html_syntax.Attr.type_)[@merlin.focus ]) "checkbox" :
    -|           Virtual_dom.Vdom.Attr.t)] ();
    +|    ~attrs:[((Html_syntax.Attr.type_)[@merlin.focus ]) "checkbox"] ();
    -|  Html_syntax.Node.img
    -|    ~attrs:[(((Html_syntax.Attr.src)[@merlin.focus ]) "./img.png" : Virtual_dom.Vdom.Attr.t)]
    -|    ();
    +|  Html_syntax.Node.img
    +|    ~attrs:[((Html_syntax.Attr.src)[@merlin.focus ]) "./img.png"] ();
        Html_syntax.Node.hr ()]
    |}]
;;

let%expect_test "Childless HTML Tags - need a closing slash" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|
    <div>
      Hello
      <br>
      World!
      <input type="checkbox">
      <img src="./img.png">
      <hr>
    </div>
  |});
  [%expect {| ("Expected closing tag </hr>, but got </div>.") |}]
;;

let%expect_test "Sexp for debugging" =
  (* NOTE: This is only demonstrates existing behavior. I think it'd be cool to be able to
     put things like sexp_for_debugging. Maybe it could be something like:

     [%html {|<div>%{(foo : string list)}</div>|}], although also not super hyped about
     it. *)
  test
    {|
    <%{Vdom.Node.sexp_for_debugging}>
    </>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Vdom.Node.sexp_for_debugging []
    |}]
;;

let%expect_test "Quoted strings inside of element's body work" =
  test {| "hello" |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.Primitives.text " \"hello\" "
    |}];
  test {| <div>"hello world"</div> |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [Html_syntax.Node.Primitives.text "\"hello world\""]
    |}];
  test {| <div>"hello world"</div> |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [Html_syntax.Node.Primitives.text "\"hello world\""]
    |}]
;;

let%expect_test "Other forms of html escaping" =
  test {|<div>& " ' // </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [Html_syntax.Node.Primitives.text "& \" ' // "]
    |}]
;;

let%expect_test "Comments" =
  (* NOTE: This only documents current behavior and is not necessary a bug. *)
  (* HTML comments do not work. It would be cool if it gave a more descriptive error
     message on the misparse. *)
  test
    {|
    <div>
      <div></div>
      <!--This is a comment. -->
      <div></div>
    </div>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [Html_syntax.Node.div []; Html_syntax.Node.div []]
    |}];
  (* Embedded OCaml comments kind of work. *)
  test
    {|
    <div>
      <div></div>
      %{(* Comment *) Vdom.Node.none}
      <div></div>
    </div>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.div []; (Vdom.Node.none : _); Html_syntax.Node.div []]
    |}]
;;

let%expect_test "Children" =
  (* NOTE: This only documents current behavior and is not necessary a bug. *)
  test
    {|
    <div>
      <input/>
      <div/>
    </div>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div [Html_syntax.Node.input (); Html_syntax.Node.div ()]
    |}]
;;

let%expect_test "Modul on a tag" =
  test {|<%{EXPR#Foo}></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.to_string EXPR []
    |}]
;;
