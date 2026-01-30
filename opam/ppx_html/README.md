`ppx_html` is a PPX that lets you write HTML inside of OCaml 🐪 programs. It is
spiritually similar to [JSX](<https://en.wikipedia.org/wiki/JSX_(JavaScript)>).

(the type annotations are unnecessary and only for educational purposes.)

<!-- $MDX skip -->
```ocaml
{%html|
  <div %{centered : Vdom.Attr.t}>
    <p>Capybaras are the world's largest living rodent.</p>
    <br />
    <img style="width: 50%" src=%{image_url : string} />
    %{description : Vdom.Node.t}
  </div>
|}
```

is equivalent to:

<!-- $MDX skip -->
```ocaml
Vdom.Node.div
  ~attrs:[ centered ]
  [ Vdom.Node.p [ Vdom.Node.text "Capybaras are the world's largest living rodent." ]
  ; Vdom.Node.br ()
  ; Vdom.Node.img ~attrs:[ {%css|width: 50%|}; Vdom.Attr.src image_url ] ()
  ; description
  ]
```

To use it in your project, add `ppx_html` to your jbuild's preprocess field:

```lisp
(preprocess (pps (ppx_jane ppx_html)))
```

Auto-formatting should happen by default.

VIM and VS Code should syntax highlight it by default. You can enable Emacs syntax
highlighting by adding `(Jane.polymode)` to your Emacs config. Eventually we want to make
syntax highlighting always happen by default in emacs.

Node Syntax
-----------

`ppx_html`'s syntax is similar to HTML's. To embed OCaml values, use `ppx_string`'s
familiar syntax:

| Syntax                             | Description                                                                              |
| -----------                        | -----------                                                                              |
| `{%html|<div>%{EXPR}</div>|}`      | `EXPR` is expected to be `Vdom.Node.t`                                                   |
| `{%html|<div>%{EXPR#Foo}</div>|}`  | Similar to ppx_string, will call [Vdom.Node.text (Foo.to_string EXPR)]                   |
| `{%html|<div>*{EXPR}</div>|}`      | `EXPR` is expected to be `Vdom.Node.t list`                                              |
| `{%html|<div>?{EXPR}</div>|}`      | `EXPR` is expected to be `Vdom.Node.t option`                                            |
| `{%html|<div>#{EXPR}</div>|}`      | `EXPR` is expected to be `string`                                                        |
| `{%html|<div>%{"a string"}</div>|}`| Will call [ Vdom.Node.text "a string" ]                                                  |
| `<TAG ATTRS...> INNER </TAG>`      | TAG must be `Vdom.Node.TAG : ?attrs:Vdom.Attr.t list -> Vdom.Node.t list -> Vdom.Node.t` |
| `<TAG ATTRS.../>`                  | TAG must be `Vdom.Node.TAG : ?attrs:Vdom.Attr.t list -> unit -> Vdom.Node.t`             |
| `<div %{EXPR} > INNER </div>`      | `EXPR` must be `Attr.t`                                                                  |
| `<div ?{EXPR} > INNER </div>`      | `EXPR` must be `Attr.t option`                                                           |
| `<div *{EXPR} > INNER </div>`      | `EXPR` must be `Attr.t list`                                                             |
| `<%{TAGEXPR} ATTRS...> INNER </>`  | Where `TAGEXPR : ?attrs:Vdom.Attr.t list -> Vdom.Node.t list -> Vdom.Node.t`             |
| `<%{TAGEXPR} ATTRS.../>`           | Where `TAGEXPR : ?attrs:Vdom.Attr.t list -> unit -> Vdom.Node.t`                         |
| `<Foo.f> INNER </>`                | Sugar for `<%{Foo.f}> INNER </>`.                                                        |
| `<Foo.f> INNER </Foo.f>`           | Alternate syntax for `<Foo.f> INNER </>`.                                                |
| `<Foo.f ~foo:%{EXPR}></>`          | Passes ~foo:EXPR to Foo.f as an OCaml argument (also supports ?optional arguments).      |
| `<Foo.f ~foo></>`                  | Shorthand for `<Foo.f ~foo:%{foo}></>` (also supports ?optional arguments).              |
| `<Foo.f ~foo:(<div/>)></>`         | Passes an HTML element to Foo.f as an OCaml argument (also supports ?optional arguments).|
| `{%html|<></>|}`                   | Will call `Vdom.Node.fragment`.                                                          |

Custom OCaml components and function-call syntax
------------------------------------------------

In addition to HTML tags, you can call OCaml functions as if they were tags.
Both the existing manual syntax and the new sugary syntax are supported.

Existing manual custom component syntax:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=simple-syntax-preamble -->
```ocaml
module Custom_typography = struct
  let text children = {%html|<span style="color: #a1a1a1"> *{children} </span>|}
end
```
<!-- $MDX file=./examples/ppx_html_examples.ml,part=simple-syntax-manual -->
```ocaml
     {%html|
       <div>
         <%{Custom_typography.text}>
           <strong>Capybara</strong> UI
         </>
       </div>
     |}
```

Or more sugary syntax for the same call:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=simple-syntax-sugar -->
```ocaml
     {%html|
       <div>
         <Custom_typography.text>
           <strong>Capybara</strong> UI
         </>
       </div>
     |}
```

You can also pass named and optional OCaml arguments directly in the tag head, and mix them with HTML-style attributes that become Vdom.Attr.t values. The %{}, *{}, and ?{} forms work both for children and for attributes.

<!-- $MDX file=./examples/ppx_html_examples.ml,part=many-syntaxes-sugar -->
```ocaml
     {%html|
       <div>
         <Custom_typography.text>
           <strong>Capybara</strong> UI
         </>

         <!-- Function with children and attributes. Named args use ~, optional args use ?. -->
         <Button.view
           ~on_click
           ~variant:%{Variant.Filled}
           ~size:%{`Xs}
           %{tomato : Vdom.Attr.t}
           disabled
         >
           Hello!
         </>

         <!-- Self-closing function with optional arg punning -->
         <Loading_indicator.spinner ?icon />
       </div>
     |}
```

Which expands to:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=many-syntaxes-expanded -->
```ocaml
     Vdom.Node.div
       [ Custom_typography.text
           [ Vdom.Node.strong [ Vdom.Node.text "Workflow" ]; Vdom.Node.text "UI" ]
       ; Button.view
           ~on_click
           ~variant:Variant.Filled
           ~size:`Xs
           ~attrs:[ tomato; Vdom.Attr.disabled ]
           [ Vdom.Node.text "Hello!" ]
       ; Loading_indicator.spinner ?icon ()
       ]
```

The Vdom.Attr.t type annotation above is only for illustration.

How to write APIs that support the sugary syntax
-----------------------------------------------

To be callable as a tag:

- With children: write a function of type `Vdom.Node.t list -> Vdom.Node.t`
  - Call sites can write `<Foo.f> child1 child2 </>` (or `</Foo.f>`)
- Self-closing: write a function of type `unit -> Vdom.Node.t`
  - Call sites can write `<Foo.f />`

If you want callers to be able to pass attributes, add the magic optional `?attrs : Vdom.Attr.t list` argument to your function. Any HTML-style attributes written at the call site will be collected and passed as this list.

For example:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=how-to-write-apis-preamble -->
```ocaml
module Components = struct
  let button ?(attrs : Vdom.Attr.t list = []) (children : Vdom.Node.t list) =
    {%html|
      <button style="background-color: tomato" *{attrs}>
        *{children}
      </button>
    |}
  ;;

  let image ?(attrs : Vdom.Attr.t list = []) () =
    {%html|<img style="width: 50%" *{attrs} />|}
  ;;
end
```

Usage:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=how-to-write-apis-usage -->
```ocaml
     {%html|
       <div>
         <Components.button on_click=%{fun _ -> order_tomato}>
           Order Tomato
         </>
         <Components.image src="./images/order-confirmation.png" />
       </div>
     |}
```

You can also add named and optional arguments; callers pass them with `~arg:%{expr}` or `?arg:%{expr}`. Punning is supported: `~foo` and `?foo` are shorthand for `~foo:%{foo}` and `?foo:%{foo}`.

<!-- $MDX file=./examples/ppx_html_examples.ml,part=how-to-write-apis-preamble-2 -->
```ocaml
  module Components = struct
    let button ?(icon : Icon.t option) ?(attrs = []) children =
      let icon = icon |> Option.map Icon.view in
      {%html|
        <button style="background-color: tomato" *{attrs}>
          ?{icon} *{children}
        </button>
      |}
    ;;
  end
```

<!-- $MDX file=./examples/ppx_html_examples.ml,part=how-to-write-apis-usage-2 -->
```ocaml
       {%html|
         <Components.button ~icon:%{Heart} on_click=%{fun _ -> order_tomato}>
           Order Tomato
         </>
       |}
```

Expands to:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=how-to-write-apis-usage-2-expanded -->
```ocaml
       Components.button
         ~icon:Heart
         ~attrs:[ Vdom.Attr.on_click (fun _ -> order_tomato) ]
         [ Vdom.Node.text "Order Tomato" ]
```

Additionally, you can pass HTML elements directly as a named argument using the nested ppx_html syntax: `~arg:(<element/>)`. This is useful for "slot" patterns where a component accepts multiple nested components as arguments. Instead of creating separate variables, you can write the HTML inline:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=how-to-write-apis-preamble-3 -->
```ocaml
  module Container = struct
    let view ?(footer : Vdom.Node.t option) ~(header : Vdom.Node.t) children =
      {%html|
        <div class="container">
          <header>%{header}</header>
          <main>*{children}</main>
          ?{footer}
        </div>
      |}
    ;;
  end
```

<!-- $MDX file=./examples/ppx_html_examples.ml,part=how-to-write-apis-usage-3 -->
```ocaml
       {%html|
         <Container.view ~header:(<h1>Capybara!</h1>) ~footer:(<p>Capyright 2026</p>)>
           <p>Capybaras are the world's largest living rodent.</p>
         </>
       |}
```

Expands to:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=how-to-write-apis-usage-3-expanded -->
```ocaml
       Container.view
         ~header:{%html|<h1>Capybara!</h1>|}
         ~footer:{%html|<p>Capyright 2026</p>|}
         [ {%html|<p>Capybaras are the world's largest living rodent.</p>|} ]
```

Rules and notes:
- Call sites must include at least one module qualifier (e.g., `Foo.f`); otherwise `ppx_html` looks for `Vdom.Node.*`.
- Only named/optional OCaml arguments are supported in tag position; besides children or unit, positional arguments are not supported.
- The magic `?attrs` is where HTML attributes from the tag head are collected, e.g., `disabled`, `on_click=%{...}`, etc.

Quick reference for %{}, *{}, and ?{}
-------------------------------------

Children position:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=quick-ref-children -->
```ocaml
     {%html|
       <>
         <div>%{child : Vdom.Node.t}<!-- single --></div>
         <div>*{children : Vdom.Node.t list}<!-- many --></div>
         <div>?{maybe_child : Vdom.Node.t option}<!-- optional --></div>
       </>
     |}
```

Attribute position:

<!-- $MDX file=./examples/ppx_html_examples.ml,part=quick-ref-attrs -->
```ocaml
     {%html|
       <>
         <div %{attr : Vdom.Attr.t}><!-- single --></div>
         <div *{attrs : Vdom.Attr.t list}><!-- many --></div>
         <div ?{maybe_attr : Vdom.Attr.t option}><!-- optional --></div>
       </>
     |}
```

Attribute Syntax
----------------

Nodes may have ATTRS as described below:

- `NAME` then NAME is `Vdom.Attr.NAME : Vdom.Attr.t`.
- `NAME=VALUE`  then NAME is a name in `Vdom.Attr.NAME : 'a -> Vdom.Attr.t` and `VALUE` is one of
    - `UNQUOTEDLITERAL` - treated as a string. There is a heuristic to parse the string
      into the correct `'a`.
    - `"QUOTED_LITERAL"` - treated as a string, but allows `ppx_string` interpolation,
      and will trigger similar heuristics to parse the string.
      In particular `style="..."` and `style=...` will use `ppx_css`.
      Additionally, `tailwind="..."` and `tailwind=...` will use `ppx_tailwind`.
    - `%{EXPR}` - arbitrary ocaml expression that should evaluate the the `'a` that is
      expected.
- `key=VALUE` will pass ~key.

Additionally some OCaml keywords that are also attributes (e.g. `for`) are special cased
to expand to `Vdom.Attr.for_`.

How can I use tailwind?
-----------------------
To use `ppx_tailwind`, , we've special cased a "tailwind" attribute `<div
tailwind="..."></div>` behaves like `<div %{[%tailwind ".."]}></div>`.


How can I use `Virtual_dom_svg`?
------------------------------

To create `virtual_dom_svg` nodes instead of `virtual_dom_svg`, open
`Virtual_dom_svg`'s `Html_syntax`:


<!-- $MDX file=test/vdom_tests/test_vdom_svg_with_hyphens.ml,part=open-example -->
```ocaml
   let open Virtual_dom_svg.Html_syntax in
   {%html|
     <svg height=%{100.} width=%{100.}>
       <circle
         cx=%{50.}
         cy=%{50.}
         r=%{40.}
         stroke=%{`Name "black"}
         stroke_width=%{3.}
         fill=%{`Name "red"}
       ></circle>
     </svg>
   |}
```

Alternatively, you can:

<!-- $MDX file=test/vdom_tests/test_vdom_svg_with_hyphens.ml,part=inline-example -->
```ocaml
    [%html.Virtual_dom_svg
      {|
        <svg height=%{100.} width=%{100.}>
          <circle
            cx=%{50.}
            cy=%{50.}
            r=%{40.}
            stroke=%{`Name "black"}
            stroke_width=%{3.}
            fill=%{`Name "red"}
          ></circle>
        </svg>
      |}]
```

You can go back to using `virtual_dom` nodes by opening `Virtual_dom.Html_syntax`.
Opening `Bonsai_web` does this for you!

How can I use `ppx_html` outside of a Javascript context (e.g. to perform server-side rendering of HTML)?
---------------------------------------------------------------------------------------------------------
You can use `ppx_html_kernel`. It is a version of `ppx_html` without JavaScript
dependencies and without `js_of_ocaml` assumptions (e.g. it does not attempt to use
`ppx_css`).

There is an `Html_syntax` for `lib/html` in the library `ppx_html_lib_html_syntax`. To use it, you must:
1. Add the library `ppx_html_lib_html_syntax` as a dependency
2. Add the ppx `ppx_html_kernel` (different from `ppx_html`!) as a dependency to your preprocess field.
3. After that you can use `ppx_html_kernel` by opening `ppx_html_lib_html_syntax`:

<!-- $MDX skip -->
```ocaml
open! Core
open Ppx_html_lib_html_syntax.Html_syntax

let hello =
    {%html|
        <html>
          <head>
            <style>
              body {
                background-color: tomato;
              }
            </style>
          </head>
          <body>
            <div><h1>Hello!!</h1></div>
          </body>
        </html>
      |};
```

Note that there are some differences. `ppx_html` special cases `style=` into using `ppx_css` while `ppx_html_kernel`
does not, which means that using nested CSS inside of style tags (e.g. `&:hover body {}` will not do what you expect
in `ppx_html_kernel`).
