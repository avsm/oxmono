open! Bonsai_web

(* $MDX part-begin=simple-syntax-preamble *)
module Custom_typography = struct
  let text children = {%html|<span style="color: #a1a1a1"> *{children} </span>|}
end
(* $MDX part-end *)

let () =
  ignore
    ((* $MDX part-begin=simple-syntax-manual *)
     {%html|
       <div>
         <%{Custom_typography.text}>
           <strong>Capybara</strong> UI
         </>
       </div>
     |}
     (* $MDX part-end *)
     : Vdom.Node.t)
;;

let () =
  ignore
    ((* $MDX part-begin=simple-syntax-sugar *)
     {%html|
       <div>
         <Custom_typography.text>
           <strong>Capybara</strong> UI
         </>
       </div>
     |}
     (* $MDX part-end *)
     : Vdom.Node.t)
;;

module Variant = struct
  type t =
    | Filled
    | Ghost
    | Outlined
    | Link
    | Soft

  let to_attr = function
    | _ -> Vdom.Attr.empty
  ;;
end

module Button = struct
  let view ?(attrs = []) ~variant ?(on_click = Effect.Ignore) ?(size = `Md) children =
    let size =
      match size with
      | `Xs ->
        {%css|
          padding: 0px 4px;
          gap: 2px;
        |}
      | `Md ->
        {%css|
          padding: 0px 8px;
          gap: 4px;
        |}
    in
    {%html|
      <button
        *{attrs}
        %{Variant.to_attr variant}
        %{size}
        %{Vdom.Attr.on_click (fun _ -> on_click)}
      >
        *{children}
      </button>
    |}
  ;;
end

let tomato = {%css|color: red;|}
let on_click = Effect.Ignore
let icon = Some (Vdom.Node.text "icon")

module Loading_indicator = struct
  let spinner ?(icon = Vdom.Node.text "Loading...") () = {%html|<div>%{icon}</div>|}
end

let () =
  ignore
    ((* $MDX part-begin=many-syntaxes-sugar *)
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
     (* $MDX part-end *)
     : Vdom.Node.t)
;;

let () =
  ignore
    ((* $MDX part-begin=many-syntaxes-expanded *)
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
     (* $MDX part-end *)
     : Vdom.Node.t)
;;

(* $MDX part-begin=how-to-write-apis-preamble *)
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
(* $MDX part-end *)

let order_tomato = Effect.Ignore

let () =
  ignore
    ((* $MDX part-begin=how-to-write-apis-usage *)
     {%html|
       <div>
         <Components.button on_click=%{fun _ -> order_tomato}>
           Order Tomato
         </>
         <Components.image src="./images/order-confirmation.png" />
       </div>
     |}
     (* $MDX part-end *)
     : Vdom.Node.t)
;;

module Foo = struct
  module Icon = struct
    type t = Heart

    let to_string = function
      | Heart -> "Heart"
    ;;

    let view t = Vdom.Node.text (to_string t)
  end

  (* $MDX part-begin=how-to-write-apis-preamble-2 *)
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
  (* $MDX part-end *)

  let () =
    ignore
      ((* $MDX part-begin=how-to-write-apis-usage-2 *)
       {%html|
         <Components.button ~icon:%{Heart} on_click=%{fun _ -> order_tomato}>
           Order Tomato
         </>
       |}
       (* $MDX part-end *)
       : Vdom.Node.t)
  ;;

  let () =
    ignore
      ((* $MDX part-begin=how-to-write-apis-usage-2-expanded *)
       Components.button
         ~icon:Heart
         ~attrs:[ Vdom.Attr.on_click (fun _ -> order_tomato) ]
         [ Vdom.Node.text "Order Tomato" ]
       (* $MDX part-end *)
       : Vdom.Node.t)
  ;;

  (* $MDX part-begin=how-to-write-apis-preamble-3 *)
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
  (* $MDX part-end *)

  let () =
    ignore
      ((* $MDX part-begin=how-to-write-apis-usage-3 *)
       {%html|
         <Container.view ~header:(<h1>Capybara!</h1>) ~footer:(<p>Capyright 2026</p>)>
           <p>Capybaras are the world's largest living rodent.</p>
         </>
       |}
       (* $MDX part-end *)
       : Vdom.Node.t)
  ;;

  let () =
    ignore
      ((* $MDX part-begin=how-to-write-apis-usage-3-expanded *)
       Container.view
         ~header:{%html|<h1>Capybara!</h1>|}
         ~footer:{%html|<p>Capyright 2026</p>|}
         [ {%html|<p>Capybaras are the world's largest living rodent.</p>|} ]
       (* $MDX part-end *)
       : Vdom.Node.t)
  ;;
end

let () =
  let child = Vdom.Node.text "child" in
  let children = [ child ] in
  let maybe_child = Some child in
  ignore
    ((* $MDX part-begin=quick-ref-children *)
     {%html|
       <>
         <div>%{child : Vdom.Node.t}<!-- single --></div>
         <div>*{children : Vdom.Node.t list}<!-- many --></div>
         <div>?{maybe_child : Vdom.Node.t option}<!-- optional --></div>
       </>
     |}
     (* $MDX part-end *)
     : Vdom.Node.t)
;;

let () =
  let attr = Vdom.Attr.empty in
  let attrs = [ attr ] in
  let maybe_attr = Some attr in
  ignore
    ((* $MDX part-begin=quick-ref-attrs *)
     {%html|
       <>
         <div %{attr : Vdom.Attr.t}><!-- single --></div>
         <div *{attrs : Vdom.Attr.t list}><!-- many --></div>
         <div ?{maybe_attr : Vdom.Attr.t option}><!-- optional --></div>
       </>
     |}
     (* $MDX part-end *)
     : Vdom.Node.t)
;;
