open Core
open Ppxlib
open Ppx_html_syntax
module C = Ast_builder.Default
module Interpolation_kind = Ppx_html_syntax.Model.Interpolation_kind

module Type = struct
  type t =
    | String
    | Attr of { interpolation_kind : Interpolation_kind.t }
    | Node of { interpolation_kind : Interpolation_kind.t }
    | Argument

  let core_type ~loc ~(runtime_kind : Runtime_kind.t) t : core_type option =
    match t, runtime_kind with
    | String, (Js_of_ocaml | Kernel) -> Some [%type: string]
    | Attr { interpolation_kind = _ }, (Js_of_ocaml | Kernel) ->
      (* This is a bit of a hack. We do not want to add a type for Vdom.Attr.t as this is
         already added elsewhere, and also adding it here results in a double Vdom.Attr.t
         annotation. *)
      None
    | Node { interpolation_kind = _ }, (Js_of_ocaml | Kernel) -> Some [%type: _]
    | Argument, _ -> None
  ;;

  let call_f ~loc fn_name = C.pexp_ident ~loc { txt = Longident.parse fn_name; loc }

  let expand_attr_with_module
    ~(interpolation_kind : Interpolation_kind.t)
    ~module_
    ~html_syntax_module
    ~expression
    =
    let loc = expression.pexp_loc in
    match interpolation_kind, module_ with
    | String, _ ->
      Location.raise_errorf ~loc "#{} string interpolation is not allowed in attributes"
    | Normal, None -> (* %{EXPR} *) expression
    | Normal, Some { loc = module_loc; txt = module_ } ->
      (* %{EXPR#Module_} *)
      [%expr [%e call_f ~loc:module_loc [%string "%{module_}.to_attr"]] [%e expression]]
    | Option, None ->
      (* ?{EXPR} *)
      [%expr
        match [%e expression] with
        | None -> [%e Shared.attr_fn ~loc ~html_syntax_module ~primitive:true "empty"]
        | Some x -> x]
    | Option, Some { loc = module_loc; txt = module_ } ->
      (* ?{EXPR#Module_} *)
      [%expr
        match [%e expression] with
        | None -> [%e Shared.attr_fn ~loc ~html_syntax_module ~primitive:true "empty"]
        | Some x -> [%e call_f ~loc:module_loc [%string "%{module_}.to_attr"]] x]
    | List, None ->
      (* *{EXPR} *)
      [%expr
        [%e Shared.attr_fn ~loc ~html_syntax_module ~primitive:true "many"]
          [%e expression]]
    | List, Some { loc = module_loc; txt = module_ } ->
      (* *{EXPR#Module_} *)
      [%expr
        [%e Shared.attr_fn ~loc ~html_syntax_module ~primitive:true "many"]
          (Ppx_html_runtime.List.map
             [%e expression]
             ~f:[%e call_f ~loc:module_loc [%string "%{module_}.to_attr"]])]
  ;;

  let expand_string ~module_ ~expression =
    match module_ with
    | None -> expression
    | Some module_ ->
      let loc = expression.pexp_loc in
      [%expr [%e call_f ~loc [%string "%{module_.txt}.to_string"]] [%e expression]]
  ;;

  let expand_argument ~module_ ~expression =
    match module_ with
    | None -> expression
    | Some module_ ->
      Location.raise_errorf
        ~loc:module_.loc
        "Error: unexpected #%s. This syntax is not allowed inside of arguments."
        module_.txt
  ;;

  let stringable_node ~expression ~html_syntax_module ~module_ =
    let loc = expression.pexp_loc in
    let to_text = Shared.node_fn ~loc ~html_syntax_module ~primitive:true "text" in
    let to_string = call_f ~loc:module_.loc [%string "%{module_.txt}.to_string"] in
    [%expr [%e to_text] ([%e to_string] [%e expression])]
  ;;

  let expand_node
    ~html_syntax_module
    ~module_
    ~expression
    ~(interpolation_kind : Interpolation_kind.t)
    =
    let loc = expression.pexp_loc in
    match interpolation_kind, module_ with
    | String, Some module_ ->
      Location.raise_errorf
        ~loc:module_.loc
        "#{} string intepolation cannot have a module identifier"
    | String, None ->
      let to_text = Shared.node_fn ~loc ~html_syntax_module ~primitive:true "text" in
      [%expr [%e to_text] [%e Merlin_helpers.focus_expression expression]]
    | Normal, None ->
      (match expression.pexp_desc with
       | Pexp_constant (Pconst_string _) ->
         (* %{"some constant"}*)
         let to_text = Shared.node_fn ~loc ~html_syntax_module ~primitive:true "text" in
         [%expr [%e to_text] [%e Merlin_helpers.focus_expression expression]]
       | _ ->
         (* %{EXPR} *)
         expression)
    | Normal, Some module_ ->
      (* %{EXPR#Module_} *)
      stringable_node ~expression ~html_syntax_module ~module_
    | Option, None ->
      (* ?{EXPR} *)
      [%expr
        match [%e expression] with
        | None -> [%e Shared.node_fn ~loc ~html_syntax_module ~primitive:true "none"]
        | Some x -> x]
    | Option, Some module_ ->
      (* ?{EXPR#Module_} *)
      [%expr
        match [%e expression] with
        | None -> [%e Shared.node_fn ~loc ~html_syntax_module ~primitive:true "none"]
        | Some x ->
          [%e stringable_node ~expression:[%expr x] ~html_syntax_module ~module_]]
    | List, None ->
      (* *{EXPR} *)
      [%expr
        [%e Shared.node_fn ~loc ~html_syntax_module ~primitive:true "fragment"]
          [%e expression]]
    | List, Some module_ ->
      (* *{EXPR#Module_} *)
      [%expr
        [%e Shared.node_fn ~loc ~html_syntax_module ~primitive:true "fragment"]
          (Ppx_html_runtime.List.map [%e expression] ~f:(fun x ->
             [%e stringable_node ~expression:[%expr x] ~html_syntax_module ~module_]))]
  ;;

  let to_t ~html_syntax_module ~module_ ~expression t =
    match t with
    | String -> expand_string ~module_ ~expression
    | Attr { interpolation_kind } ->
      expand_attr_with_module ~interpolation_kind ~module_ ~html_syntax_module ~expression
    | Node { interpolation_kind } ->
      expand_node ~html_syntax_module ~module_ ~expression ~interpolation_kind
    | Argument -> expand_argument ~module_ ~expression
  ;;
end

let expr ?type_ ~html_syntax_module ~runtime_kind (expr : Model.Expr.t) =
  let loc = expr.loc in
  let t = expr.expr in
  let t =
    Type.to_t
      ~module_:expr.to_t
      ~html_syntax_module
      ~expression:t
      (Option.value type_ ~default:Type.String)
  in
  let t =
    match type_ with
    | None -> t
    | Some type_ ->
      (match Type.core_type ~loc ~runtime_kind type_ with
       | None -> t
       | Some type_ -> [%expr ([%e t] : [%t type_])])
  in
  t
;;

let with_immediate_quote ({ txt; loc } : Model.Quote.t) ~f =
  let rec loop ~f = function
    | [] -> f []
    | ((Model.Quote.Elt.Literal _ | Expr _) as hd) :: tl ->
      loop tl ~f:(fun tl -> f (hd :: tl))
  in
  loop txt ~f:(fun txt -> f { txt; loc })
;;

let quote ~html_syntax_module ~runtime_kind = function
  | { txt = [ Model.Quote.Elt.Expr e ]; loc = _ } ->
    expr ~runtime_kind e ~html_syntax_module ~type_:String
  | quote ->
    with_immediate_quote quote ~f:(fun quote ->
      let loc = quote.loc in
      let is_constant =
        List.for_all quote.txt ~f:(function
          | Model.Quote.Elt.Literal _ -> true
          | _ -> false)
      in
      let string_expression =
        C.pexp_constant ~loc (Pconst_string (Model.Quote.to_source quote, loc, None))
      in
      if is_constant then string_expression else [%expr [%string [%e string_expression]]])
;;
