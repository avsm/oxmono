open! Base
open! Ppxlib

module Let_and_match = struct
  let expand_application ~loc ~tilde ~stack ~e ~f =
    match tilde, stack with
    | false, false -> [%expr [%e e] [%e f]]
    | true, false -> [%expr [%e e] ~f:[%e f]]
    | false, true -> [%expr [%e e] (stack_ [%e f]) [@nontail]]
    | true, true -> [%expr [%e e] ~f:(stack_ [%e f]) [@nontail]]
  ;;

  module Expand = struct
    let variables_of =
      object
        inherit [string Ppxlib.loc list] Ast_traverse.fold as super

        method! pattern p acc =
          let acc = super#pattern p acc in
          match p.ppat_desc with
          | Ppat_var var -> var :: acc
          | Ppat_alias (_, var) -> var :: acc
          | _ -> acc
      end
    ;;

    let pattern_variables pattern =
      List.dedup_and_sort
        ~compare:(fun x y -> String.compare x.txt y.txt)
        (variables_of#pattern pattern [])
    ;;

    let maybe_enter_value pat expr =
      match pattern_variables pat with
      | [ { loc; txt } ] ->
        let loc = { loc with loc_ghost = true } in
        let attr =
          { attr_loc = loc
          ; attr_name = { loc; txt = Attribute.name Ast_traverse.enter_value }
          ; attr_payload =
              PStr
                [ Ast_builder.Default.pstr_eval
                    ~loc
                    (Ast_builder.Default.evar ~loc txt)
                    []
                ]
          }
        in
        { expr with pexp_attributes = attr :: expr.pexp_attributes }
      | [] | _ :: _ :: _ -> expr
    ;;

    let let_ ~loc ~bindings ~rest ~tilde ~stack =
      let loc = { loc with loc_ghost = true } in
      List.fold_right bindings ~init:rest ~f:(fun pvb expr ->
        expand_application
          ~loc
          ~tilde
          ~stack
          ~e:(maybe_enter_value pvb.pvb_pat pvb.pvb_expr)
          ~f:[%expr fun [%p pvb.pvb_pat] -> [%e expr]])
    ;;

    let match_ ~loc ~expr ~cases ~tilde ~stack =
      let loc = { loc with loc_ghost = true } in
      let f = Ast_builder.Default.pexp_function cases ~loc in
      expand_application ~loc ~tilde ~stack ~e:expr ~f
    ;;
  end

  let rewrite ~tilde ~stack =
    let name_suffix_for_tilde = if tilde then ".tilde" else "" in
    let name_suffix_for_stack = if stack then ".stack" else "" in
    let name = "with" ^ name_suffix_for_tilde ^ name_suffix_for_stack in
    Extension.declare
      name
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (fun ~loc ~path:_ expr ->
        match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc with
        | Pexp_let (Immutable, Nonrecursive, bindings, expr) ->
          Expand.let_ ~loc ~bindings ~rest:expr ~tilde ~stack
        | Pexp_match (expr, cases) -> Expand.match_ ~loc ~expr ~cases ~tilde ~stack
        | Pexp_let (_, Recursive, _, _) ->
          Location.raise_errorf ~loc "[ppx_with] is not supported with [let rec]"
        | Pexp_let (Mutable, _, _, _) ->
          Location.raise_errorf ~loc "[ppx_with] is not supported with [let mutable]"
        | _ ->
          Location.raise_errorf
            ~loc
            "[ppx_with] must be used in a [let] or [match] expression")
  ;;
end

module Const = struct
  let expand ~loc ~expr =
    let loc = { loc with loc_ghost = true } in
    [%expr fun f -> f [%e expr]]
  ;;

  let rewrite =
    Extension.declare
      "with.const"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (fun ~loc ~path:_ expr -> expand ~loc ~expr)
  ;;
end

let () =
  Driver.register_transformation
    "ppx_with"
    ~extensions:
      [ Let_and_match.rewrite ~tilde:true ~stack:false
      ; Let_and_match.rewrite ~tilde:false ~stack:false
      ; Let_and_match.rewrite ~tilde:true ~stack:true
      ; Let_and_match.rewrite ~tilde:false ~stack:true
      ; Const.rewrite
      ]
;;
