open! Stdppx
open! Import
open Language.Untyped
open Ast_pattern

(** A [('a, 'node) pat] constructs an [Ast_pattern] that parse ['node]s and produce ['a]s.
    This is used to get around the value restriction when the same pattern needs to be
    used multiple times. *)
type ('a, 'node) pat = { pat : 'b. unit -> ('node, 'a -> 'b, 'b) Ast_pattern.t }
[@@unboxed]

let loc1 pat = map1' pat ~f:(fun loc a -> { txt = a; loc })
let map_pat { pat } ~f = { pat = (fun () -> map1 (pat ()) ~f) }
let at_most_one_pattern p = p ^:: nil ||| map0 nil ~f:[]
let at_most_one_eval p = pstr (at_most_one_pattern (pstr_eval p nil))

let ident_pattern =
  { pat =
      (fun () ->
        pexp_ident
          (map1' (lident __) ~f:(fun loc ident ->
             match ident with
             | "@" | "=" ->
               (* This helps break parsing ambiguity between punning and alternate forms
                  of alloc-poly (otherwise, [[@@alloc a = heap]] can be parsed as a punned
                  binding with the identifiers [( = )], [a], and [heap]). *)
               Ast_pattern.fail loc ("Invalid ppx_template identifier: " ^ ident)
             | _ -> Pattern.Identifier { ident }))
        ||| map0 pexp_hole ~f:Pattern.Wildcard)
  }
;;

let pexp_tuple p =
  map1
    (pexp_tuple (many p))
    ~f:(function
      | [] | [ _ ] ->
        failwith "parsetree invariant violated: tuples have at least two elements"
      | hd :: (_ :: _ as tl) -> (hd :: tl : _ Nonempty_list.t))
;;

let one_or_many a b = map1 a ~f:(fun x -> [ x ]) ||| b
let tuple_or_one p = pexp_tuple p ||| map1 p ~f:(fun x : _ Nonempty_list.t -> [ x ])
let one_or_tuple p = map1 p ~f:(fun x : _ Nonempty_list.t -> [ x ]) ||| pexp_tuple p

let one_or_many_as_list { pat } =
  one_or_many
    (pat ())
    (map2 (pexp_apply (pat ()) (many (pair nolabel (pat ())))) ~f:List.cons)
;;

let alloc_pattern =
  { pat =
      (fun () ->
        pexp_apply
          (pexp_ident (lident (string "@")))
          (no_label (ident_pattern.pat ()) ^:: no_label (ident_pattern.pat ()) ^:: nil)
        |> map2 ~f:(fun alloc mode -> Pattern.Tuple [ alloc; mode ]))
  }
;;

let expr =
  let expected ~loc message =
    (* Error message automatically has " expected" appended to the end *)
    Ast_pattern.fail loc ("[ppx_template] syntax error: " ^ message)
  in
  let rec of_expr : expression -> Expression.t =
    fun ({ pexp_desc; pexp_loc = loc; pexp_attributes; pexp_loc_stack = _ } as expr) ->
    let () =
      match pexp_attributes with
      | attr :: _ -> expected ~loc:attr.attr_loc "no attributes"
      | [] -> ()
    in
    match expr, Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc pexp_desc with
    | _, Pexp_constraint ({ pexp_desc; pexp_loc; _ }, Some { ptyp_desc; _ }, []) ->
      (match
         ( Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:pexp_loc pexp_desc
         , Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc )
       with
       | Pexp_hole, Ptyp_any (Some jkind) ->
         let rec of_jkind : jkind_annotation -> Expression.t = function
           | { pjkind_desc = Pjk_abbreviation ident; _ } ->
             Typed (Identifier { ident }, P (Non_tuple Kind))
           | { pjkind_desc = Pjk_mod (jkind, mode :: modes); _ } ->
             let modes =
               Nonempty_list.map (mode :: modes) ~f:(fun { txt = Mode ident; _ } ->
                 Expression.Identifier { ident })
             in
             Kind_mod (of_jkind jkind, modes)
           | { pjkind_desc = Pjk_product (jkind :: jkinds); _ } ->
             Kind_product (Nonempty_list.map (jkind :: jkinds) ~f:of_jkind)
           | { pjkind_loc = loc; _ } -> expected ~loc "kind abbreviation, mod, or product"
         in
         of_jkind jkind
       | _ -> expected ~loc "(_ : (_ : <kind>))")
    | _, Pexp_ident { txt = Lident ident; _ } -> Identifier { ident }
    | [%expr [%e? lhs] & [%e? rhs]], _ ->
      let lhs = of_expr lhs in
      let rhs =
        match of_expr rhs with
        | Kind_product rhs ->
          (* Special case: because we're using the expression language to represent kinds,
             we need to cheat and parse [a & b & c === a & (b & c)] as a flat kind. *)
          rhs
        | (Comma_separated _ | Identifier _ | Kind_mod _ | Kind_coercion _ | Typed _) as
          rhs -> [ rhs ]
      in
      Kind_product (lhs :: Nonempty_list.to_list rhs)
    | [%expr [%e? base] mod [%e? modifiers_exp]], _ ->
      let base = of_expr base in
      let modifier_exps : _ Nonempty_list.t =
        match modifiers_exp with
        | { pexp_desc = Pexp_apply (modifiers_hd, modifiers_tl); _ } ->
          let modifiers_tl =
            List.map modifiers_tl ~f:(fun (label, modifier) ->
              match label with
              | Nolabel -> modifier
              | Labelled _ | Optional _ ->
                expected ~loc:modifier.pexp_loc "unlabeled kind modifier")
          in
          modifiers_hd :: modifiers_tl
        | modifiers_hd -> [ modifiers_hd ]
      in
      let modifiers = Nonempty_list.map modifier_exps ~f:of_expr in
      Kind_mod (base, modifiers)
    | [%expr [%e? lhs] or [%e? rhs]], _ -> Kind_coercion (of_expr lhs, of_expr rhs)
    | [%expr [%e? lhs] @ [%e? rhs]], _ -> Comma_separated [ of_expr lhs; of_expr rhs ]
    | _, Pexp_tuple lab_exprs ->
      Comma_separated
        (lab_exprs
         |> (function
               | [] | _ :: [] ->
                 failwith "parsetree invariant: tuples must have at least two elements"
               | hd :: (_ :: _ as tl) -> (hd :: tl : _ Nonempty_list.t))
         |> Nonempty_list.map ~f:(function
           | Some _label, { pexp_loc = loc; _ } -> expected ~loc "unlabeled tuple element"
           | None, expr -> of_expr expr))
    | _, Pexp_construct _ -> expected ~loc "no constructors in template expressions"
    | _ -> expected ~loc "kind expression"
  in
  { pat =
      (fun () ->
        Ast_pattern.of_func (fun (_ : Ast_pattern.context) (_ : location) expr k ->
          k { txt = of_expr expr; loc = expr.pexp_loc }))
  }
;;

let pattern =
  { pat =
      (fun () ->
        one_or_tuple (ident_pattern.pat ())
        |> map1 ~f:(function
          | ([ pat ] : _ Nonempty_list.t) -> pat
          | pats -> Pattern.Tuple pats)
        ||| alloc_pattern.pat ())
  }
;;

(* Parses an [expression] of the form [l = (a, b)] as ["l", [ "a"; "b" ]]. *)
let binding =
  { pat =
      (fun () ->
        pexp_apply
          (pexp_ident (lident (string "=")))
          (pair nolabel (pattern.pat ())
           ^:: pair nolabel (tuple_or_one (expr.pat ()))
           ^:: nil)
        |> pack2)
  }
;;

(* Parses an [expression] of the form [a] as [_, [ "a" ]]. *)
let punned_binding =
  map_pat expr ~f:(fun expr -> Pattern.Wildcard, ([ expr ] : _ Nonempty_list.t))
;;

let single_ident () = pstr (pstr_eval (expr.pat ()) nil ^:: nil)
let ident_expr () = expr.pat ()
let multiple_idents () = expr |> one_or_many_as_list |> at_most_one_eval

let bindings () =
  map1 (one_or_tuple (loc1 (binding.pat ()))) ~f:Nonempty_list.to_list
  ||| ({ pat = (fun () -> loc1 (punned_binding.pat ())) } |> one_or_many_as_list)
  |> at_most_one_eval
;;

let set_bindings () =
  let set_binding =
    { pat =
        (fun () ->
          pexp_apply
            (pexp_ident (lident (string "=")))
            (pair nolabel (pattern.pat ()) ^:: pair nolabel (expr.pat ()) ^:: nil)
          |> pack2)
    }
  in
  set_binding.pat ()
  |> loc1
  |> one_or_tuple
  |> map1 ~f:Nonempty_list.to_list
  |> at_most_one_eval
;;
