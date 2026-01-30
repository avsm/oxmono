open! Ppxlib

let is_debug_condition =
  ref (Ast_builder.Default.ebool Build_env.unsafe_accesses_are_checked)
;;

let expand ~loc ~path:_ expr =
  let loc = { loc with loc_ghost = true } in
  (* We intentionally keep the assertion expression even if we know it's going to be
     disabled, since removing it might cause some variables to be marked as unused. The
     [if ... then] already forces [expr] to be of type unit, but adding a type constraint
     will give a better error message. *)
  let conditional_expr expr =
    [%expr
      if [%e !is_debug_condition ~loc]
      then
        ([%e expr]
         : unit)
         [@error_message "The expression given to [%debug] must have type [unit]"]]
  in
  match expr.pexp_desc with
  | Pexp_assert assertion -> conditional_expr [%expr assert [%e assertion]]
  | _ ->
    (* There are many expressions that don't make sense to use with [%debug] due to the
       type not working out (e.g., [Pexp_function]), and more that are mostly odd choices
       (e.g., [Pexp_while]), but it seems hard to be restrictive in just the right way.
       It's also possible to use the infix extension form, e.g., [while%debug], though
       this is confusing and also not something we can prevent. *)
    conditional_expr expr
;;

let register_assert_debug =
  Context_free.Rule.extension
  @@ Extension.declare
       "debug_assert.debug"
       Extension.Context.expression
       Ast_pattern.(single_expr_payload __)
       expand
;;

(* This cookie can be used to inject a different condition for testing *)
let () =
  Driver.Cookies.add_simple_handler
    "debug_assert.condition"
    Ast_pattern.(pexp_ident __)
    ~f:(function
      | None -> ()
      | Some x ->
        is_debug_condition
        := fun ~loc -> Ast_builder.Default.(Located.mk ~loc x |> pexp_ident ~loc))
;;

let () = Driver.register_transformation "debug_assert" ~rules:[ register_assert_debug ]
