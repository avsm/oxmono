open! Core
open Ppxlib
open Ppx_quick_test_expander

let pattern () =
  let open Ast_pattern in
  single_expr_payload (pexp_let __ (__ ^:: nil) __)
;;

let quick_test =
  Extension.declare
    "quick_test"
    Expression
    (pattern ())
    (fun ~loc ~path:_ rec_flag value_binding rest ->
       expand ~loc value_binding rec_flag rest)
;;

let enclose_impl = function
  | None -> [], []
  | Some loc ->
    let loc = { loc with loc_ghost = true } in
    Ppx_quick_test_expander.enclose_impl loc
;;

let () =
  Driver.register_transformation "quick_test" ~extensions:[ quick_test ] ~enclose_impl
;;
