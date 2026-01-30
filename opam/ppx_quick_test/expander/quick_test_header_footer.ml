open! Core
open! Ppxlib

let expand_impl_header loc =
  let loc = { loc with loc_end = loc.loc_start } in
  let open (val Ast_builder.make loc) in
  let open Merlin_helpers in
  let filename_expr =
    estring (Ppx_here_expander.expand_filename loc.loc_start.pos_fname)
  in
  Ppx_inline_test.guard_toplevel_test_effects
    loc
    (hide_expression
       [%expr
         Ppx_quick_test_runtime_lib.File_corrections.create
           ~filename_rel_to_project_root:[%e filename_expr]])
;;

let expand_impl_footer loc =
  let loc = { loc with loc_start = loc.loc_end } in
  let open (val Ast_builder.make loc) in
  let open Merlin_helpers in
  let filename_expr =
    estring (Ppx_here_expander.expand_filename loc.loc_start.pos_fname)
  in
  Ppx_inline_test.guard_toplevel_test_effects
    loc
    (hide_expression
       [%expr
         Ppx_quick_test_runtime_lib.File_corrections.make_corrected_file
           ~filename_rel_to_project_root:[%e filename_expr]])
;;

let expand_enclose_impl loc = expand_impl_header loc, expand_impl_footer loc
