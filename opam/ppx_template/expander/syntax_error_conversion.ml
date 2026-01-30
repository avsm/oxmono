open! Stdppx
open! Import

(* drop attributes and ghost locations in the error node *)
let explicitly_drop =
  object
    inherit Ast_traverse.map as super

    method! attribute attr =
      Attribute.mark_as_handled_manually attr;
      attr

    method! attributes attrs =
      let _ : attribute list = super#attributes attrs in
      []

    method! location loc = { loc with loc_ghost = true }
  end
;;

let to_extension_node
  : type a. ?also_drop:a list -> a Attributes.Context.any -> a -> Syntax_error.t -> a
  =
  fun ?(also_drop = []) ctx node t ->
  let extension, loc, explicitly_drop =
    match ctx with
    | Expression ->
      ( (Ast_builder.pexp_extension : loc:_ -> Ppxlib.extension -> a)
      , node.pexp_loc
      , (explicitly_drop#expression : a -> a) )
    | Module_expr ->
      Ast_builder.pmod_extension, node.pmod_loc, explicitly_drop#module_expr
    | Core_type -> Ast_builder.ptyp_extension, node.ptyp_loc, explicitly_drop#core_type
    | Module_type ->
      Ast_builder.pmty_extension, node.pmty_loc, explicitly_drop#module_type
    | Value_binding ->
      ( (fun ~loc ext -> { node with pvb_expr = Ast_builder.pexp_extension ~loc ext })
      , node.pvb_loc
      , explicitly_drop#value_binding )
    | Value_description ->
      ( (fun ~loc ext -> { node with pval_type = Ast_builder.ptyp_extension ~loc ext })
      , node.pval_loc
      , explicitly_drop#value_description )
    | Module_binding ->
      ( (fun ~loc ext -> { node with pmb_expr = Ast_builder.pmod_extension ~loc ext })
      , node.pmb_loc
      , explicitly_drop#module_binding )
    | Module_declaration ->
      ( (fun ~loc ext -> { node with pmd_type = Ast_builder.pmty_extension ~loc ext })
      , node.pmd_loc
      , explicitly_drop#module_declaration )
    | Type_declaration ->
      ( (fun ~loc ext ->
          { node with ptype_manifest = Some (Ast_builder.ptyp_extension ~loc ext) })
      , node.ptype_loc
      , explicitly_drop#type_declaration )
    | Module_type_declaration ->
      ( (fun ~loc ext ->
          { node with pmtd_type = Some (Ast_builder.pmty_extension ~loc ext) })
      , node.pmtd_loc
      , explicitly_drop#module_type_declaration )
    | Include_infos ->
      ( (fun ~loc ext ->
          match node.pincl_mod with
          | Left _ -> { node with pincl_mod = Left (Ast_builder.pmod_extension ~loc ext) }
          | Right _ ->
            { node with pincl_mod = Right (Ast_builder.pmty_extension ~loc ext) })
      , node.pincl_loc
      , explicitly_drop#include_infos (function
          | (Left mod_ : _ Either.t) -> Left (explicitly_drop#module_expr mod_)
          | Right mty -> Right (explicitly_drop#module_type mty)) )
  in
  let loc = { loc with loc_ghost = true } in
  (* We run [explicitly_drop] on both the original node(s) that we are replacing and on
     the error-containing node that we are creating.
     1. We run it on the input node so that any attributes that are being removed from the
        AST without opportunity for further processing are [mark_as_handled_manually]ed.
     2. We run it on any sibling nodes that are dropped along the way, if provided via
        [also_drop].
     3. We run it on the output node so that we delete any attributes that are still
        around (e.g. because they are "outside" the part that was replaced by the error)
        and mark all locations as ghost.

     Really, these are two orthogonal operations, but we use one object because they don't
     get in each other's way, and having to fish out the right method from both in the
     match above would be extra noisy.
  *)
  let _ : a = explicitly_drop node in
  let _ : a list = List.map also_drop ~f:explicitly_drop in
  extension ~loc (Syntax_error.to_extension t) |> explicitly_drop
;;

let to_extension_node_floating
  : type a. a Attributes.Floating.Context.poly -> loc:location -> Syntax_error.t -> a
  =
  fun ctx ~loc t ->
  let loc = { loc with loc_ghost = true } in
  match ctx with
  | Structure_item -> Ast_builder.pstr_extension ~loc (Syntax_error.to_extension t) []
  | Signature_item -> Ast_builder.psig_extension ~loc (Syntax_error.to_extension t) []
;;
