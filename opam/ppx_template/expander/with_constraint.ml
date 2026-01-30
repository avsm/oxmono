open! Stdppx
open! Import
open Import.Result.Let_syntax

module Path = struct
  type t =
    | No_prefix
    | Prefix of Longident.t

  let dot t str =
    match t with
    | No_prefix -> Lident str
    | Prefix path -> Ldot (path, str)
  ;;
end

let error ~loc err =
  Error (Syntax_error.createf ~loc "Invalid [@template.with] payload:\n%s" err)
;;

(* extract items from a signature, error on default modalities (they have no analogue in
   with constraints) *)
let items sig_ =
  match Ppxlib_jane.Shim.Signature.of_parsetree sig_ with
  | { psg_modalities = []; psg_items = sigis; psg_loc = _ } -> Ok sigis
  | { psg_modalities = moda :: _; psg_items = _; psg_loc = _ } ->
    error ~loc:moda.loc "default modalities not allowed"
;;

(* remove attributes for toggling docs and verify that there are no modalities and that
   all remaining attributes are doc comments *)
let check_attributes_and_modalities mty attrs modas =
  let atts =
    List.filter attrs ~f:(fun attr ->
      match Ppx_helpers.Docs.of_attribute attr with
      | Some Toggle | Some Inline -> false
      | Some (Doc _) | None ->
        Attribute.mark_as_handled_manually attr;
        true)
  in
  let+ () =
    match atts, modas with
    | attr :: _, _ ->
      error ~loc:attr.attr_loc ("non-doc attributes not allowed\n" ^ attr.attr_name.txt)
    | _, moda :: _ -> error ~loc:moda.loc "modalities not allowed"
    | _ -> Ok ()
  in
  { mty with pmty_attributes = attrs @ mty.pmty_attributes }
;;

let convert mty with_ =
  let rec loop mty path = function
    | [] -> Ok mty
    | { psig_desc; psig_loc = loc } :: with_ ->
      (* convert [type t = ...] to [with type t = ...]; same for [:=] *)
      let map_decls decls wrap =
        let decls =
          List.map decls ~f:(fun ({ ptype_name; _ } as decl) ->
            wrap { ptype_name with txt = Path.dot path ptype_name.txt } decl)
        in
        loop (Ast_builder.pmty_with ~loc mty decls) path with_
      in
      (* convert [module] and [module type] bindings *)
      let mod_decl mty attrs modas ident wrap =
        let* mty = check_attributes_and_modalities mty attrs modas in
        let mty =
          Ast_builder.pmty_with
            ~loc
            mty
            [ wrap { ident with txt = Path.dot path ident.txt } ]
        in
        loop mty path with_
      in
      (* process a signature inside a [include sig ... end] or [[%%template:]], then
         continue *)
      let inline sig_ attrs modalities =
        let* inner_with = items sig_ in
        let* mty = check_attributes_and_modalities mty attrs modalities in
        let* mty = loop mty path inner_with in
        loop mty path with_
      in
      (match Ppxlib_jane.Shim.Signature_item_desc.of_parsetree psig_desc with
       | Psig_type (_, decls) -> map_decls decls (fun id decl -> Pwith_type (id, decl))
       | Psig_typesubst decls ->
         map_decls decls (fun id decl -> Pwith_typesubst (id, decl))
       | Psig_modtype { pmtd_name; pmtd_type = Some binding; pmtd_attributes; _ } ->
         mod_decl mty pmtd_attributes [] pmtd_name (fun id -> Pwith_modtype (id, binding))
       | Psig_modsubst { pms_name; pms_manifest; pms_attributes; _ } ->
         mod_decl mty pms_attributes [] pms_name (fun id ->
           Pwith_modsubst (id, pms_manifest))
       | Psig_modtypesubst { pmtd_name; pmtd_type = Some binding; pmtd_attributes; _ } ->
         mod_decl mty pmtd_attributes [] pmtd_name (fun id ->
           Pwith_modtypesubst (id, binding))
       | Psig_include (include_descr, modalities) ->
         (match Ppxlib_jane.Shim.Include_infos.of_parsetree include_descr with
          | { pincl_kind = Structure
            ; pincl_mod = { pmty_desc = Pmty_signature sig_; _ }
            ; pincl_loc = _
            ; pincl_attributes = attrs
            } -> inline sig_ attrs modalities
          | { pincl_loc; _ } ->
            error ~loc:pincl_loc "[include] only allowed for [sig ... end]")
       | Psig_extension (({ txt = "template.inline"; loc = _ }, PSig sig_), attrs) ->
         (match Ppxlib_jane.Shim.Signature.of_parsetree sig_ with
          | { psg_modalities = modalities; _ } -> inline sig_ attrs modalities)
       | Psig_attribute attr ->
         let* mty = check_attributes_and_modalities mty [ attr ] [] in
         loop mty path with_
       | Psig_module module_decl ->
         (match Ppxlib_jane.Shim.Module_declaration.of_parsetree module_decl with
          | { pmd_name
            ; pmd_type = { pmty_desc; _ }
            ; pmd_modalities
            ; pmd_attributes
            ; pmd_loc = loc
            } ->
            let* mty =
              check_attributes_and_modalities mty pmd_attributes pmd_modalities
            in
            let* pmd_name =
              match pmd_name.txt with
              | Some txt -> Ok { pmd_name with txt }
              | None -> error ~loc "anonymous modules not allowed"
            in
            (match pmty_desc with
             | Pmty_signature sig_ ->
               let* inner_with = items sig_ in
               (* extend the path (used to express with constraints of the shape
                  [type M.t := ...] ) *)
               let* mty = loop mty (Prefix (Path.dot path pmd_name.txt)) inner_with in
               loop mty path with_
             | Pmty_alias binding ->
               mod_decl mty pmd_attributes pmd_modalities pmd_name (fun id ->
                 Pwith_module (id, binding))
             | _ -> error ~loc "only [module M : sig ... end] and [module M : S] allowed"))
       | _ ->
         error ~loc "signature can only contain type, module, and module type bindings")
  in
  let* with_ = items with_ in
  loop mty No_prefix (Ppx_helpers.ghoster#list Ppx_helpers.ghoster#signature_item with_)
;;
