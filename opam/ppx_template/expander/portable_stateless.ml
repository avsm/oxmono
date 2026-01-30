open! Stdppx
open! Import

type t =
  | Portable
  | Stateless

let all = [ Portable; Stateless ]

let to_string = function
  | Portable -> "portable"
  | Stateless -> "stateless"
;;

(* Convert [functor (X1 : S1) (X2 : S2) -> S3] to
   {[
     functor
       (X1 : sig @@ portable include S1 end)
       (X2 : sig @@ portable include S2 end)
       -> sig @@ portable include S3 end
   ]}
*)
let mk_pmb_type pmb_type ~loc ~mvar =
  let rec loop pmb_type =
    match Ppxlib_jane.Shim.Module_type_desc.of_parsetree pmb_type.pmty_desc with
    | Pmty_functor (param, ret_type, modes) ->
      let param =
        match Ppxlib_jane.Shim.Functor_parameter.of_parsetree param with
        | Unit -> Unit
        | Named (name, pmb_type, modes) ->
          Ppxlib_jane.Shim.Functor_parameter.to_parsetree
            (Named (name, loop pmb_type, modes))
      in
      { pmb_type with
        pmty_desc =
          Ppxlib_jane.Shim.Module_type_desc.to_parsetree
            ~loc
            (Pmty_functor (param, loop ret_type, modes))
      ; pmty_loc = loc
      }
    | Pmty_signature signature ->
      let signature = Ppxlib_jane.Shim.Signature.of_parsetree signature in
      { pmb_type with
        pmty_desc =
          Ppxlib_jane.Shim.Module_type_desc.to_parsetree
            ~loc
            (Pmty_signature
               (Ppxlib_jane.Shim.Signature.to_parsetree
                  { signature with
                    psg_modalities =
                      Loc.make ~loc (Modality mvar) :: signature.psg_modalities
                  ; psg_loc = loc
                  }))
      ; pmty_loc = loc
      }
    | _ ->
      Ast_builder.pmty_signature
        ~loc
        (Ast_builder.signature
           ~loc
           ~modalities:[ Loc.make ~loc (Modality mvar) ]
           [ Ast_builder.psig_include
               ~loc
               ~modalities:[]
               (Ast_builder.include_infos ~loc ~kind:Structure pmb_type)
           ])
  in
  loop pmb_type
;;

(* Convert [functor (X1 : S1) (X2 : S2) : S3 -> ...] to
   {[
     functor
       (X1 : sig @@ portable include S1 end)
       (X2 : sig @@ portable include S2 end)
       : sig @@ portable include S3 end
       ->
         ...
   ]}
*)
let mk_pmb_expr pmb_expr ~loc ~mvar =
  let rec loop pmb_expr =
    match Ppxlib_jane.Shim.Module_expr_desc.of_parsetree pmb_expr.pmod_desc with
    | Pmod_constraint (pmb_expr, pmb_type, modes) ->
      { pmb_expr with
        pmod_desc =
          Ppxlib_jane.Shim.Module_expr_desc.to_parsetree
            ~loc
            (Pmod_constraint
               (loop pmb_expr, Option.map pmb_type ~f:(mk_pmb_type ~loc ~mvar), modes))
      ; pmod_loc = loc
      }
    | Pmod_functor (param, pmb_expr) ->
      let param =
        match Ppxlib_jane.Shim.Functor_parameter.of_parsetree param with
        | Unit -> Unit
        | Named (name, pmb_type, modes) ->
          Ppxlib_jane.Shim.Functor_parameter.to_parsetree
            (Named (name, mk_pmb_type pmb_type ~loc ~mvar, modes))
      in
      { pmb_expr with pmod_desc = Pmod_functor (param, loop pmb_expr); pmod_loc = loc }
    | _ -> pmb_expr
  in
  loop pmb_expr
;;

let mvals ~loc = function
  | Portable -> [%expr portable, nonportable]
  | Stateless -> [%expr stateless, observing, stateful]
;;

(* [@@template.modality `mvar` = `mvals ~loc t`] *)
let modality_attribute t ~loc ~mvar =
  Ast_builder.attribute
    ~loc
    ~name:(Loc.make ~loc "template.modality")
    ~payload:(PStr [%str [%e Ast_builder.evar ~loc mvar] = [%e mvals ~loc t]])
;;

let modality_attr t context =
  Attribute.declare
    (Printf.sprintf "template.%s.modality" (to_string t))
    context
    Ast_pattern.(single_expr_payload (pexp_ident (lident __)))
    Fn.id
;;

let modality_attrs_binding = List.map all ~f:(fun t -> t, modality_attr t Module_binding)

let modality_attrs_declaration =
  List.map all ~f:(fun t -> t, modality_attr t Module_declaration)
;;

let modality_attr_binding t = List.assoc t modality_attrs_binding
let modality_attr_declaration t = List.assoc t modality_attrs_declaration

(* Look for [@@modality m], e.g.
   {[
     module%template.portable [@modality m] F ...
   ]}
*)
let consume attr mod_ =
  match Attribute.consume attr mod_ with
  | Some value -> value
  | None -> mod_, Ppxlib.gen_symbol ~prefix:"modality" ()
;;

let module_binding t ~loc ~mod_ =
  let mod_, mvar = consume (modality_attr_binding t) mod_ in
  Ast_builder.pstr_module
    ~loc
    { mod_ with
      pmb_expr = mk_pmb_expr mod_.pmb_expr ~loc ~mvar
    ; pmb_attributes = modality_attribute t ~loc ~mvar :: mod_.pmb_attributes
    }
;;

let module_declaration t ~loc ~mod_ =
  let mod_, mvar = consume (modality_attr_declaration t) mod_ in
  Ast_builder.psig_module
    ~loc
    { mod_ with
      pmd_type = mk_pmb_type mod_.pmd_type ~loc ~mvar
    ; pmd_attributes = modality_attribute t ~loc ~mvar :: mod_.pmd_attributes
    }
;;
