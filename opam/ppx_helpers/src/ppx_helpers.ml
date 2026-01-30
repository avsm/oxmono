open! Stdppx
open Ppxlib
open Ast_builder.Default
module Ox = Ox

let ghoster =
  object
    inherit Ast_traverse.map
    method! location l = { l with loc_ghost = true }
  end
;;

module Docs = struct
  type t =
    | Toggle
    | Inline
    | Doc of string

  let of_attribute = function
    | { attr_name = { txt = "ocaml.doc" | "ocaml.text"; loc = _ }
      ; attr_payload =
          PStr
            [%str
              [%e?
                { pexp_desc = Pexp_constant (Pconst_string (contents, _loc, _delim)); _ }]]
      ; _
      } ->
      Some
        (match contents with
         | "/*" -> Toggle
         | " @inline " -> Inline
         | _ -> Doc contents)
    | _ -> None
  ;;

  let hide ~loc sigis = List.concat [ [ [%sigi: (**/**)] ]; sigis; [ [%sigi: (**/**)] ] ]

  let[@tail_mod_cons] rec simplify = function
    | [%sigi: (**/**)] :: [%sigi: (**/**)] :: rest -> simplify rest
    | sigi :: rest -> sigi :: simplify rest
    | [] -> []
  ;;
end

let is_implicit_unboxed typename = String.is_suffix typename ~suffix:"#"

let drop_unboxed_suffix typename =
  if String.is_suffix typename ~suffix:"#"
  then String.drop_suffix typename 1, true
  else typename, false
;;

let demangle_template typename =
  let typename, unboxed = drop_unboxed_suffix typename in
  List.init ~len:(String.length typename - 1) ~f:Fn.id
  |> List.find_opt ~f:(fun i -> Char.(typename.[i] = '_' && typename.[i + 1] = '_'))
  |> Option.map ~f:(fun i -> String.prefix typename i, String.drop_prefix typename i)
  |> Option.value ~default:(typename, "")
  |> if unboxed then fun (typename, mangling) -> typename ^ "_u", mangling else Fn.id
;;

let mangle_unboxed typename =
  let typename, mangling = demangle_template typename in
  typename ^ mangling
;;

(* These are private functions taken from [Ppxlib.Ast_builder] with minor adjustments to
   suit our purposes. *)
open struct
  (* Like [Ast_builder.unapplied_type_constr_conv_without_apply], but returns the
     identifier directly rather than a [pexp_ident]. *)
  let unapplied_type_constr_conv_without_apply ~loc ~functor_ (ident : Longident.t) ~f =
    match ident with
    | Lident n -> { txt = Lident (f ~functor_ n); loc }
    | Ldot (arg, n) -> { txt = Ldot (arg, f ~functor_ n); loc }
    | Lapply _ -> Location.raise_errorf ~loc "unexpected applicative functor type"
  ;;

  (* [Ppxlib] inlines this function in [Ast_builder.unapplied_type_constr_conv] directly;
     we pull it out for readability. *)
  let functor_conv_name_and_args module_path n =
    let rec gather_lapply functor_args : Longident.t -> _ * Longident.t * _ = function
      | Lapply (rest, arg) -> gather_lapply (arg :: functor_args) rest
      | Lident functor_ -> String.uncapitalize_ascii functor_, Lident n, functor_args
      | Ldot (functor_path, functor_) ->
        String.uncapitalize_ascii functor_, Ldot (functor_path, n), functor_args
    in
    gather_lapply [] module_path
  ;;
end

let type_constr_conv_and_apply ~loc:apply_loc { loc; txt = longident } ~f apply =
  let loc = { loc with loc_ghost = true } in
  match (longident : Longident.t) with
  | Lident _ | Ldot ((Lident _ | Ldot _), _) | Lapply _ ->
    let ident =
      unapplied_type_constr_conv_without_apply ~functor_:None longident ~loc ~f
    in
    apply ~loc:apply_loc ident []
  | Ldot ((Lapply _ as module_path), n) ->
    let module_path, ident, functor_args = functor_conv_name_and_args module_path n in
    apply
      ~loc:apply_loc
      (unapplied_type_constr_conv_without_apply
         ~functor_:(Some module_path)
         ident
         ~loc
         ~f)
      functor_args
;;

let type_constr_conv_expr ~loc longident ~f args =
  let f ~functor_ x = f ?functor_ x in
  type_constr_conv_and_apply ~loc longident ~f (fun ~loc ident functor_args ->
    let expr = pexp_ident ~loc ident in
    let functor_args =
      List.map functor_args ~f:(fun path ->
        pexp_pack ~loc (pmod_ident ~loc { txt = path; loc }))
    in
    match functor_args @ args with
    | [] -> expr
    | args -> eapply ~loc expr args)
;;

let type_constr_conv_pat ~loc longident ~f =
  let f ~functor_ x = f ?functor_ x in
  type_constr_conv_and_apply ~loc longident ~f (fun ~loc ident _ ->
    match ident.txt with
    | Lident name -> ppat_var ~loc { loc; txt = name }
    | _ ->
      ppat_extension
        ~loc
        (Location.error_extensionf
           ~loc
           "Invalid identifier %s for converter in pattern position. Only simple \
            identifiers (like t or string) or applications of functors with simple \
            identifiers (like M(K).t) are supported."
           (Longident.name longident.txt)))
;;

let has_unboxed_attribute td =
  List.exists td.ptype_attributes ~f:(fun attr ->
    match attr.attr_name.txt with
    | "unboxed" | "ocaml.unboxed" -> true
    | _ -> false)
;;

let implicit_unboxed_record td =
  let make_immutable ld =
    { ld with
      pld_mutable = Immutable
    ; pld_attributes =
        List.filter ld.pld_attributes ~f:(fun { attr_name; _ } ->
          not (String.equal attr_name.txt "deprecated_mutable"))
    }
  in
  match td.ptype_kind with
  | Ptype_record lds when not (has_unboxed_attribute td) ->
    Some
      { td with
        ptype_kind =
          Ppxlib_jane.Shim.Type_kind.Ptype_record_unboxed_product
            (List.map lds ~f:make_immutable)
          |> Ppxlib_jane.Shim.Type_kind.to_parsetree
      ; ptype_name = { td.ptype_name with txt = td.ptype_name.txt ^ "#" }
      }
  | _ -> None
;;

let with_implicit_unboxed_records ~loc ~unboxed tds =
  match unboxed with
  | false -> tds
  | true ->
    let with_unboxed =
      List.concat_map tds ~f:(fun td -> td :: Option.to_list (implicit_unboxed_record td))
    in
    if List.length tds = List.length with_unboxed
    then
      Location.raise_errorf
        ~loc
        "Unused [~unboxed] flag: none of these types have an implicit unboxed version"
    else with_unboxed
;;

module Polytype = struct
  type t =
    { loc : Location.t
    ; vars : (string loc * Ppxlib_jane.jkind_annotation option) list
    ; body : core_type
    }

  let to_core_type
    ?(universally_quantify_only_if_jkind_annotation = false)
    { loc; vars; body }
    =
    let universally_quantify =
      match universally_quantify_only_if_jkind_annotation with
      | false -> true
      | true -> List.exists vars ~f:(fun (_name, jkind) -> Option.is_some jkind)
    in
    if universally_quantify
    then Ppxlib_jane.Ast_builder.Default.ptyp_poly ~loc vars body
    else body
  ;;
end

let consume_phantom_params phantom_attr td =
  let non_phantom_params =
    List.filter td.ptype_params ~f:(fun (param, _) ->
      not (Option.is_some (Attribute.get phantom_attr param)))
  in
  let td =
    { td with
      ptype_params =
        List.map td.ptype_params ~f:(fun (param, variance) ->
          Attribute.remove_seen Core_type [ T phantom_attr ] param, variance)
    }
  in
  td, non_phantom_params
;;

let combinator_type_of_type_declaration ?phantom_attr td ~f =
  (* We have to name the params first to avoid repeating the gensym for [ptyp_any]. *)
  let td = name_type_params_in_td td in
  let td, non_phantom_params =
    match phantom_attr with
    | Some attr -> consume_phantom_params attr td
    | None -> td, td.ptype_params
  in
  let result_type = core_type_of_type_declaration td in
  let result_type = f ~loc:td.ptype_name.loc result_type in
  let vars = List.map td.ptype_params ~f:Ppxlib_jane.get_type_param_name_and_jkind in
  let t =
    List.fold_right non_phantom_params ~init:result_type ~f:(fun (tp, _variance) acc ->
      let loc = tp.ptyp_loc in
      ptyp_arrow ~loc Nolabel (f ~loc tp) acc)
  in
  ({ loc = td.ptype_loc; vars; body = t } : Polytype.t)
;;
