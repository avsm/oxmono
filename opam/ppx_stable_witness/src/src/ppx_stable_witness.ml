open! Base
open Ppxlib
open Ast_builder.Default

let ghost loc = { loc with loc_ghost = true }

let strip_locs =
  (* Replace locations with a dummy so that syntax can be compared for equality. *)
  object
    inherit Ast_traverse.map
    method! location _ = Location.none
  end
;;

let copy =
  (* Strip off attributes and mark all locations as ghost so that a copy of syntax
     (usually a type) can be included in generated code. *)
  object
    inherit Ast_traverse.map
    method! attributes _ = []
    method! location = ghost
  end
;;

let custom_attr =
  Attribute.declare
    "stable_witness.custom"
    Core_type
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    Fn.id
;;

let stable_witness_name = function
  | "t" -> "stable_witness"
  | type_name -> "stable_witness_" ^ type_name
;;

let stable_witness_type ~loc core_type =
  [%type: [%t copy#core_type core_type] Ppx_stable_witness_runtime.Stable_witness.t]
;;

let stable_witness_variable var = "__'" ^ var ^ "_stable_witness"

module Value_binding = struct
  let compare a b = Comparable.lift Poly.compare ~f:strip_locs#value_binding a b
end

module Signature = struct
  let expand_type_declaration td ~portable =
    let td = name_type_params_in_td td in
    let loc = ghost td.ptype_loc in
    Ppxlib_jane.Ast_builder.Default.value_description
      ~loc
      ~name:(Loc.map ~f:stable_witness_name td.ptype_name)
      ~type_:(combinator_type_of_type_declaration td ~f:stable_witness_type)
      ~modalities:(if portable then Ppxlib_jane.Shim.Modalities.portable ~loc else [])
      ~prim:[]
    |> psig_value ~loc
  ;;

  let expand ~loc:_ ~path:_ (_, tds) ~portable : signature_item list =
    List.map tds ~f:(expand_type_declaration ~portable)
  ;;
end

module Structure = struct
  (* We generate [let] clauses to check types of stable witnesses that the current one
     depends on. *)
  let check ~loc witness_expr witness_type =
    value_binding ~loc ~pat:[%pat? (_ : [%t witness_type])] ~expr:witness_expr
  ;;

  let unsupported ~loc description =
    (* Rather than raise at expansion time, we generate a [%error] node that can complain
       during compile time. This is more merlin-friendly, among other benefits. *)
    let message = Printf.sprintf "ppx_stable_witness: %s not supported" description in
    check ~loc [%expr [%ocaml.error [%e estring ~loc message]]] [%type: _]
  ;;

  let check_type_constructor ~loc id params =
    let witness_expr = unapplied_type_constr_conv ~loc id ~f:stable_witness_name in
    let witness_type =
      List.fold_right
        params
        ~init:(stable_witness_type ~loc (ptyp_constr ~loc id params))
        ~f:(fun param core_type ->
          let loc = ghost param.ptyp_loc in
          ptyp_arrow ~loc Nolabel (stable_witness_type ~loc param) core_type)
    in
    check ~loc witness_expr witness_type
  ;;

  let check_type_variable ~loc var =
    let witness_expr = evar ~loc (stable_witness_variable var) in
    let witness_type = stable_witness_type ~loc (ptyp_var ~loc var) in
    check ~loc witness_expr witness_type
  ;;

  let rec check_core_type core_type =
    let loc = ghost core_type.ptyp_loc in
    match Attribute.get custom_attr core_type with
    | Some expr -> [ check ~loc expr (stable_witness_type ~loc core_type) ]
    | None ->
      (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
       | Ptyp_var (var, _) -> [ check_type_variable ~loc var ]
       | Ptyp_tuple tuple -> List.concat_map tuple ~f:(fun (_, t) -> check_core_type t)
       | Ptyp_constr (id, params) ->
         check_type_constructor ~loc id params
         :: List.concat_map params ~f:check_core_type
       | Ptyp_alias (core_type, _, _) -> check_core_type core_type
       | Ptyp_variant (rows, _, _) -> List.concat_map rows ~f:check_row_field
       | unsupported_type ->
         [ unsupported
             ~loc
             (Ppxlib_jane.Language_feature_name.of_core_type_desc unsupported_type)
         ])

  and check_row_field row =
    match row.prf_desc with
    | Rtag (_, _, core_types) -> List.concat_map ~f:check_core_type core_types
    | Rinherit core_type -> check_core_type core_type
  ;;

  let check_label_declaration ld = check_core_type ld.pld_type

  let check_constructor_declaration cd =
    match cd.pcd_res with
    | Some _ -> [ unsupported ~loc:cd.pcd_loc "GADT" ]
    | None ->
      (match cd.pcd_args with
       | Pcstr_tuple tuple ->
         List.concat_map
           ~f:(fun arg ->
             let type_ = Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg in
             check_core_type type_)
           tuple
       | Pcstr_record record -> List.concat_map ~f:check_label_declaration record)
  ;;

  let param_patterns td =
    List.map td.ptype_params ~f:(fun param ->
      let core_type = fst param in
      let loc = ghost core_type.ptyp_loc in
      ppat_constraint
        ~loc
        (pvar ~loc (stable_witness_variable (get_type_param_name param).txt))
        (stable_witness_type ~loc core_type))
  ;;

  (* Generate all the witness type checks for a type declaration. *)
  let check_type_declaration td =
    let loc = ghost td.ptype_loc in
    let pat = pvar ~loc ("__stable_witness_checks_for_" ^ td.ptype_name.txt ^ "__") in
    let checks =
      match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
      | Ptype_open -> [ unsupported ~loc "open type" ]
      | Ptype_record fields -> List.concat_map fields ~f:check_label_declaration
      | Ptype_record_unboxed_product _ -> [ unsupported ~loc "unboxed record type" ]
      | Ptype_variant clauses -> List.concat_map clauses ~f:check_constructor_declaration
      | Ptype_abstract ->
        (match td.ptype_manifest with
         | None -> []
         | Some core_type -> check_core_type core_type)
    in
    let checks =
      (* Don't bother generating obviously redundant checks. *)
      List.stable_dedup ~compare:Value_binding.compare checks
    in
    match List.is_empty checks with
    | true -> []
    | false ->
      (* If there are any witnesses to check, we generate a function parameterized by any
         arguments to the current witness and a unit argument. Since this is always a
         lambda, it is safe inside [let rec]. *)
      let expr =
        eunit ~loc
        |> pexp_let ~loc Nonrecursive checks
        |> eabstract ~loc (param_patterns td @ [ punit ~loc ])
      in
      [ value_binding ~loc ~pat ~expr ]
  ;;

  let assert_witness_expr ~loc =
    [%expr Ppx_stable_witness_runtime.Stable_witness.assert_stable]
  ;;

  (* Create a stable witness for a type we trust to be stable. Evalutes to a variable
     reference so that it is safe inside [let rec]. *)
  let assert_witness_for core_type =
    let loc = ghost core_type.ptyp_loc in
    pexp_constraint ~loc (assert_witness_expr ~loc) (stable_witness_type ~loc core_type)
  ;;

  let assert_witness_with_params_for core_type ~params =
    match params with
    | [] -> assert_witness_for core_type
    | params ->
      let loc = ghost core_type.ptyp_loc in
      Ppxlib_jane.Ast_builder.Default.eabstract
        params
        ~loc
        ~return_constraint:(stable_witness_type ~loc core_type)
        (assert_witness_expr ~loc)
  ;;

  (* Generate the actual stable witness definition for a type declaration. *)
  let expand_type_declaration td =
    let loc = ghost td.ptype_loc in
    let expr =
      List.map td.ptype_params ~f:fst
      |> ptyp_constr ~loc (Located.map_lident td.ptype_name)
      |> assert_witness_with_params_for ~params:(param_patterns td)
    in
    let pat = pvar ~loc:td.ptype_name.loc (stable_witness_name td.ptype_name.txt) in
    value_binding ~loc ~pat ~expr
  ;;

  let shadows_self_without_redefining td =
    match td.ptype_manifest with
    | Some { ptyp_desc = Ptyp_constr ({ txt = Lident name; _ }, params); _ }
      when String.equal name td.ptype_name.txt ->
      (match
         List.for_all2 params td.ptype_params ~f:(fun actual (formal, _) ->
           match
             ( Ppxlib_jane.Shim.Core_type_desc.of_parsetree actual.ptyp_desc
             , Ppxlib_jane.Shim.Core_type_desc.of_parsetree formal.ptyp_desc )
           with
           | Ptyp_var (a, _), Ptyp_var (b, _) -> String.equal a b
           | _ -> false)
       with
       | Ok bool -> bool
       | Unequal_lengths -> false)
    | _ -> false
  ;;

  class refers_to_redefined_type tds =
    object
      (* We pass [Recursive] here, even though this is only actually called on
         non-recursive definitions. What we really want to check for is whether a type
         refers to its own name, not whether it's recursive. This is equivalent to
         [type_is_recursive Recursive], so we use that even though it reads wrong. *)
      inherit type_is_recursive Recursive tds

      val! type_names =
        List.filter_map tds ~f:(fun td ->
          match shadows_self_without_redefining td with
          | true ->
            (* No need to check for references to types that redefine a name to precisely
               what it was before. *)
            None
          | false ->
            (* Anything else, we need to look for. *)
            Some td.ptype_name.txt)
    end

  let refers_to_redefined_type tds =
    let obj = new refers_to_redefined_type tds in
    match obj#go () with
    | Nonrecursive -> false
    | Recursive -> true
  ;;

  (* Define both the stable witness, and all the checks, for a type declaration. Define
     them both in the same [let] with a shared [rec_flag] so that the checks have the same
     scope as the stable witness itself. *)
  let expand ~loc ~path:_ (rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    match rec_flag with
    | Nonrecursive when refers_to_redefined_type tds ->
      let message =
        Printf.sprintf
          "ppx_stable_witness: This definition shadows a type that it also refers to. \
           Expanded code needs to refer to the shadowed name.\n\n\
           Perhaps you can define an alias for the shadowed type, then use the alias in \
           this definition."
      in
      [%str [%%ocaml.error [%e estring ~loc message]]]
    | _ ->
      let rec_flag = really_recursive rec_flag tds in
      List.concat
        [ List.map tds ~f:expand_type_declaration
        ; List.concat_map tds ~f:check_type_declaration
        ]
      |> pstr_value_list ~loc rec_flag
  ;;

  (* Expand a single type to an expression containing its checks and a stable witness. *)
  let extension ~loc ~path:_ core_type =
    let checks = check_core_type core_type in
    let body = assert_witness_for core_type in
    match List.is_empty checks with
    | true -> body
    | false -> pexp_let ~loc Nonrecursive checks body
  ;;
end

let extension = Structure.extension

let sig_type_decl =
  Deriving.Generator.make
    Deriving.Args.(empty +> flag "portable")
    (fun ~loc ~path tds portable -> Signature.expand ~loc ~path tds ~portable)
;;

let str_type_decl = Deriving.Generator.make_noarg Structure.expand

let () =
  Deriving.add "stable_witness" ~sig_type_decl ~str_type_decl ~extension
  |> Deriving.ignore
;;
