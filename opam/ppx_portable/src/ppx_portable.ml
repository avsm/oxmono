open! Stdppx
open! Ppxlib
open Ppxlib.Ast_builder.Default
open Ppxlib_jane.Ast_builder.Default

let gensym prefix = gen_symbol ~prefix ()
let evar_loc { txt; loc } = evar ~loc txt
let pvar_loc { txt; loc } = pvar ~loc txt

let ghost =
  object
    inherit Ppxlib.Ast_traverse.map
    method! location l = { l with loc_ghost = true }
  end
;;

module Lazy_extension = struct
  let expand ~loc body =
    [%expr Ppx_portable_runtime.Portable_lazy.from_fun (fun () -> [%e body])]
  ;;
end

module Let_rec_extension = struct
  type value =
    | Custom of
        { of_portable_lazy : expression
        ; expr : expression
        }
    | Fun of
        { args : arg_label loc list
        ; expr : expression
        }
    | Lazy of expression

  type ty =
    | Typed of core_type
    | Untyped of { tyvar : string }

  type field =
    { loc : Ppxlib.location
    ; field_name : string loc
    ; value : value
    ; ty : ty
    }

  let custom_attr =
    Attribute.declare
      "portable.custom"
      Value_binding
      Ast_pattern.(single_expr_payload __)
      Fn.id
  ;;

  let create_field value_binding =
    let modes, value_binding =
      Ppxlib_jane.Shim.Value_binding.extract_modes value_binding
    in
    let loc = { value_binding.pvb_loc with loc_ghost = true } in
    if not (List.is_empty modes)
    then Location.raise_errorf ~loc "%%portable: modes on bindings are not supported";
    let ppat_desc =
      Ppxlib_jane.Shim.Pattern_desc.of_parsetree value_binding.pvb_pat.ppat_desc
    in
    let pexp_desc =
      Ppxlib_jane.Shim.Expression_desc.of_parsetree
        ~loc:value_binding.pvb_expr.pexp_loc
        value_binding.pvb_expr.pexp_desc
    in
    let field_name =
      match ppat_desc with
      | Ppat_var name | Ppat_constraint ({ ppat_desc = Ppat_var name; _ }, _, _) -> name
      | ppat_desc ->
        Location.raise_errorf
          ~loc
          "%%portable: expected an identifier, possibly with type constraint; got: %s"
          (Ppxlib_jane.Language_feature_name.of_pattern_desc ppat_desc)
    in
    let value =
      match Attribute.get custom_attr value_binding with
      | Some of_portable_lazy ->
        Custom { of_portable_lazy; expr = value_binding.pvb_expr }
      | None ->
        let rhs =
          match pexp_desc with
          | Pexp_constraint (body, _, _) -> body
          | _ -> value_binding.pvb_expr
        in
        (match rhs with
         | [%expr lazy%portable [%e? e]] -> Lazy e
         | _ ->
           (match
              Ppxlib_jane.Shim.Pexp_function.of_parsetree
                value_binding.pvb_expr.pexp_desc
                ~loc
            with
            | Some (params, _, body) ->
              let args =
                [ List.filter_map params ~f:(fun param ->
                    match param.pparam_desc with
                    | Pparam_newtype _ -> None
                    | Pparam_val (arg_label, _, pat) ->
                      Some
                        { txt = arg_label; loc = { pat.ppat_loc with loc_ghost = true } })
                ; (match body with
                   | Pfunction_body _ -> []
                   | Pfunction_cases (_, loc, _) ->
                     [ { txt = Nolabel; loc = { loc with loc_ghost = true } } ])
                ]
                |> List.concat
              in
              Fun { args; expr = value_binding.pvb_expr }
            | None ->
              Location.raise_errorf
                ~loc
                "%%portable: expected a [fun], [function], or [lazy%%portable] (possibly \
                 with type constraint); got: %s"
                (Ppxlib_jane.Language_feature_name.of_expression_desc pexp_desc)))
    in
    let ty =
      match ppat_desc, pexp_desc with
      | Ppat_constraint (_, Some core_type, _), _ -> Typed (ghost#core_type core_type)
      | _, Pexp_constraint (_, Some core_type, _) -> Typed (ghost#core_type core_type)
      | _ -> Untyped { tyvar = gensym "tyvar_" }
    in
    { loc; field_name; value; ty }
  ;;

  let create_type_definition ~loc ~fields =
    [ type_declaration
        ~loc
        ~name:{ loc; txt = gensym "_type_" }
        ~params:
          (List.filter_map fields ~f:(fun { loc; field_name = _; value = _; ty } ->
             match ty with
             | Typed _ -> None
             | Untyped { tyvar } -> Some (ptyp_var ~loc tyvar, (NoVariance, NoInjectivity))))
        ~cstrs:[]
        ~kind:
          (Ptype_record
             (List.map fields ~f:(fun { loc; field_name; value = _; ty } ->
                label_declaration
                  ~loc
                  ~name:field_name
                  ~mutable_:Immutable
                  ~modalities:[]
                  ~type_:
                    (match ty with
                     | Typed core_type -> core_type
                     | Untyped { tyvar } -> ptyp_var ~loc tyvar))))
        ~private_:Public
        ~manifest:None
        ()
    ]
    |> pstr_type ~loc Nonrecursive
  ;;

  let lambda_args ~loc:fn_loc ~args body =
    List.fold_right
      args
      ~init:body
      ~f:(fun (name, { loc = arg_loc; txt = arg_label }) body ->
        pexp_fun ~loc:fn_loc arg_label None (pvar ~loc:arg_loc name) body)
  ;;

  let apply_args ~loc ~args fn =
    List.map args ~f:(fun (name, { loc; txt = arg_label }) -> arg_label, evar ~loc name)
    |> pexp_apply ~loc fn
  ;;

  let field_of field_name = ghost#longident_loc (Located.map_lident field_name)

  let force_field ~loc ~lazy_name ~field_name =
    let record =
      [%expr Ppx_portable_runtime.Portable_lazy.force [%e evar_loc lazy_name]]
    in
    pexp_field ~loc record (field_of field_name)
  ;;

  let custom_of_lazy ~loc ~of_portable_lazy expr = eapply ~loc of_portable_lazy [ expr ]

  let nonrec_custom ~loc ~of_portable_lazy ~field_name ~lazy_name =
    force_field ~loc ~lazy_name ~field_name
    |> Lazy_extension.expand ~loc
    |> custom_of_lazy ~loc ~of_portable_lazy
  ;;

  let nonrec_function ~loc ~field_name ~args ~lazy_name =
    let args = List.map args ~f:(fun arg -> gensym "_x_", arg) in
    force_field ~loc ~lazy_name ~field_name
    |> apply_args ~loc ~args
    |> lambda_args ~loc ~args
  ;;

  let nonrec_lazy ~loc ~field_name ~lazy_name =
    [%expr
      Ppx_portable_runtime.Portable_lazy.force
        [%e force_field ~loc ~lazy_name ~field_name]]
    |> Lazy_extension.expand ~loc
  ;;

  let ignore_fields ~loc ~fields =
    List.map fields ~f:(fun { loc = _; field_name; value = _; ty = _ } ->
      [%expr Ppx_portable_runtime.ignore [%e evar_loc field_name]])
    |> esequence ~loc
  ;;

  type expanded =
    { type_defn : structure_item
    ; record_pat : pattern
    ; record_expr : expression
    ; ignore_expr : expression
    }

  let expand_internal ~loc rec_binds =
    let fields = List.map rec_binds ~f:create_field in
    let type_defn = create_type_definition ~loc ~fields in
    let record_pat =
      let alist =
        List.map fields ~f:(fun { loc = _; field_name; value = _; ty = _ } ->
          field_of field_name, pvar_loc field_name)
      in
      ppat_record ~loc alist Closed
    in
    let lazy_name = { loc; txt = gensym "_portable_lazy_" } in
    let nonrec_binds =
      List.map fields ~f:(fun { loc; field_name; value; ty } ->
        let pat =
          match ty with
          | Untyped _ -> pvar_loc field_name
          | Typed ty -> ppat_constraint ~loc (pvar_loc field_name) (Some ty) []
        in
        let expr =
          match value with
          | Custom { of_portable_lazy; expr = _ } ->
            nonrec_custom ~loc ~of_portable_lazy ~field_name ~lazy_name
          | Fun { args; expr = _ } -> nonrec_function ~loc ~args ~field_name ~lazy_name
          | Lazy _ -> nonrec_lazy ~loc ~field_name ~lazy_name
        in
        value_binding ~loc ~pat:(ghost#pattern pat) ~expr ~modes:[])
    in
    let inner_record_expr =
      let alist =
        List.map fields ~f:(fun { loc; field_name; value; ty = _ } ->
          ( field_of field_name
          , match value with
            | Custom { of_portable_lazy = _; expr } -> expr
            | Fun { args = _; expr } -> expr
            | Lazy expr -> Lazy_extension.expand ~loc expr ))
      in
      pexp_record ~loc alist None |> pexp_let ~loc Immutable Nonrecursive nonrec_binds
    in
    let record_expr =
      [%expr
        Ppx_portable_runtime.Portable_lazy.force
          (Ppx_portable_runtime.Portable_lazy.from_fun_fixed
             (fun [%p pvar_loc lazy_name] -> [%e inner_record_expr]))]
    in
    let ignore_expr = ignore_fields ~loc ~fields in
    { type_defn; record_pat; record_expr; ignore_expr }
  ;;

  let expand ~loc rec_binds body =
    let { type_defn; record_pat; record_expr; ignore_expr } =
      expand_internal ~loc rec_binds
    in
    [%expr
      let open struct
        [%%i type_defn]
      end in
      let [%p record_pat] = [%e record_expr] in
      let () = [%e ignore_expr] in
      [%e body]]
  ;;

  let expand_str_item ~loc rec_binds =
    let { type_defn; record_pat; record_expr; ignore_expr } =
      expand_internal ~loc rec_binds
    in
    [%stri
      include struct
        open struct
          [%%i type_defn]
        end

        let [%p record_pat] = [%e record_expr]
        let () = [%e ignore_expr]
      end]
  ;;
end

let expr_extension =
  Extension.V3.declare
    "portable.portable"
    Expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt expr ->
      let loc =
        { (Ppxlib.Expansion_context.Extension.extension_point_loc ctxt) with
          loc_ghost = true
        }
      in
      try
        match
          Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:expr.pexp_loc expr.pexp_desc
        with
        | Pexp_lazy body -> Lazy_extension.expand ~loc body
        | Pexp_let (Immutable, Recursive, binds, body) ->
          Let_rec_extension.expand ~loc binds body
        | Pexp_let (_, Nonrecursive, _, _) ->
          Location.raise_errorf
            ~loc
            "%%portable: expected [let rec], got: nonrecursive [let]"
        | Pexp_let (Mutable, _, _, _) ->
          Location.raise_errorf
            ~loc
            "%%portable: expected immutable [let rec], got: mutable [let rec]"
        | pexp_desc ->
          Location.raise_errorf
            ~loc
            "%%portable: expected [lazy] or [let rec], got: %s"
            (Ppxlib_jane.Language_feature_name.of_expression_desc pexp_desc)
      with
      | Location.Error error -> pexp_extension ~loc (Location.Error.to_extension error))
;;

(* Hack: ppxlib inserts extra binds for ppx-generated code. E.g. if a ppx generates:

   [%%portable let rec f x = x and g x = x]

   ppxlib will rewrite it to this, to suppress w32 (unused bindings):

   [%%portable let rec f x = x and g x = x let _ = f and _ = g]

   This function checks whether the trailing bindings are suitable for being ignored,
   because they were plausibly generated by ppxlib in this way.
*)
let assert_only_w32_suppression_binds tail =
  List.iter tail ~f:(fun tail_item ->
    match tail_item.pstr_desc with
    | Pstr_value (Nonrecursive, tail_binds)
      when List.for_all tail_binds ~f:(fun tail_binds ->
             match tail_binds.pvb_pat with
             | { ppat_desc = Ppat_any; _ } -> true
             | _ -> false) -> ()
    | _ ->
      Location.raise_errorf
        ~loc:tail_item.pstr_loc
        "%%portable: expected single [let rec], possibly ignored by [ppxlib]; got: \
         trailing structure item")
;;

let str_item_extension =
  Extension.V3.declare
    "portable.portable"
    Structure_item
    Ast_pattern.(pstr (__ ^:: __))
    (fun ~ctxt item tail ->
      let loc =
        { (Ppxlib.Expansion_context.Extension.extension_point_loc ctxt) with
          loc_ghost = true
        }
      in
      try
        match Ppxlib_jane.Shim.Structure_item_desc.of_parsetree item.pstr_desc with
        | Pstr_value (Recursive, binds) ->
          assert_only_w32_suppression_binds tail;
          Let_rec_extension.expand_str_item ~loc binds
        | Pstr_value (Nonrecursive, _) ->
          Location.raise_errorf
            ~loc
            "%%portable: expected [let rec], got: nonrecursive [let]"
        | pstr_desc ->
          Location.raise_errorf
            ~loc
            "%%portable: expected [let rec], got: %s"
            (Ppxlib_jane.Language_feature_name.of_structure_item_desc pstr_desc)
      with
      | Location.Error error -> pstr_extension ~loc (Location.Error.to_extension error) [])
;;

let registered =
  Driver.register_transformation
    "portable"
    ~extensions:[ expr_extension; str_item_extension ]
;;
