open! Base
open! Ppxlib
module Builder = Ppxlib.Ast_builder.Default

(* Same as combinator_type_of_type_declaration except the return type doesn't have to be
   the same as the type parameter type.

   For example, we can use combinator_type_of_type_declaration to derive xml_of_t for
   ['a t] because the type of that would be [('a -> Xml.element) -> 'a t -> Xml.element],
   but we can't use it to deriving t_of_xml because the type would be
   ['a Ppx_simple_xml_conv_lib.Of_xml.t -> Xml.element -> t].
*)
let combinator_type_of_type_declaration_hybrid
  (type_declaration : type_declaration)
  ~init
  ~f
  =
  let loc = type_declaration.ptype_loc in
  List.fold_right type_declaration.ptype_params ~init ~f:(fun (type_, _) func ->
    Builder.ptyp_arrow ~loc Nolabel (f ~loc:type_.ptyp_loc type_) func)
;;

let constant_or_variable_to_expression v ~loc =
  match (v : Attributes.Constant_or_variable.t) with
  | Constant s -> Builder.estring s ~loc
  | Variable txt -> Builder.pexp_ident ~loc { loc; txt }
;;

module Namespace = Attributes.Namespace

module Parameters = struct
  module Tag_deserializer = struct
    type t =
      { tag : expression
      ; namespace : Namespace.t
      ; allow_extra_elements : bool
      ; allow_extra_attributes : bool
      }
  end

  module Tag_serializer = struct
    type t =
      { tag : expression
      ; namespace : expression option
      ; prefix_declarations : expression option
      }
  end

  type 'tag t =
    | Tag of 'tag
    | Inlined
    | No_parameters

  module For_records = struct
    type 'tag t =
      | Tag of 'tag
      | Inlined
  end

  let for_records loc = function
    | Tag tag -> For_records.Tag tag
    | Inlined -> Inlined
    | No_parameters ->
      Location.raise_errorf
        ~loc
        "ppx_simple_xml_conv: record types need either [tag] or [inlined] provided in \
         the implementation"
  ;;

  let create_deserializer
    loc
    ~tag
    ~inlined
    ~allow_extra_elements
    ~allow_extra_attributes
    ~namespace
    =
    match tag with
    | Some tag ->
      if inlined
      then
        Location.raise_errorf
          ~loc
          "ppx_simple_xml_conv: cannot specify both [tag] and [inlined]";
      Tag
        { Tag_deserializer.tag; allow_extra_elements; allow_extra_attributes; namespace }
    | None ->
      if allow_extra_elements
      then
        Location.raise_errorf
          ~loc
          "ppx_simple_xml_conv: cannot specify [allow_extra_elements] without [tag]";
      if allow_extra_attributes
      then
        Location.raise_errorf
          ~loc
          "ppx_simple_xml_conv: cannot specify [allow_extra_attributes] without [tag]";
      (match (namespace : Namespace.t) with
       | Do_not_care -> ()
       | Assert_no_namespace ->
         Location.raise_errorf
           ~loc
           "ppx_simple_xml_conv: cannot specify [assert_no_namespace] without [tag]"
       | Namespace _ ->
         Location.raise_errorf
           ~loc
           "ppx_simple_xml_conv: cannot specify [namespace] without [tag]");
      if inlined then Inlined else No_parameters
  ;;

  let create_serializer loc ~tag ~inlined ~prefix_declarations ~namespace
    : Tag_serializer.t t
    =
    match tag with
    | Some (tag : expression) ->
      if inlined
      then
        Location.raise_errorf
          ~loc
          "ppx_simple_xml_conv: cannot specify both [tag] and [inlined]";
      Tag { Tag_serializer.tag; prefix_declarations; namespace }
    | None ->
      if Option.is_some prefix_declarations
      then
        Location.raise_errorf
          ~loc
          "ppx_simple_xml_conv: cannot specify [prefixes] without [tag]";
      if Option.is_some namespace
      then
        Location.raise_errorf
          ~loc
          "ppx_simple_xml_conv: cannot specify [namespace] without [tag]";
      if inlined then Inlined else No_parameters
  ;;
end

module Requested = struct
  type t =
    { of_xml : bool
    ; to_xml : bool
    }

  let validate t context loc ~name =
    match context with
    | `Of_xml when not t.of_xml ->
      Location.raise_errorf
        ~loc
        "ppx_simple_xml_conv: %S not allowed in this context, did you mean to derive \
         of_xml or xml?"
        name
    | `To_xml when not t.to_xml ->
      Location.raise_errorf
        ~loc
        "ppx_simple_xml_conv: %S not allowed in this context, did you mean to derive \
         to_xml or xml?"
        name
    | _ -> ()
  ;;

  let validate_opt t context loc ~name = function
    | None -> ()
    | Some _ -> validate t context loc ~name
  ;;

  let validate_bool t context loc ~name bool = if bool then validate t context loc ~name

  let validate_namespace t (namespace : Namespace.t) loc =
    match namespace with
    | Do_not_care | Namespace _ -> ()
    | Assert_no_namespace ->
      if not t.of_xml
      then
        Location.raise_errorf
          ~loc
          "ppx_simple_xml_conv: [assert_no_namespace] is not allowed in this context, \
           did you mean to derive of_xml or xml? (for serialization, if you don't \
           specify [namespace], none will be used)"
  ;;
end

let rec string_fn_from_core_type (ty : core_type) ~name ~string_fn_from_type =
  let loc = { ty.ptyp_loc with loc_ghost = true } in
  match ty.ptyp_desc with
  | Ptyp_constr (longident, type_parameters) ->
    (* Types of the form ('a1, ..., 'an) t, where 0 <= n (i.e. types of the form t are
       allowed) *)
    let arguments =
      List.map ~f:(string_fn_from_core_type ~name ~string_fn_from_type) type_parameters
    in
    Builder.type_constr_conv ~loc longident ~f:string_fn_from_type arguments
  | _ ->
    Location.raise_errorf
      ~loc
      "ppx_simple_xml_conv: type not supported in deriving %s"
      name
;;

module Namer = struct
  type t =
    { namer : string -> string
    ; type_argument_namer : string -> string
    }

  let reduce t = { t with namer = t.type_argument_namer }
end

let rec process_core_type
  (ty : core_type)
  ~requested
  ~type_params
  ~make_leaf
  ~make_empty
  ~namer
  =
  let loc = { ty.ptyp_loc with loc_ghost = true } in
  (* If we have a custom type parser (e.g. of_string), we don't care what the type looks
     like. Otherwise: we just support type aliases for now. *)
  match Attributes.Core_type.handle ty ~loc with
  | From_type ->
    (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ty.ptyp_desc with
     | Ptyp_constr (longident, type_parameters) ->
       (* Types of the form ('a1, ..., 'an) t, where 0 <= n (i.e. types of the form t are
          allowed) *)
       let arguments =
         List.map
           ~f:
             (process_core_type
                ~type_params
                ~make_leaf
                ~namer:(Namer.reduce namer)
                ~make_empty
                ~requested)
           type_parameters
       in
       Builder.type_constr_conv ~loc longident ~f:namer.namer arguments
     | Ptyp_var (name, _) ->
       (match Map.find type_params name with
        | None ->
          Location.raise_errorf ~loc "ppx_simple_xml_conv: unknown type variable %s" name
        | Some expression -> Builder.evar ~loc expression)
     | _ -> Location.raise_errorf ~loc "ppx_simple_xml_conv: type not supported")
  | Leaf { tag; of_string; to_string; ignore_attributes; preserve_space; namespace } ->
    Requested.validate_opt requested `To_xml loc ~name:"to_string" to_string;
    Requested.validate_opt requested `Of_xml loc ~name:"of_string" of_string;
    Requested.validate_bool
      requested
      `Of_xml
      loc
      ~name:"ignore_attributes"
      ignore_attributes;
    Requested.validate_bool requested `Of_xml loc ~name:"preserve_space" preserve_space;
    Requested.validate_namespace requested namespace loc;
    make_leaf
      ty
      loc
      ~tag
      ~of_string
      ~to_string
      ~ignore_attributes
      ~preserve_space
      ~namespace
  | Empty { tag; ignore_children; ignore_attributes; namespace } ->
    Requested.validate_bool
      requested
      `Of_xml
      loc
      ~name:"ignore_attributes"
      ignore_attributes;
    Requested.validate_namespace requested namespace loc;
    Requested.validate_bool requested `Of_xml loc ~name:"ignore_children" ignore_children;
    make_empty loc ~tag ~ignore_children ~ignore_attributes ~namespace
;;

(* Only records allow the ~tag, ~inlined, ~allow_extra_elements, and
   ~allow_extra_attributes derivers. *)
let validate_non_record ~loc ~type_type = function
  | Parameters.No_parameters -> ()
  | Tag _ ->
    Location.raise_errorf
      ~loc
      "ppx_simple_xml_conv: %s types should not specify a tag"
      type_type
  | Inlined ->
    Location.raise_errorf
      ~loc
      "ppx_simple_xml_conv: %s types should not have [inlined]"
      type_type
;;

module Variant_common = struct
  let parse_empty_variant constructor_declaration ~requested ~name ~loc ~make_branch =
    match Attributes.Variant.handle constructor_declaration ~loc with
    | From_type ->
      Location.raise_errorf
        ~loc
        "ppx_simple_xml_conv: empty variants need the attribute [@xml.empty <tag>]"
    | Empty ({ tag = _; ignore_attributes; ignore_children; namespace } as empty) ->
      Requested.validate_bool
        requested
        `Of_xml
        loc
        ~name:"ignore_attributes"
        ignore_attributes;
      Requested.validate_bool
        requested
        `Of_xml
        loc
        ~name:"ignore_children"
        ignore_children;
      Requested.validate_namespace requested namespace loc;
      make_branch loc ~name (`Empty empty)
  ;;

  let parse_single_type_variant constructor_declaration type_ ~name ~loc ~make_branch =
    match Attributes.Variant.handle constructor_declaration ~loc with
    | Empty _ ->
      Location.raise_errorf
        ~loc
        "ppx_simple_xml_conv: [@xml.empty _] not supported on variants with types. Did \
         you mean to use [@xml.from_string _]?"
    | From_type -> make_branch loc ~name (`Type type_)
  ;;

  let handle_constructor_declaration
    (constructor_declaration : constructor_declaration)
    ~requested
    ~make_branch
    =
    let loc = constructor_declaration.pcd_name.loc in
    let name = constructor_declaration.pcd_name.txt in
    (match constructor_declaration.pcd_res with
     | None -> ()
     | Some core_type ->
       Location.raise_errorf
         ~loc:core_type.ptyp_loc
         "ppx_simple_xml_conv: GADTs not supported");
    match constructor_declaration.pcd_args with
    | Pcstr_tuple [] ->
      parse_empty_variant constructor_declaration ~name ~loc ~requested ~make_branch
    | Pcstr_tuple [ arg ] ->
      let type_ = Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg in
      parse_single_type_variant constructor_declaration type_ ~name ~loc ~make_branch
    | Pcstr_tuple _ ->
      Location.raise_errorf
        ~loc
        "ppx_simple_xml_conv: only variants with exactly one argument are currently \
         supported"
    | Pcstr_record _ ->
      Location.raise_errorf
        ~loc
        "ppx_simple_xml_conv: inline records are not currently supported"
  ;;
end

module Record_common = struct
  module Named = struct
    type 'a t =
      { content : 'a
      ; field_name : longident_loc
      }
  end

  module Non_leaf = struct
    type 'a t =
      | Attribute of 'a Named.t
      | Element of 'a Named.t
      | Inlined of 'a Named.t
  end

  module Leaf = struct
    type 'a t =
      { attributes : 'a Named.t list
      ; content : 'a Named.t
      }
  end

  type 'a t =
    | Non_leaf of 'a Non_leaf.t list
    | Leaf of 'a Leaf.t

  let handle ~requested ~loc label_declaration ~(parameters : _ Parameters.For_records.t) =
    let attribute = Attributes.Record.handle ~loc label_declaration in
    (match attribute with
     | Attribute { namespace; _ } | Boolean_element { namespace; _ } ->
       Requested.validate_namespace requested namespace loc
     | Element _ | Content _ | Inlined _ -> ());
    (match attribute with
     | Element { count = Default (_ : expression); type_ = (_ : core_type) }
     | Attribute
         { count = Default (_ : expression)
         ; key = (_ : Attributes.Constant_or_variable.t)
         ; namespace = _
         ; of_string = _
         ; to_string = _
         } -> Requested.validate requested `Of_xml loc ~name:"default"
     | Attribute
         { count = Required | Option
         ; key = (_ : Attributes.Constant_or_variable.t)
         ; namespace = _
         ; of_string
         ; to_string
         } ->
       (match of_string with
        | Custom _ -> Requested.validate requested `Of_xml loc ~name:"of_string"
        | Derive_from_type _ -> ());
       (match to_string with
        | Custom _ -> Requested.validate requested `To_xml loc ~name:"to_string"
        | Derive_from_type _ -> ())
     | Boolean_element
         { tag = _; namespace = _; values = _; ignore_children; ignore_attributes } ->
       Requested.validate_bool
         requested
         `Of_xml
         loc
         ~name:"ignore_children"
         ignore_children;
       Requested.validate_bool
         requested
         `Of_xml
         loc
         ~name:"ignore_attributes"
         ignore_attributes
     | Element { count = Required | Option | List; type_ = (_ : core_type) } -> ()
     | Content (_ : core_type) ->
       (match parameters with
        | Inlined ->
          Location.raise_errorf
            ~loc
            "Can't use the %s attribute when deriving inlined"
            Attributes.content_attribute_name
        | Tag _ -> ())
     | Inlined (_ : core_type) -> ());
    attribute
  ;;

  let reduce_parsers parsers =
    let error_mix_content_other ~content ~other ~other_type =
      Location.raise_errorf
        ~loc:content.Named.field_name.loc
        "Field %s specified as text content field, field %s is being parsed as %s. Text \
         content parsers and %s parsers can't mix."
        (Longident.name content.Named.field_name.txt)
        (Longident.name other.Named.field_name.txt)
        other_type
        other_type
        ()
    in
    let error_mix_content_element ~content ~element =
      error_mix_content_other ~content ~other:element ~other_type:"child element"
    in
    let error_mix_content_inlined ~content ~inlined =
      error_mix_content_other ~content ~other:inlined ~other_type:"inlined"
    in
    List.fold parsers ~init:(Non_leaf []) ~f:(fun struct_parser (type_, parser) ->
      match type_, struct_parser with
      | `Attribute, Non_leaf non_leafs -> Non_leaf (Attribute parser :: non_leafs)
      | `Attribute, Leaf { content; attributes } ->
        Leaf { content; attributes = parser :: attributes }
      | `Element, Non_leaf non_leafs -> Non_leaf (Element parser :: non_leafs)
      | `Inlined, Non_leaf non_leafs -> Non_leaf (Inlined parser :: non_leafs)
      | `Content, Non_leaf maybe_non_leafs ->
        let attributes =
          List.map maybe_non_leafs ~f:(function
            | Attribute attribute -> attribute
            | Element element -> error_mix_content_element ~content:parser ~element
            | Inlined inlined -> error_mix_content_inlined ~content:parser ~inlined)
        in
        Leaf { content = parser; attributes }
      | `Element, Leaf { content; _ } ->
        error_mix_content_element ~content ~element:parser
      | `Inlined, Leaf { content; _ } ->
        error_mix_content_inlined ~content ~inlined:parser
      | `Content, Leaf { content; _ } ->
        Location.raise_errorf
          ~loc:content.field_name.loc
          "Field %s specified as text content field, but another field %s is being \
           parsed as a text content field. Can only have one text content field."
          (Longident.name content.field_name.txt)
          (Longident.name parser.field_name.txt)
          ())
  ;;
end

let single_var loc =
  let symbol = gen_symbol () in
  Builder.pvar symbol ~loc, Builder.evar symbol ~loc
;;

let single_var_function loc ~f =
  let pattern, expression = single_var loc in
  [%expr fun [%p pattern] -> [%e f expression]]
;;

let handle_type_declaration
  (type_declaration : Parsetree.type_declaration)
  ~handle_variant
  ~handle_record
  ~handle_core_type
  ~f
  =
  let type_declaration = name_type_params_in_td type_declaration in
  let { ptype_name = { txt = type_name; loc = _ }
      ; ptype_loc = loc
      ; ptype_params
      ; ptype_manifest
      ; _
      }
    =
    type_declaration
  in
  (* We rev map because we will fold later to add arguments, and we want last argument
     first. *)
  let type_param_list =
    List.map ptype_params ~f:(fun param ->
      let name = get_type_param_name param in
      name.txt, gen_symbol ~prefix:"_type_param" ())
  in
  let type_params = Map.of_alist_exn (module String) type_param_list in
  let body =
    match Ppxlib_jane.Shim.Type_kind.of_parsetree type_declaration.ptype_kind with
    | Ptype_variant constructor_declarations ->
      handle_variant constructor_declarations ~loc ~type_params
    | Ptype_record label_declarations ->
      handle_record label_declarations ~loc ~type_params
    | Ptype_record_unboxed_product _ ->
      Location.raise_errorf ~loc "ppx_simple_xml_conv: unboxed record types not supported"
    | Ptype_open ->
      Location.raise_errorf ~loc "ppx_simple_xml_conv: open types not supported"
    | Ptype_abstract ->
      (match ptype_manifest with
       | None ->
         Location.raise_errorf ~loc "ppx_simple_xml_conv: abstract types not supported"
       | Some core_type -> handle_core_type core_type ~loc ~type_params)
  in
  let type_param_patterns =
    List.map type_param_list ~f:(fun (_, symbol) -> Builder.pvar ~loc symbol)
  in
  let type_param_values =
    List.map type_param_list ~f:(fun (_, symbol) -> Builder.evar ~loc symbol)
  in
  f loc ~type_name ~type_param_patterns ~type_param_values ~body
;;

let extract_type_declaration_and_recursion_flag ~loc (rec_flag, type_declarations) =
  (* Check if the type is really recursive, else the compiler raises an error about a
     redundant rec flag *)
  let rec_flag = (new type_is_recursive rec_flag type_declarations)#go () in
  let type_declaration =
    match type_declarations with
    | [ type_declaration ] -> type_declaration
    | _ ->
      Location.raise_errorf ~loc "ppx_simple_xml_conv: multiple types not yet supported"
  in
  rec_flag, type_declaration
;;

module Serializer = struct
  module Namers = struct
    let xml_of_function ty = "xml_of_" ^ ty
    let inlined_xml_of_function ty = "inlined_xml_of_" ^ ty
    let xml_of = { Namer.namer = xml_of_function; type_argument_namer = xml_of_function }

    let inlined_xml_of =
      { Namer.namer = inlined_xml_of_function; type_argument_namer = xml_of_function }
    ;;

    let to_string = function
      | "t" -> "to_string"
      | ty -> ty ^ "_to_string"
    ;;
  end

  let make_element loc tag ?(attributes = []) ~namespace children ~generate =
    let element =
      [%expr
        { Ppx_simple_xml_conv_lib.Xml.tag =
            { ns = [%e Namespace.to_value ~loc namespace]
            ; tag = [%e constant_or_variable_to_expression ~loc tag]
            }
        ; attributes =
            [%e
              List.map attributes ~f:(fun (ns, key, value) ->
                [%expr
                  { ns = [%e Namespace.to_value ~loc ns]
                  ; key = [%e key]
                  ; value = [%e value]
                  }])
              |> Builder.elist ~loc]
        ; children = [%e Builder.elist ~loc children]
        }]
    in
    match generate with
    | `Element -> element
    | `Xml -> [%expr Ppx_simple_xml_conv_lib.Xml.Element [%e element]]
  ;;

  let to_string_from_core_type =
    string_fn_from_core_type ~name:"to_string" ~string_fn_from_type:Namers.to_string
  ;;

  let xml_of_from_core_type core_type ~namer ~requested ~type_params ~generate =
    let generated =
      process_core_type
        core_type
        ~requested
        ~type_params
        ~namer
        ~make_leaf:
          (fun
            ty
            loc
            ~tag
            ~of_string:_
            ~to_string
            ~ignore_attributes:_
            ~preserve_space:_
            ~namespace
          ->
          let to_string =
            match to_string with
            | None -> to_string_from_core_type ty
            | Some to_string -> to_string
          in
          single_var_function loc ~f:(fun x ->
            make_element
              ~generate:`Element
              loc
              tag
              ~namespace
              [ [%expr Text ([%e to_string] [%e x])] ]))
        ~make_empty:(fun loc ~tag ~ignore_children:_ ~ignore_attributes:_ ~namespace ->
          [%expr fun () -> [%e make_element ~namespace ~generate:`Element loc tag []]])
    in
    match generate with
    | `Element -> generated
    | `Xml ->
      let loc = core_type.ptyp_loc in
      single_var_function loc ~f:(fun x ->
        [%expr Ppx_simple_xml_conv_lib.Xml.Element ([%e generated] [%e x])])
  ;;

  module Variant_serializer = struct
    let create loc constructors ~requested ~type_params ~parameters =
      validate_non_record ~loc ~type_type:"variant" parameters;
      List.map constructors ~f:(fun constructor_declaration ->
        Variant_common.handle_constructor_declaration
          constructor_declaration
          ~requested
          ~make_branch:(fun loc ~name:_ -> function
          | `Empty { tag; namespace; _ } ->
            Builder.case
              ~lhs:(Builder.pconstruct constructor_declaration None)
              ~guard:None
              ~rhs:(make_element loc tag ~namespace ~generate:`Element [])
          | `Type type_ ->
            let symbol = gen_symbol () in
            Builder.case
              ~lhs:
                (Builder.pconstruct
                   constructor_declaration
                   (Some (Builder.ppat_var ~loc { loc; txt = symbol })))
              ~guard:None
              ~rhs:
                (Builder.eapply
                   ~loc
                   (xml_of_from_core_type
                      type_
                      ~namer:Namers.xml_of
                      ~generate:`Element
                      ~requested
                      ~type_params)
                   [ Builder.evar ~loc symbol ])))
      |> Builder.pexp_function ~loc
    ;;
  end

  module Record_serializer = struct
    let rec convert_constant_expression_to_pattern
      ({ pexp_desc; pexp_loc = loc; pexp_loc_stack = _; pexp_attributes = _ } :
        expression)
      =
      match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
      | Pexp_tuple labeled_exprs ->
        Ppxlib_jane.Ast_builder.Default.ppat_tuple
          ~loc
          (List.map labeled_exprs ~f:(fun (l, e) ->
             l, convert_constant_expression_to_pattern e))
          Closed
      | Pexp_unboxed_tuple labeled_exprs ->
        Ppxlib_jane.Ast_builder.Default.ppat_unboxed_tuple
          ~loc
          (List.map labeled_exprs ~f:(fun (l, e) ->
             l, convert_constant_expression_to_pattern e))
          Closed
      | Pexp_lazy expr ->
        Builder.ppat_lazy ~loc (convert_constant_expression_to_pattern expr)
      | Pexp_construct (constructor, expr) ->
        Builder.ppat_construct
          ~loc
          constructor
          (Option.map expr ~f:convert_constant_expression_to_pattern)
      | Pexp_array (Mutable, exprs) ->
        Builder.ppat_array ~loc (List.map exprs ~f:convert_constant_expression_to_pattern)
      | Pexp_open ({ popen_expr = { pmod_desc = Pmod_ident path; _ }; _ }, expr) ->
        Builder.ppat_open ~loc path (convert_constant_expression_to_pattern expr)
      | Pexp_constant constant -> Builder.ppat_constant ~loc constant
      | Pexp_record (fields, None) ->
        Builder.ppat_record
          ~loc
          (List.map fields ~f:(fun (field, expr) ->
             field, convert_constant_expression_to_pattern expr))
          Closed
      | Pexp_variant (label, expression) ->
        Builder.ppat_variant
          ~loc
          label
          (Option.map expression ~f:convert_constant_expression_to_pattern)
      | Pexp_constraint (expression, Some ty, _) ->
        Builder.ppat_constraint
          ~loc
          (convert_constant_expression_to_pattern expression)
          ty
      | desc ->
        Location.raise_errorf
          ~loc
          "Cannot convert this expression to a pattern: %s"
          (Ppxlib_jane.Language_feature_name.of_expression_desc desc)
    ;;

    let extract
      label_declaration
      ~requested
      ~type_params
      ~(parameters : Parameters.Tag_serializer.t Parameters.For_records.t)
      ~attribute_acc_pat
      ~attribute_acc_expr
      ~element_acc_pat
      ~element_acc_expr
      =
      let loc = label_declaration.pld_name.loc in
      let name = label_declaration.pld_name.txt in
      let field_name = ({ loc; txt = lident name } : _ loc) in
      let field_name_var = Builder.evar ~loc name in
      let type_, expression =
        match Record_common.handle ~requested ~loc label_declaration ~parameters with
        | Element { count; type_ } ->
          let xml_of =
            xml_of_from_core_type
              type_
              ~requested
              ~type_params
              ~generate:`Xml
              ~namer:Namers.xml_of
          in
          let count_function =
            match count with
            | Required | Default _ ->
              [%expr Ppx_simple_xml_conv_lib.To_xml.Builder.prepend]
            | Option -> [%expr Ppx_simple_xml_conv_lib.To_xml.Builder.prepend_opt]
            | List -> [%expr Ppx_simple_xml_conv_lib.To_xml.Builder.prepend_list]
          in
          let chain rest =
            [%expr
              let [%p element_acc_pat] =
                [%e count_function]
                  ~convert:[%e xml_of]
                  [%e field_name_var]
                  [%e element_acc_expr]
              in
              [%e rest]]
          in
          `Element, chain
        | Boolean_element
            { tag; namespace; ignore_children = _; ignore_attributes = _; values } ->
          let prepender =
            [%expr
              Ppx_simple_xml_conv_lib.To_xml.Builder.prepend_opt
                ~convert:(fun () ->
                  Ppx_simple_xml_conv_lib.Xml.Element
                    { tag =
                        { ns = [%e Namespace.to_value ~loc namespace]
                        ; tag = [%e constant_or_variable_to_expression ~loc tag]
                        }
                    ; children = []
                    ; attributes = []
                    })
                (match [%e field_name_var] with
                 | [%p convert_constant_expression_to_pattern values.true_] -> Some ()
                 | [%p convert_constant_expression_to_pattern values.false_] -> None)
                [%e element_acc_expr]]
          in
          let chain rest =
            [%expr
              let [%p element_acc_pat] = [%e prepender] in
              [%e rest]]
          in
          `Element, chain
        | Inlined type_ ->
          let xml_of =
            xml_of_from_core_type
              type_
              ~requested
              ~type_params
              ~generate:`Element
              ~namer:Namers.inlined_xml_of
          in
          let inlined_attr_pat, inlined_attr_expr = single_var loc in
          let inlined_element_pat, inlined_element_expr = single_var loc in
          let chain rest =
            [%expr
              let [%p inlined_element_pat], [%p inlined_attr_pat] =
                [%e xml_of] [%e field_name_var]
              in
              let [%p element_acc_pat] =
                Ppx_simple_xml_conv_lib.To_xml.Builder.prepend_list
                  ~convert:(fun x -> x)
                  [%e inlined_element_expr]
                  [%e element_acc_expr]
              in
              let [%p attribute_acc_pat] =
                Ppx_simple_xml_conv_lib.To_xml.Builder.prepend_list
                  ~convert:(fun x -> x)
                  [%e inlined_attr_expr]
                  [%e attribute_acc_expr]
              in
              [%e rest]]
          in
          `Inlined, chain
        | Content type_ ->
          let to_string = to_string_from_core_type type_ in
          let chain rest =
            [%expr
              let [%p element_acc_pat] =
                [ Ppx_simple_xml_conv_lib.Xml.Text ([%e to_string] [%e field_name_var]) ]
              in
              [%e rest]]
          in
          `Content, chain
        | Attribute { count; key; namespace; of_string = _; to_string } ->
          let count_function =
            match count with
            | Required | Default _ ->
              [%expr Ppx_simple_xml_conv_lib.To_xml.Builder.prepend]
            | Option -> [%expr Ppx_simple_xml_conv_lib.To_xml.Builder.prepend_opt]
          in
          let to_string =
            match to_string with
            | Derive_from_type type_ -> to_string_from_core_type type_
            | Custom to_string -> to_string
          in
          let chain rest =
            [%expr
              let [%p attribute_acc_pat] =
                [%e count_function]
                  ~convert:
                    [%e
                      single_var_function loc ~f:(fun x ->
                        [%expr
                          { Ppx_simple_xml_conv_lib.Xml.Attribute.ns =
                              [%e Namespace.to_value ~loc namespace]
                          ; key = [%e constant_or_variable_to_expression ~loc key]
                          ; value = [%e to_string] [%e x]
                          }])]
                  [%e field_name_var]
                  [%e attribute_acc_expr]
              in
              [%e rest]]
          in
          `Attribute, chain
      in
      type_, { Record_common.Named.field_name; content = expression }
    ;;

    let create
      ~requested
      ~loc
      label_declarations
      ~type_params
      ~(parameters : Parameters.Tag_serializer.t Parameters.t)
      =
      let parameters = Parameters.for_records loc parameters in
      let attribute_acc_pat, attribute_acc_expr = single_var loc in
      let element_acc_pat, element_acc_expr = single_var loc in
      let let_statements =
        List.map
          ~f:
            (extract
               ~requested
               ~type_params
               ~parameters
               ~attribute_acc_pat
               ~attribute_acc_expr
               ~element_acc_pat
               ~element_acc_expr)
          label_declarations
        |> Record_common.reduce_parsers
      in
      let pattern =
        Builder.ppat_record
          ~loc
          (List.map label_declarations ~f:(fun declaration ->
             let { txt = name; loc } = declaration.pld_name in
             { loc; txt = lident name }, Builder.ppat_var ~loc declaration.pld_name))
          Closed
      in
      let let_statements =
        match let_statements with
        | Non_leaf list ->
          List.map
            list
            ~f:
              (fun
                (Attribute let_statement | Inlined let_statement | Element let_statement)
              -> let_statement)
        | Leaf { content; attributes } -> content :: attributes
      in
      let generated_element =
        match parameters with
        | Inlined -> [%expr [%e element_acc_expr], [%e attribute_acc_expr]]
        | Tag { tag; prefix_declarations; namespace } ->
          let attributes =
            match prefix_declarations with
            | None -> attribute_acc_expr
            | Some prefix_declarations ->
              [%expr
                Ppx_simple_xml_conv_lib.To_xml.Builder.prepend_list
                  [%e prefix_declarations]
                  [%e attribute_acc_expr]
                  ~convert:(fun (prefix, namespace) ->
                    { Ppx_simple_xml_conv_lib.Xml.Attribute.ns =
                        Ppx_simple_xml_conv_lib.To_xml.Builder.xmlns_namespace
                    ; key = prefix
                    ; value = namespace
                    })]
          in
          let namespace =
            match namespace with
            | None -> [%expr ""]
            | Some namespace -> namespace
          in
          [%expr
            { Ppx_simple_xml_conv_lib.Xml.tag = { ns = [%e namespace]; tag = [%e tag] }
            ; attributes = [%e attributes]
            ; children = [%e element_acc_expr]
            }]
      in
      let body =
        List.fold_right let_statements ~init:generated_element ~f:(fun chain body ->
          chain.content body)
      in
      [%expr
        fun [%p pattern] ->
          let [%p attribute_acc_pat] = [] in
          let [%p element_acc_pat] = [] in
          [%e body]]
    ;;
  end

  let type_signature_of_xml type_declaration ~inlined =
    let type_declaration = name_type_params_in_td type_declaration in
    let loc = type_declaration.ptype_loc in
    let type_of_element ~loc ty =
      [%type: [%t ty] -> Ppx_simple_xml_conv_lib.Xml.element]
    in
    if inlined
    then (
      let type_ =
        combinator_type_of_type_declaration_hybrid
          type_declaration
          ~init:
            [%type:
              [%t core_type_of_type_declaration type_declaration]
                Ppx_simple_xml_conv_lib.To_xml.inlined]
          ~f:type_of_element
      in
      let parse_name = Namers.inlined_xml_of.namer type_declaration.ptype_name.txt in
      Builder.value_description ~loc ~name:{ loc; txt = parse_name } ~type_ ~prim:[]
      |> Builder.psig_value ~loc)
    else (
      let serializer_type =
        combinator_type_of_type_declaration type_declaration ~f:type_of_element
      in
      let parse_name = Namers.xml_of.namer type_declaration.ptype_name.txt in
      Builder.value_description
        ~loc
        ~name:{ loc; txt = parse_name }
        ~type_:serializer_type
        ~prim:[]
      |> Builder.psig_value ~loc)
  ;;

  let sig_type_decl ~loc:_ (_, type_declarations) ~inlined : signature_item list =
    List.map ~f:(type_signature_of_xml ~inlined) type_declarations
  ;;

  let str_type_decl ~loc declarations ~requested ~parameters =
    let rec_flag, type_declaration =
      extract_type_declaration_and_recursion_flag declarations ~loc
    in
    handle_type_declaration
      type_declaration
      ~handle_variant:(fun declaration ~loc ~type_params ->
        Variant_serializer.create loc declaration ~parameters ~requested ~type_params)
      ~handle_record:(fun declaration ~loc ~type_params ->
        Record_serializer.create ~loc declaration ~parameters ~requested ~type_params)
      ~handle_core_type:(fun core_type ~loc:_ ~type_params ->
        validate_non_record ~loc ~type_type:"alias" parameters;
        xml_of_from_core_type
          core_type
          ~requested
          ~type_params
          ~generate:`Element
          ~namer:Namers.xml_of)
      ~f:(fun loc ~type_name ~type_param_patterns ~type_param_values:_ ~body ->
        let func = Builder.eabstract ~loc type_param_patterns body in
        let namer =
          match parameters with
          | Tag _ | No_parameters -> Namers.xml_of
          | Inlined -> Namers.inlined_xml_of
        in
        let pattern = Builder.ppat_var ~loc { txt = namer.namer type_name; loc } in
        Builder.pstr_value_list
          ~loc
          rec_flag
          [ Builder.value_binding ~loc ~pat:pattern ~expr:func ])
  ;;
end

module Deserializer = struct
  module Namers = struct
    let of_xml_description_function ty = ty ^ "_of_xml_description"
    let of_xml_inlined_function ty = ty ^ "_of_xml_inlined"
    let of_xml_function ty = ty ^ "_of_xml"

    let of_xml_description =
      { Namer.namer = of_xml_description_function
      ; type_argument_namer = of_xml_description_function
      }
    ;;

    let of_xml_inlined =
      { Namer.namer = of_xml_inlined_function
      ; type_argument_namer = of_xml_description_function
      }
    ;;

    let of_xml =
      { Namer.namer = of_xml_function; type_argument_namer = of_xml_description_function }
    ;;

    let of_string = function
      | "t" -> "of_string"
      | ty -> ty ^ "_of_string"
    ;;
  end

  let of_string_from_core_type =
    string_fn_from_core_type ~name:"of_string" ~string_fn_from_type:Namers.of_string
  ;;

  let generic_xml_of_from_core_type ~namer =
    process_core_type
      ~namer
      ~make_leaf:
        (fun
          ty
          loc
          ~tag
          ~of_string
          ~to_string:_
          ~ignore_attributes
          ~preserve_space
          ~namespace
        ->
        let of_string =
          match of_string with
          | Some expression -> expression
          | None -> of_string_from_core_type ty
        in
        [%expr
          Ppx_simple_xml_conv_lib.Of_xml.leaf
            ~ignore_attributes:[%e Builder.ebool ~loc ignore_attributes]
            ~preserve_space:[%e Builder.ebool ~loc preserve_space]
            ~namespace:[%e Namespace.to_parser ~loc namespace]
            [%e constant_or_variable_to_expression ~loc tag]
            ~of_string:[%e of_string]])
      ~make_empty:(fun loc ~tag ~ignore_children ~ignore_attributes ~namespace ->
        [%expr
          Ppx_simple_xml_conv_lib.Of_xml.empty_element
            ~ignore_children:[%e Builder.ebool ~loc ignore_children]
            ~ignore_attributes:[%e Builder.ebool ~loc ignore_attributes]
            ~namespace:[%e Namespace.to_parser ~loc namespace]
            [%e constant_or_variable_to_expression ~loc tag]])
  ;;

  let xml_description_from_core_type ~inlined =
    generic_xml_of_from_core_type
      ~namer:(if inlined then Namers.of_xml_inlined else Namers.of_xml_description)
  ;;

  module Variant_parser = struct
    let parse_variant loc constructors ~type_params ~requested =
      List.map
        constructors
        ~f:
          (Variant_common.handle_constructor_declaration
             ~requested
             ~make_branch:(fun loc ~name -> function
             | `Empty { tag; namespace; ignore_attributes; ignore_children } ->
               let constructor =
                 Builder.pexp_construct ~loc { loc; txt = lident name } None
               in
               [%expr
                 Parser_and_constructor
                   { parse =
                       Ppx_simple_xml_conv_lib.Of_xml.empty_element
                         [%e constant_or_variable_to_expression ~loc tag]
                         ~namespace:[%e Namespace.to_parser ~loc namespace]
                         ~ignore_attributes:[%e Builder.ebool ~loc ignore_attributes]
                         ~ignore_children:[%e Builder.ebool ~loc ignore_children]
                   ; construct = (fun () -> [%e constructor])
                   }]
             | `Type core_type ->
               let parse =
                 xml_description_from_core_type
                   core_type
                   ~requested
                   ~type_params
                   ~inlined:false
               in
               let construct_argument = gen_symbol () in
               let constructor =
                 Builder.pexp_construct
                   ~loc
                   { loc; txt = lident name }
                   (Some (Builder.evar ~loc construct_argument))
               in
               [%expr
                 Parser_and_constructor
                   { parse = [%e parse]
                   ; construct =
                       (fun [%p Builder.pvar ~loc construct_argument] -> [%e constructor])
                   }]))
      |> Builder.elist ~loc
    ;;

    let parse ~requested loc constructor_declarations ~type_params ~parameters =
      validate_non_record ~loc ~type_type:"variant" parameters;
      [%expr
        Ppx_simple_xml_conv_lib.Of_xml.flatten_variants
          [%e parse_variant loc constructor_declarations ~type_params ~requested]]
    ;;
  end

  module Record_parser = struct
    type t =
      { field_var_binding : pattern
      ; field_var_variable : expression
      ; parse_xml : expression
      }

    let extract
      label_declaration
      ~requested
      ~children_var
      ~attributes_var
      ~type_params
      ~parameters
      =
      let loc = label_declaration.pld_name.loc in
      let name = label_declaration.pld_name.txt in
      let field_var = gen_symbol ~prefix:("_" ^ name) () in
      let field_name = ({ loc; txt = lident name } : _ loc) in
      let field_var_variable = Builder.evar ~loc field_var in
      let field_var_binding = Builder.ppat_var ~loc { loc; txt = field_var } in
      let type_, parse_xml =
        match Record_common.handle ~requested ~loc label_declaration ~parameters with
        | Element { count; type_ } ->
          let xml_description =
            xml_description_from_core_type type_ ~requested ~type_params ~inlined:false
          in
          let count =
            match count with
            | Required -> [%expr Required]
            | Option -> [%expr Option]
            | List -> [%expr List]
            | Default default -> [%expr Default [%e default]]
          in
          let parse_xml =
            [%expr
              Ppx_simple_xml_conv_lib.Of_xml.element
                ?path_rev
                [%e count]
                [%e children_var]
                [%e xml_description]]
          in
          `Element, parse_xml
        | Boolean_element { tag; namespace; ignore_children; ignore_attributes; values }
          ->
          let parse_xml =
            [%expr
              let value, rest =
                Ppx_simple_xml_conv_lib.Of_xml.element
                  ?path_rev
                  Option
                  [%e children_var]
                  (Ppx_simple_xml_conv_lib.Of_xml.empty_element
                     [%e constant_or_variable_to_expression ~loc tag]
                     ~namespace:[%e Namespace.to_parser ~loc namespace]
                     ~ignore_attributes:[%e Builder.ebool ~loc ignore_attributes]
                     ~ignore_children:[%e Builder.ebool ~loc ignore_children])
              in
              let value =
                match value with
                | Some () -> [%e values.true_]
                | None -> [%e values.false_]
              in
              value, rest]
          in
          `Element, parse_xml
        | Inlined type_ ->
          let xml_description =
            xml_description_from_core_type type_ ~requested ~type_params ~inlined:true
          in
          ( `Inlined
          , [%expr [%e xml_description] ?path_rev [%e children_var] [%e attributes_var]] )
        | Content type_ ->
          (match parameters with
           | Inlined ->
             Location.raise_errorf
               ~loc
               "Can't use the %s attribute when deriving inlined"
               Attributes.content_attribute_name
           | Tag { Parameters.Tag_deserializer.tag; _ } ->
             let of_string = of_string_from_core_type type_ in
             let parse_xml =
               [%expr
                 [%e of_string]
                   (Ppx_simple_xml_conv_lib.Of_xml.extract_text
                      ?path_rev
                      ~tag:[%e tag]
                      [%e children_var])]
             in
             `Content, parse_xml)
        | Attribute { count; key; namespace; of_string; to_string = _ } ->
          let count =
            match count with
            | Required -> [%expr Required]
            | Option -> [%expr Option]
            | Default default -> [%expr Default [%e default]]
          in
          let of_string =
            match of_string with
            | Derive_from_type type_ -> of_string_from_core_type type_
            | Custom of_string -> of_string
          in
          let parse_xml =
            [%expr
              Ppx_simple_xml_conv_lib.Of_xml.attribute
                ?path_rev
                [%e count]
                [%e attributes_var]
                ~of_string:[%e of_string]
                ~namespace:[%e Namespace.to_parser ~loc namespace]
                ~key:[%e constant_or_variable_to_expression ~loc key]]
          in
          `Attribute, parse_xml
      in
      ( type_
      , { Record_common.Named.field_name
        ; content = { field_var_variable; field_var_binding; parse_xml }
        } )
    ;;

    let generate_record ts ~loc =
      Builder.pexp_record
        ~loc
        (List.map ts ~f:(fun (_, t) ->
           t.Record_common.Named.field_name, t.content.field_var_variable))
        None
    ;;

    let generate_bindings
      ts
      ~loc
      ~children_var
      ~attributes_var
      ~children_pattern
      ~attributes_pattern
      ~record
      ~parameters
      =
      let struct_parser = Record_common.reduce_parsers ts in
      let init =
        match (parameters : Parameters.Tag_deserializer.t Parameters.For_records.t) with
        | Inlined -> [%expr [%e record], [%e children_var], [%e attributes_var]]
        | Tag { tag = _; namespace = _; allow_extra_elements; allow_extra_attributes } ->
          let init =
            match struct_parser, allow_extra_elements with
            | Non_leaf _, false ->
              [%expr
                Ppx_simple_xml_conv_lib.Of_xml.check_no_extra_children
                  ?path_rev
                  [%e children_var];
                [%e record]]
            | Leaf _, false | Non_leaf _, true -> record
            | Leaf { content; _ }, true ->
              Location.raise_errorf
                ~loc
                "Cannot use [allow_extra_elements] when deriving on a type that uses a \
                 text content parser (field: %s)."
                (Longident.name content.field_name.txt)
                ()
          in
          if allow_extra_attributes
          then init
          else
            [%expr
              Ppx_simple_xml_conv_lib.Of_xml.check_no_extra_attributes
                ?path_rev
                [%e attributes_var];
              [%e init]]
      in
      let parse_attribute { Record_common.Named.content; _ } init =
        [%expr
          let [%p content.field_var_binding], [%p attributes_pattern] =
            [%e content.parse_xml]
          in
          [%e init]]
      in
      match struct_parser with
      | Non_leaf elements ->
        let body =
          List.fold_right elements ~init ~f:(fun element init ->
            match element with
            | Element { content; _ } ->
              [%expr
                let [%p content.field_var_binding], [%p children_pattern] =
                  [%e content.parse_xml]
                in
                [%e init]]
            | Inlined { content; _ } ->
              [%expr
                let ( [%p content.field_var_binding]
                    , [%p children_pattern]
                    , [%p attributes_pattern] )
                  =
                  [%e content.parse_xml]
                in
                [%e init]]
            | Attribute attribute -> parse_attribute attribute init)
        in
        (match parameters with
         | Inlined -> body
         | Tag _ ->
           [%expr
             let [%p children_pattern] =
               Ppx_simple_xml_conv_lib.Of_xml.elements_only [%e children_var]
             in
             [%e body]])
      | Leaf { content = { content; _ }; attributes } ->
        let init =
          [%expr
            let [%p content.field_var_binding] = [%e content.parse_xml] in
            [%e init]]
        in
        List.fold_right ~init attributes ~f:parse_attribute
    ;;

    let parse ~requested ~loc label_declarations ~type_params ~parameters =
      let parameters = Parameters.for_records loc parameters in
      let children = gen_symbol ~prefix:"_children" () in
      let attributes = gen_symbol ~prefix:"_attrs" () in
      let children_var = Builder.evar children ~loc in
      let attributes_var = Builder.evar attributes ~loc in
      let children_pattern = Builder.pvar children ~loc in
      let attributes_pattern = Builder.pvar attributes ~loc in
      let ts =
        List.map
          label_declarations
          ~f:(extract ~children_var ~attributes_var ~type_params ~parameters ~requested)
      in
      let body =
        generate_bindings
          ts
          ~loc
          ~children_var
          ~attributes_var
          ~children_pattern
          ~attributes_pattern
          ~record:(generate_record ts ~loc)
          ~parameters
      in
      match parameters with
      | Inlined ->
        [%expr
          fun ?(path_rev @ local) [%p children_pattern] [%p attributes_pattern] ->
            [%e body]]
      | Tag { tag; namespace; allow_extra_elements = _; allow_extra_attributes = _ } ->
        [%expr
          Ppx_simple_xml_conv_lib.Of_xml.Element
            { tag = [%e tag]
            ; namespace = [%e Namespace.to_parser ~loc namespace]
            ; parse =
                (fun ?(path_rev @ local)
                  { children = [%p children_pattern]
                  ; attributes = [%p attributes_pattern]
                  ; tag = _
                  } ->
                  [%e body])
            }]
    ;;
  end

  let type_declaration_of_xml
    (type_declaration : Parsetree.type_declaration)
    ~parameters
    ~requested
    ~rec_flag
    =
    handle_type_declaration
      type_declaration
      ~handle_variant:(fun constructor_declarations ~loc ~type_params ->
        Variant_parser.parse
          loc
          constructor_declarations
          ~parameters
          ~type_params
          ~requested)
      ~handle_record:(fun label_declarations ~loc ~type_params ->
        Record_parser.parse ~loc label_declarations ~parameters ~type_params ~requested)
      ~handle_core_type:(fun core_type ~loc ~type_params ->
        validate_non_record ~loc ~type_type:"alias" parameters;
        xml_description_from_core_type core_type ~type_params ~requested ~inlined:false)
      ~f:(fun loc ~type_name ~type_param_patterns ~type_param_values ~body ->
        (* This takes in the type param parsers. *)
        let body = Builder.eabstract ~loc type_param_patterns body in
        match parameters with
        | Inlined ->
          let inlined_parser_name = Namers.of_xml_inlined.namer type_name in
          [ Builder.pstr_value
              ~loc
              rec_flag
              [ Builder.value_binding
                  ~loc
                  ~pat:(Builder.pvar ~loc inlined_parser_name)
                  ~expr:body
              ]
          ]
        | Tag _ | No_parameters ->
          let description_name = Namers.of_xml_description.namer type_name in
          let parse_name = Namers.of_xml_function type_name in
          let body_parser =
            let description =
              Builder.eapply (Builder.evar ~loc description_name) type_param_values ~loc
            in
            let parse_without_type_params =
              [%expr
                fun xml ->
                  Ppx_simple_xml_conv_lib.Of_xml.parse ~path_rev:[] [%e description] xml]
            in
            Builder.eabstract ~loc type_param_patterns parse_without_type_params
          in
          [ Builder.pstr_value
              ~loc
              rec_flag
              [ Builder.value_binding
                  ~loc
                  ~pat:(Builder.pvar ~loc description_name)
                  ~expr:body
              ]
          ; Builder.pstr_value
              ~loc
              Nonrecursive
              [ Builder.value_binding
                  ~loc
                  ~pat:(Builder.pvar ~loc parse_name)
                  ~expr:body_parser
              ]
          ])
  ;;

  let description_type_declaration ~loc ty =
    [%type: [%t ty] Ppx_simple_xml_conv_lib.Of_xml.t]
  ;;

  let type_signature_of_xml type_declaration ~inlined =
    let type_declaration = name_type_params_in_td type_declaration in
    let { ptype_loc = loc; ptype_name = { txt = type_name; _ }; _ } = type_declaration in
    let binding ~loc ~name ~type_ =
      Builder.value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[]
      |> Builder.psig_value ~loc
    in
    if inlined
    then (
      let inlined_type =
        combinator_type_of_type_declaration_hybrid
          type_declaration
          ~init:
            [%type:
              [%t core_type_of_type_declaration type_declaration]
                Ppx_simple_xml_conv_lib.Of_xml.inlined]
          ~f:description_type_declaration
      in
      let inlined_name = Namers.of_xml_inlined.namer type_name in
      [ binding ~loc ~name:inlined_name ~type_:inlined_type ])
    else (
      let description_type =
        combinator_type_of_type_declaration
          type_declaration
          ~f:description_type_declaration
      in
      let parse_type =
        combinator_type_of_type_declaration_hybrid
          type_declaration
          ~init:
            [%type:
              Ppx_simple_xml_conv_lib.Xml.element
              -> [%t core_type_of_type_declaration type_declaration]]
          ~f:description_type_declaration
      in
      let description_name = Namers.of_xml_description.namer type_name in
      let parse_name = Namers.of_xml_function type_name in
      [ binding ~loc ~name:description_name ~type_:description_type
      ; binding ~loc ~name:parse_name ~type_:parse_type
      ])
  ;;

  let str_type_decl ~loc (rec_flag, type_declarations) ~requested ~parameters : structure =
    (* Check if the type is really recursive, else the compiler raises an error about a
       redundant rec flag *)
    let rec_flag = (new type_is_recursive rec_flag type_declarations)#go () in
    let type_declaration =
      match type_declarations with
      | [ type_declaration ] -> type_declaration
      | _ ->
        Location.raise_errorf ~loc "ppx_simple_xml_conv: multiple types not yet supported"
    in
    type_declaration_of_xml type_declaration ~requested ~rec_flag ~parameters
  ;;

  let sig_type_decl ~loc:_ (_, type_declarations) ~inlined : signature_item list =
    List.concat_map ~f:(type_signature_of_xml ~inlined) type_declarations
  ;;
end

module Extensions = struct
  let of_xml_description core_type =
    Deserializer.generic_xml_of_from_core_type
      ~namer:Deserializer.Namers.of_xml_description
      core_type
      ~requested:{ to_xml = true; of_xml = false }
      ~type_params:(Map.empty (module String))
  ;;

  let of_xml core_type =
    Deserializer.generic_xml_of_from_core_type
      ~namer:Deserializer.Namers.of_xml
      core_type
      ~requested:{ to_xml = true; of_xml = false }
      ~type_params:(Map.empty (module String))
  ;;

  let xml_of core_type =
    Serializer.xml_of_from_core_type
      ~namer:Serializer.Namers.xml_of
      ~generate:`Element
      core_type
      ~requested:{ to_xml = false; of_xml = true }
      ~type_params:(Map.empty (module String))
  ;;
end

let top_level_open loc = [%str open! Ppx_simple_xml_conv_lib.Primitives]
