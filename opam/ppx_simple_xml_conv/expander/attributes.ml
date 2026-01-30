open! Base
open! Ppxlib

module Namespace = struct
  type t =
    | Do_not_care
    | Assert_no_namespace
    | Namespace of expression

  let to_parser ~loc : t -> expression = function
    | Do_not_care -> [%expr Do_not_care]
    | Assert_no_namespace -> [%expr Assert_no_namespace]
    | Namespace ns -> [%expr Assert_equals [%e ns]]
  ;;

  let to_value ~loc : t -> expression = function
    | Do_not_care | Assert_no_namespace -> [%expr ""]
    | Namespace ns -> [%expr [%e ns]]
  ;;

  let of_parameters ~loc ~assert_no_namespace ~namespace : t =
    match assert_no_namespace, namespace with
    | false, None -> Do_not_care
    | false, Some ns -> Namespace ns
    | true, None -> Assert_no_namespace
    | true, Some _ ->
      Location.raise_errorf
        ~loc
        "ppx_simple_xml_conv: [assert_no_namespace] and [namespace] cannot be used \
         together"
  ;;
end

module Constant_or_variable = struct
  type t =
    | Constant of string
    | Variable of Longident.t

  let ast_pattern () =
    let open Ast_pattern in
    let constant = estring __ |> map ~f:(fun _ s -> Constant s) in
    let variable = pexp_ident __ |> map ~f:(fun _ ident -> Variable ident) in
    alt constant variable
  ;;
end

module Leaf = struct
  type t =
    { tag : Constant_or_variable.t
    ; namespace : Namespace.t
    ; of_string : expression option
    ; to_string : expression option
    ; ignore_attributes : bool
    ; preserve_space : bool
    }
end

module Empty = struct
  type t =
    { tag : Constant_or_variable.t
    ; namespace : Namespace.t
    ; ignore_children : bool
    ; ignore_attributes : bool
    }
end

module Boolean = struct
  module Values = struct
    type t =
      { true_ : expression
      ; false_ : expression
      }

    let default ~loc =
      { true_ = Ast_builder.Default.ebool true ~loc
      ; false_ = Ast_builder.Default.ebool false ~loc
      }
    ;;
  end

  type t =
    { tag : Constant_or_variable.t
    ; namespace : Namespace.t
    ; ignore_children : bool
    ; ignore_attributes : bool
    ; values : Values.t
    }
end

module Attribute_args = struct
  type t =
    { loc : Location.t
    ; key : Constant_or_variable.t
    ; namespace : Namespace.t
    ; of_string : expression option
    ; to_string : expression option
    ; optional : bool
    ; default : expression option
    }
end

let default =
  Attribute.declare
    "xml.default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil)) (* Evaluates a generic expression. *)
    (fun x -> x)
;;

let option =
  Attribute.declare
    "xml.option"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil) (* The attribute has no arguments. *)
    ()
;;

let list =
  Attribute.declare
    "xml.list"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil) (* The attribute has no arguments. *)
    ()
;;

let inlined =
  Attribute.declare
    "xml.inlined"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil) (* The attribute has no arguments. *)
    ()
;;

let leaf =
  Labelled_args.(
    declare
      ~allow_no_args:true
      "xml.leaf"
      Core_type
      (Constant_or_variable.ast_pattern ())
      (just_label "assert_no_namespace"
       ^?-> ("namespace", __, Fn.id)
       ^?-> ("of_string", __, Fn.id)
       ^?-> ("to_string", __, Fn.id)
       ^?-> just_label "ignore_attributes"
       ^?-> just_label "preserve_space"
       ^?-> ret)
      (fun loc
        tag
        assert_no_namespace
        namespace
        of_string
        to_string
        ignore_attributes
        preserve_space ->
        { Leaf.of_string
        ; to_string
        ; tag
        ; namespace =
            Namespace.of_parameters
              ~loc
              ~assert_no_namespace:(Option.is_some assert_no_namespace)
              ~namespace
        ; ignore_attributes = Option.is_some ignore_attributes
        ; preserve_space = Option.is_some preserve_space
        }))
;;

let attribute =
  Labelled_args.(
    declare
      ~allow_no_args:true
      "xml.attribute"
      Attribute.Context.label_declaration
      (Constant_or_variable.ast_pattern ())
      (just_label "assert_no_namespace"
       ^?-> ("namespace", __, Fn.id)
       ^?-> ("of_string", __, Fn.id)
       ^?-> ("to_string", __, Fn.id)
       ^?-> just_label "optional"
       ^?-> ("default", __, Fn.id)
       ^?-> ret)
      (fun loc key assert_no_namespace namespace of_string to_string optional default ->
        { Attribute_args.loc
        ; key
        ; of_string
        ; to_string
        ; namespace =
            Namespace.of_parameters
              ~loc
              ~assert_no_namespace:(Option.is_some assert_no_namespace)
              ~namespace
        ; optional = Option.is_some optional
        ; default
        }))
;;

let empty_generic context =
  Labelled_args.(
    declare
      "xml.empty"
      ~allow_no_args:true
      context
      (Constant_or_variable.ast_pattern ())
      (just_label "assert_no_namespace"
       ^?-> ("namespace", __, Fn.id)
       ^?-> just_label "ignore_attributes"
       ^?-> just_label "ignore_children"
       ^?-> ret)
      (fun loc tag assert_no_namespace namespace ignore_attributes ignore_children ->
        { Empty.tag
        ; namespace =
            Namespace.of_parameters
              ~loc
              ~assert_no_namespace:(Option.is_some assert_no_namespace)
              ~namespace
        ; ignore_attributes = Option.is_some ignore_attributes
        ; ignore_children = Option.is_some ignore_children
        }))
;;

let empty_variant = empty_generic Attribute.Context.constructor_declaration
let empty_core_type = empty_generic Attribute.Context.core_type

let boolean =
  Labelled_args.(
    declare
      "xml.bool"
      ~allow_no_args:true
      Attribute.Context.label_declaration
      (Constant_or_variable.ast_pattern ())
      (just_label "assert_no_namespace"
       ^?-> ("namespace", __, Fn.id)
       ^?-> just_label "ignore_attributes"
       ^?-> just_label "ignore_children"
       ^?-> ("true_", __, Fn.id)
       ^?-> ("false_", __, Fn.id)
       ^?-> ret)
      (fun loc
        tag
        assert_no_namespace
        namespace
        ignore_attributes
        ignore_children
        true_
        false_ ->
        let values =
          match true_, false_ with
          | None, None -> Boolean.Values.default ~loc
          | Some true_, Some false_ -> { Boolean.Values.true_; false_ }
          | None, Some _ ->
            Location.raise_errorf ~loc "Cannot specifiy ~false_ without ~true_"
          | Some _, None ->
            Location.raise_errorf ~loc "Cannot specifiy ~true_ without ~false_"
        in
        { Boolean.tag
        ; namespace =
            Namespace.of_parameters
              ~loc
              ~assert_no_namespace:(Option.is_some assert_no_namespace)
              ~namespace
        ; ignore_attributes = Option.is_some ignore_attributes
        ; ignore_children = Option.is_some ignore_children
        ; values
        }))
;;

let content_attribute_name = "xml.text"

let content =
  Attribute.(
    declare
      content_attribute_name
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil) (* The attribute has no arguments. *)
      ())
;;

let invalid_attribute ~loc attribute_string description =
  Location.raise_errorf
    ~loc
    "ppx_simple_xml_conv: [@%s] is only allowed on type [%s]."
    attribute_string
    description
;;

module Helper = struct
  type 'a packed = T : ('context, _) Attribute.t -> 'context packed

  let get_attribute attr ld ~f =
    Option.map (Attribute.get attr ld) ~f:(fun x -> f x, Attribute.name attr)
  ;;

  let get_exclusive declaration attributes ~loc =
    match List.filter_map ~f:(fun attribute -> attribute declaration) attributes with
    | [] -> None
    | [ (v, _) ] -> Some v
    | _ :: _ :: _ as attributes ->
      Location.raise_errorf
        ~loc
        "The following attributes are mutually exclusive: %s"
        (String.concat ~sep:" " (List.map attributes ~f:snd))
  ;;

  let validate_not_set declaration attributes ~fail =
    match
      List.filter_map attributes ~f:(fun (T attribute) ->
        match Attribute.get attribute declaration with
        | None -> None
        | Some _ -> Some (Attribute.name attribute))
    with
    | [] -> ()
    | _ :: _ as attributes -> fail (String.concat ~sep:" " attributes)
  ;;
end

module Core_type = struct
  type t =
    | From_type
    | Leaf of Leaf.t
    | Empty of Empty.t

  open! Helper

  let get_empty core_type ~loc =
    get_attribute empty_core_type core_type ~f:(fun empty ->
      match core_type with
      | [%type: unit] -> Empty empty
      | _ ->
        Location.raise_errorf
          ~loc
          "For core types, you can only use [@xml.empty] with `unit`.")
  ;;

  let handle core_type ~loc =
    get_exclusive
      core_type
      ~loc
      [ get_empty ~loc; get_attribute leaf ~f:(fun leaf -> Leaf leaf) ]
    |> Option.value ~default:From_type
  ;;
end

module Record = struct
  open! Helper

  module Element_count = struct
    type t =
      | Required
      | Option
      | List
      | Default of expression
  end

  module Attribute_count = struct
    type t =
      | Required
      | Option
      | Default of expression
  end

  module Attribute_string_fn = struct
    type t =
      | Custom of expression
      | Derive_from_type of core_type

    let of_option core_type = function
      | None -> Derive_from_type core_type
      | Some e -> Custom e
    ;;
  end

  type t =
    | Element of
        { count : Element_count.t
        ; type_ : core_type
        }
    | Boolean_element of Boolean.t
    | Inlined of core_type
    | Content of core_type
    | Attribute of
        { count : Attribute_count.t
        ; key : Constant_or_variable.t
        ; namespace : Namespace.t
        ; of_string : Attribute_string_fn.t
        ; to_string : Attribute_string_fn.t
        }

  let get_option core_type ~attribute_string ~loc =
    match core_type with
    | [%type: [%t? ty] option] -> ty
    | _ -> invalid_attribute ~loc attribute_string "_ option"
  ;;

  let get_list core_type ~attribute_string ~loc =
    match core_type with
    | [%type: [%t? ty] list] -> ty
    | _ -> invalid_attribute ~loc attribute_string "_ list"
  ;;

  let get_attribute_validate attribute label_declaration ~loc ~get ~f =
    match Attribute.get attribute label_declaration with
    | Some () ->
      let name = Attribute.name attribute in
      let result = get label_declaration.pld_type ~attribute_string:name ~loc |> f in
      Some (result, name)
    | None -> None
  ;;

  let validate_attribute label_declaration ~loc =
    validate_not_set
      label_declaration
      [ T option; T list; T default; T content ]
      ~fail:
        (Location.raise_errorf
           ~loc
           "The following attributes are only valid for XML tags: %s.")
  ;;

  let handle_attribute
    label_declaration
    ~loc
    { Attribute_args.loc = attribute_loc
    ; of_string
    ; to_string
    ; key
    ; optional
    ; default
    ; namespace
    }
    =
    validate_attribute label_declaration ~loc;
    let type_ = label_declaration.pld_type in
    let type_, (count : Attribute_count.t) =
      match optional, default with
      | false, None -> type_, Required
      | true, None ->
        get_option type_ ~attribute_string:"xml.attribute _ ~optional" ~loc, Option
      | false, Some default -> type_, Default default
      | true, Some _ ->
        Location.raise_errorf
          ~loc:attribute_loc
          "Cannot set both optional and default for attributes"
    in
    Attribute
      { key
      ; namespace
      ; count
      ; of_string = Attribute_string_fn.of_option type_ of_string
      ; to_string = Attribute_string_fn.of_option type_ to_string
      }
  ;;

  let handle_element label_declaration ~loc =
    match
      get_exclusive
        label_declaration
        ~loc
        [ get_attribute_validate option ~get:get_option ~loc ~f:(fun x -> `option x)
        ; get_attribute_validate list ~get:get_list ~loc ~f:(fun x -> `list x)
        ; get_attribute default ~f:(fun x -> `default x)
        ; get_attribute content ~f:(fun () -> `content)
        ; get_attribute boolean ~f:(fun boolean -> `boolean boolean)
        ; get_attribute inlined ~f:(fun () -> `inlined)
        ]
    with
    | None -> Element { count = Required; type_ = label_declaration.pld_type }
    | Some (`default default) ->
      Element { count = Default default; type_ = label_declaration.pld_type }
    | Some (`option type_) -> Element { count = Option; type_ }
    | Some (`list type_) -> Element { count = List; type_ }
    | Some `content -> Content label_declaration.pld_type
    | Some (`boolean boolean) -> Boolean_element boolean
    | Some `inlined -> Inlined label_declaration.pld_type
  ;;

  let handle label_declaration ~loc =
    match Attribute.get attribute label_declaration with
    | None -> handle_element label_declaration ~loc
    | Some args -> handle_attribute label_declaration ~loc args
  ;;
end

module Variant = struct
  open! Helper

  type t =
    | Empty of Empty.t
    | From_type

  let handle constructor ~loc =
    match
      get_exclusive
        constructor
        [ get_attribute empty_variant ~f:(fun empty -> Empty empty) ]
        ~loc
    with
    | None -> From_type
    | Some custom -> custom
  ;;
end
