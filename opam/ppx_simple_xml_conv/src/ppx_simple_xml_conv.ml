open Ppxlib

module Deserializer = struct
  let name = "of_xml"

  (** These are the arguments that are used when deriving [of_xml] on record types
      ([@@deriving of_xml ~tag:"tag-name" ~allow_extra_elements ~allow_extra_attributes]).

      Not always valid (e.g. on variant types), will raise where appropriate. *)
  let str_args =
    Deriving.Args.(
      empty
      +> arg "tag" __
      +> arg "namespace" __
      +> flag "assert_no_namespace"
      +> flag "inlined"
      +> flag "allow_extra_elements"
      +> flag "allow_extra_attributes")
  ;;

  (** Declares the deriver for the implementation code. *)
  let str_type_decl =
    Deriving.Generator.make
      str_args
      (fun
          ~loc
          ~path:_
          type_
          tag
          namespace
          assert_no_namespace
          inlined
          allow_extra_elements
          allow_extra_attributes
        ->
         Ppx_simple_xml_conv_expander.Deserializer.str_type_decl
           ~loc
           ~parameters:
             (Ppx_simple_xml_conv_expander.Parameters.create_deserializer
                loc
                ~tag
                ~namespace:
                  (Ppx_simple_xml_conv_expander.Namespace.of_parameters
                     ~loc
                     ~namespace
                     ~assert_no_namespace)
                ~inlined
                ~allow_extra_elements
                ~allow_extra_attributes)
           ~requested:{ of_xml = true; to_xml = false }
           type_)
  ;;

  let sig_args = Deriving.Args.(empty +> flag "inlined")

  (** Declares the deriver for the signature code. *)
  let sig_type_decl =
    Deriving.Generator.make sig_args (fun ~loc ~path:_ type_ inlined ->
      Ppx_simple_xml_conv_expander.Deserializer.sig_type_decl ~loc type_ ~inlined)
  ;;

  let deriving =
    Deriving.add
      ~str_type_decl
      ~sig_type_decl
      ~extension:(fun ~loc:_ ~path:_ -> Ppx_simple_xml_conv_expander.Extensions.of_xml)
      name
  ;;

  let deriving_description =
    Deriving.add
      ~extension:(fun ~loc:_ ~path:_ ->
        Ppx_simple_xml_conv_expander.Extensions.of_xml_description)
      "of_xml_description"
  ;;
end

module Serializer = struct
  let name = "xml_of"

  (** These are the arguments that are used when deriving [xml_of] on record types
      ([@@deriving of_xml ~tag:"tag-name"]).

      Not always valid (e.g. on variant types), will raise where appropriate. *)
  let args =
    Deriving.Args.(
      empty +> arg "tag" __ +> flag "inlined" +> arg "prefixes" __ +> arg "namespace" __)
  ;;

  (** Declares the deriver for the implementation code. *)
  let str_type_decl =
    Deriving.Generator.make
      args
      (fun ~loc ~path:_ type_ tag inlined prefix_declarations namespace ->
         Ppx_simple_xml_conv_expander.Serializer.str_type_decl
           ~loc
           ~requested:{ of_xml = false; to_xml = true }
           ~parameters:
             (Ppx_simple_xml_conv_expander.Parameters.create_serializer
                loc
                ~tag
                ~inlined
                ~prefix_declarations
                ~namespace)
           type_)
  ;;

  let sig_args = Deriving.Args.(empty +> flag "inlined")

  (** Declares the deriver for the signature code. *)
  let sig_type_decl =
    Deriving.Generator.make sig_args (fun ~loc ~path:_ type_ inlined ->
      Ppx_simple_xml_conv_expander.Serializer.sig_type_decl ~loc type_ ~inlined)
  ;;

  let deriving =
    Deriving.add
      ~str_type_decl
      ~sig_type_decl
      ~extension:(fun ~loc:_ ~path:_ -> Ppx_simple_xml_conv_expander.Extensions.xml_of)
      name
  ;;
end

module Serializer_deserializer = struct
  let name = "xml"

  (** These are the arguments that are used when deriving [of_xml] on record types
      ([@@deriving of_xml ~tag:"tag-name" ~allow_extra_elements ~allow_extra_attributes]).

      Not always valid (e.g. on variant types), will raise where appropriate. *)
  let args =
    Deriving.Args.(
      empty
      +> arg "tag" __
      +> arg "namespace" __
      +> flag "assert_no_namespace"
      +> flag "inlined"
      +> flag "allow_extra_elements"
      +> flag "allow_extra_attributes"
      +> arg "prefixes" __)
  ;;

  (** Declares the deriver for the implementation code. *)
  let str_type_decl =
    let requested =
      { Ppx_simple_xml_conv_expander.Requested.of_xml = true; to_xml = true }
    in
    Deriving.Generator.make
      args
      (fun
          ~loc
          ~path:_
          type_
          tag
          namespace
          assert_no_namespace
          inlined
          allow_extra_elements
          allow_extra_attributes
          prefix_declarations
        ->
         Ppx_simple_xml_conv_expander.Serializer.str_type_decl
           ~loc
           ~requested
           type_
           ~parameters:
             (Ppx_simple_xml_conv_expander.Parameters.create_serializer
                loc
                ~tag
                ~inlined
                ~prefix_declarations
                ~namespace)
         @ Ppx_simple_xml_conv_expander.Deserializer.str_type_decl
             ~loc
             ~requested
             ~parameters:
               (Ppx_simple_xml_conv_expander.Parameters.create_deserializer
                  loc
                  ~tag
                  ~namespace:
                    (Ppx_simple_xml_conv_expander.Namespace.of_parameters
                       ~loc
                       ~namespace
                       ~assert_no_namespace)
                  ~inlined
                  ~allow_extra_elements
                  ~allow_extra_attributes)
             type_)
  ;;

  let sig_args = Deriving.Args.(empty +> flag "inlined")

  (** Declares the deriver for the signature code. *)
  let sig_type_decl =
    Deriving.Generator.make sig_args (fun ~loc ~path:_ type_ inlined ->
      Ppx_simple_xml_conv_expander.Serializer.sig_type_decl ~loc type_ ~inlined
      @ Ppx_simple_xml_conv_expander.Deserializer.sig_type_decl ~loc type_ ~inlined)
  ;;

  let deriving = Deriving.add ~str_type_decl ~sig_type_decl name
end

(** We need certain primitives when deriving of_xml, so this opens
    Ppx_simple_xml_conv_lib.Primitives at the top of the file. *)
let () =
  Driver.register_transformation
    "Ppx_simple_xml_conv.enclose_impl"
    ~enclose_impl:(function
    | None -> [], []
    | Some loc ->
      let loc = { loc with loc_end = loc.loc_start } in
      Ppx_simple_xml_conv_expander.top_level_open loc, [])
;;
