open! Base
open! Ppxlib
open! Ast_builder.Default

let ppx_name = "ppx_var_name"

let underscore_to_dashes_lowercase s =
  String.tr s ~target:'_' ~replacement:'-' |> String.lowercase
;;

let error ~loc =
  Printf.ksprintf (fun message ->
    pexp_extension
      ~loc
      ({ loc; txt = "ocaml.error" }, PStr [ pstr_eval ~loc (estring ~loc message) [] ]))
;;

let extension extension_name get_name postprocess_string =
  Extension.V3.declare
    extension_name
    Expression
    Ast_pattern.(pstr nil)
    (fun ~ctxt ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let code_path = Expansion_context.Extension.code_path ctxt in
      match get_name code_path with
      | Some variable_name -> estring ~loc (postprocess_string variable_name)
      | None ->
        error
          ~loc
          "ppx_var_name: Unable to determine what [%%%s] should resolve to."
          extension_name)
;;

let () =
  let extensions =
    let extensions prefix name_from_code_path =
      let ext ?extension_name f =
        let extension_name =
          match extension_name with
          | Some name -> name
          | None -> f (prefix ^ "_name")
        in
        let extension_name =
          (* We can't add an optional prefix if the first letter of [extension_name] is
             uppercase, because capitalized identifiers after dots are instead used for
             the syntax for module paths in extensions like [let%bind.Deferred]. *)
          let should_add_optional_prefix =
            not (String.equal extension_name (String.capitalize extension_name))
          in
          if should_add_optional_prefix
          then "var_name." ^ extension_name
          else extension_name
        in
        extension extension_name name_from_code_path f
      in
      [ ext String.lowercase
      ; ext String.capitalize
      ; ext String.uppercase
      ; ext underscore_to_dashes_lowercase ~extension_name:(prefix ^ "_dash_name")
      ]
    in
    List.concat
      [ extensions "var" Code_path.enclosing_value
      ; extensions "module" (fun cp -> Some (Code_path.enclosing_module cp))
      ]
  in
  let rules = List.map ~f:Context_free.Rule.extension extensions in
  Driver.register_transformation ppx_name ~rules
;;
