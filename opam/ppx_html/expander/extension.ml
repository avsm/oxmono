open! Core
open Ppxlib
open Ppx_html_syntax

let experimental_component_shorthand_syntax_allowed = ref false

let set_experimental_component_shorthand_syntax_allowed () =
  experimental_component_shorthand_syntax_allowed := true
;;

let register_experimental_component_syntax_flag () =
  (* NOTE: This is dead code should we want to do a similar feature-flag release lock in
     the future. *)
  Driver.add_arg
    "-experimental-component-shorthand-syntax-allowed"
    (Unit set_experimental_component_shorthand_syntax_allowed)
    ~doc:
      {|Turns on the utilization of the experimental ppx_html syntax for shorter components. (i.e. <Foo></> will call Foo.component')|}
;;

let () = register_experimental_component_syntax_flag ()

let loc_ghoster =
  object
    inherit Ast_traverse.map as super
    method! location location = super#location { location with loc_ghost = true }
  end
;;

let is_capitalized s = (not (String.is_empty s)) && Char.is_uppercase s.[0]

let experimental_feature_checker ~loc:_ =
  object
    inherit Model.Traverse.map as super

    method! literal literal =
      match literal with
      | Literal _ as literal -> super#literal literal
      | Component { name = { loc; txt = name }; string_relative_location = _; code = _ }
        ->
        (match name with
         | (Lident x | Ldot (_, x)) when is_capitalized x ->
           Location.raise_errorf
             ~loc
             "Error this syntax is currently experimental not enabled by default in \
              ppx_html. If you would like to use a syntax like this please reach out! \
              Thanks."
         | _ -> super#literal literal)
  end
;;

let extension ~name ~runtime_kind ~experimental_features_allowed =
  Extension.declare_with_path_arg
    name
    Extension.Context.expression
    Ast_pattern.(
      pstr (pstr_eval (pexp_constant (pconst_string __' __ (some __))) nil ^:: nil))
    (fun ~loc:outer_loc
      ~path:_
      ~arg:html_syntax_module
      { loc = _; txt = string }
      string_loc
      _delimiter ->
      let loc = string_loc in
      let model = Model_parser.of_string ~loc string in
      let experimental_features_allowed =
        experimental_features_allowed || !experimental_component_shorthand_syntax_allowed
      in
      if not experimental_features_allowed
      then
        List.iter model ~f:(fun node ->
          (ignore : Model.Node.t -> unit) ((experimental_feature_checker ~loc)#node node));
      Model_code_gen.code ~loc:outer_loc ~html_syntax_module ~runtime_kind model
      |> loc_ghoster#expression)
;;
