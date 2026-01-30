open! Base
open Ppxlib
open Ast_builder.Default

let loc_to_expr loc =
  let mk_pos { pos_fname; pos_lnum; pos_bol; pos_cnum } =
    [%expr
      { pos_fname = [%e estring ~loc pos_fname]
      ; pos_lnum = [%e eint ~loc pos_lnum]
      ; pos_bol = [%e eint ~loc pos_bol]
      ; pos_cnum = [%e eint ~loc pos_cnum]
      }]
  in
  [%expr
    ({ loc_start = [%e mk_pos loc.loc_start]
     ; loc_end = [%e mk_pos loc.loc_end]
     ; loc_ghost = [%e ebool ~loc loc.loc_ghost]
     }
     : Ppx_quick_test_runtime_lib.Location.t)]
;;

module Parameters = struct
  type one =
    | Locally_abstract_type
    | Param of
        { label : arg_label
        ; core_type : core_type
        }

  type t = one list

  let parse_one (parameter : Ppxlib_jane.Shim.Pexp_function.function_param) =
    match parameter.pparam_desc with
    | Pparam_newtype _ -> Locally_abstract_type
    | Pparam_val (label, _, pattern) ->
      let () =
        match label with
        | Nolabel -> ()
        | Optional _ | Labelled _ ->
          Location.raise_errorf
            ~loc:parameter.pparam_loc
            "labelled and optional parameters are not currently supported"
      in
      let core_type =
        match Ppxlib_jane.Shim.Pattern_desc.of_parsetree pattern.ppat_desc with
        | Ppat_constraint (_, Some core_type, _) -> core_type
        | _ ->
          Location.raise_errorf
            ~loc:parameter.pparam_loc
            {|"ppx_quick_test" expects the parameter to be a constraint in the form (PARAM_NAME : TYPE). For example:
{[
    let%%quick_test my_test (x : int) = assert (x * x >= 0) in
]}
|}
      in
      Param { label; core_type }
  ;;

  let parse { pvb_expr = expression; pvb_loc; _ } =
    match
      Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:pvb_loc expression.pexp_desc
    with
    | Pexp_function (parameters, _constraint, Pfunction_body _body) ->
      List.map parameters ~f:parse_one
    | Pexp_function (_, _, Pfunction_cases _) ->
      Location.raise_errorf ~loc:expression.pexp_loc "[function] syntax not supported"
    | _ ->
      Location.raise_errorf
        ~loc:expression.pexp_loc
        {|
"ppx_quick_test" expects a function with at least one parameter. For example:
{[
    let%%quick_test my_test (x : int) = assert (x * x >= 0) in
]}
|}
  ;;
end

let make_run_test
  ~loc
  ({ passthroughs; remember_failures; examples } : Quick_test_attributes.t)
  (parameters : Parameters.t)
  predicate
  rest
  =
  let here_pos = Ppx_here_expander.lift_position ~loc in
  let passthrough param =
    match Map.find passthroughs param with
    | None -> [%expr None]
    | Some expr -> [%expr Some [%e expr]]
  in
  let sexp_examples =
    match remember_failures with
    | Ignore -> [%expr Ignore]
    | Empty attr_location -> [%expr Empty [%e loc_to_expr attr_location]]
    | List examples ->
      let example_exprs =
        Nonempty_list.to_list examples
        |> List.map ~f:(fun { expr; body; body_location; delimiters } ->
          let body = pexp_constant ~loc (Pconst_string (body, loc, delimiters)) in
          let body_location = loc_to_expr body_location in
          let delimiters =
            match delimiters with
            | None -> [%expr None]
            | Some delimiters -> [%expr Some [%e estring ~loc delimiters]]
          in
          [%expr
            { sexp_string = [%e expr]
            ; body = [%e body]
            ; body_location = [%e body_location]
            ; delimiters = [%e delimiters]
            }])
      in
      [%expr List [%e elist ~loc example_exprs]]
  in
  let examples =
    let examples_flat_tuple_list = Option.value examples ~default:[%expr []] in
    match parameters with
    | [] -> assert false
    | [ _ ] ->
      (* single-parameter examples are not represented with a tuple *)
      examples_flat_tuple_list
    | _ :: _ :: _ ->
      let idents =
        List.filter_map parameters ~f:(function
          | Locally_abstract_type -> None
          | Param _ -> Some (gen_symbol ()))
      in
      let flat_tuple_pattern =
        (* e.g. [x1, x2, x3] *)
        ppat_tuple ~loc (List.map idents ~f:(pvar ~loc))
      in
      let nested_tuple_expression =
        (* e.g. [(x1, (x2, (x3, ())))] *)
        List.fold_right idents ~init:[%expr ()] ~f:(fun ident acc ->
          pexp_tuple ~loc [ evar ~loc ident; acc ])
      in
      [%expr
        Core.List.map [%e examples_flat_tuple_list] ~f:(fun [%p flat_tuple_pattern] ->
          [%e nested_tuple_expression])]
  in
  let params =
    let ones =
      List.filter_map parameters ~f:(fun parameter ->
        match parameter with
        | Locally_abstract_type -> None
        | Param { label = _; core_type } ->
          let () = Ppx_quickcheck.registered in
          let generator = [%expr [%quickcheck.generator: [%t core_type]]] in
          let shrinker = [%expr [%quickcheck.shrinker: [%t core_type]]] in
          let () = Ppx_sexp_conv.registered in
          let sexp_of = [%expr [%sexp_of: [%t core_type]]] in
          let of_sexp =
            match remember_failures with
            | Ignore -> [%expr None]
            | Empty _ | List _ -> [%expr Some [%of_sexp: [%t core_type]]]
          in
          Some
            [%expr
              { sexp_of = [%e sexp_of]
              ; of_sexp = [%e of_sexp]
              ; generator = [%e generator]
              ; shrinker = [%e shrinker]
              }])
    in
    match ones with
    | [] -> assert false
    | [ one ] -> [%expr One [%e one]]
    | many -> [%expr Many [%e elist ~loc many]]
  in
  let filename_rel_to_project_root =
    estring ~loc (Ppx_here_expander.expand_filename loc.loc_start.pos_fname)
  in
  [%expr
    Ppx_quick_test_config.IO.bind
      (Ppx_quick_test_config.run_quick_test
         ~here_pos:[%e here_pos]
         ~config:[%e passthrough Config]
         ~cr:[%e passthrough Cr]
         ~hide_positions:[%e passthrough Hide_positions]
         ~trials:[%e passthrough Trials]
         ~examples:[%e examples]
         ~sexp_examples:[%e sexp_examples]
         ~params:[%e params]
         ~filename_rel_to_project_root:[%e filename_rel_to_project_root]
         [%e predicate])
      ~f:(fun () -> [%e rest])]
;;

let expand ~loc value_binding rec_flag rest =
  let value_binding, vb_attrs = Quick_test_attributes.consume value_binding in
  let params = Parameters.parse value_binding in
  let predicate =
    match value_binding.pvb_pat.ppat_desc with
    | Ppat_var { txt; loc } -> evar ~loc txt
    | _ ->
      Location.raise_errorf
        ~loc:value_binding.pvb_pat.ppat_loc
        {|"ppx_quick_test" expects the predicate to be bound to an identifier. For example:
{[
    let%%quick_test my_test = fun (x : int) -> assert (x * x >= 0) in
]}
or
{[
    let%%quick_test my_test (x : int) = assert (x * x >= 0) in
]}
|}
  in
  let run_test_and_rest =
    make_run_test ~loc vb_attrs params predicate rest |> Ppx_helpers.ghoster#expression
  in
  pexp_let ~loc rec_flag [ value_binding ] run_test_and_rest
;;
