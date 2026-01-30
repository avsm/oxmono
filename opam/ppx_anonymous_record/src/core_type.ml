open! Base
open! Ppxlib
open! Ast_builder.Default

let payload = Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))

let constraint_ pexp_desc ~loc ~parent_name =
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
  | Pexp_constraint
      ( { pexp_desc; pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ }
      , Some core_type
      , _ ) ->
    (match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
     | Pexp_ident { txt; loc = _ } when String.equal (Longident.name txt) parent_name ->
       core_type
     | _ ->
       Common.raise_unsupported
         ~loc
         ~why:"records fields cannot be assigned a value when defining a core type")
  | _ -> Common.raise_unsupported ~loc ~why:"record fields must be constrained to a type"
;;

let expand' fields ~loc =
  let fields = Common.sort_by_field_name_exn fields in
  let field_names, tuple_representation =
    fields
    |> List.map
         ~f:
           (fun
             ( { txt; loc }
             , { pexp_desc; pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ } )
           ->
           ( Common.ptyp_variant ~loc ~name:txt ()
           , constraint_ pexp_desc ~loc ~parent_name:(Longident.name txt) ))
    |> List.unzip
    (* Tuple types must have at least 2 components *)
    |> function
    | [ field_name ], [ constraint_ ] -> field_name, constraint_
    | field_names, tuple_representation ->
      ( ptyp_tuple (* This interferes with [Common.anonymous_record_t]'s ident location *)
          field_names
          ~loc:{ loc with loc_ghost = true }
      , ptyp_tuple tuple_representation ~loc )
  in
  ptyp_constr
    (Common.anonymous_record_t ~loc:{ loc with loc_ghost = true })
    [ field_names; tuple_representation ]
    ~loc
;;

let expand { pexp_desc; pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ } ~loc =
  match pexp_desc with
  | Pexp_record (fields, (None | Some (_ : expression))) ->
    Common.validate_unique_field_names fields ~loc;
    expand' fields ~loc
  | _ -> Common.raise_only_records_allowed ~loc
;;
