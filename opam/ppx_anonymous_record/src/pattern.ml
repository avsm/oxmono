open! Base
open! Ppxlib
open! Ast_builder.Default

let payload = Ast_pattern.(ppat __ none)

let create_anonymous_record fields ~loc =
  let tuple_representation =
    ppat_tuple ~loc (List.map fields ~f:(fun ({ txt = _; loc = _ }, pattern) -> pattern))
  in
  ppat_construct
    (Common.ppx_anonymous_record_internal__don't_match_on_this_manually ~loc)
    (Some tuple_representation)
    ~loc
;;

let constrain_anonymous_record fields ~loc =
  let field_names =
    (* Tuple types must have at least 2 components *)
    match fields with
    | [ ({ txt; loc }, (_ : pattern)) ] -> Common.ptyp_variant ~loc ~name:txt ()
    | _ ->
      ptyp_tuple
        (List.map fields ~f:(fun ({ txt; loc }, (_ : pattern)) ->
           Common.ptyp_variant ~loc ~name:txt ()))
        ~loc
  in
  ptyp_constr (Common.anonymous_record_t ~loc) [ field_names; ptyp_any ~loc ] ~loc
;;

let expand' fields ~loc =
  let fields = Common.sort_by_field_name_exn fields in
  ppat_constraint
    (create_anonymous_record fields ~loc)
    (constrain_anonymous_record fields ~loc)
    ~loc
;;

let expand { ppat_desc; ppat_loc = _; ppat_loc_stack = _; ppat_attributes = _ } ~loc =
  match ppat_desc with
  | Ppat_record (fields, (_ : closed_flag)) ->
    Common.validate_unique_field_names fields ~loc;
    expand' fields ~loc
  | _ -> Common.raise_only_records_allowed ~loc
;;
