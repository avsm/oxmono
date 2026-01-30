open! Base
open! Ppxlib
open! Ast_builder.Default

let payload = Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))

let create_anonymous_record fields ~loc ~create =
  let tuple_representation =
    pexp_tuple
      (List.map fields ~f:(fun ({ txt = _; loc = _ }, expression) -> expression))
      ~loc
  in
  pexp_apply (pexp_ident (create ~loc) ~loc) [ Nolabel, tuple_representation ] ~loc
;;

let constrain_anonymous_record fields ~loc =
  let field_names =
    (* Tuple types must have at least 2 components *)
    match fields with
    | [ ({ txt; loc }, (_ : expression)) ] -> Common.ptyp_variant ~loc ~name:txt ()
    | _ ->
      ptyp_tuple
        (List.map fields ~f:(fun ({ txt; loc }, (_ : expression)) ->
           Common.ptyp_variant ~loc ~name:txt ()))
        ~loc
  in
  ptyp_constr (Common.anonymous_record_t ~loc) [ field_names; ptyp_any ~loc ] ~loc
;;

let expand' fields ~loc ~create =
  let fields = Common.sort_by_field_name_exn fields in
  pexp_constraint
    (create_anonymous_record fields ~loc ~create)
    (constrain_anonymous_record fields ~loc)
    ~loc
;;

let expand_internal
  { pexp_desc; pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ }
  ~loc
  ~create
  =
  match pexp_desc with
  | Pexp_record (fields, (None | Some (_ : expression))) ->
    Common.validate_unique_field_names fields ~loc;
    expand' fields ~loc ~create
  | _ -> Common.raise_only_records_allowed ~loc
;;

let expand = expand_internal ~create:Common.create
let expand_local = expand_internal ~create:Common.create_local
