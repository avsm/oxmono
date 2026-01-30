open! Base
open! Ppxlib
open! Ast_builder.Default

let ppx_anonymous_record = "ppx_anonymous_record"
let ppx_anonymous_record_runtime = "Ppx_anonymous_record_runtime"
let anonymous_record = "Anonymous_record"

let anonymous_record_t ~loc =
  Loc.make
    ~loc
    (Longident.parse [%string "%{ppx_anonymous_record_runtime}.%{anonymous_record}.t"])
;;

let create =
  [%string "%{ppx_anonymous_record_runtime}.%{anonymous_record}.Private.create"]
;;

let create_local ~loc = Loc.make ~loc (Longident.parse [%string "%{create}_local"])
let create ~loc = Loc.make ~loc (Longident.parse create)

let ppx_anonymous_record_internal__don't_match_on_this_manually ~loc =
  Loc.make ~loc (Lident "Ppx_anonymous_record_internal__don't_match_on_this_manually")
;;

let raise_unsupported ~loc ~why =
  Location.raise_errorf ~loc "Unsupported use of `%s' (%s)." ppx_anonymous_record why
;;

let raise_only_records_allowed ~loc =
  raise_unsupported ~loc ~why:"you can only use it with a single record"
;;

let lident_identifier_exn longident ~loc =
  match longident with
  | Lident identifier -> identifier
  | (Ldot _ | Lapply _) as longident ->
    raise_unsupported
      ~loc
      ~why:
        ("record field names are expected to conform to normal record field name \
          constraints. Malformed field name: "
         ^ Longident.name longident)
;;

let ptyp_variant ?field_type ~loc ~name () =
  let constructor_name = lident_identifier_exn name ~loc in
  let field_type =
    match field_type with
    | None -> []
    | Some field_type -> [ field_type ]
  in
  ptyp_variant
    ~loc
    [ { prf_desc = Rtag (Loc.make ~loc constructor_name, true, field_type)
      ; prf_loc = loc
      ; prf_attributes = []
      }
    ]
    Closed
    None
;;

let sort_by_field_name_exn fields =
  List.sort
    fields
    ~compare:
      (Comparable.lift String.ascending ~f:(fun ({ txt; loc }, (_ : _)) ->
         lident_identifier_exn txt ~loc))
;;

let contains_duplicates field_names =
  let rec check_duplicates set = function
    | [] -> None
    | hd :: tl ->
      if Set.mem set hd
      then Some hd
      else (
        let new_set = Set.add set hd in
        check_duplicates new_set tl)
  in
  check_duplicates (Set.empty (module String)) field_names
;;

let validate_unique_field_names fields ~loc =
  match
    contains_duplicates
      (List.map fields ~f:(fun ({ txt; loc }, (_ : _)) -> lident_identifier_exn txt ~loc))
  with
  | None -> ()
  | Some field_name ->
    raise_unsupported
      ~loc
      ~why:("record field names must be unique. Duplicate field name: " ^ field_name)
;;
