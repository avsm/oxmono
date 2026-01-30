open! Base
open! Ppxlib
open! Ast_builder.Default

val ppx_anonymous_record : label

(** [anonymous_record_t] is {!type:Anonymous_record.t}. *)
val anonymous_record_t : loc:location -> longident_loc

(** [create] is {!val:Anonymous_record.Private.create}. *)
val create : loc:location -> longident_loc

(** [create_local] is {!val:Anonymous_record.Private.create_local}. *)
val create_local : loc:location -> longident_loc

(** [ppx_anonymous_record_internal__don't_match_on_this_manually] is a constructor in
    {!type:Anonymous_record.t}. *)
val ppx_anonymous_record_internal__don't_match_on_this_manually
  :  loc:location
  -> longident_loc

val raise_unsupported : loc:location -> why:label -> _
val raise_only_records_allowed : loc:location -> _

(** [lident_identifier_exn] returns the name of a [Lident].

    It raises if the [longident] is not a [Lident]. *)
val lident_identifier_exn : longident -> loc:location -> label

(** [ptyp_variant] returns a polymorphic constructor with [name] as its name and
    [field_type] as its field type. *)
val ptyp_variant
  :  ?field_type:core_type
  -> loc:location
  -> name:longident
  -> unit
  -> core_type

(** [sort_by_field_name_exn] sorts field names in lexicographic order.

    It raises if {!val:lident_identifier_exn} raises on any of the [longident_loc]s. *)
val sort_by_field_name_exn : (longident_loc * 'a) list -> (longident_loc * 'a) list

(** [validate_unique_field_names] validates whether a field name has only been used once. *)
val validate_unique_field_names : (longident_loc * _) list -> loc:location -> unit
