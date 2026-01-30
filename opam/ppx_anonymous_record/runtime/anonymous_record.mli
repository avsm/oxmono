open! Base

(** [t] is a record without a name or type declaration.

    @param 'field_names refers to the labeled elements (field names)
    @param 'tuple_representation refers to the underlying tuple type *)
type (+'field_names, +'tuple_representation) t = private
  | Ppx_anonymous_record_internal__don't_match_on_this_manually of 'tuple_representation
[@@unboxed]

(** [Private] exposes a value for [ppx_anonymous_record]. It is not meant to be used
    elsewhere.

    See {{:https://opensource.janestreet.com/standards/#private-submodules} style guide}. *)
module Private : sig
  val create : 'tuple_representation -> (_, 'tuple_representation) t
  val create_local : local_ 'tuple_representation -> local_ (_, 'tuple_representation) t
end
