open! Base

type ('field_names, 'tuple_representation) t =
  | Ppx_anonymous_record_internal__don't_match_on_this_manually of 'tuple_representation
[@@unboxed]

module Private = struct
  let create x = Ppx_anonymous_record_internal__don't_match_on_this_manually x
  let create_local x = Ppx_anonymous_record_internal__don't_match_on_this_manually x
end
