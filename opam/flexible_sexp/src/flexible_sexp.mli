@@ portable

module Record = Record
module Variant = Variant

(** Run [f] within a "deep inflexible" context. Inside such a context, *all*
    [flexible_sexp] types will be deserialised inflexibly. See [Record.Deep_inflexible] or
    [Variant.Deep_inflexible] for details on what inflexible means.

    This function can safely be nested; inner nestings have no special effect. *)
val within_inflexible_context : f:(unit -> 'a) @ local unyielding -> 'a

module Stable : sig
  module Record = Record.Stable
  module Variant = Variant.Stable
end
