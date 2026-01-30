open! Core
open! Import

module type%template [@modality p = (portable, nonportable)] S = sig @@ p
  type t

  include Sexpable.S with type t := t

  (** The binable implementation goes via sexp. This gives us the same flexibility
      guarantees, however at the cost of bin_io performance. *)
  include Binable.S with type t := t

  module Shallow_inflexible : sig
    (** This [t_of_sexp] implementation raises on unknown variant constructors and unknown
        record fields, instead of putting them into [Other] or [Tags] *)
    val t_of_sexp : Sexp.t -> t
  end

  module Deep_inflexible : sig
    (** This [t_of_sexp] implementation behaves like [Shallow_inflexible], but forces all
        contained flexible_sexp types to behave inflexibly as well. *)
    val t_of_sexp : Sexp.t -> t
  end
end
