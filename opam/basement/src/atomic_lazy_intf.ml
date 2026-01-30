(** A ['a Atomic_lazy.t] is a thread-safe version of the built-in [lazy_t], i.e. it both
    is sound with respect to modes and is safe to force from multiple threads. The
    thread-safety is relatively straight-forward, implemented using an atomic reference
    around the state of the [Atomic_lazy.t] and using blocking when multiple threads force
    the same lazy simultaneously. Making the implementation both efficient *and* sound
    with respect to modes, however, is more complicated.

    The implementation of [Atomic_lazy] in this library is optimized for performance, but
    requires magic to expose a safe and expressive interface into it. The safety of the
    interface alone is justified by a non-magic implementation in the tests of this
    library, which implements [S_any] below using a slightly less efficient
    representation. *)

(*_ NOTE: Do *not* rearrange the signatures below without considering how it affects the
    magic in [atomic_lazy.ml]. *)

(** The nonportable interface to [Atomic_lazy]. *)
module type S_any_nonportable = sig @@ portable
  type ('a : value_or_null) t : any

  val from_val : ('a : value_or_null). 'a -> 'a t
  val from_fun : ('a : value_or_null). (unit -> 'a) @ once -> 'a t
  val from_fun_fixed : ('a : value_or_null). ('a t -> 'a) @ once -> 'a t
  val force : ('a : value_or_null). 'a t @ local -> 'a
  val is_val : ('a : value_or_null). 'a t -> bool
  val peek : ('a : value). 'a t -> 'a Or_null_shim.t
  val peek_opt : ('a : value_or_null). 'a t -> 'a option
  val globalize : ('a : value_or_null) ('b : value_or_null). 'b -> 'a t @ local -> 'a t
end

(** The portable interface to [Atomic_lazy]. The behavior of the functions is identical to
    that of their corresponding function in [S_any_nonportable], and the naming scheme is
    written to align with what is expected by [ppx_template]. *)
module type S_any_portable = sig @@ portable
  type ('a : value_or_null) t : any

  val from_val__portable : ('a : value_or_null). 'a @ portable -> 'a t @ portable

  val from_fun__portable
    : ('a : value_or_null).
    (unit -> 'a @ portable) @ once portable -> 'a t @ portable

  val from_fun_fixed__portable
    : ('a : value_or_null).
    ('a t @ contended portable -> 'a @ portable) @ once portable -> 'a t @ portable

  val force__contended
    : ('a : value_or_null).
    'a t @ contended local portable -> 'a @ contended

  val is_val__contended : ('a : value_or_null). 'a t @ contended portable -> bool

  val peek__contended
    : ('a : value).
    'a t @ contended portable -> 'a Or_null_shim.t @ contended

  val peek_opt__contended
    : ('a : value_or_null).
    'a t @ contended portable -> 'a option @ contended

  val globalize__contended
    : ('a : value_or_null) ('b : value_or_null).
    'b -> 'a t @ contended local portable -> 'a t @ contended portable
end

(** The nonportable portion of [Atomic_lazy], specialized to [value] layout. *)
module type S_nonportable = sig @@ portable
  type ('a : value_or_null) t

  include S_any_nonportable with type ('a : value_or_null) t := 'a t
end

(** The portable portion of [Atomic_lazy], specialized to [value] layout. *)
module type S_portable = sig @@ portable
  type ('a : value_or_null) t

  include S_any_portable with type ('a : value_or_null) t := 'a t
end

(** The nonportable portion of [Atomic_lazy], specialized to [value] layout with a
    covariant type parameter. *)
module type S_nonportable_covariant = sig @@ portable
  type (+'a : value_or_null) t

  include S_nonportable with type ('a : value_or_null) t := 'a t
end

(** The full interface to [Atomic_lazy], with no kind/variance restrictions on the type.
    Specifically used in tests to show soundness of mode annotations. *)
module type S_any = sig @@ portable
  type ('a : value_or_null) t : any

  include S_any_nonportable with type ('a : value_or_null) t := 'a t
  include S_any_portable with type ('a : value_or_null) t := 'a t
end

(** The full interface to [Atomic_lazy], specialized to [value] layout with a covariant
    type parameter. This is the actual exposed interface of [Atomic_lazy]. *)
module type S = sig @@ portable
  type (+'a : value_or_null) t

  include S_any with type ('a : value_or_null) t := 'a t
end
