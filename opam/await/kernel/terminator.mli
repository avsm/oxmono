@@ portable

(** A termination token represents an implicit and unexpected interrupt request to be used
    in case of panics. *)

(** [t] is the type of termination tokens. Tokens become terminated by calls to
    [Source.terminate]. *)
type t : value mod contended portable

(** [is_terminated t] is [true] if [t] has been terminated and [false] otherwise. Once
    [is_terminated t] is [true] it will never again become [false]. *)
val is_terminated : t @ local -> bool

(** [same t1 t2] determines whether the tokens [t1] and [t2] are the one and the same. *)
val same : t @ local -> t @ local -> bool

(** [never] is a termination token that is never terminated *)
val never : t

(** [always] is a termination token that is already terminated. *)
val always : t

module Source : sig
  (** [t] is the type of termination sources that can be used to cause an associated
      termination token to become terminated. *)
  type t : value mod contended portable

  (** [terminate t] makes the token associated with [t] terminated, and signals any
      triggers attached to it. *)
  val terminate : t @ local -> unit
end

(** [is_terminatable t] is [true] if the token has an associated source and can be
    terminated through it and otherwise [false]. *)
val is_terminatable : t @ local -> bool

(** [source t] returns the source of the termination token [This t] or [Null] in case the
    token is not terminatable. *)
val source : t @ local -> Source.t or_null

(** [with_ f] creates a fresh termination token with an associated source and passes both
    to [f]. The token is terminated when [Source.terminate] has been called on the source.

    Panics if [t] still has unsignaled attached triggers when [f] finishes. *)
val with_ : ('a : value_or_null). (t @ local -> 'a) @ local once -> 'a

(** [with_linked t f] creates a fresh termination token with an associated source and
    passes both to [f]. The token is terminated when either [t] is terminated or
    [Source.terminate] has been called on the source.

    Panics if [t] still has unsignaled attached triggers when [f] finishes. *)
val with_linked : ('a : value_or_null). t @ local -> (t @ local -> 'a) @ local once -> 'a

(** [with_linked_multi ts f] creates a fresh termination token with an associated source
    and passes them both to [f]. The token is canceled when any of [ts] are terminated or
    when [Source.terminate] has been called on the source.

    Panics if [t] still has unsignaled attached triggers when [f] finishes. *)
val with_linked_multi
  : ('a : value_or_null).
  t list @ local -> (t @ local -> 'a) @ local once -> 'a

module Link : sig
  (** The result of {!add_trigger}. *)
  type t =
    | Attached
    | Terminated
    | Signaled
  [@@deriving equal ~localize, sexp ~stackify]
end

(** [add_trigger t s] attaches [s] to the token [t] so that it will be signalled when [t]
    is terminated.

    Returns
    - [Terminated] if [t] was already terminated and so [s] was not attached to it,
    - [Signaled] if [s] was already signaled and so wasn't attached to [t], or
    - [Attached] if [t] was not terminated and [s] was not signaled and so [s] has been
      attached to [t].

    [add_trigger] does not update the trigger [s]. In particular, a return value of
    [Terminated] tells nothing about the state of [s]. *)
val add_trigger : t @ local -> Trigger.Source.t -> Link.t

(** [cancellation t] is the underlying cancellation token of the terminator [t].

    This allows turning termination into cancellation for the purpose of carefully
    handling resources in critical sections where implicit termination is not desired. *)
val cancellation : t @ local -> Cancellation.t @ local

module Expert : sig
  (** [globalize t] is [t @ global].

      Termination tokens are generally strictly scoped as they are implicitly terminated
      at the end of their scope. Within a token's scope, however, one may e.g. need to
      link to it from a newly spawned thread or domain and in that case one the token
      needs to be globalized. *)
  val globalize : t @ local -> t

  (** [create ()] creates a fresh unscoped termination token.

      Termination tokens are generally strictly scoped. When using an unscoped token one
      should arrange for the token to be termminated after it is no longer needed to
      ensure that any attached resources will be cleaned up. *)
  val create : unit -> t
end
