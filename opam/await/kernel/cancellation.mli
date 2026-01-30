@@ portable

(** A cancellation token represents an explicit, expected, and voluntary submission or
    desire to be interrupted asynchronously.

    Operations that potentially block or take a long time may take an explicit
    cancellation token as a parameter to allow such operations to be interrupted and
    canceled. Canceled operations should still return normally, as opposed to raising an
    exception, perhaps using the [Or_canceled.t] type to indicate that they have been
    canceled.

    Tokens can be created that are linked to a token from some outer level scope of the
    program. This way an individual operation or sequence of operations can be canceled
    independently of the outer level token while also respecting the cancelation of the
    outer level token.

    To implement a cancellable operation, one can poll the token or temporarily add
    triggers to the token using [add_trigger]. *)

(** [t] is the type of cancellation tokens. Tokens become canceled by calls to
    [Source.cancel]. *)
type t : value mod contended portable

(** [is_canceled t] is [true] if [t] has been canceled and [false] otherwise. Once
    [is_canceled t] is [true] it will never again become [false]. *)
val is_canceled : t @ local -> bool

(** [check t] is [Canceled] if [t] has been canceled, or [Completed ()] otherwise. *)
val check : t @ local -> unit Or_canceled.t

(** [same t1 t2] determines whether the tokens [t1] and [t2] are the one and the same. *)
val same : t @ local -> t @ local -> bool

(** [never] is a cancellation token that can never be canceled. *)
val never : t

(** [always] is a cancellation token that is already canceled. *)
val always : t

module Source : sig
  (** [t] is the type of cancellation token source that can be used to cause an associated
      token to become canceled. *)
  type t : value mod contended portable

  (** [cancel t] makes any tokens associated with [t] canceled. It will signal the
      triggers for any linked tokens. *)
  val cancel : t @ local -> unit
end

(** [is_cancellable t] is [true] if the token has an associated source and can be
    cancelled through it and otherwise [false]. *)
val is_cancellable : t @ local -> bool

(** [source t] returns the source of the cancellation token [This t] or [Null] in case the
    token is not cancellable. *)
val source : t @ local -> Source.t or_null

(** [with_ f] creates a fresh cancellation token and passes it to [f]. The token is
    canceled when [Source.cancel] has been called on the source.

    Panics if [t] still has unsignaled attached triggers when [f] finishes. *)
val with_ : ('a : value_or_null). (t @ local -> 'a) @ local once -> 'a

(** [with_linked t f] creates a fresh cancellation token linked to [t] and passes it to
    [f]. The token is canceled when either [t] is canceled or [Source.cancel] has been
    called on the source.

    Panics if [t] still has unsignaled attached triggers when [f] finishes. *)
val with_linked : ('a : value_or_null). t @ local -> (t @ local -> 'a) @ local once -> 'a

(** [with_linked_multi ts f] creates a fresh cancellation token and passes it to [f]. The
    token is canceled when any of [ts] are canceled or when [Source.cancel] has been
    called on the source.

    Panics if [t] still has unsignaled attached triggers when [f] finishes. *)
val with_linked_multi
  : ('a : value_or_null).
  t list @ local -> (t @ local -> 'a) @ local once -> 'a

module Link : sig
  (** The result of {!add_trigger}. *)
  type t =
    | Attached
    | Canceled
    | Signaled
  [@@deriving equal ~localize, sexp ~stackify]
end

(** [add_trigger t s] attaches [s] to the token [t] so that it will be signaled when [t]
    is canceled.

    Returns
    - [Canceled] if [t] was already canceled and so [s] was not attached to it,
    - [Signaled] if [s] was already signaled and so wasn't attached to [t], or
    - [Attached] if [t] was not canceled and [s] was not signaled and so [s] has been
      attached to [t].

    [add_trigger] does not update the trigger [s]. In particular, a return value of
    [Canceled] tells nothing about the state of [s]. *)
val add_trigger : t @ local -> Trigger.Source.t -> Link.t

module Expert : sig
  (** [globalize t] is [t @ global].

      Cancellation tokens are generally strictly scoped as they are implicitly canceled at
      the end of their scope. Within a token's scope, however, one may e.g. need to link
      to it from a newly spawned thread or domain and in that case one the token needs to
      be globalized. *)
  val globalize : t @ local -> t

  (** [create ()] creates a fresh unscoped cancellation token.

      Cancellation tokens are generally strictly scoped. When using an unscoped token one
      should arrange for the token to be canceled after it is no longer needed to ensure
      that any attached resources will be cleaned up. *)
  val create : unit -> t
end

module For_testing : sig
  (** [get_countdown t] returns the internal countdown to cleanup of signaled triggers for
      the purpose of testing that the cleanup logic works properly. *)
  val get_countdown : t @ local -> int
end
