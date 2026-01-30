@@ portable

open Base

(** [Terminated] is an exception indicating that an operation has been terminated. *)
exception Terminated

(** [t] is the type of implementations of awaiting. Operations that need to await
    something take a [t] that provides an implementation of awaiting for them to use.

    Any awaiting operation can be terminated by the awaiting implementation, which results
    in a [Terminated] exception being raised. *)
type t : value mod contended portable

(** [with_ terminator ~yield ~await c ~f] calls [f] with an awaiter that has [terminator]
    as its terminator, [yield c] as its implementation of [yield], and [await c] as its
    implementation of [await]. *)
val with_
  : 'c ('r : value_or_null).
  terminator:Terminator.t @ local
  -> yield:('c @ local -> unit) or_null @ local
  -> await:('c @ local -> Trigger.t -> unit) @ local
  -> 'c @ local
  -> f:(t @ local -> 'r @ forkable local once unique) @ local once
  -> 'r @ forkable local once unique

(** [create_global ~terminator ~yield ~await c] creates a global awaiter with [yield c] as
    its implementation of [yield] and [await c] as its implementation of [await]. Both
    [yield] and [await] must be portable as the returned [Await.t] is not local, allowing
    it to escape to other capsules. *)
val create_global
  :  terminator:Terminator.t
  -> yield:('c @ contended portable -> unit) or_null @ portable
  -> await:('c @ contended portable -> Trigger.t -> unit) @ portable
  -> 'c @ contended portable
  -> t

(** [terminator t] is the terminator associated with [t]. Awaiting operations should
    attempt to cancel themselves if they have been terminated, raising [Terminated] if
    they succeed in doing so. *)
val terminator : t @ local -> Terminator.t @ local

(** [await t ~on_terminate ~await_on] will use [t] to attach [on_terminate] to be
    signalled on termination and to wait until [await_on] has been signaled.

    Note that the [on_terminate] trigger may not be used by [await]. The [await_on]
    trigger will be used before [await] returns. *)
val await : t @ local -> on_terminate:Trigger.Source.t -> await_on:Trigger.t -> unit

(** [await_until_terminated t trigger] is equivalent to
    [await t ~on_terminate:(Trigger.source trigger) ~await_on:trigger]. *)
val await_until_terminated : t @ local -> Trigger.t -> unit

(** [await_until_terminated_or_canceled t c trigger] is like
    [await_until_terminated t trigger] except it will also return in case the cancellation
    token has been cancelled . *)
val await_until_terminated_or_canceled
  :  t @ local
  -> Cancellation.t @ local
  -> Trigger.t
  -> unit

(** [await_with_terminate t trigger ~terminate r] will attach a trigger to call
    [terminate r] to the {!terminator} of [t] and wait until [trigger] has been signalled. *)
val await_with_terminate
  :  t @ local
  -> Trigger.t
  -> terminate:('r @ contended once portable unique -> unit) @ once portable
  -> 'r @ contended once portable unique
  -> unit

(** [await_with_terminate_or_cancel t c trigger ~terminate_or_cancel r] will attach a
    trigger to call [terminate_or_cancel r] to both the {!terminator} of [t] and to the
    given cancellation token [c] and then wait until [trigger] has been signalled. *)
val await_with_terminate_or_cancel
  :  t @ local
  -> Cancellation.t @ local
  -> Trigger.t
  -> terminate_or_cancel:('r @ contended once portable unique -> unit) @ once portable
  -> 'r @ contended once portable unique
  -> unit

(** [is_terminated t] is [Terminator.is_terminated (terminator t)]. *)
val is_terminated : t @ local -> bool

(** [with_terminator t new_terminator] is an awaiter [u] like [t] where [terminator u] is
    [new_terminator].

    The main use case of [with_terminator] is to protect a blocking operation from being
    terminated by replacing the terminator with {!Terminator.never}:
    {[
      blocking_operation (with_terminator t Terminator.never)
    ]} *)
val with_terminator : t @ local -> Terminator.t @ local -> t @ local

(** [await_never_terminated t trigger] is
    [await_until_terminated (with_terminator t Terminator.never) trigger]. *)
val await_never_terminated : t @ local -> Trigger.t -> unit

(** [yield t] yields to the scheduler using the implementation of yielding associated with
    [t].

    @raise [Terminated] if the terminator associated with [t] has been terminated. *)
val yield : t @ local -> unit

module For_testing : sig
  (** [with_never ~f] calls [f] with an implementation of awaiting that should never be
      used. If [await] is called with the implementation, it will raise.

      This is useful in tests, for testing operations which otherwise might conditionally
      block in a single-threaded manner that never needs to block.

      Bear in mind that proper implementations of [await] do not usually raise and are not
      documented to potentially raise. This means that abstractions built on await may
      e.g. leave the program in an invalid state when using [with_never]. *)
  val with_never
    : ('r : value_or_null).
    f:(t @ local -> 'r @ forkable local once unique) @ local once
    -> 'r @ forkable local once unique
end
