@@ portable

(** An awaitable represents an atomic location whose change of value can be awaited upon.
    Awaitables can be used to implement all kinds of blocking data structures, including
    locks, condition variables, and blocking queues. *)

open Portable_kernel
open Await_kernel

(** {1 Atomic API} *)

(** An awaitable atomic reference to a value of type ['a]. *)
type (!'a : value_or_null) t : value mod contended portable

(** [make v] creates a new awaitable atomic reference with the given initial value [v].

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation. *)
val make : ('a : value_or_null). ?padded:bool @ local -> 'a @ contended portable -> 'a t

(** [get t] gets the current value of the awaitable atomic reference. *)
val get : ('a : value_or_null). 'a t @ local -> 'a @ contended portable

(** [set t v] sets a new value [v] for the awaitable atomic reference. *)
val set : ('a : value_or_null). 'a t @ local -> 'a @ contended portable -> unit

(** [exchange t v] sets a new value [v] for the awaitable atomic reference and returns the
    current value. *)
val exchange
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable -> 'a @ contended portable

module Compare_failed_or_set_here = Atomic.Compare_failed_or_set_here

(** [compare_and_set t ~if_phys_equal_to:seen ~replace_with:v] sets the new value of [t]
    to [v] only if its current value is physically equal to [seen] -- the comparison and
    the set occur atomically. Returns [true] if the comparison succeeded (so the set
    happened) and [false] otherwise. *)
val compare_and_set
  : ('a : value_or_null).
  'a t @ local
  -> if_phys_equal_to:'a @ contended portable
  -> replace_with:'a @ contended portable
  -> Compare_failed_or_set_here.t

(** [compare_exchange t ~if_phys_equal_to:seen ~replace_with:v] sets the new value of [s]
    to [v] only if its current value is physically equal to [seen] -- the comparison and
    the set occur atomically. Returns the previous value. *)
val compare_exchange
  : ('a : value_or_null).
  'a t @ local
  -> if_phys_equal_to:'a @ contended portable
  -> replace_with:'a @ contended portable
  -> 'a @ contended portable

(** [fetch_and_add t n] atomically increments the value of [t] by [n], and returns the
    current value (before the increment). *)
val fetch_and_add : int t @ local -> int -> int

(** [incr t] atomically increments the value of [t] by [1]. *)
val incr : int t @ local -> unit

(** [decr t] atomically decrements the value of [t] by [1]. *)
val decr : int t @ local -> unit

(** [update t ~pure_f] atomically updates [t] to be the result of [pure_f (get t)].
    [pure_f] may be called multiple times, so should be free of side effects. *)
val update
  : ('a : value_or_null).
  'a t @ local
  -> pure_f:('a @ contended portable -> 'a @ contended portable) @ local
  -> unit

(** [get_and_update t ~pure_f] atomically updates [t] to be the result of
    [pure_f (get t)]. [pure_f] may be called multiple times, so should be free of side
    effects. Returns the old value. *)
val get_and_update
  : ('a : value_or_null).
  'a t @ local
  -> pure_f:('a @ contended portable -> 'a @ contended portable) @ local
  -> 'a @ contended portable

(** {1 Futex API} *)

(** Result of [await] or [await_or_cancel]. *)
type _ await =
  | Signaled : [> `Signaled ] await
  | Terminated : [> `Terminated ] await
  | Canceled : [> `Canceled ] await

(** [await w t ~until_phys_unequal_to:v] suspends the caller until the awaitable [t] is
    explicitly signaled and/or has a value physically unequal to [v].

    The return value is {!Signaled} in case the await was woken up due to a {!signal} or
    {!broadcast} on the awaitable. Otherwise the return value is {!Terminated}.

    This operation is subject to the ABA problem. An await for a value other than [A] may
    not return after the awaitable is signaled while having the value [B], because at a
    later point the awaitable has again the value [A]. Furthermore, by the time an await
    for value other than [A] returns, the awaitable might already again have the value
    [A].

    Atomic operations that change the value of an awaitable do not implicitly wake up
    awaiters.

    [await] establishes a {b happens-before} relationship with any reads and writes on the
    current thread that precede it, and the comparison of the current value of the
    awaitable [t] with the given value [v]. *)
val await
  : ('a : value_or_null).
  Await.t @ local
  -> 'a t @ local
  -> until_phys_unequal_to:'a @ contended portable
  -> [ `Signaled | `Terminated ] await

(** [await_or_cancel w c t ~until_phys_unequal_to:v] is like
    [await w t ~until_phys_unequal_to:v], but the caller will also be resumed in case the
    cancellation token [c] becomes canceled.

    The return value is {!Signaled} in case the await was woken up due to a {!signal} or
    {!broadcast} on the awaitable. Otherwise the return value is either {!Terminated} in
    case [w] is terminated, and {!Canceled} in case [c] is canceled. *)
val await_or_cancel
  : ('a : value_or_null).
  Await.t @ local
  -> Cancellation.t @ local
  -> 'a t @ local
  -> until_phys_unequal_to:'a @ contended portable
  -> [ `Signaled | `Terminated | `Canceled ] await

(** [signal t] tries to wake up at least one awaiter on the awaitable location [t]. *)
val signal : ('a : value_or_null). 'a t @ local -> unit

(** [broadcast t] tries to wake up all the awaiters on the awaitable location [t]. *)
val broadcast : ('a : value_or_null). 'a t @ local -> unit

(** An expert interface that allows an await to be setup separately from suspending the
    thread of control. This makes it possible to e.g. implement condition variables, which
    requires setting up the await before unlocking the associated mutex, and also makes it
    possible to await for one of many things. *)
module Awaiter : sig
  type ('a : value_or_null) awaitable := 'a t

  (** Represents a single use awaiter of a signal to an awaitable. *)
  type t

  (** [create_and_add t s ~until_phys_unequal_to:v] creates a single-use awaiter with the
      given trigger [t], adds the awaiter to the FIFO associated with the awaitable [t],
      and returns the awaiter. *)
  val%template create_and_add
    : ('a : value_or_null).
    'a awaitable @ l
    -> Trigger.Source.t
    -> until_phys_unequal_to:'a @ contended portable
    -> t @ l
  [@@mode l = (global, local)]

  (** [cancel_and_remove a] signals the trigger of the awaiter [a] and cancels the awaiter
      such that it will not be selected by {!signal} or {!broadcast}, and removes the
      awaiter [a] from the awaitable into which [a] was originally added.

      This can be called even after something has already signaled the trigger of the
      awaiter [a]. In such cases [cancel_and_remove] will signal another awaiter on the
      awaitable, if one is available, so that the other awaiter can react to the signal
      instead.

      If the associated trigger is used with only one awaiter and the await on the trigger
      was neither terminated nor canceled, there is no need to explicitly
      [cancel_and_remove], because the awaiter [a] has already been used and removed from
      the awaitable . *)
  val cancel_and_remove : t @ local -> unit
end

module For_testing : sig
  (** [length t] returns an upper bound on the length of the internal queue of awaiters
      for testing purposes. *)
  val length : ('a : value_or_null). 'a t @ local -> int
end
