@@ portable

(** A poisonable one-shot countdown latch. *)

open Base
open Await_kernel

(** A [Countdown_latch.t] is a synchronization aid that allows one or more threads to wait
    until a number of operations being performed in other threads complete.

    A countdown latch is {{!create} initialized} with a {i count}. The {!await} method
    blocks until a call to {!decr} decrements the count to zero, after which all waiting
    threads are released. A countdown latch is one-shot -- the count cannot be changed
    after it reaches zero. If you need a version that resets the count, consider using a
    {!Barrier} instead, though note that with a barrier the only operation available is
    waiting - it is not possible to decrement without blocking. *)
type t : value mod contended portable [@@deriving sexp_of]

(** [max_count] is the maximum allowed count for a countdown latch. *)
val max_count : int

(** [create n] returns a new countdown latch with its count initialized to [n].

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation.

    @raise Invalid_argument if [n] is negative or greater than {!max_count}. *)
val create : ?padded:bool @ local -> int -> t

(** [count t] returns the current count of the countdown latch [t]. *)
val count : t @ local -> int

(** [poison t] marks the countdown latch as poisoned. Concurrent and subsequent calls to
    {!await}, {!await_or_cancel}, and {!incr} will raise the {!Poisoned} exception. *)
val poison : t @ local -> unit

(** [is_poisoned t] determines whether the countdown latch has been poisoned. *)
val is_poisoned : t @ local -> bool

(** [incr t] increments the count of the countdown latch [t].

    The count is not logically modified after it has reached zero.

    @raise Poisoned if the latch has been poisoned.
    @raise Invalid_argument in case the latch has already previously reached zero. *)
val incr : t @ local -> unit

(** [decr t] decrements the count of the countdown latch [t], releasing all waiting
    threads if the count reaches zero.

    The count is not logically modified after it has reached zero.

    @raise Invalid_argument in case the latch has already previously reached zero. *)
val decr : t @ local -> unit

(** [await w t] suspends the caller until the latch [t] is decremented to zero.

    @raise Poisoned if the latch has been poisoned.
    @raise Terminated if [w] is terminated before the latch reaches zero. *)
val await : Await.t @ local -> t @ local -> unit

(** [await_or_cancel w c t] is [Completed ()] if [c] is not cancelled or [Canceled]
    otherwise

    @raise Poisoned if the latch has been poisoned.
    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val await_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> t @ local
  -> unit Or_canceled.t
