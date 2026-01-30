@@ portable

(** A multi-producer, multi-consumer concurrent stack. *)

open Base
open Await_kernel

(** A simple, list-based multi-producer multi-consumer stack which provides both lock-free
    and blocking operations. *)
type !'a t : value mod contended portable

[%%rederive: type nonrec ('a : value mod contended) t = 'a t [@@deriving sexp_of]]

(** [create ()] creates and returns a new empty stack.

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation. *)
val create : ?padded:bool @ local -> unit -> 'a t

(** [push t a] enqueues [a] at the head of [t]. *)
val push : 'a t @ local -> 'a @ contended portable -> unit

(** [pop await t] removes and returns the most-recently-pushed value from the head of [t],
    blocking using [await] if [t] is empty. *)
val pop : Await.t @ local -> 'a t @ local -> 'a @ contended portable

(** [pop_or_cancel await c t] is [Completed (pop await t)] if [c] is not cancelled,
    otherwise it is [Canceled].

    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val pop_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'a t @ local
  -> 'a Or_canceled.t @ contended portable

(** [pop_nonblocking t] removes and returns the most-recently-pushed value from the head
    of [t], or returns [Null] if the stack is empty. *)
val pop_nonblocking : 'a t @ local -> 'a or_null @ contended portable

(** [drain t] removes and returns all items from [t] without blocking, returning a list
    with the most-recently-enqueued item first. *)
val drain : 'a t @ local -> 'a list @ contended portable

module For_testing : sig
  (** [length t] returns an upper bound on the length of the internal queue of awaiters
      for testing purposes. *)
  val length : 'a t @ local -> int
end
