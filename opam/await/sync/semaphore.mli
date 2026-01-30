@@ portable

(** A poisonable counting semaphore. *)

open Await_kernel

module Acquired_or_would_block : sig
  (** The return value of {!try_acquire} *)
  type t =
    | Acquired
    | Would_block
  [@@deriving sexp_of]
end

(** A poisonable counting semaphore. *)

(** Represents a poisonable counting semaphore. *)
type t : value mod contended portable [@@deriving sexp_of ~stackify]

(** Maximum counter value allowed by the semaphore implementation. *)
val max_value : int

(** [create n] creates a new counting semaphore with the given count [n].

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation.

    @raise Invalid_argument
      in case the given count is negative or higher than {!max_value}. *)
val create : ?padded:bool @ local -> int -> t

(** [release t] increments the count of the semaphore or does nothing in case the
    semaphore has been {{!poison} poisoned}.

    @raise Sys_error in case the count would overflow. *)
val release : t @ local -> unit

(** [acquire w t] waits until the count of the semaphore is greater than [0] and then
    atomically decrements the count.

    @raise Poisoned in case the semaphore has been {{!poison} poisoned}. *)
val acquire : Await.t @ local -> t @ local -> unit

(** [acquire_or_cancel w c t] is [Completed (acquire w t)] if [c] is not canceled,
    otherwise it is [Canceled]. *)
val acquire_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> t @ local
  -> unit Or_canceled.t

(** [try_acquire t] attempts to atomically and in a wait-free way decrement the count of
    the semaphore, unless the count is already [0]. Returns {!Acquired} if the semaphore
    was successfully acquired, or {!Would_block} if the count is already [0] and the
    operation would block

    @raise Poisoned in case the semaphore has been {{!poison} poisoned}. *)
val try_acquire : t @ local -> Acquired_or_would_block.t

(** [get_value t] returns the current count of the semaphore or [0] in case the semaphore
    has been {{!poison} poisoned}.

    This should only be used for debugging or informational messages. *)
val get_value : t @ local -> int

(** [poison t] marks the semaphore as poisoned. *)
val poison : t @ local -> unit

(** [is_poisoned t] determines whether the semaphore has been {{!poison} poisoned}. *)
val is_poisoned : t @ local -> bool
