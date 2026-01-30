@@ portable

open Await_kernel

(** A poisonable multi-shot synchronization barrier.

    To use a barrier, one first {!create}s a barrier by specifying the number of
    independent threads of control, or {!parties}, participating in barrier
    synchronization. The parties can then {!await} on the barrier. Calls to {!await}
    return once all of the parties are simultaneously awaiting on the barrier. The barrier
    will then reset and can be {!await}ed again.

    Barriers have a variety of use cases, many of which can perhaps be done better using
    other synchronization constructs. One plausible use case is to synchronize threads for
    the purpose of benchmarking or testing thread-safe data structures.

    Consider:
    {[
      let barrier = Barrier.create n_domains in
      run_parallel_in ~n_domains ~f:(fun aw ->
        for _ = 1 to n_rounds do
          Barrier.await aw barrier;
          use_shared_data_structure ()
        done)
    ]}
    The idea behind using the [Barrier.await] is to synchronize all the domains such that
    operations on the shared data structure would actually be maximally contended. What
    could easily happen otherwise is that different domains start running the [~f] at
    different times and there is actually no or very little contention. *)

(** Represents a poisonable barrier. *)
type t : value mod contended portable

(** Maximum number of participants that a barrier can be configured with. *)
val max_parties : int

(** [create parties] creates a new barrier for given number of parties.

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation.

    @raise Invalid_argument
      in case the given number of [parties] is less than [1] or greater than
      [max_parties]. *)
val create : ?padded:bool @ local -> int -> t

(** [parties t] returns the number of parties the barrier was {{!create} created} with. *)
val parties : t @ local -> int

(** [await w t] awaits until the configured number of {!parties} are awaiting on the
    barrier and returns. After returning normally the barrier will be reset such that
    [await] can be called on the barrier again.

    If the await is terminated, then [await] will {!poison} the barrier before raising the
    [Terminated] exception.

    @raise Poisoned in case the barrier was poisoned.
    @raise Terminated in case [w] was terminated. *)
val await : Await.t @ local -> t @ local -> unit

(** [await_or_cancel w c t] is [Completed (await w t)] if [c] is not canceled, otherwise
    it is [Canceled].

    Cancelation does not poison the barrier and the program should otherwise ensure that
    other awaiters will not get stuck.

    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val await_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> t @ local
  -> unit Or_canceled.t

(** [poison t] marks the barrier as poisoned. Concurrent and subsequent calls of {!await}
    will raise the {!Poisoned} exception. *)
val poison : t @ local -> unit
