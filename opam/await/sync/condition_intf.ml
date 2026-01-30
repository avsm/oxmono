open Await_kernel

module type Condition = sig
  (** Condition variable for waiting for changes to state protected by a lock. *)

  type 'k lock

  (** ['k t] is the type of a condition variable associated with the capsule ['k]. This
      condition may only be used with the matching ['k lock]. *)
  type 'k t : value mod contended portable

  (** [create ()] creates a new condition variable associated with the matching ['k lock]
      and with a certain property {i P} that is protected by the lock.

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val create : ?padded:bool @ local -> unit -> 'k t

  (** [wait w t ~lock key] atomically releases the [lock] and blocks on the condition
      variable [t]. To ensure exception safety, it takes hold of the ['k Key.t] associated
      with the lock.

      [wait] returns with the [lock] reacquired once the condition variable [t] has been
      signaled via {!signal} or {!broadcast}. [wait] may also return for no reason -
      callers cannot assume that the property {i P} associated with the condition variable
      [c] holds when [wait] returns.

      @raise Poisoned if [t] cannot be reacquired because it is poisoned.
      @raise Frozen if [t] cannot be reacquired because it is frozen.
      @raise Terminated if [w] is terminated before the lock is reacquired. *)
  val wait
    :  Await.t @ local
    -> 'k t @ local
    -> lock:'k lock @ local
    -> 'k Capsule.Expert.Key.t @ unique
    -> 'k Capsule.Expert.Key.t @ unique

  (** [signal t] wakes up one waiter on the condition variable [t], if there is one. If
      there is none, this call has no effect. It is recommended to call [signal t] after a
      critical section - that is, after the lock associated with [t] has been acquired,
      the condition for signaling has been confirmed, and the lock is released. *)
  val signal : 'k t @ local -> unit

  (** [broadcast t] wakes up all waiters on the condition variable [t]. If there are none,
      this call has no effect. It is recommended to call [broadcast t] after a critical
      section, that is, after the lock associated with [t] has been acquired, the
      condition for broadcasting has been confirmed, and the lock released. *)
  val broadcast : 'k t @ local -> unit
end
