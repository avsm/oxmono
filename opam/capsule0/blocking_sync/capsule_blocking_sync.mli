@@ portable

[@@@alert deprecated "Use synchronization primitives from [Await]."]

module Capsule := Capsule_expert

(** Blocking versions of synchronization primitives

    The synchronization primitives in this module are heavyweight and slow, and should
    generally not be depended on for new code. Instead, new code should use
    synchronization primitives from the [Await] library, using an appropriate
    implementation of awaiting.

    These are defined here only to provide some form of synchronization for libraries
    which cannot depend on [Await] *)

(** If threads are available, yields the current thread. Does nothing in bytecode. *)
val yield : unit -> unit

module Mutex : sig
  (** Private implementation type exposed to allow packing mutexes into [@@unboxed]
      records/variants. *)
  type mutex : value mod contended portable

  (** ['k t] is the type of the mutex that controls access to the capsule ['k]. A mutex
      can be created from a ['k Key @ unique]. *)
  type 'k t : value mod contended portable = private mutex

  (** [packed] is the type of a mutex for some unknown capsule. Unpacking one provides a
      ['k Mutex.t] together with a fresh existential type brand for ['k]. *)
  type packed : value mod contended portable = P : 'k t -> packed [@@unboxed]

  (** [create k] creates a new mutex for the capsule ['k], consuming its key. *)
  val create : 'k Capsule.Key.t @ unique -> 'k t

  (** Raising an uncaught exception while holding the lock poisons the mutex. All
      operations on a poisoned mutex raise the [Poisoned] exception. *)
  exception Poisoned

  (** [with_lock m ~f] tries to acquire the mutex [m]. If [m] is already locked, blocks
      the current thread until it's unlocked. If successful, provides [f] a password for
      the capsule ['k] associated with [m].

      If [f] raises an exception, the mutex is marked as poisoned and the exception is
      reraised.

      If [m] is already locked by the current thread, raises [Sys_error]. *)
  val with_lock
    : ('a : value_or_null) 'k.
    'k t
    -> f:('k Capsule.Password.t @ local -> 'a @ once unique) @ local once
    -> 'a @ once unique

  (** [with_key m ~f] tries to acquire the mutex [m]. If [m] is already locked, blocks the
      current thread until it's unlocked. If successful, provides [f] the key for the
      capsule ['k] associated with [m].

      If [f] raises an exception, the mutex is marked as poisoned and the exception is
      reraised.

      If [m] is already locked by the current thread, raises [Sys_error]. *)
  val with_key
    : ('a : value_or_null) 'k.
    'k t
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
       @ local once
    -> 'a @ once unique

  (** [destroy m] acquires the mutex [m] and returns the key representing ownership of
      ['k]. The mutex is marked as poisoned. *)
  val destroy : 'k t -> 'k Capsule.Key.t @ unique
end

module Condition : sig
  (** ['k t] is the type of a condition variable associated with the capsule ['k]. This
      condition may only be used with the matching ['k Mutex.t]. *)
  type 'k t : value mod contended portable

  (** [create ()] creates a new condition variable associated with the matching
      ['k Mutex.t] and with a certain property {i P} that is protected by the mutex. *)
  val create : unit -> 'k t

  (** [wait t ~mutex key] atomically unlocks the [mutex] and blocks the current thread on
      the condition variable [t]. To ensure exception safety, it takes hold of the
      ['k Key.t] associated with the mutex, provided by [Mutex.with_key].

      This thread will be woken up when the condition variable [t] has been signaled via
      {!signal} or {!broadcast}. [mutex] is locked again before [wait] returns.

      However, this thread can also be woken up for no reason. One cannot assume that the
      property {i P} associated with the condition variable [c] holds when [wait] returns;
      one must explicitly test whether {i P} holds after calling [wait].

      If called on an already poisoned [mutex], raises [Mutex.Poisoned]. *)
  val wait
    :  'k t
    -> mutex:'k Mutex.t
    -> 'k Capsule.Key.t @ unique
    -> 'k Capsule.Key.t @ unique

  (** [signal t] wakes up one of the threads waiting on the condition variable [t], if
      there is one. If there is none, this call has no effect. It is recommended to call
      [signal t] inside a critical section, that is, while the mutex associated with [t]
      is locked. *)
  val signal : 'k t -> unit

  (** [broadcast t] wakes up all threads waiting on the condition variable [t]. If there
      are none, this call has no effect. It is recommended to call [broadcast t] inside a
      critical section, that is, while the mutex associated with [t] is locked. *)
  val broadcast : 'k t -> unit
end
