@@ portable

(** A poisonable mutual exclusion lock. *)

open Await_kernel
module Capsule := Capsule.Expert

(** {1 Creating a mutex} *)

(** [k t] is the type of a mutex protecting the contents of the [k] capsule. *)
type 'k t : value mod contended portable

(** [create k] creates a new mutex for the capsule ['k] associated with [k], consuming the
    key itself.

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation. *)
val create : ?padded:bool @ local -> 'k Capsule.Key.t @ unique -> 'k t

(** {1 Executing critical sections} *)

(** {2 With uncontended [Access]} *)

(** [with_access w t ~f] acquires [t], runs [f] within the associated capsule, then
    releases [t]. If [t] is locked then uses [w] to wait until it is unlocked.

    @raise Poisoned if [t] cannot be acquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is acquired. *)
val with_access
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Access.t -> 'a @ contended once portable unique) @ local once portable
  -> 'a @ contended once portable unique

(** [with_access_poisoning w t ~f] acquires [t], runs [f] within the associated capsule,
    then releases [t]. If [t] is locked then uses [w] to wait until it is unlocked.

    Poisons [t] if [f] raises an uncaught exception.

    @raise Poisoned if [t] cannot be acquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is acquired. *)
val with_access_poisoning
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Access.t -> 'a @ contended portable unique) @ local once portable
  -> 'a @ contended once portable unique

(** [with_access_or_cancel w c t f] is [Completed (with_access w t f)] if [c] is not
    canceled, otherwise it is [Canceled].

    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val with_access_or_cancel
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Access.t -> 'a @ contended portable unique) @ local once portable
  -> 'a Or_canceled.t @ contended once portable unique

(** [with_access_or_cancel_poisoning w c t f] is [Completed (with_access_poisoning w t f)]
    if [c] is not canceled, otherwise it is [Canceled].

    Poisons [t] if [f] raises an uncaught exception.

    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val with_access_or_cancel_poisoning
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Access.t -> 'a @ contended portable unique) @ local once portable
  -> 'a Or_canceled.t @ contended once portable unique

(** {2 With a local [Password]} *)

(** [with_password w t f] acquires [t], runs [f] with permission to access the associated
    capsule, then releases [t]. If [t] is locked then uses [w] to wait until it is
    unlocked.

    @raise Poisoned if [t] cannot be acquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is acquired. *)
val with_password
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
  -> 'a @ unique

(** [with_password_poisoning w t f] acquires [t], runs [f] with permission to access the
    associated capsule, then releases [t]. If [t] is locked then uses [w] to wait until it
    is unlocked.

    Poisons [t] if [f] raises an exception.

    @raise Poisoned if [t] cannot be acquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is acquired. *)
val with_password_poisoning
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
  -> 'a @ unique

(** [with_password_or_cancel w c t f] is [Completed (with_password w t f)] if [c] is not
    canceled, otherwise it is [Canceled].

    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val with_password_or_cancel
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
  -> 'a Or_canceled.t @ unique

(** [with_password_or_cancel_poisoning w c t f] is
    [Completed (with_password_poisoning w t f)] if [c] is not canceled, otherwise it is
    [Canceled].

    Poisons [t] if [f] raises an exception.

    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val with_password_or_cancel_poisoning
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
  -> 'a Or_canceled.t @ unique

(** {2 With a unique [Key]} *)

[%%template:
[@@@alloc.default a @ l = (heap_global, stack_local)]

(** [with_key t ~f] locks [t] and runs [f], providing it a key for ['k] uniquely. If [t]
    is locked, then [with_key] uses [w] to wait until it is unlocked.

    @raise Poisoned if [t] cannot be acquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is acquired. *)
val with_key
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
     @ local once
  -> 'a @ l once unique

(** [with_key_poisoning t ~f] locks [t] and runs [f], providing it a key for ['k]
    uniquely. If the function raises without returning the key back, the mutex is
    poisoned. If [t] is locked, then [with_key_poisoning] uses [w] to wait until it is
    unlocked.

    @raise Poisoned if [t] cannot be acquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is acquired. *)
val with_key_poisoning
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
     @ local once
  -> 'a @ l once unique

(** [with_key_or_cancel w c t ~f] is [Completed (with_key w t ~f)] if [c] is not canceled,
    otherwise it is [Canceled].

    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val with_key_or_cancel
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
     @ local once
  -> 'a Or_canceled.t @ l once unique

(** [with_key_or_cancel_poisoning w c t ~f] is [Completed (with_key_poisoning w t ~f)] if
    [c] is not canceled, otherwise it is [Canceled].

    @raise Terminated if [w] is terminated, even if [c] is canceled. *)
val with_key_or_cancel_poisoning
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
     @ local once
  -> 'a Or_canceled.t @ l once unique]

(** {3 And waiting for a [Condition]} *)

module Condition : Condition_intf.Condition with type 'k lock := 'k t (** @open *)

(** {3 And releasing temporarily} *)

(** [release_temporarily w t k ~f] releases [t], runs [f] without the mutex, reacquires
    [t] and returns the result of running [f] along with the [k].

    To obtain a key to pass to [release_temporarily], use {!with_key} or similar
    functions.

    @raise Poisoned if [t] cannot be reacquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is reacquired. *)
val release_temporarily
  : ('a : value_or_null) 'k.
  Await.t @ local
  -> 'k t @ local
  -> 'k Capsule.Key.t @ unique
  -> f:(unit -> 'a @ unique) @ local once
  -> #('a * 'k Capsule.Key.t) @ unique

(** [release_temporarily_or_cancel w c t k ~f] is
    [Completed (release_temporarily w t k ~f)] if [c] is not canceled, otherwise it is
    [Canceled]. *)
val release_temporarily_or_cancel
  : ('a : value_or_null).
  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> 'k Capsule.Key.t @ unique
  -> f:(unit -> 'a @ unique) @ local once
  -> (#('a * 'k Capsule.Key.t) Or_canceled.t[@kind value_or_null & void]) @ unique

(** {3 And poisoning indefinitely} *)

(** [poison t key] poisons the mutex associated with the [key].

    Note that poisoning a mutex does not signal waiters on associated {{!Condition}
    condition variables}. *)
val poison : 'k t @ local -> 'k Capsule.Key.t @ unique -> 'k Capsule.Key.t @ unique

(** {2 Indefinitely through poisoning} *)

(** [acquire_and_poison w t] acquires [t] and then immediately poisons it, returning the
    key to the protected capsule.

    @raise Poisoned if [t] cannot be acquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is acquired. *)
val acquire_and_poison : Await.t @ local -> 'k t @ local -> 'k Capsule.Key.t @ unique

(** [acquire_and_poison_or_cancel w c t] is [Completed (acquire_and_poison w t)] if [c] is
    not canceled, otherwise it is [Canceled]. *)
val acquire_and_poison_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> ('k Capsule.Key.t Or_canceled.t[@kind void]) @ unique

(** {2 With arbitrary dynamic scope} *)

module Guard : sig
  (** ['k Mutex.Guard.t] represents a locked mutex. It morally contains a
      {!Capsule.Key.t}, but also has a finalizer that poisons the mutex if it is garbage
      collected without {!release} being called. *)
  type 'k t : value mod contended many portable

  (** [with_key t ~f] runs [f], providing it a key for ['k] uniquely. If the function
      raises without returning the key back, the mutex is poisoned. *)
  val with_key
    : ('a : value_or_null) 'k.
    'k t @ unique
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
       @ local once
    -> 'a * 'k t @ once unique

  (** [with_password g ~f] runs [f], providing it a password for ['k], and returns the
      result of [f] together with the guard.

      If [f] raises an exception, the mutex is poisoned, and the exception is reraised. *)
  val with_password
    :  'k t @ unique
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a * 'k t @ unique

  (** [access g ~f] runs [f], providing it access to the capsule ['k], and returns the
      result of [f] together with the guard.

      If [f] raises an exception, the mutex is poisoned, and the exception is reraised. *)
  val access
    : ('a : value_or_null) 'k.
    'k t @ unique
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a * 'k t @ contended once portable unique

  (** [release guard] releases the mutex protected by [guard], consuming the guard. *)
  val release : 'k t @ unique -> unit

  (** [poison guard] poisons the mutex protected by [guard], consuming the guard and
      returning the key to the protected capsule.

      Note that poisoning a mutex does not signal waiters on associated {{!Condition}
      condition variables}. *)
  val poison : 'k t @ unique -> 'k Capsule.Key.t @ unique

  (** [is_poisoning guard] is [true] if [guard] will poison if its finalizer runs without
      [release] being called. *)
  val is_poisoning : 'k t @ local -> bool
end

(** [acquire w t] acquires [t] and returns a guard for the locked mutex. If [t] is locked,
    then acquire uses [w] to wait until it is unlocked.

    @raise Poisoned if [t] cannot be acquired because it is poisoned.
    @raise Terminated if [w] is terminated before the mutex is acquired. *)
val acquire : Await.t @ local -> 'k t -> 'k Guard.t @ unique

(** [acquire_or_cancel w c t] is [Completed (acquire w t)] if [c] is not canceled,
    otherwise it is [Canceled]. *)
val acquire_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t
  -> 'k Guard.t Or_canceled.t @ unique

(** {1 Poisoning as a signal} *)

(** [is_poisoned t] determines whether the mutex [t] is poisoned. *)
val is_poisoned : 'k t @ local -> bool

(** [poison_unacquired t] poisons [t] without acquiring it. Does nothing if [t] is already
    poisoned.

    Note that this can render the capsule associated with [t] innaccessible, since it
    potentially destroys the associated key.

    Note that poisoning a mutex does not signal waiters on associated {{!Condition}
    condition variables}. *)
val poison_unacquired : 'k t @ local -> unit

(**/**)

module For_testing : sig
  (** [is_exclusive t] determines whether the mutex [t] is locked. *)
  val is_exclusive : 'k t @ local -> bool

  (** [length t] returns an upper bound on the length of the internal queue of awaiters
      for testing purposes. *)
  val length : 'k t @ local -> int
end
