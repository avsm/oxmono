@@ portable

open Await_kernel

(** An atom is intended to be an easy-to-use waitable atomic location for storing state
    represented as an immutable data structure.

    The key feature of atom over [Atomic] and [Awaitable] is that {!update} operations use
    an approach to make starvation unlikely. No matter how expensive the function given to
    {!update} is, the update should be unlikely to starve. The starvation avoiding
    approach adds a bit of overhead over [Atomic] and [Awaitable], but should make [Atom]
    easier to use. *)

type !'a t : value mod contended portable

(** [make value] creates an atom with the given initial [value].

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation. *)
val make : ?padded:bool @ local -> 'a @ contended portable -> 'a t

(** [get t] gets the the current value of [t].

    This is a wait-free operation and returns a value even after the atom has become
    poisoned. *)
val get : 'a t @ local -> 'a @ contended portable

(** [poison t] poisons the atom.

    If a [pure_f] given to {!update} raises an exception, it will be reraised by at most
    one of the threads trying to concurrently {!poison}, {!update}, or {!wait} on the
    atom, and the atom will be poisoned.

    In other words, {!poison} only raises in case a concurrent {!update} raised. The idea
    is that the atom implementation makes sure that at least one exception raised by a
    [pure_f] given to a concurrent {!update} will be reraised. *)
val poison : _ t -> unit

(** [update t ~pure_f] atomically updates [t] to be the result of [pure_f (get t)].
    [pure_f] may be called multiple times, so should be free of side effects.

    While this is technically not a wait-free operation, starvation should be unlikely.

    If a [pure_f] given to {!update} raises an exception, it will be reraised by at most
    one of the threads trying to concurrently {!poison}, {!update}, or {!wait} on the
    atom, and the atom will be poisoned.

    @raise Poisoned if the atom was previously poisoned. *)
val update
  :  'a t @ local
  -> pure_f:('a @ contended portable -> 'a @ contended portable) @ portable
  -> unit

(** [get_and_update t ~pure_f] is like [update t ~pure_f], but also returns the old value
    of the atom. *)
val get_and_update
  :  'a t @ local
  -> pure_f:('a @ contended portable -> 'a @ contended portable) @ portable
  -> 'a @ contended portable

(** [wait w t ~until_phys_unequal_to] waits until the current value of [t] is physically
    unequal to given value.

    It is not guaranteed that [wait] returns after every update. In other words, [wait]
    may miss updates and only returns after a physically unequal value is witnessed.

    If a [pure_f] given to {!update} raises an exception, it will be reraised by at most
    one of the threads trying to concurrently {!poison}, {!update}, or {!wait} on the
    atom, and the atom will be poisoned.

    @raise Poisoned if the atom was previously poisoned. *)
val wait
  :  Await.t @ local
  -> 'a t @ local
  -> until_phys_unequal_to:'a @ contended portable
  -> unit

(** [wait_and_return w t ~until_phys_unequal_to] is like
    [wait w t ~until_phys_unequal_to], but also returns the current value. *)
val wait_and_return
  :  Await.t @ local
  -> 'a t @ local
  -> until_phys_unequal_to:'a @ contended portable
  -> 'a @ contended portable

(** [wait_for w t ~f] repeatedly {{!wait} waits} until the current value of the atom
    passes the predicate [f].

    Use the more primitive {!wait} when you simply wait for the current value to change. *)
val wait_for
  :  Await.t @ local
  -> 'a t @ local
  -> f:('a @ contended portable -> bool) @ local
  -> unit

(** [wait_for_and_return w t ~f] is like [wait_for w t ~f], but also returns the current
    value. *)
val wait_for_and_return
  :  Await.t @ local
  -> 'a t @ local
  -> f:('a @ contended portable -> bool) @ local
  -> 'a @ contended portable
