@@ portable

open! Base

(** A lock-free multi-producer, single-consumer queue of [once unique] elements. *)
type 'a t : value mod portable

(** [create ()] creates a new empty queue.

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation. *)
val create : ?padded:bool @ local -> unit -> 'a t

(** [is_empty t] determines whether the queue [t] is empty. *)
val is_empty : 'a t @ local -> bool

(** [dequeue_or_null t] atomically removes an element [x] from the front of the queue [t]
    and returns [This x] or does nothing and returns [Null] in case the queue is empty. *)
val dequeue_or_null : 'a t @ local -> 'a or_null @ contended once portable unique

(** [dequeue_all t] atomically removes all elements from the queue [t] and returns them as
    a list in front to back order. *)
val dequeue_all : 'a t @ local -> 'a list @ contended once portable unique

(** [enqueue t x] atomically adds [x] to the back of the queue [t]. *)
val enqueue : 'a t @ contended local -> 'a @ contended once portable unique -> unit
