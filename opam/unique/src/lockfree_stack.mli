@@ portable

open! Base

(** A lock-free stack of [once unique] elements. *)
type !'a t : value mod contended portable

(** [create ()] creates a new empty stack.

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation. *)
val create : ?padded:bool @ local -> unit -> 'a t

(** [is_empty t] determines whether the stack [t] is empty. *)
val is_empty : 'a t @ local -> bool

(** [pop_or_null t] atomically removes an element [x] from the top of the stack [t] and
    returns [This x] or does nothing and returns [Null] in case the stack is empty. *)
val pop_or_null : 'a t @ local -> 'a or_null @ contended once portable unique

(** [pop_all t] atomically removes all elements from the stack [t] and returns them as a
    list in top to bottom order. *)
val pop_all : 'a t @ local -> 'a list @ contended once portable unique

(** [push t x] atomically adds [x] to the top of the stack [t]. *)
val push : 'a t @ local -> 'a @ contended once portable unique -> unit
