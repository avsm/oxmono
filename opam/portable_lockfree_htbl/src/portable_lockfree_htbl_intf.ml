open Base

module type Hashable = sig @@ portable
  (** Signature for the keys of a portable hashtbl. *)

  (** The type of hashable keys. *)
  type t

  (** The equality predicate used to compare keys. *)
  val equal : t @ contended -> t @ contended -> bool

  (** A hashing function on keys. It must be such that when two keys compare as equal,
      then they have identical hash values. *)
  val hash : t @ contended -> int
end

(** Alias for the first-class [Hashable] module type. *)
type 'k hashable = (module Hashable with type t = 'k)

module type S_basic = sig @@ portable
  (** Represents a lock-free hash table mapping keys of type ['k] to values of type ['v]. *)
  type (!'k, !'v) t : value mod contended portable

  (** [create (module Key)] creates a new empty lock-free hash table.

      - The default [min_buckets] is unspecified and a given [min_buckets] may be adjusted
        by the implementation.
      - The default [max_buckets] is unspecified and a given [max_buckets] may be adjusted
        by the implementation.
      - The default [shrinking_allowed] is true. If set to false, the number of buckets
        will never decrease.

      The initial capacity of the internal bucket table is determined by [min_buckets] and
      naturally the internal bucket table will not grow beyond given [max_buckets]. If you
      know the number of bindings a priori, you can set [min_buckets] and [max_buckets] to
      the same value and prevent resizing. *)
  val create
    :  ?min_buckets:int
    -> ?max_buckets:int
    -> ?shrinking_allowed:bool
    -> 'k hashable
    -> ('k, 'v) t

  (** [find t key] tries to find a binding of [key] from the hash table [t]. Returns
      [This current] in case the hash table contained a binding of [key] to [current] and
      [Null] if there was no binding of [key]. *)
  val find : ('k, 'v) t @ local -> 'k @ contended -> 'v or_null @ contended portable

  (** [add t ~key ~data] tries to add a new binding of [key] to [data] to the hash table
      [t]. Returns [Null] on success and [This current] in case the hash table already
      contained a binding of [key] to [current] in which case the [t] was not modified. *)
  val add
    :  ('k, 'v) t @ local
    -> key:'k @ contended portable
    -> data:'v @ contended portable
    -> 'v or_null @ contended portable

  (** [set t ~key ~data] either {{!add} adds} a new binding of [key] to [data] or
      atomically {{!exchange} exchanges} the value of an existing binding. Returns [Null]
      in case a new binding was added and [This current] in case a previous binding of
      [key] to [current] was replaced. *)
  val set
    :  ('k, 'v) t @ local
    -> key:'k @ contended portable
    -> data:'v @ contended portable
    -> 'v or_null @ contended portable

  (** [compare_exchange t key ~if_phys_equal_to:expected ~replace_with:data] tries to
      update an existing binding of [key] from the [expected] value to the [data] value in
      the hash table [t]. Returns [Null] in case [t] contained no binding of [key] and
      [This current] in case [t] contained a binding of [key] to [current] and updated [t]
      in case [current] and [expected] are physically equal. *)
  val compare_exchange
    :  ('k, 'v) t @ local
    -> 'k @ contended portable
    -> if_phys_equal_to:'v @ contended portable
    -> replace_with:'v @ contended portable
    -> 'v or_null @ contended portable

  (** [exchange t ~key ~data] tries to update an existing binding of [key] from some
      [current] value to the [data] value in the hash table [t]. Returns [This current] in
      case the [t] contained a binding of [key] to [current] or [Null] in case there was
      no binding of [key] in which case the [t] was not modified. *)
  val exchange
    :  ('k, 'v) t @ local
    -> key:'k @ contended portable
    -> data:'v @ contended portable
    -> 'v or_null @ contended portable

  (** [compare_remove t key ~if_phys_equal_to:expected] tries to remove an existing
      binding of [key] to the [expected] value from the hash table [t]. Returns [Null] in
      case [t] contained no binding of [key] and [This current] in case the [t] contained
      a binding of [key] to [current] and updated [t] in case [current] and [expected] are
      physically equal. *)
  val compare_remove
    :  ('k, 'v) t @ local
    -> 'k @ contended portable
    -> if_phys_equal_to:'v @ contended portable
    -> 'v or_null @ contended portable

  (** [remove t key] tries to remove a binding of [key] from the hash table [t]. Returns
      [This current] in case the hash table contained a binding of [key] to [current] and
      [Null] in case there was no binding of [key]. *)
  val remove : ('k, 'v) t @ local -> 'k @ contended -> 'v or_null @ contended portable

  (** [to_alist t] takes a snapshot of the bindings in the hash table [t] and returns them
      as an association list.

      This is a linear time operation. *)
  val to_alist : ('k, 'v) t @ local -> ('k * 'v) List.t @ contended portable

  (** [remove_all_as_alist t] takes a snapshot of the bindings in the hash table [t],
      removes the bindings from the hash table, and returns the snapshot as an association
      list.

      This is a linear time operation. *)
  val remove_all_as_alist : ('k, 'v) t @ local -> ('k * 'v) List.t @ contended portable
end

module type S = sig @@ portable
  (** Lock-free hash table designed for use in concurrent and parallel algorithms.

      The signatures of operations provided by this hash table are intentionally different
      from what you would see in a typical sequential hash table. In a concurrent setting
      it is often expected that multiple threads may concurrently access the table with
      the same key and it is crucial for the correctness of such concurrent algorithms to
      take this into account. This is one of the reasons why operations return a value
      indicating the previous or current state of the accessed binding and, in many cases,
      do not modify the table in case the state was different from what the operation
      expected. In most cases the caller needs to examine the result and act accordingly.
      More technically, the operation signatures and semantics are designed to allow
      building
      {{:https://dl.acm.org/doi/10.1145/62546.62593} consensus protocols over arbitrary
        numbers of processes}.

      Single key reads with this hash table are actually wait-free rather than just
      lock-free. Internal resizing automatically uses all the threads that are trying to
      write to the hash table. *)

  include S_basic (** @inline *)

  (** [update t key ~pure_f] calls [pure_f] with the result of [find t key] and then tries
      to to atomically either {!add}, {!compare_exchange}, or {!compare_remove} the
      binding of [key] depending on the return value of [pure_f] and the result of [find].
      The update is retried until it succeeds and [pure_f] may be called multiple times.
      Returns the last result of [find t key] before a successful update. *)
  val update
    :  ('k, 'v) t @ local
    -> 'k @ contended portable
    -> pure_f:('v or_null @ contended portable -> 'v or_null @ contended portable) @ local
    -> 'v or_null @ contended portable

  (** [hashable_of t] returns a copy of the hashable used when the hash table [t] was
      created. *)
  val hashable_of : ('k, 'v) t -> (module Hashable with type t = 'k)

  (** [min_buckets_of t] returns the minimum number of buckets of the hash table [t].

      The returned value may not be the same as given to {!create}. *)
  val min_buckets_of : ('k, 'v) t @ local -> int

  (** [max_buckets_of t] returns the maximum number of buckets of the hash table [t].

      The returned value may not be the same as given to {!create}. *)
  val max_buckets_of : ('k, 'v) t @ local -> int

  (** [estimate_current_num_buckets_of t] is a non-linearizable estimate of the current
      number of buckets of the hash table [t].

      If called during a resize, this may under- or over-count buckets. *)
  val estimate_current_num_buckets_of : ('k, 'v) t @ local -> int

  (** [copy t] creates an independent snapshot copy of the hash table [t].

      This is a linear time operation. *)
  val copy : ('k, 'v) t @ local -> ('k, 'v) t

  (** [remove_all_as_htbl t] creates an independent snapshot copy of the hash table [t],
      removes all of bindings from [t], and returns the copy. *)
  val remove_all_as_htbl : ('k, 'v) t @ local -> ('k, 'v) t

  (** [random_key t] tries to find a random binding from the hash table [t] and returns
      the key of the binding or [Null] in case the hash table is empty.

      This is an expected constant time operation with worst case linear time complexity. *)
  val random_key : ('k, 'v) t @ local -> 'k or_null @ contended portable

  (** [non_linearizable_length t] reads the current length of the hash table in a
      non-linearizable manner. During concurrent updates the result will not be precise
      and may even be negative. *)
  val non_linearizable_length : ('k, 'v) t @ local -> int
end

module type Portable_lockfree_htbl = sig
  module type Hashable = Hashable

  type nonrec 'k hashable = 'k hashable

  module type S_basic = S_basic

  include S (** @inline *)
end
