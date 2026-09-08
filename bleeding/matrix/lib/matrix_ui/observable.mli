(** observable — race-free observable values and lists for UI models.

    A subscription is created atomically with its initial snapshot, so no change
    can slip between the two. Producers never wait for consumers. Repeated value
    updates are coalesced, and a list subscriber that falls too far behind
    receives a {!List.Reset} carrying the current snapshot instead of the diffs
    it missed.

    A subscription lives on an [Eio.Switch.t] and is read by blocking a fiber on
    [next]. *)

module Value : sig
  (** A cell whose distinct values are published to its subscribers. *)

  type 'a t
  (** The type for observable values. *)

  type 'a subscription
  (** The type for one reader's view of the values published since it
      subscribed. *)

  val create : ?equal:('a -> 'a -> bool) -> 'a -> 'a t
  (** [create v] is a cell holding [v]. [equal] decides which updates count as
      distinct and are published, and defaults to structural equality. *)

  val get : 'a t -> 'a
  (** [get t] is the value [t] holds. *)

  val set : 'a t -> 'a -> unit
  (** [set t v] makes [v] the value [t] holds and publishes it to every
      subscriber. A value equal to the one held is not published. *)

  val subscribe : sw:Eio.Switch.t -> 'a t -> 'a * 'a subscription
  (** [subscribe ~sw t] is the current value and a subscription to later
      distinct values, as one atomic operation. The subscription is closed when
      [sw] finishes. *)

  val next : 'a subscription -> 'a option
  (** [next s] is the next value published to [s], waiting for one. It is [None]
      when the subscription has been closed. Values published while the reader
      was away are coalesced, so [next] answers the newest rather than each in
      turn. *)

  val unsubscribe : 'a subscription -> unit
  (** [unsubscribe s] closes [s], so that a fiber waiting in {!next} is answered
      [None]. Idempotent. *)
end

module List : sig
  (** An ordered sequence whose changes are published as the diffs a toolkit's
      list model takes. *)

  (** The type for one change to the list. Indices are into the list as the
      preceding diffs of the same batch left it. *)
  type 'a diff =
    | Insert of { index : int; value : 'a }
    | Remove of { index : int }
    | Set of { index : int; value : 'a }
    | Move of { from : int; to_ : int }
    | Truncate of { length : int }
    | Reset of 'a array
        (** The list as a whole, sent to a subscriber that fell too far behind
            for its pending diffs to be worth keeping. *)

  type 'a t
  (** The type for observable lists. *)

  type 'a subscription
  (** The type for one reader's view of the diffs published since it subscribed.
  *)

  val create : ?max_pending_diffs:int -> 'a list -> 'a t
  (** [create values] is a list holding [values]. [max_pending_diffs] is how
      many diffs may queue for one subscriber before the next batch collapses to
      a {!Reset}, 256 by default.

      Raises [Invalid_argument] if [max_pending_diffs] is not positive. *)

  val snapshot : 'a t -> 'a array
  (** [snapshot t] is the list as it stands. *)

  val length : 'a t -> int
  (** [length t] is how many values [t] holds. *)

  val get : 'a t -> int -> 'a
  (** [get t index] is the value at [index].

      Raises [Invalid_argument] if [index] is out of range. *)

  val subscribe : sw:Eio.Switch.t -> 'a t -> 'a array * 'a subscription
  (** [subscribe ~sw t] is a snapshot and a subscription to the batches of diffs
      that follow it, as one atomic operation. The subscription is closed when
      [sw] finishes. *)

  val next : 'a subscription -> 'a diff list option
  (** [next s] is the next batch of diffs published to [s], waiting for one. It
      is [None] when the subscription has been closed. A batch is never empty,
      and folding it through {!apply_all} brings a copy of the list up to date.
  *)

  val unsubscribe : 'a subscription -> unit
  (** [unsubscribe s] closes [s], so that a fiber waiting in {!next} is answered
      [None]. Idempotent. *)

  val insert : 'a t -> index:int -> 'a -> unit
  (** [insert t ~index value] puts [value] at [index] and moves what stood there
      and after it along.

      Raises [Invalid_argument] if [index] is negative or beyond the length. *)

  val append : 'a t -> 'a -> unit
  (** [append t value] puts [value] after the last one. *)

  val remove : 'a t -> index:int -> unit
  (** [remove t ~index] drops the value at [index].

      Raises [Invalid_argument] if [index] is out of range. *)

  val set : 'a t -> index:int -> 'a -> unit
  (** [set t ~index value] replaces the value at [index].

      Raises [Invalid_argument] if [index] is out of range. *)

  val reconcile_by :
    ?key_equal:('key -> 'key -> bool) ->
    key:('a -> 'key) ->
    equal:('a -> 'a -> bool) ->
    'a t ->
    'a list ->
    unit
  (** [reconcile_by ~key ~equal t values] brings [t] to hold [values],
      publishing granular insert, move, set and truncate diffs where practical;
      a sufficiently large change may publish one [Reset] to bound
      reconciliation work. [key] identifies a value across the change, so that
      one which merely moved is moved rather than replaced, and [equal] decides
      whether a value under an unchanged key needs republishing. [key_equal]
      compares keys, and defaults to structural equality.

      Keys need not be unique. Duplicates cost extra diffs, never correctness.
  *)

  val apply : 'a array -> 'a diff -> 'a array
  (** [apply values diff] is [values] with [diff] applied. A consumer that keeps
      a copy of the list folds each batch through this.

      Raises [Invalid_argument] if the diff's indices are out of range for
      [values]. *)

  val apply_all : 'a array -> 'a diff list -> 'a array
  (** [apply_all values diffs] folds {!apply} over [diffs], left to right. *)
end
