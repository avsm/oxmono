(** A deterministic cache for downloaded media.

    The first implementation is in-memory. Its key deliberately includes the
    thumbnail variant, matching the Matrix SDK media-store model; a persistent
    backend can use the same operations without changing send-queue callers. *)

type format =
  | File
  | Thumbnail of {
      width : int;
      height : int;
      resize : [ `Crop | `Scale ] option;
    }

type key = { uri : Media.Mxc.t; format : format }

type retention_policy = {
  max_file_size : int option;
  max_total_size : int option;
  expiry : Ptime.Span.t option;
  cleanup_frequency : Ptime.Span.t option;
      (** Automatic cleanup cadence. [None] disables opportunistic cleanup; the
          default is one day. A cleanup is attempted on the next [add] or [get]
          once this span has elapsed since the last cleanup. *)
}

module type S = sig
  type t

  val retention : t -> retention_policy
  val set_retention : t -> retention_policy -> (unit, Error.t) result

  val add :
    ?ignore_retention:bool ->
    ?protected:bool ->
    ?owner:string ->
    ?now:Ptime.t ->
    t ->
    key ->
    data:string ->
    (unit, Error.t) result

  val get : now:Ptime.t -> t -> key -> (string option, Error.t) result
  val protect : t -> key -> (unit, Error.t) result
  val unprotect : t -> key -> (unit, Error.t) result
  val is_protected : t -> key -> (bool, Error.t) result
  val set_ignore_retention : t -> key -> bool -> (unit, Error.t) result
  val replace_key : t -> from_:key -> to_:key -> (unit, Error.t) result
  val remove : t -> key -> (unit, Error.t) result
  val remove_uri : t -> Media.Mxc.t -> (unit, Error.t) result

  val prune_local :
    owner:string ->
    keep:key list ->
    older_than:Ptime.t ->
    t ->
    (unit, Error.t) result

  val clean : now:Ptime.t -> t -> (unit, Error.t) result
  val last_cleanup : t -> Ptime.t option
  val set_last_cleanup : t -> Ptime.t option -> (unit, Error.t) result
  val close : t -> unit
end

type t
(** The cache wrapper. Calls made after {!close} return
    [Error.Policy_denied "media store is closed"]. *)

val v : (module S with type t = 'backend) -> 'backend -> t
(** [v (module Backend) backend] wraps a cache backend. Backend calls through
    the wrapper are serialized, so backends do not need their own locking. *)

val memory : ?retention:retention_policy -> unit -> t
(** An in-memory backend with no persistence. *)

val create : ?retention:retention_policy -> unit -> t
(** [create ?retention ()] creates an empty cache. The default follows the
    pinned matrix-rust-sdk: 20 MiB per item, 400 MiB total and 60 days since
    last access. It is a compatibility alias for {!memory}. Pass an all-[None]
    policy for an unlimited cache. *)

val retention : t -> retention_policy
(** Reads the active policy. This accessor remains available after [close]. *)

val last_cleanup : t -> Ptime.t option
(** [last_cleanup t] is the timestamp of the last completed cleanup, persisted
    by backends that support reopening. *)

val set_last_cleanup : t -> Ptime.t option -> (unit, Error.t) result
(** Backend-facing persistence seam for restoring or recording the last
    completed cleanup timestamp. *)

val set_retention : t -> retention_policy -> (unit, Error.t) result
(** Negative byte limits or expiry spans are reported as [Error.Policy_denied].
    Zero is a valid limit. *)

val local_uri : txn_id:string -> Media.Mxc.t
(** A local-only URI suitable for referring to an upload before it has a
    homeserver MXC URI. The exact transaction-id bytes are hashed into a
    deterministic, fixed-size, versioned media id, so every Matrix transaction
    identifier is accepted without changing the identifier used on the wire.
    Pre-v2 local URIs already persisted by this library remain recognizable by
    {!is_local_uri}. *)

val is_local_uri : Media.Mxc.t -> bool

val derived_key : namespace:string -> identity:string -> format -> key
(** [derived_key ~namespace ~identity format] returns a deterministic cache key
    for bytes that must not collide with ordinary URI-keyed content. *)

val add :
  ?ignore_retention:bool ->
  ?protected:bool ->
  ?owner:string ->
  ?now:Ptime.t ->
  t ->
  key ->
  data:string ->
  (unit, Error.t) result
(** Adds or replaces an entry. An entry exceeding the lower of [max_file_size]
    and [max_total_size] is not cached unless [ignore_retention] is true,
    matching matrix-rust-sdk's retention behaviour. [protected] entries are
    retained during cleanup; [ignore_retention] entries additionally ignore all
    retention limits. *)

val get : now:Ptime.t -> t -> key -> (string option, Error.t) result
(** Reads an entry and updates its last-access time. *)

val protect : t -> key -> (unit, Error.t) result
val unprotect : t -> key -> (unit, Error.t) result
val is_protected : t -> key -> (bool, Error.t) result

val set_ignore_retention : t -> key -> bool -> (unit, Error.t) result
(** Changes whether an existing entry ignores every retention limit. Missing
    entries are ignored. *)

val replace_key : t -> from_:key -> to_:key -> (unit, Error.t) result
(** Moves an entry to a new key. Missing sources are a no-op; an existing
    destination is replaced. Repeating the same move is therefore safe. *)

val remove : t -> key -> (unit, Error.t) result
val remove_uri : t -> Media.Mxc.t -> (unit, Error.t) result

val prune_local :
  owner:string ->
  keep:key list ->
  older_than:Ptime.t ->
  t ->
  (unit, Error.t) result
(** [prune_local ~owner ~keep ~older_than t] removes local upload entries owned
    by [owner] whose last access is older than [older_than], except keys in
    [keep]. This includes stale protected entries, which are crash orphans when
    absent from the restored queue's keep-set. Remote MXC entries and entries
    owned by another queue/account are never touched. This is intended for
    queue-restore orphan reconciliation; callers should retain active queue keys
    in [keep]. *)

val clean : now:Ptime.t -> t -> (unit, Error.t) result
(** Removes expired and over-size unprotected entries. Candidates are evicted
    oldest-first by last access, with insertion order as a stable tie-breaker.
    With no policy limitations this is a no-op and does not advance the cleanup
    marker. Automatic cleanup failures are logged and retried later; manual
    cleanup reports backend errors. *)

val close : t -> unit
(** Closes the cache. It is idempotent. Operations that return a result reject
    later calls with [Error.Policy_denied "media store is closed"]. *)
