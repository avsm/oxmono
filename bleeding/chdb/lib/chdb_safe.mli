[@@@alert "-deprecated"]

(** Domain-safe connection with capsule-based access control.

    Wraps a {!Chdb_connection.t} with a
    {!Capsule_blocking_sync.Mutex} so that connection operations
    are always performed under a lock.  The lock provides a
    {!Capsule_expert.Password.t} that must be threaded through
    every operation -- the password has [void] layout and is
    [@ local], so it cannot escape the lock scope.

    {2 Usage}

    {[
      let (Safe.Pack conn) = Safe.create () in
      Safe.with_lock conn ~f:(fun password ->
        let s = Safe.query_string conn ~password ~format:TSV
                  "SELECT 1 + 1" in
        print_string s)
    ]}

    All query results must be consumed inside {!with_lock}.  The
    return type is ['a @ once unique] which is naturally satisfied
    by [unit].

    {2 Compile-time safety}

    Each connection gets a unique existential brand ['k] from
    unpacking {!packed}.  The password
    ['k Capsule_expert.Password.t] ties operations to a specific
    capsule, so using a password from one connection with another
    is a static type error:

    {[
      let (Pack a) = Safe.create () in
      let (Pack b) = Safe.create () in
      Safe.with_lock a ~f:(fun pw ->
        Safe.query_string b ~password:pw ...)
      (* Type error: the brands of a and b differ *)
    ]} *)

(** {2 Types} *)

type 'k t
(** A connection branded with capsule ['k]. *)

type packed = Pack : 'k t -> packed
(** Existentially packed connection.  Pattern-match to obtain a
    fresh brand: [let (Pack conn) = Safe.create () in ...]. *)

(** {2 Lifecycle} *)

val create : ?path:string -> unit -> packed
(** [create ?path ()] opens a new chdb connection wrapped in a
    fresh capsule.

    @param path Filesystem path for persistent storage.
                When omitted the database is in-memory only. *)

val close : 'k t -> password:'k Capsule_expert.Password.t @ local -> unit
(** [close t ~password] closes the underlying connection.
    Idempotent. *)

(** {2 Locking} *)

val mutex : 'k t -> 'k Capsule_blocking_sync.Mutex.t
(** [mutex t] returns the raw capsule mutex.  Prefer
    {!with_lock} for typical usage. *)

val with_lock :
  'k t ->
  f:('k Capsule_expert.Password.t @ local -> 'a @ once unique) @ local once ->
  'a @ once unique
(** [with_lock t ~f] acquires the capsule mutex, provides a
    branded password to [f], and releases the lock when [f]
    returns or raises.

    The password cannot be used with a different connection due
    to the ['k] brand, and cannot escape scope due to its [void]
    layout and [@ local] mode. *)

(** {2 Query execution}

    Each function requires a password obtained from {!with_lock},
    proving the caller holds the connection lock.  Results are
    typically consumed inside the lock scope. *)

val query_string :
  'k t -> password:'k Capsule_expert.Password.t @ local ->
  ?format:Chdb_format.t -> string -> string
(** [query_string t ~password ?format sql] executes [sql] and
    returns the result as a string. *)

val execute_ignore :
  'k t -> password:'k Capsule_expert.Password.t @ local ->
  ?format:Chdb_format.t -> string -> unit
(** [execute_ignore t ~password ?format sql] executes [sql] and
    discards the result.  Suitable for DDL statements. *)
