(** Persistent local job queues.

    {1 NAME}
    Duraml_sqlite - multi-process SQLite backend for Duraml.

    {1 SYNOPSIS}
    {[
      let store = Duraml_sqlite.open_ ~sw ~clock:env#clock
        ~random:env#secure_random Eio.Path.(env#fs / "queue.db") in
      Duraml.enqueue store spec
    ]}

    {1 DESCRIPTION}
    Indexed tables store jobs, attempts, events and store metadata. Mutations
    use short [BEGIN IMMEDIATE] transactions through [sqlite3-eio]. Time is
    sampled after acquiring the write lock. An idle claim poll reads in a
    deferred transaction without the write lock. The shared Duraml engine runs
    inside the transaction. Application handlers run outside it.

    One handle serializes access between fibers in one Eio domain. Independent
    handles and processes coordinate through SQLite on the same host. Changes
    made by other handles are discovered by bounded polling. Lease renewal
    changes are also observed within the polling interval.

    {1 STORAGE}
    Use a local filesystem, not NFS or Ceph. New database files have mode 0600.
    A writable open enables WAL and FULL synchronization, and fails if WAL is
    unavailable. Store identity survives reopening. Schema version 4 stores
    timestamps as fixed-width UTC text with picosecond precision in both
    records and the dispatch indexes. There is no conversion through
    floating point when persisting or ordering timestamps.

    A writable open upgrades version 2 and 3 stores in place. Unrelated
    databases, version 1 experimental stores and unknown versions are
    refused. No retention is performed. Backups must
    include committed WAL state, using SQLite backup facilities or coordinated
    checkpointing. Copying only the database file while live is insufficient.

    {1 RECOVERY}
    Cancelling an operation attempts interruption and rolls back unfinished
    transactions. A commit may nevertheless precede cancellation of its
    response. A transaction that cannot be rolled back closes the handle
    rather than hold the write lock. Reconcile through keyed enqueues or identical settlements. Dead processes
    leave running jobs recoverable by lease expiry. SQLite durability does not
    make external handler effects exactly once. *)

val open_ : sw:Eio.Switch.t -> clock:_ Eio.Time.clock ->
  random:_ Eio.Flow.source -> ?poll_interval:Ptime.Span.t ->
  ?read_only:bool -> _ Eio.Path.t -> Duraml.plain
(** [open_ ~sw ~clock ~random path] creates or opens a Duraml database owned by
    [sw]. [path] must have a native filesystem path. [clock] supplies Unix time.
    All cooperating processes must use consistent wall clocks. Supply
    [env#secure_random] for production identities and claim nonces.

    [read_only] (default [false]) opens an existing, initialized database
    with SQLite's read-only mode for inspection. Identity, lookup, listing
    and event reads work normally. Enqueue, claim, renewal, settlement and
    cancellation raise [Duraml.E (Storage _)]. Workers must not use such a
    handle. The database's WAL and shared-memory files must remain
    accessible to the reader. An uninitialized file is refused.

    [poll_interval] must be positive and defaults to 100 milliseconds. It bounds
    idle polling sleep, not scheduling latency under load or lock contention.
    The SQLite busy timeout is five seconds. Opening and operations may suspend.
    Closing [sw] closes the connection, as does a failed open. Invalid
    intervals raise [Invalid_argument]. Schema, storage and damaged-record
    failures raise [Duraml.E] I/O errors. Filesystem and SQLite opening
    errors may retain their provider error type. *)
