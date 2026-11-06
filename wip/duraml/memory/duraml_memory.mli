(** Transactional in-memory job queues.

    {1 NAME}
    Duraml_memory - ephemeral implementation of the Duraml resource protocol.

    {1 SYNOPSIS}
    {[
      let store = Duraml_memory.create ~sw ~clock:env#clock
        ~random:env#secure_random () in
      Duraml.enqueue store spec
    ]}

    {1 DESCRIPTION}
    Each constructor creates an independent store with a random identity.
    Immutable maps hold jobs, keys, attempts and events. An Eio mutex serializes
    transaction-local updates. Exceptions discard the whole transaction.
    Waiters use a condition variable and a revision independent of audit events,
    so enqueue and shortened-lease wakeups cannot be lost.

    {1 LIFETIME AND COST}
    Share a handle between fibers in one Eio domain. Closing [sw] invalidates
    the handle and wakes waiters. There is no persistence or retention.
    Eligibility selection scans the queue's queued and running jobs.
    Listings filtered by queue, or by queue and state, read only those jobs
    from their cursor. Other listings read every job from it. This
    backend is intended for tests and small ephemeral queues, not a large
    persistent catalogue.

    {1 TESTING}
    Use [Eio_mock.Backend.run_full] for automatically advancing test clocks.
    Supply deterministic bytes through [Eio.Flow.string_source] for test
    randomness. Production callers supply [env#secure_random]. The same
    transition engine is used by the SQLite backend. *)

val create : sw:Eio.Switch.t -> clock:_ Eio.Time.clock ->
  random:_ Eio.Flow.source -> unit -> Duraml.plain
(** [create ~sw ~clock ~random ()] constructs an empty store owned by [sw].
    [clock] supplies Unix wall time. [random] supplies store identities and
    claim nonces. Construction consumes 16 random bytes and may suspend. Jobs and
    claims from other stores have no authority in this one. *)
