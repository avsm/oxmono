(** Cross-process advisory locking primitive.

    Backed by [Unix.lockf] (POSIX fcntl record locks). The lock is held while a
    file descriptor to the lockfile remains open; it is released on explicit
    {!release}, on the surrounding {!with_lock}'s exit (success or exception),
    on switch teardown for the {!acquire} form, and on process termination — so
    a crashed process never strands a held lock.

    The single user of this primitive is {!Oi.Lock.acquire_global}, which takes
    [<data_dir>/.oi.lock] at the top of every [oi] command in
    {!Cmd.Harness.bootstrap}. The lockfile is a tiny sentinel created on first
    use; we record the holder's pid into it as a debug aid.

    {1 Modes}

    Multiple {!Shared} holders coexist. An {!Exclusive} holder excludes every
    other holder of any mode. The [oi] command lock uses {!Exclusive}.

    {1 Limitations}

    - Advisory: code that bypasses this primitive and writes the cache directly
      can still corrupt it.
    - Local-FS only. [Unix.lockf] over NFS is fragile in mixed-version clusters;
      this is an unsupported configuration.
    - Not re-entrant. A process that already holds [lock(R)] must not try to
      acquire it again — closing the inner fd would release the outer's
      kernel-side lock (POSIX fcntl record locks are owned per-process). *)

type mode = Shared | Exclusive

type strategy =
  | Block
      (** Wait indefinitely. Status logged every [log_interval_s] seconds via
          [on_wait]. *)
  | Block_timeout of float
      (** Wait up to N seconds; raise {!Lock_unavailable} on timeout. *)
  | No_wait
      (** Fail immediately with {!Lock_unavailable} if the lock is held. *)

type t
(** A held lock. Owned by the calling fiber until {!release} fires (manually or
    via {!with_lock}'s [Fun.protect]). *)

exception Lock_unavailable of { path : string; held_by_pid : int option }

val release : t -> unit
(** [release t] drops the lock and closes the file descriptor. Idempotent —
    calling twice is safe. *)

val path : t -> string
(** [path t] is the lockfile path the [t] is holding. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the lockfile path. *)

val acquire :
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  fs:_ Eio.Path.t ->
  path:string ->
  unit ->
  t
(** [acquire ~sw ~clock ~fs ~path ()] acquires the lockfile at [path] in [mode]
    (default {!Exclusive}) under [strategy] (default {!Block}) and returns the
    held lock. The lock is automatically released when [sw] ends (normal
    completion or cancellation); call {!release} to release it earlier.

    Use this when the lock's lifetime spans several function calls, when you
    need to hold multiple locks side-by-side in one scope, or when integrating
    with other Eio resources that already accept a [~sw].

    [on_wait] (default no-op) is invoked at most every [log_interval_s] seconds
    (default 5.0) while blocked, with the elapsed wait time and the pid recorded
    in the lockfile (if any).

    @raise Lock_unavailable per [strategy]. *)

val with_lock :
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  clock:_ Eio.Time.clock ->
  fs:_ Eio.Path.t ->
  path:string ->
  (t -> 'a) ->
  'a
(** [with_lock ~clock ~fs ~path f] is the block-scoped wrapper around
    {!acquire}: it opens a fresh switch, acquires, runs [f t], and releases on
    [f]'s return or exception. Equivalent to:

    {[
    Eio.Switch.run @@ fun sw ->
    let t = acquire ~sw ~clock ~fs ~path () in
    Fun.protect ~finally:(fun () -> release t) (fun () -> f t)
    ]}

    Prefer this for one-shot acquisitions; reach for {!acquire} when the lock
    needs to outlive a single block. *)
