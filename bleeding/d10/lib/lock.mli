(** Cross-process advisory locks on a local filesystem.

    {!with_lock} releases on return or exception. {!acquire} releases when its
    switch ends. Locks are also released when the process exits. The lockfile
    records the holder's PID for diagnostics.

    Shared holders coexist. An exclusive holder excludes other processes. POSIX
    record locks belong to a process, so this API neither synchronizes fibers in
    one process nor supports nested acquisition of the same lock. Closing
    another descriptor for the same file can release the lock. Callers must
    separately serialize fibers and keep all cache writers under the same lock.
    Network filesystems are unsupported. *)

type mode = Shared | Exclusive

type strategy =
  | Block
      (** Wait indefinitely. Status logged every [log_interval_s] seconds via
          [on_wait]. *)
  | Block_timeout of float
      (** Wait up to N seconds. Raise {!Lock_unavailable} on timeout. *)
  | No_wait
      (** Fail immediately with {!Lock_unavailable} if the lock is held. *)

type t
(** A held lock. Owned by the calling fiber until {!release} fires (manually or
    via {!with_lock}'s [Fun.protect]). *)

exception Lock_unavailable of { path : string; held_by_pid : int option }

val release : t -> unit
(** [release t] drops the lock and closes the file descriptor. Idempotent
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
    completion or cancellation). Call {!release} to release it earlier.

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

    Prefer this for one-shot acquisitions. Reach for {!acquire} when the lock
    needs to outlive a single block. *)
