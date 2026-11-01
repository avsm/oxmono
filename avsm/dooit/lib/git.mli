(** Explicit Git synchronization of store metadata and task notes. *)

type config = { remote : string; branch : string }

type status = {
  initialized : bool;
  branch : string option;
  dirty : bool;
  ahead : int option;
  behind : int option;
  conflicts : string list;
}

val status : process_mgr:_ Eio_unix.Process.mgr -> Store.t -> config -> status
(** [status ~process_mgr store config] reports local Git and Dooit conflicts.
    [dirty] concerns store metadata and notes. Ahead/behind counts use the last
    fetched remote branch, or are absent if either commit is unavailable. This
    makes no network request and does not create private state or refresh the
    Git index. A detached HEAD has no branch name. *)

val sync : process_mgr:_ Eio_unix.Process.mgr -> Store.t -> config -> string
(** [sync ~process_mgr store config] commits valid local changes, integrates the
    remote branch and pushes. An empty remote receives the first commit.
    Independent changes merge only when Git finds no conflict and the result
    preserves valid notes, store identity and immutable metadata. A failed merge
    is aborted, retaining the local commit for explicit resolution. A failed
    push retains all local commits. No force-push, reset or stash is used. *)

exception
  Pending of {
    id : string;
    operation : string;
    revision : string;
    message : string;
  }
(** The operation was saved locally, but synchronization is not confirmed. Retry
    the same operation and arguments after resolving the reported error. *)

val apply :
  process_mgr:_ Eio_unix.Process.mgr ->
  check:(unit -> unit) ->
  Store.t ->
  config ->
  id:string ->
  expected:string option ->
  operation:string ->
  payload:string ->
  (Doc.t option -> Doc.t) ->
  Doc.t
(** [apply ... change] pulls, checks authority and the expected revision,
    applies [change], commits its note and pushes before returning success. The
    repository and remote branch must already be initialized. [change] runs
    synchronously in a worker thread. [check] runs on the calling Eio fiber.

    One store lock covers the sequence. A pending retry validates its recorded
    request and result without reapplying the change, and refuses unrelated
    dirty notes. A confirmed retry returns the recorded result without
    publishing later edits. Cancellation propagates with the local journal
    retained. *)

val default : config
(** [default] selects [fork.recoil.org:git/dooit] and branch [main]. *)

val setup : process_mgr:_ Eio_unix.Process.mgr -> Store.t -> config -> string
(** [setup ~process_mgr store config] initializes a repository at the store root
    and configures origin. It makes no network request or commit. An existing
    origin or branch must match [config]. Private state is locally excluded. *)

val clone :
  process_mgr:_ Eio_unix.Process.mgr -> root:string -> config -> Store.t
(** [clone ~process_mgr ~root config] fetches and validates a store into a new
    directory, preserving its identity. [root] must not already exist.
    Validation happens in a private sibling directory before rename. A
    concurrently created empty destination can be replaced, but a nonempty
    destination is preserved. *)

val pull : process_mgr:_ Eio_unix.Process.mgr -> Store.t -> config -> string
(** [pull ~process_mgr store config] fast-forwards clean notes from origin. It
    refuses different store identities, removed notes, invalid files, unresolved
    conflicts and divergent history. *)

val push : process_mgr:_ Eio_unix.Process.mgr -> Store.t -> config -> string
(** [push ~process_mgr store config] commits current valid notes and pushes
    normally. It refuses removed notes, unrelated tracked files, unresolved
    conflicts and a remote branch not contained in local history. A rejected
    push retains its local commit. No operation resets, stashes or force-pushes.

    Operations use the store's nonblocking lock. External Git clients and text
    editors do not honor it and must not mutate the repository concurrently. All
    functions require an Eio scheduler and raise [Common.Error] on refusal. *)
