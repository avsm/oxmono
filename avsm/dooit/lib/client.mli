(** Backend-aware task operations for applications, CLIs and agent adapters. *)

type t
type write_policy = Local_only | Synchronize
type publication = Local | Pushed

type pending = {
  id : string;
  operation : string;
  revision : string;
  message : string;
}

type outcome = Applied of Doc.t * publication | Pending of pending

type sync_result =
  | Local_status
  | Git_status of Git.status
  | Git_synced of string
  | Webdav_synced of Sync.action list

val local : Store.t -> t
(** [local store] selects an explicitly local store. *)

val git : process_mgr:_ Eio_unix.Process.mgr -> Store.t -> Git.config -> t
(** [git ~process_mgr store config] selects Git synchronization. Construction
    does not initialize the repository or contact the remote. *)

val of_config :
  process_mgr:_ Eio_unix.Process.mgr ->
  ?remote:(readonly:bool -> Remote.t) ->
  Store.t ->
  Config.t ->
  t
(** [of_config ~process_mgr ?remote store config] selects the configured
    backend. [remote] constructs a scoped WebDAV transport when synchronization
    is requested. Reads and local writes do not construct that transport. *)

val store : t -> Store.t
val backend : t -> Config.backend

val apply :
  t ->
  policy:write_policy ->
  check:(unit -> unit) ->
  id:string ->
  expected:string option ->
  operation:string ->
  payload:string ->
  (Doc.t option -> Doc.t) ->
  outcome
(** [apply t ~policy ~check ... change] performs a revision-checked, idempotent
    write. [Synchronize] pulls before Git writes and commits and pushes before
    returning [Pushed]. It returns [Pending] if the local write survived an
    unconfirmed push. Retry the same operation and arguments. The Local backend
    always returns [Local]. WebDAV synchronized writes are refused before any
    mutation; local writes followed by explicit [sync] remain available.

    [check] runs on the Eio fiber before and after work. [change] runs in a
    worker thread. Cancellation and authority failures propagate. *)

val saved : outcome -> Doc.t
(** [saved outcome] returns the saved document, or raises [Common.Error] with
    the task and operation IDs when its publication is pending. *)

val modify : t -> policy:write_policy -> string -> (Doc.t -> Doc.t) -> outcome
(** [modify t ~policy id change] reads [id] and applies [change] conditionally
    using the observed revision and a new operation ID. *)

val add : t -> policy:write_policy -> Doc.t -> outcome

val capture :
  ?new_task:bool ->
  ?tags:string list ->
  ?body:string ->
  ?due:string ->
  ?hints:Common.value ->
  t ->
  policy:write_policy ->
  title:string ->
  Common.value ->
  Doc.t list
(** [capture t ~policy ~title target] returns existing linked tasks or creates
    one. A pending publication raises [Common.Error] identifying the saved task.
    Existing tasks are returned without synchronizing them. *)

val sync : ?report:string -> dry_run:bool -> t -> sync_result
(** [sync ~dry_run t] synchronizes the selected backend. Git previews use local
    status without contacting the remote. Explicit Git synchronization may
    initialize its repository. WebDAV previews use a read-only transport. *)

val sync_lines : sync_result -> string list
(** [sync_lines result] describes a synchronization or preview result. *)
