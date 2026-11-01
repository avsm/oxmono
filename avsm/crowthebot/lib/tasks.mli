type t

val create :
  tools:Dooit_agent.t ->
  store_id:string ->
  profile:string ->
  allowed:(actor:string -> room:string -> bool) ->
  t
(** [create ~tools ~store_id ~profile ~allowed] wraps existing task tools with
    access checks and stable, host-generated operation identifiers. *)

val available : t -> actor:string -> room:string -> bool
val system_prompt : string
val unavailable_prompt : string

val invoke :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  source_event:string ->
  call_id:string ->
  check:(unit -> unit) ->
  string ->
  string ->
  (string, string) result
(** [invoke t ... name arguments] checks access before and after I/O. Supply
    authentic event IDs and the tool call ID. Retrying the same event and call
    reuses its operation. Cancellation propagates. The caller owns auditing. *)

val load :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  process_mgr:_ Eio_unix.Process.mgr ->
  store:Store.t ->
  profile:string ->
  max_bytes:int ->
  unit ->
  t option
(** [load ...] discovers Dooit's existing XDG configuration and store without
    writing or synchronizing them. A missing store returns [None]. Malformed
    configuration or store metadata raises. Each invocation reloads the
    configuration and rechecks Crow's account and room authorization. Git writes
    pull, commit and push. Local writes report [sync=local_only]. WebDAV reads
    are available, but writes fail explicitly. A failed Git push reports a
    locally saved task with synchronization pending. *)
