val default_path : unit -> string
val open_session : sw:Eio.Switch.t -> fs:_ Eio.Path.t ->
  component:string -> Taposaur_audit.session
(** Uses TAPOSAUR_AUDIT_DB, default taposaur-audit.db, and TAPOSAUR_ACTOR,
    default system. No raw command-line arguments are logged. *)
val command : (unit, string) result Cmdliner.Cmd.t
