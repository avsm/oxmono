(** SQLite schema validation, version migration and durable connection setup. *)

val open_readonly : sw:Eio.Switch.t -> _ Eio.Path.t -> Database.t
val open_path : sw:Eio.Switch.t -> ?blob_dir:_ Eio.Path.t ->
  _ Eio.Path.t -> Database.t
(** [open_path ~sw path] opens a WAL database with full synchronous writes.
    Schema migration shares the connection's transaction owner. A failed
    open closes the SQLite handle before raising. Otherwise [sw] owns the
    handle and releases it on scope exit. *)
