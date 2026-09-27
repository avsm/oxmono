(** SQLite schema creation, validation and durable connection setup. *)

val open_readonly : sw:Eio.Switch.t -> _ Eio.Path.t -> Database.t
val open_path : sw:Eio.Switch.t -> ?blob_dir:_ Eio.Path.t ->
  _ Eio.Path.t -> Database.t
(** [open_path ~sw path] opens a WAL database with full synchronous writes
    and creates the schema in an empty one. A failed open closes the SQLite
    handle before raising. Otherwise [sw] owns the handle and releases it on
    scope exit. *)
