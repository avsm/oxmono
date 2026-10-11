(** Static sqlite-vec registration against the workspace SQLite. *)
val register : unit -> unit
(** [register ()] installs sqlite-vec for subsequently opened SQLite handles.
    Registration is idempotent and thread-safe. No loadable extension is used. *)
