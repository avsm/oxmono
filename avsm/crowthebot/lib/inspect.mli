val sections : string list
(** [sections] lists the supported database views. *)

val read :
  Sqlite3_eio.t -> section:string -> after:int -> limit:int -> Jsont.json
(** [read db ~section ~after ~limit] returns one page and outstanding counts in
    a read-only snapshot. It neither migrates the database nor recovers jobs.
    [after] is a row cursor, and [limit] is between 1 and 100. *)

val run :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  section:string ->
  after:int ->
  limit:int ->
  unit
(** [run ~env ~sw ~profile ~section ~after ~limit] prints a JSON page from the
    profile database opened read-only, without taking the bot's process lock. *)
