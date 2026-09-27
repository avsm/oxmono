(** Shared SQLite connection, statement scopes and transaction ownership. *)

type blob_dir = Dir : _ Eio.Path.t -> blob_dir
type t = {
  db : Sqlite3_eio.t;
  mutex : Eio.Mutex.t;
  blob_dir : blob_dir option;
  schema_version : int64;
}

val fail : string -> 'a
val check : Sqlite3.Rc.t -> unit
(** Low-level operations below require caller serialization on [mutex]. *)
val sql : t -> string -> unit
val with_stmt : t -> string -> (Sqlite3.stmt -> 'a) -> 'a
(** [with_stmt t sql f] finalizes the statement on every callback outcome.
    The callback must neither retain nor finalize the statement. *)
val bind : Sqlite3.stmt -> Sqlite3.Data.t list -> unit
val run : t -> string -> Sqlite3.Data.t list -> unit
val run_prepared : t -> Sqlite3.stmt -> Sqlite3.Data.t list -> unit
val rows : t -> string -> Sqlite3.Data.t list -> Sqlite3.Data.t array list
val text : Sqlite3.Data.t -> string
val int : Sqlite3.Data.t -> int64
val nullable_int : Sqlite3.Data.t -> int64 option
val nullable_text : Sqlite3.Data.t -> string option
val i : int64 -> Sqlite3.Data.t
val s : string -> Sqlite3.Data.t
val ni : int64 option -> Sqlite3.Data.t
val ns : string option -> Sqlite3.Data.t
val transaction : ?begin_sql:string -> t -> (unit -> 'a) -> 'a
(** [transaction t f] serializes the transaction on [t.mutex]. The callback
    must use unlocked database operations, not nest another transaction.
    Acquisition, commit and rollback protect their resource transitions from
    cancellation; callback cancellation rolls back before releasing the mutex. *)
