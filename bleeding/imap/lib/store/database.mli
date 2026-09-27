(** Shared SQLite connection, statement scopes and transaction ownership. *)

type blob_dir = Dir : _ Eio.Path.t -> blob_dir
type t = {
  db : Sqlite3_eio.t;
  handle : Sqlite3.db;
  mutex : Eio.Mutex.t;
  blob_dir : blob_dir option;
}
(** [handle] is [Sqlite3_eio.db db], looked up once because each lookup
    allocates. *)

val fail : string -> 'a

val check : t -> Sqlite3.Rc.t -> unit
(** [check t rc] raises [Sqlite3.SqliteError] carrying the name of [rc] and
    the connection's error message unless [rc] is a success code. *)

(** The operations from here to {!ns} require the caller to hold
    [t.mutex], normally through {!transaction} or {!locked}. *)

val sql : t -> string -> unit

val with_stmt : t -> string -> (Sqlite3.stmt -> 'a) -> 'a
(** [with_stmt t sql f] finalizes the statement on every callback outcome.
    The callback must neither retain nor finalize the statement. *)

val bind : t -> Sqlite3.stmt -> Sqlite3.Data.t list -> unit
(** [bind t stmt values] raises [Invalid_argument] unless [values] has one
    entry per statement parameter. *)

val run : t -> string -> Sqlite3.Data.t list -> unit

val run_prepared : t -> Sqlite3.stmt -> Sqlite3.Data.t list -> unit
(** [run_prepared t stmt values] executes a write and then resets [stmt]
    and clears its bindings, whether or not the write succeeded. *)

val rows : t -> string -> Sqlite3.Data.t list -> Sqlite3.Data.t array list

val rows_prepared : t -> Sqlite3.stmt -> Sqlite3.Data.t list ->
  Sqlite3.Data.t array list
(** [rows_prepared t stmt values] reads every row and then resets [stmt]
    and clears its bindings, whether or not the read succeeded. *)

val batch : t -> (unit -> 'a) -> 'a
(** [batch t f] is [f ()] evaluated in one system thread, so a loop of
    statements costs one thread hop rather than two or more per statement.
    [f] runs outside Eio and must not perform an Eio operation. Of the
    operations here it may use only the [bind_] functions, {!batch_exec},
    {!batch_row}, {!changes} and the value codecs. *)

val bind_text : t -> Sqlite3.stmt -> int -> string -> unit
val bind_int64 : t -> Sqlite3.stmt -> int -> int64 -> unit
val bind_null : t -> Sqlite3.stmt -> int -> unit
(** [bind_text t stmt n x], [bind_int64 t stmt n x] and [bind_null t stmt n]
    bind parameter [n] of [stmt], counting from 1, without building a
    value list. *)

val batch_exec : t -> Sqlite3.stmt -> unit
(** [batch_exec t stmt] executes a write whose parameters the caller has
    bound, then resets [stmt] and clears its bindings, whether or not the
    write succeeded. It is for use inside {!batch}. *)

val batch_row : t -> Sqlite3.stmt -> Sqlite3.Data.t array option
(** [batch_row t stmt] is the first row of a read whose parameters the
    caller has bound, or [None] if it has none. It then resets [stmt] and
    clears its bindings, whether or not the read succeeded. It is for use
    inside {!batch}. *)

val changes : t -> int
(** [changes t] is the number of rows changed by the last write. *)

val text : Sqlite3.Data.t -> string
val int : Sqlite3.Data.t -> int64
val nullable_int : Sqlite3.Data.t -> int64 option
val nullable_text : Sqlite3.Data.t -> string option
val i : int64 -> Sqlite3.Data.t
val s : string -> Sqlite3.Data.t
val ni : int64 option -> Sqlite3.Data.t
val ns : string option -> Sqlite3.Data.t

val locked : t -> (unit -> 'a) -> 'a
(** [locked t f] runs [f] holding [t.mutex] without opening an SQL
    transaction. It raises [Invalid_argument] if the calling fiber already
    holds [t.mutex]. *)

val transaction : ?begin_sql:string -> t -> (unit -> 'a) -> 'a
(** [transaction t f] runs [f] inside [begin_sql] while holding [t.mutex].
    [begin_sql] defaults to ["BEGIN IMMEDIATE"]. The callback must use the
    unlocked operations above. A nested call from the same fiber raises
    [Invalid_argument]. Cancellation cannot interrupt the begin, commit or
    rollback statements. Waiting for the mutex remains cancellable, and a
    cancelled callback is rolled back before the mutex is released. *)
