@@ portable

(** Shared SQLite connection, statement scopes and transaction ownership. *)

type blob_dir = Dir : _ Eio.Path.t -> blob_dir

type conn = { db : Sqlite3_eio.t; handle : Sqlite3.db }
(** [handle] is [Sqlite3_eio.db db], looked up once because each lookup
    allocates. *)

type t : value mod portable contended
(** A connection behind an Eio mutex, with the blob directory. A portable
    closure may capture a [t], since {!locked} is the only way to reach its
    connection. *)

val v : Sqlite3_eio.t -> blob_dir option -> t
(** [v db dir] is a store connection over [db] with blob directory [dir]. *)

val blob_dir : t -> blob_dir option @@ nonportable
(** [blob_dir t] is the blob directory of [t]. It is nonportable so that
    only the domain that opened [t] uses the directory. *)

val fail : string -> 'a

val check : conn -> Sqlite3.Rc.t -> unit
(** [check t rc] raises [Sqlite3.SqliteError] carrying the name of [rc] and
    the connection's error message unless [rc] is a success code. *)

(** The operations from here to {!ns} take the connection that {!locked}
    or {!transaction} hands to its callback, and use it only there. *)

val sql : conn -> string -> unit

val with_stmt : conn -> string -> (Sqlite3.stmt -> 'a) -> 'a
(** [with_stmt t sql f] finalizes the statement on every callback outcome.
    The callback must neither retain nor finalize the statement. *)

val bind : conn -> Sqlite3.stmt -> Sqlite3.Data.t list -> unit
(** [bind t stmt values] raises [Invalid_argument] unless [values] has one
    entry per statement parameter. *)

val run : conn -> string -> Sqlite3.Data.t list -> unit

val run_prepared : conn -> Sqlite3.stmt -> Sqlite3.Data.t list -> unit
(** [run_prepared t stmt values] executes a write and then resets [stmt]
    and clears its bindings, whether or not the write succeeded. *)

val rows : conn -> string -> Sqlite3.Data.t list -> Sqlite3.Data.t array list

val rows_prepared : conn -> Sqlite3.stmt -> Sqlite3.Data.t list ->
  Sqlite3.Data.t array list
(** [rows_prepared t stmt values] reads every row and then resets [stmt]
    and clears its bindings, whether or not the read succeeded. *)

val batch : conn -> (unit -> 'a) -> 'a
(** [batch t f] is [f ()] evaluated in one system thread, so a loop of
    statements costs one thread hop rather than two or more per statement.
    [f] runs outside Eio and must not perform an Eio operation. Of the
    operations here it may use only the [bind_] functions, {!batch_exec},
    {!batch_row}, {!changes} and the value codecs. *)

val bind_text : conn -> Sqlite3.stmt -> int -> string -> unit
val bind_int : conn -> Sqlite3.stmt -> int -> int -> unit
val bind_int64 : conn -> Sqlite3.stmt -> int -> int64 -> unit
val bind_null : conn -> Sqlite3.stmt -> int -> unit
(** [bind_text t stmt n x], [bind_int t stmt n x], [bind_int64 t stmt n x]
    and [bind_null t stmt n] bind parameter [n] of [stmt], counting from 1,
    without building a value list. *)

val batch_exec : conn -> Sqlite3.stmt -> unit
(** [batch_exec t stmt] executes a write whose parameters the caller has
    bound, then resets [stmt] and clears its bindings, whether or not the
    write succeeded. It is for use inside {!batch}. *)

val batch_row : conn -> Sqlite3.stmt -> Sqlite3.Data.t array option
(** [batch_row t stmt] is the first row of a read whose parameters the
    caller has bound, or [None] if it has none. It then resets [stmt] and
    clears its bindings, whether or not the read succeeded. It is for use
    inside {!batch}. *)

val changes : conn -> int
(** [changes t] is the number of rows changed by the last write. *)

val text : Sqlite3.Data.t -> string
val int : Sqlite3.Data.t -> int64
val nullable_int : Sqlite3.Data.t -> int64 option
val nullable_text : Sqlite3.Data.t -> string option
val i : int64 -> Sqlite3.Data.t
val s : string -> Sqlite3.Data.t
val ni : int64 option -> Sqlite3.Data.t
val ns : string option -> Sqlite3.Data.t

val locked : t -> (conn -> 'a) -> 'a
(** [locked t f] is [f] applied to the connection of [t] while holding its
    mutex, without opening an SQL transaction. [f] must not retain the
    connection. It raises [Invalid_argument] if the calling fiber already
    holds the mutex. *)

val with_stmt_across_locks : t -> string -> (Sqlite3.stmt -> 'a) -> 'a
(** [with_stmt_across_locks t sql f] is {!with_stmt} for a statement that
    [f] executes in several {!locked} sections. It takes the lock, without
    cancellation, only to prepare and to finalize. *)

val transaction : ?begin_sql:string -> t -> (conn -> 'a) -> 'a
(** [transaction t f] is {!locked} [t f] inside [begin_sql].
    [begin_sql] defaults to ["BEGIN IMMEDIATE"]. The callback must use the
    unlocked operations above. A nested call from the same fiber raises
    [Invalid_argument]. Cancellation cannot interrupt the begin, commit or
    rollback statements. Waiting for the mutex remains cancellable, and a
    cancelled callback is rolled back before the mutex is released. *)
