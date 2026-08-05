(** Eio-friendly wrapper for SQLite3.

    This module wraps blocking SQLite3 operations so they run in system
    threads, allowing other Eio fibers to proceed concurrently. Database
    handles are Eio resources tied to an {!Eio.Switch.t} for automatic cleanup.

    {2 Error handling}

    Operations that can fail during open or prepare raise {!Eio.Exn.Io}
    with contextual information. Operations like {!exec} and {!step}
    return [Sqlite3.Rc.t] — use [Sqlite3.Rc.check] to convert to exceptions.

    {2 Cancellation}

    All blocking operations respect Eio's structured cancellation. If the
    calling fiber is cancelled (e.g. because the enclosing switch fails),
    the underlying SQLite operation is interrupted via [sqlite3_interrupt]
    and the cancellation exception propagates normally. There is no need
    to call interrupt manually.

    {2 Thread safety}

    SQLite defaults to serialized threading mode ([SQLITE_THREADSAFE=1]),
    so multiple fibers sharing a handle via system threads are safe.

    {2 Non-blocking operations}

    Use {!db} to obtain the underlying [Sqlite3.db] handle for operations
    that do not perform I/O, such as binding parameters ([Sqlite3.bind]),
    reading columns ([Sqlite3.column], [Sqlite3.column_text]), inspecting
    return codes, and so on. These do not need to run in a system thread. *)

(** {2 Errors} *)

type Eio.Exn.Backend.t += Sqlite3_rc of Sqlite3.Rc.t * string
(** Backend error carrying the SQLite return code and error message. *)

type error =
  | Open_failed of Eio.Exn.Backend.t  (** Database could not be opened. *)
  | Closed                            (** Operation on a closed database. *)

type Eio.Exn.err += E of error
(** Raised as [Eio.Exn.Io (E err, ctx)]. *)

val err : error -> exn
(** [err e] creates an {!Eio.Exn.Io} exception from [e]. *)

(** {2 Types} *)

type t = [ `Sqlite3 | `Close ] Eio.Resource.t
(** A database connection as an Eio resource. Supports {!Eio.Resource.close}. *)

val db : t -> Sqlite3.db
(** [db t] returns the underlying [Sqlite3.db] handle for use with
    non-blocking operations that do not perform I/O (binding parameters,
    reading column values, inspecting error codes, etc.). *)

(** {2 Opening and closing} *)

val open_path :
  sw:Eio.Switch.t ->
  ?busy_timeout:int ->
  ?mode:[ `READONLY | `NO_CREATE ] ->
  ?uri:bool ->
  ?mutex:[ `NO | `FULL ] ->
  ?cache:[ `SHARED | `PRIVATE ] ->
  ?vfs:string ->
  _ Eio.Path.t ->
  t
(** [open_path ~sw path] opens the database file at [path]. The database
    is automatically closed when [sw] finishes.

    The path is resolved via {!Eio.Path.native_exn}. The open runs in a
    system thread so other fibers are not blocked.

    @param busy_timeout if set, installs a busy handler that sleeps and
    retries for up to the given number of milliseconds when a table is
    locked. The retry sleeps happen inside the system thread, so other
    Eio fibers are not blocked.
    @param mode [`READONLY] opens read-only; [`NO_CREATE] opens read-write
    but will not create the file if it does not exist. Default is
    read-write-create.
    @param uri when [true], enables URI filename interpretation
    (corresponding to [SQLITE_OPEN_URI]).
    @param mutex [`NO] corresponds to [SQLITE_OPEN_NOMUTEX]; [`FULL]
    corresponds to [SQLITE_OPEN_FULLMUTEX].
    @param cache [`SHARED] corresponds to [SQLITE_OPEN_SHAREDCACHE];
    [`PRIVATE] corresponds to [SQLITE_OPEN_PRIVATECACHE].
    @param vfs the name of the VFS module to use.
    @raises Eio.Exn.Io with {!Open_failed} on failure, including the path
    in the error context. *)

val open_memory :
  sw:Eio.Switch.t ->
  ?busy_timeout:int ->
  ?mutex:[ `NO | `FULL ] ->
  ?cache:[ `SHARED | `PRIVATE ] ->
  unit ->
  t
(** [open_memory ~sw ()] opens a private in-memory database. The database
    is automatically closed when [sw] finishes.

    @param busy_timeout see {!open_path}.
    @raises Eio.Exn.Io with {!Open_failed} on failure. *)

(** {2 SQL execution} *)

val exec :
  t ->
  ?cb:(Sqlite3.row -> Sqlite3.headers -> unit) ->
  string ->
  Sqlite3.Rc.t
(** [exec t ?cb sql] executes the SQL string [sql] on database [t]. If
    the statement returns rows and [cb] is provided, [cb row headers] is
    called for each result row. Runs in a system thread.

    @return the SQLite return code for the operation. *)

val exec_no_headers :
  t -> cb:(Sqlite3.row -> unit) -> string -> Sqlite3.Rc.t
(** [exec_no_headers t ~cb sql] is like {!exec} but the callback receives
    only the row data, without column headers. Runs in a system thread.

    @return the SQLite return code for the operation. *)

(** {2 Prepared statements} *)

val prepare : t -> string -> Sqlite3.stmt
(** [prepare t sql] compiles [sql] into a prepared statement for database
    [t]. Runs in a system thread.

    Bind parameters to the returned statement and read column values
    using the underlying [Sqlite3.db] handle obtained via {!db}.

    @raises Eio.Exn.Io on failure, with the SQL string in the error
    context. *)

val step : t -> Sqlite3.stmt -> Sqlite3.Rc.t
(** [step t stmt] evaluates the prepared statement [stmt] until it has
    produced a result row or has finished executing. Runs in a system
    thread.

    @return [Sqlite3.Rc.ROW] if a new row of data is ready,
    [Sqlite3.Rc.DONE] when the statement has finished executing, or
    another return code on error. *)

val reset : t -> Sqlite3.stmt -> Sqlite3.Rc.t
(** [reset t stmt] resets the prepared statement [stmt] back to its initial
    state, ready to be re-executed (possibly with new bound parameters).
    Runs in a system thread.

    @return the SQLite return code for the operation. *)

val finalize : t -> Sqlite3.stmt -> Sqlite3.Rc.t
(** [finalize t stmt] destroys the prepared statement [stmt] and releases
    its resources. Runs in a system thread.

    @return the SQLite return code for the operation. *)

val iter :
  t -> Sqlite3.stmt -> f:(Sqlite3.Data.t array -> unit) -> Sqlite3.Rc.t
(** [iter t stmt ~f] steps through [stmt], calling [f row_data] for each
    result row. The statement is automatically reset afterwards. Runs in
    a system thread.

    @return [Sqlite3.Rc.DONE] on success, or another return code on
    error. *)

val fold :
  t ->
  Sqlite3.stmt ->
  f:('a -> Sqlite3.Data.t array -> 'a) ->
  init:'a ->
  Sqlite3.Rc.t * 'a
(** [fold t stmt ~f ~init] folds [f] over the rows returned by [stmt],
    starting from [init]. The statement is automatically reset afterwards.
    Runs in a system thread.

    @return [(rc, acc)] where [acc] is the final accumulated value and
    [rc] is [Sqlite3.Rc.DONE] on success. *)
