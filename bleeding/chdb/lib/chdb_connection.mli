(** Connection management for chdb.

    A {!t} wraps an in-process ClickHouse instance.  Only one active
    connection is allowed per process; opening a second while the
    first is alive is undefined behaviour in the C library.

    All operations are mutex-protected for thread safety.  The C
    stubs release the OCaml domain lock during heavyweight calls
    (connect, close, query) so other domains can run concurrently.

    {2 Typical usage}

    {[
      Connection.with_connection ~f:(fun conn ->
        let s = Query.execute_string conn ~format:TSV
                  "SELECT 42" in
        print_string s) ()
    ]} *)

type t
(** An open chdb connection. *)

val connect : ?path:string -> unit -> t
(** [connect ?path ()] creates a new chdb connection.

    @param path Filesystem path for persistent storage.
                When omitted the database is in-memory only.
    @raise Chdb_error.E if the C library returns [NULL]. *)

val close : t -> unit
(** [close t] closes the connection and releases C resources.
    Idempotent: calling on an already-closed connection is a no-op.
    Thread-safe: acquires the connection mutex. *)

val is_open : t -> bool
(** [is_open t] is [true] if the connection has not been closed. *)

val with_connection : ?path:string -> f:(t -> 'a) -> unit -> 'a
(** [with_connection ?path ~f ()] opens a connection, passes it
    to [f], and closes it when [f] returns or raises. *)

val path : t -> string option
(** [path t] is the database path, or [None] for in-memory. *)

val with_handle : t -> f:(Chdb_raw.connection_handle -> 'a) -> 'a
(** [with_handle t ~f] acquires the connection mutex, extracts the
    underlying C handle, and calls [f handle].  The mutex is held
    for the duration of [f].

    @raise Chdb_error.E if the connection is closed. *)
