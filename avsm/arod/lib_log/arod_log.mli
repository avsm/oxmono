(** SQLite-backed HTTP request logging.

    Stores comprehensive request/response metadata for webstats analysis.
    Uses WAL mode for fast synchronous inserts (~50-100us per request). *)

type t

val create : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [create ~sw path] opens or creates the access log database at [path].
    Enables WAL mode and creates the schema if needed.
    The database is automatically closed when [sw] finishes. *)

val globalize : string @ local -> string
(** [globalize s] copies a local string to a global one. *)

val reader : t -> Sqlite3_eio.t
(** [reader t] is a read-only handle on the log database, for analytics.
    It is a separate connection from the one {!log_request} writes on:
    SQLite serializes calls per connection, so a query issued on the
    writer would block request logging for as long as it scans. *)

val log_request : t -> Httpz_eio_server.request_info @ local -> unit
(** [log_request t info] inserts a request log entry synchronously.
    Accepts the record as [@ local] — all values are extracted
    and bound to SQLite parameters before returning. *)
