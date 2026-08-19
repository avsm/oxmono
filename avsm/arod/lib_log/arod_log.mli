(** SQLite-backed HTTP request logging.

    Stores comprehensive request/response metadata for webstats analysis.
    Uses WAL mode for fast synchronous inserts (~50-100us per request). *)

type t

val create : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [create ~sw path] opens or creates the access log database at [path].
    Enables WAL mode and creates the schema if needed.
    The database is automatically closed when [sw] finishes. *)

val reader : t -> Sqlite3_eio.t
(** [reader t] is a read-only handle on the log database, for analytics.
    It is a separate connection from the one {!log_request} writes on:
    SQLite serializes calls per connection, so a query issued on the
    writer would block request logging for as long as it scans. *)

val log_request : t -> timestamp:float -> Proffer_httpz.event -> unit
(** [log_request t ~timestamp event] inserts one row for [event], recording
    it at [timestamp] seconds since the epoch. Host, User-Agent, Referer,
    Accept and the forwarding fields are read out of the event's request
    headers case-insensitively, and the whole header block is stored as JSON
    beside them. The insert is synchronous, so it must run on the domain that
    owns [t]. *)
