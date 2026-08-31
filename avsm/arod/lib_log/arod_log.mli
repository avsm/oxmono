(** SQLite-backed HTTP request logging. *)

type t

val create : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [create ~sw path] is the access log at [path]. It closes with [sw]. *)

val reader : t -> Sqlite3_eio.t
(** [reader t] is a read-only analytics connection to [t]. *)

val log_request : t -> timestamp:float -> Proffer_httpz.event -> unit
(** [log_request t ~timestamp event] records [event] at Unix time [timestamp]. *)
