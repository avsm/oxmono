type context = {
  actor : string;
  room : string;
  event : string;
  source_event : string;
  source : string;
}

val with_context : context -> (unit -> 'a) -> 'a
(** [with_context context f] attaches provenance to exchanges made by [f].
    Bindings are local to the calling fiber and restored on return. *)

type t

val init : Sqlite3_eio.t -> now:string -> unit
(** [init db ~now] creates the trace table and marks unfinished exchanges
    interrupted. Call only while opening the exclusively locked profile. *)

val create : db:Sqlite3_eio.t -> mutex:Eio.Mutex.t -> now:(unit -> string) -> t
(** [create ~db ~mutex ~now] shares the profile's database and writer lock. *)

val wrap : t -> Fetch.plain -> Fetch.plain
(** [wrap t client] records complete request and response bodies in SQLite. It
    omits HTTP headers and URLs. Responses over 1 MiB are recorded as truncated
    failures with their prefix. Transport failures preserve any received prefix.
    Requests are committed before network effects. *)

val current : unit -> context option
(** [current ()] is the provenance bound to the calling fiber. *)
