(** Immutable, profile-shared email result pages, separate from agent memory. *)

type t

val init : Sqlite3_eio.t -> unit

val create :
  db:Sqlite3_eio.t ->
  mutex:Eio.Mutex.t ->
  admin:string ->
  now:(unit -> float) ->
  t

val authorize : t -> actor:string -> unit

val save :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  connection:string ->
  writable:bool ->
  operation:string ->
  string ->
  int
(** [save t ~actor ~room ~event ~connection ~writable ~operation data] records
    provenance and an immutable result. Keeps 50 results, at most 64 MiB, for up
    to 24 hours. Each result is limited to 32 MiB. *)

type page = {
  data : string;
  total : int;
  observed : float;
  connection : string;
  mode : string;
}

val read : t -> actor:string -> id:int -> offset:int -> page
(** [read t ~actor ~id ~offset] reads up to 4096 bytes at a byte offset. The
    caller must preserve UTF-8 boundaries when returning smaller pages. *)
