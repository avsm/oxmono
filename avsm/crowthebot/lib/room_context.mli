type t

val init : Sqlite3_eio.t -> unit
(** [init db] creates the room observation table during profile migration. *)

val create : db:Sqlite3_eio.t -> mutex:Eio.Mutex.t -> now:(unit -> string) -> t
(** [create ~db ~mutex ~now] shares the profile database and writer lock. *)

val seen : t -> event:string -> bool
(** [seen t ~event] checks the durable cache of the last 2048 events per room,
    including events whose observation has been compacted. *)

val record :
  t ->
  room:string ->
  sender:string ->
  event:string ->
  body:string ->
  max_messages:int ->
  max_bytes:int ->
  int option
(** [record t ~room ~sender ~event ~body ~max_messages ~max_bytes] retains a
    bounded message before model processing. Duplicate events return [None]. *)

val finish :
  t -> id:int -> note:string -> max_messages:int -> max_bytes:int -> unit
(** [finish t ~id ~note ~max_messages ~max_bytes] adds a model observation and
    trims the room to its configured bounds. Deleted rows stay deleted. *)

val context : t -> room:string -> bytes:int -> string
(** [context t ~room ~bytes] returns recent observation excerpts as a bounded
    JSON array in chronological order. [bytes] must be at least two. Sender and
    event identities are data. *)
