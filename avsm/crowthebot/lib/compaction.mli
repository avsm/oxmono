type scope = Thread of { room : string; user : string } | Room of string
type t
type plan

val init : Sqlite3_eio.t -> unit
(** [init db] creates summary state during profile migration. *)

val create : db:Sqlite3_eio.t -> mutex:Eio.Mutex.t -> now:(unit -> string) -> t
(** [create ~db ~mutex ~now] shares the profile database and writer lock. *)

val touch : Sqlite3_eio.t -> scope -> unit
(** [touch db scope] invalidates pending snapshots after raw context changes.
    The caller must hold the database writer lock. *)

val clear : Sqlite3_eio.t -> room:string option -> user:string -> unit
(** [clear db ~room ~user] clears the user's thread summaries and affected
    shared summaries. [None] covers every room. The caller holds the writer
    lock. Revision counters survive clearing to reject in-flight results. *)

val context : t -> scope -> bytes:int -> string option
(** [context t scope ~bytes] returns bounded summary JSON, or [None]. *)

val prepare :
  t ->
  scope ->
  max_messages:int ->
  max_bytes:int ->
  incoming_messages:int ->
  incoming_bytes:int ->
  plan option
(** [prepare t scope ~max_messages ~max_bytes ~incoming_messages
     ~incoming_bytes] snapshots an older prefix near the configured bounds. It
    retains up to eight recent messages, preserving complete chat exchanges when
    they fit. Incoming data is counted for capacity but is absent from the
    snapshot. *)

val input : plan -> string
(** [input plan] is the bounded JSON snapshot for a tool-free model request. *)

val limit : plan -> int
(** [limit plan] is the maximum UTF-8 byte length of its replacement summary. *)

val commit : t -> plan -> body:string -> bool
(** [commit t plan ~body] atomically saves a nonempty bounded summary and
    removes its covered prefix. It returns [false] if context changed since
    preparation. Invalid summaries raise [Invalid_argument]. *)
