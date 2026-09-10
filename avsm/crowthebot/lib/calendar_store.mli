(** Profile-wide calendar source data, separate from agent memory. *)

type t

val create :
  db:Sqlite3_eio.t ->
  mutex:Eio.Mutex.t ->
  admin:string ->
  now:(unit -> float) ->
  timestamp:(float -> string) ->
  t

val init : Sqlite3_eio.t -> unit
val now : t -> float
val authorize : t -> actor:string -> unit

type mirror = {
  mirror_id : int;
  connection : string;
  identity : string;
  account : string;
  username : string;
  job_id : int;
  checked_at : string option;
  error : string option;
  pending : bool;
}

type cursor = {
  mirror : int;
  kind : Jmap_eio.Calendars.kind;
  state : string option;
  phase : string;
  generation : int;
  visible : int;
  position : int;
  query_state : string option;
  revision : int;
  synced_at : string option;
}

type entry = {
  version : int;
  remote_id : string;
  kind : string;
  hash : string;
  observed_at : string;
  excerpt : string;
}

val get : t -> actor:string -> int -> mirror
val list : t -> actor:string -> after:int -> mirror list

val ensure :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  connection:string ->
  identity:string ->
  account:string ->
  username:string ->
  cron:string ->
  next_at:float ->
  mirror
(** [ensure t ~actor ~room ~event ~connection ~identity ~cron ~next_at]
    registers one persistent polling job per connection. Existing jobs keep
    their source, owner and schedule. Account rebinding is rejected. *)

val cursors : t -> actor:string -> int -> cursor list

val commit :
  t ->
  actor:string ->
  cursor ->
  cursor ->
  items:Jmap_eio.Calendars.item list ->
  destroyed:string list ->
  receipts:Jmap_eio.Calendars.receipt list ->
  unit
(** [commit t ~actor before after ~items ~destroyed ~receipts] atomically saves
    a page and advances its cursor, checking the revision and active job. *)

val reset :
  t ->
  actor:string ->
  cursor ->
  receipts:Jmap_eio.Calendars.receipt list ->
  unit

val finish :
  t ->
  actor:string ->
  int ->
  error:string option ->
  pending:bool ->
  more:bool ->
  unit

val search :
  t ->
  actor:string ->
  int ->
  kind:Jmap_eio.Calendars.kind ->
  query:string ->
  after:int ->
  entry list

val read :
  t ->
  actor:string ->
  int ->
  version:int ->
  ical:bool ->
  offset:int ->
  string * int
(** [read t ~actor id ~version ~ical ~offset] reads up to 4097 source bytes and
    the total length. Only a currently visible version is readable. *)

val counts : t -> actor:string -> int -> (string * int * int) list
val pending_blobs : t -> actor:string -> int -> string list

val save_blob :
  t -> actor:string -> int -> blob:string -> (string, string) result -> unit

val blob_counts : t -> actor:string -> int -> int * int
val running : t -> actor:string -> int -> bool
val blob_failures : t -> actor:string -> int -> int
