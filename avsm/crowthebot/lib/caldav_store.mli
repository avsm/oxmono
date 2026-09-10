(** Profile-wide CalDAV source data, separate from agent memory. *)

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
  principal : string;
  job_id : int;
  checked_at : string option;
  error : string option;
  pending : bool;
}

type cursor = {
  id : int;
  mirror : int;
  collection : Caldav_data.collection;
  token : string option;
  generation : int;
  visible : int;
  rebuilding : bool;
  staged : bool;
  next_token : string option;
  more : bool;
  revision : int;
  synced_at : string option;
}

type entry = {
  version : int;
  collection : int;
  href : string;
  hash : string;
  observed_at : string;
  excerpt : string;
  parsed : bool;
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
  principal:string ->
  cron:string ->
  next_at:float ->
  mirror
(** [ensure t ~actor ~room ~event ~connection ~identity ~cron ~next_at]
    registers one persistent polling job per connection. Existing jobs keep
    their source, owner and schedule. Account rebinding is rejected. *)

val running : t -> actor:string -> int -> bool
val discover : t -> actor:string -> int -> Caldav_data.collection list -> unit
val cursors : t -> actor:string -> int -> cursor list

val stage : t -> actor:string -> cursor -> Caldav_data.page -> unit
(** [stage t ~actor cursor page] durably queues a report before any downloads.
    The previous complete generation stays visible during a rebuild. *)

val pending : t -> actor:string -> cursor -> (int * Caldav_data.change) list
val held : t -> actor:string -> cursor -> Caldav_data.change -> int option

type fetched = Gone | Held of int | Body of Caldav_data.item

val commit :
  t ->
  actor:string ->
  cursor ->
  (int * Caldav_data.change * fetched) list ->
  unit
(** [commit t ~actor cursor items] atomically saves bytes and ETags, removes
    completed work and advances the token only when the whole page is saved. It
    rejects stale revisions, cancelled jobs and revoked access. *)

val reset : t -> actor:string -> cursor -> unit
val finish : t -> actor:string -> int -> error:string option -> unit
val search : t -> actor:string -> int -> query:string -> after:int -> entry list
val read : t -> actor:string -> int -> version:int -> offset:int -> string * int
val counts : t -> actor:string -> cursor -> int * int
val agendas : t -> Caldav_agenda_store.t
