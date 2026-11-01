type t
(** Profile-shared, immutable agenda snapshots with source provenance. *)

val init : Sqlite3_eio.t -> unit

val create :
  db:Sqlite3_eio.t ->
  mutex:Eio.Mutex.t ->
  admin:string ->
  now:(unit -> float) ->
  timestamp:(float -> string) ->
  t

type snapshot = {
  id : int;
  mirror : int;
  start : string;
  finish : string;
  fetched_at : string;
  complete : bool;
  metadata : Jsont.json;
  count : int;
}

val get : t -> actor:string -> int -> snapshot

val fresh :
  t ->
  actor:string ->
  mirror:int ->
  collection:int ->
  Caldav_agenda.window ->
  snapshot option
(** [fresh t ~actor ~mirror ~collection window] reuses only complete snapshots
    fetched within five minutes. Selection zero means all event calendars. *)

type collection = {
  id : int;
  title : string;
  timezone : string option;
  resources : Caldav_agenda.resource list;
}

val save :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  mirror:int ->
  collection:int ->
  window:Caldav_agenda.window ->
  metadata:Jsont.json ->
  complete:bool ->
  collection list ->
  snapshot
(** [save t ~actor ~room ~event ~mirror ~collection ~window ~metadata ~complete
     resources] commits one immutable snapshot. Keep at most 20 snapshots and 64
    MiB. A snapshot is capped at 16 MiB. Raw expanded data does not replace the
    mirror. *)

type entry = { id : int; resource : int; data : Jsont.json }

val page : t -> actor:string -> snapshot:int -> offset:int -> entry list

val read :
  t ->
  actor:string ->
  snapshot:int ->
  resource:int ->
  offset:int ->
  string * int
(** [read t ~actor ~snapshot ~resource ~offset] reads up to 4096 bytes of the
    original expanded calendar response, with the full byte length. *)
