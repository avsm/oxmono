type t
(** Typed location state, separate from conversational memory. *)

type point = {
  latitude : float;
  longitude : float;
  accuracy : float option;
  recorded_at : float;
  reported_at : float option;
  ssid : string option;
  bssid : string option;
  conn : string option;
}

type link = {
  person : string;
  connection : string;
  user : string;
  device : string;
  actor : string;
  room : string;
  event : string;
  attached_at : string;
  point : point option;
  checked_at : string option;
}

val init : Sqlite3_eio.t -> unit

val create :
  db:Sqlite3_eio.t ->
  mutex:Eio.Mutex.t ->
  admin:string ->
  now:(unit -> float) ->
  timestamp:(float -> string) ->
  t

val authorize : t -> actor:string -> unit
val validate_label : string -> unit
val valid_point : now:float -> point -> bool

val report_time : point -> float
(** [report_time point] is the report construction time, falling back to its fix
    timestamp when the device omitted [created_at]. *)

val attach :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  person:string ->
  connection:string ->
  user:string ->
  device:string ->
  link

val get : t -> actor:string -> person:string -> link option
val list : t -> actor:string -> after:string -> link list

val update : t -> actor:string -> link -> point option -> link
(** [update t ~actor link point] records a completed poll only if the link still
    matches, retaining the last position when there is no newer report. Wi-Fi
    fields belong to that report. A newer report without them clears the cached
    Wi-Fi data. *)

val detach : t -> actor:string -> person:string -> bool
