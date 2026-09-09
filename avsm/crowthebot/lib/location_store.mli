type t
(** Typed location state, separate from conversational memory. *)

type point = {
  latitude : float;
  longitude : float;
  accuracy : float option;
  recorded_at : float;
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
    matches, retaining the last position when there is no newer fix. *)

val detach : t -> actor:string -> person:string -> bool
