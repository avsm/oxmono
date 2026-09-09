type t
(** An initialized Recorder capability. It exposes no credentials or endpoint.
*)

val configuration : Tool_config.t

val initialize :
  fetch:Fetch.plain ->
  clock:_ Eio.Time.Mono.t ->
  now:(unit -> float) ->
  Jsont.json ->
  t

val users : t -> string list
val devices : t -> user:string -> string list
val latest : t -> user:string -> device:string -> Location_store.point option
