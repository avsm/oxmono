type t
(** An initialized Recorder capability. It exposes no credentials or endpoint.
*)

val configuration : Tool_config.t

val load_config : string -> Owntracks_config.t
(** [load_config path] reads a private OwnTracks TOML file in trusted startup or
    operator code. Errors never contain file contents. *)

val initialize :
  load:(string -> Owntracks_config.t) ->
  fetch:Fetch.plain ->
  clock:_ Eio.Time.Mono.t ->
  now:(unit -> float) ->
  Jsont.json ->
  t

val user : t -> string
val device : t -> string
val permits : t -> user:string -> device:string -> bool

val latest : t -> Location_store.point option
(** [latest t] retrieves only the tracker selected during initialization. The
    latest report includes optional Wi-Fi context and its construction time. The
    returned capability retains neither [load] nor file access. *)

val history : t -> from:float -> until:float -> Location_store.point list
(** [history t ~from ~until] returns chronological, deduplicated fixes in the
    inclusive interval, within the configured recent lookback and not in the
    future. Tracker and date bounds apply to redirects as well. *)

val resolve : t -> Overpass.request -> Overpass.result
(** [resolve t request] queries the connection's configured Overpass endpoint
    without attaching Recorder credentials. *)
