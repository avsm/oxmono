type t
(** An initialized Recorder capability. It exposes no credentials or endpoint.
*)

val configuration : Tool_config.t

val load_config : string -> Owntracks_config.t
(** [load_config path] reads a private OwnTracks TOML file in trusted startup or
    operator code. Errors never contain file contents. *)

type publish = Mqttz_config.t -> topic:string -> string -> unit
(** Publishes one message with the configured MQTT broker settings. *)

val initialize :
  ?publish:publish ->
  load:(string -> Owntracks_config.t) ->
  fetch:Fetch.plain ->
  clock:_ Eio.Time.Mono.t ->
  now:(unit -> float) ->
  Jsont.json ->
  t

val user : t -> string
val device : t -> string
val permits : t -> user:string -> device:string -> bool

val can_request : t -> bool
(** [can_request t] is [true] when [t] was given a publisher. *)

val request_fix : t -> unit
(** [request_fix t] asks the tracker for a new fix by publishing an OwnTracks
    [reportLocation] command to [owntracks/USER/DEVICE/cmd], with the device
    identifier as the config writes it. The phone answers only if it is
    connected and allows remote commands, so a later {!latest} may still be
    the old fix. Raises [Invalid_argument] when [t] cannot publish or the
    broker refuses within 15 seconds. *)

val fresh_fix :
  ?wait:float -> ?every:float -> t -> after:float -> Location_store.point option
(** [fresh_fix t ~after] calls {!request_fix}, then checks the Recorder every
    [every] seconds, 3 by default, for a fix recorded after [after]. It gives
    up after [wait] seconds, 30 by default, and returns [None]. *)

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
