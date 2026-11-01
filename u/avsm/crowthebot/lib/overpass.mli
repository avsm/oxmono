type request = {
  latitude : float;
  longitude : float;
  radius : int;
  tag : string option;
  value : string option;
}

type result = { features : Jsont.json list; limited : bool }
type t

val create :
  config:Owntracks_config.overpass ->
  fetch:Fetch.plain ->
  clock:_ Eio.Time.Mono.t ->
  t
(** [create ~config ~fetch ~clock] confines unauthenticated map queries to the
    configured interpreter, with a 20-second deadline and 1 MiB response cap.
    Pass an HTTP capability without Recorder or MQTT credentials. *)

val resolve : t -> request -> result
(** [resolve t request] finds administrative areas and nearby OSM features.
    Queries have a radius at most 2000 metres. Optional tags use exact matching.
    Feature centres and proximity do not prove a person's address. *)
