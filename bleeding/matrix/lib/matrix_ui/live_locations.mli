(** Live-location shares projected from durable room state and the event cache.

    The projection has no persistence of its own. With [~sw], cached beacon
    events update it automatically until {!close}; call {!refresh_state} after a
    sync changes room state and {!refresh_time} at a clock boundary. *)

type last_location = {
  location : Matrix_proto.Event.Beacon_content.location;
  timestamp : Matrix_proto.Event.Timestamp.t;
}
(** The newest cached beacon for a share, if one has arrived. *)

type share = {
  user_id : Matrix_proto.Id.User_id.t;
  beacon_id : Matrix_proto.Id.Event_id.t;
  beacon_info : Matrix_proto.Event.Beacon_info_content.t;
  last_location : last_location option;
}
(** One currently active live-location share. *)

type t
(** A room's live-location projection. *)

val create :
  event_cache:Event_cache.t ->
  state:Matrix_client.Base_client.state ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?sw:Eio.Switch.t ->
  ?now:(unit -> int64) ->
  unit ->
  t
(** Builds a projection seeded immediately from current durable state and cached
    beacon events. When [sw] is supplied, it subscribes to later cache changes
    until the switch closes or {!close} is called; without [sw], the caller
    drives it through {!refresh}. [now] returns milliseconds since the Unix
    epoch and defaults to the local wall clock. *)

val shares : t -> share Observable.List.t
(** The active shares, sorted deterministically by user id. *)

val refresh : t -> Matrix_client.Base_client.state -> unit
(** Recomputes state and cached locations, replacing the state used by later
    {!refresh_time} calls. *)

val refresh_state : t -> Matrix_client.Base_client.state -> unit
(** Alias for {!refresh}. *)

val refresh_time : t -> unit
(** Recomputes expiry using the injected clock without changing room state. *)

val close : t -> unit
(** Unsubscribes from the cache and stops automatic beacon updates. Idempotent.
*)

val user_id : share -> Matrix_proto.Id.User_id.t
val beacon_id : share -> Matrix_proto.Id.Event_id.t
val last_location : share -> last_location option
