(** MSC1763 message-retention policies. *)

type policy = Matrix_proto.Event.Room_retention_content.t
(** A room policy, with optional minimum and maximum lifetimes in milliseconds.
    {!val-policy} rejects negative values and a minimum greater than its
    maximum. The JSON codecs apply the same validation and report malformed
    values as a decoding error. {!effective_policy} applies the server's
    independent clamps and resolves a remaining min/max conflict in favour of
    the maximum. *)

type lifetime_limits = {
  min_lifetime : int64 option;
  max_lifetime : int64 option;
}

type limits = {
  min_lifetime : lifetime_limits option;
  max_lifetime : lifetime_limits option;
}

type configuration = { limits : limits; policies : (string * policy) list }
(** The server's global limits and policies. The ["*"] policy is the default;
    other keys are room-id overrides. *)

val policy : ?min_lifetime:int64 -> ?max_lifetime:int64 -> unit -> policy
val policy_min_lifetime : policy -> int64 option
val policy_max_lifetime : policy -> int64 option
val policy_jsont : policy Jsont.t
val lifetime_limits_jsont : lifetime_limits Jsont.t
val limits_jsont : limits Jsont.t
val configuration_jsont : configuration Jsont.t

val get_configuration : Client.t -> (configuration, Error.t) result
(** Fetches
    [GET /_matrix/client/unstable/org.matrix.msc1763/retention/configuration].
*)

val get_room_policy :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (policy option, Error.t) result
(** Reads the stable [m.room.retention] state, falling back to the MSC1763
    spelling when the stable event is absent. *)

val set_room_policy :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  policy ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** Sets the stable [m.room.retention] state event. *)

val effective_policy :
  room_id:Matrix_proto.Id.Room_id.t ->
  room_policy:policy option ->
  configuration ->
  policy option
(** Applies MSC1763 precedence and independent min/max clamping. A server
    per-room override wins; without room state, the ["*"] policy is used.
    Missing room values fall back to the corresponding limit's minimum, and a
    resulting minimum is capped at the resulting maximum. *)

val effective :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (policy option, Error.t) result
(** Fetches the server configuration and combines it with room state. If the
    endpoint is unsupported ([M_UNRECOGNIZED], [M_NOT_FOUND], 404 or 501),
    [None] is returned, matching the Rust SDK: an unsupported homeserver does
    not provide a server retention policy. *)
