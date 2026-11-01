(** calls — VoIP call signalling.

    A Matrix call is a WebRTC session negotiated through ordinary timeline
    events. An invite carries an SDP offer, an answer carries the SDP reply, ICE
    candidates go out as they are gathered, and a hangup or a reject ends it.
    The functions here send those events and fetch the TURN credentials the
    media path needs. Nothing here speaks WebRTC, so the descriptions and the
    candidates come from the caller's own stack.

    Version 1 of the specification's call model, which everything here defaults
    to, requires a party identifier so that a call answered on two devices can
    be told apart. {!generate_call_id} and {!generate_party_id} mint them. *)

(** {1 Identifiers} *)

type call_id = private string
(** The identifier every event of one call carries. *)

type party_id = private string
(** The identifier of one device within a call. *)

val call_id_of_string : string -> call_id
(** [call_id_of_string s] is [s] read as a call identifier, for one taken from a
    received event. *)

val party_id_of_string : string -> party_id
(** [party_id_of_string s] is [s] read as a party identifier, for one taken from
    a received event. *)

val generate_call_id : Client.t -> call_id
(** [generate_call_id t] is a fresh call identifier drawn from [t]'s randomness,
    unique to one call for as long as it lasts. *)

val generate_party_id : Client.t -> party_id
(** [generate_party_id t] is a fresh party identifier drawn from [t]'s
    randomness. *)

(** {1 Signalling}

    Each of these sends one event to the room and is its event identifier. Every
    peer in the room sees them, so a call in a large room is signalled to
    everyone in it. [version] is the call protocol version and defaults to [1]
    throughout. *)

val send_invite :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  call_id:call_id ->
  party_id:party_id ->
  offer:Matrix_proto.Event.Sdp.t ->
  lifetime:int ->
  ?version:int ->
  ?invitee:Matrix_proto.Id.User_id.t ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_invite t ~room_id ~call_id ~party_id ~offer ~lifetime ()] sends
    [m.call.invite].

    [lifetime] is how long the offer stands, in milliseconds. A peer that sees
    the invite later than that must ignore it, so it should be short. [invitee]
    rings only that user, in a room with more than two members, and defaults to
    absent, ringing everyone. *)

val send_candidates :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  call_id:call_id ->
  party_id:party_id ->
  candidates:Matrix_proto.Event.Call_candidates_content.candidate list ->
  ?version:int ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_candidates t ~room_id ~call_id ~party_id ~candidates ()] sends
    [m.call.candidates]. Candidates may be sent in several events as they are
    gathered, before or after the answer. *)

val send_answer :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  call_id:call_id ->
  party_id:party_id ->
  answer:Matrix_proto.Event.Sdp.t ->
  ?version:int ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_answer t ~room_id ~call_id ~party_id ~answer ()] sends
    [m.call.answer], accepting an invite. The first answer wins, and a device
    that sees another party's answer to a call it also answered must hang up. *)

val send_hangup :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  call_id:call_id ->
  party_id:party_id ->
  ?reason:Matrix_proto.Event.Hangup_reason.t ->
  ?version:int ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_hangup t ~room_id ~call_id ~party_id ()] sends [m.call.hangup], ending
    a call this device is part of.

    [reason] is why it ended, and defaults to absent, which the specification
    reads as the user having hung up. *)

val send_reject :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  call_id:call_id ->
  party_id:party_id ->
  ?version:int ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_reject t ~room_id ~call_id ~party_id ()] sends [m.call.reject],
    declining an invite this device has not answered. Unlike a hangup it leaves
    the call ringing on the user's other devices. *)

(** {1 TURN} *)

type turn_server = {
  username : string;
  password : string;
  uris : string list;  (** [turn:] and [turns:] URIs to hand to WebRTC. *)
  ttl : int;  (** Seconds the credentials stay valid. *)
}
(** Credentials for the homeserver's TURN relays. *)

val turn_server_jsont : turn_server Jsont.t
(** [turn_server_jsont] is the JSON codec for {!turn_server}. *)

val get_turn_server : Client.t -> (turn_server, Error.t) result
(** [get_turn_server t] is [GET /_matrix/client/v3/voip/turnServer] (Matrix
    1.0). The credentials expire after [ttl] seconds, so a long call has to
    fetch them again. A homeserver with no TURN relay configured answers with an
    empty [uris]. *)
