(** presence — whether a user is around.

    Presence is server-wide rather than per-room, and a homeserver may have it
    turned off entirely, in which case every user reads as {!Offline} and
    {!set_presence} is accepted and ignored. *)

(** The three states the specification defines. *)
type presence_state =
  | Online  (** The user is at the client. *)
  | Offline  (** The user has no client connected. *)
  | Unavailable  (** The user is idle rather than gone. *)

val presence_state_to_string : presence_state -> string
(** [presence_state_to_string s] is the wire form of [s]. *)

val presence_state_of_string :
  string -> (presence_state, [> `Msg of string ]) result
(** [presence_state_of_string s] is the state [s] names. *)

val presence_state_jsont : presence_state Jsont.t
(** [presence_state_jsont] is the JSON codec for {!presence_state}. Decoding
    fails on a state the specification does not define. *)

type presence = {
  presence : presence_state;
  status_msg : string option;  (** Free text the user set alongside it. *)
  last_active_ago : int option;
      (** Milliseconds since the user was last active. *)
  currently_active : bool option;
      (** [true] while the server considers the user active, which stops
          [last_active_ago] from being meaningful. *)
}
(** A user's presence, as the server reports it. *)

val get_presence :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> (presence, Error.t) result
(** [get_presence t ~user_id] is
    [GET /_matrix/client/v3/presence/{userId}/status] (Matrix 1.0). A user who
    shares no room with the caller is [M_FORBIDDEN], and one the server does not
    know is [M_NOT_FOUND]. *)

val set_presence :
  Client.t ->
  presence:presence_state ->
  ?status_msg:string ->
  ?immediate:bool ->
  unit ->
  (unit, Error.t) result
(** [set_presence t ~presence ()] is
    [PUT /_matrix/client/v3/presence/{userId}/status] (Matrix 1.0) for the
    logged-in user. The client-owned default used by future sync requests is
    updated before the request. With [immediate=false], only that local default
    changes and no session is required; it defaults to [true]. A client with no
    session fails with {!Error.No_session} for an immediate request.

    [status_msg] is free text to show alongside the state for an immediate
    request. It defaults to absent, which leaves whatever was set before. The
    specification has no way to clear it other than an empty string. *)
