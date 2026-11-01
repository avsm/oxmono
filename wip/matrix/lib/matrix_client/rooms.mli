(** rooms — creating rooms, joining them and managing their members.

    A room is addressed by its identifier everywhere but {!join} and {!knock},
    which also take an alias. Nothing here is cached. Every call is a request,
    and state read through {!get_power_levels} may already be stale by the time
    it returns.

    A reply naming a room, a user or an event that is not a well-formed
    identifier is an {!Error.Json_error} rather than an entry quietly left out
    of the result. *)

(** {1 Creation} *)

(** The join rules and power levels the server applies at creation. *)
type preset =
  | Private_chat  (** Invite-only. Invitees join at power level 0. *)
  | Public_chat  (** Anyone may join. *)
  | Trusted_private_chat
      (** Invite-only, and every invitee is made an administrator. *)

val create :
  Client.t ->
  ?name:string ->
  ?topic:string ->
  ?visibility:Matrix_proto.Common.Visibility.t ->
  ?preset:preset ->
  ?room_alias_local_part:string ->
  ?invite:Matrix_proto.Id.User_id.t list ->
  ?is_direct:bool ->
  ?room_type:string ->
  ?encrypted:bool ->
  unit ->
  (Matrix_proto.Id.Room_id.t, Error.t) result
(** [create t ()] is [POST /_matrix/client/v3/createRoom] (Matrix 1.0) and is
    the new room's identifier.

    [name] defaults to absent, leaving the room unnamed, and [topic] likewise.
    [visibility] defaults to absent, which the server reads as
    {!Matrix_proto.Common.Visibility.Private}. [preset] defaults to absent,
    which the server derives from [visibility]. [invite] defaults to the empty
    list.

    [room_alias_local_part] is the part of an alias before the colon, to which
    the server appends its own name. It defaults to absent, creating no alias.
    One already in use is [M_ROOM_IN_USE].

    [is_direct] marks the room as a direct message in the {e invitees'}
    [m.direct]. It does not touch the caller's own, which
    {!Account_data.get_or_create_dm} maintains. It defaults to [false].

    [room_type] is ["m.space"] for a space, and defaults to absent, making an
    ordinary room.

    [encrypted] puts an [m.room.encryption] event in the room's initial state,
    so that no event in the room predates encryption. It defaults to [false],
    and encryption cannot be turned off once on. *)

(** {1 Joining and leaving} *)

val join :
  Client.t ->
  room_id_or_alias:Directory.room_id_or_alias ->
  ?via:string list ->
  ?reason:string ->
  unit ->
  (Matrix_proto.Id.Room_id.t, Error.t) result
(** [join t ~room_id_or_alias ()] is
    [POST /_matrix/client/v3/join/{roomIdOrAlias}] (Matrix 1.0) and is the
    room's identifier, which differs from the argument when an alias was used.

    An invite-only room the user was not invited to is [M_FORBIDDEN]. A room
    version the homeserver will not join is [M_UNSUPPORTED_ROOM_VERSION].

    [via] names servers already in the room, sent as [server_name] parameters.
    It defaults to the empty list. Joining a remote room by identifier needs at
    least one, and joining by alias needs none. [reason] is free text recorded
    in the membership event and defaults to absent. *)

val knock :
  Client.t ->
  room_id_or_alias:Directory.room_id_or_alias ->
  ?reason:string ->
  ?via:string list ->
  unit ->
  (Matrix_proto.Id.Room_id.t, Error.t) result
(** [knock t ~room_id_or_alias ()] is
    [POST /_matrix/client/v3/knock/{roomIdOrAlias}] (MSC2403, stable since
    Matrix 1.1) and is the room's identifier. It asks a room whose join rule is
    [knock] to admit the user. Someone already in the room then invites them or
    does not.

    A room whose join rule is not [knock] is [M_FORBIDDEN], as is a user already
    banned from it.

    [reason] is free text shown alongside the request and defaults to absent.
    [via] names servers to try, sent as [server_name] parameters, and defaults
    to the empty list. *)

val leave :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?reason:string ->
  unit ->
  (unit, Error.t) result
(** [leave t ~room_id ()] is [POST /_matrix/client/v3/rooms/{roomId}/leave]
    (Matrix 1.0). It also rejects an outstanding invite. The room stays in the
    [leave] section of [/sync] until {!forget}. [reason] defaults to absent. *)

val forget :
  Client.t -> room_id:Matrix_proto.Id.Room_id.t -> (unit, Error.t) result
(** [forget t ~room_id] is [POST /_matrix/client/v3/rooms/{roomId}/forget]
    (Matrix 1.0), which drops the room from [/sync] entirely. A room the user
    has not left first is [M_UNKNOWN]. *)

(** {1 Membership}

    Each of the four below is [M_FORBIDDEN] when the caller's power level is
    below what the room requires for the action, or not above the target's own.
    [reason] is free text recorded in the membership event, and defaults to
    absent throughout. *)

val invite :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  (unit, Error.t) result
(** [invite t ~room_id ~user_id ()] is
    [POST /_matrix/client/v3/rooms/{roomId}/invite] (Matrix 1.0). *)

val kick :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  (unit, Error.t) result
(** [kick t ~room_id ~user_id ()] is
    [POST /_matrix/client/v3/rooms/{roomId}/kick] (Matrix 1.0). The user may be
    invited again. *)

val ban :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  (unit, Error.t) result
(** [ban t ~room_id ~user_id ()] is [POST /_matrix/client/v3/rooms/{roomId}/ban]
    (Matrix 1.0), which removes the user and stops them rejoining until
    {!unban}. *)

val unban :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  (unit, Error.t) result
(** [unban t ~room_id ~user_id ()] is
    [POST /_matrix/client/v3/rooms/{roomId}/unban] (Matrix 1.0). It lifts the
    ban, and does not invite. *)

(** {1 Members and joined rooms} *)

val get_joined_rooms :
  Client.t -> (Matrix_proto.Id.Room_id.t list, Error.t) result
(** [get_joined_rooms t] is [GET /_matrix/client/v3/joined_rooms] (Matrix 1.0).
*)

type member = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;  (** The name used in this room only. *)
  avatar_url : Media.Mxc.t option;
  membership : Matrix_proto.Event.Membership.t;
}
(** A member, from that user's [m.room.member] event. *)

val get_members :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?membership:Matrix_proto.Event.Membership.t ->
  ?not_membership:Matrix_proto.Event.Membership.t ->
  unit ->
  (member list, Error.t) result
(** [get_members t ~room_id ()] is
    [GET /_matrix/client/v3/rooms/{roomId}/members] (Matrix 1.0).
    {!get_joined_members} is a lighter reply when only joined members are
    wanted.

    [membership] keeps only members in that state and [not_membership] drops
    members in that state. Both default to absent, keeping every member. *)

type joined_member = {
  display_name : string option;
  avatar_url : Media.Mxc.t option;
}
(** A joined member, as [joined_members] reports them. *)

val get_joined_members :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ((Matrix_proto.Id.User_id.t * joined_member) list, Error.t) result
(** [get_joined_members t ~room_id] is
    [GET /_matrix/client/v3/rooms/{roomId}/joined_members] (Matrix 1.0), a
    lighter alternative to {!get_members} that reports only the joined members
    and only their profile. *)

(** {1 Power levels} *)

val get_power_levels :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (Matrix_proto.Event.Room_power_levels_content.t, Error.t) result
(** [get_power_levels t ~room_id] is the room's [m.room.power_levels] state
    event. {!Matrix_proto.Event.Room_power_levels_content.user_level} reads one
    user's level out of it. *)

val set_power_levels :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  power_levels:Matrix_proto.Event.Room_power_levels_content.t ->
  (unit, Error.t) result
(** [set_power_levels t ~room_id ~power_levels] writes the whole
    [m.room.power_levels] event, replacing what was there. A caller may not
    raise anyone, itself included, above its own level, nor change a user at or
    above it. Either is [M_FORBIDDEN]. *)

val set_user_power_level :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  level:int ->
  (unit, Error.t) result
(** [set_user_power_level t ~room_id ~user_id ~level] reads the current levels,
    changes the one user and writes them back, so a concurrent edit of another
    user's level can be lost. *)

(** {1 Live locations} *)

val start_live_location_share :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  duration_millis:int64 ->
  ?description:string ->
  ?timestamp:Matrix_proto.Event.Timestamp.t ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** Sets [org.matrix.msc3672.beacon_info] under the caller's user id. The
    timestamp defaults to now and the asset is [m.self]. *)

val stop_live_location_share :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** Stops the caller's current share, preserving all fields except [live]. *)

val send_location_beacon :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  geo_uri:string ->
  ?timestamp:Matrix_proto.Event.Timestamp.t ->
  ?description:string ->
  ?now:(unit -> Matrix_proto.Event.Timestamp.t) ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** Sends a beacon with an [m.reference] to the current beacon-info event;
    elapsed shares are rejected before sending. *)

(** {1 Upgrades and aliases} *)

val upgrade :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  new_version:string ->
  ?additional_creators:Matrix_proto.Id.User_id.t list ->
  unit ->
  (Matrix_proto.Id.Room_id.t, Error.t) result
(** [upgrade t ~room_id ~new_version ()] is
    [POST /_matrix/client/v3/rooms/{roomId}/upgrade] (Matrix 1.0) and is the
    replacement room's identifier. It replaces the room with a new one at
    [new_version], leaving a tombstone behind.

    [additional_creators] names extra users to make creators of the replacement
    room (Matrix 1.16), and defaults to the empty list. *)

val get_aliases :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (Matrix_proto.Id.Room_alias.t list, Error.t) result
(** [get_aliases t ~room_id] is [GET /_matrix/client/v3/rooms/{roomId}/aliases]
    (Matrix 1.0), the aliases this homeserver holds for the room. Aliases on
    other servers are not listed. *)

(** {1 Timestamps} *)

type timestamp_event = {
  event_id : Matrix_proto.Id.Event_id.t;
  origin_server_ts : Matrix_proto.Event.Timestamp.t;
}
(** The event closest to a timestamp. *)

val timestamp_to_event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ts:Matrix_proto.Event.Timestamp.t ->
  dir:Matrix_proto.Common.Direction.t ->
  (timestamp_event, Error.t) result
(** [timestamp_to_event t ~room_id ~ts ~dir] is
    [GET /_matrix/client/v1/rooms/{roomId}/timestamp_to_event] (Matrix 1.6). It
    is the closest event at or after [ts] under
    {!Matrix_proto.Common.Direction.Forward}, and the closest at or before [ts]
    under {!Matrix_proto.Common.Direction.Backward}. No event in that direction
    is [M_NOT_FOUND]. *)
