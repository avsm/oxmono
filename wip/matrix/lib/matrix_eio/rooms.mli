(** rooms — creating rooms, joining them and managing their members, raising
    instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Rooms} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces. *)

(** {1 Creation} *)

(** The join rules and power levels the server applies at creation. *)
type preset = Matrix_client.Rooms.preset =
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
  Matrix_proto.Id.Room_id.t
(** [create c ()] is {!Matrix_client.Rooms.create} with the result unwrapped. *)

(** {1 Joining and leaving} *)

val join :
  Client.t ->
  room_id_or_alias:Directory.room_id_or_alias ->
  ?via:string list ->
  ?reason:string ->
  unit ->
  Matrix_proto.Id.Room_id.t
(** [join c ~room_id_or_alias ()] is {!Matrix_client.Rooms.join} with the result
    unwrapped. *)

val knock :
  Client.t ->
  room_id_or_alias:Directory.room_id_or_alias ->
  ?reason:string ->
  ?via:string list ->
  unit ->
  Matrix_proto.Id.Room_id.t
(** [knock c ~room_id_or_alias ()] is {!Matrix_client.Rooms.knock} with the
    result unwrapped. *)

val leave :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?reason:string ->
  unit ->
  unit
(** [leave c ~room_id ()] is {!Matrix_client.Rooms.leave} with the result
    unwrapped. *)

val forget : Client.t -> room_id:Matrix_proto.Id.Room_id.t -> unit
(** [forget c ~room_id] is {!Matrix_client.Rooms.forget} with the result
    unwrapped. *)

(** {1 Membership} *)

val invite :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  unit
(** [invite c ~room_id ~user_id ()] is {!Matrix_client.Rooms.invite} with the
    result unwrapped. *)

val kick :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  unit
(** [kick c ~room_id ~user_id ()] is {!Matrix_client.Rooms.kick} with the result
    unwrapped. *)

val ban :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  unit
(** [ban c ~room_id ~user_id ()] is {!Matrix_client.Rooms.ban} with the result
    unwrapped. *)

val unban :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  unit
(** [unban c ~room_id ~user_id ()] is {!Matrix_client.Rooms.unban} with the
    result unwrapped. *)

(** {1 Members and joined rooms} *)

val get_joined_rooms : Client.t -> Matrix_proto.Id.Room_id.t list
(** [get_joined_rooms c] is {!Matrix_client.Rooms.get_joined_rooms} with the
    result unwrapped. *)

type member = Matrix_client.Rooms.member = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
  avatar_url : Matrix_client.Media.Mxc.t option;
  membership : Matrix_proto.Event.Membership.t;
}
(** A member of a room, from that user's [m.room.member] event. *)

val get_members :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?membership:Matrix_proto.Event.Membership.t ->
  ?not_membership:Matrix_proto.Event.Membership.t ->
  unit ->
  member list
(** [get_members c ~room_id ()] is {!Matrix_client.Rooms.get_members} with the
    result unwrapped. *)

type joined_member = Matrix_client.Rooms.joined_member = {
  display_name : string option;
  avatar_url : Matrix_client.Media.Mxc.t option;
}
(** The profile of a joined member. *)

val get_joined_members :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (Matrix_proto.Id.User_id.t * joined_member) list
(** [get_joined_members c ~room_id] is {!Matrix_client.Rooms.get_joined_members}
    with the result unwrapped. *)

(** {1 Power levels} *)

val get_power_levels :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Event.Room_power_levels_content.t
(** [get_power_levels c ~room_id] is {!Matrix_client.Rooms.get_power_levels}
    with the result unwrapped. *)

val set_power_levels :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  power_levels:Matrix_proto.Event.Room_power_levels_content.t ->
  unit
(** [set_power_levels c ~room_id ~power_levels] is
    {!Matrix_client.Rooms.set_power_levels} with the result unwrapped. *)

val set_user_power_level :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  level:int ->
  unit
(** [set_user_power_level c ~room_id ~user_id ~level] is
    {!Matrix_client.Rooms.set_user_power_level} with the result unwrapped. It
    reads the levels, changes the one user and writes them back, so a concurrent
    edit of another user's level can be lost. *)

(** {1 Live locations} *)

val start_live_location_share :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  duration_millis:int64 ->
  ?description:string ->
  ?timestamp:Matrix_proto.Event.Timestamp.t ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** Sets the caller's [org.matrix.msc3672.beacon_info] state, including its
    current timestamp and [m.self] asset. *)

val stop_live_location_share :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** Stops the caller's current share, preserving its other fields. *)

val send_location_beacon :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  geo_uri:string ->
  ?timestamp:Matrix_proto.Event.Timestamp.t ->
  ?description:string ->
  ?now:(unit -> Matrix_proto.Event.Timestamp.t) ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** Sends an [m.reference]-related location beacon. *)

(** {1 Upgrades and aliases} *)

val upgrade :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  new_version:string ->
  ?additional_creators:Matrix_proto.Id.User_id.t list ->
  unit ->
  Matrix_proto.Id.Room_id.t
(** [upgrade c ~room_id ~new_version ()] is {!Matrix_client.Rooms.upgrade} with
    the result unwrapped. *)

val get_aliases :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Id.Room_alias.t list
(** [get_aliases c ~room_id] is {!Matrix_client.Rooms.get_aliases} with the
    result unwrapped. *)

(** {1 Timestamps} *)

type timestamp_event = Matrix_client.Rooms.timestamp_event = {
  event_id : Matrix_proto.Id.Event_id.t;
  origin_server_ts : Matrix_proto.Event.Timestamp.t;
}
(** The event closest to a timestamp. *)

val timestamp_to_event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ts:Matrix_proto.Event.Timestamp.t ->
  dir:Matrix_proto.Common.Direction.t ->
  timestamp_event
(** [timestamp_to_event c ~room_id ~ts ~dir] is
    {!Matrix_client.Rooms.timestamp_to_event} with the result unwrapped. *)
