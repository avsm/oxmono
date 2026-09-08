(** account_data — the per-user key-value store a homeserver keeps.

    Account data is one JSON object per event type, held either for the whole
    account or for one room, and [/sync] delivers a copy of every change. It is
    where a client keeps its own settings, and what the specification builds
    [m.direct], the ignore list, room tags and secret storage on.

    Every call acts as the logged-in user. The user identifier comes from the
    client's session, and a client with no session fails with
    {!Error.No_session}. *)

(** {1 Reading and writing} *)

val get :
  Client.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  (Jsont.json, Error.t) result
(** [get t ~event_type] is
    [GET /_matrix/client/v3/user/{userId}/account_data/{type}] (Matrix 1.0). A
    type the user has never set is [M_NOT_FOUND]. *)

val set :
  Client.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  content:Jsont.json ->
  (unit, Error.t) result
(** [set t ~event_type ~content] is
    [PUT /_matrix/client/v3/user/{userId}/account_data/{type}] (Matrix 1.0).
    [content] replaces whatever was stored under [event_type]. *)

val get_room :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  (Jsont.json, Error.t) result
(** [get_room t ~room_id ~event_type] is
    [GET /_matrix/client/v3/user/{userId}/rooms/{roomId}/account_data/{type}]
    (Matrix 1.0). *)

val set_room :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  content:Jsont.json ->
  (unit, Error.t) result
(** [set_room t ~room_id ~event_type ~content] is
    [PUT /_matrix/client/v3/user/{userId}/rooms/{roomId}/account_data/{type}]
    (Matrix 1.0). *)

val set_marked_unread :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  unread:bool ->
  (unit, Error.t) result
(** [set_marked_unread t ~room_id ~unread] writes the stable [m.marked_unread]
    room account-data event with the given flag. *)

(** {1 Direct messages}

    Which rooms are direct messages is the user's own [m.direct] account data
    rather than a property of the room, so both calls below read and write it.
*)

val find_dm_rooms :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  (Matrix_proto.Id.Room_id.t list, Error.t) result
(** [find_dm_rooms t ~user_id] is the rooms [m.direct] records as direct
    messages with [user_id], and [[]] when there are none. The user may have
    left them, and nothing here checks. An [m.direct] member whose name is not a
    user identifier is skipped rather than failing the call. *)

val mark_as_dm :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (unit, Error.t) result
(** Add one room to one user's [m.direct] entry. An existing association makes
    no write. *)

val unmark_as_dm :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (unit, Error.t) result
(** Remove one room from one user's entry, dropping an empty entry. An absent
    association makes no write. *)

val mark_room_as_dm :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  users:Matrix_proto.Id.User_id.t list ->
  (unit, Error.t) result
(** Add [room_id] to every user's entry with one read and at most one write. *)

val unmark_room_as_dm :
  Client.t -> room_id:Matrix_proto.Id.Room_id.t -> (unit, Error.t) result
(** Remove [room_id] from every user's entry with one read and at most one
    write. *)

val get_or_create_dm :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?encrypted:bool ->
  unit ->
  (Matrix_proto.Id.Room_id.t, Error.t) result
(** [get_or_create_dm t ~user_id ()] is the first room {!find_dm_rooms} reports,
    or else a new invite-only room with [user_id] invited and [m.direct] updated
    to record it. A new room is returned even when the [m.direct] update fails,
    since the room exists either way.

    [encrypted] is passed to {!Rooms.create} for a room this call makes, and
    defaults to [false]. *)
