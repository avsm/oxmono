(** account_data — the per-user key-value store a homeserver keeps, raising
    instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Account_data} returns [Error e]. That module documents what
    each call does, which endpoint it uses and which errors it produces. *)

(** {1 Reading and writing} *)

val get : Client.t -> event_type:Matrix_proto.Event.Event_type.t -> Jsont.json
(** [get c ~event_type] is {!Matrix_client.Account_data.get} with the result
    unwrapped. *)

val set :
  Client.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  content:Jsont.json ->
  unit
(** [set c ~event_type ~content] is {!Matrix_client.Account_data.set} with the
    result unwrapped. *)

val get_room :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  Jsont.json
(** [get_room c ~room_id ~event_type] is {!Matrix_client.Account_data.get_room}
    with the result unwrapped. *)

val set_room :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  content:Jsont.json ->
  unit
(** [set_room c ~room_id ~event_type ~content] is
    {!Matrix_client.Account_data.set_room} with the result unwrapped. *)

val set_marked_unread :
  Client.t -> room_id:Matrix_proto.Id.Room_id.t -> unread:bool -> unit
(** [set_marked_unread c ~room_id ~unread] is
    {!Matrix_client.Account_data.set_marked_unread} with the result unwrapped.
*)

(** {1 Direct messages} *)

val find_dm_rooms :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  Matrix_proto.Id.Room_id.t list
(** [find_dm_rooms c ~user_id] is {!Matrix_client.Account_data.find_dm_rooms}
    with the result unwrapped. *)

val get_or_create_dm :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?encrypted:bool ->
  unit ->
  Matrix_proto.Id.Room_id.t
(** [get_or_create_dm c ~user_id ()] is
    {!Matrix_client.Account_data.get_or_create_dm} with the result unwrapped. *)
