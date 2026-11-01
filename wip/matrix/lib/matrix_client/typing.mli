(** typing — telling a room that the user is typing.

    A typing notification is ephemeral. The server broadcasts it to the room and
    forgets it, so nothing about it survives in the timeline. *)

val users_of_content :
  Jsont.json -> (Matrix_proto.Id.User_id.t list, Error.t) result
(** [users_of_content content] decodes the [user_ids] member of an [m.typing]
    event's content. The member is required, each value must be a valid user
    identifier, and malformed content is reported as {!Error.Json_error}. *)

val set_typing :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  typing:bool ->
  ?timeout:int ->
  unit ->
  (unit, Error.t) result
(** [set_typing t ~room_id ~typing ()] is
    [PUT /_matrix/client/v3/rooms/{roomId}/typing/{userId}] (Matrix 1.0) for the
    logged-in user. A client with no session fails with {!Error.No_session}.

    The server forgets a [true] on its own after [timeout], so a client that
    keeps typing has to repeat the call. Sending [false] ends it at once.

    [timeout] is how long the notification stands, in milliseconds, and is
    meaningful only with [~typing:true]. It defaults to absent, sending no
    [timeout] member, and the server then applies its own default of 30 seconds.
*)
