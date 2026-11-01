(** typing — telling a room that the user is typing, raising instead of
    returning.

    {!set_typing} raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Typing.set_typing} returns [Error e]. *)

val set_typing :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  typing:bool ->
  ?timeout:int ->
  unit ->
  unit
(** [set_typing c ~room_id ~typing ()] is {!Matrix_client.Typing.set_typing}
    with the result unwrapped. *)
