(** report — reporting content to the homeserver administrator, raising instead
    of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Report} returns [Error e]. A return says the report was
    accepted and nothing about whether the target exists. *)

val event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?reason:string ->
  ?score:int ->
  unit ->
  unit
(** [event c ~room_id ~event_id ()] is {!Matrix_client.Report.event} with the
    result unwrapped. The deprecated [score] argument is accepted for source
    compatibility but ignored. *)

val room :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?reason:string ->
  unit ->
  unit
(** [room c ~room_id ()] is {!Matrix_client.Report.room} with the result
    unwrapped. *)

val user :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  unit
(** [user c ~user_id ()] is {!Matrix_client.Report.user} with the result
    unwrapped. *)
