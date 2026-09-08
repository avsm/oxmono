(** report — reporting content to the homeserver administrator.

    - [POST /_matrix/client/v3/rooms/{roomId}/report/{eventId}] (Matrix 1.0)
    - [POST /_matrix/client/v3/rooms/{roomId}/report] (Matrix 1.13), or
      MSC4151's unstable path
    - [POST /_matrix/client/v3/users/{userId}/report] (Matrix 1.14), or
      MSC4260's unstable path

    All three answer with an empty object. A successful result says the report
    was accepted and nothing about whether the target exists. *)

val event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?reason:string ->
  ?score:int ->
  unit ->
  (unit, Error.t) result
(** [event t ~room_id ~event_id ()] is
    [POST /_matrix/client/v3/rooms/{roomId}/report/{eventId}] (Matrix 1.0).
    [reason] is free text shown to the administrator and defaults to absent.
    [score] is deprecated and ignored; it is retained only for source
    compatibility and is never encoded. The current Matrix request body has no
    score member. *)

val room :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?reason:string ->
  unit ->
  (unit, Error.t) result
(** [room t ~room_id ()] reports a whole room, selecting the Matrix 1.13 stable
    path or MSC4151's unstable path from [/versions]. [reason] is as in
    {!event}. *)

val user :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  unit ->
  (unit, Error.t) result
(** [user t ~user_id ()] selects Matrix 1.14's stable path or MSC4260's unstable
    path from [/versions]. [reason] is as in {!event}. *)
