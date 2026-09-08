(** tags — the per-user, per-room labels of the Room Tagging module, Matrix 1.0.

    - [GET /_matrix/client/v3/user/{userId}/rooms/{roomId}/tags]
    - [PUT /_matrix/client/v3/user/{userId}/rooms/{roomId}/tags/{tag}]
    - [DELETE /_matrix/client/v3/user/{userId}/rooms/{roomId}/tags/{tag}]

    A tag carries an order, a number in [0.0, 1.0] that sorts the rooms holding
    it, smaller first. A room tagged without an order sorts after the rooms that
    have one. Nothing else the server stores under a tag is modelled. *)

(** {1 Well-known tag names} *)

val favourite : string
(** [favourite] is ["m.favourite"], marking the room as a favourite. *)

val low_priority : string
(** [low_priority] is ["m.lowpriority"], sorting the room to the bottom. *)

val server_notice : string
(** [server_notice] is ["m.server_notice"], marking a room that carries server
    notices. *)

(** {1 Reading and writing tags} *)

val get :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ((string * float option) list, Error.t) result
(** [get t ~user_id ~room_id] is
    [GET /_matrix/client/v3/user/{userId}/rooms/{roomId}/tags] (Matrix 1.0), as
    the tag names paired with their order and sorted by name. *)

val set :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  tag:string ->
  ?order:float ->
  unit ->
  (unit, Error.t) result
(** [set t ~user_id ~room_id ~tag ()] is
    [PUT /_matrix/client/v3/user/{userId}/rooms/{roomId}/tags/{tag}] (Matrix
    1.0). [order] defaults to absent, which leaves the tag unordered. Setting a
    tag that is already set replaces its order. *)

val remove :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  tag:string ->
  (unit, Error.t) result
(** [remove t ~user_id ~room_id ~tag] is
    [DELETE /_matrix/client/v3/user/{userId}/rooms/{roomId}/tags/{tag}] (Matrix
    1.0). *)

(** {1 Tagging as the logged-in user} *)

val set_favourite :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  favourite:bool ->
  ?order:float ->
  unit ->
  (unit, Error.t) result
(** [set_favourite t ~room_id ~favourite ()] tags the room {!favourite} when
    [favourite] is [true] and removes that tag when it is [false]. [order] is as
    in {!set} and is ignored when the tag is being removed.

    Fails with {!Error.No_session} when the client carries none. *)

val set_low_priority :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  low_priority:bool ->
  ?order:float ->
  unit ->
  (unit, Error.t) result
(** [set_low_priority t ~room_id ~low_priority ()] tags the room {!low_priority}
    when [low_priority] is [true] and removes that tag when it is [false].
    [order] is as in {!set} and is ignored when the tag is being removed.

    Fails with {!Error.No_session} when the client carries none. *)
