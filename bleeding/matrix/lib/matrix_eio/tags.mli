(** tags — the per-user, per-room labels of the Room Tagging module, raising
    instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Tags} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces. *)

(** {1 Well-known tag names} *)

val favourite : string
(** [favourite] is {!Matrix_client.Tags.favourite}. *)

val low_priority : string
(** [low_priority] is {!Matrix_client.Tags.low_priority}. *)

val server_notice : string
(** [server_notice] is {!Matrix_client.Tags.server_notice}. *)

(** {1 Reading and writing tags} *)

val get :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (string * float option) list
(** [get c ~user_id ~room_id] is {!Matrix_client.Tags.get} with the result
    unwrapped. *)

val set :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  tag:string ->
  ?order:float ->
  unit ->
  unit
(** [set c ~user_id ~room_id ~tag ()] is {!Matrix_client.Tags.set} with the
    result unwrapped. *)

val remove :
  Client.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  tag:string ->
  unit
(** [remove c ~user_id ~room_id ~tag] is {!Matrix_client.Tags.remove} with the
    result unwrapped. *)

(** {1 Tagging as the logged-in user} *)

val set_favourite :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  favourite:bool ->
  ?order:float ->
  unit ->
  unit
(** [set_favourite c ~room_id ~favourite ()] is
    {!Matrix_client.Tags.set_favourite} with the result unwrapped. *)

val set_low_priority :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  low_priority:bool ->
  ?order:float ->
  unit ->
  unit
(** [set_low_priority c ~room_id ~low_priority ()] is
    {!Matrix_client.Tags.set_low_priority} with the result unwrapped. *)
