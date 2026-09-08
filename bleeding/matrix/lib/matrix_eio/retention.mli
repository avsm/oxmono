(** Eio wrappers for MSC1763 retention operations. *)

type policy = Matrix_client.Retention.policy
type lifetime_limits = Matrix_client.Retention.lifetime_limits
type limits = Matrix_client.Retention.limits
type configuration = Matrix_client.Retention.configuration

val get_configuration : Client.t -> configuration

val get_room_policy :
  Client.t -> room_id:Matrix_proto.Id.Room_id.t -> policy option

val set_room_policy :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  policy ->
  Matrix_proto.Id.Event_id.t

val effective : Client.t -> room_id:Matrix_proto.Id.Room_id.t -> policy option
