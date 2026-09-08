(** Raising wrappers for incoming knock requests. *)

type t = Matrix_client.Knock_requests.t

val list : Matrix_client.Store.t -> room_id:Matrix_proto.Id.Room_id.t -> t list
val all : Matrix_client.Store.t -> t list
val mark_seen : Matrix_client.Store.t -> t -> unit
val accept : Client.t -> t -> unit
val decline : Client.t -> t -> ?reason:string -> unit -> unit
val decline_and_ban : Client.t -> t -> ?reason:string -> unit -> unit
