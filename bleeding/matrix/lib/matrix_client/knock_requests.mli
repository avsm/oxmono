(** Incoming room-knock requests and their moderation actions. *)

type t = {
  room_id : Matrix_proto.Id.Room_id.t;
  event_id : Matrix_proto.Id.Event_id.t option;
  timestamp : Matrix_proto.Event.Timestamp.t option;
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
  avatar_url : Media.Mxc.t option;
  reason : string option;
  is_seen : bool;
}
(** A current [m.room.member] event whose membership is [knock]. Stripped
    invite/knock state has no event id or timestamp in Matrix, so those fields
    are [None] in that case. *)

val list : Store.t -> room_id:Matrix_proto.Id.Room_id.t -> t list
(** [list store ~room_id] derives requests from the current persisted state. *)

val all : Store.t -> t list
(** [all store] derives requests from every known room. *)

val mark_seen : Store.t -> t -> (unit, Error.t) result
(** Persists this request as seen. The store's normal atomic flush is used for
    on-disk stores, so a restart cannot observe a half-written seen set. A
    malformed previously persisted seen set is reported as {!Error.Json_error}
    and is not overwritten. *)

val accept : Client.t -> t -> (unit, Error.t) result
(** Invites the requester into the room. *)

val decline : Client.t -> t -> ?reason:string -> unit -> (unit, Error.t) result
(** Kicks the requester from the room. *)

val decline_and_ban :
  Client.t -> t -> ?reason:string -> unit -> (unit, Error.t) result
(** Bans the requester from the room. *)
