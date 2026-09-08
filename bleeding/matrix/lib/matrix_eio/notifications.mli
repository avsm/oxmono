(** notifications — the server-side notification list, raising instead of
    returning.

    {!get} raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Notifications.get} returns [Error e]. This is the server's
    own view, independent of any local push-rule evaluation. *)

type notification = Matrix_client.Notifications.notification = {
  actions : Matrix_proto.Push.Action.t list;
  event : Matrix_proto.Event.Raw_event.t;
  profile_tag : string option;
  read : bool;
  room_id : Matrix_proto.Id.Room_id.t;
  ts : Matrix_proto.Event.Timestamp.t;
}
(** One event the server decided to notify about. *)

type notifications = Matrix_client.Notifications.notifications = {
  chunk : notification list;
  next_token : string option;
}
(** One page of the notification list. *)

val get :
  Client.t ->
  ?from:string ->
  ?limit:int ->
  ?only:[ `Highlight ] ->
  unit ->
  notifications
(** [get c ()] is {!Matrix_client.Notifications.get} with the result unwrapped.
*)
