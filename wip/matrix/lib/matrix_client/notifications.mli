(** notifications — the server-side notification list.

    The events the homeserver's own push rules decided should notify this user,
    newest first. It is the server's view, independent of any local evaluation
    by {!Push_evaluator}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#push-notifications>
      Push Notifications *)

type notification = {
  actions : Matrix_proto.Push.Action.t list;
      (** The actions the matching rule produced. *)
  event : Matrix_proto.Event.Raw_event.t;
      (** The event that triggered the notification. The server omits its
          [room_id], which is in {!room_id} instead. *)
  profile_tag : string option;
      (** The profile tag of the rule that matched, when the rule had one. *)
  read : bool;  (** Whether the user has read the event. *)
  room_id : Matrix_proto.Id.Room_id.t;  (** The room the event is in. *)
  ts : Matrix_proto.Event.Timestamp.t;  (** When the event was sent. *)
}
(** The type for one notification. *)

type notifications = {
  chunk : notification list;  (** The notifications, newest first. *)
  next_token : string option;
      (** Token for the next page, absent at the end of the list. *)
}
(** The type for a page of notifications. *)

val get :
  Client.t ->
  ?from:string ->
  ?limit:int ->
  ?only:[ `Highlight ] ->
  unit ->
  (notifications, Error.t) result
(** [get t ()] is a page of the user's notifications. Uses
    [GET /_matrix/client/v3/notifications].

    [from] is a [next_token] from an earlier page and defaults to the start of
    the list. [limit] caps the number returned and defaults to the server's own
    limit. [only] restricts the page to the notifications that highlight, and
    defaults to returning every kind. *)
