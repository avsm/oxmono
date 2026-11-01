(** presence — whether a user is around, raising instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Presence} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces.

    A homeserver may have presence turned off, in which case every user reads as
    [Offline] and {!set_presence} is accepted and ignored. *)

(** The three states the specification defines. *)
type presence_state = Matrix_client.Presence.presence_state =
  | Online
  | Offline
  | Unavailable  (** Connected, but idle. *)

type presence = Matrix_client.Presence.presence = {
  presence : presence_state;
  status_msg : string option;
  last_active_ago : int option;
  currently_active : bool option;
}
(** A user's presence, as the server reports it. *)

val get_presence : Client.t -> user_id:Matrix_proto.Id.User_id.t -> presence
(** [get_presence c ~user_id] is {!Matrix_client.Presence.get_presence} with the
    result unwrapped. *)

val set_presence :
  Client.t ->
  presence:presence_state ->
  ?status_msg:string ->
  ?immediate:bool ->
  unit ->
  unit
(** [set_presence c ~presence ()] is {!Matrix_client.Presence.set_presence} with
    the result unwrapped. [immediate] defaults to [true]; [false] changes the
    client-owned default without making a request. *)
