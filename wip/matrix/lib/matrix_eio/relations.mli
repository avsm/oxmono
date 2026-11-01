(** relations — reactions, edits, replies and threads, raising instead of
    returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Relations} returns [Error e]. That module documents what
    each call does, which endpoint it uses and which errors it produces. *)

(** {1 Sending} *)

val send_reaction :
  ?extra_content:Jsont.json ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  key:string ->
  Matrix_proto.Id.Event_id.t
(** [send_reaction c ~room_id ~event_id ~key] is
    {!Matrix_client.Relations.send_reaction} with the result unwrapped. *)

val edit_message :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  new_body:string ->
  ?formatted_body:string ->
  ?format:string ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** [edit_message c ~room_id ~event_id ~new_body ()] is
    {!Matrix_client.Relations.edit_message} with the result unwrapped. *)

val send_reply :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  body:string ->
  ?formatted_body:string ->
  ?format:string ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** [send_reply c ~room_id ~event_id ~body ()] is
    {!Matrix_client.Relations.send_reply} with the result unwrapped. *)

val send_in_thread :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root_id:Matrix_proto.Id.Event_id.t ->
  ?reply_to_id:Matrix_proto.Id.Event_id.t ->
  body:string ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** [send_in_thread c ~room_id ~thread_root_id ~body ()] is
    {!Matrix_client.Relations.send_in_thread} with the result unwrapped. *)

(** {1 Querying} *)

type related_event = Matrix_client.Relations.related_event = {
  event_id : Matrix_proto.Id.Event_id.t;
  origin_server_ts : Matrix_proto.Event.Timestamp.t;
  sender : Matrix_proto.Id.User_id.t;
  key : string option;
}
(** An event pointing at the queried one, with the annotation key of a reaction.
*)

val get_relations :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?rel_type:Matrix_proto.Event.Rel_type.t ->
  ?event_type:Matrix_proto.Event.Event_type.t ->
  ?limit:int ->
  ?from:string ->
  unit ->
  related_event Matrix_proto.Common.Page.t
(** [get_relations c ~room_id ~event_id ()] is
    {!Matrix_client.Relations.get_relations} with the result unwrapped. *)

val get_raw_relations :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?rel_type:Matrix_proto.Event.Rel_type.t ->
  ?event_type:Matrix_proto.Event.Event_type.t ->
  ?limit:int ->
  ?from:string ->
  unit ->
  Matrix_proto.Event.Raw_event.t Matrix_proto.Common.Page.t
(** [get_raw_relations c ~room_id ~event_id ()] is
    {!Matrix_client.Relations.get_raw_relations} with the result unwrapped. *)

val get_edit_revisions :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  unit ->
  Matrix_proto.Event.Raw_event.t list
(** [get_edit_revisions c ~room_id ~event_id ()] is
    {!Matrix_client.Relations.get_edit_revisions} with the result unwrapped. *)

val get_reactions :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  related_event Matrix_proto.Common.Page.t
(** [get_reactions c ~room_id ~event_id] is
    {!Matrix_client.Relations.get_reactions} with the result unwrapped. *)

(** {1 Threads} *)

(** Which of a room's threads to list. *)
type thread_filter = Matrix_client.Relations.thread_filter =
  | All  (** Every thread in the room. *)
  | Participated  (** Only the threads the user has taken part in. *)

val list_threads :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?filter:thread_filter ->
  ?from:string ->
  ?limit:int ->
  unit ->
  Matrix_proto.Event.Raw_event.t Matrix_proto.Common.Page.t
(** [list_threads c ~room_id ()] is {!Matrix_client.Relations.list_threads} with
    the result unwrapped. *)
