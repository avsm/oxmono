(** directory — room aliases, visibility, summaries and the published list,
    raising instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Directory} returns [Error e]. That module documents what
    each call does, which endpoint it uses and which errors it produces. *)

(** {1 Addressing a room} *)

type room_id_or_alias = Matrix_client.Directory.room_id_or_alias
(** How an endpoint that accepts either names a room. *)

(** {1 Aliases} *)

type alias_info = Matrix_client.Directory.alias_info = {
  room_id : Matrix_proto.Id.Room_id.t;
  servers : string list;
}
(** What an alias resolves to. *)

val resolve_alias : Client.t -> alias:Matrix_proto.Id.Room_alias.t -> alias_info
(** [resolve_alias c ~alias] is {!Matrix_client.Directory.resolve_alias} with
    the result unwrapped. *)

val create_alias :
  Client.t ->
  alias:Matrix_proto.Id.Room_alias.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  unit
(** [create_alias c ~alias ~room_id] is {!Matrix_client.Directory.create_alias}
    with the result unwrapped. *)

val delete_alias : Client.t -> alias:Matrix_proto.Id.Room_alias.t -> unit
(** [delete_alias c ~alias] is {!Matrix_client.Directory.delete_alias} with the
    result unwrapped. *)

(** {1 Visibility} *)

val get_visibility :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Common.Visibility.t
(** [get_visibility c ~room_id] is {!Matrix_client.Directory.get_visibility}
    with the result unwrapped. *)

val set_visibility :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  visibility:Matrix_proto.Common.Visibility.t ->
  unit
(** [set_visibility c ~room_id ~visibility] is
    {!Matrix_client.Directory.set_visibility} with the result unwrapped. *)

(** {1 Room summaries} *)

type space_child = Matrix_client.Directory.space_child = {
  child_id : Matrix_proto.Id.Room_id.t;
  content : Matrix_proto.Event.Space_child_content.t;
}
(** One [m.space.child] event of a room in a hierarchy. *)

type room_summary = Matrix_client.Directory.room_summary = {
  room_id : Matrix_proto.Id.Room_id.t;
  name : string option;
  topic : string option;
  avatar_url : Matrix_client.Media.Mxc.t option;
  canonical_alias : Matrix_proto.Id.Room_alias.t option;
  num_joined_members : int;
  room_type : string option;
  room_version : string option;
  join_rule : Matrix_proto.Event.Join_rule.t option;
  guest_can_join : bool;
  world_readable : bool;
  encryption : string option;
  membership : Matrix_proto.Event.Membership.t option;
  children_state : space_child list;
}
(** What a homeserver says about a room to someone who need not be in it. *)

val get_summary :
  Client.t ->
  room_id_or_alias:room_id_or_alias ->
  ?via:string list ->
  unit ->
  room_summary
(** [get_summary c ~room_id_or_alias ()] is
    {!Matrix_client.Directory.get_summary} with the result unwrapped. *)

(** {1 The published room list} *)

type search_filter = Matrix_client.Directory.search_filter = {
  generic_search_term : string option;
  room_types : string list option;
}
(** What to narrow a directory search to. *)

type published_rooms = Matrix_client.Directory.published_rooms = {
  page : room_summary Matrix_proto.Common.Page.t;
  total_room_count_estimate : int option;
}
(** One page of the published room list. *)

val get_public_rooms :
  Client.t ->
  ?limit:int ->
  ?from:string ->
  ?server:string ->
  unit ->
  published_rooms
(** [get_public_rooms c ()] is {!Matrix_client.Directory.get_public_rooms} with
    the result unwrapped. *)

val search_public_rooms :
  Client.t ->
  ?server:string ->
  ?limit:int ->
  ?from:string ->
  ?filter:search_filter ->
  unit ->
  published_rooms
(** [search_public_rooms c ()] is {!Matrix_client.Directory.search_public_rooms}
    with the result unwrapped. *)
