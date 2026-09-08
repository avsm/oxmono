(** directory — room aliases, visibility, summaries and the published list.

    An alias is a human-readable name for a room on one homeserver. A room may
    have many, and only the server named in an alias can create or delete it.
    Visibility governs whether a room appears in that server's published list.

    What a homeserver discloses about a room the caller is not in depends on the
    room's history visibility and join rules, so a room that exists can read as
    absent. *)

(** {1 Addressing a room} *)

type room_id_or_alias =
  [ `Room_id of Matrix_proto.Id.Room_id.t
  | `Room_alias of Matrix_proto.Id.Room_alias.t ]
(** How an endpoint that accepts either names a room. *)

val room_id_or_alias_to_string : room_id_or_alias -> string
(** [room_id_or_alias_to_string r] is [r] in the form a [{roomIdOrAlias}] path
    segment takes, before percent-encoding. *)

(** {1 Aliases} *)

type alias_info = {
  room_id : Matrix_proto.Id.Room_id.t;
  servers : string list;
      (** Servers known to be in the room, to pass as [via] when joining. *)
}
(** What an alias resolves to. *)

val resolve_alias :
  Client.t -> alias:Matrix_proto.Id.Room_alias.t -> (alias_info, Error.t) result
(** [resolve_alias t ~alias] is
    [GET /_matrix/client/v3/directory/room/{roomAlias}] (Matrix 1.0). An alias
    no server publishes is [M_NOT_FOUND]. *)

val create_alias :
  Client.t ->
  alias:Matrix_proto.Id.Room_alias.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (unit, Error.t) result
(** [create_alias t ~alias ~room_id] is
    [PUT /_matrix/client/v3/directory/room/{roomAlias}] (Matrix 1.0). An alias
    already in use is [M_UNKNOWN]. One in a namespace an application service has
    claimed is [M_EXCLUSIVE]. *)

val delete_alias :
  Client.t -> alias:Matrix_proto.Id.Room_alias.t -> (unit, Error.t) result
(** [delete_alias t ~alias] is
    [DELETE /_matrix/client/v3/directory/room/{roomAlias}] (Matrix 1.0). A user
    who neither created the alias nor has the power to remove it gets
    [M_FORBIDDEN]. *)

(** {1 Visibility} *)

val get_visibility :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (Matrix_proto.Common.Visibility.t, Error.t) result
(** [get_visibility t ~room_id] is
    [GET /_matrix/client/v3/directory/list/room/{roomId}] (Matrix 1.0). *)

val set_visibility :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  visibility:Matrix_proto.Common.Visibility.t ->
  (unit, Error.t) result
(** [set_visibility t ~room_id ~visibility] is
    [PUT /_matrix/client/v3/directory/list/room/{roomId}] (Matrix 1.0).
    Publishing a room is [M_FORBIDDEN] unless the server's policy allows this
    user to. The policy is the server's, not the room's power levels. *)

(** {1 Room summaries} *)

type space_child = {
  child_id : Matrix_proto.Id.Room_id.t;  (** The child's room identifier. *)
  content : Matrix_proto.Event.Space_child_content.t;
}
(** One [m.space.child] event of a room, as a hierarchy reports it. *)

type room_summary = {
  room_id : Matrix_proto.Id.Room_id.t;
  name : string option;
  topic : string option;
  avatar_url : Media.Mxc.t option;
  canonical_alias : Matrix_proto.Id.Room_alias.t option;
  num_joined_members : int;
  room_type : string option;  (** ["m.space"] for a space. *)
  room_version : string option;
  join_rule : Matrix_proto.Event.Join_rule.t option;
  guest_can_join : bool;
  world_readable : bool;  (** Whether history is readable without joining. *)
  encryption : string option;
      (** The room's encryption algorithm, when it has one. *)
  membership : Matrix_proto.Event.Membership.t option;
      (** The caller's own membership. Absent for an unauthenticated request. *)
  children_state : space_child list;
      (** The room's own [m.space.child] events. *)
}
(** What a homeserver says about a room to someone who need not be in it.

    The three endpoints that report one fill in different members.
    {!get_public_rooms} and {!search_public_rooms} leave {!room_version},
    {!encryption}, {!membership} and {!children_state} empty,
    {!Spaces.get_hierarchy} leaves the first three empty, and {!get_summary}
    leaves {!children_state} empty. *)

val room_summary_jsont : room_summary Jsont.t
(** [room_summary_jsont] is the JSON codec for {!room_summary}. *)

val get_summary :
  Client.t ->
  room_id_or_alias:room_id_or_alias ->
  ?via:string list ->
  unit ->
  (room_summary, Error.t) result
(** [get_summary t ~room_id_or_alias ()] is
    [GET /_matrix/client/v1/room_summary/{roomIdOrAlias}] (MSC3266, stable since
    Matrix 1.15). It is the cheapest way to describe a room the user has not
    joined.

    A room whose rules do not let the caller see it is [M_NOT_FOUND], the same
    reply as one that does not exist.

    [via] names servers to ask about a room this homeserver does not hold, and
    defaults to the empty list. *)

(** {1 The published room list} *)

type search_filter = {
  generic_search_term : string option;
      (** Matched against room names, topics and canonical aliases. *)
  room_types : string list option;
      (** Room types to keep, [null] among them for rooms with no type. *)
}
(** What to narrow a search to. Both members left [None] search everything. *)

type published_rooms = {
  page : room_summary Matrix_proto.Common.Page.t;
  total_room_count_estimate : int option;
      (** The server's estimate of the whole result set, when it offers one. *)
}
(** One page of the published list. *)

val get_public_rooms :
  Client.t ->
  ?limit:int ->
  ?from:string ->
  ?server:string ->
  unit ->
  (published_rooms, Error.t) result
(** [get_public_rooms t ()] is [GET /_matrix/client/v3/publicRooms] (Matrix
    1.0). {!search_public_rooms} is the [POST] form, which also takes a search
    term.

    [limit] caps the rooms in the page and defaults to absent, leaving the size
    to the server. [from] is a {!Matrix_proto.Common.Page.next_batch} from an
    earlier page, sent as the [since] parameter, and defaults to absent, which
    starts at the first page. [server] asks another homeserver for its published
    list instead of this one's, and defaults to absent. *)

val search_public_rooms :
  Client.t ->
  ?server:string ->
  ?limit:int ->
  ?from:string ->
  ?filter:search_filter ->
  unit ->
  (published_rooms, Error.t) result
(** [search_public_rooms t ()] is [POST /_matrix/client/v3/publicRooms] (Matrix
    1.0).

    [server], [limit] and [from] are as for {!get_public_rooms}. [filter]
    narrows the result and defaults to absent, listing every published room. A
    server that will not answer for [server] is [M_FORBIDDEN]. *)
