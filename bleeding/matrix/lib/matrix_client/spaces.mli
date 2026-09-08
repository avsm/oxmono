(** spaces — rooms that hold other rooms.

    A space is an ordinary room whose type is ["m.space"]. Membership of a space
    is state on both sides. An [m.space.child] event in the space is keyed by
    the child's room identifier, an [m.space.parent] event in the child is keyed
    by the space's, and neither side is authoritative on its own, so a client
    that trusts a parent link should check that the space names the child too.
*)

(** {1 Hierarchy} *)

type space_child = Directory.space_child = {
  child_id : Matrix_proto.Id.Room_id.t;
  content : Matrix_proto.Event.Space_child_content.t;
}
(** One [m.space.child] event of a room in the hierarchy. *)

val get_hierarchy :
  Client.t ->
  space:Matrix_proto.Id.Room_id.t ->
  ?suggested_only:bool ->
  ?limit:int ->
  ?max_depth:int ->
  ?from:string ->
  unit ->
  (Directory.room_summary Matrix_proto.Common.Page.t, Error.t) result
(** [get_hierarchy t ~space ()] is
    [GET /_matrix/client/v1/rooms/{roomId}/hierarchy] (Matrix 1.2). The page
    holds [space] itself first, then its descendants, depth first. Rooms the
    user may not see are omitted rather than refused, and the page carries no
    [prev_batch].

    [suggested_only] keeps only children the space marks as suggested and
    defaults to [false]. [limit] caps the rooms per page and defaults to absent,
    leaving the size to the server. [max_depth] is how far to descend, [0] being
    [space] alone, and defaults to absent. [from] is a [next_batch] from an
    earlier page and defaults to absent, which starts at the first page.

    A [from] used with different other parameters than the call that produced it
    is [M_INVALID_PARAM]. *)

(** {1 Links}

    Each of these writes a state event, so it needs the power level the room
    requires for that event type and is [M_FORBIDDEN] without it. Each is the
    new event's identifier.

    [via] names servers to route a join through, and defaults to the empty list.
    A child whose [via] is empty is not considered part of the space, which is
    also how the specification spells removal. *)

val add_child :
  Client.t ->
  space:Matrix_proto.Id.Room_id.t ->
  child:Matrix_proto.Id.Room_id.t ->
  ?via:string list ->
  ?order:string ->
  ?suggested:bool ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [add_child t ~space ~child ()] writes the [m.space.child] event in [space].

    [order] is the sort key among siblings, which the specification restricts to
    printable ASCII, and defaults to absent, sorting [child] last. [suggested]
    defaults to [false]. *)

val remove_child :
  Client.t ->
  space:Matrix_proto.Id.Room_id.t ->
  child:Matrix_proto.Id.Room_id.t ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [remove_child t ~space ~child] empties the [m.space.child] event. The event
    stays in the room's state with no [via]. *)

val set_parent :
  Client.t ->
  room:Matrix_proto.Id.Room_id.t ->
  parent:Matrix_proto.Id.Room_id.t ->
  ?via:string list ->
  ?canonical:bool ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [set_parent t ~room ~parent ()] writes the [m.space.parent] event in [room].

    [canonical] names [parent] as the room's main space, of however many claim
    it, and defaults to [false]. *)

val remove_parent :
  Client.t ->
  room:Matrix_proto.Id.Room_id.t ->
  parent:Matrix_proto.Id.Room_id.t ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [remove_parent t ~room ~parent] empties the [m.space.parent] event, as
    {!remove_child} does for the other direction. *)

(** {1 Creation} *)

val is_space : Directory.room_summary -> bool
(** [is_space r] is [true] when [r] describes a space rather than an ordinary
    room. *)

val create_space :
  Client.t ->
  ?name:string ->
  ?topic:string ->
  ?visibility:Matrix_proto.Common.Visibility.t ->
  ?invite:Matrix_proto.Id.User_id.t list ->
  unit ->
  (Matrix_proto.Id.Room_id.t, Error.t) result
(** [create_space t ()] is {!Rooms.create} with [~room_type:"m.space"], and is
    the new space's room identifier. Every argument is as for {!Rooms.create}.
*)
