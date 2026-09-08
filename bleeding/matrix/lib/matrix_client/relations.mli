(** relations — reactions, edits, replies and threads.

    A relation is an [m.relates_to] member in an event's content pointing at an
    earlier event. The senders here build that member for the four relations the
    specification defines and the queries read the events that point at a given
    one. Nothing aggregates them, so a caller that wants the current text of an
    edited message folds the replacements itself.

    @see <https://spec.matrix.org/v1.11/client-server-api/#forming-relationships-between-events>
      Forming relationships between events *)

(** {1 Sending} *)

val send_reaction :
  ?extra_content:Jsont.json ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  key:string ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_reaction client ~room_id ~event_id ~key] sends an [m.reaction]
    annotating [event_id] and is the new event's id. [key] is what the room
    sees, usually a single emoji. [extra_content] appends vendor fields without
    overriding [m.relates_to] or any of its typed fields. Reacting twice with
    the same key from the same user is accepted by the server and ignored by
    clients. *)

val edit_message :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  new_body:string ->
  ?formatted_body:string ->
  ?format:string ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [edit_message client ~room_id ~event_id ~new_body ()] sends an
    [m.room.message] replacing [event_id] and is the new event's id. The
    original stays in the timeline. The outer body carries [new_body] behind a
    ["* "] prefix as the fallback for clients that do not follow [m.replace].
    Only the sender of an event may edit it.

    [formatted_body] is the formatted replacement and defaults to absent, in
    which case the edit is plain text. [format] names its format and defaults to
    ["org.matrix.custom.html"]. [format] alone, without [formatted_body], is
    ignored. *)

val send_reply :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  body:string ->
  ?formatted_body:string ->
  ?format:string ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_reply client ~room_id ~event_id ~body ()] sends an [m.room.message]
    replying to [event_id] and is the new event's id. [formatted_body] and
    [format] default as they do in {!edit_message}. *)

val send_in_thread :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  thread_root_id:Matrix_proto.Id.Event_id.t ->
  ?reply_to_id:Matrix_proto.Id.Event_id.t ->
  body:string ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_in_thread client ~room_id ~thread_root_id ~body ()] sends a plain-text
    [m.room.message] in the thread rooted at [thread_root_id] and is the new
    event's id.

    [reply_to_id] is an event within the thread to reply to, and defaults to
    absent. Without it the message carries an [m.in_reply_to] pointing at the
    root, flagged [is_falling_back] so that a threaded client renders it as a
    plain thread message rather than a reply. *)

(** {1 Querying} *)

type related_event = {
  event_id : Matrix_proto.Id.Event_id.t;
  origin_server_ts : Matrix_proto.Event.Timestamp.t;
  sender : Matrix_proto.Id.User_id.t;
  key : string option;
      (** The annotation the relation carries, which is the emoji of a reaction.
          Absent for every relation other than [m.annotation]. *)
}
(** One event pointing at the queried one. Only the members every relation
    carries are read. The event itself must be fetched for its content. *)

val get_relations :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?rel_type:Matrix_proto.Event.Rel_type.t ->
  ?event_type:Matrix_proto.Event.Event_type.t ->
  ?limit:int ->
  ?from:string ->
  ?dir:Matrix_proto.Common.Direction.t ->
  ?recurse:bool ->
  unit ->
  (related_event Matrix_proto.Common.Page.t, Error.t) result
(** [get_relations client ~room_id ~event_id ()] is one page of the events
    relating to [event_id], most recent first
    ([GET /_matrix/client/v1/rooms/{roomId}/relations/{eventId}], Matrix 1.3).

    [rel_type] narrows the path to one relation type and defaults to every type.
    [event_type] narrows it further to one event type, is only honoured together
    with [rel_type] as the path shape requires, and defaults to every type.
    [limit] is the server's page size and defaults to whatever the server
    chooses. [from] is a page's [next_batch] and defaults to the first page.

    [dir] selects the pagination direction ([Backward] or [Forward]) and is
    omitted by default, letting the server use its backwards default. [recurse]
    requests indirect relations when [true] and excludes them when [false]; it
    is omitted by default. These controls are useful for thread pagination.

    An event the user cannot see is [M_FORBIDDEN] rather than an empty page. *)

val get_raw_relations :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?rel_type:Matrix_proto.Event.Rel_type.t ->
  ?event_type:Matrix_proto.Event.Event_type.t ->
  ?limit:int ->
  ?from:string ->
  ?dir:Matrix_proto.Common.Direction.t ->
  ?recurse:bool ->
  unit ->
  (Matrix_proto.Event.Raw_event.t Matrix_proto.Common.Page.t, Error.t) result
(** [get_raw_relations] makes the same request as {!get_relations}, retaining
    each complete event envelope and content. It is intended for consumers such
    as edit validation that cannot use the smaller {!related_event} projection.
    Its filters are caller-selected; it is not implicitly narrowed to
    replacements. [dir] and [recurse] have the same meanings and defaults as in
    {!get_relations}. *)

val get_edit_revisions :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  unit ->
  (Matrix_proto.Event.Raw_event.t list, Error.t) result
(** [get_edit_revisions client ~room_id ~event_id ()] fetches the original
    [m.room.message], exhausts its [m.replace] relation pages, and returns the
    original followed by valid revisions in chronological order. A revision must
    be an unredacted, non-state [m.room.message] from the original sender,
    target exactly [event_id], and carry object-valued [m.new_content]. Repeated
    event ids and pagination tokens are handled once. A state event, an original
    which is itself a replacement, or a non-message original gives [[]].

    This uses the plaintext relations endpoint. Encrypted replacement events
    remain unavailable in usable form; callers with a shared decrypted event
    cache should use [Matrix_ui.Room_timeline.edit_revisions] instead, which
    also enforces encrypted-event provenance. *)

val get_reactions :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  (related_event Matrix_proto.Common.Page.t, Error.t) result
(** [get_reactions client ~room_id ~event_id] is {!get_relations} narrowed to
    the [m.reaction] events annotating [event_id], each carrying its emoji in
    {!related_event.key}. *)

(** {1 Threads} *)

(** Which of a room's threads to list. *)
type thread_filter =
  | All  (** Every thread in the room. *)
  | Participated  (** Only threads the calling user has taken part in. *)

val list_threads :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?filter:thread_filter ->
  ?from:string ->
  ?limit:int ->
  unit ->
  (Matrix_proto.Event.Raw_event.t Matrix_proto.Common.Page.t, Error.t) result
(** [list_threads client ~room_id ()] is one page of the room's threads, most
    recently active first. It uses
    [GET /_matrix/client/v1/rooms/{roomId}/threads] on Matrix 1.4+ servers and
    the MSC3856 unstable path otherwise. Each element of the page is a thread's
    root event.

    [filter] defaults to the server's default of {!All}. [from] is a page's
    [next_batch] and defaults to the first page. [limit] is the page size and
    defaults to whatever the server chooses. The endpoint returns no backward
    token, so the page's [prev_batch] is always absent. *)
