(** timeline — a room's events in order, with edits and redactions applied.

    A timeline is the list a client needs in order to draw a room, and it does
    the three things that cannot be left to the caller. It deduplicates, it
    reconciles a local echo with the event [/sync] later returns for it, and it
    applies [m.replace] and [m.room.redaction] to the events they target.

    An edit or a redaction is not an item of its own. It changes the item it
    points at, which is why {!content} differs from {!event}. There is no
    grouping, no read-receipt rendering and no persistence.

    A timeline is mutable and is not safe to share between fibers that run
    concurrently.

    @see <https://spec.matrix.org/v1.11/client-server-api/#event-replacements>
      Event Replacements
    @see <https://spec.matrix.org/v1.11/client-server-api/#redactions>
      Redactions *)

(** {1 Items} *)

type item
(** One event in a timeline, together with what later events did to it. *)

val event : item -> Matrix_proto.Event.Raw_event.t
(** [event i] is the event as it was received. *)

val local_echo : item -> bool
(** [local_echo i] is [true] while [i] was enqueued locally and the server has
    not confirmed it. See {!Send_queue.local_echo}. *)

val redacted : item -> bool
(** [redacted i] is [true] when a redaction in this timeline targeted [i]. *)

val replacement : item -> Jsont.json option
(** [replacement i] is the [m.new_content] of the most recent edit of [i], and
    [None] when nothing has edited it. *)

val content : item -> Jsont.json
(** [content i] is what to display. It is {!replacement} when [i] was edited, an
    empty object when [i] was redacted, and the event's own content otherwise.
*)

(** {1 Timelines} *)

type t
(** The type for timelines. *)

val create : room_id:Matrix_proto.Id.Room_id.t -> ?limit:int -> unit -> t
(** [create ~room_id ()] is an empty timeline for [room_id]. [limit] caps how
    many items are kept, dropping the oldest, and defaults to 1000. *)

val room_id : t -> Matrix_proto.Id.Room_id.t
(** [room_id t] is the room [t] was created for. *)

val items : t -> item list
(** [items t] is every item, oldest first. *)

val length : t -> int
(** [length t] is the number of items in [t]. *)

val find : t -> Matrix_proto.Id.Event_id.t -> item option
(** [find t id] is the item whose event has identifier [id], and [None] when [t]
    holds no such event. *)

val last : t -> item option
(** [last t] is the most recent item, and [None] when [t] is empty. *)

val clear : t -> unit
(** [clear t] drops every item and the pagination token, leaving [t] as
    {!create} made it. *)

(** {1 Adding events} *)

val add : t -> ?local_echo:bool -> Matrix_proto.Event.Raw_event.t -> unit
(** [add t event] appends [event], unless

    - an item with the same event identifier is already present, in which case
      nothing happens;
    - an item is a local echo with the same [unsigned.transaction_id], in which
      case that item is replaced in place by [event], keeping its position;
    - the event is an [m.replace] edit, in which case the target item's
      {!replacement} is updated and no item is added;
    - the event is an [m.room.redaction], in which case the target item is
      marked {!redacted} and no item is added.

    [local_echo] marks the new item as not yet confirmed by the server, and
    defaults to [false].

    A redaction is matched by [content.redacts], the room-version-11 shape, or
    by the top-level [redacts] used in room versions 1–10. *)

val add_many : t -> Matrix_proto.Event.Raw_event.t list -> unit
(** [add_many t l] is {!add} over [l], oldest first. *)

val prepend : t -> Matrix_proto.Event.Raw_event.t list -> unit
(** [prepend t l] inserts older events at the front, oldest first, for
    back-pagination. It deduplicates, and applies edits and redactions in [l] as
    {!add} does. *)

(** {1 Pagination} *)

val prev_batch : t -> string option
(** [prev_batch t] is the token {!paginate_back} will continue from, and [None]
    when there is nothing older to fetch. *)

val set_prev_batch : t -> string option -> unit
(** [set_prev_batch t token] sets the token {!paginate_back} continues from.
    [token] is normally the [prev_batch] a [/sync] gave for this room. *)

val paginate_back :
  Client.t ->
  t ->
  ?limit:int ->
  unit ->
  (Matrix_proto.Event.Raw_event.t list, Error.t) result
(** [paginate_back client t ()] fetches the next older chunk through
    [GET /rooms/{roomId}/messages], prepends it, and advances {!prev_batch} to
    the token the server returned. The chunk is oldest first.

    [Ok []] means there is nothing older, and {!prev_batch} becomes [None].
    Without a {!prev_batch} the call is [Ok []] and makes no request.

    [limit] caps the events fetched and defaults to 20. *)
