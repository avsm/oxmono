(** room_timeline — a reactive, aggregated timeline model for UI toolkits.

    {!val-items} is a room's events in order, with the events that act on other
    events folded in rather than shown beside them, so a reaction, a redaction
    and the [m.replace] carrying an edit all become fields of the item they act
    on. Date dividers, gaps and the read marker appear as {!virtual_item}s among
    them, and {!Event_cache.snapshot} holds everything the timeline hides.

    This is the model a toolkit renders. {!Matrix_client.Timeline} is the
    unaggregated event list the sync layer keeps. *)

(** {1 Items} *)

type reaction = {
  key : string;
  count : int;
  senders : Matrix_proto.Id.User_id.t list;
  own : bool;  (** The own user is among the senders. *)
}
(** The type for the reactions to one event, gathered by key. *)

type event_item = {
  id : string;  (** Identifies the item for as long as the event is held. *)
  event : Presentation.t;
      (** The event as it should be shown, which is the edit's content where the
          event was edited. *)
  delivery : Event_cache.delivery;
  reactions : reaction list;  (** By key, ascending. *)
  edited : bool;
  redacted : bool;
  reply_to : Matrix_proto.Id.Event_id.t option;
}
(** The type for the items that stand for an event. *)

type gap_id = Event_cache.Gap_id.t
(** The type for gap identifiers, as {!Event_cache.Gap_id.t}. *)

type date = { year : int; month : int; day : int }
(** The type for calendar dates, in UTC. *)

(** The type for items that stand for something other than an event. *)
type virtual_item =
  | Gap of { id : gap_id }
      (** Events the server skipped. There is one per gap the event cache holds,
          at the gap's own position. {!paginate_gap} fills the one [id] names
          and {!paginate_back} the oldest. *)
  | Timeline_start
      (** The room's beginning has been reached, so there is nothing left to
          page in. It can only be the head, and it and a leading {!Gap} are
          mutually exclusive, since paginating past the last page turns the one
          into the other. *)
  | Read_marker  (** After the last event the own user has read. *)
  | Date_divider of date  (** Before the first event of that day. *)

(** The type for timeline items. *)
type item =
  | Event of event_item
  | Virtual of { id : string; content : virtual_item }

type event_filter = Presentation.t -> bool
(** A predicate deciding whether a presented event gets its own event item. *)

val item_id : item -> string
(** [item_id item] identifies the item within its timeline, so that a toolkit's
    list model can key its rows on it. *)

(** {1 Opening a timeline} *)

type t
(** The type for timelines. A timeline projects one room and follows the event
    cache until {!close}. *)

val default_event_filter : event_filter
(** The default event-item filter. It keeps messages, meaningful membership and
    profile changes, and supported room metadata, while hiding edits, reactions,
    redactions and unchanged memberships. Relation events rejected by this
    predicate are still aggregated onto their targets. Compose it with a custom
    predicate to add state types without making relation events standalone
    items. *)

val create :
  sw:Eio.Switch.t ->
  client:Matrix_client.Client.t ->
  send_queue:Matrix_client.Send_queue.t ->
  ?encryption:Matrix_client.Encryption.t ->
  ?own_user:Matrix_proto.Id.User_id.t ->
  ?read_state:(unit -> Matrix_client.Read_state.t) ->
  ?read_marker:(unit -> Matrix_proto.Id.Event_id.t option) ->
  ?resolve_mxc:(string -> string option) ->
  ?event_filter:event_filter ->
  Event_cache.t ->
  Matrix_proto.Id.Room_id.t ->
  t
(** [create ~sw ~client ~send_queue cache room_id] projects the room's cached
    events and keeps projecting as the cache changes, under [sw]. [client]
    fetches the pages {!paginate_back} and {!paginate_gap} splice in, and
    [send_queue] carries what the send functions queue.

    [encryption] decrypts the events a pagination brings back; without it the
    events of an encrypted room stay {!Presentation.Unable_to_decrypt} until
    sync delivers them again.

    [event_filter] replaces the default event-item choice. Virtual items (date
    dividers, gaps, the timeline start and read marker) remain mandatory, and
    relation events are always considered for aggregation. Returning [true] for
    an edit, reaction or redaction deliberately gives it a standalone item as
    well as any aggregate on its target. Compose with {!default_event_filter} to
    add state types without doing that. An undecryptable ciphertext remains
    visible regardless of the predicate, so it can be retried when its key
    arrives.

    [read_marker] is consulted on every projection and answers the room's
    [m.fully_read] event, which is {!Matrix_client.Read_state.fully_read} of
    {!Matrix_client.Base_client.receipts}, or
    {!Matrix_client.Read_state.latest_read} where the account has no marker.
    {!Runtime} passes one that follows its sync state. Call {!refresh} when it
    may have moved. Without it no {!Read_marker} item is ever produced, and
    without [own_user] no reaction is ever {!reaction.own}.

    [read_state], when supplied, is consulted by the receipt helpers below. It
    should return the room's current own read state; {!Runtime} supplies a
    provider backed by its sync service. Standalone timelines may omit it, in
    which case receipts sent through this handle are still tracked locally.

    [resolve_mxc], when supplied, rewrites [mxc://] media URLs while presenting
    events, including edited presentations and cache refreshes. It is fixed for
    the lifetime of this timeline; close and recreate the timeline to use a
    different resolver.

    {!Runtime.timeline} is the supported way to build one. *)

val close : t -> unit
(** [close t] unsubscribes from the event cache, so that the fibers {!create}
    forked return and the timeline stops updating. The last snapshot stays
    readable. Idempotent. {!Runtime.close_timeline} is the usual caller. *)

val discard : t -> unit
(** [discard t] closes [t] and synchronously clears its items, loading state,
    pagination error, and presentation cache. It is the stronger lifecycle
    operation used when {!Runtime.forget} must leave no readable room data on an
    existing timeline handle. Idempotent. *)

val room_id : t -> Matrix_proto.Id.Room_id.t
(** [room_id t] is the room [t] projects. *)

val items : t -> item Observable.List.t
(** [items t] is the room's items, oldest first. The state events a member is
    told about are among them, which are {!Presentation.Membership} and
    {!Presentation.Profile} changes and the room name, topic, avatar, main
    address, encryption, pinned messages and tombstone. The configuration a room
    is built out of is not, and neither is a membership event that changed
    nothing. {!Event_cache.snapshot} holds them all. *)

val snapshot : t -> item array
(** [snapshot t] is {!val-items} as it stands. *)

val loading : t -> bool Observable.Value.t
(** [loading t] is whether a pagination is in flight. *)

val pagination_error : t -> Matrix_client.Error.t option Observable.Value.t
(** [pagination_error t] is what the last pagination failed with, cleared by one
    that succeeds. It carries the same error the failing call returned, for a
    toolkit that watches rather than one that called. *)

val refresh : t -> unit
(** [refresh t] re-projects immediately. The timeline does this by itself for
    every change to the event cache, so a caller needs it only when something
    outside the cache moved, which means the read marker. *)

val edit_revisions :
  t -> event_id:Matrix_proto.Id.Event_id.t -> Presentation.t list
(** [edit_revisions t ~event_id] returns the cached original event followed by
    its valid [m.replace] revisions in chronological order. The replacement
    relation is removed from each revision's presentation, as it is from the
    current timeline item. Redacted, duplicate and invalid revisions are
    omitted, including revisions that violate encrypted-event provenance. This
    is a cache-only lookup: edits outside the cached event window are not
    fetched. It returns [[]] when the original event is not cached. *)

(** {1 Pagination} *)

type pagination =
  [ `Reached_start
    (** The page landed and there is nothing older left to fetch at that edge.
    *)
  | `More  (** The page landed and a token remains. *)
  | `Nothing_to_do
    (** Nothing was fetched. There is no token, or the gap named is gone, or
        another pagination is in flight. *) ]
(** The type for what a pagination did, so that a caller can loop on it rather
    than re-read the head of {!val-items} and race the projection. *)

val paginate_back :
  t -> ?limit:int -> unit -> (pagination, Matrix_client.Error.t) result
(** [paginate_back t ()] first hydrates one locally persisted predecessor chunk
    when one is available; only then does it fetch a page from the resident
    timeline's oldest edge, filling the leading gap or extending the front. It
    re-projects before returning, so {!val-items} already shows the events.
    [`Reached_start] means neither an unloaded predecessor nor an edge token is
    left, which is the {!Timeline_start} item. A call made while a page is in
    flight fetches nothing and answers [`Nothing_to_do] rather than waiting.
    [limit] is how many events to ask from the server, 30 by default; a local
    chunk retains its persisted size. A storage or request failure is returned
    and also published on {!pagination_error}.

    Raises [Invalid_argument] if [limit] is not positive. *)

val paginate_gap :
  t ->
  ?limit:int ->
  gap:gap_id ->
  unit ->
  (pagination, Matrix_client.Error.t) result
(** [paginate_gap t ~gap ()] fills the gap a {!Gap} item names. The page is
    fetched backwards from the gap's token and spliced in at the gap's position,
    and the timeline is re-projected before this returns. The gap survives,
    retargeted at the response's [end] token, which is [`More], until a page
    reaches the room's beginning or events the room already held on the older
    side, at which point the hole closes and the item goes. The answer is
    [`More] while an older persisted prefix remains, and otherwise
    [`Reached_start]. A gap another pagination has already closed is
    [`Nothing_to_do]. [limit] is as in {!paginate_back}, and so are the
    concurrency and failure behaviour.

    Raises [Invalid_argument] if [limit] is not positive. *)

(** {1 Sending}

    Each of these puts a request on the send queue {!create} was given and
    returns at once. The item appears on the timeline as a local echo, and its
    {!event_item.delivery} follows it. *)

val send_message :
  t ->
  ?msgtype:[ `Text | `Notice | `Emote ] ->
  ?formatted:string ->
  ?reply_to:Matrix_proto.Id.Event_id.t ->
  body:string ->
  unit ->
  Matrix_client.Send_queue.request
(** [send_message t ~body ()] queues an [m.room.message]. [msgtype] is [`Text]
    by default; [`Notice] is what an automated sender should use, since a client
    that answers messages is required to ignore it. [formatted] is HTML, sent as
    [formatted_body] under [format: org.matrix.custom.html] after
    {!Presentation.Html.sanitize} has cut it down to the subset the
    specification permits, with [body] left as the plain-text fallback beside
    it. [reply_to] adds the [m.in_reply_to] relation, which is what
    {!event_item.reply_to} reads back; no fallback quotation is prepended to
    either body.

    @see <https://spec.matrix.org/v1.11/client-server-api/#mroommessage-msgtypes>
      the message types and [formatted_body] *)

val send_text : t -> body:string -> Matrix_client.Send_queue.request
(** [send_text t ~body] is {!send_message} with every option left out, a plain
    [m.text]. *)

val send_reply :
  t ->
  ?formatted:string ->
  event_id:Matrix_proto.Id.Event_id.t ->
  body:string ->
  unit ->
  Matrix_client.Send_queue.request
(** [send_reply t ~event_id ~body ()] is {!send_message} with an [m.in_reply_to]
    relation targeting [event_id]. *)

type location_asset =
  | Self
  | Pin
  | Custom of string
      (** The Matrix location asset. [Self] is the sender's current location,
          [Pin] is a fixed place, and [Custom type_] preserves a future
          namespaced type. *)

val send_location :
  t ->
  ?description:string ->
  ?zoom_level:int ->
  ?asset:location_asset ->
  ?reply_to:Matrix_proto.Id.Event_id.t ->
  geo_uri:string ->
  body:string ->
  unit ->
  Matrix_client.Send_queue.request
(** [send_location t ~geo_uri ~body ()] queues an [m.location] message with the
    legacy [geo_uri] fallback and the extensible [org.matrix.msc3488.location]
    content. [description], [zoom_level] and [asset] populate the extensible
    location fields; [reply_to] adds the same relation as {!send_reply}. The
    zoom level must be between 0 and 20 inclusive.

    Raises [Invalid_argument] for an out-of-range [zoom_level]. *)

val send_edit :
  t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?formatted:string ->
  body:string ->
  unit ->
  Matrix_client.Send_queue.request
(** [send_edit t ~event_id ~body ()] queues an [m.replace] edit of [event_id].
    The fallback body is prefixed with ["* "] and the real body is in
    [m.new_content]. [formatted], when given, is sanitised with
    {!Presentation.Html.sanitize}; it is written to both the replacement and its
    prefixed formatted fallback as [org.matrix.custom.html]. *)

val send_reaction :
  t ->
  relates_to:Matrix_proto.Id.Event_id.t ->
  key:string ->
  Matrix_client.Send_queue.request
(** [send_reaction t ~relates_to ~key] queues an [m.reaction] annotating the
    event [relates_to] with [key], which is the emoji a toolkit shows. *)

val redact :
  t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?reason:string ->
  unit ->
  Matrix_client.Send_queue.request
(** [redact t ~event_id ()] queues a redaction of the event, with [reason] as
    the explanation the server keeps beside it. *)

val item_of_request : t -> Matrix_client.Send_queue.request -> event_item option
(** [item_of_request t request] is the item a queued send produced, and [None]
    where this timeline holds no such item, because the request belongs to
    another room or was cancelled. The same item answers before and after
    delivery, since the local echo and the server's copy of it are one item, so
    a caller may hold the request and ask again. *)

val delivery :
  t -> Matrix_client.Send_queue.request -> Event_cache.delivery option
(** [delivery t request] is the {!event_item.delivery} of {!item_of_request},
    which is how far a send has got as the timeline shows it. *)

(** {1 Read receipts} *)

type receipt_type = Matrix_client.Receipts.receipt_type =
  | Read
  | Read_private
  | Fully_read  (** The server-side kind of read position to move. *)

type receipt = {
  receipt_type : receipt_type;
  event_id : Matrix_proto.Id.Event_id.t;
}
(** One receipt to send through {!send_multiple_receipts}. *)

val latest_user_read_receipt :
  t ->
  ?receipt_type:receipt_type ->
  ?thread_id:Matrix_proto.Id.Event_id.t ->
  unit ->
  Matrix_proto.Id.Event_id.t option
(** [latest_user_read_receipt t ()] is the latest own public/private read
    receipt represented by [t], ordered by timeline position rather than event
    timestamp. [receipt_type] restricts the lookup to one kind; [thread_id]
    restricts it to a thread and includes the thread root. A fully-read marker
    is returned only when [receipt_type] is explicitly [Fully_read]. *)

val send_single_receipt :
  t ->
  ?thread_id:Matrix_proto.Id.Event_id.t ->
  receipt_type ->
  event_id:Matrix_proto.Id.Event_id.t ->
  (bool, Matrix_client.Error.t) result
(** [send_single_receipt t kind ~event_id] sends a receipt when it advances the
    corresponding own position. [thread_id] explicitly scopes a public or
    private receipt; it is unthreaded when omitted, and fully-read markers are
    always unthreaded. A normal read receipt targeting one of the own user's
    events is redirected to the latest preceding event from another user;
    [mark_as_read] may override that fallback when the room has only own events.
    [false] means the request was suppressed as a duplicate, regression, or
    own-event-only target. An unthreaded no-op still clears the room's
    marked-unread flag, as the Rust timeline API does. *)

val send_multiple_receipts :
  t -> receipt list -> (unit, Matrix_client.Error.t) result
(** [send_multiple_receipts t receipts] applies each receipt with the same
    monotonic checks as {!send_single_receipt}. The list is unthreaded, and its
    first surviving fully-read, public-read and private-read positions use one
    combined read-markers request as Rust does. Additional duplicate kinds use
    the corresponding individual endpoint. An empty list (or a list containing
    only suppressed receipts) clears the room's marked-unread flag. *)

val mark_as_read :
  t ->
  ?thread_id:Matrix_proto.Id.Event_id.t ->
  receipt_type ->
  (bool, Matrix_client.Error.t) result
(** [mark_as_read t kind] chooses the latest remote event known to the timeline,
    including a relation folded into a visible item (or the latest event in
    [thread_id] when supplied), and sends [kind]. It returns [false] and clears
    marked-unread when there is no candidate. *)
