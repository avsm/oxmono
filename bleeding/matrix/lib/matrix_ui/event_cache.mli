(** event_cache — the shared event cache feeding timelines, room previews and
    notifications.

    A room is held as an ordered list of chunks, each either a run of events or
    a {b gap}, a hole the server skipped which carries the token that fills it.
    {!prev_batch} and {!has_gap} are projections of that list, {!val-gaps} is
    the list of holes, and {!val-events} flattens the whole thing. Everything
    the cache publishes is an {!Observable}, so a toolkit subscribes rather than
    polls.

    Every room is addressed by its {!Matrix_proto.Id.Room_id.t}. A lazy-capable
    store initially loads only the newest persisted events chunk; older chunks
    remain a private persisted prefix until {!hydrate_previous} is requested.
    Without a store a room starts empty. *)

(** {1 Events} *)

(** The type for how far a send has got. An event the server sent is always
    {!Synced}. *)
type delivery = Event_store.Internal.delivery =
  | Synced
  | Sending
  | Queued
  | Failed of string  (** The send was given up on, with this reason. *)

type event = Event_store.Internal.event = {
  stable_id : string;
      (** Identifies the event across saves and reloads, and across the moment a
          local echo becomes the server's copy of itself. *)
  event : Matrix_proto.Event.Raw_event.t;
  clear_event : Matrix_proto.Event.Raw_event.t option;
      (** The plaintext of an [m.room.encrypted] event, once one is known. *)
  delivery : delivery;
}
(** The type for cached events. *)

val effective : event -> Matrix_proto.Event.Raw_event.t
(** [effective event] is the event as it should be read, its plaintext where the
    cache holds a decryption and the wire event otherwise. *)

(** {1 Gaps} *)

module Gap_id : sig
  (** Names one hole in a room's history, for as long as that hole lasts. *)

  type t
  (** The type for gap identifiers. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] name the same gap. *)

  val to_string : t -> string
  (** [to_string id] is a printable form of [id], unique within its room. *)
end

type gap = {
  id : Gap_id.t;
  token : string;
      (** The [/messages] token that fills the gap, paginating {e backwards}
          from the start of what follows it. *)
  index : int;
      (** How many cached events precede the gap, so that a consumer can place
          it among {!val-events}. *)
}
(** The type for holes in a room's cached history. *)

(** {1 The cache} *)

type t
(** The type for event caches. A cache is shared by every model over one
    account, and may be used from several fibers. *)

val create :
  ?store:Event_store.t ->
  ?max_events_per_room:int ->
  ?chunk_capacity:int ->
  unit ->
  t
(** [create ()] is a new cache. Without [store] it is memory-only and starts
    empty. [max_events_per_room] bounds the resident events one room holds
    before the oldest are traded for a gap, 10000 by default; lazily unloaded
    persisted chunks do not count against it. [chunk_capacity] bounds the events
    one chunk holds, 128 by default; a sync window opens a new chunk once the
    newest one is full, which is what gives a room a boundary token to keep when
    [max_events_per_room] forces a trim.

    Raises [Invalid_argument] if [max_events_per_room] or [chunk_capacity] is
    not positive. *)

val last_error : t -> Event_store.Error.t option Observable.Value.t
(** [last_error t] is the last store failure, or [None]. A cache whose store
    fails keeps working in memory. *)

val forget_room : t -> Matrix_proto.Id.Room_id.t -> unit
(** [forget_room t room_id] removes the room from memory and, when present, from
    the event store. Existing {!val-events}, {!prev_batch}, {!has_gap} and
    {!val-gaps} handles are emptied and publish their new empty values before
    the room is discarded. A store failure is reported through {!last_error},
    but the in-memory removal still completes; other rooms are unaffected.

    The room is tombstoned until a real sync change or a newly enqueued pending
    send for it arrives. This prevents cancellation and late completion
    callbacks from recreating a forgotten room. *)

val is_forgotten : t -> Matrix_proto.Id.Room_id.t -> bool
(** [is_forgotten t room_id] reports whether the room is tombstoned. *)

val subscribe_forget_room :
  t -> Matrix_proto.Id.Room_id.t -> (unit -> unit) -> unit -> unit
(** [subscribe_forget_room t room_id callback] arranges for [callback] to be
    called after [forget_room t room_id] clears the room. It returns an
    idempotent function that removes the subscription. Callbacks are invoked
    outside the cache lock, so they may safely call cache operations. Ordinary
    callback exceptions are contained and logged; [Eio.Cancel.Cancelled]
    propagates. If the room is already forgotten, [callback] is invoked
    immediately instead. *)

val subscribe_physical_decryption :
  t -> (Matrix_proto.Id.Room_id.t -> unit) -> unit -> unit
(** [subscribe_physical_decryption t callback] calls [callback room_id] after a
    physical timeline record gains different plaintext through {!set_decrypted}.
    Detached external events do not notify. Callbacks are snapshotted under the
    cache lock and invoked afterwards; ordinary exceptions are logged and
    cancellation propagates. The returned idempotent function removes the
    subscription. *)

(** {1 Reading a room} *)

val events : t -> Matrix_proto.Id.Room_id.t -> event Observable.List.t
(** [events t room_id] is the room's cached events, oldest first. *)

val snapshot : t -> Matrix_proto.Id.Room_id.t -> event array
(** [snapshot t room_id] is {!val-events} as it stands. *)

val snapshot_with_gaps :
  t -> Matrix_proto.Id.Room_id.t -> event array * gap list
(** [snapshot_with_gaps t room_id] atomically captures the physical timeline and
    its gaps. External [/event] results are not included. *)

val with_snapshot_if_current :
  t ->
  Matrix_proto.Id.Room_id.t ->
  event array ->
  gap list ->
  (unit -> 'a) ->
  'a option
(** [with_snapshot_if_current t room_id events gaps callback] validates the
    physical timeline and gaps, then runs [callback] while still holding the
    cache lock. [None] means the snapshot was stale. The callback must not call
    back into {!Event_cache} or invoke user callbacks; it may acquire the
    sync-service lock. Exceptions, including cancellation, propagate. *)

val position :
  t -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.Event_id.t -> int option
(** [position t room_id event_id] is the event's index in {!val-snapshot}, or
    [None] where the room's cache does not hold it. It is what answers "what has
    arrived since?" without guessing at a clock. A caller persists the id of the
    last event it handled, asks for its position after a restart, and takes the
    events after it.

    The answer is bounded by what the cache holds. Without an {!Event_store} a
    fresh process starts empty and the first sync window is all there is, so an
    event older than that window is [None] and cannot be told apart from one
    this cache has never seen. A configured lazy store has the same bound until
    its older chunks are hydrated. A caller that wants the distinction paginates
    first. Local echoes have no event id and are never found. *)

val find_event :
  t ->
  Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Id.Event_id.t ->
  Matrix_proto.Event.Raw_event.t option
(** [find_event t room_id event_id] returns the effective event with that
    identifier from the bounded shared cache, including an event registered
    out-of-band by a focused view, or [None]. Decrypted plaintext is returned
    where available. An out-of-band event has no timeline position and is not
    present in {!val-events}. The lookup does not perform I/O. *)

val register_external_event :
  t -> Matrix_proto.Id.Room_id.t -> event:Matrix_proto.Event.Raw_event.t -> unit
(** [register_external_event t room_id ~event] remembers an identified server
    event without inserting it into the room's ordered timeline. It is intended
    for [/event] results used by focused views. Events without an event ID and
    events for forgotten rooms are ignored; an existing event with the same ID
    is replaced. The per-room external registry is bounded by the lesser of the
    cache's [max_events_per_room] and the store's detached-event bound, and is
    persisted with the room when a store is configured. It does not affect
    chunks, gaps or pagination. *)

val related_events :
  t ->
  Matrix_proto.Id.Room_id.t ->
  target:Matrix_proto.Id.Event_id.t ->
  ?rel_type:Matrix_proto.Event.Rel_type.t ->
  unit ->
  Matrix_proto.Event.Raw_event.t list
(** [related_events t room_id ~target ?rel_type ()] returns the cached raw
    events whose [m.relates_to.event_id] points at [target]. The optional
    relation type narrows the result. Physical timeline events and detached
    focused-view events are scanned, duplicate stable identities are removed,
    and the result is ordered chronologically (timestamp, then event ID).
    Physical copies win over detached copies. This is a cache-only operation; it
    never performs network I/O. *)

val prev_batch :
  t -> Matrix_proto.Id.Room_id.t -> string option Observable.Value.t
(** [prev_batch t room_id] is the token at the resident timeline's oldest edge,
    whether that chunk is a gap or a run of events. Consumers performing
    pagination must call {!hydrate_previous} first: while
    {!has_unloaded_history} is true, [None] alone does not prove that the room's
    beginning has been reached. *)

val has_gap : t -> Matrix_proto.Id.Room_id.t -> bool Observable.Value.t
(** [has_gap t room_id] is whether the resident timeline holds a gap
    {e anywhere}, not only at its oldest edge. A gap in the unloaded prefix
    becomes visible together with the events chunk that follows it. *)

val gaps : t -> Matrix_proto.Id.Room_id.t -> gap list Observable.Value.t
(** [gaps t room_id] is every resident gap, oldest first. *)

(** The outcome of one local persisted-chunk hydration. *)
type hydration =
  | Hydrated of event list
  | No_persisted_history
  | Hydration_failed of Event_store.Error.t

val hydrate_previous : t -> Matrix_proto.Id.Room_id.t -> hydration
(** [hydrate_previous t room_id] loads exactly one persisted events chunk
    immediately before the resident topology. It never crosses a resident server
    gap or performs network I/O. A successful hydration also brings that chunk's
    immediately preceding persisted gap into the resident topology and publishes
    the stable-id keyed observable update. Hydration may write the event store:
    it first retries any pending cache delta so lazy metadata cannot race stale
    rows, and atomically removes/promotes a detached copy when the newly
    physical event carries its plaintext. *)

val has_unloaded_history : t -> Matrix_proto.Id.Room_id.t -> bool
(** [has_unloaded_history] reports whether the room has persisted chunks that
    are not currently resident. *)

val undecrypted :
  t -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Event.Raw_event.t list
(** [undecrypted t room_id] is the resident cached [m.room.encrypted] events
    with no plaintext beside them, oldest first. After a restart over a
    {!Event_store.Ciphertext_only} store, older encrypted events join this set
    as pagination hydrates their chunks. {!Runtime} and {!Room_timeline} run
    {!Matrix_client.Encryption.decrypt_room_event} over resident records and
    feed successful results back through {!set_decrypted}. *)

(** {1 Filling a room} *)

val apply_room_change : t -> Matrix_client.Base_client.room_change -> unit
(** [apply_room_change t change] merges a sync delta, including its decryption
    results.

    A [limited] delta whose window shares no synced event with the history
    already held has a hole before it. A {!gap} carrying the response's
    [prev_batch] is pushed, the window goes after it, and the older history
    stays where it is for {!Room_timeline.paginate_gap} to reach later. A window
    that does overlap merges into what is held, which is what the first,
    [since]-less sync of a process that reloaded its cache looks like, so a
    reloaded history keeps its place. A response with no [prev_batch] cannot be
    given a gap, since there would be no token to fill it, so its events are
    appended as if contiguous. Unsent local echoes are moved back to the end. *)

val prepend :
  t ->
  Matrix_proto.Id.Room_id.t ->
  events:Matrix_proto.Event.Raw_event.t list ->
  prev_batch:string option ->
  unit
(** [prepend t room_id ~events ~prev_batch] splices a backwards page fetched
    from {!prev_batch} into the room's oldest edge. [events] is oldest first,
    and [prev_batch] is the response's [end] token, where [None] means the
    room's beginning was reached and drops the gap at that edge. Events already
    held are ignored. *)

type prepend_applied = { inserted : event list; reached_start : bool }
(** The exact cache outcome of an applied page. [reached_start] reflects the
    resulting cache topology, including the case where a non-empty page was
    entirely duplicate and its obsolete gap was closed. *)

type prepend_result =
  | Applied of prepend_applied
  | Stale
  | Forgotten
      (** The outcome of {!prepend_if_token}. [Applied.inserted] contains the
          newly inserted events in oldest-first order. [Stale] means another
          sync or pagination changed the room's oldest token while the request
          was in flight. [Forgotten] means the room was tombstoned before the
          response arrived. *)

val prepend_if_token :
  t ->
  Matrix_proto.Id.Room_id.t ->
  expected_prev_batch:string ->
  events:Matrix_proto.Event.Raw_event.t list ->
  prev_batch:string option ->
  prepend_result
(** [prepend_if_token ... ~expected_prev_batch] is {!prepend} guarded by an
    exact check of the room's current oldest token. The check, splice and
    persistence happen under one cache lock, so a late network response cannot
    insert into a position already changed by another operation. A forgotten
    room is never recreated. *)

val resolve_gap :
  t ->
  Matrix_proto.Id.Room_id.t ->
  gap:Gap_id.t ->
  events:Matrix_proto.Event.Raw_event.t list ->
  prev_batch:string option ->
  unit
(** [resolve_gap t room_id ~gap ~events ~prev_batch] splices a backwards page
    fetched from the gap's token into that gap's position. The events land
    between the gap and what follows it, and the gap is retargeted at
    [prev_batch] unless the page reached the room's beginning ([prev_batch] is
    [None]) or events the room already held on the older side, in which case the
    hole is closed and the gap goes. An unknown [gap] is ignored, as one that
    moved under a concurrent pagination would be. *)

val set_decrypted :
  t ->
  Matrix_proto.Id.Room_id.t ->
  encrypted:Matrix_proto.Event.Raw_event.t ->
  plaintext:Matrix_proto.Event.Raw_event.t ->
  bool
(** [set_decrypted t room_id ~encrypted ~plaintext] installs plaintext obtained
    after the original sync, answering whether the encrypted event was found. *)

(** {1 Local echoes} *)

val track_send_queue : t -> Matrix_client.Send_queue.t -> unit
(** [track_send_queue t queue] adds the queue's local echoes to the rooms they
    name and follows their delivery status. Call it once per queue. *)

val find_echo : t -> Matrix_client.Send_queue.request -> event option
(** [find_echo t request] is the cached event a queued send produced, and [None]
    where the cache holds none, which is what a cancelled request leaves behind.
    The same event answers before and after delivery, because the local echo and
    the server's copy of it are one cached event. *)

(** {1 Persistence} *)

val flush_room : t -> Matrix_proto.Id.Room_id.t -> unit
(** [flush_room t room_id] atomically writes the complete resident layout and
    resident event chunks, preserving any unloaded persisted prefix. It is how a
    caller makes the store consistent at a point of its own choosing. *)
