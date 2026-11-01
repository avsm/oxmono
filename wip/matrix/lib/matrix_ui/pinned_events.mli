(** The pinned-event projection for one room.

    This is a deliberately bounded view: the state event supplies the ordered
    identifiers, the shared {!Event_cache} supplies already-known events, and
    the Matrix [/event] endpoint fills only identifiers absent from that cache.
    Fetched events are registered in the shared cache as out-of-band values:
    they are discoverable by {!Event_cache.find_event} but have no timeline
    position. The registry is bounded and persisted when the cache has a store;
    it is cleared when its room is forgotten. *)

type t
(** The type for one room's pinned-event view. *)

val create :
  ?max_events_to_load:int ->
  client:Matrix_client.Client.t ->
  event_cache:Event_cache.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  unit ->
  t
(** [create ~client ~event_cache ~room_id ()] makes an empty view. Call
    {!refresh} with the current base-client state to populate it. At most
    [max_events_to_load] identifiers are resolved (128 by default); when the
    state contains more, the newest tail of its ordered list is retained. Zero
    is valid and produces an empty view without making requests. Negative values
    raise [Invalid_argument]. *)

val events : t -> Matrix_proto.Event.Raw_event.t Observable.List.t
(** [events t] is the ordered observable list of successfully resolved pinned
    events. *)

val snapshot : t -> Matrix_proto.Event.Raw_event.t array
(** [snapshot t] is the current pinned-event list. *)

val refresh :
  t ->
  state:Matrix_client.Base_client.state ->
  (unit, Matrix_client.Error.t) result
(** [refresh t ~state] reads the current [m.room.pinned_events] state event,
    ignores malformed or duplicate identifiers, resolves missing events in state
    order, enriches each target from bounded recursive relation pages with
    annotation and replacement events, and publishes a new list only after every
    requested event has resolved. A failed fetch leaves the previous list
    visible and retains successful fetches so retrying is deterministic. Both
    relation types are conservatively re-queried because the store has no
    completeness marker. Cancellation propagates. *)

val close : t -> unit
(** [close t] closes the view and clears its observable list. It is idempotent;
    later refreshes are no-ops. *)
