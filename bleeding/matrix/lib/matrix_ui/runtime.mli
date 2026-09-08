(** runtime — the event cache, room list, timelines, sync loop and send queue,
    wired together.

    A runtime owns the models a client renders and drives them from one sync
    loop. It is the supported way to build them; {!Event_cache.create},
    {!Room_list.create} and {!Room_timeline.create} are for a test or for wiring
    of a caller's own.

    A runtime does not own the switch it is built on. {!stop} ends the loop,
    {!close_timeline} releases a timeline, and everything else lives as long as
    the caller's switch does. *)

(** The type for what the sync loop is doing. *)
type sync_state =
  | Not_started
  | Syncing  (** The loop has started and no response has arrived yet. *)
  | Live of { batch : string }  (** The token the last response carried. *)
  | Failed of string
      (** The last request failure, formatted by {!Matrix_eio.Error.pp_err}. It
          is published for every failure, retried or not, so a retry that
          continues through {!Offline} and succeeds returns to {!Live}; one that
          the loop gives up on is followed by {!Stopped}. *)
  | Offline
      (** The last request failed and the loop is waiting or retrying. It does
          not run a separate connectivity supervisor; the next sync request is
          what returns the runtime to {!Live}. *)
  | Stopped
      (** The loop has stopped and will not restart, because {!stop} was called
          or [on_error] answered [Stop]. *)

type t
(** The type for runtimes. *)

val create :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  client:Matrix_eio.Client.t ->
  sync:Matrix_eio.Sync_service.t ->
  ?encryption:Matrix_eio.Encryption.t ->
  ?recovery_manager:Matrix_client.Recovery.Manager.t ->
  ?verification:Matrix_eio.Verification_service.t ->
  ?event_store:Event_store.t ->
  ?send_queue:Matrix_client.Send_queue.t ->
  ?send_queue_store:Matrix_client.Store.t ->
  ?send_queue_media_store:Matrix_client.Media_store.t ->
  ?send_queue_media_owner:string ->
  ?on_room_key_request:(Matrix_eio.Encryption.request -> unit) ->
  ?utd_hook:Utd_hook.t ->
  ?device_created_at:Ptime.t ->
  unit ->
  t
(** [create ~sw ~clock ~client ~sync ()] builds the UI services. Without
    [send_queue] a queue is created for the logged-in identity. With
    [encryption] the sync loop decrypts and the queue encrypts, and with
    [verification] the loop answers verification requests. [event_store]
    persists the event cache and stays the caller's to close. When [send_queue]
    is absent, [send_queue_store], [send_queue_media_store] and
    [send_queue_media_owner] are passed to the newly created queue as its
    [store], [media_store] and [media_owner], so queued sends and upload bytes
    can survive a restart. These stores also stay the caller's to close.
    Supplying any of these construction arguments with an explicit [send_queue]
    raises [Invalid_argument], since that queue has already fixed its
    persistence dependencies. [on_room_key_request] receives each automatically
    generated [m.room_key_request] for a distinct room, session and sender key;
    when absent, requests are sent through [client]. Only retryable unknown
    sessions with a valid or absent sender key are requested, and pending
    identities are bounded. [utd_hook], when supplied, receives deduplicated
    reports for undecryptable timeline events. [device_created_at] supplies
    local age metadata for those reports when the caller has retained it.
    [recovery_manager], when supplied, is projected into {!recovery_state} and
    refreshed after each committed sync; its subscription is removed when [sw]
    is released. Nothing runs until {!start}. *)

val event_cache : t -> Event_cache.t
(** [event_cache t] is the cache every model over [t] reads. *)

val pinned_events : t -> Matrix_proto.Id.Room_id.t -> Pinned_events.t
(** [pinned_events t room_id] returns the runtime-owned pinned-event view for
    [room_id]. It performs an initial best-effort refresh, then refreshes the
    view after later committed sync changes. The view is closed and removed by
    {!forget} and {!stop}. *)

val event_focused :
  t ->
  Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Id.Event_id.t ->
  ?limit:int ->
  ?thread_mode:Event_focused.thread_mode ->
  unit ->
  Event_focused.t
(** [event_focused t room_id event_id ?limit ?thread_mode ()] creates an
    event-focused view using the runtime's shared event cache. Events fetched by
    the view are therefore available to cache-only consumers, including
    {!pinned_events}, and survive when the runtime's event store is configured.
    The returned short-lived view is caller-owned and must be closed. *)

val thread_list : t -> Matrix_proto.Id.Room_id.t -> Thread_list.t
(** [thread_list t room_id] returns the runtime-owned paginated thread list for
    [room_id]. Repeated calls return the same handle until the room is forgotten
    or the runtime is stopped; a later call then creates a fresh handle. *)

val thread_info : t -> Thread_info.t
(** [thread_info t] is the persisted observable thread-summary aggregate. *)

val thread_cache : t -> Thread_cache.t
(** [thread_cache t] is the durable per-thread event projection. *)

val room_list : t -> Room_list.t
(** [room_list t] is the room list the sync loop republishes. *)

val send_queue : t -> Matrix_client.Send_queue.t
(** [send_queue t] is the queue [t] sends through, the one given to {!create} or
    the one it made. *)

val sync_service : t -> Matrix_eio.Sync_service.t
(** [sync_service t] is the sync service given to {!create}, which is where the
    room state, the members and the receipts live. *)

val sync_state : t -> sync_state Observable.Value.t
(** [sync_state t] is what the loop is doing, {!Not_started} until {!start}. *)

val recovery_state : t -> Matrix_client.Recovery.state Observable.Value.t option
(** [recovery_state t] is the optional recovery projection supplied by
    [recovery_manager]. It is seeded at construction and follows committed sync
    state changes. *)

val room_identity : t -> Room_identity.t
(** [room_identity t] is the room-scoped identity-warning projection. It is
    empty when [create] was given no encryption machine. *)

val typing_users :
  t ->
  Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Id.User_id.t list Observable.Value.t
(** [typing_users t room_id] is the ordered list from the latest valid
    [m.typing] event for [room_id], with the logged-in user removed. It starts
    empty, stays unchanged on responses without a valid typing event, and is
    cleared by the server's empty event or by {!forget}. Repeated calls return
    the same observable until the room is forgotten; a later call then creates a
    fresh empty one. This is ephemeral UI state and is never persisted. *)

val timeline :
  ?event_filter:Room_timeline.event_filter ->
  ?resolve_mxc:(string -> string option) ->
  t ->
  Matrix_proto.Id.Room_id.t ->
  Room_timeline.t
(** [timeline ?event_filter t room_id] is the room's shared timeline, created on
    the first call. [event_filter] is passed to {!Room_timeline.create}; when
    the room already has a timeline, its original filter and MXC resolver remain
    in force. It stays alive until {!close_timeline}. *)

val close_timeline : t -> Matrix_proto.Id.Room_id.t -> unit
(** [close_timeline t room_id] unsubscribes the room's timeline from the event
    cache and forgets it, so that the fibers it forked return and a later
    {!timeline} builds a fresh one. It does nothing for a room with no open
    timeline. *)

(** {1 Membership} *)

val join :
  t -> Matrix_proto.Id.Room_id.t -> (unit, Matrix_client.Error.t) result
(** [join t room_id] joins the room, or accepts its invitation, by id, which is
    what makes the {!Room_list.Invites} section actionable. It is [M_FORBIDDEN]
    where the room is invite-only and the user was not invited, and joining a
    room the user is already in succeeds. The call goes to the homeserver and
    does not touch the models, so {!Room_list.find} reports the old membership
    until the sync that carries the new one arrives, which is one round of the
    loop and not a wait the caller has to arrange. {!Matrix_client.Rooms.join}
    is the endpoint, and also takes an alias and the [via] servers a remote room
    needs. When the runtime has an encryption machine and the current sync state
    has an invitation with a discoverable inviter, a successful join records
    that acceptance for the machine's pending key-bundle handling. If saving
    that updated encryption state fails, the error is returned after the HTTP
    join has succeeded. *)

val join_room :
  t ->
  room_id_or_alias:Matrix_client.Directory.room_id_or_alias ->
  ?via:string list ->
  unit ->
  (unit, Matrix_client.Error.t) result
(** [join_room t ~room_id_or_alias ?via ()] is the alias-aware form of {!join}.
    An alias is resolved before the current invited-room state is inspected, so
    an inviter can be captured before the join request races a sync. The
    original ID or alias and the supplied [via] servers are passed unchanged to
    {!Matrix_client.Rooms.join}. When the server returns a room ID, a successful
    invited join records the acceptance against that returned ID. It does not
    amend the base-client room model. *)

val leave :
  t -> Matrix_proto.Id.Room_id.t -> (unit, Matrix_client.Error.t) result
(** [leave t room_id] leaves the room, which is also how an invitation is
    declined. The room moves to the {!Room_list.Historical} section, on the same
    sync as {!join} and not before it, and stays there until
    {!Matrix_client.Rooms.forget}. *)

val forget :
  t -> Matrix_proto.Id.Room_id.t -> (unit, Matrix_client.Error.t) result
(** [forget t room_id] first asks the homeserver to forget the room. If that
    succeeds, it invalidates older in-flight sync responses, best-effort removes
    the room from remote [m.direct] account data when it was a direct room,
    synchronously empties and closes the open timeline, and removes the room
    from the event cache, base state, receipts, thread subscriptions, and
    durable send queue. A failed server request leaves all local state
    untouched. Successful remote [m.direct] cleanup is also reflected in the
    local account-data projection.

    Event-store failures are retained in {!Event_cache.last_error};
    account-data, thread-store, and queue-store failures are logged and do not
    turn a successful server forget into a retryable HTTP result. Failure to
    persist the base state is raised as [Eio.Io], as for
    {!Matrix_eio.Sync_service.forget_room}.

    A send already in flight may still reach the homeserver, but it is detached
    from persistence and its late callback cannot send a compensating redaction
    or recreate a local echo. A later sync or deliberately newly queued pending
    send may recreate the room's local projections. *)

(** {1 Running} *)

val start :
  ?params:Matrix_client.Sync.params ->
  ?on_change:
    (Matrix_client.Base_client.state ->
    Matrix_client.Base_client.changes ->
    unit) ->
  ?on_error:(Matrix_eio.Error.err -> Matrix_eio.Sync.action) ->
  ?on_encryption_error:(Matrix_eio.Error.err -> unit) ->
  t ->
  unit
(** [start t] starts the sync and send-queue services under the switch given to
    {!create}. [params] is what each [/sync] asks for, and defaults to
    {!Matrix_client.Sync.default_params}. [on_change] is called after the models
    have taken each response.

    [on_encryption_error], when supplied, observes failures from individual
    encryption-side requests and crypto-state persistence without stopping the
    remainder of that sync response from being folded. Cancellation still
    propagates normally.

    Every failed request sets {!val-sync_state} to {!Failed} before [on_error]
    is consulted, so a caller that only wants to observe a failure need not pass
    one. A [Continue] or [Retry_after] decision then publishes {!Offline} while
    the loop retries; the next successful response publishes {!Live}. [on_error]
    answering [Stop] publishes {!Stopped} after the observable {!Failed}.
    Without it the loop retries what {!Matrix_eio.Error.is_retryable} calls
    transient, with the 500 ms to 60 s backoff {!Matrix_eio.Sync_service.run}
    uses, and stops on anything else, an [M_UNKNOWN_TOKEN] included.

    Raises [Invalid_argument] if [t] was started already. *)

val stop : t -> unit
(** [stop t] cancels the fibers {!start} forked and publishes {!Stopped}. They
    run under a switch of the runtime's own. It also closes and forgets every
    timeline the runtime has handed out, unsubscribing their event-cache
    observers without cancelling anything else on the switch given to {!create}.
    Idempotent, and harmless before {!start}, which it does not prevent. It
    returns as soon as the service cancellation is requested, and those fibers
    unwind on the next scheduler turn. A later {!timeline} call constructs a
    fresh projection. *)
