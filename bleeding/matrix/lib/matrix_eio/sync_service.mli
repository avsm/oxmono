(** sync_service — the [/sync] loop that maintains room state.

    {!Matrix_client.Base_client} is the pure fold from a response to room state.
    This is the fiber that keeps calling [/sync] and feeding it, with reconnect
    backoff, cancellation through the switch, and persistence to a
    {!Matrix_client.Store} after each response.

    {[
    Eio.Switch.run @@ fun sw ->
    let store = Matrix_client.Store.on_disk ~dir in
    let service =
      Matrix_eio.Sync_service.of_store ~store
        ~user_id:(Matrix_eio.Client.user_id client)
        ()
    in
    Matrix_eio.Sync_service.run ~sw ~clock client service
      ~on_change:(fun _state changes ->
        List.iter
          (fun (c : Matrix_client.Base_client.room_change) ->
            print_endline (Matrix_client.Base_client.display_name c.info))
          changes.room_changes)
      ()
    ]} *)

type state = Matrix_client.Base_client.state
(** The client's picture of the world. *)

type changes = Matrix_client.Base_client.changes
(** Everything one response moved. *)

type room_change = Matrix_client.Base_client.room_change
(** What one response moved in one room. *)

type decrypted = Matrix_client.Base_client.decrypted
(** A timeline event that arrived encrypted and came back out. *)

(** What a callback asks the loop to do next, as in {!Sync}. *)
type action = Sync.action = Continue | Stop | Retry_after of float

(** {1 The service} *)

type t
(** The type for services. A service holds the state, the hooks and the store
    the loop writes through. *)

val create : ?store:Matrix_client.Store.t -> state -> t
(** [create state] wraps an existing pure state. [store] is written after every
    applied response, so the caller never has to flush it, and defaults to no
    persistence. *)

val of_store :
  store:Matrix_client.Store.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?display_name:string ->
  ?ruleset:Matrix_proto.Push.Ruleset.t ->
  unit ->
  t
(** [of_store ~store ~user_id ()] resumes from what [store] holds. Its sync
    token, room summaries, account data and receipts become the initial state,
    so the first request is an incremental sync. [display_name] and [ruleset]
    are as in {!Matrix_client.Base_client.of_store}. *)

val of_user :
  user_id:Matrix_proto.Id.User_id.t ->
  ?display_name:string ->
  ?ruleset:Matrix_proto.Push.Ruleset.t ->
  ?plaintext_policy:Matrix_client.Store.plaintext_policy ->
  unit ->
  t
(** [of_user ~user_id ()] is a service with no persistence, so every run starts
    from an initial sync. [display_name] and [ruleset] are as in
    {!Matrix_client.Base_client.create}; [plaintext_policy] defaults to
    {!Matrix_client.Store.Ciphertext_only}, while
    {!Matrix_client.Store.Store_plaintext} explicitly permits decrypted event
    contents at rest. *)

val state : t -> state
(** [state t] is the state as the last applied response left it. *)

val store : t -> Matrix_client.Store.t option
(** [store t] is the optional persistent store used by [t]. *)

val forget_room : t -> Matrix_proto.Id.Room_id.t -> unit
(** [forget_room t room_id] removes the room and its room-scoped receipts from
    the service state and store, then flushes the store when present. Global
    account data is preserved. *)

val remove_direct_room : t -> Matrix_proto.Id.Room_id.t -> unit
(** [remove_direct_room t room_id] updates the service's local [m.direct]
    projection after its corresponding remote account-data write succeeds and
    persists it when a store is present. Any response staged from the previous
    state is invalidated. *)

val members : t -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.User_id.t list
(** [members t room_id] is everybody this service has seen join or be invited to
    [room_id], which is what {!Encryption.encrypt_room_event} and
    {!Send_queue.start} want. See {!Matrix_client.Base_client.members} for how
    complete it is. *)

val replace_members :
  t -> Matrix_proto.Id.Room_id.t -> Matrix_client.Rooms.member list -> unit
(** [replace_members t room_id members] installs an authoritative [/members]
    result in the base state and persists it. This is the state-side half of a
    lazy member refresh; see {!Matrix_client.Base_client.replace_members}. Any
    response staged from the previous state is invalidated. *)

val on_response :
  t -> (state -> Matrix_proto.Sync.Response.t -> changes -> unit) -> unit
(** [on_response t f] registers [f] to run once per applied response. See
    {!Matrix_client.Base_client.Hooks.on_response}. *)

val on_sliding_response :
  t ->
  (state -> Matrix_proto.Sliding_sync.Response.t -> changes -> unit) ->
  unit
(** [on_sliding_response t f] registers [f] to run once per applied MSC4186
    response, after its state and common store have been committed. *)

val on_room_event :
  t ->
  (Matrix_proto.Id.Room_id.t -> Matrix_proto.Event.Raw_event.t -> unit) ->
  unit
(** [on_room_event t f] registers [f] to run for every new timeline event of
    every room. See {!Matrix_client.Base_client.Hooks.on_room_event}. *)

val on_profile_change :
  t -> (state -> Matrix_client.Base_client.profile_change list -> unit) -> unit
(** [on_profile_change t f] registers [f] for effective global-profile changes
    committed through {!apply_profile_updates} or {!clear_profiles}. The
    callback receives the committed base state and changes after persistence;
    non-cancellation exceptions are logged and cancellation propagates. *)

type profile_subscription
(** A cancellable registration for global-profile changes. *)

val subscribe_profile_changes :
  t ->
  (state -> Matrix_client.Base_client.profile_change list -> unit) ->
  profile_subscription
(** [subscribe_profile_changes t f] registers [f] for effective global-profile
    changes committed after persistence. Unlike {!on_profile_change}, the
    registration can be removed with {!unsubscribe_profile_changes}. The
    callback is not called immediately; no-op updates do not notify it.
    Non-cancellation exceptions are logged and isolated, while
    [Eio.Cancel.Cancelled] propagates. *)

val unsubscribe_profile_changes : t -> profile_subscription -> unit
(** [unsubscribe_profile_changes t subscription] removes [subscription]. It is
    idempotent and may be called from inside the callback. *)

val apply_profile_updates :
  t ->
  Matrix_proto.Sliding_sync.Response.profiles ->
  Matrix_client.Base_client.profile_change list
(** [apply_profile_updates t updates] folds an MSC4262 profile delta into the
    active base state. Effective changes are persisted to the common store and
    committed before callbacks run. A persistence failure raises [Eio.Io] and
    leaves the active state and in-memory common profile snapshot unchanged.
    No-op updates do not flush, advance the service generation or notify. *)

val clear_profiles : t -> Matrix_client.Base_client.profile_change list
(** [clear_profiles t] removes every global profile from the active state and
    common store, returning deterministic changes. It is a no-op when no profile
    is present. *)

val persist : t -> unit
(** [persist t] writes the current state into the service's store and flushes it
    to disk. {!run} does this after each response, so a caller needs it only to
    save state it changed itself. It does nothing without a store.

    Raises [Eio.Io] with [Error.E e] if the store cannot be written. *)

val set_local_unread_counts_if_current :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  expected:state ->
  Matrix_client.Read_state.counts ->
  state option
(** [set_local_unread_counts_if_current t ~room_id ~expected counts] commits a
    room-only counter update only when [expected] is still the active physical
    state. [None] means stale; [Some] is the unchanged or committed state.
    Persistence writes only the room record; a failed flush restores it and
    raises after releasing the service lock. *)

val bootstrap_push_rules : Client.t -> t -> unit
(** [bootstrap_push_rules client t] installs the server's push rules when
    neither [t]'s state nor its common store has a valid [m.push_rules] event.
    Existing synced/stored rules perform no request. A successful endpoint
    result is persisted before publication; a later synced event wins a
    concurrent bootstrap. [M_NOT_FOUND], unsupported/404 responses and a
    malformed response retain the current server/default rules. Other client
    errors are raised, and cancellation propagates. *)

val begin_forget : t -> unit
(** [begin_forget t] invalidates any response already fetched by the sync loop.
    Call it immediately after a server forget succeeds and before yielding to
    account-data cleanup. *)

(** {1 Applying a response} *)

val apply :
  ?coverage:Matrix_client.Base_client.state_coverage ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  Client.t ->
  t ->
  Matrix_proto.Sync.Response.t ->
  changes
(** [apply client t response] folds one response in, through the encryption
    machine and a staged base state, commits that state and its store together,
    then runs the hooks. It is the changes that committed. [coverage] defaults
    to {!Matrix_client.Base_client.unknown_state_coverage}; {!sync_once} and
    {!run} derive it from their request parameters. [encryption] and
    [verification] are as in {!run}.

    A failure on the encryption side is logged rather than raised, and the
    response normally still commits, so a transient crypto request does not lose
    the [next_batch]. [on_encryption_error], when supplied, receives each caught
    encryption-side error after it is logged; exceptions from that callback are
    logged and do not prevent folding. Pending MSC4268 invite-acceptance records
    are cleared when the current room state is left/knocked/missing or their
    24-hour window expires. A retained authenticated room-key bundle for a
    joined room is then advanced through a fresh inviter-key query, encrypted
    media download and trust-gated import before the crypto snapshot is saved;
    transient failures remain durable for a later sync. The guarded loop drops
    and refetches a staged response if a concurrent room-forget or authoritative
    member/account-data update invalidates its input state before commit. A
    caller invoking [apply] directly is responsible for serializing it with
    those local mutation functions; {!sync_once} and {!run} provide the guarded
    behaviour. *)

val generation : t -> int
(** [generation t] is the lifecycle generation used to reject stale network
    responses. *)

val migrate_legacy_sliding_state : t -> bool
(** [migrate_legacy_sliding_state t] atomically removes the private snapshot
    written by the former standalone sliding-sync fold and, when the common
    state has no sliding position, replays that snapshot into it. An existing
    common position wins. A legacy snapshot without a position is discarded. The
    migration persists before publishing, invalidates in-flight responses, and
    invokes no hooks. It returns [true] when a legacy slot was consumed and
    [false] when there is no store or no slot.

    A load or persistence failure raises [Eio.Io] after releasing the lifecycle
    lock and leaves the active state and in-memory store unchanged. *)

val reset_sliding_session : t -> unit
(** [reset_sliding_session t] drops sliding cursors and list metadata, persists
    the reset in the common store, and invalidates in-flight responses. *)

val apply_sliding :
  ?to_device_enabled:bool ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  Client.t ->
  t ->
  Matrix_proto.Sliding_sync.Response.t ->
  changes
(** [apply_sliding client t response] processes crypto before folding an MSC4186
    response, commits the state and common store, then runs sliding hooks. *)

val apply_sliding_if_current :
  int ->
  ?to_device_enabled:bool ->
  ?on_committed:(state -> changes -> unit) ->
  ?before_commit:(unit -> unit) ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  Client.t ->
  t ->
  Matrix_proto.Sliding_sync.Response.t ->
  changes option
(** Guarded form of {!apply_sliding}; [None] means the generation is stale.
    [before_commit], when supplied, runs while the generation check and common
    persistence are serialized under the service lifecycle lock. It can stage an
    auxiliary response store before the common cursor is persisted; raising
    aborts the common commit. [on_committed] runs after durable publication and
    before service hooks. *)

val sync_once :
  Client.t ->
  t ->
  ?params:Matrix_client.Sync.params ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  unit ->
  Matrix_proto.Sync.Response.t * changes
(** [sync_once client t ()] is one iteration of {!run}, for a caller driving the
    loop itself. It performs the request and is the response together with the
    changes folding it in produced. [params] defaults to
    {!Matrix_client.Sync.default_params}, and its [since] is replaced by the
    service's own token.

    Raises [Eio.Io] with [Error.E e] on a failed request. *)

(** {1 Running} *)

val run :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  t ->
  ?params:Matrix_client.Sync.params ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_response:(Matrix_proto.Sync.Response.t -> action) ->
  ?on_error:(Error.err -> action) ->
  ?on_encryption_error:(Error.err -> unit) ->
  on_change:(state -> changes -> unit) ->
  unit ->
  unit
(** [run ~sw ~clock client t ~on_change ()] forks a fiber on [sw] that syncs
    until a callback returns {!Stop} or [sw] is released. It returns as soon as
    the fiber is forked.

    [on_change] receives the new state and changes immediately after the store
    commit and before extension hooks run. This ordering lets the runtime
    publish the committed projection before a hook performs a reentrant local
    lifecycle operation. [on_response] is an extra callback on the raw response
    whose {!type-action} decides whether the loop goes on, and defaults to
    {!Continue}. [on_error] decides after a failed request, and defaults to an
    exponential backoff from 500 ms to a 60 s ceiling, reset by the next
    success.

    [encryption] is a machine run over every response before anything else sees
    it. To-device events are processed and the requests they produce performed,
    encrypted timeline events are decrypted into
    {!Matrix_client.Base_client.room_change.decrypted}, the members of encrypted
    rooms are tracked, and the machine is saved. Without it, encrypted events
    stay encrypted. [verification] receives the [m.key.verification.*] to-device
    events, and is ignored without [encryption], which is what decrypts them.

    [params] defaults to {!Matrix_client.Sync.default_params} and its [since] is
    ignored, because the loop takes the token from the state. A service built
    with {!of_store} therefore resumes where the last process stopped rather
    than asking for an initial sync. When [params.set_presence] is [None], each
    poll reads the current client-owned presence. An effective change through
    {!Client.set_sync_presence} cancels an in-flight poll or retry delay and
    restarts it with the same [since] token; the stale response is not folded
    and does not invoke [on_change], [on_response] or [on_error]. An explicit
    [Some] override is fixed for the run and does not install this wakeup. Since
    the wakeup cancels an Eio request, callers should update the client presence
    on the same Eio domain as this loop.

    An exception out of a callback, out of a hook or out of the store leaves the
    loop and fails [sw]. *)
