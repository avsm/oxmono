(** sliding_sync — simplified sliding sync (MSC4186), as a fiber.

    The protocol of {!Matrix_client.Sliding_sync}, with failures raised as
    [Eio.Io] rather than returned and the loop forked onto a switch, so that
    releasing the switch ends it. {!Matrix_proto.Sliding_sync} documents the
    request and response types and how a session is built. The loop speaks
    {!Sync.type-action} and {!Sync.type-callbacks}, as the [/sync] loop does. *)

module Request = Matrix_proto.Sliding_sync.Request
(** What one sliding-sync call asks for, and the builders that assemble it. *)

module Response = Matrix_proto.Sliding_sync.Response
(** What one sliding-sync call answers with. *)

(** {1 Mutable request control} *)

module Controller : sig
  type t
  (** A fiber-safe sliding-sync request whose room subscriptions can be changed
      while {!sync_forever_controlled} is polling. Mutations must run on the
      same Eio domain as the loop, because Eio cancellation contexts are
      domain-local.

      The rest of the request is immutable. Subscription mutations normally
      cancel the current long poll when they make an effective change, so the
      next request carries the new set immediately. This internal cancellation
      is not reported through [on_error] and does not advance [pos]. One
      controller may drive at most one loop at a time; it can be reused after
      that loop stops. *)

  val create : Request.t -> t
  (** [create request] starts with [request], normalising its room subscriptions
      into deterministic room-id order. *)

  val request : t -> Request.t
  (** [request t] is the latest request snapshot. *)

  val add_room_subscriptions :
    ?cancel_in_flight_request:bool ->
    ?required_state:Matrix_proto.Sliding_sync.Required_state.t list ->
    ?timeline_limit:int ->
    room_ids:Matrix_proto.Id.Room_id.t list ->
    t ->
    unit
  (** Add or update [room_ids], preserving subscriptions for other rooms.
      [required_state] defaults to [[]], [timeline_limit] to [10], and
      [cancel_in_flight_request] to [true]. Repeating the exact settings is a
      no-op and does not cancel the poll. *)

  val remove_room_subscriptions :
    ?cancel_in_flight_request:bool ->
    room_ids:Matrix_proto.Id.Room_id.t list ->
    t ->
    unit
  (** Remove [room_ids]. Missing rooms are a no-op. *)

  val set_room_subscriptions :
    ?cancel_in_flight_request:bool ->
    ?required_state:Matrix_proto.Sliding_sync.Required_state.t list ->
    ?timeline_limit:int ->
    room_ids:Matrix_proto.Id.Room_id.t list ->
    t ->
    unit
  (** Make [room_ids] the exact subscription set and give every room the same
      settings. An identical set and settings is a no-op. *)

  val reset_and_add_room_subscriptions :
    ?cancel_in_flight_request:bool ->
    ?required_state:Matrix_proto.Sliding_sync.Required_state.t list ->
    ?timeline_limit:int ->
    room_ids:Matrix_proto.Id.Room_id.t list ->
    t ->
    unit
  (** Clear every subscription and add [room_ids]. Like the Rust SDK operation,
      resetting a non-empty set is an effective change even when the resulting
      values compare equal; empty-to-empty is a no-op. *)
end

(** {1 Own profile projection} *)

module Own_profile : sig
  type t
  (** A projection of the logged-in user's MSC4262 global profile.

      This is deliberately a bridge over
      {!type:Matrix_client.Base_client.state}: it does not fold the common
      [/sync] base state, and it only changes when the sliding-sync loop has
      accepted and (when configured) durably saved a response. *)

  type subscription
  (** A registration for profile changes. *)

  val create : user_id:Matrix_proto.Id.User_id.t -> t
  (** [create ~user_id] makes an initially empty projection for [user_id]. *)

  val user_id : t -> Matrix_proto.Id.User_id.t
  (** [user_id t] is the account whose profile [t] projects. *)

  val current : t -> Matrix_client.Profile.profile option
  (** [current t] is the latest committed profile, or [None] when no profile has
      been received or the server dropped it. Unlike the Rust SDK observer, this
      deliberately preserves absence as [None] instead of collapsing it into a
      profile whose optional fields are all empty. *)

  val refresh : t -> Matrix_client.Base_client.state -> unit
  (** [refresh t state] folds the own-user profile from an already accepted
      sliding-sync [state]. The loop calls this only after its persistence
      boundary; callers normally do not need to call it directly. *)

  val refresh_base : t -> Sync_service.state -> unit
  (** [refresh_base t state] refreshes from the common base state used by a
      service-backed sliding-sync loop. *)

  val subscribe :
    t -> (Matrix_client.Profile.profile option -> unit) -> subscription
  (** [subscribe t f] registers [f], first calling it with the current profile
      (including [None]) and then with each distinct committed value. Listener
      exceptions are logged and isolated; [Eio.Cancel.Cancelled] propagates. *)

  val unsubscribe : t -> subscription -> unit
  (** [unsubscribe t s] removes [s]. Idempotent. *)

  val watch :
    t -> (Matrix_client.Profile.profile option -> unit) -> unit -> unit
  (** [watch t f] is [subscribe t f] followed by an unsubscribe function. *)
end

(** {1 One request} *)

val path : string
(** [path] is the unstable endpoint,
    [/_matrix/client/unstable/org.matrix.simplified_msc3575/sync]. There is no
    stable path. *)

val default_timeout_ms : int
(** [default_timeout_ms] is [30_000], the poll timeout a call sends when it is
    given none. *)

val native_feature : string
(** The [/_matrix/client/versions] feature flag for the native endpoint. *)

val is_available_in : Matrix_client.Server.versions -> bool
(** [is_available_in versions] is {!Matrix_client.Sliding_sync.is_available_in}.
*)

val is_available : Client.t -> bool
(** [is_available client] fetches [/_matrix/client/versions] and unwraps
    {!Matrix_client.Sliding_sync.is_available}. *)

val sync_once :
  Client.t ->
  ?pos:string ->
  ?timeout_ms:int ->
  ?set_presence:[ `Online | `Offline | `Unavailable ] ->
  Request.t ->
  Response.t
(** [sync_once client request] posts one sliding-sync request. [pos] continues
    an existing session, and omitting it starts a new one, which is expensive
    for the server. [timeout_ms] is how long the server may hold the request
    open, and defaults to {!default_timeout_ms}.

    Raises [Eio.Io] with [Error.E e] on any failure, a homeserver that does not
    implement MSC4186 included, which arrives as [Error.Matrix] with
    [M_UNRECOGNIZED]. *)

(** {1 Classifying a failure} *)

val is_unsupported : Error.err -> bool
(** [is_unsupported e] is [true] for the error a homeserver without MSC4186
    gives, which is a bare [404] or a Matrix error with code [M_UNRECOGNIZED]. A
    caller can fall back to {!Sync} on it. *)

val is_expired_pos : Error.err -> bool
(** [is_expired_pos e] is [true] for [M_UNKNOWN_POS], which the server sends
    when the session behind [pos] is gone. {!sync_forever} recovers from it on
    its own. *)

(** {1 Looping} *)

val sync_forever :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  ?initial_pos:string ->
  ?timeout_ms:int ->
  ?txn_id:bool ->
  ?set_presence:[ `Online | `Offline | `Unavailable ] ->
  ?thread_subscription_store:Matrix_client.Store.t ->
  ?state_store:Matrix_client.Store.t ->
  ?own_profile:Own_profile.t ->
  ?profile_service:Sync_service.t ->
  ?service:Sync_service.t ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  ?on_change:(Sync_service.state -> Sync_service.changes -> unit) ->
  callbacks:Response.t Sync.callbacks ->
  Request.t ->
  unit
(** [sync_forever ~sw ~clock client ~callbacks request] forks a fiber on [sw]
    that syncs until a callback returns {!Sync.Stop} or [sw] is released. It
    returns as soon as the fiber is forked.

    [initial_pos] continues an existing session. [pos] and the to-device [since]
    token are threaded across iterations, and the whole request is resent each
    time because MSC4186 has no sticky parameters. To change the request, return
    {!Sync.Stop} and start a new loop with the new one. Without [state_store],
    [pos] advances only after [on_response] returns, so a handler that raises
    leaves it where it was and the next request replays the same window. With a
    [state_store], the complete accumulated state (including the new [pos]) is
    flushed before [on_response]. A callback exception does not roll that state
    back; a restarted loop resumes after the stored response and does not
    redeliver its callback.

    [service] selects the common {!Sync_service}/{!Matrix_client.Base_client}
    fold. Its persisted sliding position and to-device cursor override
    [initial_pos] and the request's [since]. At startup, a former standalone
    [state_store] snapshot in the same common store is consumed exactly once: an
    existing common position wins; otherwise its cursor, rooms, lists and
    profiles are migrated before the first request. Each response is persisted
    and published before [on_change] and [on_response]. [encryption] processes
    to-device keys before the common room fold, and [verification] receives its
    decrypted verification events. These options, [on_encryption_error], and
    [on_change] require a canonical service, whether supplied as [service] or
    created privately by the compatibility entry point. With an explicit
    [service], [state_store] and [profile_service] are incompatible. Without
    [service], [state_store] is used to construct the private canonical service,
    and [profile_service] is accepted only as that service (it is not a second
    profile fold); when both are supplied they must share the exact same
    [Store.t]. The service, logged-in client, and [own_profile] must belong to
    the same user.

    [txn_id] defaults to [false]. Set it to [true] to mint a fresh transaction
    id per request, so that the server can report which parameters it saw.

    [set_presence], when present, is used on every poll as the [set_presence]
    query parameter, using [online], [offline], or [unavailable]; [online] is
    omitted because it is the Matrix wire default. When absent, the client-owned
    presence default is used. An effective change to that default while a poll
    is in flight, or while the loop is waiting after [Retry_after], cancels and
    restarts the poll with the latest value, preserving the accepted [pos] and
    invoking none of the callbacks or persistence hooks for the cancelled
    response. An explicit [set_presence] disables this wakeup. Since the wakeup
    cancels an Eio request or timer, callers should update the client presence
    on the same Eio domain as this loop.

    [thread_subscription_store], when supplied, persists MSC4308 changes and
    queues any [prev_batch] range before [on_response] runs or [pos] advances.
    The request should enable the thread-subscriptions extension.

    On [M_UNKNOWN_POS] the session has expired, so [pos], the to-device cursor,
    and room subscriptions are cleared before [on_error] is consulted. The
    canonical service preserves common rooms, profiles, account data and the
    independent classic-sync cursor. With [state_store], the complete common
    accumulated state is loaded before the first request and saved before each
    response callback; a stored position and to-device token take precedence
    over the caller's initial values. When [own_profile] is supplied, the
    logged-in user's accumulated MSC4262 profile is published after the state
    snapshot has been saved and before [on_response]. A stored profile is
    published while the loop starts. A failed save publishes neither the
    response nor a profile change. On [M_UNKNOWN_POS], the projection is cleared
    only after the empty state has been saved successfully. Persistence failures
    are reported through [on_error] and retry the same position without invoking
    [on_response]. The compatibility [profile_service], when supplied without
    [service], is the canonical service and applies profile deltas as part of
    the common fold. Effective changes are persisted to its common store and are
    available through {!Sync_service.on_profile_change}; a failure retries the
    same response without invoking [on_response] or [own_profile]. A later fold
    failure may leave the common profile one response ahead, so retries must be
    idempotent. On [M_UNKNOWN_POS] the common profile is cleared before the
    error is published. If it is paired with [state_store], they must be the
    same [Store.t] handle. Nothing else is retried, so backoff is [on_error]'s
    business, through {!Sync.Retry_after}.

    Sliding state and [thread_subscription_store] are flushed independently; no
    atomic commit is possible when they are distinct stores. If the second flush
    fails, retrying the same position safely reapplies the first update.

    An exception out of a callback leaves the loop and fails [sw]. *)

val sync_forever_controlled :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  ?initial_pos:string ->
  ?timeout_ms:int ->
  ?txn_id:bool ->
  ?set_presence:[ `Online | `Offline | `Unavailable ] ->
  ?thread_subscription_store:Matrix_client.Store.t ->
  ?state_store:Matrix_client.Store.t ->
  ?own_profile:Own_profile.t ->
  ?profile_service:Sync_service.t ->
  ?service:Sync_service.t ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  ?on_change:(Sync_service.state -> Sync_service.changes -> unit) ->
  callbacks:Response.t Sync.callbacks ->
  Controller.t ->
  unit
(** [sync_forever_controlled] is {!sync_forever} with its request supplied by a
    {!Controller}. Effective subscription changes can interrupt the current
    request or a [Retry_after] wait and restart it immediately with the same
    accepted [pos]. When [set_presence] is omitted, effective client-presence
    changes have the same restart semantics; these internal restarts do not
    invoke callbacks or persistence hooks. Parent switch cancellation still
    terminates the loop normally. *)

val sync_to_stream :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  stream:Response.t Eio.Stream.t ->
  ?initial_pos:string ->
  ?timeout_ms:int ->
  ?txn_id:bool ->
  ?set_presence:[ `Online | `Offline | `Unavailable ] ->
  ?thread_subscription_store:Matrix_client.Store.t ->
  ?state_store:Matrix_client.Store.t ->
  ?own_profile:Own_profile.t ->
  ?profile_service:Sync_service.t ->
  ?service:Sync_service.t ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  ?on_change:(Sync_service.state -> Sync_service.changes -> unit) ->
  ?on_error:(Error.err -> Sync.action) ->
  Request.t ->
  unit
(** [sync_to_stream ~sw ~clock client ~stream request] is {!sync_forever}
    pushing every response onto [stream], so that syncing and processing run in
    separate fibers. The loop blocks while the stream is full. *)

val create_sync_stream :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  Client.t ->
  ?capacity:int ->
  ?initial_pos:string ->
  ?timeout_ms:int ->
  ?txn_id:bool ->
  ?set_presence:[ `Online | `Offline | `Unavailable ] ->
  ?thread_subscription_store:Matrix_client.Store.t ->
  ?state_store:Matrix_client.Store.t ->
  ?own_profile:Own_profile.t ->
  ?profile_service:Sync_service.t ->
  ?service:Sync_service.t ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  ?on_encryption_error:(Error.err -> unit) ->
  ?on_change:(Sync_service.state -> Sync_service.changes -> unit) ->
  ?on_error:(Error.err -> Sync.action) ->
  Request.t ->
  Response.t Eio.Stream.t
(** [create_sync_stream ~sw ~clock client request] is a stream of responses,
    with {!sync_to_stream} already running behind it. [capacity] is the stream's
    buffer and defaults to 10. *)
