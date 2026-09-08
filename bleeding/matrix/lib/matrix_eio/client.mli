(** client — the client value every module in this library takes.

    A client is a {!Matrix_client.Client.t} together with the switch and
    environment the endpoint wrappers need. Like the client underneath it, it is
    immutable as a value, so {!with_session} gives a new one rather than
    changing the session in hand. Its media fetcher is an explicitly shared,
    replaceable capability. *)

type t
(** The type for clients. *)

val create :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  homeserver:Uriz.t ->
  ?user_agent:string ->
  ?well_known_policy:Matrix_client.Client.well_known_policy ->
  ?fetch:Fetch.plain ->
  ?request_timeout:float ->
  ?media_fetcher:Matrix_client.Media_fetcher.t ->
  unit ->
  t
(** [create ~sw ~env ~homeserver ()] is a client bound to [sw] and not yet
    logged in. Pass it to {!Auth.login_password} or to {!with_session}.

    [user_agent] is sent instead of the HTTP backend's default.
    [well_known_policy] controls whether homeserver discovery may request
    [/.well-known/matrix/client], and defaults to {!Matrix_client.Client.Query}.
    [fetch] is the capability requests go through, and defaults to
    [Fetch_httpz.std ~retry:(Matrix_client.Http_retry.default ~homeserver) env],
    an httpz backend with TLS from the system trust store. Its bounded retry
    policy includes Fetch's idempotent methods and the read-like Matrix
    [/keys/query] POST on this homeserver, but no other POST. Pass a fetcher to
    share a connection pool, to configure retries or rate limits, or to
    substitute a mock in tests. [media_fetcher] controls high-level attachment
    retrieval and defaults to {!Matrix_client.Media_fetcher.default}.
    [request_timeout], when supplied, bounds each complete logical HTTP
    operation, including response-body consumption, retries and time spent
    awaiting automatic token refresh/replay, using [env#mono_clock]. A timeout
    is reported as {!Matrix_client.Error.Network_error}; the default is
    unlimited. The fetcher is held in a shared atomic replaceable cell: clients
    derived through {!with_session}, {!with_access_token} and the auto-refresh
    helpers observe later replacements. Either way [fetch] is narrowed to the
    homeserver's origin before use.

    Raises [Invalid_argument] if [homeserver] has no host, has a scheme other
    than [http] or [https], or [request_timeout] is not finite and positive. *)

val base : t -> Matrix_client.Client.t
(** [base t] is the result-returning client underneath [t], for a call this
    library does not wrap. *)

val switch : t -> Eio.Switch.t
(** [switch t] is the switch [t] was created on. *)

val http : t -> Fetch.plain
(** [http t] is the HTTP transport with which [t] was created, before
    {!Matrix_client.Client.create} restricted its client-server view to the
    homeserver origin. It can therefore reach an OAuth issuer on a different
    origin when the original transport's own policy permits that origin. *)

val media_fetcher : t -> Matrix_client.Media_fetcher.t
(** [media_fetcher t] is the current high-level attachment fetcher. *)

val get_media_fetcher : t -> Matrix_client.Media_fetcher.t
(** Alias for {!media_fetcher}. *)

val set_media_fetcher : t -> Matrix_client.Media_fetcher.t -> unit
(** [set_media_fetcher t fetcher] replaces the fetcher observed by [t] and every
    client derived from it. Replacements are safe to perform concurrently with
    retrievals. *)

val homeserver : t -> Uriz.t
(** [homeserver t] is the homeserver [t] is bound to. *)

val well_known_policy : t -> Matrix_client.Client.well_known_policy
(** [well_known_policy t] is the homeserver well-known discovery policy from
    [t]'s configuration, preserved by {!with_session} and {!with_access_token}.
*)

val session : t -> Matrix_client.Client.session option
(** [session t] is the session [t] carries, and [None] before login. *)

val sync_presence : t -> Matrix_client.Client.sync_presence
(** [sync_presence t] is the client-owned default presence used by sync
    requests. It starts at [`Online] and is shared by derived clients. *)

val set_sync_presence : t -> Matrix_client.Client.sync_presence -> unit
(** [set_sync_presence t state] updates the client-owned default presence. *)

val register_presence_wakeup : t -> (unit -> unit) -> unit -> unit
(** [register_presence_wakeup t f] registers [f] for effective default-presence
    changes and returns an idempotent unregister function. *)

val is_logged_in : t -> bool
(** [is_logged_in t] is [true] when [t] carries a session. *)

val user_id : t -> Matrix_proto.Id.User_id.t
(** [user_id t] is the user [t]'s session belongs to. Raises [Eio.Io] with
    [E Not_logged_in] when [t] carries no session. *)

val device_id : t -> Matrix_proto.Id.Device_id.t
(** [device_id t] is the device [t]'s session belongs to. Raises [Eio.Io] with
    [E Not_logged_in] when [t] carries no session. *)

val access_token : t -> string
(** [access_token t] is [t]'s access token. Raises [Eio.Io] with
    [E Not_logged_in] when [t] carries no session. *)

val with_session : t -> Matrix_client.Client.session -> t
(** [with_session t session] is [t] with [session]'s access token attached to
    every request. [t] itself is unchanged. *)

val with_access_token : t -> string -> t
(** [with_access_token t access_token] is [t] with the bearer [access_token]
    attached to every request. [t] itself is unchanged, and the returned client
    carries no session; use this before the token's user id is known. *)

val with_auto_refresh :
  ?store:Matrix_client.Profile_store.t ->
  ?on_session_update:
    (Matrix_client.Client.session -> (unit, Matrix_client.Error.t) result) ->
  refresh:
    (Matrix_client.Client.session ->
    (Matrix_client.Client.refreshed_tokens, Matrix_client.Error.t) result) ->
  t ->
  t
(** [with_auto_refresh ~refresh t] enables the low-level client's opt-in OAuth
    refresh behavior while retaining [t]'s switch and unrestricted transport. It
    requires [t] to carry a session. The complete refresh, token commit and
    persistence hook run on [t]'s switch, independently of the request that
    first noticed expiry.

    With [store], {!Matrix_client.Profile_store.refresh_session} coordinates
    exchanges across processes and persists tokens before publishing completion.
    [on_session_update] becomes a notification and must not write credentials.
    An interrupted or failed exchange conservatively requires a new login; see
    the store contract for recovery and cancellation details. *)

val with_auto_refresh_expiry :
  ?store:Matrix_client.Profile_store.t ->
  ?on_session_update:
    (Matrix_client.Client.session ->
    Ptime.t option ->
    (unit, Matrix_client.Error.t) result) ->
  ?expires_at:Ptime.t ->
  ?early_refresh:Ptime.Span.t ->
  ?now:(unit -> Ptime.t) ->
  refresh:
    (Matrix_client.Client.session ->
    ( Matrix_client.Client.refreshed_tokens_with_expiry,
      Matrix_client.Error.t )
    result) ->
  t ->
  t
(** [with_auto_refresh_expiry] is the Eio facade for
    {!Matrix_client.Client.with_auto_refresh_expiry}. Refresh completion is
    attached to the client's switch rather than the initiating request's
    cancellation context. [store] has the same coordination and persistence
    contract as {!with_auto_refresh}. *)

val with_prepared_auto_refresh_expiry :
  ?store:Matrix_client.Profile_store.t ->
  ?on_session_update:
    (Matrix_client.Client.session ->
    Ptime.t option ->
    (unit, Matrix_client.Error.t) result) ->
  ?expires_at:Ptime.t ->
  ?early_refresh:Ptime.Span.t ->
  ?now:(unit -> Ptime.t) ->
  prepare:
    (Matrix_client.Client.session ->
    ( unit ->
      ( Matrix_client.Client.refreshed_tokens_with_expiry,
        Matrix_client.Error.t )
      result,
      Matrix_client.Error.t )
    result) ->
  t ->
  t
(** [with_prepared_auto_refresh_expiry ~prepare t] is
    {!with_auto_refresh_expiry} with read-only preparation separated from the
    token exchange. [prepare] must not consume the refresh token; it returns the
    thunk that does so. With [store], preparation failures/cancellation remain
    retryable without marking the token uncertain. Already persisted rotations
    skip preparation. Both phases run on the client's switch. See
    {!Matrix_client.Profile_store.refresh_session_prepared}. *)
