(** client — the value every endpoint module takes.

    A client binds a homeserver origin, an HTTP backend, a source of randomness
    and, once {!with_session} has been called, an access token. Configuration
    and session transformations return new client values; the client-owned sync
    presence and Server's small metadata cache are shared mutable cells. *)

type well_known_policy =
  | Query
  | Do_not_query
      (** Whether discovery may request [/.well-known/matrix/client]. [Query] is
          the default; [Do_not_query] makes all such discovery calls behave as
          if the document were absent. *)

(** Validated, canonical HTTP(S) URLs used at client trust boundaries.
    Public records use [Uriz.t]. Validate them here before comparing origins
    or issuing a request. *)
module Url : sig
  @@ portable
  type t = Fetch.Middleware.Url.t
  type scheme = [ `Http | `Https ]

  val of_string : string -> (t, string) result
  val of_uri : Uriz.t -> (t, string) result
  val to_uri : t -> Uriz.t
  val scheme : t @ local -> scheme
  val same_origin : t @ local -> t @ local -> bool
  val origin : t -> string
  val path_and_query : t -> string
  val path_segments : t -> string list
  val has_query : t @ local -> bool
  val has_fragment : t @ local -> bool
  val set_query_params : t -> (string * string) list -> t
  val resolve : base:t -> string -> (t, string) result
  val to_string : t -> string
  val effective_string : t -> string

  val homeserver : Uriz.t -> (t, string) result
  (** [homeserver uri] additionally rejects a query or fragment. A path prefix
      is retained as the base beneath which Matrix endpoint paths are joined; a
      missing or single trailing root slash denotes the same base. *)

  val homeserver_string : string -> (t, string) result
  (** [homeserver_string value] applies the same policy without first passing an
      untrusted wire spelling through [Uriz.t]. *)

  val append_path :
    t ->
    path:string ->
    ?query:(string * string) list ->
    unit ->
    (t, string) result
  (** [append_path base ~path ()] appends a Matrix endpoint path beneath a
      validated base-path prefix, ignoring one trailing slash on [base]. A raw
      query or fragment delimiter in [path] is rejected; use [query] or encode a
      delimiter that is data. *)
end

type config
(** What a client is built from. *)

val config :
  homeserver:Uriz.t ->
  ?user_agent:string ->
  ?well_known_policy:well_known_policy ->
  unit ->
  config
(** [config ~homeserver ()] is the configuration for a client talking to
    [homeserver]. [user_agent] is sent instead of the backend's default.
    [well_known_policy] controls whether calls to {!Server.get_well_known} and
    {!Server.discover} may request the Matrix homeserver well-known document; it
    defaults to {!Query}.

    Raises [Invalid_argument] if [homeserver] is not an absolute HTTP(S) URL or
    contains userinfo, a query, or a fragment. A path prefix is retained and
    Matrix endpoints are appended beneath it. *)

type session = {
  user_id : Matrix_proto.Id.User_id.t;
  access_token : string;
  device_id : Matrix_proto.Id.Device_id.t;
  refresh_token : string option;
}
(** What a successful login yields, and what {!with_session} takes back. *)

type refreshed_tokens = { access_token : string; refresh_token : string option }
(** The tokens returned by an automatic refresh. A [None] refresh token keeps
    the previous refresh token. *)

type refreshed_tokens_with_expiry = {
  refreshed_tokens : refreshed_tokens;
  expires_at : Ptime.t option;
}
(** The tokens and absolute access-token deadline returned by an expiry-aware
    refresh callback. [None] means that the server supplied no deadline. *)

(** The raw server-metadata cache used by {!Server}. Its values are response
    bodies rather than endpoint records so that [Client] stays independent of
    [Server]. This is an implementation-facing module; callers should use
    {!Server.invalidate_cache} when they need to clear it. *)
module Server_metadata_cache : sig
  type t

  val get : t -> [ `Versions | `Capabilities ] -> string option
  val set : t -> [ `Versions | `Capabilities ] -> string -> unit
  val clear : t -> unit
  val get_oauth_metadata : t -> (float * string) option
  val set_oauth_metadata : t -> float -> string -> unit

  val get_oauth_metadata_with_expiry :
    t -> (float * float option * string) option
  (** Like {!get_oauth_metadata}, retaining the optional absolute monotonic
      expiry. The timestamps are opaque cache-clock readings. *)

  val set_oauth_metadata_with_expiry :
    t -> float -> float option -> string -> unit
  (** Stores a body with its fetch timestamp and optional absolute monotonic
      expiry. This low-level hook is intended for cache tests and adapters. *)

  val invalidate_oauth_metadata : t -> unit
end

type t
(** The type for clients. *)

type sync_presence = [ `Online | `Offline | `Unavailable ]
(** The presence state used by requests that do not supply an explicit
    per-request override. *)

val create : config:config -> fetch:_ Fetch.t -> random:Random.t -> t
(** [create ~config ~fetch ~random] is a client that is not logged in. Pass it
    to {!Auth.login_password} or {!with_session} to authenticate.

    [fetch] is narrowed with [Fetch.restrict] to the origin of the
    configuration's homeserver, so the client cannot reach any other server,
    whatever a redirect or a caller-supplied path asks for. The library never
    constructs an HTTP client of its own. [Fetch_httpz.std], used by the
    [matrix-chat.eio] library, is one that is ready to use.

    [random] is what the modules handed this client draw their keys, nonces and
    transaction identifiers from. *)

val with_request_timeout : mono_clock:_ Eio.Time.Mono.t -> float -> t -> t
(** [with_request_timeout seconds t] returns a client derived from [t] whose
    complete logical HTTP operations are bounded by [seconds], including
    response-body consumption, retries, backoff and time spent awaiting
    automatic token refresh/replay. [mono_clock] is required and normally comes
    from the Eio facade; it is injectable for deterministic tests. A timeout is
    reported as {!Error.Network_error}, while the default client remains
    unlimited.

    Raises [Invalid_argument] when [seconds] is not finite and positive. *)

val with_session : t -> session -> t
(** [with_session t session] is [t] with [session]'s access token attached to
    every request. [t] itself is unchanged, and the unauthenticated client
    behind it is retained for {!Http.post_unauthenticated}. *)

val without_session : t -> t
(** [without_session t] removes the session and any automatic refresh behavior,
    retaining the origin-restricted unauthenticated client. *)

val with_auto_refresh :
  ?spawn_refresh:((unit -> unit) -> unit) ->
  ?on_session_update:(session -> (unit, Error.t) result) ->
  refresh:(session -> (refreshed_tokens, Error.t) result) ->
  t ->
  t
(** [with_auto_refresh ~refresh t] enables opt-in refresh for an existing
    session. On [M_UNKNOWN_TOKEN], one refresh is shared by concurrent callers;
    replayable buffered requests are retried once with the rotated token. The
    persistence callback, when supplied, runs after an atomic session update;
    its errors are logged and do not roll the update back. Raises
    [Invalid_argument] when [t] has no session. [spawn_refresh], when supplied,
    must start its callback independently and return immediately; the client
    then waits on the shared result. It is intended for a runtime facade that
    can attach refresh completion to a client lifetime rather than a request
    lifetime. *)

val with_auto_refresh_expiry :
  ?spawn_refresh:((unit -> unit) -> unit) ->
  ?on_session_update:(session -> Ptime.t option -> (unit, Error.t) result) ->
  ?expires_at:Ptime.t ->
  ?early_refresh:Ptime.Span.t ->
  ?now:(unit -> Ptime.t) ->
  refresh:(session -> (refreshed_tokens_with_expiry, Error.t) result) ->
  t ->
  t
(** [with_auto_refresh_expiry ~refresh t] enables proactive access-token refresh
    for an existing session. Before each buffered authenticated request and GET
    stream, an access token whose [expires_at] is at or before
    [now () + early_refresh] is refreshed when a refresh token exists. The
    default window is 60 seconds and [now] defaults to the system clock.

    Concurrent proactive and reactive refreshes are serialized and waiters share
    the result. A failed refresh leaves the old session and deadline in place,
    so a later request can retry. A successful update changes both in one
    critical section and then invokes [on_session_update] with the pair. A
    returned [None] refresh token retains the old one; a returned [None]
    deadline clears proactive scheduling. [spawn_refresh] has the same lifetime
    contract as in {!with_auto_refresh}. Raises [Invalid_argument] when [t] has
    no session or [early_refresh] is negative. *)

val with_access_token : t -> string -> t
(** [with_access_token t access_token] is [t] with the bearer [access_token]
    attached to every request. [t] itself is unchanged, and the returned client
    carries no {!type-session}; use this for a token before its user id is
    known, such as the OAuth [/account/whoami] request. *)

val session : t -> session option
(** The session the client carries, or [None] before login. *)

val sync_presence : t -> sync_presence
(** [sync_presence t] is the client-owned default presence state. It starts at
    [`Online] and is shared by clients derived from [t]. *)

val set_sync_presence : t -> sync_presence -> unit
(** [set_sync_presence t state] changes the client-owned default presence.
    Registered wakeups are notified after an effective change. *)

val register_presence_wakeup : t -> (unit -> unit) -> unit -> unit
(** [register_presence_wakeup t f] registers [f] for effective changes to the
    client-owned presence and returns an idempotent unregister function. A
    callback already copied by a concurrent setter may still run after
    unregistration. *)

val homeserver : t -> Uriz.t
(** The canonical homeserver base URL the client is bound to. *)

val homeserver_url : t -> Url.t
(** The same homeserver as a validated URL. *)

val same_origin : t -> Uriz.t -> bool
(** [same_origin t uri] validates [uri] and compares its canonical scheme, host
    and effective port with [t]'s homeserver. *)

val endpoint_uri :
  t -> path:string -> ?query:(string * string) list -> unit -> Uriz.t
(** [endpoint_uri t ~path ()] appends the absolute Matrix endpoint [path] to
    [t]'s configured base-path prefix and returns its canonical URL. [path] must
    begin with a slash. *)

val well_known_policy : t -> well_known_policy
(** [well_known_policy t] is the policy from [t]'s configuration. It is
    preserved by {!with_session} and {!with_access_token}. *)

val random : t -> Random.t
(** The randomness the client was created with. Modules handed a client draw
    from this rather than from a global generator. *)

val server_metadata_cache : t -> Server_metadata_cache.t
(** The per-client cache used by {!Server.get_versions} and
    {!Server.get_capabilities}. It is shared by calls on the same client, but a
    client returned by {!with_access_token} or {!with_session} gets a fresh
    cache. *)

(** The requests the endpoint modules are built from.

    [path] is relative to [/_matrix/client/v3] unless stated otherwise, bodies
    and responses are JSON, and the result is the response body. A non-2xx
    response becomes {!Error.Matrix_error} when it parses as a Matrix error and
    {!Error.Http_error} otherwise. A transport failure becomes
    {!Error.Network_error}. A local origin or credential policy refusal is
    {!Error.Policy_denied}, a TLS failure is {!Error.Tls_error}, and
    [Eio.Cancel.Cancelled] propagates.

    A caller that only wants the client-server API wants the endpoint modules
    instead. This is what they are written against, and what an endpoint the
    library does not cover can be reached through. *)
module Http : sig
  val get :
    t ->
    path:string ->
    ?query:(string * string) list ->
    unit ->
    (string, Error.t) result
  (** [get t ~path ()] is the body of [GET path]. *)

  val get_absolute :
    t ->
    path:string ->
    ?query:(string * string) list ->
    unit ->
    (string, Error.t) result
  (** [get_absolute t ~path ()] is the body of an authenticated JSON [GET] to
      the absolute [path] on the homeserver. *)

  val post :
    t ->
    path:string ->
    ?query:(string * string) list ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** [post t ~path ~body ()] is the body of [POST path] carrying [body]. *)

  val post_absolute :
    t ->
    path:string ->
    ?query:(string * string) list ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** [post_absolute t ~path ~body ()] is the body of an authenticated JSON
      [POST] to the absolute [path] on the homeserver. *)

  val post_absolute_unauthenticated :
    t ->
    path:string ->
    ?query:(string * string) list ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** [post_absolute_unauthenticated t ~path ~body ()] is an origin-restricted
      JSON [POST] to the absolute [path] without a bearer token. HTTP status
      failures are mapped to {!Error.t} in the same way as {!post_absolute}. *)

  val put :
    t ->
    path:string ->
    ?query:(string * string) list ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** [put t ~path ~body ()] is the body of [PUT path] carrying [body]. *)

  val put_absolute :
    t ->
    path:string ->
    ?query:(string * string) list ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** [put_absolute t ~path ~body ()] is the body of an authenticated JSON [PUT]
      to the absolute [path] on the homeserver. *)

  val delete :
    t ->
    path:string ->
    ?query:(string * string) list ->
    ?body:string ->
    unit ->
    (string, Error.t) result
  (** [delete t ~path ()] is the body of [DELETE path]. [body] is sent when
      given, and no request body is sent otherwise. *)

  val delete_absolute :
    t ->
    path:string ->
    ?query:(string * string) list ->
    ?body:string ->
    unit ->
    (string, Error.t) result
  (** [delete_absolute t ~path ()] is the body of an authenticated JSON [DELETE]
      to the absolute [path] on the homeserver. [body] is sent when given, and
      no request body is sent otherwise. *)

  val post_unauthenticated :
    t ->
    path:string ->
    ?query:(string * string) list ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** As {!post}, but without the access token, for [/login] and [/register]. *)

  (** {1 Raw bodies}

      These take an {e absolute} path on the homeserver, and do not prepend
      [/_matrix/client/v3], so they reach the media endpoints, which live under
      bases of their own. *)

  val get_bytes :
    t ->
    path:string ->
    ?query:(string * string) list ->
    unit ->
    (string * string option, Error.t) result
  (** [get_bytes] buffers an authenticated GET and returns its body and
      [Content-Type], if the server sent one. No [Accept] header is asserted, so
      the server chooses the representation. *)

  val get_bytes_with_cache_control :
    t ->
    path:string ->
    ?query:(string * string) list ->
    unit ->
    (string * string option * string option, Error.t) result
  (** [get_bytes_with_cache_control] is [get_bytes] with the raw [Cache-Control]
      response header. *)

  val get_bytes_with_cache_headers :
    t ->
    path:string ->
    ?query:(string * string) list ->
    unit ->
    (string * string option * string option * string option, Error.t) result
  (** Like {!get_bytes_with_cache_control}, also returning a valid [Expires]
      response header. *)

  val get_bytes_unauthenticated :
    t ->
    path:string ->
    ?query:(string * string) list ->
    unit ->
    (string * string option, Error.t) result
  (** [get_bytes_unauthenticated t ~path ()] is {!get_bytes} through the
      client's origin-restricted transport without its bearer token. It is for
      legacy media endpoints whose authentication scheme is [None], not for
      ordinary Client-Server API calls. *)

  type raw_response = { status : int; headers : Http.Header.t; body : string }

  val request_url_unauthenticated :
    t ->
    meth:Http.Method.t ->
    url:Url.t ->
    ?headers:Fetch.Header.headers ->
    ?body:string ->
    unit ->
    (raw_response, Error.t) result
  (** Like {!request_unauthenticated}, but preserves the exact path and query of
      an already validated URL. The client's origin restriction still applies.
  *)

  val request_unauthenticated :
    t ->
    meth:Http.Method.t ->
    path:string ->
    ?query:(string * string) list ->
    ?headers:Fetch.Header.headers ->
    ?body:string ->
    unit ->
    (raw_response, Error.t) result
  (** [request_unauthenticated t ~meth ~path ()] makes an origin-restricted
      request without the session bearer token and returns its status, headers,
      and body without interpreting the status. [path] is absolute within the
      configured homeserver origin. This is intended for protocol endpoints such
      as MSC4108 rendezvous, which need conditional request headers. *)

  val get_url_with_cache_control :
    t ->
    url:Url.t ->
    unit ->
    (string * string option * string option, Error.t) result
  (** The validated-URL form of {!get_bytes_with_cache_control}. *)

  val get_url_with_cache_headers :
    t ->
    url:Url.t ->
    unit ->
    (string * string option * string option * string option, Error.t) result
  (** Like {!get_url_with_cache_control}, also returning the canonical [Expires]
      response header when one was supplied and valid. *)

  val get_stream :
    t ->
    path:string ->
    ?query:(string * string) list ->
    on_response:
      (content_type:string option -> Eio.Flow.source_ty Eio.Resource.t -> unit) ->
    unit ->
    (unit, Error.t) result
  (** [get_stream t ~path ~on_response ()] performs an authenticated raw GET.

      For a 2xx response, [on_response] is called with the response's optional
      [Content-Type] and its one-shot body flow while the response is still
      open. The callback must consume the body before returning; the flow and
      response are then closed by this function. For a non-2xx response the
      callback is not called: the body is buffered only for the usual
      Matrix/HTTP error mapping, and an automatic refresh may replay the GET
      before calling it. Transport failures use the same {!Error.Network_error}
      mapping as the other HTTP helpers, and a local capability refusal is
      {!Error.Policy_denied}. TLS failures are {!Error.Tls_error}. *)

  val get_stream_unauthenticated :
    t ->
    path:string ->
    ?query:(string * string) list ->
    on_response:
      (content_type:string option -> Eio.Flow.source_ty Eio.Resource.t -> unit) ->
    unit ->
    (unit, Error.t) result
  (** [get_stream_unauthenticated] is {!get_stream} through the client's
      origin-restricted transport without its bearer token. It is for legacy
      media endpoints whose authentication scheme is [None]. *)

  val post_empty :
    t ->
    path:string ->
    ?query:(string * string) list ->
    unit ->
    (string, Error.t) result
  (** [post_empty t ~path ()] posts an empty body to the absolute [path] and
      returns the response body. The request carries the session bearer token
      and an [Accept: application/json] header, but no [Content-Type]. *)

  val post_url_bytes :
    t ->
    url:Url.t ->
    content_type:string ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** The validated-URL form of {!post_bytes}. The client's origin restriction
      still applies and the URL's existing query is preserved. *)

  val post_bytes :
    t ->
    path:string ->
    ?query:(string * string) list ->
    content_type:string ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** [post_bytes t ~path ~content_type ~body ()] posts [body] verbatim under
      [content_type] and is the response body, which the media endpoints answer
      as JSON. *)

  val put_bytes :
    t ->
    path:string ->
    ?query:(string * string) list ->
    content_type:string ->
    body:string ->
    unit ->
    (string, Error.t) result
  (** [put_bytes t ~path ~content_type ~body ()] is {!post_bytes} with [PUT]. *)

  val post_stream :
    t ->
    path:string ->
    ?query:(string * string) list ->
    content_type:string ->
    ?length:int64 ->
    body:_ Eio.Flow.source ->
    unit ->
    (string, Error.t) result
  (** [post_stream] posts a one-shot source body. [length], when present, is
      passed as the exact request length to Fetch; without it the request is
      sent with an unknown/chunked length. A streaming body cannot be replayed
      for a redirect or retry, so it does not trigger automatic token refresh
      and such a transport failure is returned as a normal {!Error.t}.

      Raises [Invalid_argument] when [length] is negative. *)

  (** {1 JSON} *)

  val decode_response : 'a Jsont.t -> string -> ('a, Error.t) result
  (** [decode_response codec body] reads [body] through [codec], failing with
      {!Error.Json_error}. *)

  val encode_body : 'a Jsont.t -> 'a -> (string, Error.t) result
  (** [encode_body codec v] renders [v] as a request body, failing with
      {!Error.Json_error}. *)
end
