(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A JMAP client.

    A client holds the session resource of
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-2} RFC 8620 Section
     2} and everything derived from it, which is the URL requests are POSTed to,
    the origins the credential may travel to and the concurrency the server
    allows. It runs on a {!Transport.t}, which carries the authority to make
    requests and the clocks that bound them.

    Credentials are not attached per call. An {!Auth.t} is given once to
    {!connect} and the client attaches it only to requests under the origins the
    session names. The credential wraps the transport rather than sitting inside
    it, so its thunk runs once per request this module makes rather than once
    per attempt the transport's own retry layer makes underneath. That
    distinction is visible only to {!Auth.refreshing} under a retry policy that
    allows POST, which [Fetch.Retry.default] does not.

    A request is made with {!val-call} for a chain ending in one handle, with
    {!run} for a chain ending in several, and with {!chain} when the response
    itself is wanted rather than the values its calls decode to. *)

(** {1 Types} *)

type t
(** The type for JMAP clients. A client may be shared by fibers running in one
    OCaml domain. Do not use the same value concurrently from multiple domains:
    its mutable session state and Eio synchronization primitives are
    single-domain. Construct one client per domain. A {!Transport.t} built by
    {!Transport.v} uses the domain-shareable Fetch/httpz backend and may be
    reused for that; one built by {!Transport.of_fetch} inherits the wrapped
    Fetch backend's domain restrictions. *)

(** The type for the failures of an exchange. *)
type error =
  | Http_error of int * string
      (** An HTTP failure, with the status code and the response body, bounded
          by the [max_body] of {!connect}. A body that claims to be
          [application/problem+json] and is not carries the decoder's message in
          place of itself. *)
  | Jmap_error of Jmap.Proto.Error.Request_error.t
      (** A request level error, decoded from an [application/problem+json]
          response body (RFC 8620 Section 3.6.1). *)
  | Method_error of Jmap.Proto.Error.Method_error.t
      (** The server answered a method call with an error response
          ({{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.6.2} RFC
            8620 Section 3.6.2}) where {!val-call} or {!run} needed the
          response. The request itself succeeded. *)
  | Json_error of Jsont.Error.t
      (** A response body the JSON codec rejected. It names the member at fault
          and where in the body it is. *)
  | Session_error of string  (** The session resource could not be fetched. *)
  | Transport of Fetch.error * string
      (** A network, TLS or policy failure reported by {!Fetch}, with the text
          [Eio.Exn.pp] gives it. I/O diagnostics retain their underlying context
          and identify the JMAP operation. Request context includes up to eight
          method names, excluding arguments. JMAP adds only the endpoint origin,
          excluding user information, path, query and fragment. Context supplied
          by the transport or caller is retained unchanged. The [Fetch.error] is
          what to match on. [Connection_failure] and [Protocol_error] may be
          worth retrying, [Denied], [Tls_failure] and [Invalid_request] never
          are. The last also reports a locally invalid chain, upload length,
          source or sink. *)
  | Timeout of float
      (** An exchange deadline, or one idle wait for download body bytes,
          exceeded the [timeout] seconds given to {!connect}. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints a one line description of [e]. The body of an
    {!Http_error} is cut to its first line and to 120 characters, the whole body
    being in the constructor for a caller that wants it. Control bytes in all
    server-supplied text are escaped rather than emitted to the terminal. *)

val error_to_string : error -> string
(** [error_to_string e] is {!pp_error} of [e] as a string. *)

exception Jmap_client_error of error
(** The exception the [_exn] functions raise. Its registered exception printer
    includes {!pp_error}, preserving the diagnostic context in uncaught-error
    reports. *)

(** {1 Connecting} *)

val connect :
  sw:Eio.Switch.t ->
  ?auth:Auth.t ->
  ?timeout:float ->
  ?allow_insecure:bool ->
  ?trust_redirects:bool ->
  ?max_body:int ->
  Transport.t ->
  string ->
  (t, error) result
(** [connect ~sw ~auth ~timeout ~allow_insecure ~trust_redirects ~max_body
     transport url] fetches the session at [url] over [transport] and is a
    client for it. It is the only way to build a client.

    [sw] is the switch a session refresh runs under, so that the exchange
    belongs to the client rather than to whichever fiber first noticed the
    session was stale. It must still be on when the client is used, since a
    refresh after it finishes raises [Invalid_argument] from
    [Eio.Fiber.fork_promise]. {!switch} reports it, and it is also where
    {!Push.subscribe} attaches its fiber.

    [auth] is the credential and defaults to {!Auth.none}.

    [timeout] is the number of seconds allowed for each exchange this module
    reads to its end, which is the session fetch, a request and an upload. It
    defaults to no deadline. Exceeding it cancels the exchange and gives
    {!Timeout}. It does not cover the wait for a [maxConcurrentRequests] or
    [maxConcurrentUpload] slot, which happens before the deadline starts so that
    the deadline measures the server's answer rather than the queue in front of
    it. Nor does it cover the session refresh a response's [sessionState] may
    trigger after the exchange, which has its own deadline, the body of a
    {!download_to} or {!download_with_type}, where it bounds the response head
    and then each wait for body bytes rather than the total transfer, or
    {!with_get}, whose body the caller consumes. A download may therefore take
    longer than [timeout] while it keeps making progress, but a stalled read is
    {!Timeout}; by then its sink may contain a prefix, so use a temporary file
    and rename it if an all-or-nothing file is required. The wall clock cost of
    a {!request} is queueing, then the deadline, then at most one refresh. The
    whole session fetch, redirects included, is one deadline rather than one per
    hop.

    [allow_insecure] allows credentials on [http://] requests and defaults to
    [false], which makes an in-scope [http://] request fail with
    {!constructor-Transport}. A plain HTTP test server such as the Cyrus one
    needs [true].

    [trust_redirects] follows a session redirect that leaves the credential's
    site, sending the credential to it, and defaults to [false]. Without it the
    scope is extended to a hop's origin only when both URLs are [https] on the
    same port and the target host is the current host or shares its registrable
    domain in the Public Suffix List, so [api.example.com] reaches
    [jmap.example.com] but not [example.net], and a change of port or a hop out
    of an [http://] session under [allow_insecure] is not an extension. An IP
    literal host shares its registrable domain with no other host. A hop to an
    origin already in the scope, which is what the [.well-known] redirect of RFC
    8620 Section 2.2 usually is, is always followed, and so is any hop when
    there is no credential to lose. Anything else is {!Session_error} naming the
    origin, because a redirect followed blindly hands a bearer token to whoever
    wrote the [Location] header. A hop from [https] to [http] is refused by the
    transport before the policy is consulted and is
    [Transport (Fetch.Denied _, _)].

    [max_body] is the largest response body read into memory, in bytes, and
    defaults to 64 MiB.

    RFC 8620 Section 2.2 lets the well-known URL redirect, possibly to another
    origin. The walk follows at most five hops under a [Fetch.Redirect] policy
    and is one deadline. The URL of the last hop becomes {!session_url}, which
    {!refresh_session} refetches, and the origins the credential may then travel
    to are those of the session's [apiUrl], [uploadUrl], [downloadUrl] and
    [eventSourceUrl] together with the ones the walk extended the scope to.

    The authenticated session document is trusted to name those endpoint
    origins, as RFC 8620 requires: a bearer token is sent to each one when it is
    used. A caller that trusts only a fixed set should pass
    [Transport.restrict ~under:allowlist transport]; this applies Fetch's URL
    policy to the session fetch and every advertised endpoint as an outer
    boundary, independently of credential scoping and [trust_redirects].

    Before constructing the client, [connect] parses and resolves every
    advertised URL against the final session URL. The download, upload and
    event-source values must be valid RFC 6570 level-1 templates containing
    their JMAP-required variables, and a required variable may not occur in the
    scheme or authority. A malformed endpoint gives {!Session_error}.

    @raise Invalid_argument
      if [timeout] is not finite, non-negative and representable by
      [Duration.t], if it is given and [transport] carries no clock (which only
      {!Transport.of_fetch} can produce), or if [max_body] is not positive. *)

val connect_env :
  sw:Eio.Switch.t ->
  ?auth:Auth.t ->
  ?timeout:float ->
  ?allow_insecure:bool ->
  ?trust_redirects:bool ->
  ?max_body:int ->
  Eio_unix.Stdenv.base ->
  string ->
  (t, error) result
(** [connect_env ~sw env url] is {!connect} over [Transport.v env], which is an
    httpz backend with TLS against the system trust anchors, a cookie jar,
    per-origin pacing and retries. It is the one line form for a program that
    wants no say in how the HTTP is done. The optional arguments are those of
    {!connect}. I/O failures during transport construction are also returned as
    {!constructor-Transport} with connection context.

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match
        Client.connect_env ~sw ~auth:(Auth.bearer token) env
          "https://api.example.com/.well-known/jmap"
      with
      | Ok client -> ...
      | Error e -> Fmt.epr "%a@." Client.pp_error e
    ]} *)

(** {1 The session} *)

val session : t -> Jmap.Proto.Session.t
(** [session t] is the session resource [t] currently holds. *)

val switch : t -> Eio.Switch.t
(** [switch t] is the switch given to {!connect}. It is where the client's own
    session refresh runs, and where a caller attaches a fiber whose lifetime
    should follow the client's, as {!Push.subscribe} does. *)

val fetch : t -> Fetch.plain
(** [fetch t] is the HTTP client of [t], which is the transport's own client
    with the credentials of {!connect} attached under the origins the session
    names. Whoever holds it holds the authority to make requests with those
    credentials. Narrow it with [Fetch.restrict] before passing it on.

    It exists for the parts of JMAP that are not a request or a blob and that
    Fetch already implements, such as the event source of RFC 8620 Section 7.3,
    which {!Push} reaches with [Fetch.Sse]. A caller that wants a response
    classified as this module classifies one wants {!with_get} instead.

    A session refresh may replace it, so read it per use rather than holding the
    result. *)

val mono_clock : t -> Eio.Time.Mono.ty Eio.Resource.t option
(** [mono_clock t] is the monotonic clock of the transport [t] runs on, or
    [None] if it carries none. It is the clock {!Push.subscribe} measures a
    backoff with. *)

val session_url : t -> string
(** [session_url t] is the URL of the session resource after any redirects,
    against which the session's relative URLs are resolved. *)

val api_url : t -> string
(** [api_url t] is the session's [apiUrl]. RFC 8620 Section 2 leaves the session
    URLs open and servers such as Cyrus send absolute paths, so every URL here
    is resolved against {!session_url}. *)

val event_source_url : t -> string
(** [event_source_url t] is the session's [eventSourceUrl] template (RFC 8620
    Section 7.3), resolved like {!api_url}. *)

val event_source_template : t -> Httpz_uri.Template.t
(** [event_source_template t] is the parsed form of {!event_source_url}. *)

val expand_event_source_url :
  t -> (string * Httpz_uri.Template.value) list -> (string, error) result
(** [expand_event_source_url t bindings] is the event-source URL with [bindings]
    substituted into the session's original template before resolving it against
    {!session_url}. An omitted binding is undefined. Duplicate names use their
    first binding. Malformed values or an expansion that changes the endpoint's
    origin are {!constructor-Transport} errors carrying [Fetch.Invalid_request].
    {!Push.event_source_url} supplies the JMAP event-source arguments. *)

val upload_url : t -> string
(** [upload_url t] is the session's [uploadUrl] template, resolved like
    {!api_url}. *)

val upload_template : t -> Httpz_uri.Template.t
(** [upload_template t] is the parsed form of {!upload_url}. *)

val download_url : t -> string
(** [download_url t] is the session's [downloadUrl] template, resolved like
    {!api_url}. *)

val download_template : t -> Httpz_uri.Template.t
(** [download_template t] is the parsed form of {!download_url}. *)

val concurrency_limits : t -> int * int
(** [concurrency_limits t] is the number of API requests and of blob uploads [t]
    will have in flight at once, taken from [maxConcurrentRequests] and
    [maxConcurrentUpload] of the session's core capability (RFC 8620 Section 2)
    and updated on every refresh. Exchanges already running remain counted
    against the refreshed limits. A server that states zero gets a conservative
    default of 4. A larger value than 4096 is clamped to 4096, since each limit
    bounds the number of concurrent exchanges.

    {!request}, {!chain} and the upload functions wait for a slot before they
    send, so no caller has to count. Reading the limits is for a caller that
    wants to size its own fan-out, as {!Sync} does. *)

val refresh_session : t -> (unit, error) result
(** [refresh_session t] fetches a fresh session from the server and updates the
    session state, credential scope and concurrency limits of [t], then runs the
    observers registered with {!on_session_change} and broadcasts
    {!session_changed}.

    A client does not normally have to call this. {!request} and {!chain}
    compare the [sessionState] of every response against the session they hold,
    as RFC 8620 Section 3.4 intends, and refetch on their own when the two
    disagree. It is here for the case the protocol cannot signal, such as a
    credential that has just gained an account, and for a caller that wants the
    fetch to happen at a moment of its choosing.

    Concurrent calls serialise. The first to ask forks the exchange under the
    client's switch and the others await it, so fibers that ask at once make one
    request between them. The wait is a cancellation point, unlike a lock held
    across the exchange, and a caller that gives up leaves the refresh running
    for the fibers that have not.

    A failure is returned here and also recorded in {!last_refresh_error}, and
    the client goes on using the session it holds. On success the new session's
    endpoint origins replace the old ones in the credential scope; an endpoint
    the server removed no longer receives the credential. *)

val refresh_session_exn : t -> unit
(** [refresh_session_exn t] is {!refresh_session}.

    @raise Jmap_client_error if the refresh fails. *)

val on_session_change : t -> (Jmap.Proto.Session.t -> unit) -> unit
(** [on_session_change t f] registers [f] to be called with the new session
    after every refresh, whether the refresh was asked for by {!refresh_session}
    or forced by a [sessionState] mismatch (RFC 8620 Section 3.4).

    Observers are called in registration order, outside the lock the refresh is
    made under, so an observer may itself use the client. One that raises does
    not stop the others and does not fail the request that triggered the
    refresh. Its escaped exception description, including existing I/O context
    and the observer operation, is written to the Eio trace. A cancellation
    propagates. *)

val session_changed : t -> Eio.Condition.t
(** [session_changed t] is broadcast after every session refresh, once its
    observers have run. It is the form to wait on rather than to be called back
    on.
    {[
    Eio.Condition.await_no_mutex (Client.session_changed client)
    ]}
    A condition has no memory, so a fiber that wants to be sure it sees a change
    should read the state of {!session} before and after waiting. *)

val last_refresh_error : t -> error option
(** [last_refresh_error t] is why the last session refresh failed, or [None] if
    the last one succeeded or none has been attempted. It is recorded by the
    refresh itself, so a fiber that gives up on a refresh does not affect it.

    A refresh is usually not asked for. {!request} starts one when a response's
    [sessionState] disagrees with the session held (RFC 8620 Section 3.4), and
    that request still succeeds. When the refetch fails the client keeps the
    session it has, with a stale [apiUrl], credential scope and set of limits,
    and the request that triggered it says nothing, so this is where a caller
    looks to find out. The attempt is made once per new [sessionState] rather
    than once per request, so a session endpoint that is down does not double
    the cost of every call. *)

(** {1 Requests} *)

val request : t -> Jmap.Proto.Request.t -> (Jmap.Proto.Response.t, error) result
(** [request t req] POSTs [req] to the session's [apiUrl] as [application/json]
    (RFC 8620 Section 3.3) and is the response.

    The response retains its original JSON body through
    {!Jmap.Proto.Response.source}. Typed objects with location metadata can
    recover their source bytes with {!Jmap.Proto.Response.source_fragment}.

    The encoded byte length and number of method calls are checked against the
    session's [maxSizeRequest] and [maxCallsInRequest] before the request is
    sent, after any wait for a concurrency slot. Exceeding either is returned as
    [Transport (Fetch.Invalid_request _, _)].

    An error response carrying [application/problem+json] is {!Jmap_error} (RFC
    8620 Section 3.6.1) and any other is {!Http_error}. *)

val request_exn : t -> Jmap.Proto.Request.t -> Jmap.Proto.Response.t
(** [request_exn t req] is {!request}.

    @raise Jmap_client_error if the exchange fails. *)

val default_capabilities : t -> string list
(** [default_capabilities t] is the [using] array {!chain}, {!val-call} and
    {!run} send when the caller names none. It is those of
    [urn:ietf:params:jmap:core], [urn:ietf:params:jmap:mail],
    [urn:ietf:params:jmap:submission], [urn:ietf:params:jmap:vacationresponse],
    [urn:ietf:params:jmap:contacts] and [urn:ietf:params:jmap:calendars] that
    the session held by [t] advertises, in that order, so it follows a session
    refresh.

    Those six are the capabilities this library builds method calls for. A
    request may name any capability the server supports, and naming one outside
    them is what the explicit argument is for. *)

val chain :
  t ->
  ?capabilities:string list ->
  'a Jmap.Chain.t ->
  ('a * Jmap.Proto.Response.t, error) result
(** [chain t ~capabilities c] builds the chain [c] into a request and executes
    it, and is the value of [c], which is the handles to parse with, beside the
    response.

    It is {!Jmap.Chain.build} followed by {!request}, which is how a chain is
    always used. RFC 8620 Section 3.2 puts every method call of one request in a
    single [methodCalls] array, so a chain is one round trip.

    {!val-call} and {!run} are this followed by the reading of the handles, and
    are what a caller after the decoded responses uses. This is for one that
    wants the response itself, to print it with {!Jmap.Proto.Response.pp} or to
    ask {!Jmap.Chain.method_error} about a single call.
    {[
      match
        Client.chain client
          Jmap.Chain.(
            let* q = email_query ~account_id ~limit:10L () in
            email_get ~account_id ~ids:(from_query q)
              ~properties:[ `Id; `Subject ] ())
      with
      | Ok (g, response) -> Jmap.Chain.parse g response
      | Error e -> ...
    ]}
    [capabilities] is the [using] array of the request (RFC 8620 Section 3.2),
    which names the capabilities the method calls come from. It defaults to
    {!default_capabilities} of [t], read at the time of the call.

    The error is a transport or request level failure. If building [c] raises
    [Invalid_argument], as a typed builder may for invalid arguments, it is
    returned as [Transport (Fetch.Invalid_request _, _)] without making an HTTP
    request. A method that failed is reported per call by
    {!Jmap.Chain.val-parse}. *)

val chain_exn :
  t ->
  ?capabilities:string list ->
  'a Jmap.Chain.t ->
  'a * Jmap.Proto.Response.t
(** [chain_exn t ~capabilities c] is {!chain}.

    @raise Jmap_client_error if the exchange fails. *)

val call :
  t ->
  ?capabilities:string list ->
  (_, 'r) Jmap.Chain.handle Jmap.Chain.t ->
  ('r, error) result
(** [call t ~capabilities c] is the decoded response of the one call the chain
    [c] ends in. It is {!chain} followed by {!Jmap.Chain.val-parse}.
    [capabilities] is that of {!chain}.

    A chain may add several method calls and end in the handle of one of them,
    as {!Jmap.Chain.mailbox_by_role} does, so this is one round trip rather than
    one method call.
    {[
    Client.call client
      Jmap.Chain.(
        let* q = email_query ~account_id ~limit:10L () in
        email_get ~account_id ~ids:(from_query q) ~properties:[ `Id; `Subject ]
          ())
    ]}
    A call the server answered with an error response is {!Method_error}, and
    one whose response did not decode is {!Json_error}. *)

val call_exn :
  t -> ?capabilities:string list -> (_, 'r) Jmap.Chain.handle Jmap.Chain.t -> 'r
(** [call_exn t ~capabilities c] is {!val-call}.

    @raise Jmap_client_error if the exchange fails or the response is not read.
*)

val run :
  t ->
  ?capabilities:string list ->
  'rs Jmap.Chain.Handles.t Jmap.Chain.t ->
  ('rs Jmap.Chain.Results.t, error) result
(** [run t ~capabilities c] is the decoded responses of the handles the chain
    [c] ends in, in the order it names them. It is {!chain} followed by
    {!Jmap.Chain.parse_all}. [capabilities] is that of {!chain}.
    {[
      match
        Client.run client
          Jmap.Chain.(
            let* q = email_query ~account_id ~limit:10L () in
            let+ g =
              email_get ~account_id ~ids:(from_query q)
                ~properties:[ `Id; `Subject ] ()
            in
            Handles.[ q; g ])
      with
      | Ok Results.[ query; got ] -> (query.total, got.list)
      | Error e -> ...
    ]}
    The [Results.[ ... ]] pattern is read in the order of the handle list rather
    than by name, so naming its variables after the handles, in that order, is
    what keeps the response of one call from being read as the response of
    another of the same type.

    The first handle that does not read fails the whole read, so a call that may
    legitimately answer with an error is wrapped in {!Jmap.Chain.attempt}. Its
    result is then an [(_, Jmap.Proto.Error.Method_error.t) result] of its own
    and the rest of the read survives it. *)

val run_exn :
  t ->
  ?capabilities:string list ->
  'rs Jmap.Chain.Handles.t Jmap.Chain.t ->
  'rs Jmap.Chain.Results.t
(** [run_exn t ~capabilities c] is {!run}.

    @raise Jmap_client_error if the exchange fails or a response is not read. *)

val run_with_response :
  t ->
  ?capabilities:string list ->
  'rs Jmap.Chain.Handles.t Jmap.Chain.t ->
  ( 'rs Jmap.Chain.Handles.t * 'rs Jmap.Chain.Results.t * Jmap.Proto.Response.t,
    error )
  result
(** [run_with_response t ~capabilities c] is the handles [c] ends in, {!run} of
    [c], and the response the results were decoded from. [capabilities] is that
    of {!chain}.

    It is for a caller that wants the response as well as the results, such as
    one that asks {!Jmap.Proto.Response.find_responses} under the call id of a
    handle for the implicit [Email/set] an [onSuccessUpdateEmail] of RFC 8621
    Section 7.5 adds, which no handle stands for. Without it such a caller pairs
    {!chain} with {!Jmap.Chain.parse_all}. *)

val run_with_response_exn :
  t ->
  ?capabilities:string list ->
  'rs Jmap.Chain.Handles.t Jmap.Chain.t ->
  'rs Jmap.Chain.Handles.t * 'rs Jmap.Chain.Results.t * Jmap.Proto.Response.t
(** [run_with_response_exn t ~capabilities c] is {!run_with_response}.

    @raise Jmap_client_error if the exchange fails or a response is not read. *)

(** {1 Blobs} *)

val upload :
  t ->
  account_id:Jmap.Proto.Id.t ->
  content_type:string ->
  data:string ->
  (Jmap.Proto.Blob.upload_response, error) result
(** [upload t ~account_id ~content_type ~data] uploads the blob [data] with the
    media type [content_type] to [account_id], POSTing it to the session's
    [uploadUrl] expanded per RFC 8620 Section 6.1. Data larger than the
    session's [maxSizeUpload] is returned as
    [Transport (Fetch.Invalid_request _, _)] without being sent. [content_type]
    is encoded with Fetch's typed [Content-Type] header and must be a valid HTTP
    media type. *)

val upload_exn :
  t ->
  account_id:Jmap.Proto.Id.t ->
  content_type:string ->
  data:string ->
  Jmap.Proto.Blob.upload_response
(** [upload_exn t ~account_id ~content_type ~data] is {!upload}.

    @raise Jmap_client_error if the exchange fails. *)

val upload_flow :
  t ->
  account_id:Jmap.Proto.Id.t ->
  content_type:string ->
  ?length:int64 ->
  _ Eio.Flow.source ->
  (Jmap.Proto.Blob.upload_response, error) result
(** [upload_flow t ~account_id ~content_type ~length source] is {!upload}
    reading its bytes from [source] instead of holding them in memory, which is
    what an attachment on disk wants. RFC 8620 Section 6.1 puts no bound on a
    blob other than the session's [maxSizeUpload], and a client should not have
    to be able to hold one.

    [length] is the exact number of bytes [source] will yield and is sent as
    [Content-Length]. Without it the body is chunked, which every HTTP/1.1
    server must accept but which a JMAP server may still refuse to buffer, so
    give it whenever the size is known. [Eio.File.stat] knows it for a file. A
    known length larger than the session's [maxSizeUpload] is rejected before
    the request is sent. Without [length], the source is stopped if it crosses
    that limit. A negative value is returned as
    [Transport (Fetch.Invalid_request _, _)].

    The body is one shot, so unlike {!upload} it can be sent only once. A
    redirect or a retry of this request fails with [Fetch.Body_not_replayable]
    reported as {!constructor-Transport}, and a failure of [source] itself is
    reported the same way rather than raised. {!connect}'s [timeout] covers the
    whole exchange here, the streaming of the body included, since the client
    controls how fast the bytes are produced.

    {[
    Eio.Path.with_open_in path @@ fun file ->
    let length = Optint.Int63.to_int64 (Eio.File.size file) in
    Client.upload_flow client ~account_id ~content_type:"message/rfc822" ~length
      file
    ]} *)

val upload_flow_exn :
  t ->
  account_id:Jmap.Proto.Id.t ->
  content_type:string ->
  ?length:int64 ->
  _ Eio.Flow.source ->
  Jmap.Proto.Blob.upload_response
(** [upload_flow_exn t ~account_id ~content_type ~length source] is
    {!upload_flow}.

    @raise Jmap_client_error if the exchange fails. *)

val download :
  t ->
  account_id:Jmap.Proto.Id.t ->
  blob_id:Jmap.Proto.Id.t ->
  ?name:string ->
  ?accept:string ->
  unit ->
  (string, error) result
(** [download t ~account_id ~blob_id ~name ~accept ()] is the bytes of the blob
    [blob_id] of [account_id], downloaded from the session's [downloadUrl]
    expanded per RFC 8620 Section 6.2. [name] is the filename hint the server
    puts in [Content-Disposition] and defaults to ["download"]. [accept] is the
    media type the server is asked to serve the blob as and defaults to
    ["application/octet-stream"]. The body is bounded by {!connect}'s
    [max_body]. *)

val download_exn :
  t ->
  account_id:Jmap.Proto.Id.t ->
  blob_id:Jmap.Proto.Id.t ->
  ?name:string ->
  ?accept:string ->
  unit ->
  string
(** [download_exn t ~account_id ~blob_id ()] is {!download}.

    @raise Jmap_client_error if the exchange fails. *)

val download_with_type :
  t ->
  account_id:Jmap.Proto.Id.t ->
  blob_id:Jmap.Proto.Id.t ->
  ?name:string ->
  ?accept:string ->
  unit ->
  (string * string, error) result
(** [download_with_type t ~account_id ~blob_id ~name ~accept ()] is the pair of
    the media type the server served the blob as and the bytes of the blob, in
    that order. The media type is the response's [Content-Type], or [accept]
    when the response carries none. RFC 8620 Section 6.2 lets the server refuse
    to serve a blob as the requested type, so a client that reinterprets the
    bytes should look at what came back rather than at what it asked for. The
    returned media type is server-provided data. A terminal program must
    sanitize it before display. *)

val download_with_type_exn :
  t ->
  account_id:Jmap.Proto.Id.t ->
  blob_id:Jmap.Proto.Id.t ->
  ?name:string ->
  ?accept:string ->
  unit ->
  string * string
(** [download_with_type_exn t ~account_id ~blob_id ()] is {!download_with_type}.

    @raise Jmap_client_error if the exchange fails. *)

val download_to :
  t ->
  account_id:Jmap.Proto.Id.t ->
  blob_id:Jmap.Proto.Id.t ->
  ?name:string ->
  ?accept:string ->
  _ Eio.Flow.sink ->
  (string, error) result
(** [download_to t ~account_id ~blob_id ~name ~accept sink] copies the blob's
    bytes to [sink] as they arrive and is the media type the server served it
    as, on the terms of {!download_with_type}. The returned media type is
    server-provided data and must be sanitized before terminal display.

    It is the download of RFC 8620 Section 6.2 without a copy in memory, which
    is what saving an attachment to a file wants.
    {[
    Eio.Path.with_open_out ~create:(`Exclusive 0o600) path @@ fun file ->
    Client.download_to client ~account_id ~blob_id file
    ]}
    The client's body limit does not apply, the bytes being the caller's to
    place, so a caller that must not be handed an unbounded stream should bound
    the sink itself. {!download_with_type} is {!download_to} pointed at a buffer
    that stops at the limit.

    {!connect}'s [timeout] bounds the response head and, after it arrives, every
    wait for more body bytes. A large blob on a slow link may take longer than
    [timeout] in total while it keeps making progress. An idle-read {!Timeout}
    or a failure of [sink], such as a full disk or a broken pipe, can leave a
    prefix already written. The sink failure is returned as
    {!constructor-Transport} carrying [Fetch.Invalid_request] rather than
    raised. Use a temporary file and rename it after success when the target
    must appear atomically. *)

val download_to_exn :
  t ->
  account_id:Jmap.Proto.Id.t ->
  blob_id:Jmap.Proto.Id.t ->
  ?name:string ->
  ?accept:string ->
  _ Eio.Flow.sink ->
  string
(** [download_to_exn t ~account_id ~blob_id sink] is {!download_to}.

    @raise Jmap_client_error if the exchange fails. *)

(** {1 Other endpoints} *)

val with_get :
  ?headers:Fetch.Header.headers ->
  t ->
  string ->
  (Fetch.response -> ('a, error) result) ->
  ('a, error) result
(** [with_get ~headers t url f] is [f response], where [response] answers a GET
    for [url] made with the credentialed, origin scoped HTTP client of [t].
    [headers] defaults to none. The response and its body are valid only while
    [f] runs and are closed when [f] returns or raises, so [f] may consume the
    body incrementally and stop early. Exceptions from [f] propagate unchanged.

    It exists because a JMAP endpoint that is not a request or a blob, such as
    the event source of RFC 8620 Section 7.3, still has to be reached with the
    client's scoped credentials.

    [f] sees only a 2xx response. Any other status is read, bounded by the body
    limit of [t], and is {!Jmap_error} or {!Http_error} without [f] being
    called. A transport failure is {!constructor-Transport}.

    {!connect}'s [timeout] does not apply. The deadline would have to cover [f],
    and a caller that streams a response would see it fire on a healthy
    connection, the event source of RFC 8620 Section 7.3 having nothing to say
    for minutes at a time. Bound the read inside [f] instead. *)
