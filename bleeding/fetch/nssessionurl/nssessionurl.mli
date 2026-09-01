(** Fetch URLs via macOS NSURLSession using the Eio runloop. *)

val available : bool
(** [true] iff NSURLSession is available (i.e. this is macOS). *)

module Session : sig
  type t
  (** An NSURLSession *)

  type cache_policy =
    | Use_protocol_cache_policy
      (** Follow the HTTP caching headers (the default). *)
    | Reload_ignoring_local_cache      (** Always refetch from origin. *)
    | Return_cache_data_else_load
      (** Use any cached copy regardless of age, else fetch. *)
    | Return_cache_data_dont_load
      (** Offline mode: cached copy or failure, never the network. *)
    | Reload_revalidating_cache
      (** Revalidate cached copies with the origin before use. *)

  type cookie_accept_policy =
    | Always
    | Never
    | Only_from_main_document_domain

  type tls_version =
    | TLS_1_2
    | TLS_1_3

  val create :
    ?ephemeral:bool ->
    ?request_timeout:float ->
    ?resource_timeout:float ->
    ?max_connections_per_host:int ->
    ?allows_cellular:bool ->
    ?allows_expensive:bool ->
    ?allows_constrained:bool ->
    ?waits_for_connectivity:bool ->
    ?cache_policy:cache_policy ->
    ?set_cookies:bool ->
    ?cookie_accept_policy:cookie_accept_policy ->
    ?min_tls:tls_version ->
    ?additional_headers:(string * string) list ->
    unit ->
    t
  (** [create ()] makes a fresh session, isolated from the default one.
      Omitted parameters keep NSURLSession's defaults.

      @param ephemeral no cookies, cache or credentials touch disk, and
      all state dies with the session (default false).
      @param request_timeout seconds the transfer may sit idle, counted
      afresh whenever data arrives (NSURLSession default 60.0).
      @param resource_timeout seconds the whole transfer may take,
      including retries (NSURLSession default 7 days).
      @param max_connections_per_host cap on simultaneous connections to
      one origin.
      @param allows_cellular permit cellular interfaces (default true).
      @param allows_expensive permit interfaces the system marks expensive,
      e.g. personal hotspots (default true).
      @param allows_constrained permit use in Low-Data-Mode (default true).
      @param waits_for_connectivity queue the request until connectivity
      appears instead of failing immediately (default false). The wait
      counts against [resource_timeout].
      @param cache_policy default cache behaviour for requests on this
      session.
      @param set_cookies attach stored cookies to requests (default true).
      @param cookie_accept_policy which response cookies to store.
      @param min_tls lowest TLS version to negotiate.
      @param additional_headers headers merged into every request on this
      session. A per-request header wins on conflict. *)
end

type info = {
  status : int;                     (** HTTP status code, 0 if not HTTP. *)
  headers : (string * string) list; (** Response headers. *)
}

type body =
  | Fixed of string
  (** Body held in memory. NSURLSession can replay it on redirects and
      auth retries, and sets Content-Length itself. *)
  | Stream : _ Eio.Flow.source -> body
  (** Body streamed from an Eio flow with bounded buffering. Sent with
      chunked transfer-encoding unless a Content-Length request header is
      supplied. Cannot be replayed, so if the server redirects or requires
      an auth retry the request fails. *)

type error : immutable_data = {
  domain : string;
      (** The NSError domain, usually ["NSURLErrorDomain"]. A failure of
          the binding itself uses ["nssessionurl"]. *)
  code : int;  (** The NSError code, e.g. -1001 for a timeout. *)
  message : string;  (** The localized description. *)
}
(** A failed transfer, as NSURLSession reported it. *)

exception Error of error

val fetch :
  sw:Eio.Switch.t ->
  ?session:Session.t ->
  ?meth:string ->
  ?headers:(string * string) list ->
  ?body:body ->
  ?max_buffer:int ->
  ?follow_redirects:bool ->
  string ->
  info * Eio.Flow.source_ty Eio.Std.r
(** [fetch ~sw url] starts fetching [url], waits for the response headers,
    and returns them with a flow streaming the body. Reading the flow
    raises [End_of_file] at the end of the body, or {!Error} if the
    transfer fails midway.

    The transfer is cancelled and resources are released when [sw]
    finishes, or immediately if the fibre is cancelled.

    @param session session to use, default a process-wide shared
    session, so connections are pooled across requests.
    @param meth HTTP method, default ["GET"].
    @param headers extra request headers.
    @param body request body.
    @param max_buffer soft limit in bytes on data buffered between the
    task and the reading/writing fibres, applied to each direction
    (default 1MiB). Past it the transfer pauses until the slower side
    catches up.
    @param follow_redirects follow HTTP redirects inside NSURLSession
    (default true). When false a 3xx response is delivered like any
    other, headers and body, for a caller running its own redirect
    policy. *)

val get :
  sw:Eio.Switch.t ->
  ?session:Session.t ->
  ?headers:(string * string) list ->
  string ->
  info * Eio.Flow.source_ty Eio.Std.r
(** [get ~sw url] is [fetch ~sw url]. *)

val post :
  sw:Eio.Switch.t ->
  ?session:Session.t ->
  ?headers:(string * string) list ->
  body:body ->
  string ->
  info * Eio.Flow.source_ty Eio.Std.r
(** [post ~sw ~body url] is [fetch ~sw ~meth:"POST" ~body url]. *)

val fetch_into :
  ?session:Session.t ->
  ?meth:string ->
  ?headers:(string * string) list ->
  ?body:body ->
  ?max_buffer:int ->
  ?follow_redirects:bool ->
  sink:_ Eio.Flow.sink ->
  string ->
  info
(** [fetch_into ~sink url] streams the response body into [sink] (a file,
    socket, buffer, ...) and returns once it is complete. Nothing is
    buffered beyond [max_buffer]. *)

val fetch_string :
  ?session:Session.t ->
  ?meth:string ->
  ?headers:(string * string) list ->
  ?body:body ->
  ?max_buffer:int ->
  ?follow_redirects:bool ->
  string ->
  info * string
(** [fetch_string url] buffers the complete response body into a string.
    It is the only function here that does so. Prefer {!fetch} or
    {!fetch_into} for bodies of unbounded size. The request body may
    still be streamed, as the buffering concerns only the response. *)
