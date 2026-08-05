(** A libcurl backend for {!Fetch}.

    {[
      Eio.Switch.run @@ fun sw ->
      let t = Fetch_curl.std ~sw env in
      Fetch.read t "https://example.com/"
    ]}

    {!std} mints the stack most applications want, and {!v} the bare
    backend. Whoever creates a client holds full authority over it and
    can narrow it with the {!Fetch} wrappers before passing it on.

    Requests through one client share a connection cache, so connections
    are reused, HTTP/2 streams multiplex, and libcurl enforces the
    connection caps. Concurrent fibers issue concurrent transfers, and
    cancelling a fiber aborts its transfer. Bodies stream in both
    directions, with the transfer paused while a reader falls behind or
    a [Stream] request body lags, so a request and its response each
    cost bounded memory.

    Redirects are followed by the portable {!Fetch.fetch} loop, never by
    libcurl, so policy applies to every hop. Proxy environment
    variables, [.netrc] and non-http(s) protocols are ignored unless
    configured.

    A client must be used from the domain that created it. Using one
    elsewhere raises [Invalid_argument]. *)

type tag = [ `Generic | `Curl ]

type t = tag Fetch.ty Eio.Resource.t
(** A curl-backed client. *)

type Eio.Exn.Backend.t += Curl_error of Curl.curlCode * string
(** libcurl's own detail, attached to a [Connection_failure]. *)

val v :
  sw:Eio.Switch.t ->
  ?tls_verify:bool ->
  ?http_version:[ `Auto | `Http1_1 ] ->
  ?proxy:string ->
  ?timeout:float ->
  ?connect_timeout:float ->
  ?max_response:int ->
  ?max_request:int ->
  ?user_agent:string ->
  ?verbose:bool ->
  ?max_connections_per_host:int ->
  ?max_total_connections:int ->
  ?multiplex:bool ->
  unit -> t
(** [v ~sw ()] is a new client whose event fibers and connection cache
    live until [sw] finishes.

    @param tls_verify verify certificates against system trust
      (default [true]).
    @param http_version [`Auto] uses HTTP/2 over TLS when the server
      offers it (the default). [`Http1_1] pins HTTP/1.1.
    @param proxy route through this proxy URL. There is no default, and
      proxy environment variables are ignored.
    @param timeout limit on a whole transfer, in seconds. There is no
      default. The portable and composable bound on a request is the
      Eio cancellation {!Fetch} documents. This one is defence in
      depth, releasing the connection promptly when a peer goes quiet.
    @param connect_timeout limit on the connection phase, in seconds
      (default 30). Defence in depth in the same way.
    @param max_response cap on a response body, in bytes (default 256
      MiB). Exceeding it fails the transfer with [Protocol_error].
    @param max_request cap on a [Stream] request body, in bytes
      (default 256 MiB). A declared length over it is refused before
      the request is sent and an undeclared body is failed as soon as
      it grows past it, both with [Invalid_request].
    @param user_agent sent when the request does not set one.
    @param verbose write libcurl's transfer chatter to stderr.
    @param max_connections_per_host cap on connections to one host.
      Defaults to libcurl's.
    @param max_total_connections cap on connections in all. Defaults to
      libcurl's.
    @param multiplex share a connection between HTTP/2 streams
      (default [true]). *)

val std :
  sw:Eio.Switch.t ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:float ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Fetch.plain
(** [std ~sw env] is a client with the defaults an application usually
    wants, built from the capabilities in [env]. [Eio_main.run]'s [env]
    works as it stands, since only its clocks and randomness are used.
    It combines the {!v} defaults with a cookie jar, per-origin flow
    control and retries, stacked so that a retried request is paced
    afresh and consults the jar again.

    @param cookies [`Memory] keeps a jar for the client's lifetime (the
      default), [`File path] persists it in curl's cookies.txt format,
      and [`Off] stores no cookies.
    @param retry retry policy (default {!Fetch.Retry.default}).
    @param max_concurrent requests in flight per origin (default 6, as
      a browser does).
    @param min_interval minimum seconds between request starts per
      origin. Unset by default.

    Policy composes on top as with any client:

    {[
      Fetch_curl.std ~sw env
      |> Fetch.restrict ~under:[ "https://api.github.com" ]
    ]}

    For a different arrangement, such as sharing one jar between clients
    or passing curl-specific options, build the stack from {!v},
    [Fetch_cookies.with_jar] and the {!Fetch} wrappers directly. *)
