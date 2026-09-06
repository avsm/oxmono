(** A libcurl backend for {!Fetch}.

    {[
    Eio.Switch.run @@ fun sw ->
    let t = Fetch_curl.std ~sw env in
    Fetch.read t "https://example.com/"
    ]}

    {!std} includes cookies, retries and request pacing. {!v} is the bare
    backend. Whoever creates a client holds full authority over it and can
    narrow it with the {!Fetch} wrappers before passing it on.

    Requests through one client share a connection cache, so connections are
    reused, HTTP/2 streams multiplex, and libcurl enforces the connection caps.
    Concurrent fibers issue concurrent transfers, and cancelling a fiber aborts
    its transfer. Bodies stream in both directions, with the transfer paused
    while a reader falls behind or a [Stream] request body lags. Fetch bounds
    its queues. Libcurl manages its own transport and decoding buffers.

    Libcurl owns HTTP parsing, transfer framing, content decoding and connection
    reuse. Response headers and trailers come from its parsed header API.
    Trailers remain separate from response headers and are available after the
    body has been fully read. This backend requires libcurl 7.83.0 or later.

    Redirects are followed by {!Fetch.fetch}, so policy applies to every hop.
    Proxy environment variables, [.netrc] and non-http(s) protocols are ignored
    unless configured. When a request does not set [Accept-Encoding], libcurl
    negotiates its supported content codings and decodes the response. The
    decoded view omits [Content-Encoding] and [Content-Length]. Setting
    [Accept-Encoding] explicitly disables automatic content decoding and
    preserves the coded representation and its metadata.

    Name resolution is libcurl's. A libcurl built with the synchronous resolver
    blocks the whole Eio domain for the duration of a lookup; one built against
    c-ares or with threaded resolution does not. Check [curl --version] for
    [AsynchDNS] if a stalled domain matters.

    A client must be used from the domain that created it. Using one elsewhere
    raises [Invalid_argument]. *)

type tag = [ `Generic | `Curl ]
(** [tag] is the resource-interface tag for this backend. *)

type t = tag Fetch.ty Eio.Resource.t
(** [t] is a client backed by libcurl. *)

type Eio.Exn.Backend.t +=
  | Curl_error of Curl.curlCode * string
        (** [Curl_error (code, message)] is libcurl's error detail. The backend
            attaches it to a {!Fetch.Connection_failure}. *)

val v :
  sw:Eio.Switch.t ->
  ?tls_verify:bool ->
  ?http_version:[ `Auto | `Http1_1 ] ->
  ?proxy:string ->
  ?timeout:Duration.t ->
  ?connect_timeout:Duration.t ->
  ?max_response:int ->
  ?max_request:int ->
  ?user_agent:string ->
  ?verbose:bool ->
  ?resolve:(string * int * string) list ->
  ?max_connections_per_host:int ->
  ?max_total_connections:int ->
  ?multiplex:bool ->
  unit ->
  t
(** [v ~sw ()] is a client whose connection cache and event fibers live until
    [sw] finishes.

    [tls_verify] defaults to [true] and checks certificates against system
    trust. [http_version] defaults to [`Auto], which negotiates HTTP/2 over TLS
    when offered. [`Http1_1] selects HTTP/1.1. [proxy] is an optional proxy URL.
    Proxy environment variables are ignored.

    [timeout] bounds the whole transfer and is unset by default. Zero disables
    it. [connect_timeout] defaults to 30 seconds. Zero selects libcurl's
    connection timeout of 300 seconds. Positive fractions of a millisecond round
    up for both options.

    [max_response] defaults to 256 MiB and caps the body bytes delivered by
    libcurl, after transfer decoding and any automatic content decoding. It
    excludes response headers, trailers and transfer framing. [max_request]
    defaults to 256 MiB and caps streamed request bytes. A declared length over
    the cap is refused before sending. Exceeding the cap while streaming or
    ending before a declared length fails the request.

    [user_agent] defaults to ["fetch-curl"] and is sent only when the request
    omits it. [verbose] defaults to [false]. When enabled, it writes transfer
    directions and byte counts to stderr, omitting URLs, header values, payloads
    and libcurl diagnostics that may contain credentials.

    [resolve] defaults to [[]]. Its [(host, port, address)] entries select
    numeric IPv4 or IPv6 addresses while preserving the URL authority and Host
    field. Hosts use the request URL's canonicalization rules. Ports must be
    between 1 and 65535.

    [max_connections_per_host] and [max_total_connections] default to libcurl's
    settings. [multiplex] defaults to [true], allowing HTTP/2 streams to share
    connections.

    @raise Invalid_argument
      if a byte or connection limit is negative or a connection limit exceeds
      the portable C-long range; a timeout exceeds [min max_int 2147483647]
      milliseconds; a resolve entry is invalid; [proxy] contains NUL; or
      [user_agent] contains a forbidden control byte; or libcurl is older than
      7.83.0.
    @raise Eio.Io
      on transport failure. A response-limit violation carries [Protocol_error].
      A request-body limit or length violation carries [Invalid_request]. *)

val std :
  sw:Eio.Switch.t ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:Duration.t ->
  ?resolve:(string * int * string) list ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Fetch.plain
(** [std ~sw env] is a client with the defaults an application usually wants,
    built from the capabilities in [env]. [Eio_main.run]'s [env] works as it
    stands, since only its clocks and randomness are used. It combines the {!v}
    defaults with a cookie jar, per-origin flow control and retries, stacked so
    that a retried request is paced afresh and consults the jar again.
    [Fetch_cookies.std] documents [cookies], [retry], [max_concurrent] and
    [min_interval], and [resolve] is as in {!v}.

    Policy composes on top as with any client:

    {[
    Fetch_curl.std ~sw env |> Fetch.restrict ~under:[ "https://api.github.com" ]
    ]}

    For a different arrangement, such as sharing one jar between clients or
    passing curl-specific options, build the stack from {!v},
    [Fetch_cookies.with_jar] and the {!Fetch} wrappers directly. *)
