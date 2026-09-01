(** A libcurl backend for {!Fetch}.

    {[
    Eio.Switch.run @@ fun sw ->
    let t = Fetch_curl.std ~sw env in
    Fetch.read t "https://example.com/"
    ]}

    {!std} mints the stack most applications want, and {!v} the bare backend.
    Whoever creates a client holds full authority over it and can narrow it with
    the {!Fetch} wrappers before passing it on.

    Requests through one client share a connection cache, so connections are
    reused, HTTP/2 streams multiplex, and libcurl enforces the connection caps.
    Concurrent fibers issue concurrent transfers, and cancelling a fiber aborts
    its transfer. Bodies stream in both directions, with the transfer paused
    while a reader falls behind or a [Stream] request body lags, so a request
    and its response each cost bounded memory.

    Redirects are followed by the portable {!Fetch.fetch} loop, never by
    libcurl, so policy applies to every hop. Proxy environment variables,
    [.netrc] and non-http(s) protocols are ignored unless configured. When a
    request does not set [Accept-Encoding], the backend negotiates gzip. It
    disables libcurl's content decoder and validates and decodes gzip locally,
    including concatenated members, the RFC 1952 header and trailer, and the
    decoded-size limit. The decoded view omits [Content-Encoding] and
    [Content-Length]. An unadvertised or unsupported coding such as [br] or
    [zstd] remains byte-for-byte coded with its metadata intact, rather than
    being interpreted according to how libcurl happened to be built. Callers
    can set [Accept-Encoding] explicitly to request an encoded representation;
    that disables automatic gzip decoding too.

    Libcurl's transfer decoder is also disabled. Before a response is exposed,
    the backend validates Content-Length and Transfer-Encoding with
    {!Httpz.Span}'s shared field-value parsers, requires an unambiguous
    singleton [chunked] coding, and parses chunk sizes, data CRLFs, extensions,
    and trailers with {!Httpz.Chunk}. Forbidden trailer fields are discarded.
    A malformed or incomplete frame raises [Protocol_error], and a [close]
    token anywhere in Connection prevents reuse. Bytes after a terminal chunk
    likewise quarantine the connection rather than becoming another response.
    Libcurl still owns the status-line parser, whose conservative behavior
    rejects status 099 and HTTP/1 minor versions above 1.1 instead of applying
    RFC 9112's interoperability [SHOULD]s.

    Name resolution is libcurl's. A libcurl built with the synchronous resolver
    blocks the whole Eio domain for the duration of a lookup; one built against
    c-ares or with threaded resolution does not. Check
    [curl --version] for [AsynchDNS] if a stalled domain matters.

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
  ?timeout:float ->
  ?connect_timeout:float ->
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
(** [v ~sw ()] is a new client whose event fibers and connection cache live
    until [sw] finishes.

    @param tls_verify
      [tls_verify] verifies certificates against system trust and defaults to
      [true].
    @param http_version
      [http_version] uses HTTP/2 over TLS when [`Auto] and the server offers it,
      which is the default; [`Http1_1] pins HTTP/1.1.
    @param proxy
      [proxy] routes requests through this proxy URL. It has no default, and
      proxy environment variables are ignored.
    @param timeout
      [timeout] limits a whole transfer in seconds and has no default. The
      portable and composable request bound is the Eio cancellation documented
      by {!Fetch}; this option adds defence in depth by promptly releasing a
      connection when a peer goes quiet. Zero disables this libcurl timeout.
    @param connect_timeout
      [connect_timeout] limits the connection phase in seconds and defaults to
      30, providing the same defence in depth. Zero does not disable it: it
      selects libcurl's own default of 300 seconds.
    @param max_response
      [max_response] caps both the encoded on-wire response body (including
      chunk framing and trailers) and the decoded representation in bytes, and
      defaults to 256 MiB. Exceeding either bound fails the transfer with
      [Protocol_error].
    @param max_request
      [max_request] caps a [Stream] request body in bytes and defaults to 256
      MiB. A declared length over it is refused before the request is sent,
      while an undeclared body fails as soon as it grows past the limit. A
      stream that ends before its declared length also fails. These errors raise
      [Invalid_request].
    @param user_agent
      [user_agent] is sent when the request does not set one and defaults to
      ["fetch-curl"].
    @param verbose
      [verbose] writes transfer event directions and byte counts to stderr.
      Header values, URLs, payload bytes, and libcurl's free-form diagnostic
      text are redacted because they can contain credentials.
    @param resolve
      [resolve] supplies static [(host, port, address)] mappings to libcurl. It
      preserves the URL authority and Host field while directing the TCP
      connection to [address], which is useful for service discovery and
      controlled test fixtures. Each host is canonicalized by the same URL
      parser as request authorities, so alternate numeric-IP spellings map the
      connection without changing the logical origin. Ports must be in
      [1..65535], and addresses must be numeric IPv4 or IPv6 literals; all
      entries are checked when the client is constructed.
    @param max_connections_per_host
      [max_connections_per_host] caps connections to one host and defaults to
      libcurl's setting.
    @param max_total_connections
      [max_total_connections] caps connections in total and defaults to
      libcurl's setting.
    @param multiplex
      [multiplex] shares a connection between HTTP/2 streams and defaults to
      [true].

    The call raises [Invalid_argument] if a byte or connection limit is
    negative; a timeout is negative, not finite, or outside libcurl's portable
    range; [proxy] contains NUL; or [user_agent] contains a forbidden control
    byte. *)

val std :
  sw:Eio.Switch.t ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:float ->
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
