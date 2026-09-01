(** This module provides an httpz backend for {!Fetch}.

    {[
    Eio_main.run @@ fun env ->
    let t = Fetch_httpz.std env in
    Fetch.read t "https://example.com/"
    ]}

    {!std} mints the stack most applications want, and {!v} the bare backend.
    Whoever creates a client holds full authority over it and can narrow it with
    the {!Fetch} wrappers before passing it on.

    This backend speaks HTTP/1.1 through the httpz protocol library and does
    not use libcurl. A client is configuration and nothing more:
    it keeps no connection cache and runs no background fibers, so it needs no
    switch and may be shared between domains.

    Each request opens its own connection and drops it once the response body
    has been read or its switch has finished. There is no connection reuse, no
    HTTP/2 and no pipelining, so a workload of many small requests to one host
    is better served by a backend with connection pooling.

    Request and response bodies both stream, so neither costs its own size in
    memory. A request with no content carries no framing header, which
    {{:https://www.rfc-editor.org/rfc/rfc9112#section-6.3}RFC 9112 §6.3}
    defines as a request with no message body. An unsolicited
    interim [1xx] response is skipped to reach the response it precedes, and the
    trailer fields of a chunked response are kept, so {!Fetch.trailers} answers
    once the body has been read to its end. A response is decompressed
    transparently and bounded by [max_response]. Redirects are followed by the
    {!Fetch.fetch} policy loop rather than here, so policy applies to every hop,
    and no proxy environment variable or other ambient configuration is
    consulted.

    Chunked transfer coding is decoded by the backend and removed from the
    presented fields. A body-bearing response with any other transfer coding
    is rejected: this backend cannot safely present its decoded semantics and
    will not pass a still-coded body to a downstream consumer as ordinary
    representation data. Transfer-Encoding metadata on a bodyless HEAD or 304
    response remains observable.

    The head of a message is bounded on both sides: the request head this
    backend writes and the response head it parses must each fit in 30000 bytes,
    which is httpz's [int16#]-addressed parse window with room to spare.
    Exceeding it raises [Invalid_request] for the request and [Protocol_error]
    for the response.

    {!std} supports HTTPS out of the box with the operating system's trust
    anchors and the pure-OCaml {!Httpz_tls} stack. {!v} keeps [~https] as a
    low-level injection point and refuses an HTTPS URL before dialling when no
    wrapper is supplied.

    When transparent decoding is enabled, only [gzip] is negotiated. A
    representation is decoded member by member as it streams, and neither the
    peer's write boundaries nor a member ending at a read boundary change the
    bytes produced. The gzip wrapper validates each RFC 1952 header before
    decoding: the compression method must be DEFLATE, reserved flag bits must
    be clear, [FEXTRA]'s [XLEN] is little-endian, and [FHCRC] covers the complete
    preceding header. It compensates locally for the latter two bugs in
    decompress 1.6.0 while retaining that decoder's CRC32 and ISIZE checks.
    Malformed or truncated members raise [Protocol_error]. *)

type tag = [ `Generic | `Httpz ]
(** [tag] is the resource-interface tag for this backend. *)

type t = tag Fetch.ty Eio.Resource.t
(** [t] is a client backed by httpz. *)

type conn = Httpz_tls.flow
(** [conn] is a closeable, bidirectional connection to an origin server. *)

type connect = sw:Eio.Switch.t -> host:string -> port:int -> conn
(** [connect ~sw ~host ~port] opens a connection for an HTTP origin. Supplying
    one is useful for controlled routing, service discovery, and tests that
    preserve a logical URL authority while connecting to a fixture. *)

type https = Httpz_tls.client
(** [https uri connection] is [connection] wrapped in TLS for [uri]. The wrapper
    should use the URI's host for certificate verification and SNI. *)

val no_https : https
(** [no_https] refuses an HTTPS connection with {!Fetch.Tls_failure}. Pass it
    to {!std} when an application deliberately permits plaintext HTTP only. *)

type Eio.Exn.Backend.t +=
  | Httpz_error of string
        (** [Httpz_error message] is transport error detail that the backend
            attaches to a {!Fetch.Connection_failure}. *)

val v :
  ?clock:_ Eio.Time.Mono.t ->
  ?connect:connect ->
  ?https:https ->
  ?max_response:int ->
  ?user_agent:string ->
  ?decode:bool ->
  ?connect_timeout:float ->
  ?idle_timeout:float ->
  _ Eio.Net.t ->
  unit ->
  t
(** [v net ()] is a new client that connects through [net].

    @param clock
      [clock] is the monotonic clock the timeouts below are measured against.
      {b Without it nothing is bounded}: a peer that accepts a connection and
      then goes silent holds a fiber, a socket and any flow-control slot the
      request took for as long as the caller allows, and only the caller's own
      [Eio.Time.with_timeout] ends it. {!std} always supplies one.
    @param connect_timeout
      [connect_timeout] bounds name resolution, the TCP connection and the TLS
      handshake together, and defaults to 30 seconds. Exceeding it raises
      [Connection_failure Timeout].
    @param idle_timeout
      [idle_timeout] bounds each individual read from and write to the
      connection — the request head and body, the response head, body and
      trailers, including the reads made underneath a decompressed body — and
      defaults to 60 seconds. It is not a bound on the exchange as a whole: a
      peer that keeps trickling bytes never trips it. Exceeding it raises
      [Protocol_error] naming the timeout and drops the connection.

    @param connect
      [connect] overrides origin connection establishment. It defaults to DNS
      resolution and TCP connection through [net]. The request URL and Host
      field are not rewritten when this override is used.

    @param https
      [https] wraps the connection for an HTTPS URL. Without it, an https URL
      raises [Tls_failure] before connecting. {!std} supplies
      {!Httpz_tls.system}; [v] deliberately does not.
    @param max_response
      [max_response] independently caps the coded and decoded response body in
      bytes and defaults to 256 MiB. Exceeding either bound raises
      [Protocol_error] and drops the connection, which bounds a compression
      bomb as well as a large response.
    @param user_agent
      [user_agent] is sent when the request does not set one and defaults to
      ["fetch-httpz"].
    @param decode
      [decode] requests gzip and presents the decoded view's headers; it
      defaults to [true]. A request that supplies its own [Accept-Encoding] is
      returned raw regardless of this option. *)

val std :
  ?connect:connect ->
  ?https:https ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:float ->
  ?connect_timeout:float ->
  ?idle_timeout:float ->
  < net : _ Eio.Net.t
  ; clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Fetch.plain
(** [std env] is a client with the defaults an application usually wants, built
    from the capabilities in [env]. [Eio_main.run]'s [env] works as it stands.
    It combines the {!v} defaults with a cookie jar, per-origin flow control and
    retries, stacked so that a retried request is paced afresh and consults the
    jar again. [Fetch_cookies.std] documents [cookies], [retry],
    [max_concurrent] and [min_interval], and [connect], [https],
    [connect_timeout] and [idle_timeout] are as in {!v}. The clock comes from
    [env], so the timeouts of {!v} apply to a client built this way.

    [https] defaults to {!Httpz_tls.system}, which verifies DNS names and IP
    literals against the operating system's trust anchors. Pass a custom
    {!type-https} for pinning or an alternate TLS implementation, or
    {!no_https} to refuse HTTPS explicitly.

    Policy composes on top as with any client.

    {[
    Fetch_httpz.std env |> Fetch.restrict ~under:[ "https://api.github.com" ]
    ]}

    For a different arrangement, such as sharing one jar between clients, build
    the stack from {!v}, [Fetch_cookies.with_jar] and the {!Fetch} wrappers
    directly. *)
