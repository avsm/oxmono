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
    memory. A POST, PUT or PATCH with no content carries [Content-Length: 0], as
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-8.6}RFC 9110 §8.6} asks of
    a method whose enclosed content has a defined meaning; any other method with
    no content carries no framing header at all, which
    {{:https://www.rfc-editor.org/rfc/rfc9112#section-6.3}RFC 9112 §6.3}
    defines as a request with no message body. An unsolicited
    interim [1xx] response is skipped to reach the response it precedes, and the
    trailer fields of a chunked response are kept, so {!Fetch.trailers} answers
    once the body has been read to its end. A trailer section that is present
    but empty answers [None], which is also the answer for a response with no
    trailer section and for a backend that surfaces no trailers, so [None] says
    only that no trailer field was received. A response is decompressed
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
    backend writes and the response head it parses must each fit in 30000
    bytes, and a response head may carry at most 100 header fields. Exceeding a
    bound raises [Invalid_request] for the request and [Protocol_error] for the
    response, naming the bound that was reached.

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
    preceding header. CRC32 and ISIZE are checked for each member. Malformed
    or truncated members raise [Protocol_error].

    Two bounds go beyond RFC 1952, which places none of its own. A member
    header, [FEXTRA], [FNAME] and [FCOMMENT] included, may not exceed 262144
    bytes, and a representation may not carry more than 1024 members. Each
    raises [Protocol_error], so a conforming stream past either bound is
    refused. *)

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
    to {!std} when an application deliberately permits plaintext HTTP only.

    The two ways to refuse HTTPS differ in what reaches the network. [no_https]
    is installed as a wrapper, so the URL's host is resolved and the TCP
    connection completed before it raises. {!v} with no [~https] at all refuses
    before dialling, and is the spelling to prefer where the connection attempt
    itself matters. {!std} always installs a wrapper, so [no_https] is the only
    way to refuse there. *)

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
  ?connect_timeout:Duration.t ->
  ?idle_timeout:Duration.t ->
  _ Eio.Net.t ->
  unit ->
  t
(** [v net ()] is a new client that connects through [net].

    [clock] measures timeouts. If omitted, neither timeout applies. {!std}
    supplies the environment's monotonic clock. [connect_timeout] defaults to
    30 seconds and bounds DNS, TCP and TLS together. [idle_timeout] defaults
    to 60 seconds and bounds each connection read or write, including body
    and trailer I/O. It does not bound the total exchange.

    [connect] defaults to DNS resolution and TCP through [net]. An override
    preserves the request URL and Host field. [https] wraps HTTPS connections.
    If omitted, HTTPS fails before connecting. {!std} supplies
    {!Httpz_tls.system}.

    [max_response] defaults to 256 MiB and independently caps coded and decoded
    response bytes. [user_agent] defaults to ["fetch-httpz"] and is sent only
    when the request omits it. [decode] defaults to [true]. It negotiates gzip
    and presents decoded headers, unless the request sets [Accept-Encoding].

    @raise Invalid_argument if [max_response] is negative, a timeout is
    negative, or [user_agent] contains a forbidden control byte.
    @raise Eio.Io on transport failure. A connection timeout carries
    [Connection_failure Timeout]. An idle timeout or body-limit violation
    carries [Protocol_error] and closes the connection. Missing TLS support
    carries [Tls_failure]. *)

val std :
  ?connect:connect ->
  ?https:https ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:Duration.t ->
  ?connect_timeout:Duration.t ->
  ?idle_timeout:Duration.t ->
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
    {!no_https} to refuse HTTPS explicitly, which refuses only once the
    connection is established.

    Policy composes on top as with any client.

    {[
    Fetch_httpz.std env |> Fetch.restrict ~under:[ "https://api.github.com" ]
    ]}

    For a different arrangement, such as sharing one jar between clients, build
    the stack from {!v}, [Fetch_cookies.with_jar] and the {!Fetch} wrappers
    directly. *)
