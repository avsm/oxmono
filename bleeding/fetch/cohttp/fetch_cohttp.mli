(** A cohttp-eio backend for {!Fetch}.

    {[
      Eio_main.run @@ fun env ->
      let t = Fetch_cohttp.std env in
      Fetch.read t "http://example.com/"
    ]}

    {!std} mints the stack most applications want, and {!v} the bare
    backend. Whoever creates a client holds full authority over it and
    can narrow it with the {!Fetch} wrappers before passing it on.

    This backend is pure OCaml with no C library to link. A client is
    configuration and nothing more: it keeps no connection cache and
    runs no background fibers, so it needs no switch and may be shared
    between domains.

    Each request opens its own connection and drops it once the response
    body has been read or its switch has finished. There is no
    connection reuse, no HTTP/2 and no pipelining, so a workload of many
    small requests to one host is better served by [fetch-curl].

    Request and response bodies both stream, so neither costs its own
    size in memory. A response is decompressed transparently and bounded
    by [max_response], and the header block a server may send before its
    response is bounded at 256 KiB. Exceeding either raises
    [Protocol_error]. Redirects are followed by the portable
    {!Fetch.fetch} loop rather than here, so policy applies to every hop,
    and no proxy environment variable or other ambient configuration is
    consulted.

    TLS is not built in. Pass [~https] to fetch https URLs.

    {2 Inherited limitations}

    cohttp-eio's HTTP/1.1 implementation shows through in four places.

    - A request with no content still carries [Content-Length: 0], which
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-8.6}RFC 9110
      §8.6} says a user agent should not send.
    - An unsolicited interim [1xx] response cannot be skipped over to
      reach the response it precedes, so it raises [Protocol_error]
      rather than being mistaken for the answer.
    - A failure that happens while the request body is being written
      surfaces on the request's switch instead of from the call, so
      {!Fetch.with_retry} never sees it.
    - Trailer fields are discarded, so {!Fetch.trailers} is always
      [None].

    Only [gzip] is negotiated. *)

type tag = [ `Generic | `Cohttp ]

type t = tag Fetch.ty Eio.Resource.t
(** A cohttp-backed client. *)

type conn = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t
(** A connection to a server. *)

type https = Uri.t -> conn -> conn
(** A function wrapping a connection in TLS, given the URL being
    fetched. This is the wrapper [Cohttp_eio.Client.make] takes, so one
    written for that can be passed here unchanged. *)

type Eio.Exn.Backend.t += Cohttp_error of string
(** Detail attached to a [Connection_failure] this backend raises. *)

val v :
  ?https:https ->
  ?max_response:int ->
  ?user_agent:string ->
  ?decode:bool ->
  _ Eio.Net.t ->
  unit -> t
(** [v net ()] is a new client that connects through [net].

    @param https wrap the connection for an https URL. Without it an
      https URL raises [Tls_failure] before connecting.
    @param max_response cap on a response body once decoded, in bytes
      (default 256 MiB). Exceeding it raises [Protocol_error] and drops
      the connection, which bounds a compression bomb as well as a large
      response.
    @param user_agent sent when the request does not set one (default
      ["fetch-cohttp"]).
    @param decode ask for gzip and decode the response, presenting the
      headers of the decoded view (default [true]). A request that
      negotiates its own [Accept-Encoding] is handed back raw whatever
      this says. *)

val std :
  ?https:https ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:float ->
  < net : _ Eio.Net.t
  ; clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Fetch.plain
(** [std env] is a client with the defaults an application usually
    wants, built from the capabilities in [env]. [Eio_main.run]'s [env]
    works as it stands. It combines the {!v} defaults with a cookie jar,
    per-origin flow control and retries, stacked so that a retried
    request is paced afresh and consults the jar again.

    @param cookies [`Memory] keeps a jar for the client's lifetime (the
      default), [`File path] persists it in curl's cookies.txt format,
      and [`Off] stores no cookies.
    @param retry retry policy (default {!Fetch.Retry.default}).
    @param max_concurrent requests in flight per origin (default 6, as a
      browser does).
    @param min_interval minimum seconds between request starts per
      origin. Unset by default.

    Policy composes on top as with any client.

    {[
      Fetch_cohttp.std env
      |> Fetch.restrict ~under:[ "https://api.github.com" ]
    ]}

    For a different arrangement, such as sharing one jar between clients,
    build the stack from {!v}, [Fetch_cookies.with_jar] and the {!Fetch}
    wrappers directly. *)
