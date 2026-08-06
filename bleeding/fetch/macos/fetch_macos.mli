(** A macOS NSURLSession backend for {!Fetch}.

    {[
      Eio_main.run @@ fun env ->
      let t = Fetch_macos.std env in
      Fetch.read t "https://example.com/"
    ]} *)

type tag = [ `Generic | `Macos ]

type t = tag Fetch.ty Eio.Resource.t
(** An NSURLSession-backed client. *)

val available : bool
(** [true] iff NSURLSession is available (i.e. this is macOS). *)

type Eio.Exn.Backend.t += Nsurl_error of string * int * string
(** The NSError domain, code and message, attached to a
    [Connection_failure]. *)

val v :
  ?request_timeout:float ->
  ?resource_timeout:float ->
  ?max_connections_per_host:int ->
  ?allows_cellular:bool ->
  ?allows_expensive:bool ->
  ?allows_constrained:bool ->
  ?waits_for_connectivity:bool ->
  ?min_tls:Nssessionurl.Session.tls_version ->
  ?max_response:int ->
  ?user_agent:string ->
  ?decode:bool ->
  unit -> t
(** [v ()] is a new client over a fresh ephemeral session. Omitted
    parameters keep NSURLSession's defaults.

    @param request_timeout seconds a transfer may sit idle, counted
      afresh whenever data arrives (NSURLSession default 60). The
      portable and composable bound on a request is the Eio
      cancellation {!Fetch} documents. This one is defence in depth,
      releasing the connection promptly when a peer goes quiet.
    @param resource_timeout seconds a whole transfer may take
      (NSURLSession default 7 days). Defence in depth in the same way.
    @param max_connections_per_host cap on simultaneous connections to
      one origin.
    @param allows_cellular permit cellular interfaces (default true).
    @param allows_expensive permit interfaces the system marks
      expensive, e.g. personal hotspots (default true).
    @param allows_constrained permit use in Low-Data-Mode (default
      true).
    @param waits_for_connectivity queue requests until connectivity
      appears instead of failing immediately (default false). While a
      request waits, the platform raises none of the connection
      failures {!Fetch.with_retry} would otherwise see and retry, so
      combine the two deliberately.
    @param min_tls lowest TLS version to negotiate.
    @param max_response cap on a response body, in bytes (default 256
      MiB). Exceeding it fails the transfer with [Protocol_error].
    @param user_agent sent when the request does not set one.
    @param decode let NSURLSession negotiate compression and hand back
      decoded bytes, with the headers describing the decoded view
      (default true). When false, [Accept-Encoding: identity] is sent. A
      request carrying its own [Accept-Encoding] opts out either way and
      receives the encoded body untouched.

    @raise Failure on platforms other than macOS. *)

val std :
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:float ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Fetch.plain
(** [std env] is a client with the defaults an application usually
    wants, built from the capabilities in [env].

    @param cookies [`Memory] keeps a jar for the client's lifetime (the
      default), [`File path] persists it in cookies.txt format, and
      [`Off] stores no cookies.
    @param retry retry policy (default {!Fetch.Retry.default}).
    @param max_concurrent requests in flight per origin (default 6, as
      a browser does).
    @param min_interval minimum seconds between request starts per
      origin. Unset by default.

    Policy composes on top as with any client:

    {[
      Fetch_macos.std env
      |> Fetch.restrict ~under:[ "https://api.github.com" ]
    ]}

    For a different arrangement, such as sharing one jar between
    clients or passing session options, build the stack from {!v},
    [Fetch_cookies.with_jar] and the {!Fetch} wrappers directly. *)
