(** The best {!Fetch} backend for the platform, selected at runtime.

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let t = Fetch_main.std ~sw env in
      Fetch.read t "https://example.com/"
    ]}

    On macOS the system networking stack ([Fetch_macos]) is used, and
    libcurl ([Fetch_curl]) everywhere else. The choice can be forced
    with the [FETCH_BACKEND] environment variable or the [?backend]
    argument. Either way the result is the backend's [std] stack, a
    cookie jar, per-origin flow control and retries, as a
    {!Fetch.plain}, so callers cannot tell which backend answered.
    Policy composes on top as with any client. *)

type backend = [ `Curl | `Macos ]

val env_var : string
(** ["FETCH_BACKEND"]. When set to ["curl"] or ["macos"]
    (case-insensitive) it overrides the platform default. Empty or
    unset means no override. *)

val select : ?backend:backend -> unit -> backend
(** [select ()] is the backend {!std} would use: [backend] if given,
    else {!env_var}'s value, else [`Macos] where NSURLSession is
    available and [`Curl] everywhere else.

    @raise Invalid_argument if {!env_var} holds anything else, or if
      the choice lands on [`Macos] on a platform without
      NSURLSession. *)

val pp : backend Fmt.t
(** [pp] formats a backend as ["curl"] or ["macos"]. *)

val of_string : string -> backend option
(** [of_string s] parses {!env_var}'s syntax: ["curl"] or ["macos"],
    case-insensitively, and [None] for anything else. *)

val std :
  sw:Eio.Switch.t ->
  ?backend:backend ->
  ?user_agent:string ->
  ?headers:Fetch.Header.headers ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:float ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Fetch.plain
(** [std ~sw env] is {!select}'s backend's [std] stack, built from the
    capabilities in [env]: an unrestricted client, able to make any
    request ({!std_ro} is its read-only counterpart). [Eio_main.run]'s
    [env] works as it stands, since only its clocks and randomness are
    used. [sw] bounds the client's lifetime, as the curl engine's event
    fibers and connection cache live under it. The macOS session does
    not need it.

    A whole-transfer time limit is Eio cancellation,
    [Eio.Time.with_timeout] around the request, rather than a client
    parameter.

    @param backend force the backend, taking precedence over
      {!env_var}.
    @param user_agent sent as [User-Agent] on every request that does
      not set its own, on whichever backend is selected. The
      application's name belongs here rather than the backend's
      default.
    @param headers default headers for every request, applied
      [`If_absent] so a request's own value wins. A credential does
      not belong here: bind it with {!Fetch.with_credentials}, which
      scopes it to a URL prefix.
    @param cookies [`Memory] keeps a jar for the client's lifetime (the
      default), [`File path] persists it in cookies.txt format, and
      [`Off] stores no cookies.
    @param retry retry policy (default {!Fetch.Retry.default}).
    @param max_concurrent requests in flight per origin (default 6, as
      a browser does).
    @param min_interval minimum seconds between request starts per
      origin. Unset by default.

    @raise Invalid_argument as {!select}. *)

val std_ro :
  sw:Eio.Switch.t ->
  ?backend:backend ->
  ?user_agent:string ->
  ?headers:Fetch.Header.headers ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:float ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Fetch.plain
(** [std_ro ~sw env] is {!Fetch.read_only} over {!std}, narrowed to the
    safe methods (GET, HEAD and OPTIONS) before it is ever handed out,
    so any other method is denied at runtime with [Denied]. Parameters
    are those of {!std}. *)
