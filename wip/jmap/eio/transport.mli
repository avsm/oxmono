(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The HTTP stack a JMAP client runs on.

    A JMAP client is HTTP and nothing else
    ({{:https://www.rfc-editor.org/rfc/rfc8620#section-3}RFC 8620 §3}), so every
    program that talks to a server builds the same stack of an httpz backend, a
    TLS wrapper with the system trust anchors, retries and a per-origin rate
    limit. A transport is that stack built once, carrying beside it the clocks
    {!Client} times an exchange with.

    A transport is configuration rather than a connection. It opens no socket,
    runs no fiber and needs no switch. The Fetch/httpz stack built by {!v} may
    be shared between clients and domains. A transport built with {!of_fetch}
    inherits the wrapped backend's sharing restrictions. *)

type t
(** The type for HTTP stacks. A stack is an HTTP client and the clocks that pace
    it. *)

val v :
  ?https:Fetch_httpz.https ->
  ?retry:Fetch.Retry.config ->
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?max_concurrent:int ->
  ?min_interval:float ->
  < net : _ Eio.Net.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  t
(** [v ~https ~retry ~cookies ~max_concurrent ~min_interval env] is the stack
    [Fetch_httpz.std] builds from [env], with the wall and monotonic clocks of
    [env] beside it. I/O failures retain their backtraces and gain transport
    initialization context.

    [https] is the TLS wrapper and defaults through [Fetch_httpz.std] to
    [Httpz_tls.system], which verifies the URL's DNS name or IP address against
    the certificate subjectAltName and uses a DNS name for SNI. Pass a wrapper
    of your own to pin a certificate, or {!Fetch_httpz.no_https} to refuse
    HTTPS; returning [conn] unchanged would instead send the HTTPS request over
    a plaintext connection. [retry] is the retry policy and defaults to
    [Fetch.Retry.default], which reissues an idempotent request up to three
    times on [429] and [5xx] and honours [Retry-After]. A JMAP request is a
    POST, which that policy never retries.

    [cookies] selects an in-memory jar by default. [`Off] stores no cookies.
    [`File path] persists cookies at [path]. One jar is shared by all session
    endpoints, but cookie selection applies the host or domain, path and
    [Secure] rules to every request. An explicit cookie [Domain] may share a
    cookie with matching subdomains. Fetch atomically replaces [path] through a
    mode [0600] file on each persistent save. Fetch does not reject permissive
    modes when loading an existing file. The caller is responsible for the
    authority and initial permissions of [path].

    [max_concurrent] is the number of requests in flight per origin and defaults
    to what [Fetch_httpz.std] chooses. A server advertises its own bound as
    [maxConcurrentRequests]
    ({{:https://www.rfc-editor.org/rfc/rfc8620#section-2}RFC 8620 §2}), which is
    known only once the session has been fetched, so this is the bound to set
    before that. [min_interval] is the minimum spacing in seconds between
    request starts and defaults to none.

    @raise Invalid_argument
      if [max_concurrent] is below 1, if [min_interval] is not finite,
      non-negative and representable by [Duration.t], or if a [`File] cookie
      path names no file. *)

val of_fetch :
  ?clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  ?mono_clock:[> Eio.Time.Mono.ty ] Eio.Resource.t ->
  _ Fetch.t ->
  t
(** [of_fetch ~clock ~mono_clock client] is a transport over the existing HTTP
    client [client], which is how a test drives a JMAP client from a mock
    backend and how a program with its own stack reuses it. Both clocks default
    to absent. Without [clock] the transport cannot measure time and
    {!Client.connect} raises [Invalid_argument] for a [?timeout] given with it.
    Without [mono_clock] {!Push.subscribe} raises [Invalid_argument]. The
    resulting transport inherits [client]'s domain and concurrency contract. *)

val restrict : under:string list -> t -> t
(** [restrict ~under t] confines every request made through [t] to the URL
    prefixes in [under], using {!Fetch.restrict}, while preserving its clocks.
    An origin such as ["https://api.example.com"] allows every path on that
    origin; a URL with a path allows only that path and its descendants.

    This is the endpoint allowlist for a caller that does not want an
    authenticated JMAP session document to authorize arbitrary origins. The list
    must include the initial session URL and every session, API, upload,
    download, and event-source endpoint the caller intends to use. A request
    outside it is a [Fetch.Denied] transport error. *)

(** {1 Access} *)

val fetch : t -> Fetch.plain
(** [fetch t] is the HTTP client of [t]. Whoever holds it holds the authority to
    make requests with it. Narrow it with [Fetch.restrict] before passing it on.
*)

val clock : t -> float Eio.Time.clock_ty Eio.Resource.t option
(** [clock t] is the wall clock of [t], which {!Client} bounds an exchange with,
    or [None] if [t] carries none. *)

val mono_clock : t -> Eio.Time.Mono.ty Eio.Resource.t option
(** [mono_clock t] is the monotonic clock of [t], or [None] if [t] carries none.
    It is the clock to measure an interval or a backoff with, since a correction
    to the wall clock does not move it. *)
