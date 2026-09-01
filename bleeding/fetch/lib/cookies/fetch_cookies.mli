(** This module attaches cookie jars to {!Fetch} clients.

    {!with_jar} attaches a jar to a client. It adds a [Cookie] header to
    each request and stores the [Set-Cookie] headers of each response,
    following the client rules of
    {{:https://www.rfc-editor.org/rfc/rfc6265}RFC 6265} and the prefix and
    [Secure] rules of the
    {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC
    6265bis draft}.

    {[
      Eio.Switch.run @@ fun _sw ->
      let jar = Fetch_cookies.Jar.in_memory ~clock:env#clock () in
      let t = Fetch_cookies.with_jar jar (Fetch_curl.v ~sw ()) in
      Fetch.read t "https://example.com/login"
    ]}

    Each redirect hop selects cookies by host/domain, path and [Secure] rules.
    Host-only cookies require an exact host match; [Domain] cookies may span
    subdomains, and ports do not partition storage. A [Set-Cookie] field on a
    3xx response is stored before following the redirect. *)

module Jar : sig
  type t
  (** [t] is a cookie store, bounded as
      {{:https://www.rfc-editor.org/rfc/rfc6265#section-6.1}RFC
      6265 §6.1} expects. A cookie's name and value together may not exceed
      4096 bytes, the minimum
      {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC
       6265bis} section 5.7 asks a user agent to support. The full stored tuple,
      including path and domain, is capped separately at 8192 bytes. The jar
      holds at most 50 cookies per domain and 3000 in all, evicting the least
      recently used when it is full. *)

  val in_memory : clock:_ Eio.Time.clock -> unit -> t
  (** [in_memory ~clock ()] is an empty jar that is never written to
      disk. *)

  val of_file :
    clock:_ Eio.Time.clock ->
    ?save:[ `On_change | `Manual ] ->
    ?missing:[ `Empty | `Error ] ->
    _ Eio.Path.t -> t
  (** [of_file ~clock path] is a jar backed by [path] in the Netscape
      cookies.txt format used by curl. The file is loaded if it exists,
      created on the first save, and replaced atomically when saved.
      [`On_change], the default, saves after every change, while
      [`Manual] saves only on {!flush}. [missing] defaults to [`Empty];
      [`Error] also reports a missing file as a read error. Other read failures
      always propagate. Atomic replacement requires create, rename and remove
      authority in the containing directory, retained through [path]. Each save
      owns an exclusive temporary sibling with private permissions. Saves promise
      atomic visibility, not power-loss durability (no fsync). *)

  val flush : t -> unit
  (** [flush jar] is [()] after writing [jar] to its backing file immediately.
      It has no effect on an in-memory jar. *)

  val clear : t -> unit
  (** [clear jar] is [()] after removing every cookie from [jar]. *)

  val set : t -> string -> string -> unit
  (** [set jar url line] is [()] after attempting to store the [Set-Cookie]
      value [line] as if it had arrived from [url]. It applies the same rules
      as a response, including those that depend on the scheme. An invalid URL
      or a malformed or rejected cookie is ignored. *)

  val header_for : t -> string -> string option
  (** [header_for jar url] is the [Cookie] header value to send to [url],
      or [None] if no cookie matches. Any expired cookie the lookup
      passes is evicted. An invalid URL also produces [None]. *)
end

val with_jar :
  ?scope:string list ->
  Jar.t -> _ Fetch.t -> Fetch.plain
(** [with_jar jar t] is [t] with [jar] attached, for requests under
    [scope] (all of them by default), whose entries are the URL prefixes
    {!Fetch.restrict} describes. A [Cookie] header the caller set is
    left alone.
    It raises [Invalid_argument] if a [scope] entry is not an HTTP or HTTPS
    URL, or carries a query or fragment. *)

val std :
  ?cookies:[ `Memory | `File of Eio.Fs.dir_ty Eio.Path.t | `Off ] ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?min_interval:float ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  _ Fetch.t ->
  Fetch.plain
(** [std ~cookies ~retry ~max_concurrent ~min_interval env backend] is
    [backend] wrapped in the stack a {!Fetch} backend's own [std] mints: a
    cookie jar, then per-origin flow control, then retries, so that a
    retried request is paced afresh and consults the jar again.

    @param cookies
      [cookies] selects [`Memory] for a jar kept for the client's lifetime,
      which is the default; [`File path] for persistence in curl's cookies.txt
      format; or [`Off] to store no cookies.
    @param retry
      [retry] is the retry policy and defaults to {!Fetch.Retry.default}. The
      wall clock in [env] is passed on, so a [Retry-After] in HTTP-date form is
      honoured as well as the delta-seconds form.
    @param max_concurrent
      [max_concurrent] is the maximum number of requests in flight per origin
      and defaults to 6.
    @param min_interval
      [min_interval] is the minimum number of seconds between request starts per
      origin and is unset by default. *)
