(** Cookie jars for {!Fetch}.

    {!with_jar} attaches a jar to a client. It adds a [Cookie] header to
    each request and stores the [Set-Cookie] headers of each response,
    following the client rules of
    {{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265} and the
    prefix and [Secure] rules of
    {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC
    6265bis}.

    {[
      Eio.Switch.run @@ fun sw ->
      let jar = Fetch_cookies.Jar.in_memory ~clock:env#clock () in
      let t = Fetch_cookies.with_jar jar (Fetch_curl.v ~sw ()) in
      Fetch.read t "https://example.com/login"
    ]}

    Each redirect hop consults the jar for its own URL, so a cookie
    never travels cross-site with a hop, and a [Set-Cookie] on a 3xx is
    stored as a login flow expects. *)

module Jar : sig
  type t
  (** A cookie store, bounded as
      {{:https://datatracker.ietf.org/doc/html/rfc6265#section-6.1}RFC
      6265 §6.1} expects. A cookie's name and value together may not
      exceed 4096 bytes, and the jar holds at most 50 cookies per domain
      and 3000 in all, evicting the least recently used when it is
      full. *)

  val in_memory : clock:_ Eio.Time.clock -> unit -> t
  (** [in_memory ~clock ()] is an empty jar that is never written to
      disk. *)

  val of_file :
    clock:_ Eio.Time.clock ->
    ?save:[ `On_change | `Manual ] ->
    _ Eio.Path.t -> t
  (** [of_file ~clock path] is a jar backed by [path] in the Netscape
      cookies.txt format that curl reads and writes. The file is loaded
      if it exists and created on the first save, and saves are atomic.
      [`On_change], the default, saves after every change, while
      [`Manual] saves only on {!flush}. The jar retains [path] and no
      other filesystem access. *)

  val flush : t -> unit
  (** [flush t] writes [t] out now, and does nothing for an in-memory
      jar. *)

  val clear : t -> unit
  (** [clear t] discards every cookie in [t]. *)

  val set : t -> string -> string -> unit
  (** [set t url line] stores the [Set-Cookie] value [line] as if it had
      arrived from [url], applying the same rules a response goes
      through, including those that depend on the scheme. A malformed or
      rejected value is ignored. *)

  val header_for : t -> string -> string option
  (** [header_for t url] is the [Cookie] header value to send to [url],
      or [None] if no cookie matches. Any expired cookie the lookup
      passes is evicted. *)
end

val with_jar :
  ?scope:string list ->
  Jar.t -> _ Fetch.t -> Fetch.plain
(** [with_jar jar t] is [t] with [jar] attached, for requests under
    [scope] (all of them by default), whose entries are the URL prefixes
    {!Fetch.restrict} describes. A [Cookie] header the caller set is
    left alone.
    @raise Invalid_argument if a [scope] entry is not an http or https
      URL, or carries a query. *)
