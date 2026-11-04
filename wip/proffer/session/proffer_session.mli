(** Server-side browser sessions, with CSRF protection, for Proffer sites.

    A session lives in a {!Store}. The browser holds only an opaque ID of 256
    random bits in a [__Host-] cookie that is [Secure], [HttpOnly],
    [SameSite=Lax] and scoped to the whole host. The store is keyed by the
    SHA-256 of that ID, so a copy of the store cannot be replayed as cookies.

    {2 Behind a proxy}

    A site behind a reverse proxy that terminates TLS sees plaintext
    connections, so {!Proffer.Req.transport} is [Insecure]. Its cookies must
    still be [Secure], because the browser reaches it over HTTPS, and its
    origin must come from configuration. Request fields such as [Host] and
    [X-Forwarded-Proto] are chosen by the client and must not decide the origin
    that requests are checked against.

    {2 Cross-site requests}

    {!check_unsafe} guards requests that change state. It needs both a CSRF
    token bound to the session and a request that the browser marks as coming
    from the site's own origin. [SameSite=Lax] already withholds the cookie from
    cross-site [POST]s in current browsers, and the token covers older ones. *)

module Store : sig
  type record = {
    subject : string;  (** Who the session is for, such as a DID. *)
    created : Ptime.t;
    seen : Ptime.t;  (** When it was last used, to within a minute. *)
    expires : Ptime.t;  (** When it ends however it is used. *)
    csrf : string;  (** The secret its CSRF tokens are derived from. *)
    data : string;  (** The application's own state, opaque here. *)
  }

  type t = {
    get : string -> record option;
    put : string -> record -> unit;
    delete : string -> unit;
  }
  (** A store keyed by the hex SHA-256 of a session's ID. An application can
      back one with a database. *)

  val memory : unit -> t
  (** [memory ()] is a store held in memory, lost when the process exits. *)
end

type t
(** A site's sessions. *)

val v :
  ?idle:Ptime.Span.t ->
  ?lifetime:Ptime.Span.t ->
  name:string ->
  origin:string ->
  random:_ Eio.Flow.source ->
  clock:_ Eio.Time.clock ->
  Store.t ->
  t
(** [v ~name ~origin ~random ~clock store] keeps sessions in [store] under the
    cookie [__Host-name], for the site at [origin], such as
    [https://ci.example.org]. A session ends after [idle] without use, 2 hours by
    default, and [lifetime] after it began, 12 hours by default. [random] must
    be a cryptographically secure source, such as
    [Eio.Stdenv.secure_random env].

    Raises [Invalid_argument] if [name] is not made of ASCII letters, digits,
    [-] and [_], or if [origin] is not an [https] or loopback [http] origin
    without a path. *)

val cookie_name : t -> string
(** [cookie_name t] is [__Host-name]. *)

type session

val subject : session -> string
val data : session -> string
val created : session -> Ptime.t
val expires : session -> Ptime.t

val create : t -> subject:string -> ?data:string -> unit -> session * Proffer.Headers.field
(** [create t ~subject ~data ()] starts a session for [subject], with [data] [""]
    by default, and is the session and the [Set-Cookie] field that gives it to
    the browser. It always mints a new ID, so a session from before a login
    can never become the logged-in one. Call {!destroy} on any session the
    request already had. *)

val find : t -> Proffer.Req.t @ local -> session option
(** [find t req] is the session whose cookie [req] carries, if it is live. A
    session past either expiry is deleted. A request carrying the cookie twice
    has no session, since only one of the two can be the site's. *)

val set_data : t -> session -> string -> session
(** [set_data t s data] stores [data] in place of [s]'s and is the updated
    session. *)

val destroy : t -> session -> Proffer.Headers.field
(** [destroy t s] deletes [s] and is a [Set-Cookie] field that removes its
    cookie. *)

val clear_cookie : t -> Proffer.Headers.field
(** [clear_cookie t] is a [Set-Cookie] field that removes the session cookie,
    for a request whose session was not found. *)

(** {1 Cross-site request forgery} *)

val csrf_field : string
(** [csrf_field] is [_csrf], the form field {!check_csrf} reads. *)

val csrf_token : ?purpose:string -> session -> string
(** [csrf_token ~purpose s] is the token a form posted within [s] carries in
    {!csrf_field}. It is an HMAC of [purpose], [form] by default, under [s]'s
    secret, so each purpose has its own token and none reveals the secret. *)

val check_csrf : ?purpose:string -> session -> Proffer.Req.t @ local -> bool
(** [check_csrf ~purpose s req] holds when [req] carries [s]'s token for
    [purpose], in the form field {!csrf_field} or else the [X-CSRF-Token]
    field, compared in constant time. *)

type origin = [ `Same | `Cross | `Unknown ]

val request_origin : t -> Proffer.Req.t @ local -> origin
(** [request_origin t req] is what [req]'s [Sec-Fetch-Site] and [Origin] fields
    say about where it came from. It is [`Cross] when [Sec-Fetch-Site] is
    [cross-site] or [same-site], when [Origin] names another origin or
    [null], or when either field is repeated, and [`Same] when they name only
    the site itself. It is [`Unknown] when neither is present, as from an older
    browser. *)

val unsafe : Proffer.Method.t -> bool
(** [unsafe m] holds for [POST], [PUT], [PATCH] and [DELETE], methods that may
    change state. *)

val check_unsafe : ?purpose:string -> t -> session -> Proffer.Req.t @ local -> bool
(** [check_unsafe ~purpose t s req] holds for a request with a safe method,
    and for an unsafe one only when it is not {!request_origin} [`Cross] and
    {!check_csrf} holds. *)

(** {1 Logging in}

    A login that leaves the site, such as an OAuth authorization, must come
    back to the same browser. {!Login.start} gives the browser a random nonce in
    a cookie of its own, and the application keeps {!Login.hash} of it beside
    the login's other state. When the browser returns, {!Login.matches} checks
    the cookie against that hash.

    The cookie is [SameSite=Lax], not [Strict]. The browser returns from the
    authorization server by a top-level navigation from another site, which
    sends [Lax] cookies but not [Strict] ones. It lasts ten minutes. *)

module Login : sig
  val cookie_name : t -> string
  (** [cookie_name t] is [__Host-name-login]. *)

  val start : t -> string * Proffer.Headers.field
  (** [start t] is a new nonce and the [Set-Cookie] field that gives it to the
      browser. Keep only {!hash} of the nonce. *)

  val hash : string -> string
  (** [hash nonce] is the hex SHA-256 of [nonce]. *)

  val matches : t -> Proffer.Req.t @ local -> hash:string -> bool
  (** [matches t req ~hash] holds when [req] carries exactly one login cookie
      whose nonce has [hash], compared in constant time. *)

  val clear : t -> Proffer.Headers.field
  (** [clear t] is a [Set-Cookie] field that removes the login cookie. Send it
      when the login finishes, whether or not it succeeded. *)
end

(**/**)

val id_hash : string -> string
(** [id_hash id] is the store key of the session ID [id]. *)
