(** HTTP cookies for clients and servers.

    It implements the syntax and matching rules of
    {{:https://www.rfc-editor.org/rfc/rfc6265.html}RFC 6265}. A client parses a
    [Set-Cookie] response value with {!parse_set_cookie} and formats stored
    cookies with {!cookie_header}. A server parses a [Cookie] request value with
    {!parse_cookie_header} and formats a response value with
    {!set_cookie_header}. {!Cookie_jar} provides client-side storage.

    Operations that depend on the current time take a [Ptime.t], so this module
    performs no I/O. Domain arguments must be canonical lower-case ASCII names
    without a leading dot. *)

module Same_site : sig
  type t = [ `Strict | `Lax | `None ]
  (** A [t] is the value of the [SameSite] attribute. The attribute is
      preserved, but does not affect matching because a general-purpose HTTP
      client has no browser-style notion of the current site. See
      {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.6.7}RFC
       6265bis, Section 5.6.7}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf site] is [()] after writing [site] on [ppf]. *)
end

type expiry = [ `Session | `At of Ptime.t ]
(** An [expiry] determines when a cookie expires. [`Session] lasts for the
    lifetime of the jar, and [`At t] expires at [t]. *)

type t
(** A [t] is a cookie. Cookies with the same name, domain, and path have the
    same identity and replace one another. *)

(** {1 Accessors} *)

val domain : t -> string
(** [domain cookie] is the canonical domain to which [cookie] applies. *)

val path : t -> string
(** [path cookie] is the path prefix to which [cookie] applies. *)

val name : t -> string
(** [name cookie] is the name of [cookie]. *)

val value : t -> string
(** [value cookie] is the value of [cookie], including a surrounding pair of
    double quotes when the value was received in that form. *)

val value_trimmed : t -> string
(** [value_trimmed cookie] is {!value} with one surrounding pair of double
    quotes removed, if present. The quotes delimit a value in the
    {{:https://www.rfc-editor.org/rfc/rfc6265.html#section-4.1.1}RFC 6265
     grammar}; they are not part of its contents. *)

val secure : t -> bool
(** [secure cookie] is [true] if [cookie] has the [Secure] attribute. *)

val http_only : t -> bool
(** [http_only cookie] is [true] if [cookie] has the [HttpOnly] attribute. *)

val host_only : t -> bool
(** [host_only cookie] is [true] if [cookie] applies only to the host that set
    it. Such a cookie has no [Domain] attribute. *)

val partitioned : t -> bool
(** [partitioned cookie] is [true] if [cookie] has the [Partitioned] attribute.
    The attribute is preserved, but {!Cookie_jar} does not partition storage by
    top-level site. See
    {{:https://datatracker.ietf.org/doc/html/draft-cutler-httpbis-partitioned-cookies}Cookies
     Having Independent Partitioned State}. *)

val same_site : t -> Same_site.t option
(** [same_site cookie] is the [SameSite] attribute of [cookie], if present. *)

val expiry : t -> expiry
(** [expiry cookie] is the expiry of [cookie]. *)

val creation_time : t -> Ptime.t
(** [creation_time cookie] is the time at which [cookie] was first stored. *)

val last_access : t -> Ptime.t
(** [last_access cookie] is the time at which [cookie] was last selected for a
    request. *)

(** {1 Construction} *)

val v :
  domain:string ->
  path:string ->
  name:string ->
  value:string ->
  ?secure:bool ->
  ?http_only:bool ->
  ?host_only:bool ->
  ?partitioned:bool ->
  ?same_site:Same_site.t ->
  expiry:expiry ->
  now:Ptime.t ->
  unit ->
  t
(** [v ~domain ~path ~name ~value ~expiry ~now ()] is a cookie with the given
    fields. [domain] is converted to lower case, [now] supplies both initial
    timestamps, and [host_only] defaults to [true]. The constructor does not
    validate the relationships between attributes; use {!parse_set_cookie} for
    values received from a peer.

    It raises [Invalid_argument] if [domain] or [path] is not safe to persist
    and serialize, if [name] is not a token, or if [value] holds a character
    outside the cookie grammar. See {!valid_domain}, {!valid_path},
    {!valid_name}, and {!valid_value}. *)

val touch : now:Ptime.t -> t -> t
(** [touch ~now cookie] is [cookie] with its last-access time set to [now]. *)

val with_creation_time : Ptime.t -> t -> t
(** [with_creation_time time cookie] is [cookie] with its creation time set to
    [time]. *)

(** {1 Matching and validation} *)

val is_expired : now:Ptime.t -> t -> bool
(** [is_expired ~now cookie] is [true] if [cookie] expires at or before [now]. *)

val same_identity : t -> t -> bool
(** [same_identity a b] is [true] if [a] and [b] have the same name, domain, and
    path. *)

val domain_suffix_matches : sub:string -> string -> bool
(** [domain_suffix_matches ~sub domain] is [true] if [sub] domain-matches
    [domain]: the names are equal, or [domain] is a dot-aligned suffix of the
    non-IP name [sub]. An IP literal in any spelling {!Httpz.Ip.is_literal}
    recognizes, not just the dotted quad, counts as an address rather than a
    name. Both arguments must be canonical. See
    {{:https://www.rfc-editor.org/rfc/rfc6265.html#section-5.1.3}RFC 6265,
     Section 5.1.3}. *)

val domain_matches : host:string -> t -> bool
(** [domain_matches ~host cookie] is [true] if [cookie] applies to [host]. A
    host-only cookie requires an exact match. *)

val path_matches : request_path:string -> t -> bool
(** [path_matches ~request_path cookie] is [true] if [cookie] applies to
    [request_path] under the
    {{:https://www.rfc-editor.org/rfc/rfc6265.html#section-5.1.4}RFC 6265 path
     matching rules}. An empty request path is normalized to ["/"]. *)

val compare_order : t -> t -> int
(** [compare_order a b] is negative when [a] precedes [b], positive when [b]
    precedes [a], and zero when they have equal order in a [Cookie] request
    value. Longer paths precede shorter paths, then earlier creation times
    precede later ones. Names break otherwise equal ties to make the result
    deterministic. *)

val has_secure_prefix : string -> bool
(** [has_secure_prefix name] is [true] if [name] begins with [__Secure-] or
    [__Host-], compared without regard to case. {!parse_set_cookie} validates
    the attributes promised by these prefixes, while {!Cookie_jar.set} also
    validates the request scheme. *)

val valid_name : string -> bool
(** [valid_name name] is [true] if [name] is a non-empty token accepted by the
    [Set-Cookie] grammar. *)

val valid_value : string -> bool
(** [valid_value value] is [true] if [value] contains permitted cookie octets,
    optionally surrounded by double quotes. Spaces are also accepted for
    compatibility with deployed servers. *)

val valid_domain : string -> bool
(** [valid_domain domain] is [true] for a non-empty, canonical lower-case
    ASCII hostname or IP literal with no leading or trailing dot. *)

val valid_path : string -> bool
(** [valid_path path] is [true] for an absolute cookie path with no control,
    DEL, or semicolon byte. *)

(** {1 Client operations} *)

val parse_set_cookie :
  now:Ptime.t -> host:string -> path:string -> string -> (t, string) result
(** [parse_set_cookie ~now ~host ~path value] is the cookie described by one
    [Set-Cookie] response value received for a request to [host] and [path]. It
    validates the name and value; domain and public-suffix scope; default path;
    [__Secure-], [__Host-], [SameSite], and [Partitioned] constraints; and
    [Max-Age] precedence over [Expires]. [Error reason] explains why the value
    must be ignored. [host] must be canonical.

    A [Domain] attribute equal to a public suffix, or to [host] when [host] is
    an IP literal, is ignored and the cookie is stored host-only as step 5 of
    section 5.3 requires; a public suffix that is not [host] is an error, and so
    is a public-suffix lookup that fails, since a name whose suffix is unknown
    cannot be shown to be registrable. See
    {{:https://www.rfc-editor.org/rfc/rfc6265.html#section-5.2}RFC 6265,
     Sections 5.2 and 5.3}. *)

val cookie_header : t list -> string
(** [cookie_header cookies] is a [Cookie] request value such as
    ["session=abc; theme=dark"]. [cookies] must already be filtered and sorted,
    for example by {!Cookie_jar.header_for}. *)

(** {1 Server operations} *)

val parse_cookie_header : string -> (string * string) list
(** [parse_cookie_header value] is the valid name-value pairs in a [Cookie]
    request value, in their original order. Malformed pairs are omitted and
    repeated names are retained because cookies with different paths or domains
    may share a name. *)

val set_cookie_header : t -> string
(** [set_cookie_header cookie] is a [Set-Cookie] response value for [cookie].
    Absolute expiry times use IMF-fixdate; session cookies omit [Expires]; and
    host-only cookies omit [Domain]. Construction guarantees that the emitted
    domain, path, name, and value cannot inject another attribute. The caller
    must still ensure that the attributes form a valid combination. *)

(** {1 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf cookie] is [()] after writing [cookie] on [ppf] for inspection. *)
