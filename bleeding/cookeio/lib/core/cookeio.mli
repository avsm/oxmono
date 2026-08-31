(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The cookie model of
    {{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265}, for both
    sides of the protocol.

    A client parses [Set-Cookie] with {!parse_set_cookie}, which applies
    the storage rules of §5.2 and §5.3 (domain matching against the
    setting host, the public-suffix check, default paths, [Max-Age]
    precedence, and the name-prefix rules of
    {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC
    6265bis}), then serializes stored cookies with {!cookie_header}.
    Storage lives in {!Cookeio_jar}.

    A server parses [Cookie] with {!parse_cookie_header} and emits
    [Set-Cookie] with {!set_cookie_header}.

    This module is pure: the current time arrives as a [Ptime.t]
    argument. Domains are canonical throughout, meaning lowercase ASCII
    with no leading dot. *)

module Same_site : sig
  type t = [ `Strict | `Lax | `None ]
  (** The [SameSite] attribute of RFC 6265bis §5.4.7. It is parsed and
      stored; a non-browser client has no notion of a site, so matching
      does not consult it. *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

type expiry = [ `Session | `At of Ptime.t ]
(** When a cookie expires. [Max-Age] takes precedence over [Expires]
    whatever their order, per RFC 6265 §5.3 step 3, so this is resolved
    once at parse time. *)

type t
(** A cookie. Cookies replace each other on name, domain and path, per
    RFC 6265 §5.3 step 12. See {!same_identity}. *)

(** {1 Accessors} *)

val domain : t -> string
(** [domain c] is the canonical domain [c] is scoped to. *)

val path : t -> string
(** [path c] is the path prefix [c] is scoped to. *)

val name : t -> string
(** [name c] is [c]'s name. *)

val value : t -> string
(** [value c] is [c]'s value, verbatim, including any double-quote
    wrapper it arrived with. See {!value_trimmed}. *)

val value_trimmed : t -> string
(** [value_trimmed c] is {!value} with a surrounding pair of double
    quotes removed, when both are present. The DQUOTE wrapper is part of
    the RFC 6265 §4.1.1 grammar but not of the value. *)

val secure : t -> bool
(** [secure c] is [true] if [c] may only be sent over https. *)

val http_only : t -> bool
(** [http_only c] is [true] if [c] carried the [HttpOnly] attribute. *)

val host_only : t -> bool
(** [host_only c] is [true] if [c] had no [Domain] attribute, so it is
    sent only to the host that set it (RFC 6265 §5.3 step 6). *)

val partitioned : t -> bool
(** [partitioned c] is [true] if [c] carried the [Partitioned] attribute
    of {{:https://datatracker.ietf.org/doc/html/draft-cutler-httpbis-partitioned-cookies}CHIPS}.
    A partitioned cookie is scoped to the top-level site it was set
    under; a client that does not partition its store treats it as an
    ordinary cookie. *)

val same_site : t -> Same_site.t option
(** [same_site c] is [c]'s [SameSite] attribute, if it had one. *)

val expiry : t -> expiry
(** [expiry c] is when [c] expires. *)

val creation_time : t -> Ptime.t
(** [creation_time c] is when [c] was first stored. *)

val last_access : t -> Ptime.t
(** [last_access c] is when [c] was last sent. *)

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
  unit -> t
(** [v ~domain ~path ~name ~value ~expiry ~now ()] is a cookie with
    those fields. [domain] is lowercased and [now] stamps both times.
    [host_only] defaults to [true], which is the safe direction. This is
    for building a cookie by hand — a server minting one to emit, or a
    jar loading a file. A cookie from the wire comes from
    {!parse_set_cookie} instead, which enforces what this constructor
    does not. *)

val touch : now:Ptime.t -> t -> t
(** [touch ~now c] is [c] with its last-access time set to [now]
    (RFC 6265 §5.4 step 3). *)

val with_creation_time : Ptime.t -> t -> t
(** [with_creation_time time c] is [c] created at [time]. A replacing
    cookie inherits the time of the one it replaces, which keeps the
    §5.4 ordering stable. *)

(** {1 Predicates} *)

val is_expired : now:Ptime.t -> t -> bool
(** [is_expired ~now c] is [true] if [c]'s expiry has passed. *)

val same_identity : t -> t -> bool
(** [same_identity a b] is [true] if [a] and [b] share a name, domain
    and path, so that one replaces the other. *)

val domain_suffix_matches : sub:string -> string -> bool
(** [domain_suffix_matches ~sub d] is [true] if [sub] domain-matches [d]
    per RFC 6265 §5.1.3, that is if they are equal or [d] is a
    dot-aligned suffix of [sub] and [sub] is not an IP literal. Both
    must be canonical (lowercase). *)

val domain_matches : host:string -> t -> bool
(** [domain_matches ~host c] is [true] if [c] should be sent to [host].
    A host-only cookie needs an exact match. *)

val path_matches : request_path:string -> t -> bool
(** [path_matches ~request_path c] is [true] if [c]'s path covers
    [request_path] (RFC 6265 §5.1.4). *)

val compare_order : t -> t -> int
(** [compare_order a b] orders cookies for the [Cookie] header per
    RFC 6265 §5.4 step 2, putting longer paths first, then earlier
    creation times. Cookies made in the same tick are ordered by name,
    so the result is reproducible. *)

val has_secure_prefix : string -> bool
(** [has_secure_prefix name] is [true] if [name] carries the [__Secure-]
    or [__Host-] prefix, matched case-insensitively per RFC 6265bis
    §4.1.3. {!parse_set_cookie} enforces the attributes those prefixes
    promise. Refusing one that arrives over plaintext is left to the
    jar, which knows the request scheme. *)

(** {1 Syntax} *)

val valid_name : string -> bool
(** [valid_name n] is [true] if [n] is a non-empty RFC 2616 token, the
    grammar RFC 6265 §4.1.1 requires of a cookie name. *)

val valid_value : string -> bool
(** [valid_value s] is [true] if [s] is cookie-octets per RFC 6265
    §4.1.1, optionally in a DQUOTE wrapper, with spaces also allowed as
    browsers accept them. A server should check this before emitting a
    value with {!set_cookie_header}. *)

(** {1 The client side: Set-Cookie in, Cookie out} *)

val parse_set_cookie :
  now:Ptime.t ->
  host:string ->
  path:string ->
  string ->
  (t, string) result
(** [parse_set_cookie ~now ~host ~path line] parses one [Set-Cookie]
    value received by a request to [host] at [path], per RFC 6265 §5.2
    and §5.3. [host] must be canonical (lowercase). It enforces that

    - the name is a token and the value cookie-octets, with spaces
      allowed as browsers do;
    - a [Domain] attribute domain-matches [host] (§5.3 step 6) and is
      not a public suffix unless [host] is exactly that suffix (step 5);
    - [Max-Age] wins over [Expires], and a non-positive [Max-Age] means
      the cookie has already expired;
    - a missing or relative [Path] takes the §5.1.4 default path of
      [path];
    - a [__Secure-] or [__Host-] name carries the attributes §4.1.3
      requires, [SameSite=None] carries [Secure], and so does
      [Partitioned].

    [Error reason] gives a short reason for tracing. The header is
    otherwise ignored, as §5.2 requires. *)

val cookie_header : t list -> string
(** [cookie_header cs] is the [Cookie] request header value for [cs],
    such as ["a=1; b=2"] (RFC 6265 §4.2). [cs] should already be
    filtered and sorted. *)

(** {1 The server side: Cookie in, Set-Cookie out} *)

val parse_cookie_header : string -> (string * string) list
(** [parse_cookie_header line] is the name-value pairs of a [Cookie]
    request header, in order. Parsing is lenient, as §4.2.2 advises a
    server to be: pairs whose name is not a token or whose value is not
    cookie-octets are dropped, as is a stray segment with no [=].
    Repeated names are kept — two cookies with the same name but
    different domains or paths legitimately arrive together, and §5.4
    orders the more specific one first. *)

val set_cookie_header : t -> string
(** [set_cookie_header c] is the [Set-Cookie] response header value for
    [c] (RFC 6265 §4.1). A [`At] expiry is written as an [Expires]
    attribute in IMF-fixdate form; a [`Session] expiry writes none. The
    [Domain] attribute is written only when [c] is not host-only —
    naming the domain is what widens a cookie to subdomains, so a
    host-only cookie must omit it. The caller is responsible for
    {!valid_name} and {!valid_value} holding, as when [c] was made by
    {!v}. *)

(** {1 Pretty printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf c] prints [c] for debugging. *)
