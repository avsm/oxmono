(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The client-side cookie model of
    {{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265}: parsing
    [Set-Cookie], the domain and path matching rules, and [Cookie]
    serialization.

    Internal to [fetch.cookies]. {!Fetch_cookies} is the public
    interface. This module is pure, taking the current time as an
    argument, and storage lives in [Fetch_cookies.Jar]. Domains are
    canonical throughout, meaning lowercase ASCII with no leading
    dot. *)

module Same_site : sig
  type t = [ `Strict | `Lax | `None ]
  (** The [SameSite] attribute of RFC 6265bis §5.4.7. It is parsed and
      stored, but a non-browser client has no notion of a site, so
      matching does not consult it. *)
end

type expiry = [ `Session | `At of Ptime.t ]
(** When a cookie expires. [Max-Age] takes precedence over [Expires]
    whatever their order, per RFC 6265 §5.3 step 3, so this is resolved
    once at parse time. *)

type t
(** A stored cookie. Cookies replace each other on name, domain and
    path, per RFC 6265 §5.3 step 12. See {!same_identity}. *)

(** {1 Accessors} *)

val domain : t -> string
(** [domain c] is the canonical domain [c] is scoped to. *)

val path : t -> string
(** [path c] is the path prefix [c] is scoped to. *)

val name : t -> string
(** [name c] is [c]'s name. *)

val value : t -> string
(** [value c] is [c]'s value. *)

val secure : t -> bool
(** [secure c] is [true] if [c] may only be sent over https. *)

val http_only : t -> bool
(** [http_only c] is [true] if [c] carried the [HttpOnly] attribute. *)

val host_only : t -> bool
(** [host_only c] is [true] if [c] had no [Domain] attribute, so it is
    sent only to the host that set it (RFC 6265 §5.3 step 6). *)

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
  ?same_site:Same_site.t ->
  expiry:expiry ->
  now:Ptime.t ->
  unit -> t
(** [v ~domain ~path ~name ~value ~expiry ~now ()] is a cookie with those
    fields. [domain] is lowercased and [now] stamps both times.
    [host_only] defaults to [true], which is the safe direction. The jar
    uses this when loading a file. A cookie from the wire comes from
    {!parse_set_cookie} instead. *)

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
    dot-aligned suffix of [sub] and [sub] is not an IP literal. *)

val domain_matches : host:string -> t -> bool
(** [domain_matches ~host c] is [true] if [c] should be sent to [host].
    A host-only cookie needs an exact match. *)

val path_matches : request_path:string -> t -> bool
(** [path_matches ~request_path c] is [true] if [c]'s path covers
    [request_path] (RFC 6265 §5.1.4). *)

val compare_order : t -> t -> int
(** [compare_order a b] orders cookies for the [Cookie] header per
    RFC 6265 §5.4 step 2, putting longer paths first, then earlier
    creation times. Cookies made in the same tick are ordered by name, so
    the result is reproducible. *)

val has_secure_prefix : string -> bool
(** [has_secure_prefix name] is [true] if [name] carries the [__Secure-]
    or [__Host-] prefix, matched case-insensitively per RFC 6265bis
    §4.1.3. {!parse_set_cookie} enforces the attributes those prefixes
    promise. Refusing one that arrives over plaintext is left to the jar,
    which knows the request scheme. *)

(** {1 Parsing and serialization} *)

val parse_set_cookie :
  now:Ptime.t ->
  host:string ->
  path:string ->
  string ->
  (t, string) result
(** [parse_set_cookie ~now ~host ~path line] parses one [Set-Cookie]
    value received by a request to [host] at [path], per RFC 6265 §5.2
    and §5.3. It enforces that

    - the name is a token and the value cookie-octets, with spaces
      allowed as browsers do;
    - a [Domain] attribute domain-matches [host] (§5.3 step 6) and is
      not a public suffix unless [host] is exactly that suffix (step 5);
    - [Max-Age] wins over [Expires], and a non-positive [Max-Age] means
      the cookie has already expired;
    - a missing or relative [Path] takes the §5.1.4 default path of
      [path];
    - a [__Secure-] or [__Host-] name carries the attributes §4.1.3
      requires, and [SameSite=None] carries [Secure].

    [Error reason] gives a short reason for tracing. The header is
    otherwise ignored, as §5.2 requires. *)

val cookie_header : t list -> string
(** [cookie_header cs] is the [Cookie] request header value for [cs],
    such as ["a=1; b=2"] (RFC 6265 §4.2). [cs] should already be
    filtered and sorted. *)
