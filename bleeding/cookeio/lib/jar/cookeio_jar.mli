(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cookie jar for storing and managing HTTP cookies.

    This module provides a complete cookie jar implementation following
    {{:https://datatracker.ietf.org/doc/html/rfc6265} RFC 6265} while
    integrating Eio for efficient asynchronous operations.

    A cookie jar maintains a collection of cookies with automatic cleanup of
    expired entries. It implements the standard browser behavior for cookie
    storage, including:
    - Automatic removal of expired cookies
    - Domain and path-based cookie retrieval per
      {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.4} Section 5.4}
    - Delta tracking for Set-Cookie headers
    - Mozilla format persistence for cross-tool compatibility

    @see <https://datatracker.ietf.org/doc/html/rfc6265> RFC 6265 - HTTP State Management Mechanism

    {2 Standards and References}

    This cookie jar implements the storage model from:

    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3}RFC 6265 Section 5.3} -
       Storage Model - Cookie insertion, replacement, and expiration}
    {- {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.4}RFC 6265 Section 5.4} -
       The Cookie Header - Cookie retrieval and ordering}}

    Key RFC 6265 requirements implemented:
    {ul
    {- Domain matching per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.3}Section 5.1.3}}
    {- Path matching per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.4}Section 5.1.4}}
    {- Cookie ordering per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.4}Section 5.4 Step 2}}
    {- Creation time preservation per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3}Section 5.3 Step 11.3}}}

    {2 Related Libraries}

    {ul
    {- {!Cookeio} - HTTP cookie parsing, validation, and serialization}
    {- [Requests] - HTTP client that uses this jar for cookie persistence}
    {- [Xdge] - XDG Base Directory support for cookie file paths}} *)

type t
(** Cookie jar for storing and managing cookies.

    A cookie jar maintains a collection of cookies with automatic cleanup of
    expired entries and enforcement of storage limits. It implements the
    standard browser behavior for cookie storage per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3}. *)

(** {1 Cookie Jar Creation and Loading} *)

val create : unit -> t
(** Create an empty cookie jar. *)

val load : clock:_ Eio.Time.clock -> Eio.Fs.dir_ty Eio.Path.t -> t
(** Load cookies from Mozilla format file.

    Loads cookies from a file in Mozilla format, using the provided clock to set
    creation and last access times. Returns an empty jar if the file doesn't
    exist or cannot be loaded. *)

val save : Eio.Fs.dir_ty Eio.Path.t -> t -> unit
(** Save cookies to Mozilla format file. *)

(** {1 Cookie Jar Management} *)

val add_cookie : t -> Cookeio.t -> unit
(** Add a cookie to the jar.

    The cookie is added to the delta, meaning it will appear in Set-Cookie
    headers when calling {!delta}. If a cookie with the same name/domain/path
    exists, it will be replaced per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3}.

    Per Section 5.3, Step 11.3, when replacing an existing cookie, the original
    creation-time is preserved. This ensures stable cookie ordering per
    Section 5.4, Step 2.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)

val add_original : t -> Cookeio.t -> unit
(** Add an original cookie to the jar.

    Original cookies are those received from the client (via Cookie header).
    They do not appear in the delta. This method should be used when loading
    cookies from incoming HTTP requests.

    Per Section 5.3, Step 11.3, when replacing an existing cookie, the original
    creation-time is preserved.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)

val delta : t -> Cookeio.t list
(** Get cookies that need to be sent in Set-Cookie headers.

    Returns cookies that have been added via {!add_cookie} and removal cookies
    for original cookies that have been removed. Does not include original
    cookies that were added via {!add_original}.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1> RFC 6265 Section 4.1 - Set-Cookie *)

val remove : t -> clock:_ Eio.Time.clock -> Cookeio.t -> unit
(** Remove a cookie from the jar.

    If an original cookie with the same name/domain/path exists, creates a
    removal cookie (empty value, Max-Age=0, past expiration) that appears in the
    delta. If only a delta cookie exists, simply removes it from the delta.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3},
    cookies are removed by sending a Set-Cookie with an expiry date in the past.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)

val get_cookies :
  t ->
  clock:_ Eio.Time.clock ->
  domain:string ->
  path:string ->
  is_secure:bool ->
  Cookeio.t list
(** Get cookies applicable for a URL.

    Implements the cookie retrieval algorithm from
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.4}RFC 6265 Section 5.4}
    for generating the Cookie header.

    {3 Algorithm}

    Per RFC 6265 Section 5.4, the user agent should:
    1. Filter cookies by domain matching (Section 5.1.3)
    2. Filter cookies by path matching (Section 5.1.4)
    3. Filter out cookies with Secure attribute when request is non-secure
    4. Filter out expired cookies
    5. Sort remaining cookies (longer paths first, then by creation time)
    6. Update last-access-time for retrieved cookies

    This function implements all these steps, combining original and delta cookies
    with delta taking precedence. Excludes:
    - Removal cookies (empty value)
    - Expired cookies (expiry-time in the past per Section 5.3)
    - Secure cookies when [is_secure = false]

    {3 Cookie Ordering}

    Cookies are sorted per Section 5.4, Step 2:
    - Cookies with longer paths are listed before cookies with shorter paths
    - Among cookies with equal-length paths, cookies with earlier creation-times
      are listed first

    This ordering ensures more specific cookies take precedence.

    {3 Matching Rules}

    Domain matching follows {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.3} Section 5.1.3}:
    - IP addresses require exact match only
    - Hostnames support subdomain matching unless host-only flag is set

    Path matching follows {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.4} Section 5.1.4}.

    @param t Cookie jar
    @param clock Clock for updating last-access-time
    @param domain Request domain
    @param path Request path
    @param is_secure Whether the request is over a secure channel (HTTPS)
    @return List of matching cookies, sorted per RFC 6265

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model (expiry)
    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.4> RFC 6265 Section 5.4 - The Cookie Header *)

val clear : t -> unit
(** Clear all cookies. *)

val clear_expired : t -> clock:_ Eio.Time.clock -> unit
(** Clear expired cookies.

    Removes cookies whose expiry-time is in the past per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3}. *)

val clear_session_cookies : t -> unit
(** Clear session cookies.

    Removes cookies that have no Expires or Max-Age attribute (session cookies).
    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3},
    these cookies are normally removed when the user agent "session" ends. *)

val count : t -> int
(** Get the number of unique cookies in the jar. *)

val get_all_cookies : t -> Cookeio.t list
(** Get all cookies in the jar.

    Returns all cookies including expired ones (for inspection/debugging).
    Use {!get_cookies} with appropriate domain/path for filtered results that
    exclude expired cookies, or call {!clear_expired} first. *)

val is_empty : t -> bool
(** Check if the jar is empty. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty print a cookie jar. *)

(** {1 Mozilla Format} *)

val to_mozilla_format : t -> string
(** Serialize cookies in Mozilla/Netscape cookie format.

    The Mozilla format uses tab-separated fields:
    {[domain \t include_subdomains \t path \t secure \t expires \t name \t value]}

    The [include_subdomains] field corresponds to the inverse of the
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} host-only-flag}
    in RFC 6265. *)

val from_mozilla_format : clock:_ Eio.Time.clock -> string -> t
(** Parse Mozilla format cookies.

    Creates a cookie jar from a string in Mozilla cookie format, using the
    provided clock to set creation and last access times. The [include_subdomains]
    field is mapped to the host-only-flag per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3}. *)
