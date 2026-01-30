(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cookie management library for OCaml

    HTTP cookies are a mechanism defined in
    {{:https://datatracker.ietf.org/doc/html/rfc6265} RFC 6265} that allows
    "server side connections to store and retrieve information on the client
    side." Originally designed to enable persistent client-side state for web
    applications, cookies are essential for storing user preferences, session
    data, shopping cart contents, and authentication tokens.

    This library provides a complete cookie implementation following RFC 6265
    while integrating Eio for efficient asynchronous operations.

    {2 Cookie Format and Structure}

    Cookies are set via the Set-Cookie HTTP response header
    ({{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1} Section 4.1})
    with the basic format: [NAME=VALUE] with optional attributes including:
    - [expires]: Cookie lifetime specification
      ({{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.1} Section 5.2.1})
    - [max-age]: Cookie lifetime in seconds
      ({{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.2} Section 5.2.2})
    - [domain]: Valid domains using tail matching
      ({{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.3} Section 5.2.3})
    - [path]: URL subset for cookie validity
      ({{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.4} Section 5.2.4})
    - [secure]: Transmission over secure channels only
      ({{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.5} Section 5.2.5})
    - [httponly]: Not accessible to JavaScript
      ({{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.6} Section 5.2.6})
    - [samesite]: Cross-site request behavior (RFC 6265bis)
    - [partitioned]: CHIPS partitioned storage

    {2 Domain and Path Matching}

    The library implements standard domain and path matching rules from
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.3} Section 5.1.3}
    and {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.1.4} Section 5.1.4}:
    - Domain matching uses suffix matching for hostnames (e.g., "example.com"
      matches "sub.example.com")
    - IP addresses require exact match only
    - Path matching requires exact match or prefix with "/" separator

    @see <https://datatracker.ietf.org/doc/html/rfc6265> RFC 6265 - HTTP State Management Mechanism

    {2 Standards and References}

    This library implements and references the following IETF specifications:

    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265} -
       HTTP State Management Mechanism (April 2011) - Primary specification}
    {- {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC 6265bis} -
       Cookies: HTTP State Management Mechanism (Draft) - SameSite attribute and modern updates}
    {- {{:https://datatracker.ietf.org/doc/html/rfc1034#section-3.5}RFC 1034 Section 3.5} -
       Domain Names - Preferred Name Syntax for domain validation}
    {- {{:https://datatracker.ietf.org/doc/html/rfc2616#section-2.2}RFC 2616 Section 2.2} -
       HTTP/1.1 - Token syntax definition}
    {- {{:https://datatracker.ietf.org/doc/html/rfc1123#section-5.2.14}RFC 1123 Section 5.2.14} -
       Internet Host Requirements - Date format (rfc1123-date)}}

    Additional standards:
    {ul
    {- {{:https://publicsuffix.org/}Mozilla Public Suffix List} - Registry
       of public suffixes for cookie domain validation per RFC 6265 Section 5.3 Step 5}}

    {2 Related Libraries}

    {ul
    {- [Publicsuffix] - Public Suffix List lookup used for domain validation}
    {- [Cookeio_jar] - Cookie jar storage with persistence support}} *)

(** {1 Types} *)

module SameSite : sig
  type t = [ `Strict | `Lax | `None ]
  (** Cookie same-site policy for controlling cross-site request behavior.

      Defined in RFC 6265bis draft.

      - [`Strict]: Cookie only sent for same-site requests, providing maximum
        protection
      - [`Lax]: Cookie sent for same-site requests and top-level navigation
        (default for modern browsers)
      - [`None]: Cookie sent for all cross-site requests (requires [secure]
        flag per RFC 6265bis)

      @see <https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.4.7> RFC 6265bis Section 5.4.7 - The SameSite Attribute *)

  val equal : t -> t -> bool
  (** Equality function for same-site values. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer for same-site values. *)
end

module Expiration : sig
  type t = [ `Session | `DateTime of Ptime.t ]
  (** Cookie expiration strategy.

      Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3}:
      - [`Session]: Session cookie that expires when user agent session ends
        (persistent-flag = false)
      - [`DateTime time]: Persistent cookie that expires at specific time
        (persistent-flag = true)

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)

  val equal : t -> t -> bool
  (** Equality function for expiration values. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer for expiration values. *)
end

type t
(** HTTP Cookie representation with all standard attributes.

    A cookie represents a name-value pair with associated metadata that controls
    its scope, security, and lifetime. Per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3},
    cookies with the same [name], [domain], and [path] will overwrite each other
    when stored.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)

(** {1 Cookie Accessors} *)

val domain : t -> string
(** Get the domain of a cookie.

    The domain is normalized per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.3} RFC 6265 Section 5.2.3}
    (leading dots removed). *)

val path : t -> string
(** Get the path of a cookie.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.4> RFC 6265 Section 5.2.4 - The Path Attribute *)

val name : t -> string
(** Get the name of a cookie. *)

val value : t -> string
(** Get the value of a cookie. *)

val value_trimmed : t -> string
(** Get cookie value with surrounding double-quotes removed if they form a
    matching pair.

    Only removes quotes when both opening and closing quotes are present. The
    raw value is always preserved in {!value}. This is useful for handling
    quoted cookie values.

    Examples:
    - ["value"] → ["value"]
    - ["\"value\""] → ["value"]
    - ["\"value"] → ["\"value"] (no matching pair)
    - ["\"val\"\""] → ["val\""] (removes outer pair only) *)

val secure : t -> bool
(** Check if cookie has the Secure flag.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.5} RFC 6265 Section 5.2.5},
    Secure cookies are only sent over HTTPS connections.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.5> RFC 6265 Section 5.2.5 - The Secure Attribute *)

val http_only : t -> bool
(** Check if cookie has the HttpOnly flag.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.6} RFC 6265 Section 5.2.6},
    HttpOnly cookies are not accessible to client-side scripts.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.6> RFC 6265 Section 5.2.6 - The HttpOnly Attribute *)

val partitioned : t -> bool
(** Check if cookie has the Partitioned attribute.

    Partitioned cookies are part of CHIPS (Cookies Having Independent
    Partitioned State) and are stored separately per top-level site, enabling
    privacy-preserving third-party cookie functionality. Partitioned cookies
    must always be Secure.

    @see <https://developer.chrome.com/docs/privacy-sandbox/chips/> CHIPS - Cookies Having Independent Partitioned State *)

val host_only : t -> bool
(** Check if cookie has the host-only flag set.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3 Step 6}:
    - If the Set-Cookie header included a Domain attribute, host-only-flag is
      false and the cookie matches the domain and all subdomains.
    - If no Domain attribute was present, host-only-flag is true and the cookie
      only matches the exact request host.

    Example:
    - Cookie set on "example.com" with Domain=example.com: host_only=false,
      matches example.com and sub.example.com
    - Cookie set on "example.com" without Domain attribute: host_only=true,
      matches only example.com, not sub.example.com

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)

val expires : t -> Expiration.t option
(** Get the expiration attribute if set.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.1} RFC 6265 Section 5.2.1}:
    - [None]: No expiration specified (session cookie)
    - [Some `Session]: Session cookie (expires when user agent session ends)
    - [Some (`DateTime t)]: Expires at specific time [t]

    Both [max_age] and [expires] can be present simultaneously. This library
    stores both independently.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.1> RFC 6265 Section 5.2.1 - The Expires Attribute *)

val max_age : t -> Ptime.Span.t option
(** Get the max-age attribute if set.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.2} RFC 6265 Section 5.2.2},
    Max-Age specifies the cookie lifetime in seconds. Both [max_age] and
    [expires] can be present simultaneously. When both are present in a
    Set-Cookie header, browsers prioritize [max_age] per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} Section 5.3 Step 3}.

    This library stores both independently and serializes both when present.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.2> RFC 6265 Section 5.2.2 - The Max-Age Attribute *)

val same_site : t -> SameSite.t option
(** Get the same-site policy of a cookie.

    @see <https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.4.7> RFC 6265bis Section 5.4.7 - The SameSite Attribute *)

val creation_time : t -> Ptime.t
(** Get the creation time of a cookie.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3},
    this is set when the cookie is first received. *)

val last_access : t -> Ptime.t
(** Get the last access time of a cookie.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3},
    this is updated each time the cookie is retrieved for a request. *)

val make :
  domain:string ->
  path:string ->
  name:string ->
  value:string ->
  ?secure:bool ->
  ?http_only:bool ->
  ?expires:Expiration.t ->
  ?max_age:Ptime.Span.t ->
  ?same_site:SameSite.t ->
  ?partitioned:bool ->
  ?host_only:bool ->
  creation_time:Ptime.t ->
  last_access:Ptime.t ->
  unit ->
  t
(** Create a new cookie with the given attributes.

    @param domain The cookie domain (will be normalized)
    @param path The cookie path
    @param name The cookie name
    @param value The cookie value
    @param secure If true, cookie only sent over HTTPS (default: false)
    @param http_only If true, cookie not accessible to scripts (default: false)
    @param expires Expiration time
    @param max_age Lifetime in seconds
    @param same_site Cross-site request policy
    @param partitioned CHIPS partitioned storage (default: false)
    @param host_only If true, exact domain match only (default: false). Per
           {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3},
           this should be true when no Domain attribute was present in the
           Set-Cookie header.
    @param creation_time When the cookie was created
    @param last_access Last time the cookie was accessed

    Note: If [partitioned] is [true], the cookie must also be [secure]. Invalid
    combinations will result in validation errors.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model *)

(** {1 RFC 6265 Validation}

    Validation functions for cookie names, values, and attributes per
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1} RFC 6265 Section 4.1.1}.

    These functions implement the syntactic requirements from RFC 6265 to ensure
    cookies conform to the specification before being sent in HTTP headers.
    All validation failures return detailed error messages citing the specific
    RFC requirement that was violated.

    {2 Validation Philosophy}

    Per RFC 6265 Section 4, there is an important distinction between:
    - {b Server requirements} (Section 4.1): Strict syntax for generating Set-Cookie headers
    - {b User agent requirements} (Section 5): Lenient parsing for receiving Set-Cookie headers

    These validation functions enforce the {b server requirements}, ensuring that
    cookies generated by this library conform to RFC 6265 syntax. When parsing
    cookies from HTTP headers, the library may be more lenient to maximize
    interoperability with non-compliant servers.

    {2 Character Set Requirements}

    RFC 6265 restricts cookies to US-ASCII characters with specific exclusions:
    - Cookie names: RFC 2616 tokens (no CTLs, no separators)
    - Cookie values: cookie-octet characters (0x21, 0x23-0x2B, 0x2D-0x3A, 0x3C-0x5B, 0x5D-0x7E)
    - Domain values: RFC 1034 domain name syntax or IP addresses
    - Path values: Any character except CTLs and semicolon

    These functions return [Ok value] on success or [Error msg] with a detailed
    explanation of why validation failed.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 - Syntax *)

module Validate : sig
  val cookie_name : string -> (string, string) result
  (** Validate a cookie name per RFC 6265.

      Cookie names must be valid RFC 2616 tokens: one or more characters
      excluding control characters and separators.

      Per {{:https://datatracker.ietf.org/doc/html/rfc2616#section-2.2}RFC 2616 Section 2.2},
      a token is defined as: one or more characters excluding control characters
      and the following 19 separator characters: parentheses, angle brackets, at-sign,
      comma, semicolon, colon, backslash, double-quote, forward slash, square brackets,
      question mark, equals, curly braces, space, and horizontal tab.

      This means tokens consist of visible ASCII characters (33-126) excluding
      control characters (0-31, 127) and the separator characters listed above.

      @param name The cookie name to validate
      @return [Ok name] if valid, [Error message] with explanation if invalid

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1
      @see <https://datatracker.ietf.org/doc/html/rfc2616#section-2.2> RFC 2616 Section 2.2 - Basic Rules *)

  val cookie_value : string -> (string, string) result
  (** Validate a cookie value per RFC 6265.

      Cookie values must contain only cookie-octets, optionally wrapped in
      double quotes. Invalid characters include: control characters, space,
      double quote (except as wrapper), comma, semicolon, and backslash.

      Per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1}RFC 6265 Section 4.1.1},
      cookie-value may be:
      - Zero or more cookie-octet characters, or
      - Double-quoted string containing cookie-octet characters

      Where cookie-octet excludes: CTLs (0x00-0x1F, 0x7F), space (0x20),
      double-quote (0x22), comma (0x2C), semicolon (0x3B), and backslash (0x5C).

      Valid cookie-octet characters: 0x21, 0x23-0x2B, 0x2D-0x3A, 0x3C-0x5B, 0x5D-0x7E

      @param value The cookie value to validate
      @return [Ok value] if valid, [Error message] with explanation if invalid

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 *)

  val domain_value : string -> (string, string) result
  (** Validate a domain attribute value.

      Domain values must be either:
      - A valid domain name per RFC 1034 Section 3.5
      - A valid IPv4 address
      - A valid IPv6 address

      Per {{:https://datatracker.ietf.org/doc/html/rfc1034#section-3.5}RFC 1034 Section 3.5},
      preferred domain name syntax requires:
      - Labels separated by dots
      - Labels must start with a letter
      - Labels must end with a letter or digit
      - Labels may contain letters, digits, and hyphens
      - Labels are case-insensitive
      - Total length limited to 255 octets

      Leading dots are stripped per RFC 6265 Section 5.2.3 before validation.

      @param domain The domain value to validate (leading dot is stripped first)
      @return [Ok domain] if valid, [Error message] with explanation if invalid

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.2.3> RFC 6265 Section 4.1.2.3
      @see <https://datatracker.ietf.org/doc/html/rfc1034#section-3.5> RFC 1034 Section 3.5 *)

  val path_value : string -> (string, string) result
  (** Validate a path attribute value.

      Per RFC 6265 Section 4.1.1, path-value may contain any CHAR except
      control characters and semicolon.

      @param path The path value to validate
      @return [Ok path] if valid, [Error message] with explanation if invalid

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1 *)

  val max_age : int -> (int, string) result
  (** Validate a Max-Age attribute value.

      Per RFC 6265 Section 4.1.1, max-age-av uses non-zero-digit *DIGIT.
      However, per Section 5.2.2, user agents should treat values <= 0 as
      "delete immediately". This function returns [Ok] for any integer since
      the parsing code handles negative values by converting to 0.

      @param seconds The Max-Age value in seconds
      @return [Ok seconds] always (negative values are handled in parsing)

      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1> RFC 6265 Section 4.1.1
      @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2.2> RFC 6265 Section 5.2.2 *)
end

(** {1 Cookie Creation and Parsing} *)

val of_set_cookie_header :
  now:(unit -> Ptime.t) ->
  domain:string ->
  path:string ->
  string ->
  (t, string) result
(** Parse Set-Cookie response header value into a cookie.

    Parses a Set-Cookie header following
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.2} RFC 6265 Section 5.2}:
    - Basic format: [NAME=VALUE; attribute1; attribute2=value2]
    - Supports all standard attributes: [expires], [max-age], [domain], [path],
      [secure], [httponly], [samesite], [partitioned]
    - Returns [Error msg] if parsing fails or cookie validation fails, with
      a detailed explanation of what was invalid
    - The [domain] and [path] parameters provide the request context for default
      values
    - The [now] parameter is used for calculating expiry times from [max-age]
      attributes and setting creation/access times

    Validation rules applied:
    - Cookie name must be a valid RFC 2616 token (no CTLs or separators)
    - Cookie value must contain only valid cookie-octets
    - Domain must be a valid domain name (RFC 1034) or IP address
    - Path must not contain control characters or semicolons
    - Max-Age must be non-negative
    - [SameSite=None] requires the [Secure] flag to be set (RFC 6265bis)
    - [Partitioned] requires the [Secure] flag to be set (CHIPS)
    - Domain must not be a public suffix per
      {{:https://datatracker.ietf.org/doc/html/rfc6265#section-5.3} RFC 6265 Section 5.3 Step 5}
      (unless the request host exactly matches the domain). This uses the
      {{:https://publicsuffix.org/list/} Mozilla Public Suffix List} to prevent
      domain-wide cookie attacks.

    {3 Public Suffix Validation}

    Cookies with Domain attributes that are public suffixes (e.g., [.com], [.co.uk],
    [.github.io]) are rejected to prevent a malicious site from setting cookies
    that would affect all sites under that TLD.

    Examples:
    - Request from [www.example.com], Domain=[.com] → rejected (public suffix)
    - Request from [www.example.com], Domain=[.example.com] → allowed
    - Request from [blogspot.com], Domain=[.blogspot.com] → allowed (request matches)

    Example:
    {[of_set_cookie_header ~now:(fun () -> Ptime_clock.now ())
     ~domain:"example.com" ~path:"/" "session=abc123; Secure; HttpOnly"]}

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.2> RFC 6265 Section 5.2 - The Set-Cookie Header
    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-5.3> RFC 6265 Section 5.3 - Storage Model (public suffix check)
    @see <https://publicsuffix.org/list/> Public Suffix List *)

val of_cookie_header :
  now:(unit -> Ptime.t) ->
  domain:string ->
  path:string ->
  string ->
  (t list, string) result
(** Parse Cookie request header containing semicolon-separated name=value pairs.

    Parses a Cookie header following
    {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.2} RFC 6265 Section 4.2}.
    Cookie headers contain only name=value pairs without attributes:
    ["name1=value1; name2=value2; name3=value3"]

    Validates each cookie name and value per RFC 6265 and detects duplicate
    cookie names (which is forbidden per Section 4.2.1).

    Creates cookies with:
    - Provided [domain] and [path] from request context
    - All security flags set to [false] (defaults)
    - All optional attributes set to [None]
    - [host_only = true] (since we cannot determine from the header alone
      whether cookies originally had a Domain attribute)
    - [creation_time] and [last_access] set to current time from [now]

    Returns [Ok cookies] if all cookies parse successfully with no duplicates,
    or [Error msg] if any validation fails.

    Example:
    {[of_cookie_header ~now:(fun () -> Ptime_clock.now ()) ~domain:"example.com"
     ~path:"/" "session=abc; theme=dark"]}

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.2> RFC 6265 Section 4.2 - The Cookie Header *)

val make_cookie_header : t list -> string
(** Create Cookie header value from cookies.

    Formats a list of cookies into a Cookie header value suitable for HTTP
    requests per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.2} RFC 6265 Section 4.2}.
    - Format: [name1=value1; name2=value2; name3=value3]
    - Only includes cookie names and values, not attributes
    - Cookies should already be filtered for the target domain/path

    Example: [make_cookie_header cookies] might return
    ["session=abc123; theme=dark"]

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.2> RFC 6265 Section 4.2 - The Cookie Header *)

val make_set_cookie_header : t -> string
(** Create Set-Cookie header value from a cookie.

    Formats a cookie into a Set-Cookie header value suitable for HTTP responses
    per {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1} RFC 6265 Section 4.1}.
    Includes all cookie attributes: Max-Age, Expires, Domain, Path, Secure,
    HttpOnly, Partitioned, and SameSite.

    The Expires attribute uses rfc1123-date format ("Sun, 06 Nov 1994 08:49:37 GMT")
    as specified in {{:https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1} Section 4.1.1}.

    @see <https://datatracker.ietf.org/doc/html/rfc6265#section-4.1> RFC 6265 Section 4.1 - The Set-Cookie Header *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty print a cookie. *)
