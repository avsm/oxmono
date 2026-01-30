(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Header Value Parsing

    This module provides parsing and generation functions for complex HTTP header
    values that go beyond simple strings.

    {2 Supported Headers}

    - {{!section-content_range}Content-Range} - Partial content range specification (RFC 9110 Section 14.4)
    - {{!section-if_range}If-Range} - Conditional range request (RFC 9110 Section 13.1.5)
    - {{!section-allow}Allow} - Supported HTTP methods (RFC 9110 Section 10.2.1)
    - {{!section-authentication_info}Authentication-Info} - Post-authentication info (RFC 9110 Section 11.6.3)
    - {{!section-retry_after}Retry-After} - Retry delay specification (RFC 9110 Section 10.2.3)
    - {{!section-accept_ranges}Accept-Ranges} - Range support indication (RFC 9110 Section 14.3)
    - {{!section-cache_status}Cache-Status} - Cache handling indication (RFC 9211)
    - {{!section-content_digest}Content-Digest / Repr-Digest} - Body integrity verification (RFC 9530)
    - {{!section-hsts}Strict-Transport-Security} - HSTS policy (RFC 6797)

    @see <https://www.rfc-editor.org/rfc/rfc9110> RFC 9110: HTTP Semantics
    @see <https://www.rfc-editor.org/rfc/rfc9211> RFC 9211: Cache-Status
    @see <https://www.rfc-editor.org/rfc/rfc9530> RFC 9530: Digest Fields
    @see <https://www.rfc-editor.org/rfc/rfc6797> RFC 6797: HTTP Strict Transport Security *)

(** {1:content_range Content-Range (RFC 9110 Section 14.4)}

    The Content-Range header indicates which part of a representation is
    enclosed when a 206 (Partial Content) response is returned.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-14.4> RFC 9110 Section 14.4 *)

type content_range = {
  unit : string;
  (** The range unit, typically "bytes" *)
  range : (int64 * int64) option;
  (** The byte range (start, end) inclusive, or None for unsatisfied range *)
  complete_length : int64 option;
  (** The complete representation length, or None if unknown *)
}

val content_range_to_string : content_range -> string
(** [content_range_to_string cr] formats a content range as a header value.

    Example: ["bytes 0-499/1234"] *)

val parse_content_range : string -> content_range option
(** [parse_content_range s] parses a Content-Range header value.

    Returns [None] if the value cannot be parsed.

    Examples:
    - ["bytes 0-499/1234"] -> [Some \{unit="bytes"; range=Some(0,499); complete_length=Some 1234\}]
    - ["bytes */1234"] -> [Some \{unit="bytes"; range=None; complete_length=Some 1234\}] *)

val make_content_range : start:int64 -> end_:int64 -> complete_length:int64 -> content_range
(** [make_content_range ~start ~end_ ~complete_length] creates a Content-Range
    value for a byte range response.

    @param start The first byte position (0-indexed)
    @param end_ The last byte position (inclusive)
    @param complete_length The total size of the representation *)

val make_unsatisfied_range : complete_length:int64 -> content_range
(** [make_unsatisfied_range ~complete_length] creates a Content-Range value for
    an unsatisfied range (416 response). *)

(** {1:if_range If-Range (RFC 9110 Section 13.1.5)}

    The If-Range header makes a Range request conditional. It can contain
    either an ETag or a Last-Modified date.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.5> RFC 9110 Section 13.1.5 *)

type if_range =
  | If_range_etag of string
  (** An entity tag (strong or weak) *)
  | If_range_date of string
  (** A Last-Modified date in HTTP-date format *)

val if_range_to_string : if_range -> string
(** [if_range_to_string ir] converts an If-Range value to a string. *)

val parse_if_range : string -> if_range option
(** [parse_if_range s] parses an If-Range header value.

    Distinguishes between ETags (contain quotes or start with W/) and
    HTTP-date values (start with a weekday abbreviation). *)

val if_range_of_etag : string -> if_range
(** [if_range_of_etag etag] creates an If-Range value from an ETag. *)

val if_range_of_date : string -> if_range
(** [if_range_of_date date] creates an If-Range value from a date string. *)

val if_range_is_etag : if_range -> bool
(** [if_range_is_etag ir] returns [true] if [ir] is an ETag. *)

val if_range_is_date : if_range -> bool
(** [if_range_is_date ir] returns [true] if [ir] is a date. *)

(** {1:allow Allow (RFC 9110 Section 10.2.1)}

    The Allow header lists the set of methods supported by the target resource.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.1> RFC 9110 Section 10.2.1 *)

val parse_allow : string -> Method.t list
(** [parse_allow s] parses an Allow header value into a list of methods.

    Example: ["GET, HEAD, PUT"] -> [\`GET; \`HEAD; \`PUT] *)

val allow_to_string : Method.t list -> string
(** [allow_to_string methods] formats a list of methods as an Allow header value.

    Example: [\`GET; \`HEAD] -> ["GET, HEAD"] *)

val allow_contains : Method.t -> string -> bool
(** [allow_contains method_ allow_value] checks if a method is in an Allow header value. *)

(** {1:authentication_info Authentication-Info (RFC 9110 Section 11.6.3)}

    The Authentication-Info header is sent by the server after successful
    authentication. For Digest authentication, it provides:

    - A new nonce for subsequent requests (avoiding 401 round-trips)
    - Response authentication (server proves it knows the password)
    - Echo of client nonce and nonce count for verification

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-11.6.3> RFC 9110 Section 11.6.3 *)

type authentication_info = {
  nextnonce : string option;
  (** Next nonce to use for subsequent requests *)
  qop : string option;
  (** Quality of protection that was used *)
  rspauth : string option;
  (** Response authentication (server proves it knows the password) *)
  cnonce : string option;
  (** Client nonce echoed back *)
  nc : string option;
  (** Nonce count echoed back *)
}

val parse_authentication_info : string -> authentication_info
(** [parse_authentication_info s] parses an Authentication-Info header value.

    Example: ["nextnonce=\"abc123\", qop=auth, rspauth=\"xyz789\""] *)

val has_nextnonce : authentication_info -> bool
(** [has_nextnonce info] returns [true] if a new nonce is provided.

    If present, the client should use this nonce for subsequent requests
    instead of waiting for a 401 response with a new challenge. *)

val get_nextnonce : authentication_info -> string option
(** [get_nextnonce info] returns the next nonce, if present. *)

(** {1:retry_after Retry-After (RFC 9110 Section 10.2.3)}

    The Retry-After header indicates how long to wait before retrying.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.3> RFC 9110 Section 10.2.3 *)

type retry_after =
  | Retry_after_date of string
  (** An HTTP-date when the resource will be available *)
  | Retry_after_seconds of int
  (** Number of seconds to wait before retrying *)

val parse_retry_after : string -> retry_after option
(** [parse_retry_after s] parses a Retry-After header value.

    Examples:
    - ["120"] -> Some (Retry_after_seconds 120)
    - ["Fri, 31 Dec 1999 23:59:59 GMT"] -> Some (Retry_after_date "...") *)

val retry_after_to_seconds : ?now:float -> retry_after -> int option
(** [retry_after_to_seconds ?now retry_after] converts to seconds.

    For [Retry_after_seconds], returns the value directly.
    For [Retry_after_date], parses the HTTP-date per
    {{:https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.7}RFC 9110 Section 5.6.7}
    and computes the difference from [now]. Returns 0 if the date is in the past.
    Returns [None] if the date cannot be parsed or [now] is not provided.

    @param now The current time as a Unix timestamp (required for date calculation) *)

(** {1:accept_ranges Accept-Ranges (RFC 9110 Section 14.3)}

    The Accept-Ranges header indicates whether the server supports range requests.

    @see <https://www.rfc-editor.org/rfc/rfc9110#section-14.3> RFC 9110 Section 14.3 *)

type accept_ranges =
  | Accept_ranges_bytes
  (** Server supports byte range requests *)
  | Accept_ranges_none
  (** Server does not support range requests *)
  | Accept_ranges_other of string
  (** Server supports some other range unit *)

val parse_accept_ranges : string -> accept_ranges
(** [parse_accept_ranges s] parses an Accept-Ranges header value.

    Examples:
    - ["bytes"] -> Accept_ranges_bytes
    - ["none"] -> Accept_ranges_none *)

val supports_byte_ranges : accept_ranges -> bool
(** [supports_byte_ranges ar] returns [true] if byte range requests are supported. *)

(** {1:cache_status Cache-Status (RFC 9211)}

    The Cache-Status header field indicates how caches have handled a request.
    It is a List structured field (RFC 8941) where each member is a cache
    identifier with optional parameters.

    Example: [Cache-Status: "Cloudflare"; hit, ExampleCDN; fwd=uri-miss; stored]

    @see <https://www.rfc-editor.org/rfc/rfc9211> RFC 9211: The Cache-Status HTTP Response Header Field *)

(** Forward/stored response indicator for Cache-Status *)
type cache_status_fwd =
  | Fwd_uri_miss
  (** The cache did not contain any matching response *)
  | Fwd_vary_miss
  (** The cache contained a response, but Vary header prevented match *)
  | Fwd_miss
  (** The cache did not find a usable response (generic) *)
  | Fwd_request
  (** The request semantics required forwarding (e.g., no-cache) *)
  | Fwd_stale
  (** The cache had a stale response that needed revalidation *)
  | Fwd_partial
  (** The cache had a partial response that needed completion *)
  | Fwd_bypass
  (** The cache was configured to bypass for this request *)
  | Fwd_other of string
  (** Other forward reason *)

(** A single cache status entry from the Cache-Status header *)
type cache_status_entry = {
  cache_id : string;
  (** Identifier for the cache (e.g., "CDN", "proxy", "Cloudflare") *)
  hit : bool option;
  (** True if served from cache without forwarding *)
  fwd : cache_status_fwd option;
  (** Why the request was forwarded *)
  fwd_status : int option;
  (** Status code from the forwarded response *)
  stored : bool option;
  (** Whether the response was stored in cache *)
  collapsed : bool option;
  (** Whether request was collapsed with others *)
  ttl : int option;
  (** Time-to-live remaining in seconds *)
  key : string option;
  (** Cache key used *)
  detail : string option;
  (** Implementation-specific detail *)
}

val cache_status_fwd_of_string : string -> cache_status_fwd
(** [cache_status_fwd_of_string s] parses a forward reason string. *)

val cache_status_fwd_to_string : cache_status_fwd -> string
(** [cache_status_fwd_to_string fwd] converts a forward reason to string. *)

val parse_cache_status_entry : string -> cache_status_entry option
(** [parse_cache_status_entry s] parses a single cache status entry.
    Format: [cache-id; param1; param2=value] *)

val parse_cache_status : string -> cache_status_entry list
(** [parse_cache_status s] parses a complete Cache-Status header value.

    Example: ["Cloudflare; hit, CDN; fwd=uri-miss"] -> list of entries *)

val cache_status_entry_to_string : cache_status_entry -> string
(** [cache_status_entry_to_string entry] formats a single entry. *)

val cache_status_to_string : cache_status_entry list -> string
(** [cache_status_to_string entries] formats entries as a header value. *)

val cache_status_is_hit : cache_status_entry list -> bool
(** [cache_status_is_hit entries] returns [true] if any cache reported a hit. *)

val cache_status_is_stored : cache_status_entry list -> bool
(** [cache_status_is_stored entries] returns [true] if any cache stored the response. *)

val cache_status_get_fwd : cache_status_entry list -> cache_status_fwd option
(** [cache_status_get_fwd entries] returns the forward reason from the first
    cache that forwarded the request, if any. *)

(** {1:content_digest Content-Digest / Repr-Digest (RFC 9530)}

    Content-Digest contains a digest of the content (after content coding).
    Repr-Digest contains a digest of the representation (before content coding).

    These headers allow integrity verification of HTTP message bodies.

    Example: [Content-Digest: sha-256=:base64digest:]

    @see <https://www.rfc-editor.org/rfc/rfc9530> RFC 9530: Digest Fields *)

(** Supported digest algorithms *)
type digest_algorithm =
  | Sha256
  (** SHA-256 (recommended by RFC 9530) *)
  | Sha512
  (** SHA-512 *)
  | Other of string
  (** Other algorithm (for forward compatibility) *)

val digest_algorithm_of_string : string -> digest_algorithm
(** [digest_algorithm_of_string s] parses an algorithm name.
    Example: ["sha-256"] -> Sha256 *)

val digest_algorithm_to_string : digest_algorithm -> string
(** [digest_algorithm_to_string algo] converts to standard algorithm name. *)

(** A single digest value with its algorithm *)
type digest_value = {
  algorithm : digest_algorithm;
  (** The hash algorithm used *)
  digest : string;
  (** The base64-encoded digest value *)
}

val parse_digest_header : string -> digest_value list
(** [parse_digest_header s] parses a Content-Digest or Repr-Digest header.

    Example: ["sha-256=:base64data:, sha-512=:base64data:"] -> list of values *)

val digest_value_to_string : digest_value -> string
(** [digest_value_to_string dv] formats a single digest value. *)

val digest_header_to_string : digest_value list -> string
(** [digest_header_to_string digests] formats as a header value. *)

val compute_sha256 : string -> string
(** [compute_sha256 content] computes SHA-256 and returns base64-encoded result. *)

val compute_sha512 : string -> string
(** [compute_sha512 content] computes SHA-512 and returns base64-encoded result. *)

val compute_digest : algorithm:digest_algorithm -> string -> digest_value
(** [compute_digest ~algorithm content] computes a digest using the specified algorithm. *)

val make_content_digest : ?algorithm:digest_algorithm -> string -> digest_value
(** [make_content_digest ?algorithm content] creates a Content-Digest value.
    Defaults to SHA-256 which is recommended by RFC 9530. *)

val validate_digest : digests:digest_value list -> string -> bool
(** [validate_digest ~digests content] validates content against provided digests.
    Returns [true] if any of the digests matches. *)

val get_strongest_digest : digest_value list -> digest_value option
(** [get_strongest_digest digests] returns the strongest available digest.
    Prefers SHA-512 over SHA-256 over others. *)

(** {1:hsts Strict-Transport-Security (RFC 6797)}

    The Strict-Transport-Security (HSTS) header tells browsers to only
    access the site over HTTPS, protecting against protocol downgrade
    attacks and cookie hijacking.

    Example: [Strict-Transport-Security: max-age=31536000; includeSubDomains; preload]

    @see <https://www.rfc-editor.org/rfc/rfc6797> RFC 6797: HTTP Strict Transport Security *)

(** HSTS directive values *)
type hsts = {
  max_age : int64;
  (** Required: Time in seconds the browser should remember HTTPS-only *)
  include_subdomains : bool;
  (** If true, policy applies to all subdomains *)
  preload : bool;
  (** If true, site requests inclusion in browser preload lists *)
}

val parse_hsts : string -> hsts option
(** [parse_hsts s] parses a Strict-Transport-Security header value.

    Returns [None] if the required max-age directive is missing.

    Example: ["max-age=31536000; includeSubDomains"] ->
             [Some \{max_age=31536000; include_subdomains=true; preload=false\}] *)

val hsts_to_string : hsts -> string
(** [hsts_to_string hsts] formats an HSTS value as a header string.

    Example: [\{max_age=31536000; include_subdomains=true; preload=false\}] ->
             ["max-age=31536000; includeSubDomains"] *)

val make_hsts : ?max_age:int64 -> ?include_subdomains:bool -> ?preload:bool -> unit -> hsts
(** [make_hsts ?max_age ?include_subdomains ?preload ()] creates an HSTS value.

    @param max_age Time in seconds (default: 1 year = 31536000)
    @param include_subdomains Apply to subdomains (default: false)
    @param preload Request preload list inclusion (default: false) *)

val hsts_is_enabled : hsts -> bool
(** [hsts_is_enabled hsts] returns [true] if HSTS is effectively enabled (max-age > 0). *)

(** {2 Common HSTS Configurations} *)

val hsts_one_year_subdomains : hsts
(** One year with subdomains - recommended for production *)

val hsts_preload : hsts
(** Two years with subdomains and preload - for HSTS preload submission *)

val hsts_disable : hsts
(** Disable HSTS by setting max-age to 0 *)
