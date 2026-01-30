(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP header field handling per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-5}RFC 9110 Section 5}

    This module provides an efficient implementation of HTTP headers with
    case-insensitive field names per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-5.1}RFC 9110 Section 5.1}.
    Headers can have multiple values for the same field name (e.g., Set-Cookie).

    {2 Type-Safe Header Names}

    Header names use the {!Header_name.t} type, providing compile-time safety
    for standard headers while allowing custom headers via [`Other]:

    {[
      let headers = Headers.empty
        |> Headers.set `Content_type "application/json"
        |> Headers.set `Authorization "Bearer token"
        |> Headers.set (`Other "X-Custom") "value"
    ]}

    {2 Security}

    Header names and values are validated to prevent HTTP header injection
    attacks. CR and LF characters are rejected per
    {{:https://datatracker.ietf.org/doc/html/rfc9110#section-5.5}RFC 9110 Section 5.5}.
*)

(** Log source for header operations *)
val src : Logs.Src.t

type t
(** Abstract header collection type. Headers are stored with case-insensitive
    keys and maintain insertion order. *)

(** {1 Creation and Conversion} *)

val empty : t
(** [empty] creates an empty header collection. *)

val of_list : (string * string) list -> t
(** [of_list pairs] creates headers from an association list of string pairs.
    This is useful when parsing headers from the wire format.
    Later entries override earlier ones for the same key. *)

val to_list : t -> (string * string) list
(** [to_list headers] converts headers to an association list.
    The order of headers is preserved. *)

(** {1 Header Injection Prevention} *)

exception Invalid_header of { name: string; reason: string }
(** Raised when a header name or value contains invalid characters (CR/LF)
    that could enable HTTP request smuggling attacks. *)

exception Invalid_basic_auth of { reason: string }
(** Raised when Basic auth credentials contain invalid characters.
    Per {{:https://datatracker.ietf.org/doc/html/rfc7617#section-2}RFC 7617 Section 2}:
    - Username must not contain colon characters
    - Username and password must not contain control characters (0x00-0x1F, 0x7F) *)

(** {1 Type-Safe Header Operations}

    These functions use {!Header_name.t} for compile-time type safety. *)

val add : Header_name.t -> string -> t -> t
(** [add name value headers] adds a header value. Multiple values
    for the same header name are allowed (e.g., for Set-Cookie).

    @raise Invalid_header if the header value contains CR/LF characters
           (to prevent HTTP header injection attacks). *)

val set : Header_name.t -> string -> t -> t
(** [set name value headers] sets a header value, replacing any
    existing values for that header name.

    @raise Invalid_header if the header value contains CR/LF characters
           (to prevent HTTP header injection attacks). *)

val get : Header_name.t -> t -> string option
(** [get name headers] returns the first value for a header name,
    or [None] if the header doesn't exist. *)

val get_all : Header_name.t -> t -> string list
(** [get_all name headers] returns all values for a header name.
    Returns an empty list if the header doesn't exist. *)

val remove : Header_name.t -> t -> t
(** [remove name headers] removes all values for a header name. *)

val mem : Header_name.t -> t -> bool
(** [mem name headers] checks if a header name exists. *)

(** {1 String-Based Header Operations}

    These functions accept string header names for wire format compatibility.
    Use these when parsing HTTP messages where header names arrive as strings. *)

val add_string : string -> string -> t -> t
(** [add_string name value headers] adds a header using a string name.
    Use this when parsing headers from the wire.

    @raise Invalid_header if the header name or value contains CR/LF characters. *)

val set_string : string -> string -> t -> t
(** [set_string name value headers] sets a header using a string name.

    @raise Invalid_header if the header name or value contains CR/LF characters. *)

val get_string : string -> t -> string option
(** [get_string name headers] gets a header using a string name. *)

val get_all_string : string -> t -> string list
(** [get_all_string name headers] gets all values for a string header name. *)

val remove_string : string -> t -> t
(** [remove_string name headers] removes a header using a string name. *)

val mem_string : string -> t -> bool
(** [mem_string name headers] checks if a header exists using a string name. *)

(** {1 Merging} *)

val merge : t -> t -> t
(** [merge base override] merges two header collections.
    Headers from [override] replace those in [base]. *)

(** {1 Common Header Builders}

    Convenience functions for setting common HTTP headers.
*)

val content_type : Mime.t -> t -> t
(** [content_type mime headers] sets the Content-Type header. *)

val content_length : int64 -> t -> t
(** [content_length length headers] sets the Content-Length header. *)

val accept : Mime.t -> t -> t
(** [accept mime headers] sets the Accept header. *)

val accept_language : string -> t -> t
(** [accept_language lang headers] sets the Accept-Language header.
    Per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.4}RFC 9110 Section 12.5.4}.

    Examples:
    {[
      headers |> Headers.accept_language "en-US"
      headers |> Headers.accept_language "en-US, en;q=0.9, de;q=0.8"
      headers |> Headers.accept_language "*"
    ]} *)

val authorization : string -> t -> t
(** [authorization value headers] sets the Authorization header with a raw value. *)

val bearer : string -> t -> t
(** [bearer token headers] sets the Authorization header with a Bearer token.
    Example: [bearer "abc123"] sets ["Authorization: Bearer abc123"] *)

val basic : username:string -> password:string -> t -> t
(** [basic ~username ~password headers] sets the Authorization header with
    HTTP Basic authentication (base64-encoded username:password).

    @raise Invalid_basic_auth if the username contains a colon character or if
           either username or password contains control characters (RFC 7617 Section 2). *)

val user_agent : string -> t -> t
(** [user_agent ua headers] sets the User-Agent header. *)

val host : string -> t -> t
(** [host hostname headers] sets the Host header. *)

val cookie : string -> string -> t -> t
(** [cookie name value headers] adds a cookie to the Cookie header.
    Multiple cookies can be added by calling this function multiple times. *)

val range : start:int64 -> ?end_:int64 -> unit -> t -> t
(** [range ~start ?end_ () headers] sets the Range header for partial content.
    Example: [range ~start:0L ~end_:999L ()] requests the first 1000 bytes. *)

(** {1 HTTP 100-Continue Support}

    Per Recommendation #7: Expect: 100-continue protocol for large uploads.
    RFC 9110 Section 10.1.1 (Expect) *)

val expect : string -> t -> t
(** [expect value headers] sets the Expect header.
    Example: [expect "100-continue"] for large request bodies. *)

val expect_100_continue : t -> t
(** [expect_100_continue headers] sets [Expect: 100-continue].
    Use this for large uploads to allow the server to reject the request
    before the body is sent, saving bandwidth. *)

(** {1 TE Header Support}

    Per RFC 9110 Section 10.1.4: The TE header indicates what transfer codings
    the client is willing to accept in the response, and whether the client is
    willing to accept trailer fields in a chunked transfer coding. *)

val te : string -> t -> t
(** [te value headers] sets the TE header to indicate accepted transfer codings.
    Example: [te "trailers, deflate"] *)

val te_trailers : t -> t
(** [te_trailers headers] sets [TE: trailers] to indicate the client accepts
    trailer fields in chunked transfer coding. Per RFC 9110 Section 10.1.4,
    a client MUST send this if it wishes to receive trailers. *)

(** {1 Cache Control Headers}

    Per Recommendation #17 and #19: Response caching and conditional requests.
    RFC 9111 (HTTP Caching), RFC 9110 Section 8.8.2-8.8.3 (Last-Modified, ETag) *)

val if_none_match : string -> t -> t
(** [if_none_match etag headers] sets the If-None-Match header for conditional requests.
    The request succeeds only if the resource's ETag does NOT match.
    Used with GET/HEAD to implement efficient caching (returns 304 Not Modified if matches). *)

val if_match : string -> t -> t
(** [if_match etag headers] sets the If-Match header for conditional requests.
    The request succeeds only if the resource's ETag matches.
    Used with PUT/DELETE for optimistic concurrency (prevents lost updates). *)

val if_modified_since : string -> t -> t
(** [if_modified_since date headers] sets the If-Modified-Since header.
    The date should be in HTTP-date format (RFC 9110 Section 5.6.7).
    Example: ["Sun, 06 Nov 1994 08:49:37 GMT"] *)

val if_unmodified_since : string -> t -> t
(** [if_unmodified_since date headers] sets the If-Unmodified-Since header.
    The request succeeds only if the resource has NOT been modified since the date. *)

val http_date_of_ptime : Ptime.t -> string
(** [http_date_of_ptime time] formats a Ptime.t as an HTTP-date.
    Format: "Sun, 06 Nov 1994 08:49:37 GMT" (RFC 9110 Section 5.6.7) *)

val if_modified_since_ptime : Ptime.t -> t -> t
(** [if_modified_since_ptime time headers] sets If-Modified-Since using a Ptime.t value. *)

val if_unmodified_since_ptime : Ptime.t -> t -> t
(** [if_unmodified_since_ptime time headers] sets If-Unmodified-Since using a Ptime.t value. *)

val cache_control : string -> t -> t
(** [cache_control directives headers] sets the Cache-Control header with a raw directive string.
    Example: [cache_control "no-cache, max-age=3600"] *)

val cache_control_directives :
  ?max_age:int ->
  ?max_stale:int option option ->
  ?min_fresh:int ->
  ?no_cache:bool ->
  ?no_store:bool ->
  ?no_transform:bool ->
  ?only_if_cached:bool ->
  unit -> t -> t
(** [cache_control_directives ?max_age ?max_stale ?min_fresh ~no_cache ~no_store
    ~no_transform ~only_if_cached () headers] builds a Cache-Control header
    from individual directives (RFC 9111 request directives).

    - [max_age]: Maximum age in seconds the client is willing to accept
    - [max_stale]: Accept stale responses:
      - [None]: omit max_stale entirely
      - [Some None]: "max-stale" (accept any staleness)
      - [Some (Some n)]: "max-stale=N" (accept n seconds staleness)
    - [min_fresh]: Response must be fresh for at least n more seconds
    - [no_cache]: Force revalidation with origin server
    - [no_store]: Response must not be stored in cache
    - [no_transform]: Intermediaries must not transform the response
    - [only_if_cached]: Only return cached response, 504 if not available *)

val etag : string -> t -> t
(** [etag value headers] sets the ETag header (for responses).
    Example: [etag "\"abc123\""] *)

val last_modified : string -> t -> t
(** [last_modified date headers] sets the Last-Modified header (for responses).
    The date should be in HTTP-date format. *)

val last_modified_ptime : Ptime.t -> t -> t
(** [last_modified_ptime time headers] sets Last-Modified using a Ptime.t value. *)

(** {1 Connection Header Handling}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-7.6.1}RFC 9110 Section 7.6.1}:
    The Connection header field lists hop-by-hop header fields that MUST be
    removed before forwarding the message. *)

val parse_connection_header : t -> Header_name.t list
(** [parse_connection_header headers] parses the Connection header value
    into a list of header names. *)

val get_hop_by_hop_headers : t -> Header_name.t list
(** [get_hop_by_hop_headers headers] returns all hop-by-hop headers.
    This is the union of {!Header_name.hop_by_hop_headers} and any headers
    listed in the Connection header. *)

val remove_hop_by_hop : t -> t
(** [remove_hop_by_hop headers] removes all hop-by-hop headers.
    This should be called before caching or forwarding a response.
    Per RFC 9110 Section 7.6.1. *)

val connection_close : t -> bool
(** [connection_close headers] returns [true] if Connection: close is present.
    This indicates the connection should be closed after the current message. *)

val connection_keep_alive : t -> bool
(** [connection_keep_alive headers] returns [true] if Connection: keep-alive is present.
    This is primarily used with HTTP/1.0 to request a persistent connection. *)

(** {1 Aliases} *)

val get_multi : Header_name.t -> t -> string list
(** [get_multi] is an alias for {!get_all}. *)

(** Pretty printer for headers *)
val pp : Format.formatter -> t -> unit

(** Brief pretty printer showing count only *)
val pp_brief : Format.formatter -> t -> unit

(** {1 HTTP/2 Pseudo-Header Support}

    HTTP/2 uses pseudo-header fields to convey information that was previously
    carried in the request line (HTTP/1.1) or status line. Pseudo-headers start
    with a colon character ([:]).

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-8.3}RFC 9113 Section 8.3}:
    - Pseudo-headers MUST appear before regular headers
    - Pseudo-headers MUST NOT appear in trailers
    - Unknown pseudo-headers MUST be treated as malformed

    {2 Request Pseudo-Headers}

    - [:method] - HTTP method (required)
    - [:scheme] - URI scheme (required for non-CONNECT)
    - [:authority] - Authority portion of URI (host:port)
    - [:path] - Path and query (required for non-CONNECT)

    {2 Response Pseudo-Headers}

    - [:status] - HTTP status code (required)
*)

val is_pseudo_header : string -> bool
(** [is_pseudo_header name] returns [true] if the header name starts with [:].
    Per RFC 9113 Section 8.3, pseudo-headers are identified by a colon prefix. *)

val get_pseudo : string -> t -> string option
(** [get_pseudo name headers] retrieves a pseudo-header value.
    The [name] should NOT include the colon prefix.
    Example: [get_pseudo "method" headers] retrieves [:method]. *)

val set_pseudo : string -> string -> t -> t
(** [set_pseudo name value headers] sets a pseudo-header value.
    The [name] should NOT include the colon prefix.
    Pseudo-headers are stored with the colon prefix internally.
    Example: [set_pseudo "method" "GET" headers] sets [:method: GET].

    @raise Invalid_header if the value contains CR/LF characters. *)

val remove_pseudo : string -> t -> t
(** [remove_pseudo name headers] removes a pseudo-header.
    The [name] should NOT include the colon prefix. *)

val mem_pseudo : string -> t -> bool
(** [mem_pseudo name headers] returns [true] if the pseudo-header exists.
    The [name] should NOT include the colon prefix. *)

val has_pseudo_headers : t -> bool
(** [has_pseudo_headers headers] returns [true] if any pseudo-headers are present. *)

val pseudo_headers : t -> (string * string) list
(** [pseudo_headers headers] returns all pseudo-headers as [(name, value)] pairs.
    Names are returned WITHOUT the colon prefix. *)

val regular_headers : t -> (string * string) list
(** [regular_headers headers] returns all non-pseudo headers as [(name, value)] pairs. *)

val to_list_ordered : t -> (string * string) list
(** [to_list_ordered headers] returns all headers with pseudo-headers first,
    followed by regular headers, as required by RFC 9113 Section 8.3. *)

(** {2 HTTP/2 Request Header Construction} *)

val h2_request :
  meth:string ->
  scheme:string ->
  ?authority:string ->
  path:string ->
  t ->
  t
(** [h2_request ~meth ~scheme ?authority ~path headers] sets the required
    HTTP/2 request pseudo-headers.

    Per RFC 9113 Section 8.3.1:
    - [:method] is required
    - [:scheme] is required (except for CONNECT)
    - [:path] is required (except for CONNECT, OPTIONS with empty path)
    - [:authority] is optional but recommended

    Example:
    {[
      Headers.empty
      |> Headers.h2_request ~meth:"GET" ~scheme:"https"
           ~authority:"example.com" ~path:"/"
      |> Headers.set `Accept "application/json"
    ]} *)

(** {2 HTTP/2 Header Validation}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-8.2}RFC 9113 Section 8.2}. *)

type h2_validation_error =
  | Missing_pseudo of string
      (** Required pseudo-header is missing *)
  | Invalid_pseudo of string
      (** Unknown or misplaced pseudo-header *)
  | Pseudo_after_regular
      (** Pseudo-header appeared after regular header *)
  | Invalid_header_name of string
      (** Header name contains invalid characters *)
  | Uppercase_header_name of string
      (** Header name contains uppercase (forbidden in HTTP/2) *)
  | Connection_header_forbidden
      (** Connection-specific headers are forbidden in HTTP/2 *)
  | Te_header_invalid
      (** TE header with value other than "trailers" *)

val pp_h2_validation_error : Format.formatter -> h2_validation_error -> unit
(** Pretty printer for validation errors. *)

val validate_h2_request : t -> (unit, h2_validation_error) result
(** [validate_h2_request headers] validates headers for HTTP/2 request constraints.

    Per RFC 9113 Section 8.3.1, validates:
    - Required pseudo-headers are present ([:method], [:scheme], [:path])
    - No unknown pseudo-headers
    - Pseudo-headers appear before regular headers
    - No uppercase letters in header names
    - No connection-specific headers (Connection, Keep-Alive, etc.)
    - TE header only contains "trailers" if present *)

val validate_h2_response : t -> (unit, h2_validation_error) result
(** [validate_h2_response headers] validates headers for HTTP/2 response constraints.

    Per RFC 9113 Section 8.3.2, validates:
    - [:status] pseudo-header is present
    - No other pseudo-headers
    - Pseudo-headers appear before regular headers
    - No uppercase letters in header names
    - No connection-specific headers *)

val validate_h2_user_headers : t -> (unit, h2_validation_error) result
(** [validate_h2_user_headers headers] validates user-provided headers for HTTP/2.

    Unlike {!validate_h2_request}, this validates headers {i before} pseudo-headers
    are added by the HTTP/2 layer. Use this in the HTTP adapter.

    Per RFC 9113 Section 8.2.2 and 8.3, validates:
    - No pseudo-headers (user should not provide them)
    - No uppercase letters in header names
    - No connection-specific headers (Connection, Keep-Alive, etc.)
    - TE header only contains "trailers" if present *)

(** {2 HTTP/2 Forbidden Headers}

    Per RFC 9113 Section 8.2.2, certain headers are connection-specific
    and MUST NOT appear in HTTP/2. *)

val h2_forbidden_headers : Header_name.t list
(** Headers that MUST NOT appear in HTTP/2 messages:
    - Connection
    - Keep-Alive
    - Proxy-Connection
    - Transfer-Encoding
    - Upgrade *)

val remove_h2_forbidden : t -> t
(** [remove_h2_forbidden headers] removes all HTTP/2 forbidden headers.
    Use this when converting HTTP/1.1 headers for use with HTTP/2. *)
