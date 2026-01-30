(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP response handling per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-15}RFC 9110}

    This module represents HTTP responses and provides functions to access
    status codes, headers, and response bodies. Responses support streaming
    to efficiently handle large payloads.

    Caching semantics follow {{:https://datatracker.ietf.org/doc/html/rfc9111}RFC 9111} (HTTP Caching).

    {2 Examples}

    {[
      (* Check response status *)
      if Response.ok response then
        Printf.printf "Success!\n"
      else
        Printf.printf "Error: %d\n" (Response.status_code response);

      (* Access headers *)
      match Response.content_type response with
      | Some mime -> Printf.printf "Type: %s\n" (Mime.to_string mime)
      | None -> ()

      (* Stream response body *)
      let body = Response.body response in
      Eio.Flow.copy body (Eio.Flow.buffer_sink buffer)

      (* Response automatically closes when the switch is released *)
    ]}

    {b Note}: Responses are automatically closed when the switch they were
    created with is released. Manual cleanup is not necessary.
*)

open Eio

(** Log source for response operations *)
val src : Logs.Src.t

type t
(** Abstract response type representing an HTTP response. *)

val make : sw:Eio.Switch.t -> status:int -> headers:Headers.t ->
  body:Eio.Flow.source_ty Eio.Resource.t -> url:string -> elapsed:float -> t
(** [make ~sw ~status ~headers ~body ~url ~elapsed] creates a response.
    Internal function primarily used for caching. *)

(** {1 Status Information} *)

val status : t -> Status.t
(** [status response] returns the HTTP status as a {!Status.t} value. *)

val status_code : t -> int
(** [status_code response] returns the HTTP status code as an integer (e.g., 200, 404). *)

val ok : t -> bool
(** [ok response] returns [true] if the status code is in the 2xx success range.
    This is an alias for {!Status.is_success}. *)

(** {1 Header Access} *)

val headers : t -> Headers.t
(** [headers response] returns all response headers. *)

val header : Header_name.t -> t -> string option
(** [header name response] returns the value of a specific header, or [None] if not present.
    Header names are case-insensitive.

    Example: [header `Content_type response] *)

val header_string : string -> t -> string option
(** [header_string name response] returns the value of a header by string name.
    Use this when header names come from external sources (e.g., wire format).
    Header names are case-insensitive. *)

val content_type : t -> Mime.t option
(** [content_type response] returns the parsed Content-Type header as a MIME type,
    or [None] if the header is not present or cannot be parsed. *)

val content_length : t -> int64 option
(** [content_length response] returns the Content-Length in bytes,
    or [None] if not specified or chunked encoding is used. *)

val location : t -> string option
(** [location response] returns the Location header value, typically used in redirects.
    Returns [None] if the header is not present. *)

(** {1 Conditional Request / Caching Headers}

    Per Recommendation #19: Conditional Request Helpers (ETag/Last-Modified)
    RFC 9110 Section 8.8.2-8.8.3 *)

val etag : t -> string option
(** [etag response] returns the ETag header value, which is an opaque
    identifier for a specific version of a resource.
    Use with {!Headers.if_none_match} for conditional requests.
    Example: ["\"abc123\""] or [W/"abc123"] (weak validator) *)

val last_modified : t -> string option
(** [last_modified response] returns the Last-Modified header as a raw string.
    Format: HTTP-date (e.g., ["Sun, 06 Nov 1994 08:49:37 GMT"]) *)

val parse_http_date : string -> Ptime.t option
(** [parse_http_date s] parses an HTTP-date string (RFC 9110 Section 5.6.7) to Ptime.t.
    Supports RFC 1123, RFC 850, and ANSI C asctime() formats.
    Returns [None] if parsing fails.

    This is exposed for use by other modules that need to parse HTTP dates. *)

val last_modified_ptime : t -> Ptime.t option
(** [last_modified_ptime response] parses the Last-Modified header as a Ptime.t.
    Returns [None] if the header is not present or cannot be parsed. *)

val date : t -> string option
(** [date response] returns the Date header (time response was generated). *)

val date_ptime : t -> Ptime.t option
(** [date_ptime response] parses the Date header as a Ptime.t. *)

val expires : t -> string option
(** [expires response] returns the Expires header (HTTP/1.0 cache control).
    Prefer using {!cache_control} for RFC 9111 compliant caching. *)

val expires_ptime : t -> Ptime.t option
(** [expires_ptime response] parses the Expires header as a Ptime.t. *)

val age : t -> int option
(** [age response] returns the Age header value in seconds.
    The Age header indicates how long the response has been in a cache. *)

(** {1 Cache-Control Parsing}

    Per Recommendation #17: Response Caching with RFC 7234/9111 Compliance *)

val cache_control : t -> Cache_control.response option
(** [cache_control response] parses and returns the Cache-Control header directives.
    Returns [None] if the header is not present.

    Example:
    {[
      match Response.cache_control response with
      | Some cc when cc.Cache_control.no_store -> "Do not cache"
      | Some cc -> Printf.sprintf "Max age: %d" (Option.get cc.max_age)
      | None -> "No cache directives"
    ]} *)

val cache_control_raw : t -> string option
(** [cache_control_raw response] returns the raw Cache-Control header string
    without parsing. Useful for debugging or custom parsing. *)

val is_cacheable : t -> bool
(** [is_cacheable response] returns [true] if the response may be cached
    based on its status code and Cache-Control directives.
    A response is cacheable if no-store is not present and either:
    - Status is cacheable by default (200, 203, 204, 206, 300, 301, 308, 404, 405, 410, 414, 501)
    - Explicit caching directive (max-age, s-maxage) is present *)

val freshness_lifetime : t -> int option
(** [freshness_lifetime response] calculates how long the response is fresh
    in seconds, based on Cache-Control max-age or Expires header.
    Returns [None] if freshness cannot be determined. *)

val must_revalidate : t -> bool
(** [must_revalidate response] returns [true] if cached copies must be
    revalidated with the origin server before use (must-revalidate,
    proxy-revalidate, or no-cache directive present). *)

val is_stale : now:Ptime.t -> t -> bool
(** [is_stale ~now response] returns [true] if the response's freshness
    lifetime has expired. Requires the current time as [now].
    Returns [false] if staleness cannot be determined. *)

val is_not_modified : t -> bool
(** [is_not_modified response] returns [true] if this is a 304 Not Modified
    response, indicating the cached version is still valid. *)

val vary : t -> string option
(** [vary response] returns the Vary header, which lists request headers
    that affect the response (for cache key construction). *)

val vary_headers : t -> string list
(** [vary_headers response] parses the Vary header into a list of header names.
    Returns an empty list if Vary is not present. *)

(** {1 Response Metadata} *)

val url : t -> string
(** [url response] returns the final URL after following any redirects.
    This may differ from the originally requested URL. *)

val elapsed : t -> float
(** [elapsed response] returns the time taken for the request in seconds,
    including connection establishment, sending the request, and receiving headers. *)

(** {1 Response Body} *)

val body : t -> Flow.source_ty Resource.t
(** [body response] returns the response body as an Eio flow for streaming.
    This allows efficient processing of large responses without loading them
    entirely into memory.

    Example:
    {[
      let body = Response.body response in
      let buffer = Buffer.create 4096 in
      Eio.Flow.copy body (Eio.Flow.buffer_sink buffer);
      Buffer.contents buffer
    ]}
*)

val text : t -> string
(** [text response] reads and returns the entire response body as a string.
    The response body is fully consumed by this operation.

    @raise Failure if the response has already been closed. *)

val json : t -> Jsont.json
(** [json response] parses the response body as JSON.
    The response body is fully consumed by this operation.

    Example:
    {[
      let json = Response.json response in
      process_json json
    ]}

    @raise Eio.Io with {!Error.Json_parse_error} if JSON parsing fails.
    @raise Failure if the response has already been closed. *)

val jsonv : 'a Jsont.t -> t -> 'a
(** [jsonv codec response] parses the response body as JSON and decodes it
    to a typed value using the provided [codec].
    The response body is fully consumed by this operation.

    This is the preferred way to decode JSON responses into typed OCaml values,
    as it provides type safety and works with custom record types.

    Example:
    {[
      (* Define a codec for your type *)
      type user = { name : string; age : int }

      let user_codec =
        Jsont.Obj.map ~kind:"user" (fun name age -> { name; age })
        |> Jsont.Obj.mem "name" Jsont.string ~enc:(fun u -> u.name)
        |> Jsont.Obj.mem "age" Jsont.int ~enc:(fun u -> u.age)
        |> Jsont.Obj.finish

      (* Decode the response to a typed value *)
      let user = Response.jsonv user_codec response in
      Printf.printf "User: %s, age %d\n" user.name user.age
    ]}

    @raise Eio.Io with {!Error.Json_parse_error} if JSON parsing fails.
    @raise Failure if the response has already been closed. *)

val raise_for_status : t -> t
(** [raise_for_status response] raises [Eio.Io] with [Error.Http_error] if the
    response status code indicates an error (>= 400). Returns the response
    unchanged if the status indicates success (< 400).

    This is useful for failing fast on HTTP errors:
    {[
      let response = Requests.get req url |> Response.raise_for_status in
      (* Only reaches here if status < 400 *)
      process_success response
    ]}

    @raise Eio.Io with [Error.Http_error] if status code >= 400. *)

val check_status : t -> (t, Error.error) result
(** [check_status response] returns [Ok response] if the status code is < 400,
    or [Error error] if the status code indicates an error (>= 400).

    This provides functional error handling without exceptions, complementing
    {!raise_for_status} for different coding styles.

    Example:
    {[
      match Response.check_status response with
      | Ok resp -> process_success resp
      | Error err -> handle_error err
    ]}

    Per Recommendation #21: Provides a Result-based alternative to raise_for_status. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty print a response summary *)

val pp_detailed : Format.formatter -> t -> unit
(** Pretty print a response with full headers *)

(** {1 Private API} *)

(** Internal functions exposed for use by other modules in the library.
    These are not part of the public API and may change between versions. *)
module Private : sig
  val make :
    sw:Eio.Switch.t ->
    status:int ->
    headers:Headers.t ->
    body:Flow.source_ty Resource.t ->
    url:string ->
    elapsed:float ->
    t
  (** [make ~sw ~status ~headers ~body ~url ~elapsed] constructs a response.
      The response will be automatically closed when the switch is released.
      This function is used internally by the Client module. *)
end