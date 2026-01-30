(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP response parsing using Eio.Buf_read combinators

    This module provides efficient HTTP/1.1 response parsing using Eio's
    buffered read API with parser combinators for clean, composable parsing.

    Example:
    {[
      let buf_read = Http_read.of_flow ~max_size:max_int flow in
      let (status, headers, body) = Http_read.response ~limits buf_read
    ]} *)

(** {1 Response Limits}

    This module uses {!Response_limits.t} for size limit configuration. *)

type limits = Response_limits.t
(** Alias for {!Response_limits.t}. See {!Response_limits} for documentation. *)

(** {1 HTTP Version Type}

    Per Recommendation #26: Expose HTTP version used for the response. *)

type http_version =
  | HTTP_1_0  (** HTTP/1.0 *)
  | HTTP_1_1  (** HTTP/1.1 *)
(** HTTP protocol version. Useful for debugging protocol negotiation
    and monitoring HTTP/2 adoption (when supported). *)

val http_version_to_string : http_version -> string
(** [http_version_to_string v] returns "HTTP/1.0" or "HTTP/1.1". *)

(** {1 Low-level Parsers} *)

val http_version : Eio.Buf_read.t -> string
(** [http_version r] parses HTTP version string (e.g., "HTTP/1.1"). *)

val status_code : Eio.Buf_read.t -> int
(** [status_code r] parses a 3-digit HTTP status code.
    @raise Error if the status code is invalid. *)

val status_line : Eio.Buf_read.t -> http_version * int
(** [status_line r] parses a complete HTTP status line and returns
    the HTTP version and status code as a tuple.
    Validates that the HTTP version is 1.0 or 1.1.
    @raise Error if the status line is invalid. *)

(** {1 Header Parsing} *)

val header_line : Eio.Buf_read.t -> string * string
(** [header_line r] parses a single header line.
    Returns [(name, value)] where name is lowercase.
    Returns [("", "")] for the empty line that terminates headers. *)

val headers : limits:limits -> Eio.Buf_read.t -> Headers.t
(** [headers ~limits r] parses all headers until the terminating blank line.
    Enforces header count and size limits.
    @raise Error.Headers_too_large if limits are exceeded. *)

(** {1 Body Parsing} *)

val fixed_body : limits:limits -> length:int64 -> Eio.Buf_read.t -> string
(** [fixed_body ~limits ~length r] reads exactly [length] bytes as the body.
    @raise Error.Body_too_large if length exceeds limit.
    @raise Error.Content_length_mismatch if EOF occurs before all bytes read. *)

val chunked_body : limits:limits -> Eio.Buf_read.t -> string
(** [chunked_body ~limits r] reads a chunked transfer-encoded body.
    Handles chunk sizes, extensions, and trailers.
    @raise Error.Body_too_large if total body size exceeds limit. *)

(** {1 Transfer-Encoding Validation} *)

val parse_transfer_encoding : string option -> string list
(** [parse_transfer_encoding header] parses Transfer-Encoding header value
    into a list of codings (all lowercase, in order). *)

val validate_transfer_encoding : string list ->
  [ `Chunked | `None | `Unsupported of string list ]
(** [validate_transfer_encoding codings] validates Transfer-Encoding per RFC 9112 Section 6.1.
    Returns [`Chunked] if chunked encoding should be used, [`None] if no body,
    or [`Unsupported codings] for unsupported encodings without chunked.
    @raise Error if chunked is not final encoding (RFC violation). *)

val validate_no_transfer_encoding :
  method_:Method.t option -> status:int -> string option -> bool
(** [validate_no_transfer_encoding ~method_ ~status te] validates that
    Transfer-Encoding is not present in responses that MUST NOT have it.
    Per RFC 9112 Section 6.1, these include responses to HEAD, 1xx, 204, and 304.
    If present, this logs a warning about the RFC violation.
    @return true if Transfer-Encoding is present (violation), false otherwise *)

(** {1 Trailer Header Parsing} *)

val forbidden_trailer_headers : string list
(** Headers that MUST NOT appear in trailers per RFC 9110 Section 6.5.1.
    Includes: transfer-encoding, content-length, host, content-encoding,
    content-type, content-range, trailer. *)

val parse_trailers : limits:limits -> Eio.Buf_read.t -> Headers.t
(** [parse_trailers ~limits r] parses trailer headers after final chunk.
    Forbidden headers are logged and ignored. *)

(** {1 Streaming Body Sources} *)

val fixed_body_stream : limits:limits -> length:int64 ->
                        Eio.Buf_read.t -> Eio.Flow.source_ty Eio.Resource.t
(** [fixed_body_stream ~limits ~length r] creates a flow source that reads
    [length] bytes from [r]. Useful for large bodies to avoid loading
    everything into memory at once. *)

val chunked_body_stream : limits:limits ->
                          Eio.Buf_read.t -> Eio.Flow.source_ty Eio.Resource.t
(** [chunked_body_stream ~limits r] creates a flow source that reads
    chunked transfer-encoded data from [r]. Decodes chunks on-the-fly. *)

(** {1 High-level Response Parsing} *)

val response : limits:limits -> ?method_:Method.t -> Eio.Buf_read.t -> http_version * int * Headers.t * string
(** [response ~limits ?method_ r] parses a complete HTTP response including:
    - HTTP version
    - Status code
    - Headers
    - Body (based on Transfer-Encoding or Content-Length)

    Returns [(http_version, status, headers, body)].

    @param method_ The HTTP method of the request. Per
      {{:https://datatracker.ietf.org/doc/html/rfc9110#section-6.4.1}RFC 9110 Section 6.4.1},
      certain responses have no body:
      {ul
      {- [`HEAD] - body is always empty regardless of Content-Length}
      {- [`CONNECT] with 2xx - switches to tunnel mode, no body}
      {- 1xx, 204, 304 - no content responses}}

    This reads the entire body into memory. For large responses,
    use {!response_stream} instead. *)

(** {1 Streaming Response} *)

type stream_response = {
  http_version : http_version;  (** HTTP protocol version *)
  status : int;
  headers : Headers.t;
  body : [ `String of string
         | `Stream of Eio.Flow.source_ty Eio.Resource.t
         | `None ]
}
(** A parsed response with optional streaming body.
    Per Recommendation #26: Includes HTTP version for debugging/monitoring. *)

val response_stream : limits:limits -> ?method_:Method.t -> Eio.Buf_read.t -> stream_response
(** [response_stream ~limits ?method_ r] parses status line and headers, then
    returns a streaming body source instead of reading the body into memory.
    Use this for large responses.

    @param method_ The HTTP method of the request. Used to validate
      that Transfer-Encoding is not present in responses that shouldn't have it
      (HEAD requests). *)

(** {1 Convenience Functions} *)

val of_flow : ?initial_size:int -> max_size:int -> _ Eio.Flow.source -> Eio.Buf_read.t
(** [of_flow ~max_size flow] creates a buffered reader from [flow].
    This is a thin wrapper around {!Eio.Buf_read.of_flow}. *)
