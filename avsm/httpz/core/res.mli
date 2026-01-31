(** HTTP response status codes and header writing.

    This module provides:
    - Status code enumeration per {{:https://datatracker.ietf.org/doc/html/rfc7231#section-6}RFC 7231 Section 6}
    - Low-level response header writing functions
    - Chunked transfer encoding support

    All write functions operate on bytes buffers using [int16#] offsets
    and return the new offset after writing.

    {2 Writing a Response}

    {[
      let buf = Bytes.create 4096 in
      let off = Res.write_status_line buf ~off:(i16 0) Success Http_1_1 in
      let off = Res.write_header_name buf ~off Content_type "text/html" in
      let off = Res.write_content_length buf ~off (String.length body) in
      let off = Res.write_connection buf ~off ~keep_alive:true in
      let off = Res.write_crlf buf ~off in
      (* Write buf[0..off] to socket, then write body *)
    ]} *)

(** {1 Status Codes} *)

type status =
  (* 1xx Informational *)
  | Continue              (** 100 - proceed with request body *)
  | Switching_protocols   (** 101 - protocol upgrade *)
  (* 2xx Success *)
  | Success               (** 200 OK *)
  | Created               (** 201 - resource created *)
  | Accepted              (** 202 - accepted for processing *)
  | No_content            (** 204 - no body *)
  | Partial_content       (** 206 - range request *)
  (* 3xx Redirection *)
  | Moved_permanently     (** 301 - permanent redirect *)
  | Found                 (** 302 - temporary redirect *)
  | See_other             (** 303 - redirect to GET *)
  | Not_modified          (** 304 - conditional GET *)
  | Temporary_redirect    (** 307 - temporary, preserve method *)
  | Permanent_redirect    (** 308 - permanent, preserve method *)
  (* 4xx Client Error *)
  | Bad_request           (** 400 - malformed request *)
  | Unauthorized          (** 401 - auth required *)
  | Forbidden             (** 403 - access denied *)
  | Not_found             (** 404 - resource not found *)
  | Method_not_allowed    (** 405 - method not supported *)
  | Not_acceptable        (** 406 - cannot satisfy Accept *)
  | Request_timeout       (** 408 - client too slow *)
  | Conflict              (** 409 - resource conflict *)
  | Gone                  (** 410 - permanently removed *)
  | Length_required       (** 411 - Content-Length required *)
  | Precondition_failed   (** 412 - If-* condition failed *)
  | Payload_too_large     (** 413 - body too large *)
  | Uri_too_long          (** 414 - URI too long *)
  | Unsupported_media_type (** 415 - Content-Type not accepted *)
  | Range_not_satisfiable (** 416 - invalid Range *)
  | Expectation_failed    (** 417 - Expect header failed *)
  | Unprocessable_entity  (** 422 - semantic error *)
  | Upgrade_required      (** 426 - must upgrade protocol *)
  | Precondition_required (** 428 - If-* required *)
  | Too_many_requests     (** 429 - rate limited *)
  (* 5xx Server Error *)
  | Internal_server_error (** 500 - server error *)
  | Not_implemented       (** 501 - feature not implemented *)
  | Bad_gateway           (** 502 - upstream error *)
  | Service_unavailable   (** 503 - temporarily unavailable *)
  | Gateway_timeout       (** 504 - upstream timeout *)
  | Http_version_not_supported (** 505 - HTTP version not supported *)
(** HTTP response status codes. *)

(** {2 Status Utilities} *)

val status_code : status -> int
(** [status_code status] returns the numeric code (e.g., 200, 404). *)

val status_of_int : int -> status option
(** [status_of_int code] parses a status code. Returns [None] for unknown codes. *)

val status_reason : status -> string
(** [status_reason status] returns the reason phrase (e.g., "OK", "Not Found"). *)

val status_to_string : status -> string
(** [status_to_string status] returns "CODE Reason" (e.g., "200 OK"). *)

val pp_status : Stdlib.Format.formatter -> status -> unit
(** Pretty-print status. *)

(** {1 Response Line Writing} *)

val write_status_line : bytes -> off:int16# -> status -> Version.t -> int16#
(** [write_status_line buf ~off status version] writes the status line.

    Writes: [HTTP/1.x CODE Reason\r\n]

    {[
      let off = Res.write_status_line buf ~off:(i16 0) Success Http_1_1 in
      (* buf now contains "HTTP/1.1 200 OK\r\n" *)
    ]} *)

(** {1 Header Writing} *)

val write_header : bytes -> off:int16# -> local_ string -> local_ string -> int16#
(** [write_header buf ~off name value] writes a header line.

    Writes: [Name: Value\r\n] *)

val write_header_int : bytes -> off:int16# -> local_ string -> int -> int16#
(** [write_header_int buf ~off name value] writes a header with integer value.

    Writes: [Name: 123\r\n] *)

val write_header_name : bytes -> off:int16# -> Header_name.t -> local_ string -> int16#
(** [write_header_name buf ~off name value] writes a header using typed name.

    Uses the canonical header name spelling:
    {[
      let off = Res.write_header_name buf ~off Content_type "text/html" in
      (* buf contains "Content-Type: text/html\r\n" *)
    ]} *)

val write_header_name_int : bytes -> off:int16# -> Header_name.t -> int -> int16#
(** [write_header_name_int buf ~off name value] writes header with typed name
    and integer value. *)

val write_crlf : bytes -> off:int16# -> int16#
(** [write_crlf buf ~off] writes the empty line ending headers.

    Writes: [\r\n] *)

(** {2 Common Headers} *)

val write_content_length : bytes -> off:int16# -> int -> int16#
(** [write_content_length buf ~off len] writes Content-Length header.

    Writes: [Content-Length: len\r\n] *)

val write_connection : bytes -> off:int16# -> keep_alive:bool -> int16#
(** [write_connection buf ~off ~keep_alive] writes Connection header.

    Writes: [Connection: keep-alive\r\n] or [Connection: close\r\n] *)

(** {1 Chunked Transfer Encoding}

    Functions for writing chunked transfer encoded responses per
    {{:https://datatracker.ietf.org/doc/html/rfc7230#section-4.1}RFC 7230 Section 4.1}.

    Use when the response body length is not known in advance. *)

val write_transfer_encoding_chunked : bytes -> off:int16# -> int16#
(** [write_transfer_encoding_chunked buf ~off] writes the TE header.

    Writes: [Transfer-Encoding: chunked\r\n] *)

val write_chunk_header : bytes -> off:int16# -> size:int -> int16#
(** [write_chunk_header buf ~off ~size] writes a chunk header.

    Writes: [<hex-size>\r\n]

    Call this before writing chunk data bytes. *)

val write_chunk_footer : bytes -> off:int16# -> int16#
(** [write_chunk_footer buf ~off] writes the chunk terminator.

    Writes: [\r\n]

    Call this after writing chunk data bytes. *)

val write_final_chunk : bytes -> off:int16# -> int16#
(** [write_final_chunk buf ~off] writes the final (zero-length) chunk.

    Writes: [0\r\n\r\n]

    Call this to signal end of chunked body.

    {[
      (* Complete chunked response *)
      let off = Res.write_status_line buf ~off:(i16 0) Success Http_1_1 in
      let off = Res.write_transfer_encoding_chunked buf ~off in
      let off = Res.write_crlf buf ~off in
      (* ... write chunks ... *)
      let off = Res.write_final_chunk buf ~off in
    ]} *)
