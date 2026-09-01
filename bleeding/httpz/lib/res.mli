(** This module provides HTTP response-head parsing and HTTP head writing.

    The response parser is the client-side counterpart of {!Httpz.parse}. Writers in this
    module are direction-neutral after the start line, so a client can follow
    {!Req.write_request_line} with {!write_header} and a server can follow
    {!write_status_line} with the same field writers.

    All writers are unchecked: the caller must reserve sufficient space and must prevent
    CR or LF in field names and values supplied as strings. Offsets are signed 16-bit and
    must stay below 32768. An offset past that limit wraps negative and corrupts memory
    outside the buffer, whatever the buffer size, so the caller must bound the head it
    emits. *)

(** A [status] is a response status recognized by Httpz. Status semantics are defined by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#name-status-codes} RFC 9110, Section
      15};
    WebDAV additions come from RFC 4918. *)
type status =
  | Continue (** [Continue] means 100 Continue. *)
  | Switching_protocols (** [Switching_protocols] means 101 Switching Protocols. *)
  | Success (** [Success] means 200 OK. *)
  | Created (** [Created] means 201 Created. *)
  | Accepted (** [Accepted] means 202 Accepted. *)
  | No_content (** [No_content] means 204 No Content. *)
  | Reset_content (** [Reset_content] means 205 Reset Content. *)
  | Partial_content (** [Partial_content] means 206 Partial Content. *)
  | Multi_status (** [Multi_status] means 207 Multi-Status. *)
  | Moved_permanently (** [Moved_permanently] means 301 Moved Permanently. *)
  | Found (** [Found] means 302 Found. *)
  | See_other (** [See_other] means 303 See Other. *)
  | Not_modified (** [Not_modified] means 304 Not Modified. *)
  | Temporary_redirect (** [Temporary_redirect] means 307 Temporary Redirect. *)
  | Permanent_redirect (** [Permanent_redirect] means 308 Permanent Redirect. *)
  | Bad_request (** [Bad_request] means 400 Bad Request. *)
  | Unauthorized (** [Unauthorized] means 401 Unauthorized. *)
  | Forbidden (** [Forbidden] means 403 Forbidden. *)
  | Not_found (** [Not_found] means 404 Not Found. *)
  | Method_not_allowed (** [Method_not_allowed] means 405 Method Not Allowed. *)
  | Not_acceptable (** [Not_acceptable] means 406 Not Acceptable. *)
  | Proxy_authentication_required
  (** [Proxy_authentication_required] means 407 Proxy Authentication Required. *)
  | Request_timeout (** [Request_timeout] means 408 Request Timeout. *)
  | Conflict (** [Conflict] means 409 Conflict. *)
  | Gone (** [Gone] means 410 Gone. *)
  | Length_required (** [Length_required] means 411 Length Required. *)
  | Precondition_failed (** [Precondition_failed] means 412 Precondition Failed. *)
  | Payload_too_large (** [Payload_too_large] means 413 Payload Too Large. *)
  | Uri_too_long (** [Uri_too_long] means 414 URI Too Long. *)
  | Unsupported_media_type
  (** [Unsupported_media_type] means 415 Unsupported Media Type. *)
  | Range_not_satisfiable (** [Range_not_satisfiable] means 416 Range Not Satisfiable. *)
  | Expectation_failed (** [Expectation_failed] means 417 Expectation Failed. *)
  | Unprocessable_entity (** [Unprocessable_entity] means 422 Unprocessable Entity. *)
  | Locked (** [Locked] means 423 Locked. *)
  | Failed_dependency (** [Failed_dependency] means 424 Failed Dependency. *)
  | Upgrade_required (** [Upgrade_required] means 426 Upgrade Required. *)
  | Precondition_required (** [Precondition_required] means 428 Precondition Required. *)
  | Too_many_requests (** [Too_many_requests] means 429 Too Many Requests. *)
  | Request_header_fields_too_large
  (** [Request_header_fields_too_large] means 431 Request Header Fields Too Large. *)
  | Internal_server_error (** [Internal_server_error] means 500 Internal Server Error. *)
  | Not_implemented (** [Not_implemented] means 501 Not Implemented. *)
  | Bad_gateway (** [Bad_gateway] means 502 Bad Gateway. *)
  | Service_unavailable (** [Service_unavailable] means 503 Service Unavailable. *)
  | Gateway_timeout (** [Gateway_timeout] means 504 Gateway Timeout. *)
  | Http_version_not_supported
  (** [Http_version_not_supported] means 505 HTTP Version Not Supported. *)
  | Insufficient_storage (** [Insufficient_storage] means 507 Insufficient Storage. *)

(** [status_code status] is [status]'s three-digit integer code. *)
val status_code : status -> int @@ portable

(** [status_of_int code] is the status represented by [code], or [None] when Httpz does
    not enumerate it. The response parser itself accepts any three-digit code. *)
val status_of_int : int -> status option @@ portable

(** [status_reason status] is the conventional English reason phrase for [status]. *)
val status_reason : status -> string @@ portable

(** [status_to_string status] is ["CODE Reason"]. The returned string is shared and must
    not be mutated. *)
val status_to_string : status -> string @@ portable
[@@zero_alloc]

(** [pp_status formatter status] is the formatter operation that prints
    {!status_to_string}. *)
val pp_status : Stdlib.Format.formatter -> status -> unit @@ portable

(** [write_status_line buf ~off status version] is the next offset after writing
    [version SP code SP reason CRLF]. *)
val write_status_line : bytes -> off:int16# -> status -> Version.t -> int16# @@ portable

(** [write_header buf ~off name value] is the next offset after writing
    [name ": " value CRLF]. [name] and [value] are not validated. *)
val write_header
  :  bytes
  -> off:int16#
  -> local_ string
  -> local_ string
  -> int16#
  @@ portable

(** [write_header_int buf ~off name value] is the next offset after writing a field with a
    non-negative decimal [value]. *)
val write_header_int : bytes -> off:int16# -> local_ string -> int -> int16# @@ portable

(** [write_header_name buf ~off name value] is the next offset after writing a field using
    {!Header_name.canonical}. {!Header_name.Other} writes ["(unknown)"] and is therefore
    unsuitable here. *)
val write_header_name
  :  bytes
  -> off:int16#
  -> Header_name.t
  -> local_ string
  -> int16#
  @@ portable

(** [write_header_name_int buf ~off name value] is the next offset after writing a typed
    field with a non-negative decimal [value]. *)
val write_header_name_int
  :  bytes
  -> off:int16#
  -> Header_name.t
  -> int
  -> int16#
  @@ portable

(** [write_crlf buf ~off] is [off + 2] after writing the empty line that ends a message
    head. *)
val write_crlf : bytes -> off:int16# -> int16# @@ portable

(** [write_content_length buf ~off length] is the next offset after writing a
    Content-Length field. [length] must be non-negative. *)
val write_content_length : bytes -> off:int16# -> int -> int16# @@ portable

(** [write_connection buf ~off ~keep_alive] is the next offset after writing
    ["Connection: keep-alive\r\n"] or ["Connection: close\r\n"]. *)
val write_connection : bytes -> off:int16# -> keep_alive:bool -> int16# @@ portable

(** [write_transfer_encoding_chunked buf ~off] is the next offset after writing
    ["Transfer-Encoding: chunked\r\n"]. Chunked coding is defined by
    {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-7.1} RFC 9112, Section 7.1}. *)
val write_transfer_encoding_chunked : bytes -> off:int16# -> int16# @@ portable

(** [write_chunk_header buf ~off ~size] is the next offset after writing the non-negative
    [size] in hexadecimal followed by CRLF. *)
val write_chunk_header : bytes -> off:int16# -> size:int -> int16# @@ portable

(** [write_chunk_footer buf ~off] is [off + 2] after writing the CRLF that follows chunk
    data. *)
val write_chunk_footer : bytes -> off:int16# -> int16# @@ portable

(** [write_final_chunk buf ~off] is the next offset after writing ["0\r\n\r\n"]. Use the
    lower-level writers when trailer fields follow the final chunk. *)
val write_final_chunk : bytes -> off:int16# -> int16# @@ portable

(** A [t] is a parsed response head whose spans borrow from the parse buffer. *)
type t =
  #{ version : Version.t (** [version] is the response's HTTP version. *)
   ; code : int16#
   (** [code] is the three-digit status code, including unregistered codes. *)
   ; reason : Span.t (** [reason] is the optional, informational reason phrase. *)
   ; body_off : int16# (** [body_off] is the first byte after the response head. *)
   ; content_length : int64#
   (** [content_length] is Content-Length, or [-1L] when absent. *)
   ; is_chunked : bool
   (** [is_chunked] is [true] when chunked is the final transfer coding. *)
   ; bodyless : bool
   (** [bodyless] is [true] when HTTP/1 framing ends at the response head. A
       205 is false here: RFC 9110 forbids its content, but RFC 9112 still
       frames any bytes after the head. Higher-level clients can suppress that
       semantically absent content after consuming or closing it. *)
   ; keep_alive : bool
   (** [keep_alive] is [true] when the connection can persist after this response. *)
   }

(** [parse ?request_method buf ~len ~limits] is the result of parsing one response head
    from the first [len] bytes of [buf]. It contains the status, response metadata, and
    all fields in reverse arrival order. Unlike {!Httpz.parse}, framing fields remain in
    the returned list.

    [request_method] is required to recognize responses to HEAD and successful CONNECT as
    framing-bodyless. Status-based framing rules are always applied. Body bytes already
    present after the head do not count toward [limits.max_header_size].

    {!Buf_read.Partial} means that more bytes are needed. For any other status than
    {!Buf_read.Complete}, the response and field list are placeholders. Message framing
    follows
    {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-6.3} RFC 9112, Section 6.3}. *)
val parse
  :  ?request_method:Method.t
  -> bytes
  -> len:int16#
  -> limits:Buf_read.limits
  -> #(Buf_read.status * t * Header.t list) @ local
  @@ portable

(** [pp formatter response] is the formatter operation that prints response metadata and
    span positions. *)
val pp : Stdlib.Format.formatter -> t -> unit @@ portable

(** [pp_with_buf buf formatter response] is the formatter operation that prints the parsed
    status line. *)
val pp_with_buf : bytes -> Stdlib.Format.formatter -> t -> unit @@ portable
