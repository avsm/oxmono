(** HTTP response writing utilities.

    All write functions operate on bigstrings (Base_bigstring.t) for zero-copy
    I/O with Async's Writer.write_bigstring. Uses int16# offsets throughout. *)

(** HTTP response status codes per
    {{:https://datatracker.ietf.org/doc/html/rfc7231#section-6}RFC 7231 Section 6}. *)
type status =
  (* 1xx Informational *)
  | Continue                (** 100 - for Expect: 100-continue *)
  | Switching_protocols     (** 101 - for Upgrade *)
  (* 2xx Success *)
  | Success                 (** 200 OK *)
  | Created                 (** 201 *)
  | Accepted                (** 202 *)
  | No_content              (** 204 *)
  | Partial_content         (** 206 - for Range requests *)
  (* 3xx Redirection *)
  | Moved_permanently       (** 301 *)
  | Found                   (** 302 *)
  | See_other               (** 303 *)
  | Not_modified            (** 304 *)
  | Temporary_redirect      (** 307 *)
  | Permanent_redirect      (** 308 *)
  (* 4xx Client Error *)
  | Bad_request             (** 400 *)
  | Unauthorized            (** 401 *)
  | Forbidden               (** 403 *)
  | Not_found               (** 404 *)
  | Method_not_allowed      (** 405 *)
  | Not_acceptable          (** 406 *)
  | Request_timeout         (** 408 *)
  | Conflict                (** 409 *)
  | Gone                    (** 410 *)
  | Length_required         (** 411 *)
  | Precondition_failed     (** 412 *)
  | Payload_too_large       (** 413 *)
  | Uri_too_long            (** 414 *)
  | Unsupported_media_type  (** 415 *)
  | Range_not_satisfiable   (** 416 *)
  | Expectation_failed      (** 417 *)
  | Unprocessable_entity    (** 422 *)
  | Upgrade_required        (** 426 *)
  | Precondition_required   (** 428 *)
  | Too_many_requests       (** 429 *)
  (* 5xx Server Error *)
  | Internal_server_error   (** 500 *)
  | Not_implemented         (** 501 *)
  | Bad_gateway             (** 502 *)
  | Service_unavailable     (** 503 *)
  | Gateway_timeout         (** 504 *)
  | Http_version_not_supported (** 505 *)

(** Get numeric status code. *)
val status_code : status -> int

(** Parse status from numeric code. Returns [None] for unknown codes. *)
val status_of_int : int -> status option

(** Get reason phrase. *)
val status_reason : status -> string

(** Get "CODE Reason" string. *)
val status_to_string : status -> string

(** Pretty-print status. *)
val pp_status : Stdlib.Format.formatter -> status -> unit

(** {2 Response Writers} *)

(** Write "HTTP/1.x CODE Reason\r\n" at offset. Returns new offset. *)
val write_status_line : Base_bigstring.t -> off:int16# -> status -> Version.t -> int16#

(** Write "Name: Value\r\n" at offset using string name. Returns new offset. *)
val write_header : Base_bigstring.t -> off:int16# -> string -> string -> int16#

(** Write header with integer value using string name. Returns new offset. *)
val write_header_int : Base_bigstring.t -> off:int16# -> string -> int -> int16#

(** Write "Name: Value\r\n" using typed header name. Returns new offset. *)
val write_header_name : Base_bigstring.t -> off:int16# -> Header_name.t -> string -> int16#

(** Write header with integer value using typed header name. Returns new offset. *)
val write_header_name_int : Base_bigstring.t -> off:int16# -> Header_name.t -> int -> int16#

(** Write "\r\n". Returns new offset. *)
val write_crlf : Base_bigstring.t -> off:int16# -> int16#

(** Write "Content-Length: N\r\n". Returns new offset. *)
val write_content_length : Base_bigstring.t -> off:int16# -> int -> int16#

(** Write "Connection: keep-alive\r\n" or "Connection: close\r\n".
    Returns new offset. *)
val write_connection : Base_bigstring.t -> off:int16# -> keep_alive:bool -> int16#

(** {2 Chunked Transfer Encoding}

    Functions for writing chunked transfer encoded responses per
    {{:https://datatracker.ietf.org/doc/html/rfc7230#section-4.1}RFC 7230 Section 4.1}.
    Use when response body length is unknown at the time headers are sent. *)

(** Write "Transfer-Encoding: chunked\r\n". Returns new offset. *)
val write_transfer_encoding_chunked : Base_bigstring.t -> off:int16# -> int16#

(** Write chunk header "<hex-size>\r\n". Returns new offset.
    Call before writing chunk data. *)
val write_chunk_header : Base_bigstring.t -> off:int16# -> size:int -> int16#

(** Write chunk footer "\r\n" after chunk data. Returns new offset. *)
val write_chunk_footer : Base_bigstring.t -> off:int16# -> int16#

(** Write final chunk "0\r\n\r\n". Returns new offset.
    Call after all data chunks to signal end of body. *)
val write_final_chunk : Base_bigstring.t -> off:int16# -> int16#
