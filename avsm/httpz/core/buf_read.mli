(** Buffer reading primitives and HTTP parse status.

    This module provides the core types and utilities used during HTTP parsing.
    Most users will interact with these types through {!Httpz.parse}.

    {2 Parse Status}

    The {!status} type represents all possible outcomes of parsing an HTTP
    request. Success is indicated by {!Complete}; all other values indicate
    either incomplete data or an error condition.

    {2 Security Limits}

    The {!limits} type allows configuring security-related bounds on parsed
    requests. Use {!default_limits} for sensible defaults. *)

(** {1 Parse Status} *)

type status =
  | Complete
      (** Request fully parsed. *)
  | Partial
      (** Need more data - request is incomplete. *)
  | Invalid_method
      (** Unknown HTTP method. *)
  | Invalid_target
      (** Malformed request target. *)
  | Invalid_version
      (** Unrecognized HTTP version. *)
  | Invalid_header
      (** Malformed header line. *)
  | Headers_too_large
      (** Headers exceed size limit. *)
  | Malformed
      (** General parse error. *)
  | Content_length_overflow
      (** Content-Length exceeds configured maximum. *)
  | Ambiguous_framing
      (** Both Content-Length and Transfer-Encoding present.
          This is a security violation per
          {{:https://datatracker.ietf.org/doc/html/rfc7230#section-3.3.3}RFC 7230 Section 3.3.3}. *)
  | Bare_cr_detected
      (** CR without LF detected in headers.
          This is a security violation per
          {{:https://datatracker.ietf.org/doc/html/rfc7230#section-3.5}RFC 7230 Section 3.5}
          that can enable HTTP request smuggling. *)
  | Missing_host_header
      (** HTTP/1.1 request without Host header.
          Required by {{:https://datatracker.ietf.org/doc/html/rfc7230#section-5.4}RFC 7230 Section 5.4}. *)
  | Unsupported_transfer_encoding
      (** Transfer-Encoding other than "chunked" or "identity".
          Per {{:https://datatracker.ietf.org/doc/html/rfc7230#section-3.3.1}RFC 7230 Section 3.3.1}. *)
(** HTTP request parse status. *)

val status_to_string : status -> string
(** [status_to_string status] returns a human-readable description. *)

val pp_status : Stdlib.Format.formatter -> status -> unit
(** Pretty-print status. *)

(** {1 Security Limits} *)

type limits =
  #{ max_content_length : int64#
       (** Maximum allowed Content-Length value.
           Requests with larger Content-Length return {!Content_length_overflow}. *)
   ; max_header_size : int16#
       (** Maximum total size of all headers combined (in bytes). *)
   ; max_header_count : int16#
       (** Maximum number of headers allowed. *)
   ; max_chunk_size : int
       (** Maximum size of a single chunk in chunked encoding. *)
   }
(** Configurable security limits for HTTP parsing.

    {[
      let strict_limits = #{
        max_content_length = #1_000_000L;  (* 1MB *)
        max_header_size = Buf_read.i16 8192;
        max_header_count = Buf_read.i16 50;
        max_chunk_size = 1_000_000;
      }
    ]} *)

val default_limits : limits
(** Default security limits:
    - [max_content_length]: 100MB ([#104857600L])
    - [max_header_size]: 16KB
    - [max_header_count]: 100
    - [max_chunk_size]: 16MB *)

(** {1 Buffer Constants} *)

val buffer_size : int
(** Required buffer size: 32KB (32768 bytes).

    All parsing buffers must be at least this size. *)

val max_headers : int16#
(** Maximum headers per request: 100.

    The internal header list can hold this many headers. *)

(** {1 Low-Level Utilities}

    These functions are used internally by the parser. Most users
    won't need them directly. *)

val find_crlf_check_bare_cr : local_ bytes -> pos:int16# -> len:int16# -> #(int16# * bool)
(** [find_crlf_check_bare_cr buf ~pos ~len] finds CRLF and checks for bare CR.

    Returns [#(crlf_pos, has_bare_cr)] where:
    - [crlf_pos]: Position of CRLF, or [-1] if not found
    - [has_bare_cr]: [true] if a bare CR (CR not followed by LF) was detected

    Bare CR detection is required by RFC 7230 Section 3.5 to prevent
    HTTP request smuggling attacks. *)

val i16 : int -> int16#
(** [i16 n] converts [int] to [int16#]. *)

val to_int : int16# -> int
(** [to_int n] converts [int16#] to [int]. *)

val peek : local_ bytes -> int16# -> char#
(** [peek buf pos] returns the character at [pos] without bounds checking. *)

val ( =. ) : char# -> char# -> bool
(** Unboxed character equality. *)

val ( <>. ) : char# -> char# -> bool
(** Unboxed character inequality. *)

val is_token_char : char# -> bool
(** [is_token_char c] returns [true] if [c] is valid in an HTTP token.

    Tokens are used for method names, header names, etc. per RFC 7230. *)

val is_space : char# -> bool
(** [is_space c] returns [true] if [c] is SP (space) or HTAB (tab). *)

val to_lower : char# -> char#
(** [to_lower c] converts ASCII uppercase to lowercase. *)

val pp : Stdlib.Format.formatter -> bytes -> unit
(** Pretty-print buffer contents. *)
