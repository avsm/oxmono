(** Buffer type and utilities for HTTP parsing. *)

(** HTTP parsing result status. *)
type status =
  | Complete
  | Partial
  | Invalid_method
  | Invalid_target
  | Invalid_version
  | Invalid_header
  | Headers_too_large
  | Malformed
  | Content_length_overflow    (** Content-Length value too large or invalid *)
  | Ambiguous_framing          (** Both Content-Length and Transfer-Encoding present *)
  | Bare_cr_detected           (** CR without LF - HTTP smuggling attempt *)
  | Missing_host_header        (** HTTP/1.1 requires Host header *)
  | Unsupported_transfer_encoding (** Transfer-Encoding other than chunked/identity per
                                      {{:https://datatracker.ietf.org/doc/html/rfc7230#section-3.3.1}RFC 7230 Section 3.3.1} *)

(** Convert status to string representation. *)
val status_to_string : status -> string

(** Pretty-print status. *)
val pp_status : Stdlib.Format.formatter -> status -> unit

(** Required buffer size: 32KB. Callers must allocate buffers of this size. *)
val buffer_size : int

(** Maximum headers per request. *)
val max_headers : int16#

(** Convert int to int16#. *)
val i16 : int -> int16#

(** Convert int16# to int. *)
val to_int : int16# -> int

(** Get character at int16# position (unchecked). *)
val peek : local_ bytes -> int16# -> char#

(** Unboxed char equality. *)
val ( =. ) : char# -> char# -> bool

(** Unboxed char inequality. *)
val ( <>. ) : char# -> char# -> bool

(** Check if character is valid HTTP token character. *)
val is_token_char : char# -> bool

(** Check if character is whitespace (space or tab). *)
val is_space : char# -> bool

(** Convert character to lowercase. *)
val to_lower : char# -> char#

(** Find CRLF sequence starting at [pos]. Returns position of CR or [-1] as int16# if not found. *)
val find_crlf : local_ bytes -> pos:int16# -> len:int16# -> int16#

(** Pretty-print buffer. *)
val pp : Stdlib.Format.formatter -> bytes -> unit

(** {2 Security Limits} *)

(** Configurable security limits for parsing. *)
type limits =
  #{ max_content_length : int64#  (** Maximum Content-Length value (default: 100MB) *)
   ; max_header_size : int16#     (** Maximum size of all headers combined (default: 16KB) *)
   ; max_header_count : int16#    (** Maximum number of headers (default: 100) *)
   ; max_chunk_size : int         (** Maximum chunk size for chunked encoding (default: 16MB) *)
   }

(** Default limits: 100MB content, 16KB headers, 100 header count, 16MB chunks. *)
val default_limits : limits

(** Detect bare CR (CR not followed by LF) - RFC 7230 Section 3.5.
    Used to prevent HTTP request smuggling attacks. *)
val has_bare_cr : local_ bytes -> pos:int16# -> len:int16# -> bool

(** Check if a value contains CRLF injection attempt. *)
val has_crlf_injection : local_ bytes -> pos:int16# -> len:int16# -> bool
