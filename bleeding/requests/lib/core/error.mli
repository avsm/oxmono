(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Centralized error handling for the Requests library using Eio.Io exceptions.

    This module follows the Eio.Io exception pattern for structured error handling,
    providing granular error types and query functions for smart retry logic.

    {2 Usage}

    Errors are raised using the Eio.Io pattern:
    {[
      raise (Error.err (Error.Timeout { operation = "connect"; duration = Some 30.0 }))
    ]}

    To catch and handle errors:
    {[
      try
        (* ... HTTP request ... *)
      with
      | Eio.Io (Error.E e, _) when Error.is_retryable e ->
          (* Retry the request *)
      | Eio.Io (Error.E e, _) ->
          Printf.eprintf "Request failed: %s\n" (Error.to_string e)
    ]}
*)

(** Log source for error reporting *)
val src : Logs.Src.t

(** {1 Error Type}

    Granular error variants with contextual information.
    Each variant contains a record with relevant details. *)

type error =
  (* Timeout errors *)
  | Timeout of { operation: string; duration: float option }

  (* Redirect errors *)
  | Too_many_redirects of { url: string; count: int; max: int }
  | Invalid_redirect of { url: string; reason: string }

  (* HTTP response errors *)
  (* Note: headers stored as list to avoid dependency cycle with Headers module *)
  | Http_error of {
      url: string;
      status: int;
      reason: string;
      body_preview: string option;
      headers: (string * string) list
    }

  (* Authentication errors *)
  | Authentication_failed of { url: string; reason: string }

  (* Connection errors - granular breakdown *)
  | Dns_resolution_failed of { hostname: string }
  | Tcp_connect_failed of { host: string; port: int; reason: string }
  | Tls_handshake_failed of { host: string; reason: string }

  (* Security-related errors *)
  | Invalid_header of { name: string; reason: string }
  | Body_too_large of { limit: int64; actual: int64 option }
  | Headers_too_large of { limit: int; actual: int }
  | Decompression_bomb of { limit: int64; ratio: float }
  | Content_length_mismatch of { expected: int64; actual: int64 }
  | Insecure_auth of { url: string; auth_type: string }
      (** Per RFC 7617 Section 4 and RFC 6750 Section 5.1:
          Basic, Bearer, and Digest authentication over unencrypted HTTP
          exposes credentials to eavesdropping. Raised when attempting
          to use these auth methods over HTTP without explicit opt-out. *)

  (* JSON errors *)
  | Json_parse_error of { body_preview: string; reason: string }
  | Json_encode_error of { reason: string }

  (* Other errors *)
  | Proxy_error of { host: string; reason: string }
  | Encoding_error of { encoding: string; reason: string }
  | Invalid_url of { url: string; reason: string }
  | Invalid_request of { reason: string }

  (* OAuth 2.0 errors - per RFC 6749 Section 5.2 *)
  | Oauth_error of { error_code: string; description: string option; uri: string option }
      (** OAuth 2.0 error response from authorization server.
          Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-5.2}RFC 6749 Section 5.2}. *)
  | Token_refresh_failed of { reason: string }
      (** Token refresh operation failed. *)
  | Token_expired
      (** Access token has expired and no refresh token is available. *)

  (* HTTP/2 protocol errors - per RFC 9113 *)
  | H2_protocol_error of { code: int32; message: string }
      (** HTTP/2 connection error per
          {{:https://datatracker.ietf.org/doc/html/rfc9113#section-5.4.1}RFC 9113 Section 5.4.1}.
          Error codes are defined in RFC 9113 Section 7. *)
  | H2_stream_error of { stream_id: int32; code: int32; message: string }
      (** HTTP/2 stream error per
          {{:https://datatracker.ietf.org/doc/html/rfc9113#section-5.4.2}RFC 9113 Section 5.4.2}. *)
  | H2_flow_control_error of { stream_id: int32 option }
      (** Flow control window exceeded per
          {{:https://datatracker.ietf.org/doc/html/rfc9113#section-5.2}RFC 9113 Section 5.2}. *)
  | H2_compression_error of { message: string }
      (** HPACK decompression failed per
          {{:https://datatracker.ietf.org/doc/html/rfc7541}RFC 7541}. *)
  | H2_settings_timeout
      (** SETTINGS acknowledgment timeout per
          {{:https://datatracker.ietf.org/doc/html/rfc9113#section-6.5.3}RFC 9113 Section 6.5.3}. *)
  | H2_goaway of { last_stream_id: int32; code: int32; debug: string }
      (** Server sent GOAWAY frame per
          {{:https://datatracker.ietf.org/doc/html/rfc9113#section-6.8}RFC 9113 Section 6.8}. *)
  | H2_frame_error of { frame_type: int; message: string }
      (** Invalid frame received per RFC 9113 Section 4-6. *)
  | H2_header_validation_error of { message: string }
      (** HTTP/2 header validation failed per RFC 9113 Section 8.2-8.3. *)

(** {1 Eio.Exn Integration} *)

(** Extension of [Eio.Exn.err] for Requests errors *)
type Eio.Exn.err += E of error

(** Create an Eio exception from an error.
    Usage: [raise (err (Timeout { operation = "read"; duration = Some 5.0 }))] *)
val err : error -> exn

(** {1 URL and Credential Sanitization} *)

(** Remove userinfo (username:password) from a URL for safe logging *)
val sanitize_url : string -> string

(** Redact sensitive headers (Authorization, Cookie, etc.) for safe logging.
    Takes and returns a list of (name, value) pairs. *)
val sanitize_headers : (string * string) list -> (string * string) list

(** Check if a header name is sensitive (case-insensitive) *)
val is_sensitive_header : string -> bool

(** {1 Pretty Printing} *)

(** Pretty printer for error values *)
val pp_error : Format.formatter -> error -> unit

(** {1 Query Functions}

    These functions enable smart error handling without pattern matching. *)

(** Returns [true] if the error is a timeout *)
val is_timeout : error -> bool

(** Returns [true] if the error is a DNS resolution failure *)
val is_dns : error -> bool

(** Returns [true] if the error is a TLS handshake failure *)
val is_tls : error -> bool

(** Returns [true] if the error is any connection-related failure
    (DNS, TCP connect, or TLS handshake) *)
val is_connection : error -> bool

(** Returns [true] if the error is an HTTP response error *)
val is_http_error : error -> bool

(** Returns [true] if the error is a client error (4xx status or similar) *)
val is_client_error : error -> bool

(** Returns [true] if the error is a server error (5xx status) *)
val is_server_error : error -> bool

(** Returns [true] if the error is typically retryable.
    Retryable errors include: timeouts, connection errors,
    and certain HTTP status codes (408, 429, 500, 502, 503, 504) *)
val is_retryable : error -> bool

(** Returns [true] if the error is security-related
    (header injection, body too large, decompression bomb, etc.) *)
val is_security_error : error -> bool

(** Returns [true] if the error is a JSON parsing or encoding error *)
val is_json_error : error -> bool

(** Returns [true] if the error is an OAuth-related error
    (Oauth_error, Token_refresh_failed, Token_expired) *)
val is_oauth_error : error -> bool

(** {1 Error Extraction} *)

(** Extract error from an Eio.Io exception, if it's a Requests error *)
val of_eio_exn : exn -> error option

(** {1 HTTP Status Helpers} *)

(** Get the HTTP status code from an error, if applicable *)
val get_http_status : error -> int option

(** Get the URL associated with an error, if applicable *)
val get_url : error -> string option

(** {1 String Conversion} *)

(** Convert error to human-readable string *)
val to_string : error -> string

(** {1 Convenience Constructors}

    These functions provide a more concise way to create error exceptions
    compared to the verbose [err (Error_type { field = value; ... })] pattern.

    Example:
    {[
      (* Instead of: *)
      raise (err (Invalid_request { reason = "missing host" }))

      (* Use: *)
      raise (invalid_request ~reason:"missing host")
    ]} *)

val invalid_request : reason:string -> exn
(** Create an [Invalid_request] exception *)

val invalid_redirect : url:string -> reason:string -> exn
(** Create an [Invalid_redirect] exception *)

val invalid_url : url:string -> reason:string -> exn
(** Create an [Invalid_url] exception *)

val timeout : operation:string -> ?duration:float -> unit -> exn
(** Create a [Timeout] exception *)

val body_too_large : limit:int64 -> ?actual:int64 -> unit -> exn
(** Create a [Body_too_large] exception *)

val headers_too_large : limit:int -> actual:int -> exn
(** Create a [Headers_too_large] exception *)

val proxy_error : host:string -> reason:string -> exn
(** Create a [Proxy_error] exception *)

val tls_handshake_failed : host:string -> reason:string -> exn
(** Create a [Tls_handshake_failed] exception *)

val tcp_connect_failed : host:string -> port:int -> reason:string -> exn
(** Create a [Tcp_connect_failed] exception *)

(** {1 Format String Constructors}

    These functions accept printf-style format strings for the reason field,
    making error construction more concise when messages need interpolation.

    Example:
    {[
      (* Instead of: *)
      raise (Error.err (Error.Invalid_request {
        reason = Printf.sprintf "Invalid status code: %s" code_str
      }))

      (* Use: *)
      raise (Error.invalid_requestf "Invalid status code: %s" code_str)
    ]} *)

val invalid_requestf : ('a, unit, string, exn) format4 -> 'a
(** Create an [Invalid_request] exception with a format string *)

val invalid_redirectf : url:string -> ('a, unit, string, exn) format4 -> 'a
(** Create an [Invalid_redirect] exception with a format string *)

val invalid_urlf : url:string -> ('a, unit, string, exn) format4 -> 'a
(** Create an [Invalid_url] exception with a format string *)

val proxy_errorf : host:string -> ('a, unit, string, exn) format4 -> 'a
(** Create a [Proxy_error] exception with a format string *)

val tls_handshake_failedf : host:string -> ('a, unit, string, exn) format4 -> 'a
(** Create a [Tls_handshake_failed] exception with a format string *)

val tcp_connect_failedf : host:string -> port:int -> ('a, unit, string, exn) format4 -> 'a
(** Create a [Tcp_connect_failed] exception with a format string *)

(** {1 OAuth Error Constructors} *)

val oauth_error : error_code:string -> ?description:string -> ?uri:string -> unit -> exn
(** Create an [Oauth_error] exception *)

val token_refresh_failed : reason:string -> exn
(** Create a [Token_refresh_failed] exception *)

val token_expired : unit -> exn
(** Create a [Token_expired] exception *)

(** {1 HTTP/2 Error Query Functions}

    Query functions for HTTP/2 specific errors per
    {{:https://datatracker.ietf.org/doc/html/rfc9113}RFC 9113}. *)

val is_h2_error : error -> bool
(** Returns [true] if the error is any HTTP/2 protocol error *)

val is_h2_connection_error : error -> bool
(** Returns [true] if the error is an HTTP/2 connection-level error.
    Connection errors terminate the entire HTTP/2 connection. *)

val is_h2_stream_error : error -> bool
(** Returns [true] if the error is an HTTP/2 stream-level error.
    Stream errors only affect a single stream. *)

val is_h2_retryable : error -> bool
(** Returns [true] if the HTTP/2 error is typically retryable.
    Retryable errors include:
    - GOAWAY with NO_ERROR (graceful shutdown)
    - REFUSED_STREAM (server didn't process the request)
    - ENHANCE_YOUR_CALM (after backoff) *)

val get_h2_error_code : error -> int32 option
(** Get the HTTP/2 error code from an error, if applicable.
    Error codes are defined in RFC 9113 Section 7. *)

val get_h2_stream_id : error -> int32 option
(** Get the stream ID associated with an HTTP/2 error, if applicable. *)

(** {1 HTTP/2 Error Constructors}

    Convenience constructors for HTTP/2 errors per
    {{:https://datatracker.ietf.org/doc/html/rfc9113#section-7}RFC 9113 Section 7}. *)

val h2_protocol_error : code:int32 -> message:string -> exn
(** Create an [H2_protocol_error] exception *)

val h2_stream_error : stream_id:int32 -> code:int32 -> message:string -> exn
(** Create an [H2_stream_error] exception *)

val h2_flow_control_error : ?stream_id:int32 -> unit -> exn
(** Create an [H2_flow_control_error] exception.
    If [stream_id] is provided, it's a stream-level error;
    otherwise, it's a connection-level error. *)

val h2_compression_error : message:string -> exn
(** Create an [H2_compression_error] exception *)

val h2_settings_timeout : unit -> exn
(** Create an [H2_settings_timeout] exception *)

val h2_goaway : last_stream_id:int32 -> code:int32 -> debug:string -> exn
(** Create an [H2_goaway] exception *)

val h2_frame_error : frame_type:int -> message:string -> exn
(** Create an [H2_frame_error] exception *)

val h2_header_validation_error : message:string -> exn
(** Create an [H2_header_validation_error] exception *)

(** {2 HTTP/2 Error Code Names} *)

val h2_error_code_name : int32 -> string
(** [h2_error_code_name code] returns the name of an HTTP/2 error code.
    Per RFC 9113 Section 7:
    - 0x0: NO_ERROR
    - 0x1: PROTOCOL_ERROR
    - 0x2: INTERNAL_ERROR
    - 0x3: FLOW_CONTROL_ERROR
    - 0x4: SETTINGS_TIMEOUT
    - 0x5: STREAM_CLOSED
    - 0x6: FRAME_SIZE_ERROR
    - 0x7: REFUSED_STREAM
    - 0x8: CANCEL
    - 0x9: COMPRESSION_ERROR
    - 0xa: CONNECT_ERROR
    - 0xb: ENHANCE_YOUR_CALM
    - 0xc: INADEQUATE_SECURITY
    - 0xd: HTTP_1_1_REQUIRED *)
