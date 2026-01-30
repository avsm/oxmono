(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Centralized error handling for the Requests library using Eio.Io exceptions *)

let src = Logs.Src.create "requests.error" ~doc:"HTTP Request Errors"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Error Type}

    Following the Eio.Io exception pattern for structured error handling.
    Each variant contains a record with contextual information. *)

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

  (* Connection errors - granular breakdown per Recommendation #17 *)
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
          exposes credentials to eavesdropping. *)

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

(** {1 URL and Credential Sanitization}

    Per Recommendation #20: Remove sensitive info from error messages *)

let sanitize_url url =
  try
    let uri = Uri.of_string url in
    let sanitized = Uri.with_userinfo uri None in
    Uri.to_string sanitized
  with _ -> url  (* If parsing fails, return original *)

(** List of header names considered sensitive (lowercase) *)
let sensitive_header_names =
  ["authorization"; "cookie"; "proxy-authorization"; "x-api-key"; "api-key"; "set-cookie"]

(** Check if a header name is sensitive (case-insensitive) *)
let is_sensitive_header name =
  List.mem (String.lowercase_ascii name) sensitive_header_names

(** Sanitize a header list by redacting sensitive values *)
let sanitize_headers headers =
  List.map (fun (name, value) ->
    if is_sensitive_header name then (name, "[REDACTED]")
    else (name, value)
  ) headers

(** {1 Pretty Printing} *)

let pp_error ppf = function
  | Timeout { operation; duration } ->
      (match duration with
       | Some d -> Format.fprintf ppf "Timeout during %s after %.2fs" operation d
       | None -> Format.fprintf ppf "Timeout during %s" operation)

  | Too_many_redirects { url; count; max } ->
      Format.fprintf ppf "Too many redirects (%d/%d) for URL: %s" count max (sanitize_url url)

  | Invalid_redirect { url; reason } ->
      Format.fprintf ppf "Invalid redirect to %s: %s" (sanitize_url url) reason

  | Http_error { url; status; reason; body_preview; headers = _ } ->
      Format.fprintf ppf "@[<v>HTTP %d %s@ URL: %s" status reason (sanitize_url url);
      Option.iter (fun body ->
        let preview = if String.length body > 200
          then String.sub body 0 200 ^ "..."
          else body in
        Format.fprintf ppf "@ Body: %s" preview
      ) body_preview;
      Format.fprintf ppf "@]"

  | Authentication_failed { url; reason } ->
      Format.fprintf ppf "Authentication failed for %s: %s" (sanitize_url url) reason

  | Dns_resolution_failed { hostname } ->
      Format.fprintf ppf "DNS resolution failed for hostname: %s" hostname

  | Tcp_connect_failed { host; port; reason } ->
      Format.fprintf ppf "TCP connection to %s:%d failed: %s" host port reason

  | Tls_handshake_failed { host; reason } ->
      Format.fprintf ppf "TLS handshake with %s failed: %s" host reason

  | Invalid_header { name; reason } ->
      Format.fprintf ppf "Invalid header '%s': %s" name reason

  | Body_too_large { limit; actual } ->
      (match actual with
       | Some a -> Format.fprintf ppf "Response body too large: %Ld bytes (limit: %Ld)" a limit
       | None -> Format.fprintf ppf "Response body exceeds limit of %Ld bytes" limit)

  | Headers_too_large { limit; actual } ->
      Format.fprintf ppf "Response headers too large: %d (limit: %d)" actual limit

  | Decompression_bomb { limit; ratio } ->
      Format.fprintf ppf "Decompression bomb detected: ratio %.1f:1 exceeds limit, max size %Ld bytes"
        ratio limit

  | Content_length_mismatch { expected; actual } ->
      Format.fprintf ppf "Content-Length mismatch: expected %Ld bytes, received %Ld bytes"
        expected actual

  | Insecure_auth { url; auth_type } ->
      Format.fprintf ppf "%s authentication over unencrypted HTTP rejected for %s. \
        Use HTTPS or set allow_insecure_auth=true (not recommended)"
        auth_type (sanitize_url url)

  | Json_parse_error { body_preview; reason } ->
      let preview = if String.length body_preview > 100
        then String.sub body_preview 0 100 ^ "..."
        else body_preview in
      Format.fprintf ppf "@[<v>JSON parse error: %s@ Body preview: %s@]" reason preview

  | Json_encode_error { reason } ->
      Format.fprintf ppf "JSON encode error: %s" reason

  | Proxy_error { host; reason } ->
      Format.fprintf ppf "Proxy error for %s: %s" host reason

  | Encoding_error { encoding; reason } ->
      Format.fprintf ppf "Encoding error (%s): %s" encoding reason

  | Invalid_url { url; reason } ->
      Format.fprintf ppf "Invalid URL '%s': %s" (sanitize_url url) reason

  | Invalid_request { reason } ->
      Format.fprintf ppf "Invalid request: %s" reason

  | Oauth_error { error_code; description; uri } ->
      Format.fprintf ppf "OAuth error: %s" error_code;
      Option.iter (fun desc -> Format.fprintf ppf " - %s" desc) description;
      Option.iter (fun u -> Format.fprintf ppf " (see: %s)" u) uri

  | Token_refresh_failed { reason } ->
      Format.fprintf ppf "Token refresh failed: %s" reason

  | Token_expired ->
      Format.fprintf ppf "Access token expired and no refresh token available"

  (* HTTP/2 errors *)
  | H2_protocol_error { code; message } ->
      Format.fprintf ppf "HTTP/2 protocol error (code 0x%02lx): %s" code message

  | H2_stream_error { stream_id; code; message } ->
      Format.fprintf ppf "HTTP/2 stream %ld error (code 0x%02lx): %s" stream_id code message

  | H2_flow_control_error { stream_id } ->
      (match stream_id with
       | Some id -> Format.fprintf ppf "HTTP/2 flow control error on stream %ld" id
       | None -> Format.fprintf ppf "HTTP/2 connection flow control error")

  | H2_compression_error { message } ->
      Format.fprintf ppf "HTTP/2 HPACK compression error: %s" message

  | H2_settings_timeout ->
      Format.fprintf ppf "HTTP/2 SETTINGS acknowledgment timeout"

  | H2_goaway { last_stream_id; code; debug } ->
      Format.fprintf ppf "HTTP/2 GOAWAY received (last_stream=%ld, code=0x%02lx): %s"
        last_stream_id code debug

  | H2_frame_error { frame_type; message } ->
      Format.fprintf ppf "HTTP/2 frame error (type 0x%02x): %s" frame_type message

  | H2_header_validation_error { message } ->
      Format.fprintf ppf "HTTP/2 header validation error: %s" message

(** {1 Eio.Exn Integration}

    Following the pattern from ocaml-conpool for structured Eio exceptions *)

type Eio.Exn.err += E of error

let err e = Eio.Exn.create (E e)

let () =
  Eio.Exn.register_pp (fun f -> function
    | E e ->
        Format.fprintf f "Requests: ";
        pp_error f e;
        true
    | _ -> false)

(** {1 Query Functions}

    Per Recommendation #17: Enable smarter retry logic and error handling *)

let is_timeout = function
  | Timeout _ -> true
  | _ -> false

let is_dns = function
  | Dns_resolution_failed _ -> true
  | _ -> false

let is_tls = function
  | Tls_handshake_failed _ -> true
  | _ -> false

let is_connection = function
  | Dns_resolution_failed _ -> true
  | Tcp_connect_failed _ -> true
  | Tls_handshake_failed _ -> true
  | _ -> false

let is_http_error = function
  | Http_error _ -> true
  | _ -> false

let is_client_error = function
  | Http_error { status; _ } -> status >= 400 && status < 500
  | Authentication_failed _ -> true
  | Invalid_url _ -> true
  | Invalid_request _ -> true
  | Invalid_header _ -> true
  | _ -> false

let is_server_error = function
  | Http_error { status; _ } -> status >= 500 && status < 600
  | _ -> false

let is_retryable = function
  | Timeout _ -> true
  | Dns_resolution_failed _ -> true
  | Tcp_connect_failed _ -> true
  | Tls_handshake_failed _ -> true
  | Http_error { status; _ } ->
      (* Retryable status codes: 408, 429, 500, 502, 503, 504 *)
      List.mem status [408; 429; 500; 502; 503; 504]
  | Proxy_error _ -> true
  | _ -> false

let is_security_error = function
  | Invalid_header _ -> true
  | Body_too_large _ -> true
  | Headers_too_large _ -> true
  | Decompression_bomb _ -> true
  | Invalid_redirect _ -> true
  | Insecure_auth _ -> true
  | _ -> false

let is_json_error = function
  | Json_parse_error _ -> true
  | Json_encode_error _ -> true
  | _ -> false

let is_oauth_error = function
  | Oauth_error _ -> true
  | Token_refresh_failed _ -> true
  | Token_expired -> true
  | _ -> false

(** {1 HTTP/2 Error Query Functions} *)

let is_h2_error = function
  | H2_protocol_error _ -> true
  | H2_stream_error _ -> true
  | H2_flow_control_error _ -> true
  | H2_compression_error _ -> true
  | H2_settings_timeout -> true
  | H2_goaway _ -> true
  | H2_frame_error _ -> true
  | H2_header_validation_error _ -> true
  | _ -> false

let is_h2_connection_error = function
  | H2_protocol_error _ -> true
  | H2_flow_control_error { stream_id = None } -> true
  | H2_compression_error _ -> true
  | H2_settings_timeout -> true
  | H2_goaway _ -> true
  | _ -> false

let is_h2_stream_error = function
  | H2_stream_error _ -> true
  | H2_flow_control_error { stream_id = Some _ } -> true
  | _ -> false

let is_h2_retryable = function
  (* GOAWAY with NO_ERROR is graceful shutdown - safe to retry *)
  | H2_goaway { code = 0l; _ } -> true
  (* REFUSED_STREAM means server didn't process, safe to retry *)
  | H2_stream_error { code = 0x7l; _ } -> true
  | H2_protocol_error { code = 0x7l; _ } -> true
  (* ENHANCE_YOUR_CALM might be retryable after backoff *)
  | H2_stream_error { code = 0xbl; _ } -> true
  | _ -> false

let get_h2_error_code = function
  | H2_protocol_error { code; _ } -> Some code
  | H2_stream_error { code; _ } -> Some code
  | H2_goaway { code; _ } -> Some code
  | _ -> None

let get_h2_stream_id = function
  | H2_stream_error { stream_id; _ } -> Some stream_id
  | H2_flow_control_error { stream_id } -> stream_id
  | H2_goaway { last_stream_id; _ } -> Some last_stream_id
  | _ -> None

(** {1 Error Extraction}

    Extract error from Eio.Io exception *)

let of_eio_exn = function
  | Eio.Io (E e, _) -> Some e
  | _ -> None

(** {1 HTTP Status Helpers} *)

let get_http_status = function
  | Http_error { status; _ } -> Some status
  | _ -> None

let get_url = function
  | Too_many_redirects { url; _ } -> Some url
  | Invalid_redirect { url; _ } -> Some url
  | Http_error { url; _ } -> Some url
  | Authentication_failed { url; _ } -> Some url
  | Invalid_url { url; _ } -> Some url
  | _ -> None

(** {1 String Conversion} *)

let to_string e =
  Format.asprintf "%a" pp_error e

(** {1 Convenience Constructors}

    These functions provide a more concise way to raise common errors
    compared to the verbose [raise (err (Error_type { field = value; ... }))] pattern. *)

let invalid_request ~reason =
  err (Invalid_request { reason })

let invalid_redirect ~url ~reason =
  err (Invalid_redirect { url; reason })

let invalid_url ~url ~reason =
  err (Invalid_url { url; reason })

let timeout ~operation ?duration () =
  err (Timeout { operation; duration })

let body_too_large ~limit ?actual () =
  err (Body_too_large { limit; actual })

let headers_too_large ~limit ~actual =
  err (Headers_too_large { limit; actual })

let proxy_error ~host ~reason =
  err (Proxy_error { host; reason })

let tls_handshake_failed ~host ~reason =
  err (Tls_handshake_failed { host; reason })

let tcp_connect_failed ~host ~port ~reason =
  err (Tcp_connect_failed { host; port; reason })

(** {1 Format String Constructors}

    These functions accept printf-style format strings for the reason field,
    making error construction more concise when messages need interpolation. *)

let invalid_requestf fmt =
  Printf.ksprintf (fun reason -> err (Invalid_request { reason })) fmt

let invalid_redirectf ~url fmt =
  Printf.ksprintf (fun reason -> err (Invalid_redirect { url; reason })) fmt

let invalid_urlf ~url fmt =
  Printf.ksprintf (fun reason -> err (Invalid_url { url; reason })) fmt

let proxy_errorf ~host fmt =
  Printf.ksprintf (fun reason -> err (Proxy_error { host; reason })) fmt

let tls_handshake_failedf ~host fmt =
  Printf.ksprintf (fun reason -> err (Tls_handshake_failed { host; reason })) fmt

let tcp_connect_failedf ~host ~port fmt =
  Printf.ksprintf (fun reason -> err (Tcp_connect_failed { host; port; reason })) fmt

(** {1 OAuth Error Constructors} *)

let oauth_error ~error_code ?description ?uri () =
  err (Oauth_error { error_code; description; uri })

let token_refresh_failed ~reason =
  err (Token_refresh_failed { reason })

let token_expired () =
  err Token_expired

(** {1 HTTP/2 Error Constructors}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-7}RFC 9113 Section 7}. *)

let h2_protocol_error ~code ~message =
  err (H2_protocol_error { code; message })

let h2_stream_error ~stream_id ~code ~message =
  err (H2_stream_error { stream_id; code; message })

let h2_flow_control_error ?stream_id () =
  err (H2_flow_control_error { stream_id })

let h2_compression_error ~message =
  err (H2_compression_error { message })

let h2_settings_timeout () =
  err H2_settings_timeout

let h2_goaway ~last_stream_id ~code ~debug =
  err (H2_goaway { last_stream_id; code; debug })

let h2_frame_error ~frame_type ~message =
  err (H2_frame_error { frame_type; message })

let h2_header_validation_error ~message =
  err (H2_header_validation_error { message })

(** {2 HTTP/2 Error Code Names}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-7}RFC 9113 Section 7}. *)

let h2_error_code_name = function
  | 0x0l -> "NO_ERROR"
  | 0x1l -> "PROTOCOL_ERROR"
  | 0x2l -> "INTERNAL_ERROR"
  | 0x3l -> "FLOW_CONTROL_ERROR"
  | 0x4l -> "SETTINGS_TIMEOUT"
  | 0x5l -> "STREAM_CLOSED"
  | 0x6l -> "FRAME_SIZE_ERROR"
  | 0x7l -> "REFUSED_STREAM"
  | 0x8l -> "CANCEL"
  | 0x9l -> "COMPRESSION_ERROR"
  | 0xal -> "CONNECT_ERROR"
  | 0xbl -> "ENHANCE_YOUR_CALM"
  | 0xcl -> "INADEQUATE_SECURITY"
  | 0xdl -> "HTTP_1_1_REQUIRED"
  | code -> Printf.sprintf "UNKNOWN(0x%lx)" code
