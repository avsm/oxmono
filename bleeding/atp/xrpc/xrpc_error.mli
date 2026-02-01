(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** XRPC error types.

    Errors from XRPC operations are wrapped as [Eio.Io] exceptions for
    consistency with other Eio-based libraries. The error type provides
    structured access to XRPC-specific error details.

    {2 Error Handling}

    {[
      try
        let result = Xrpc.Client.query client ~nsid ~params ~decoder in
        (* ... *)
      with
      | Eio.Io (Xrpc_error.E err, _) ->
          match err with
          | Xrpc_error (_, code, _) when code = "ExpiredToken" ->
              (* Handle token expiry *)
          | Network_error _ ->
              (* Handle network issues *)
          | _ -> (* Handle other errors *)
    ]} *)

(** {1 Error Types} *)

(** XRPC error variants. *)
type error =
  | Network_error of { reason : string }
      (** Network-level failure (connection refused, timeout, etc.) *)
  | Parse_error of { reason : string; body_preview : string option }
      (** JSON parsing or decoding failed *)
  | Xrpc_error of { status : int; error : string; message : string option }
      (** XRPC error response from server.
          @param status HTTP status code
          @param error Error code (e.g., ["ExpiredToken"], ["InvalidHandle"])
          @param message Optional detailed message *)
  | Token_expired  (** Session token has expired and could not be refreshed *)
  | Session_required
      (** Operation requires authentication but no session is set *)

(** {1 Eio Exception Integration} *)

type Eio.Exn.err +=
  | E of error
        (** Eio exception wrapper for XRPC errors.

            Raise with: [raise (Eio.Exn.create (E error))] Catch with:
            [Eio.Io (E error, _)] *)

val err : error -> exn
(** [err e] creates an Eio exception from an error. Equivalent to
    [Eio.Exn.create (E e)]. *)

(** {1 Error Properties} *)

val is_retryable : error -> bool
(** [is_retryable e] returns [true] if the error is transient and the operation
    may succeed on retry.

    Retryable errors include:
    - Network errors
    - HTTP 408 (Request Timeout)
    - HTTP 429 (Too Many Requests)
    - HTTP 5xx (Server errors) *)

val is_auth_error : error -> bool
(** [is_auth_error e] returns [true] if the error indicates an authentication
    problem that may require session refresh or re-login.

    Auth errors include:
    - [Token_expired]
    - XRPC errors with codes ["ExpiredToken"], ["InvalidToken"],
      ["AuthRequired"] *)

(** {1 Error Extraction} *)

val of_eio_exn : exn -> error option
(** [of_eio_exn exn] extracts an XRPC error from an Eio exception. Returns
    [None] if the exception is not an XRPC error. *)

(** {1 Formatting} *)

val pp : error Fmt.t
(** Pretty-print an error. *)

val to_string : error -> string
(** Convert error to human-readable string. *)
