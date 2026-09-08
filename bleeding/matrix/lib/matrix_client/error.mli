(** error — what a failed call reports.

    A failing request is a {!Matrix_error} when the homeserver answered with a
    Matrix error object, and an {!Http_error} when it answered non-2xx with
    something else. The remaining cases arise before or after the exchange, so
    the server never sees them.

    @see <https://spec.matrix.org/v1.11/client-server-api/#standard-error-response>
      Standard Error Response *)

(** The spec's [errcode] values, with anything else in {!M_UNKNOWN_CODE}. *)
type errcode =
  | M_FORBIDDEN
  | M_UNKNOWN_TOKEN
  | M_MISSING_TOKEN
  | M_BAD_JSON
  | M_NOT_JSON
  | M_NOT_FOUND
  | M_LIMIT_EXCEEDED
  | M_UNRECOGNIZED
  | M_UNKNOWN
  | M_UNAUTHORIZED
  | M_USER_DEACTIVATED
  | M_USER_IN_USE
  | M_INVALID_USERNAME
  | M_ROOM_IN_USE
  | M_INVALID_ROOM_STATE
  | M_THREEPID_IN_USE
  | M_THREEPID_NOT_FOUND
  | M_THREEPID_AUTH_FAILED
  | M_THREEPID_DENIED
  | M_SERVER_NOT_TRUSTED
  | M_UNSUPPORTED_ROOM_VERSION
  | M_INCOMPATIBLE_ROOM_VERSION
  | M_BAD_STATE
  | M_GUEST_ACCESS_FORBIDDEN
  | M_CAPTCHA_NEEDED
  | M_CAPTCHA_INVALID
  | M_MISSING_PARAM
  | M_INVALID_PARAM
  | M_TOO_LARGE
  | M_CANNOT_OVERWRITE_MEDIA
  | M_WRONG_ROOM_KEYS_VERSION
  | M_EXCLUSIVE
  | M_RESOURCE_LIMIT_EXCEEDED
  | M_CANNOT_LEAVE_SERVER_NOTICE_ROOM
  | M_WEAK_PASSWORD
  | M_UNKNOWN_CODE of string

val errcode_to_string : errcode -> string
(** The code as it appears on the wire. *)

val errcode_of_string : string -> errcode
(** The inverse of {!errcode_to_string}. An unrecognised string becomes
    {!M_UNKNOWN_CODE}, so this never fails. *)

type matrix_error = {
  errcode : errcode;
  error : string;  (** The server's human-readable message; may be empty. *)
  retry_after_ms : int option;
      (** How long to wait before retrying, sent with {!M_LIMIT_EXCEEDED}. *)
  soft_logout : bool option;
      (** Sent with {!M_UNKNOWN_TOKEN}. [Some true] means the device was not
          deleted, so the user can log in again and keep its keys. *)
}
(** A homeserver's error object. *)

val matrix_error_jsont : matrix_error Jsont.t
(** Reads and writes the spec's error object. A missing [error] member decodes
    as the empty string. *)

(** What went wrong. *)
type t =
  | Matrix_error of matrix_error
      (** The server answered with an error object. *)
  | Network_error of string  (** The request did not complete. *)
  | Policy_denied of string
      (** A local capability policy refused the request before it could leave,
          for example because a redirect crossed the homeserver origin. *)
  | Tls_error of string
      (** TLS setup, certificate validation or the encrypted transport failed.
          Repeating the same request without changing trust or server state is
          not expected to help. *)
  | Json_error of string  (** A body could not be encoded or decoded. *)
  | Http_error of { status : int; body : string }
      (** A non-2xx response whose body is not a Matrix error. *)
  | No_session
      (** The call needs an access token and the client carries none, so no
          request was made. *)
  | No_content
      (** The request succeeded and returned nothing the call could decode. *)

val errcode : t -> errcode option
(** [errcode e] is the server's code for a {!Matrix_error}, and [None] for every
    other case. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when the two errors are the same case carrying equal
    payloads. *)

val pp : Format.formatter -> t -> unit
(** Renders the error on one line, without a trailing newline. *)

val to_string : t -> string
(** [to_string e] is {!pp} into a string. *)
