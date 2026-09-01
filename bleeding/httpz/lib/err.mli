(** This module provides exception-based guards for low-level HTTP parsers. *)

(** A [status] is a parse status accepted by this module. *)
type status = Buf_read.status =
  | Complete
  | Partial
  | Invalid_method
  | Unsupported_method
  | Invalid_target
  | Uri_too_long
  | Invalid_version
  | Invalid_status
  | Invalid_header
  | Headers_too_large
  | Malformed
  | Content_length_overflow
  | Ambiguous_framing
  | Bare_cr_detected
  | Missing_host_header
  | Unsupported_transfer_encoding

(** [Parse_error status] is the exception that aborts parsing with [status]. *)
exception Parse_error of status

(** [fail status] is the operation that raises [Parse_error status]. *)
val fail : status -> 'a @@ portable

(** [when_ condition status] is the operation that raises [Parse_error status] when
    [condition] is [true]. *)
val when_ : bool -> status -> unit @@ portable

(** [partial_when condition] is the operation that raises [Parse_error Partial] when
    [condition] is [true]. *)
val partial_when : bool -> unit @@ portable

(** [malformed_when condition] is the operation that raises [Parse_error Malformed] when
    [condition] is [true]. *)
val malformed_when : bool -> unit @@ portable
