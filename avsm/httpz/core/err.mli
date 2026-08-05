(** Error handling combinators for HTTP parsing.

    This module provides zero-cost abstractions for raising parse errors
    with various status codes. All functions are inlined for performance. *)

(** {1 Status Type} *)

(** Re-export of parse status for convenience *)
type status = Buf_read.status =
  | Complete
  | Partial
  | Invalid_method
  | Invalid_target
  | Invalid_version
  | Invalid_header
  | Headers_too_large
  | Malformed
  | Content_length_overflow
  | Ambiguous_framing
  | Bare_cr_detected
  | Missing_host_header
  | Unsupported_transfer_encoding

(** {1 Exception} *)

(** Parse error with detailed status *)
exception Parse_error of status

(** {1 Direct Fail Combinators} *)

(** [fail status] raises [Parse_error status]. *)
val fail : status -> 'a

(** [partial ()] raises [Parse_error Partial]. Use when more input is needed. *)
val partial : unit -> 'a

(** [malformed ()] raises [Parse_error Malformed]. Use for format violations. *)
val malformed : unit -> 'a

(** {1 Conditional Raises (when condition is TRUE)} *)

(** [when_ cond status] raises [Parse_error status] if [cond] is true. *)
val when_ : bool -> status -> unit

(** [partial_when cond] raises [Parse_error Partial] if [cond] is true. *)
val partial_when : bool -> unit

(** [malformed_when cond] raises [Parse_error Malformed] if [cond] is true. *)
val malformed_when : bool -> unit

(** {1 Guard Combinators (raise when condition is FALSE)} *)

(** [guard cond status] raises [Parse_error status] if [cond] is false. *)
val guard : bool -> status -> unit

(** [partial_unless cond] raises [Parse_error Partial] if [cond] is false.
    Use for buffer boundary checks where you require more data. *)
val partial_unless : bool -> unit

(** [malformed_unless cond] raises [Parse_error Malformed] if [cond] is false.
    Use for validation checks where you require a condition to hold. *)
val malformed_unless : bool -> unit

(** {1 Recovery Combinator} *)

(** [optional ~save ~restore f] tries to run [f ()]. On success, returns
    the result wrapped with [Or_null.some]. On [Parse_error], restores state
    using [restore (save ())] and returns [Or_null.none]. *)
val optional : save:(unit -> 'pos) -> restore:('pos -> unit) -> (unit -> 'a) -> 'a or_null
