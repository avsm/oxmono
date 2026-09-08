(** Structured request and storage failures.

    [Api] retains the HTTP status, Zulip error code and extra JSON fields.
    [Http] describes a response without a valid Zulip error envelope. [Json]
    preserves decoding locations. [Timeout] and retry delays use seconds. Eio
    cancellation is never converted to a result. *)
type t =
  | Api of {
      status : int;
      code : string;
      message : string;
      extra : Jsont.json;
      retry_after : float option;
    }
  | Http of { status : int; message : string; retry_after : float option }
  | Json of Jsont.Error.t
  | Transport of Fetch.error
  | Timeout of float
  | Invalid_request of string
  | Storage of string

val pp : Format.formatter -> t -> unit
(** [pp ppf error] formats a failure for diagnostics. *)

val error_to_string : t -> string
(** [error_to_string error] is its diagnostic text. *)

val retry_after : t -> float option
(** [retry_after error] is the server retry delay in seconds, if supplied. *)

val is_rate_limit : t -> bool
(** [is_rate_limit error] is true for HTTP 429 or the [RATE_LIMIT_HIT] API code.
*)

val is_bad_queue : t -> bool
(** [is_bad_queue error] is true for the [BAD_EVENT_QUEUE_ID] API code. *)

val is_terminal : t -> bool
(** [is_terminal error] is [true] for authentication, permission, disabled
    account or realm, invalid-request, or permanent transport-policy failures.
*)

exception E of t
(** [E error] carries a structured failure across an exception-based boundary.
*)

val or_raise : ('a, t) result -> 'a
(** [or_raise result] is the successful value of [result].
    @raise exception-E if [result] is [Error error]. *)

val catch : (unit -> 'a) -> ('a, t) result
(** [catch f] is the result of [f ()]. It converts {!exception-E},
    {!Jsont.exception-Error} and Eio I/O exceptions into structured errors.
    Other exceptions, including cancellation and
    {!Stdlib.exception-Invalid_argument}, propagate. *)
