(** Foundation Models failures. *)

type context_size_exceeded = Apple_fm_base.context_size_exceeded = {
  context_size : int option;
  token_count : int option;
  message : string;
}
(** Context-window limit details. *)

type rate_limited = Apple_fm_base.rate_limited = {
  reset_time : float option;
  message : string;
}
(** Rate-limit details, including the Unix reset time when Apple provides it. *)

type unsupported_capability = Apple_fm_base.unsupported_capability = {
  capability : string option;
  message : string;
}
(** Details of a capability that the selected model does not provide. *)

type unsupported_guide = Apple_fm_base.unsupported_guide = {
  schema : string option;
  message : string;
}
(** Details of a generation guide that Apple rejected. *)

type unsupported_language = Apple_fm_base.unsupported_language = {
  language : string option;
  message : string;
}
(** Details of a language that the selected model does not support. *)

type t =
  [ `Context_size_exceeded of context_size_exceeded
  | `Rate_limited of rate_limited
  | `Guardrail_violation of string
  | `Refusal of string
  | `Unsupported_capability of unsupported_capability
  | `Unsupported_transcript of string
  | `Unsupported_guide of unsupported_guide
  | `Unsupported_language of unsupported_language
  | `Timeout of string
  | `Concurrent_requests of string
  | `Transcript_mutation of string
  | `Assets_unavailable of string
  | `Decoding_failure of string
  | `Tool_failure of string
  | `Cancelled of string
  | `Unsupported_version of string
  | `Framework_error of string
  | `Closed ]
(** A framework, generation, tool, or session failure. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter error] prints [error]. *)

type Eio.Exn.err +=
  | E of t
        (** [E error] is carried by [Eio.Io (E error, context)]. Use
            [Eio.Exn.pp] to print the failure and its operation context
            together. *)
