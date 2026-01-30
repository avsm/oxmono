open! Core
open! Import

(** A [t] represents a log message with additional structured information about the
    statement, for use by outputs. Most outputs flatten the event to a [Message.t], which
    predates this module and is serializable / deserializable, but has less structured
    information.

    (This module's signature is tight / bare to keep the type extensible and
    backwards-compatible with previous behaviour, but this isn't intended to deter any
    users who may wish for an accessor to be added, especially for newer structured info
    in [Message_sexp.t].) *)
type t

val create
  :  ?time:Time_float.t
  -> ?source:Message_source.t
  -> ?legacy_tags:(string * string) list
  -> ?level:Level.t
  -> Message_data.t
  -> t

val raw_message : t -> Message_data.t
val message : t -> string
val source : t -> Message_source.t
val legacy_tags : t -> (string * string) list
val level : t -> Level.t option
val time : t -> Time_float.t
val add_tags : t -> tags:(string * string) list -> t
val map_legacy_tags : t -> f:(string * string -> string * string) -> t

(** Convert any structured messages to a string and map. The structured format is not
    recovered afterwards. This is mainly used for convenience in testing outputs. *)
val stringify_message_and_map : t -> f:(string -> string) -> t

(** Convert any structured messages to the unstructured sexp format and map. The
    structured format is not recovered afterwards.

    This is used by old code that predates the structured messages, often to censor
    certain fields out of log messages. *)
val downgrade_to_unstructured_and_map : t -> f:(Sexp_or_string.t -> Sexp_or_string.t) -> t

val set_level : t -> level:Level.t option -> t
val map_time : t -> f:(Time_float.t -> Time_float.t) -> t
val to_serialized_message_lossy : t -> Message.t
val of_serialized_message : Message.t -> t

module Unstable : sig
  (** [sexp_of] is here since we explicitly do not want this serialized in a way where
      users may expect to be able to deserialize. *)
  type nonrec t = t [@@deriving sexp_of]
end

module Private : sig
  val create
    :  Message_data.t
    -> Message_source.t
    -> time:Time_float.t
    -> level:Level.t option
    -> legacy_tags:(string * string) list
    -> user_scope:string option
    -> function_name:string option
    -> async_trace_span:Univ.t option
    -> t

  val async_trace_span : t -> Univ.t option
  val user_scope : t -> string option
end
