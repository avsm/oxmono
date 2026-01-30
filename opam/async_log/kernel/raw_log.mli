open! Core
open Async_kernel
open! Import

type t

module Control_event : sig
  type t = Set_level of Level.t [@@deriving globalize, sexp_of]
end

val create
  :  level:Level.t
  -> default_outputs:Output.t list
  -> named_outputs:Output.t Output_name.Map.t
  -> on_error:On_error.t
  -> time_source:[> read ] Synchronous_time_source.T1.t option
  -> transforms:(Message_event.t -> Message_event.t option) list
  -> t

val copy : t -> t
val level : t -> Level.t
val set_level : t -> Level.t -> unit
val set_output : t -> Output.t list -> unit
val get_output : t -> Output.t list
val get_time_source : t -> Synchronous_time_source.t
val set_time_source : t -> [> read ] Synchronous_time_source.T1.t -> unit
val has_transform : t -> bool

module Transform : sig
  type log := t
  type t

  val add
    :  log
    -> (Message_event.t -> Message_event.t option)
    -> [ `Before | `After ]
    -> t

  val remove_exn : log -> t -> unit
end

val clear_transforms : t -> unit
val set_transform : t -> (Message_event.t -> Message_event.t option) option -> unit
val get_transform : t -> (Message_event.t -> Message_event.t option) option
val get_on_error : t -> On_error.t
val set_on_error : t -> On_error.t -> unit
val close : t -> unit Deferred.t
val is_closed : t -> bool
val flushed : t -> unit Deferred.t
val would_log : t -> Level.t option -> bool
val push_message_event : t -> Message_event.t -> unit
val all_live_logs_flushed : unit -> unit Deferred.t
val control_events : t -> (local_ Control_event.t -> unit) Bus.Read_only.t

module Private : sig
  val set_named_output : t -> Output_name.t -> Output.t -> unit
  val get_named_output : t -> Output_name.t -> Output.t option
  val remove_named_output : t -> Output_name.t -> unit
  val with_temporary_outputs : t -> Output.t list -> f:(unit -> unit) -> unit

  module For_testing : sig
    val get_named_outputs : t -> Output.t Output_name.Map.t
    val update_named_outputs : t -> Output.t Output_name.Map.t -> unit
  end
end

module For_testing : sig
  val transform : t -> Message_event.t -> Message_event.t option
end
