open! Core
open Async_kernel
open! Import

type t

val create
  :  default_outputs:Output.t list
  -> named_outputs:Output.t Output_name.Map.t
  -> on_background_output_error:(exn -> unit)
  -> t

val is_empty : t -> bool
val write : t -> Message_event.t -> unit
val flushed : t -> unit Deferred.t
val current_default_outputs : t -> Output.t list
val update_default_outputs : t -> Output.t list -> unit
val current_named_outputs : t -> Output.t Output_name.Map.t
val update_named_outputs : t -> Output.t Output_name.Map.t -> unit
val set_named_output : t -> Output_name.t -> Output.t -> unit
val remove_named_output : t -> Output_name.t -> unit
