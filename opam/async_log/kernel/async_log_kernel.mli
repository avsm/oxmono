open! Core
open! Async_kernel
module Global = Global
module Level = Level
module Log = Log
module Message = Message
module Message_event = Message_event

module Output : sig
  type t = Output.t

  (** [create f] returns a [t], given a function that actually performs the final output
      work. It is the responsibility of the write function to contain all state, and to
      clean up after itself when it is garbage collected (which may require a finalizer).
      The function should avoid modifying the contents of the queue; it's reused for each
      [Output.t].

      [flush] should return a deferred that is fulfilled only when all previously written
      messages are durable (e.g., on disk, out on the network, etc.). It is automatically
      called by [Log] on shutdown and at garbage collection. It can be called manually by
      calling [Log.flushed t].

      The [unit Deferred] returned by the function provides an opportunity for pushback if
      that is important. Only one batch of messages will be "in flight" at any time based
      on this deferred.

      [finalize] will be called when the output is finalized (meaning, no more open logs
      use the output, and references to the output are gone). *)
  val create
    :  ?rotate:(unit -> unit Deferred.t)
    -> ?finalize:(unit -> unit Deferred.t)
    -> flush:(unit -> unit Deferred.t)
    -> (Message.t Queue.t -> unit Deferred.t)
    -> t

  val create_expert
    :  ?finalize:(unit -> unit Deferred.t)
    -> flush:(unit -> unit Deferred.t)
    -> (Message_event.t Queue.t -> unit Deferred.t)
    -> t

  (** A version of [create] that writes without buffering internally and processing in a
      background loop as outputs usually do, and has access to richer information in the
      write callback. *)
  val create_unbuffered
    :  ?finalize:(unit -> unit Deferred.t)
    -> flush:(unit -> unit Deferred.t)
    -> (Message_event.t -> unit)
    -> t

  (** [filter_to_level] wraps an output and gives you a new output which only logs
      messages which are as/more verbose than [level].

      This functionality is intended for when you have multiple outputs being displayed in
      different places, and they need to be at different levels.

      If you have one output (or multiple outputs all at the same level), it is better to
      set the [Log.t]'s output directly with [set_level], which is equivalent and more
      efficient. *)
  val filter_to_level : t -> level:Level.t -> t

  (** [transform_message] wraps an output and gives you a new output which transforms
      messages before writing.

      This functionality is intended for when you have multiple outputs being displayed in
      different places and they need to have different messages.

      If you have one output (or multiple outputs all using the same messages), it is
      better to set the [Log.t]'s transform directly with [set_transform], which is
      equivalent and more efficient. *)
  val transform_message : t -> f:(Message_event.t -> Message_event.t) -> t

  val empty : t

  val rotate : t -> unit Deferred.t
  [@@alert deprecated "Do not introduce new uses of this function."]

  module Format = Output_format

  module Private : sig
    module Name = Output_name

    val set_async_stderr_output : t lazy_t -> here:Source_code_position.t -> unit
    val write : t -> Message_event.t -> unit
    val flush : t -> unit Deferred.t
    val buffered_batch_size : int
  end

  module For_testing : sig
    val is_async_stderr_output_set : unit -> bool
  end
end

module Ppx_log_syntax : sig
  (** [Async_log.Ppx_log_syntax.Ppx_log_syntax] exists so that people can
      [open Async_log.Ppx_log_syntax] to use ppx log, instead of doing a module alias.
      This is consistent with [Monad.Syntax.Let_syntax]. *)
  module Ppx_log_syntax : module type of Ppx_log_syntax
end

module For_testing : sig
  module Mutable_outputs = Mutable_outputs
end
