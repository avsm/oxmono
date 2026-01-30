open! Core
open! Async_kernel
open! Import

(** An interface for singleton logs. *)
module type S = sig
  val log : Log.t Lazy.t

  (** Make a copy of this log, with potentially some settings changed, or potentially
      extra tags added to each line. *)
  val copy
    :  ?level:Level.t
    -> ?on_error:[ `Call of Error.t -> unit | `Raise ]
    -> ?output:Output.t list
    -> ?extra_tags:(string * string) list
    -> unit
    -> Log.t

  val level : unit -> Level.t
  val set_level : Level.t -> unit
  val set_output : Output.t list -> unit
  val get_output : unit -> Output.t list
  val set_on_error : [ `Raise | `Call of Error.t -> unit ] -> unit
  val get_time_source : unit -> Synchronous_time_source.t
  val set_time_source : [> read ] Synchronous_time_source.T1.t -> unit

  module Transform : sig
    type t

    val append' : (Message_event.t -> Message_event.t option) -> t
    val prepend' : (Message_event.t -> Message_event.t option) -> t
    val append : (Message_event.t -> Message_event.t) -> unit
    val prepend : (Message_event.t -> Message_event.t) -> unit
    val remove_exn : t -> unit
  end

  val has_transform : unit -> bool
  val clear_transforms : unit -> unit

  val set_transform : (Message_event.t -> Message_event.t option) option -> unit
  [@@deprecated "[since 2024-10] Use [Log.Global.Transform.add] instead"]

  val get_transform : unit -> (Message_event.t -> Message_event.t option) option
  [@@deprecated
    "[since 2024-10] Getting the transform is not supported but transforms may cleared \
     with [Log.Global.clear_transforms] or added to with [Log.Global.Transform.add]"]

  val add_tags : tags:(string * string) list -> unit
  val would_log : Level.t option -> bool

  (** Sets the global log level via a flag, if provided.

      If [default] is not provided, the existing log level will be unchanged if the flag
      is not provided. *)
  val set_level_via_param : ?default:Level.t -> unit -> unit Command.Param.t

  (** Functions that operate on a given log. In this case they operate on a single log
      global to the module. *)

  val raw
    :  ?time:Time_float.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val info
    :  ?time:Time_float.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val error
    :  ?time:Time_float.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val debug
    :  ?time:Time_float.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val flushed : unit -> unit Deferred.t

  (** Generalized printf-style logging. *)
  val printf
    :  ?level:Level.t
    -> ?time:Time_float.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  (** Sexp logging for messages at each log level or raw (no level) messages. Raw messages
      still include a timestamp *)

  val raw_s : ?time:Time_float.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val info_s : ?time:Time_float.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val error_s : ?time:Time_float.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val debug_s : ?time:Time_float.t -> ?tags:(string * string) list -> Sexp.t -> unit

  (** Generalized sexp-style logging. *)
  val sexp
    :  ?level:Level.t
    -> ?time:Time_float.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> unit

  (** Log a string directly. *)
  val string
    :  ?level:Level.t
    -> ?time:Time_float.t
    -> ?tags:(string * string) list
    -> string
    -> unit

  val structured_message
    :  ?level:Level.t
    -> ?time:Time_float.t
    -> ?tags:(string * string) list
    -> Message_data.t
    -> Message_source.t
    -> unit

  (** Log a pre-created message. *)
  val message : Message.t -> unit

  val message_event : Message_event.t -> unit

  (** [surround t message f] logs [message] and a UUID once before calling [f] and again
      after [f] returns or raises. If [f] raises, the second message will include the
      exception, and [surround] itself will re-raise the exception tagged with [message].
      [on_subsequent_errors] is passed to the internal monitor as [rest] argument. As
      usual, the logging happens only if [level] exceeds the minimum level of [t]. *)
  val surround_s
    :  on_subsequent_errors:[ `Call of exn -> unit | `Log | `Raise ]
    -> ?level:Level.t
    -> ?time:Time_float.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> (unit -> 'a Deferred.t)
    -> 'a Deferred.t

  val surroundf
    :  on_subsequent_errors:[ `Call of exn -> unit | `Log | `Raise ]
    -> ?level:Level.t
    -> ?time:Time_float.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, (unit -> 'b Deferred.t) -> 'b Deferred.t) format4
    -> 'a

  module For_testing : sig
    (** Change the global log so that it
        1. Writes synchronously.
        2. By default only prints the bodies of messages to stdout, discarding any
           information about tags, levels, or timestamps. Use optional time, tags and
           level arguments to print required metadata information.

        [map_output] can be used to transform messages before they make it to stdout; by
        default it is [Fn.id].

        (This is equivalent to:
        [Log.Global.set_output [ Log.For_testing.create_output ~map_output ~time ~tags ~level () ]]) *)
    val use_test_output
      :  ?map_output:(string -> string)
      -> ?time:[ `Keep | `Omit ]
      -> ?tags:[ `Keep | `Omit ]
      -> ?level:[ `Keep | `Omit ]
      -> unit
      -> unit
  end

  (** Used internally by other [Async] libraries.

      There are two locations in [Async], [Async_command] and
      [Async_unix.Shutdown.shutdown_on_unhandled_exn], that have logic to catch unhandled
      exceptions and log them before exiting. Normally, they write these shutdown logs to
      stderr, but sometimes it's desirable to additionally write to select log outputs.

      [log_error] is used by the two locations to log to these outputs at shutdown.
      [register_error_output_name] allows downstream libraries to opt their outputs into
      shutdown logging.

      [Async_command] wraps its command's [main] function in a [try_with], and calls
      [log_error] on error.

      [shutdown_on_unhandled_exn], which is usually used with programs that directly call
      [Scheduler.go], catches exceptions with [Monitor.detach_and_iter_errors]; caught
      exceptions are sent to a mutable hook that [Async_log.assign_top_level_logs] sets
      with [Shutdown.set_shutdown_on_unhandled_exn_logger]. *)
  module For_async_shutdown : sig
    val register_error_output_name : Output_name.t -> unit
    [@@alert
      private_async_log_function
        "This function is meant for the specific use case of logging from \
         [Async_command] and [Async_unix] at shutdown without double-writing to stderr. \
         Please speak to async log devs if you need to use it for something else."]

    val log_error
      :  ?time:Time_float.t
      -> ?tags:(string * string) list
      -> ('a, unit, string, unit) format4
      -> 'a
    [@@alert
      private_async_log_function
        "This function is meant for the specific use case of logging from \
         [Async_command] and [Async_unix] at shutdown without double-writing to stderr. \
         Please speak to async log devs if you need to use it for something else."]
  end
end

module type Global = sig
  module type S = S

  (** This function can be called to generate logging modules with the [Log.t] lazily
      instantiated, and prepopulated in the arguments. *)
  module Make () : S

  (** This module provides functions like [Global.sexp] which logs without needing to
      provide a [Log.t]. At this point, it's recommended to use [ppx_log] instead. *)
  include S
end
