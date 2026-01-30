(** A library for general logging.

    Although this module is fully Async-safe it exposes almost no Deferreds. This is
    partially a design choice to minimize the impact of logging in code, and partially the
    result of organic design (i.e., older versions of this interface did the same thing).

    A (limited) [Blocking] module is supplied to accommodate the portion of a program that
    runs outside of Async. *)

open! Core
open! Async_kernel
open! Import

type t [@@deriving sexp_of]

(** Sets the log level via a flag, if provided.

    If [default] is not provided, the existing log level will be unchanged if the flag is
    not provided. *)
val set_level_via_param : ?default:Level.t -> t -> unit Command.Param.t

(** Messages sent at a level less than the current level will not be output. *)
val set_level : t -> Level.t -> unit

(** Returns the last level passed to [set_level], which will be the log level checked as a
    threshold against the level of the next message sent. *)
val level : t -> Level.t

(** Changes the output type of the log, which can be useful when daemonizing. The new
    output type will be applied to all subsequent messages. *)
val set_output : t -> Output.t list -> unit

val get_output : t -> Output.t list

(** Changes the time source of the log, which controls the default timestamp on messages. *)
val get_time_source : t -> Synchronous_time_source.t

val set_time_source : t -> [> read ] Synchronous_time_source.T1.t -> unit

(** A [transform] is a function to be called on each log invocation *synchronously* that
    can be used to change things about the message at the time that they were written.

    The transform function *will not* be called if the initial message is of a level that
    is more verbose than the level of the [Log.t].

    The transform function *will* be called if even if there are no log outputs. *)
module Transform : sig
  type log := t
  type t

  (** Adds a transform to the end of the list of transforms applied to messages sent
      through the log.

      If the function raises, the exception will be raised immediately to the monitor that
      the [[%log.t]] statement is in, and the message will not be logged.

      [append t f; prepend t g] will cause a message to first have [g] applied to it, then
      [f]. *)
  val append : log -> (Message_event.t -> Message_event.t) -> unit

  (** Adds a transform to the start of the list of transforms applied to messages sent
      through the log.

      If the function raises, the exception will be raised immediately to the monitor that
      the [[%log.t]] statement is in, and the message will not be logged.

      [append t f; prepend t g] will cause a message to first have [g] applied to it, then
      [f]. *)
  val prepend : log -> (Message_event.t -> Message_event.t) -> unit

  (** Like {!append} except that a {!t} is returned which one can call {!remove_exn} with.
      The function passed here may also return None to prevent messages from making it to
      outputs or later transforms. *)
  val append' : log -> (Message_event.t -> Message_event.t option) -> t

  (** Like {!prepend} except that a {!t} is returned which one can call {!remove_exn}
      with. The function passed here may also return None to prevent messages from making
      it to outputs or later transforms. *)
  val prepend' : log -> (Message_event.t -> Message_event.t option) -> t

  (** [remove_exn] can be later used to remove a transform previously added to a log. This
      must be called with the same (i.e. [phys_equal]) [Log.t] as the one the transform
      was added to. *)
  val remove_exn : log -> t -> unit
end

val has_transform : t -> bool
val clear_transforms : t -> unit

val set_transform : t -> (Message_event.t -> Message_event.t option) option -> unit
[@@deprecated "[since 2024-10] Use [Log.Transform.add] instead"]

val get_transform : t -> (Message_event.t -> Message_event.t option) option
[@@deprecated
  "[since 2024-10] Getting the transform is not supported but transforms may cleared \
   with [Log.clear_transforms] or added to with [Log.Transform.add]"]

(** One common transformation on message events is tagging every remaining message with a
    list of fixed tags.

    [add_tags t ~tags] is shorthand for
    [Transform.prepend t (Message_event.add_tags ~tags)]. ([prepend] in this case means
    these tags are added before any other transformation is applied, which we believe is
    more often wanted, e.g. if a later transform flattens the message and tags into a
    single rendered string.)

    This is not idempotent - if you add the same tags twice, they'll be included twice in
    the resultant message. *)
val add_tags : t -> tags:(string * string) list -> unit

(** Like {!add_tags} except that a {!Transform.t} is returned which one can call
    {!Transform.remove_exn} with. *)
val add_tags' : t -> tags:(string * string) list -> Transform.t

(** If [`Raise] is given, then background errors raised by logging will be raised to the
    monitor that was in scope when [create] was called. Errors can be redirected anywhere
    by providing [`Call f]. *)
val get_on_error : t -> [ `Raise | `Call of Error.t -> unit ]

val set_on_error : t -> [ `Raise | `Call of Error.t -> unit ] -> unit

(** Any call that writes to a log after [close] is called will raise. *)
val close : t -> unit Deferred.t

(** Returns true if [close] has been called. *)
val is_closed : t -> bool

(** Returns a [Deferred.t] that is fulfilled when the last message delivered to [t] before
    the call to [flushed] is out the door. *)
val flushed : t -> unit Deferred.t

(** Creates a new log. See [set_level], [set_on_error], [set_output], [set_time_source],
    and [set_transform] for more. *)
val create
  :  level:Level.t
  -> output:Output.t list
  -> on_error:[ `Raise | `Call of Error.t -> unit ]
  -> ?time_source:[> read ] Synchronous_time_source.T1.t
  -> ?transform:(Message_event.t -> Message_event.t)
  -> unit
  -> t

(** Log that drops messages sent to it, as if it wrote to /dev/null *)
val create_null : unit -> t

(** Make a copy of this log, with potentially some settings changed, or potentially extra
    tags added to each line. *)
val copy
  :  ?level:Level.t
  -> ?on_error:[ `Call of Error.t -> unit | `Raise ]
  -> ?output:Output.t list
  -> ?extra_tags:(string * string) list
  -> t
  -> t

module Control_event : sig
  type t = Set_level of Level.t [@@deriving globalize, sexp_of]
end

(** A bus that may be subscribed to to get control events which happen to this [t].

    (In at least one case, this was used to watch for level changes and change a different
    [Log.t]'s level in a different process in response.)

    If you subscribe to this bus and your callback raises, the error will be ignored. It
    is recommended that you subscribe with [Bus.Subscribe] so that you may pass
    [~on_callback_raise] there. *)
val control_events : t -> (local_ Control_event.t -> unit) Bus.Read_only.t

(** Printf-like logging for messages at each log level or raw (no level) messages. Raw
    messages still include a timestamp. *)

val raw
  :  ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val debug
  :  ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val info
  :  ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val error
  :  ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** Generalized printf-style logging. *)
val printf
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** Sexp logging for messages at each log level or raw (no level) messages. Raw messages
    still include a timestamp *)

val raw_s : ?time:Time_float.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val info_s : ?time:Time_float.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val error_s : ?time:Time_float.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val debug_s : ?time:Time_float.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit

(** Generalized sexp-style logging. *)
val sexp
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> Sexp.t
  -> unit

(** Log a string directly. *)
val string
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> string
  -> unit

val structured_message
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> Message_data.t
  -> Message_source.t
  -> unit

(** Log a pre-created message. *)
val message : t -> Message.t -> unit

val message_event : t -> Message_event.t -> unit

(** [surround t message f] logs [message] and a UUID once before calling [f] and again
    after [f] returns or raises. If [f] raises, the second message will include the
    exception, and [surround] itself will re-raise the exception tagged with [message].
    [on_subsequent_errors] is passed to the internal monitor as [rest] argument. As usual,
    the logging happens only if [level] exceeds the minimum level of [t]. *)
val surround_s
  :  on_subsequent_errors:[ `Call of exn -> unit | `Log | `Raise ]
  -> ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> Sexp.t
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t

val surroundf
  :  on_subsequent_errors:[ `Call of exn -> unit | `Log | `Raise ]
  -> ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, (unit -> 'b Deferred.t) -> 'b Deferred.t) format4
  -> 'a

(** [would_log] returns true if a message at the given log level would be logged if sent
    immediately.

    This will return [false] if there are no outputs for the log, unless there is a
    [transform] set. *)
val would_log : t -> Level.t option -> bool

module For_testing : sig
  (** [create_output ~map_output] creates a [Log.Output.t] which will print only
      [Message.message] to stdout, discarding any information about tags, levels, or
      timestamps.

      [map_output] will be applied to each string before printing, and is expected to be
      used to replace portions of output or identify which log a message came from.

      This function is best used with existing Log.ts, e.g. to replace Log.Global's
      outputs in expect tests. If you just want a full Log.t, see [create] below. *)
  val create_output
    :  ?map_output:(string -> string)
    -> ?time:[ `Keep | `Omit ]
    -> ?tags:[ `Keep | `Omit ]
    -> ?level:[ `Keep | `Omit ]
    -> unit
    -> Output.t

  (** [create_log ~map_output level] creates a [Log.t] with its level set to [level] using
      the output returned by [create_output], and an [on_error] value of `Raise. *)

  val create
    :  ?map_output:(string -> string)
    -> ?time:[ `Keep | `Omit ]
    -> ?tags:[ `Keep | `Omit ]
    -> ?level:[ `Keep | `Omit ]
    -> Level.t
    -> t

  val transform : t -> Message_event.t -> Message_event.t option
end

(**/**)

module Private : sig
  val push_message_event : t -> Message_event.t -> unit
  val set_async_trace_hook : (unit -> Univ.t option) -> unit

  val set_level_via_param_lazy
    :  t Lazy.t
    -> default:Level.t option
    -> unit Command.Param.t

  val all_live_logs_flushed : unit -> unit Deferred.t

  (** A named output is just a regular [Log.Output.t] with a name, but is not affected by
      [Log.set_output] and [Log.get_output]. It can be used to register outputs that you
      don't want the users to change accidentally.

      Use [set_named_output] and [remove_named_output] to modify the named outputs
      explicitly. *)
  val set_named_output : t -> Output_name.t -> Output.t -> unit

  val get_named_output : t -> Output_name.t -> Output.t option
  val remove_named_output : t -> Output_name.t -> unit
  val with_temporary_outputs : t -> Output.t list -> f:(unit -> unit) -> unit

  module For_testing : sig
    val get_named_outputs : t -> Output.t Output_name.Map.t
    val update_named_outputs : t -> Output.t Output_name.Map.t -> unit
  end
end
