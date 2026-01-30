open! Core
open! Async_kernel
open! Import

type t [@@deriving sexp_of]

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

val create_unbuffered
  :  ?finalize:(unit -> unit Deferred.t)
  -> flush:(unit -> unit Deferred.t)
  -> (Message_event.t -> unit)
  -> t

val empty : t
val write : t -> Message_event.t -> unit
val rotate : t -> unit Deferred.t
val flush : t -> unit Deferred.t
val filter_to_level : t -> level:Level.t -> t
val transform_message : t -> f:(Message_event.t -> Message_event.t) -> t

(** Provides a simple stderr output. This is equivalent to [Async_log.Output.stderr],
    which uses [Async_unix.Writer.stderr], unless [Async_log] is not linked in; in this
    case, this uses [Core.prerr_endline]. This is intended to be a sensible default for
    global logging that works out of the box in both js_of_ocaml apps and unix apps. *)
val stderr : t lazy_t

module Private : sig
  (** If a background error occurs, [background_error] becomes determined, log processing
      stops and further calls to [flush] will hang. *)
  val buffered_background_error
    :  t
    -> [ `Output_is_unbuffered | `Error of exn Deferred.t ]

  val set_async_stderr_output : t lazy_t -> here:Source_code_position.t -> unit
  val buffered_batch_size : int
end

module For_testing : sig
  val create
    :  ?map_output:(string -> string)
    -> ?time:[ `Keep | `Omit ]
    -> ?tags:[ `Keep | `Omit ]
    -> ?level:[ `Keep | `Omit ]
    -> unit
    -> t

  val is_async_stderr_output_set : unit -> bool
end
