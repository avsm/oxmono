open! Core
open! Async_kernel
open! Import

(** Async programs often have a non-Async portion that runs before the scheduler begins to
    capture command line options, do setup, read configs, etc. This module provides
    limited global logging functions to be used during that period. Calling these
    functions after the scheduler has started will raise an exception. They otherwise
    behave similarly to the logging functions in the Async world.

    There are more detailed comments for the API below near the non-blocking signatures. *)

module Output : sig
  type t

  val stdout : t
  val stderr : t

  (** See {!Log_extended} for syslog and colorized console output. *)

  val create : (Message.t -> unit) -> t
end

val level : unit -> Level.t
val set_level : Level.t -> unit
val set_output : Output.t -> unit
val set_time_source : Synchronous_time_source.t -> unit
val set_transform : (Message.t -> Message.t) option -> unit

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

val printf
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> ('a, unit, string, unit) format4
  -> 'a

val raw_s : ?time:Time_float.t -> ?tags:(string * string) list -> Sexp.t -> unit
val info_s : ?time:Time_float.t -> ?tags:(string * string) list -> Sexp.t -> unit
val error_s : ?time:Time_float.t -> ?tags:(string * string) list -> Sexp.t -> unit
val debug_s : ?time:Time_float.t -> ?tags:(string * string) list -> Sexp.t -> unit

val sexp
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> Sexp.t
  -> unit
