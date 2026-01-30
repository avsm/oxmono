open! Core
open! Async_kernel
open! Import

module T1 : sig
  type 'time t [@@deriving sexp_of]
end

type t = Time_float.t T1.t

val create
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> Sexp_or_string.t
  -> t

val time : t -> Time_float.t
val level : t -> Level.t option
val set_level : t -> Level.t option -> t
val message : t -> string
val raw_message : t -> [ `String of string | `Sexp of Sexp.t ]
val tags : t -> (string * string) list
val add_tags : t -> (string * string) list -> t
val to_write_only_text : t -> Time_float.Zone.t -> string

module For_testing : sig
  val to_string
    :  t
    -> Time_float.Zone.t
    -> time:[ `Keep | `Omit ]
    -> tags:[ `Keep | `Omit ]
    -> level:[ `Keep | `Omit ]
    -> string
end

module Stable : sig
  module T1 : sig
    module V2 : sig
      type 'time t = 'time T1.t [@@deriving bin_io, sexp]
    end
  end
end
