open! Core
open! Import

(** Describes both the level of a log and the level of a message sent to a log. There is
    an ordering to levels (`Debug < `Info < `Error), and a log set to a level will never
    display messages at a lower log level.

    Messages without a level are treated as `Info. *)
type t =
  [ `Debug
  | `Info (** default level *)
  | `Error
  ]
[@@deriving
  bin_io, compare ~localize, enumerate, equal ~localize, globalize, sexp, sexp_grammar]

include Stringable with type t := t

val arg : t Command.Spec.Arg_type.t
val as_or_more_verbose_than : log_level:t -> msg_level:t option -> bool

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, compare ~localize, sexp, stable_witness]
  end
end
