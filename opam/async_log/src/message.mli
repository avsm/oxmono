open! Core
open! Import
include module type of Async_log_kernel.Message

type t = Async_log_kernel.Message.t [@@deriving sexp_of]

val to_write_only_text : ?zone:Time_float.Zone.t -> t -> string

module Stable : sig
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end
end
