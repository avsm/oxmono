open! Core
open! Import

type t =
  [ `Sexp of Sexp.t
  | `String of string
  ]
[@@deriving sexp_of]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp]

    val to_string : t -> string
  end
end
