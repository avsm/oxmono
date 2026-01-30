open! Core
open! Import

type machine_readable =
  [ `Sexp
  | `Sexp_hum
  | `Bin_prot
  ]
[@@deriving bin_io, enumerate, sexp]

type t =
  [ machine_readable
  | `Text
  ]
[@@deriving bin_io, enumerate, sexp]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end
end
