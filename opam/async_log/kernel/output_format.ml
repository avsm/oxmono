open! Core
open! Import

module Stable = struct
  module V1 = struct
    type machine_readable =
      [ `Sexp
      | `Sexp_hum
      | `Bin_prot
      ]
    [@@deriving bin_io, sexp]

    type t =
      [ machine_readable
      | `Text
      ]
    [@@deriving bin_io, sexp]
  end
end

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
