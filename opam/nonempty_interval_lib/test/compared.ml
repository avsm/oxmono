open! Core

type t =
  [ `Below
  | `Within
  | `Above
  | `Interval_is_empty
  ]
[@@deriving compare, sexp]
