open! Core

type t =
  { prefix : string option
  ; name : string
  }
[@@deriving compare, to_string, sexp_of]

include Comparable.S_plain with type t := t
