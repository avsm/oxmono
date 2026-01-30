open! Core

type t =
  { trail : Trail.t
  ; node : Node.t
  ; position : int
  ; size : int lazy_t
  }
[@@deriving sexp_of, fields ~getters]
