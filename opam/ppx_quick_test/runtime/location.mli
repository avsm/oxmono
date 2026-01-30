open! Core

type t = Ocaml_common.Location.t =
  { loc_start : Lexing.position
  ; loc_end : Lexing.position
  ; loc_ghost : bool
  }
[@@deriving sexp_of]
