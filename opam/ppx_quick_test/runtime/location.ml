open! Core

type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }

and t = Ocaml_common.Location.t =
  { loc_start : position
  ; loc_end : position
  ; loc_ghost : bool
  }
[@@deriving sexp_of]
