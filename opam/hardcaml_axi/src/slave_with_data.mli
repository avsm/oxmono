open! Base
open! Hardcaml

type ('slave, 'data) t =
  { slave : 'slave
  ; data : 'data
  }
[@@deriving sexp_of]
