open! Core

type t =
  | Any
  | Any_in_namespace of { prefix : string }
  | Name of Qualified_name.t
[@@deriving sexp_of, compare, to_string]
