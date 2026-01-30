open! Core

type t =
  | Name_test of Name_test.t
  | Node_type of Node_type.t
  | Processing_instruction of String_literal.t
[@@deriving sexp_of, compare, variants, to_string]
