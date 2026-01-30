open! Core

type t =
  | Comment
  | Text
  | Processing_instruction
  | Node
[@@deriving sexp_of, compare, enumerate, variants, string ~capitalize:"kebab-case"]
