open! Core

type t = private string [@@deriving sexp_of, compare, string]

val create_exn : string -> t
val to_quoted_string : t -> string
