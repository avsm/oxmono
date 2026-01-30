open! Core

type t =
  | Any
  | Any_in_namespace of { prefix : string }
  | Name of Qualified_name.t
[@@deriving sexp_of, compare]

let to_string = function
  | Any -> "*"
  | Any_in_namespace { prefix } -> [%string "%{prefix}:*"]
  | Name name -> Qualified_name.to_string name
;;
