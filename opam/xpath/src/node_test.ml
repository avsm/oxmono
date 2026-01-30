open! Core

type t =
  | Name_test of Name_test.t
  | Node_type of Node_type.t
  | Processing_instruction of String_literal.t
[@@deriving sexp_of, compare, variants]

let to_string = function
  | Name_test name_test -> Name_test.to_string name_test
  | Node_type node_type -> [%string "%{node_type#Node_type}()"]
  | Processing_instruction literal ->
    [%string "processing-instruction(%{String_literal.to_quoted_string literal})"]
;;
