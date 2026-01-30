open! Core

type t = string [@@deriving sexp_of, compare]

let create_exn value =
  let has_double_quotes = String.exists value ~f:(Char.equal '"') in
  let has_single_quotes = String.exists value ~f:(Char.equal '\'') in
  if has_double_quotes && has_single_quotes
  then
    raise_s
      [%message
        "invalid string literal, has both double and single quotes. Use the concat \
         function"
          (value : string)]
  else value
;;

let of_string = create_exn
let to_string t = t

let to_quoted_string t =
  let quote_character = if String.exists t ~f:(Char.equal '\'') then '"' else '\'' in
  [%string "%{quote_character#Char}%{t}%{quote_character#Char}"]
;;
