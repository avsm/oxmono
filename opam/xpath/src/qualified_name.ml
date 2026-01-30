open! Core

type t =
  { prefix : string option
  ; name : string
  }
[@@deriving compare]

let to_string { prefix; name } =
  match prefix with
  | None -> name
  | Some prefix -> [%string "%{prefix}:%{name}"]
;;

let sexp_of_t t = Sexp.Atom (to_string t)

include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving sexp_of, compare]
  end)
