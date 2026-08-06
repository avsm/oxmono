(* version.ml - HTTP version *)

type t =
  | Http_1_0
  | Http_1_1

let to_string = function
  | Http_1_0 -> "HTTP/1.0"
  | Http_1_1 -> "HTTP/1.1"
;;

let pp fmt t = Stdlib.Format.fprintf fmt "%s" (to_string t)
