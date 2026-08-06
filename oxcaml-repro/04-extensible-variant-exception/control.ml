(* With payloads that cross (a string), matching in portable code is
   fine. *)
exception A of string

let is_a : (exn -> bool) @ portable = function
  | A _ -> true
  | _ -> false

let () = assert (is_a (A "x"))
