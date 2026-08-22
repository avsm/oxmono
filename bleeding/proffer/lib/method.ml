(* Methods are httpz's. Its parser accepts the seventeen it names and rejects
   anything else, so a method that reaches a handler is always one of them.
   There is no [Other] case to carry and no wire spelling to compare: a method
   this set lacks is added to httpz rather than modelled around here. *)

module M = Httpz.Method

type t = M.t

let to_string = M.to_string
let equal (a : t) (b : t) = a = b
