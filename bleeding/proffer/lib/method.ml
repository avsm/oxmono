module M = Httpz.Method

type t = M.t

let to_string = M.to_string
let equal (a : t) (b : t) = a = b
