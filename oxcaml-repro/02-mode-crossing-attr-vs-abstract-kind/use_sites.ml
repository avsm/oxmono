(* Without an explicit kind annotation the attribute is inert at use
   sites too: the global cannot be used from a portable function even
   though the declaration carries the attribute. *)
type t = { g : (int -> int) array }
[@@unsafe_allow_any_mode_crossing]

let global = { g = [| (fun x -> x) |] }
let use : (unit -> int) @ portable = fun () -> global.g.(0) 1
let () = ignore (use ())
