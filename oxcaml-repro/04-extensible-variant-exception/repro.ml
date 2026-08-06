(* An extension constructor is usable in portable code only when all of
   its payload types cross portability. An extensible type can never
   cross, so an exception carrying one cannot be matched or constructed
   in any portable function. Eio's [Exn.Io of err * context] with
   [type err = ..] hits this: every error-context helper needs an unsafe
   assertion (see workaround.ml). *)

type err = ..
type context = { steps : string list }
exception Io of err * context

let is_io : (exn -> bool) @ portable = function
  | Io _ -> true
  | _ -> false

let () = ignore is_io
