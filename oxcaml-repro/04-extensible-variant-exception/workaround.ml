(* The only way through today: define the matching function at legacy
   mode and assert it portable. It touches no shared state, so the
   assertion is sound, but it cannot be checked. *)
type err = ..
type context = { steps : string list }
exception Io of err * context

let is_io : exn -> bool =
  Obj.magic_portable (function
    | Io _ -> true
    | _ -> false)

let use : (exn -> bool) @ portable = is_io
let () = ignore use
