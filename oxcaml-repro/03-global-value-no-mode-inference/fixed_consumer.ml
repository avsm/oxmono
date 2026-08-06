let use : (unit -> int) @ portable = fun () -> Fixed.get Fixed.v
let () = ignore (use ())
