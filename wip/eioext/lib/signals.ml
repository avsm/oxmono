let rec install (previous @ portable) (handlers @ portable) =
  match handlers with
  | [] -> previous
  | (signal, behaviour) :: rest ->
      install ((signal, Sys.Safe.signal signal behaviour) :: previous) rest

let rec restore (previous @ portable) =
  match previous with
  | [] -> ()
  | (signal, behaviour) :: rest ->
      Sys.Safe.set_signal signal behaviour;
      restore rest

let with_handlers (handlers @ portable) f =
  let previous = install [] handlers in
  Fun.protect ~finally:(fun () -> restore previous) f
