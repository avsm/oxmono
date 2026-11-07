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

let rec bind (handler @ portable) (signals @ portable) =
  match signals with
  | [] -> []
  | signal :: rest -> (signal, handler) :: bind handler rest

let with_wait ~sw signals f =
  let rd, wr = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock rd;
  Unix.set_nonblock wr;
  Eio.Switch.on_release sw (fun () ->
      Unix.close rd;
      Unix.close wr);
  let handler =
    Sys.Signal_handle
      (fun _ ->
        try ignore (Unix.single_write_substring wr "s" 0 1)
        with Unix.Unix_error _ -> ())
  in
  let buffer = Bytes.create 64 in
  let rec drain () =
    match Unix.read rd buffer 0 (Bytes.length buffer) with
    | n when n = Bytes.length buffer -> drain ()
    | _ -> ()
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> ()
  in
  let wait () =
    Eio_unix.await_readable rd;
    drain ()
  in
  with_handlers (bind handler signals) (fun () -> f wait)
