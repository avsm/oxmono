(* Keep low-level Eio operations in the client layer as useful as the
   higher-level Path helpers.  [Eio.Exn.reraise_with_context] must receive the
   raw backtrace before doing any other work in the handler. *)
let with_context label (f @ local) =
  try f ()
  with Eio.Io _ as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context exn bt "%s" label
