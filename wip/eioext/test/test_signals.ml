let seen = Atomic.make 0

let () =
  let before = Sys.Safe.signal Sys.sigusr1 Sys.Signal_default in
  Sys.Safe.set_signal Sys.sigusr1 before;
  Eioext.Signals.with_handlers
    [ (Sys.sigusr1, Sys.Signal_handle (fun _ -> Atomic.incr seen)) ]
    (fun () ->
      Unix.kill (Unix.getpid ()) Sys.sigusr1;
      Unix.sleepf 0.05;
      assert (Atomic.get seen = 1));
  let after = Sys.Safe.signal Sys.sigusr1 Sys.Signal_default in
  assert (after = before);
  print_endline "signals: ok"
