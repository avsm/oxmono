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

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let woken = ref 0 in
  Eioext.Signals.with_wait ~sw [ Sys.sigusr2 ] (fun wait ->
      Eio.Fiber.both
        (fun () ->
          wait ();
          incr woken;
          wait ();
          incr woken)
        (fun () ->
          Unix.kill (Unix.getpid ()) Sys.sigusr2;
          Eio.Fiber.yield ();
          while !woken < 1 do
            Eio.Fiber.yield ()
          done;
          Unix.kill (Unix.getpid ()) Sys.sigusr2;
          Unix.kill (Unix.getpid ()) Sys.sigusr2));
  assert (!woken = 2);
  assert (Sys.Safe.signal Sys.sigusr2 Sys.Signal_default = Sys.Signal_default);
  print_endline "signals: wait ok"
