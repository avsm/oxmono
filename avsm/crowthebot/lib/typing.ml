type t = { start : unit -> unit; stop : unit -> unit }

let start t = t.start ()
let stop t = t.stop ()

let with_session ~clock ~room ~event ~set f =
  Eio.Switch.run @@ fun sw ->
  let accepted, accept = Eio.Promise.create () in
  let stopping, request_stop = Eio.Promise.create () in
  let finished, finish = Eio.Promise.create () in
  let initial, initialized = Eio.Promise.create () in
  let started = ref false and stopped = ref false in
  let update typing =
    try
      Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 5.) (fun () ->
          set typing)
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
        Diagnostics.Log.warn (fun m ->
            m "Typing notification failed room=%S event=%S typing=%b error=%s"
              room event typing (Diagnostics.error exn))
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve finish ())
        (fun () ->
          Eio.Fiber.first
            (fun () ->
              Eio.Promise.await accepted;
              Fun.protect
                ~finally:(fun () -> Eio.Promise.resolve initialized ())
                (fun () -> update true);
              let rec refresh () =
                Eio.Time.Mono.sleep clock 15.;
                update true;
                refresh ()
              in
              refresh ())
            (fun () -> Eio.Promise.await stopping));
      `Stop_daemon);
  let start () =
    if (not !started) && not !stopped then begin
      started := true;
      Eio.Promise.resolve accept ();
      Eio.Promise.await initial
    end
  in
  let stop () =
    if not !stopped then begin
      stopped := true;
      Eio.Cancel.protect (fun () ->
          Eio.Promise.resolve request_stop ();
          Eio.Promise.await finished;
          if !started then update false)
    end
  in
  Fun.protect ~finally:stop (fun () -> f { start; stop })
