exception Closed

let run ~sw request =
  let ready, ready_u = Eio.Promise.create () in
  let finished, finished_u = Eio.Promise.create () in
  let scope = ref None in
  Eio.Fiber.fork ~sw (fun () ->
    Fun.protect ~finally:(fun () ->
        scope := None;
        Eio.Promise.resolve finished_u ()) (fun () ->
      try
        Eio.Switch.run (fun sw ->
          scope := Some sw;
          let response = request ~sw in
          Eio.Promise.resolve ready_u (Ok response);
          Eio.Fiber.await_cancel ())
      with
      | Closed -> ()
      | ex ->
          if Eio.Promise.is_resolved ready then raise ex
          else Eio.Promise.resolve ready_u (Error ex)));
  let close () =
    Eio.Cancel.protect (fun () ->
      Option.iter (fun sw -> Eio.Switch.fail sw Closed) !scope;
      Eio.Promise.await finished)
  in
  match Eio.Promise.await_exn ready with
  | response -> response, close
  | exception ex -> close (); raise ex
