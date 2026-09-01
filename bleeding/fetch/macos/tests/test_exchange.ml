let check message condition = if not condition then failwith message

let () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let released = ref 0 and stopped = ref false in
  let response, close = Exchange.run ~sw (fun ~sw ->
      Eio.Switch.on_release sw (fun () -> incr released);
      Eio.Fiber.fork ~sw (fun () ->
        Fun.protect ~finally:(fun () -> stopped := true) Eio.Fiber.await_cancel);
      "response") in
  check "response is returned before release" (response = "response" && !released = 0);
  close ();
  check "close cancels workers and releases the exchange" (!stopped && !released = 1);
  close ();
  check "close is idempotent" (!released = 1);
  let error = Failure "request failed" in
  (match Exchange.run ~sw (fun ~sw ->
     Eio.Switch.on_release sw (fun () -> incr released);
     raise error) with
   | _ -> failwith "request failure swallowed"
   | exception ex -> check "request failure preserved" (ex == error));
  check "failed request releases its scope" (!released = 2);
  let started, started_u = Eio.Promise.create () in
  Eio.Fiber.first
    (fun () -> ignore (Exchange.run ~sw (fun ~sw ->
       Eio.Switch.on_release sw (fun () -> incr released);
       Eio.Promise.resolve started_u ();
       Eio.Fiber.await_cancel ())))
    (fun () -> Eio.Promise.await started);
  check "cancelled request releases its scope" (!released = 3);
  print_endline "macOS exchange lifecycle checks passed"
