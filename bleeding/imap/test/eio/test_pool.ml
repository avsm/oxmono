let ok = function
  | Ok value -> value
  | Error error -> failwith (Imap_eio.Client.error_to_string error)

let connection ~sw ~name replies =
  let flow=Eio_mock.Flow.make name in
  Eio_mock.Flow.on_read flow ([
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
  ] @ replies);
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  Imap_eio.Client.of_flow ~sw ~auth flow

let test_bounded_reuse () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let connections=ref 0 and active=ref 0 and peak=ref 0 in
  let connect ~sw =
    incr connections;
    connection ~sw ~name:"pooled" [] in
  let pool=Imap_eio.Pool.create ~sw ~max_connections:1 ~connect in
  let borrow () = ok (Imap_eio.Pool.use pool (fun client ->
    if not (Imap_eio.Client.is_open client) then
      failwith "pool lent a closed client";
    incr active;
    peak:=max !peak !active;
    Eio.Fiber.yield ();
    decr active;
    Ok ())) in
  Eio.Fiber.both borrow borrow;
  if !peak<>1 || !connections<>1 then
    failwith "pool exceeded capacity or failed to reuse connection";
  ok (Imap_eio.Pool.use pool (fun client ->
    Imap_eio.Client.close client;
    Ok ()));
  ok (Imap_eio.Pool.use pool (fun _ -> Ok ()));
  if !connections<>2 then failwith "closed connection was reused";
  if Imap_eio.Pool.max_connections pool<>1 then
    failwith "pool capacity was lost"

let test_rejection_and_uncertainty () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let connections=ref 0 in
  let connect ~sw =
    incr connections;
    let replies=if !connections=1 then [
      `Return "A00000004 NO denied\r\n";
      `Return "* LIST () \"/\" INBOX\r\nA00000005 OK listed\r\n";
    ] else [] in
    connection ~sw ~name:"pooled-rejection" replies in
  let pool=Imap_eio.Pool.create ~sw ~max_connections:1 ~connect in
  (match Imap_eio.Pool.use pool (fun client ->
    Imap_eio.Client.list client ~pattern:"INBOX" ()) with
   | Error (Imap_eio.Error.Rejected _) -> ()
   | _ -> failwith "tagged NO was not returned");
  let rows=ok (Imap_eio.Pool.use pool (fun client ->
    Imap_eio.Client.list client ~pattern:"INBOX" ())) in
  if rows=[] || !connections<>1 then
    failwith "tagged rejection incorrectly retired connection";
  (match Imap_eio.Pool.use pool (fun _ ->
    Error (Imap_eio.Error.Uncertain "unknown outcome")) with
   | Error (Imap_eio.Error.Uncertain _) -> ()
   | _ -> failwith "uncertain outcome was lost");
  ok (Imap_eio.Pool.use pool (fun _ -> Ok ()));
  if !connections<>2 then failwith "uncertain connection was reused"

let test_failed_connect_and_callback () =
  Eio_mock.Backend.run @@ fun () ->
  let escaped=ref None in
  Eio.Switch.run (fun sw ->
    let attempts=ref 0 in
    let connect ~sw =
      incr attempts;
      if !attempts=1 then Error Imap_eio.Error.Closed
      else connection ~sw ~name:"pool-reconnect" [] in
    let pool=Imap_eio.Pool.create ~sw ~max_connections:1 ~connect in
    escaped:=Some pool;
    (match Imap_eio.Pool.use pool (fun _ -> Ok ()) with
     | Error Imap_eio.Error.Closed -> ()
     | _ -> failwith "failed allocation was not returned");
    ok (Imap_eio.Pool.use pool (fun _ -> Ok ()));
    if !attempts<>2 then failwith "failed allocation consumed capacity";
    (try
       ignore (Imap_eio.Pool.use pool (fun _ -> raise Exit));
       failwith "callback exception was swallowed"
     with Exit -> ());
    ok (Imap_eio.Pool.use pool (fun _ -> Ok ()));
    if !attempts<>3 then failwith "throwing callback left client reusable");
  (match !escaped with
   | Some pool ->
       (match Imap_eio.Pool.use pool (fun _ -> Ok ()) with
        | Error Imap_eio.Error.Closed -> ()
        | _ -> failwith "pool outlived its owning switch")
   | None -> failwith "pool was never created")

let () =
  test_bounded_reuse ();
  test_rejection_and_uncertainty ();
  test_failed_connect_and_callback ()
