module C=Imap_eio.Client
module S=Imap_eio.Selected
module E=Imap_eio.Error
let ok=function Ok x -> x | Error error -> failwith (C.error_to_string error)
let with_client ?(caps="IMAP4rev1 UNSELECT") replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "lifecycle" in
  Eio_mock.Flow.on_read flow ([`Return "* PREAUTH ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK caps\r\n")] @ replies);
  let client=ok (C.of_flow ~sw flow) in
  f client
let test_noop () =
  with_client [`Return "* OK notice\r\nA00000002 OK noop\r\n"] (fun client ->
    match ok (C.noop client) with
    | [Imap.Response.Untagged (Imap.Response.Ok (_,"notice"))] -> ()
    | _ -> failwith "NOOP lost unsolicited response")
let test_selected_noop () =
  with_client [`Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 7] valid\r\n* OK [UIDNEXT 1] next\r\nA00000002 OK selected\r\n";
    `Return "* 1 EXISTS\r\n* 1 EXPUNGE\r\nA00000003 OK noop\r\n";
    `Return "A00000004 OK unselected\r\n"] (fun client ->
      let escaped=ref None in
      ok (C.with_mailbox client ~mode:`Read_only "INBOX" (fun selected ->
        escaped:=Some selected;
        (match ok (S.noop selected) with
         | [Imap.Response.Untagged (Imap.Response.Exists 1L);
            Imap.Response.Untagged (Imap.Response.Expunge 1L)] -> ()
         | _ -> failwith "selected NOOP reordered updates");
        Ok ()));
      match S.noop (Option.get !escaped) with
      | Error (E.State _) -> () | _ -> failwith "stale NOOP lease accepted")
let test_logout () =
  with_client [`Return "* BYE goodbye\r\nA00000002 OK logout\r\n"] (fun client ->
    ok (C.logout client);
    if C.is_open client then failwith "LOGOUT kept connection open";
    match C.noop client with Error E.Closed -> () | _ -> failwith "NOOP after LOGOUT")
let test_logout_failures () =
  List.iter (fun reply -> with_client [`Return reply] (fun client ->
    (match C.logout client with Error _ -> () | Ok () -> failwith "invalid LOGOUT accepted");
    if C.is_open client then failwith "failed LOGOUT kept connection open"))
    ["A00000002 OK missing bye\r\n";
     "* BYE goodbye\r\nA00000098 OK wrong tag\r\n";
     "+ unexpected\r\n";
     "A00000002 BAD rejected\r\n";
     "* BYE first\r\n* BYE second\r\nA00000002 OK logout\r\n"];
  with_client [`Return "* BYE goodbye\r\n";`Raise End_of_file] (fun client ->
    (match C.logout client with Error _ -> () | Ok () -> failwith "EOF accepted as completion");
    if C.is_open client then failwith "EOF kept connection open")
let test_cancelled_logout () =
  let entered,mark=Eio.Promise.create () in
  with_client [`Return "* BYE goodbye\r\n";
    `Run (fun () -> Eio.Promise.resolve mark (); Eio.Fiber.await_cancel ())] (fun client ->
    Eio.Fiber.first
      (fun () -> ignore (C.logout client))
      (fun () -> Eio.Promise.await entered);
    if C.is_open client then failwith "cancelled LOGOUT kept connection open")
let test_idle_notification_overflow () =
  let overflow="* OK [NOTIFICATIONOVERFLOW] dropped\r\n" in
  List.iter (fun before_continuation ->
    let idle=if before_continuation then overflow ^ "+ idling\r\n"
      else "+ idling\r\n" ^ overflow in
    with_client ~caps:"IMAP4rev1 UNSELECT IDLE NOTIFY"
      [`Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 7] valid\r\n* OK [UIDNEXT 1] next\r\nA00000002 OK selected\r\n";
       `Return "A00000003 OK notify enabled\r\n";
       `Return idle;
       `Return "* 1 EXISTS\r\nA00000004 OK idle complete\r\n";
       `Return "A00000005 OK noop\r\n";
       `Return "A00000006 OK unselected\r\n"] (fun client ->
      ok (C.with_mailbox client ~mode:`Read_only "INBOX" (fun selected ->
        ignore (ok (S.notify_set selected ~groups:[Imap.Command.Selected,
          [Imap.Command.Message_new;Imap.Command.Message_expunge]] ()));
        (match ok (S.wait_for_change selected) with
         | [Imap.Response.Untagged (Imap.Response.Ok
              (Some Imap.Response.Notificationoverflow,_));
            Imap.Response.Untagged (Imap.Response.Exists 1L)] -> ()
         | _ -> failwith "IDLE lost overflow or reordered completion updates");
        ignore (ok (S.noop selected));
        Ok ())))) [false;true]

let () = test_idle_notification_overflow (); test_noop (); test_selected_noop (); test_logout ();
  test_logout_failures (); test_cancelled_logout ()
