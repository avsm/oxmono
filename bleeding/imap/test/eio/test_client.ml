let fail e = failwith (Imap_eio.Client.error_to_string e)
let ok = function Ok x -> x | Error e -> fail e

let test_login_requires_tls () =
  let check mechanism =
    Eio_mock.Backend.run @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let flow=Eio_mock.Flow.make "login-tls-policy" in
    Eio_mock.Flow.on_read flow [
      `Return "* OK ready\r\n";
      `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
    ];
    let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
      ~mechanism () in
    match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Error (Imap_eio.Error.State "LOGIN requires TLS") -> ()
    | Error error -> failwith ("wrong LOGIN policy error: " ^
        Imap_eio.Client.error_to_string error)
    | Ok client ->
        Imap_eio.Client.close client;
        failwith "plaintext LOGIN was accepted" in
  check `Auto;
  check `Login

let test_wrong_uid () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "imap" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 2] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 1 FETCH (UID 999 BODY[] {3}\r\n";
    `Return "abc)\r\nA00000005 OK done\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client = ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let sink = Buffer.create 8 in
  let outcome = Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected ->
      Imap_eio.Selected.fetch_to selected ~uid:1L (Eio.Flow.buffer_sink sink)) in
  (match outcome with
   | Error e when String.length (Imap_eio.Client.error_to_string e) > 0 -> ()
   | Error _ -> failwith "empty protocol error"
   | Ok () -> failwith "wrong UID body was accepted");
  if Buffer.contents sink <> "abc" then failwith "provisional body bytes missing"

let test_cleanly_missing_uid_keeps_session () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "missing-uid" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
    `Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 2] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "A00000005 OK fetched\r\n";
    `Return "A00000006 OK unselected\r\n";
    `Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 2] next\r\nA00000007 OK [READ-ONLY] selected\r\n";
    `Return "* SEARCH\r\nA00000008 OK searched\r\n";
    `Return "A00000009 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  (match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.fetch_to selected ~uid:1L
      (Eio.Flow.buffer_sink (Buffer.create 8))) with
   | Error (Imap_eio.Error.Missing_uid 1L) -> ()
   | Error error -> failwith ("wrong missing-UID result: " ^
       Imap_eio.Client.error_to_string error)
   | Ok () -> failwith "absent UID was fetched");
  let uids=ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_search selected "ALL")) in
  if uids<>[] then failwith "missing-UID follow-up search was not empty";
  Imap_eio.Client.close client

let test_uidnotsticky_refuses_selected_callback () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "uidnotsticky" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 7] transient\r\n* OK [UIDNEXT 2] next\r\n* NO [UIDNOTSTICKY] Non-persistent UIDs\r\nA00000004 OK [READ-ONLY] selected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let called=ref false in
  (match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun _ -> called:=true; Ok ()) with
   | Error (Imap_eio.Error.State reason) when
       String.starts_with ~prefix:"UIDNOTSTICKY" reason -> ()
   | Error error -> failwith ("wrong UIDNOTSTICKY refusal: " ^
       Imap_eio.Client.error_to_string error)
   | Ok () -> failwith "UIDNOTSTICKY mailbox was accepted");
  if !called then failwith "callback ran for UIDNOTSTICKY mailbox";
  Imap_eio.Client.close client

let test_uncertain_append () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "append" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
    `Return "+ ready\r\n";
    `Raise End_of_file;
  ];
  let auth = Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client = ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  match Imap_eio.Client.append_flow client ~mailbox:"INBOX" ~length:3L
    (Eio.Flow.string_source "abc") with
  | Error (Imap_eio.Error.Uncertain _) -> ()
  | Error e -> failwith ("expected uncertain APPEND: " ^
      Imap_eio.Client.error_to_string e)
  | Ok () -> failwith "APPEND succeeded without tagged completion"

let test_cram_vector () =
  let auth = Imap_eio_core.Auth.password ~username:"tim"
    ~password:"tanstaaftanstaaf" ~mechanism:`Cram_md5 () in
  let answer = Imap_eio_core.Auth.cram_md5_response auth
    "<1896.697170952@postoffice.reston.mci.net>" in
  if answer <> "dGltIGI5MTNhNjAyYzdlZGE3YTQ5NWI0ZTZlNzMzNGQzODkw" then
    failwith "RFC 2195 CRAM-MD5 vector mismatch";
  let long_key = String.make 80 (Char.chr 0xaa) in
  let auth = Imap_eio_core.Auth.password ~username:"x" ~password:long_key () in
  let answer = Imap_eio_core.Auth.cram_md5_response auth
    "Test Using Larger Than Block-Size Key - Hash Key First"
    |> Base64.decode_exn in
  if answer <> "x 6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd" then
    failwith "RFC 2202 long-key HMAC-MD5 vector mismatch"

let test_cram_exchange () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "cram" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 AUTH=CRAM-MD5 LOGINDISABLED\r\nA00000001 OK done\r\n";
    `Return "+ PDE4OTYuNjk3MTcwOTUyQHBvc3RvZmZpY2UucmVzdG9uLm1jaS5uZXQ+\r\n";
    `Return "A00000002 OK authenticated\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"tim"
    ~password:"tanstaaftanstaaf" () in
  let client = ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  Imap_eio.Client.close client

let test_cram_rejection_no_fallback () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "cram-rejected" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 AUTH=CRAM-MD5\r\nA00000001 OK done\r\n";
    `Return "+ PDE4OTYuNjk3MTcwOTUyQHBvc3RvZmZpY2UucmVzdG9uLm1jaS5uZXQ+\r\n";
    `Return "A00000002 NO incorrect password\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"tim"
    ~password:"wrong" () in
  match Imap_eio.Client.of_flow ~sw ~auth flow with
  | Error (Imap_eio.Error.Rejected {text="authentication rejected"; _}) -> ()
  | Error e -> failwith ("wrong CRAM-MD5 rejection: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "accepted rejected CRAM-MD5 authentication"

let test_cram_not_advertised () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "no-cram" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"user"
    ~password:"pw" ~mechanism:`Cram_md5 () in
  match Imap_eio.Client.of_flow ~sw ~auth flow with
  | Error (Imap_eio.Error.Unsupported (Imap.Capability.Auth "CRAM-MD5")) -> ()
  | Error e -> failwith ("wrong missing mechanism error: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "accepted absent CRAM-MD5 mechanism"

let test_cram_bad_challenge () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "bad-cram" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 AUTH=CRAM-MD5\r\nA00000001 OK done\r\n";
    `Return "+ !!malformed!!\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"user"
    ~password:"pw" ~mechanism:`Cram_md5 () in
  match Imap_eio.Client.of_flow ~sw ~auth flow with
  | Error (Imap_eio.Error.Protocol _) -> ()
  | Error e -> failwith ("wrong malformed challenge error: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "accepted malformed CRAM-MD5 challenge"

let test_idle_fragmented () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "idle-fragmented" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT IDLE\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT IDLE\r\nA00000003 OK done\r\n";
    `Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 1] next\r\nA00000004 OK selected\r\n";
    `Return "+";
    `Return " idling\r\n* 1 EX";
    `Return "ISTS\r\n";
    `Return "A00000005 OK idle completed\r\n";
    `Return "A00000006 OK unselected\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client = ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let updates = ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only
    "INBOX" Imap_eio.Selected.wait_for_change) in
  (match updates with
   | [Imap.Response.Untagged (Imap.Response.Exists 1L)] -> ()
   | _ -> failwith "fragmented IDLE response lost");
  Imap_eio.Client.close client

let test_partial_fetch_never_completes_inventory () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "partial-fetch" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 2] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 1 FETCH (UID 1 FLAGS ())\r\nA00000005 OK [MESSAGELIMIT 1 1] partial\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client = ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let result = Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.fetch_metadata_range selected
      ~first:1L ~last:1L ~modseq:false) in
  match result with
  | Error (Imap_eio.Error.Limit _) -> ()
  | Error e -> failwith ("wrong partial FETCH error: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "partial FETCH was accepted as complete inventory"

let test_metadata_fetch_messagelimit_resume () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "metadata-messagelimit" in
  let caps="IMAP4rev1 UNSELECT MESSAGELIMIT=2" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 3 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 4] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 3 FETCH (UID 3 FLAGS ())\r\n* 2 FETCH (UID 2 FLAGS ())\r\nA00000005 OK [MESSAGELIMIT 2 2] partial\r\n";
    `Return "* 1 FETCH (UID 1 FLAGS ())\r\nA00000006 OK fetched\r\n";
    `Return "A00000007 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let rows=ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.fetch_metadata_range selected
      ~first:1L ~last:3L ~modseq:false)) in
  if List.map (fun (row:Imap.Response.fetch) -> row.uid) rows<>
      [Some 1L;Some 2L;Some 3L] then
    failwith "MESSAGELIMIT FETCH continuation lost metadata";
  Imap_eio.Client.close client

let test_metadata_fetch_missing_boundary () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "metadata-missing-boundary" in
  let caps="IMAP4rev1 MESSAGELIMIT=2" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000003 OK done\r\n");
    `Return "* 3 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 4] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 3 FETCH (UID 3 FLAGS ())\r\nA00000005 OK [MESSAGELIMIT 2] partial\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  (match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.fetch_metadata_range selected
      ~first:1L ~last:3L ~modseq:false) with
   | Error (Imap_eio.Error.Limit _) -> ()
   | Error error -> failwith ("missing FETCH boundary: " ^
       Imap_eio.Client.error_to_string error)
   | Ok _ -> failwith "partial FETCH without a UID boundary was accepted")

let test_changes_messagelimit_resume () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "changes-messagelimit" in
  let caps="IMAP4rev1 UNSELECT CONDSTORE MESSAGELIMIT=2" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000003 OK done\r\n");
    `Return "* 3 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 4] next\r\n* OK [HIGHESTMODSEQ 8] modseq\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 3 FETCH (UID 3 FLAGS (\\Seen) MODSEQ (7))\r\nA00000005 OK [MESSAGELIMIT 2 2] partial\r\n";
    `Return "* 1 FETCH (UID 1 FLAGS () MODSEQ (8))\r\nA00000006 OK fetched\r\n";
    `Return "A00000007 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let since=match Imap.Proto.Modseq.of_int64 5L with
    | Ok value -> value | Error message -> failwith message in
  let rows=ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.fetch_changes_range selected
      ~first:1L ~last:3L ~since)) in
  if List.map (fun (row:Imap.Response.fetch) -> row.uid) rows<>
      [Some 1L;Some 3L] then
    failwith "MESSAGELIMIT CHANGEDSINCE continuation lost changes";
  Imap_eio.Client.close client

let test_changes_messagelimit_missing_boundary () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "changes-missing-boundary" in
  let caps="IMAP4rev1 CONDSTORE MESSAGELIMIT=2" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000003 OK done\r\n");
    `Return "* 3 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 4] next\r\n* OK [HIGHESTMODSEQ 8] modseq\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 3 FETCH (UID 3 FLAGS () MODSEQ (7))\r\nA00000005 OK [MESSAGELIMIT 2] partial\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let since=match Imap.Proto.Modseq.of_int64 5L with
    | Ok value -> value | Error message -> failwith message in
  match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.fetch_changes_range selected
      ~first:1L ~last:3L ~since) with
  | Error (Imap_eio.Error.Limit _) -> ()
  | Error error -> failwith ("wrong CHANGEDSINCE boundary error: " ^
      Imap_eio.Client.error_to_string error)
  | Ok _ -> failwith "partial CHANGEDSINCE without boundary was accepted"

let test_typed_envelope_fetch () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "typed-envelope-fetch" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
    `Return "* 2 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 3] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 2 FETCH (UID 2 ENVELOPE (\"Sat, 26 Sep 2026 00:00:00 +0000\" \"Hello\" NIL NIL NIL NIL NIL NIL NIL \"<m@example.test>\"))\r\nA00000005 OK fetched\r\n";
    `Return "A00000006 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let rows=ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_fetch_envelopes selected
      ~uids:[1L;2L] ())) in
  (match rows with
   | [{uid=2L;envelope={subject=Some "Hello";
       message_id=Some "<m@example.test>";_}}] -> ()
   | _ -> failwith "typed ENVELOPE projection lost UID or fields");
  Imap_eio.Client.close client

let test_typed_bodystructure_fetch () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "typed-bodystructure-fetch" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
    `Return "* 2 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 3] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 2 FETCH (UID 2 BODYSTRUCTURE (\"TEXT\" \"PLAIN\" (\"CHARSET\" \"UTF-8\") NIL NIL \"7BIT\" 12 2))\r\nA00000005 OK fetched\r\n";
    `Return "A00000006 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let rows=ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_fetch_bodystructures selected
      ~uids:[1L;2L] ())) in
  (match rows with
   | [{uid=2L;bodystructure=Imap.Response.Single_part
       {media_type="TEXT";lines=Some 2L;
        parameters=Some ["CHARSET","UTF-8"];_}}] -> ()
   | _ -> failwith "typed BODYSTRUCTURE projection lost UID or fields");
  Imap_eio.Client.close client

let test_extension_wrappers () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "extension-wrappers" in
  let caps="IMAP4rev1 ACL QUOTA=RES-STORAGE QUOTASET METADATA NOTIFY" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* ACL inbox alice lr\r\nA00000004 OK done\r\n";
    `Return "* QUOTAROOT inbox \"#user/alice\"\r\n* QUOTA \"unrelated\" (STORAGE 1 9)\r\n* QUOTA \"#user/alice\" (STORAGE 2 10)\r\nA00000005 OK done\r\n";
    `Return "* METADATA inbox (/shared/comment \"hello\")\r\nA00000006 OK [METADATA LONGENTRIES 2000] partial\r\n";
    `Return "* STATUS inbox (MESSAGES 2 UIDNEXT 3 UIDVALIDITY 1)\r\nA00000007 OK done\r\n";
    `Return "A00000008 OK changed\r\n";
    `Return "A00000009 OK notifications disabled\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let acl=ok (Imap_eio.Client.get_acl client ~mailbox:"INBOX") in
  if acl.entries<>["alice","lr"] then failwith "ACL wrapper lost entries";
  let mapping,quotas=ok (Imap_eio.Client.get_quota_root client
    ~mailbox:"INBOX") in
  if mapping.roots<>["#user/alice"] || List.length quotas<>1 ||
     (List.hd quotas).root<>"#user/alice" then
    failwith "QUOTAROOT wrapper accepted unrelated QUOTA";
  let metadata=ok (Imap_eio.Client.get_metadata client ~mailbox:"INBOX"
    ~entries:["/shared/comment"] ~maxsize:1024L ()) in
  if metadata.longentries<>Some 2000L ||
     List.length metadata.responses<>1 then
    failwith "METADATA truncation receipt lost";
  let statuses=ok (Imap_eio.Client.notify_set client ~status:true
    ~groups:[Imap.Command.Inboxes,
             [Imap.Command.Message_new;Imap.Command.Message_expunge]] ()) in
  if List.length statuses<>1 then failwith "NOTIFY STATUS receipt lost";
  ignore (ok (Imap_eio.Client.set_acl client ~mailbox:"INBOX"
    ~identifier:"alice" ~operation:`Add ~rights:"w"));
  ignore (ok (Imap_eio.Client.notify_none client));
  Imap_eio.Client.close client

let test_acl_mutation_uncertain () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "acl-uncertain" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 ACL\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 ACL\r\nA00000003 OK done\r\n";
    `Raise End_of_file;
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  match Imap_eio.Client.set_acl client ~mailbox:"INBOX"
    ~identifier:"alice" ~operation:`Add ~rights:"w" with
  | Error (Imap_eio.Error.Uncertain _) -> ()
  | Error e -> failwith ("wrong SETACL failure: " ^
      Imap_eio.Client.error_to_string e)
  | Ok () -> failwith "SETACL succeeded without tagged completion"

let test_selected_notify () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "selected-notify" in
  let caps="IMAP4rev1 UNSELECT NOTIFY" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 1] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* STATUS INBOX (UIDVALIDITY 1 UIDNEXT 1 MESSAGES 0)\r\nA00000005 OK enabled\r\n";
    `Return "* OK [NOTIFICATIONOVERFLOW] dropped\r\nA00000006 OK done\r\n";
    `Return "A00000007 OK disabled\r\n";
    `Return "A00000008 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ignore (ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected ->
      let groups=[Imap.Command.Selected,
        [Imap.Command.Message_new;Imap.Command.Message_expunge]] in
      let statuses=ok (Imap_eio.Selected.notify_set selected ~status:true
        ~groups ()) in
      if List.length statuses<>1 then failwith "selected NOTIFY STATUS lost";
      (match Imap_eio.Selected.notify_set selected ~groups () with
       | Error (Imap_eio.Error.Limit _) -> ()
       | Error e -> failwith ("wrong NOTIFY overflow error: " ^
           Imap_eio.Client.error_to_string e)
       | Ok _ -> failwith "notification overflow accepted");
      Imap_eio.Selected.notify_none selected)));
  Imap_eio.Client.close client

let test_discovery () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "discovery" in
  let caps="IMAP4rev1 NAMESPACE LIST-EXTENDED LIST-STATUS SPECIAL-USE" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* NAMESPACE ((\"\" \"/\")) NIL NIL\r\nA00000004 OK done\r\n";
    `Return ("* STATUS \"Sent\" (MESSAGES 999)\r\n" ^
             "* LIST (\\HasNoChildren \\Sent) \"/\" \"Sent\"\r\n" ^
             "* STATUS \"Sent\" (MESSAGES 2 UIDNEXT 9)\r\n" ^
             "* LIST (\\NoSelect) \"/\" \"Archive\"\r\n" ^
             "* STATUS \"Unrelated\" (MESSAGES 1)\r\n" ^
             "A00000005 OK done\r\n");
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let ns=ok (Imap_eio.Client.namespace client) in
  (match ns.personal with
   | Some [{prefix="";delimiter=Some "/";_}] -> ()
   | _ -> failwith "missing personal namespace");
  let d=ok (Imap_eio.Client.list_extended client ~patterns:["*"]
    ~returns:[Imap.Command.Children;Imap.Command.Return_special_use]
    ~status:[Imap.Command.Messages;Imap.Command.Uidnext] ()) in
  (match d.mailboxes with
   | [(sent,Some status);(archive,None)] ->
       if sent.mailbox<>"Sent" || sent.special_use<>["\\Sent"] ||
          status.messages<>Some 2L || archive.selectable then
         failwith "wrong LIST-STATUS association"
   | _ -> failwith "missing LIST rows or STATUS association");
  (match d.unpaired_status with
   | [{mailbox="Sent";messages=Some 999L;_};
      {mailbox="Unrelated";_}] -> ()
   | _ -> failwith "lost unmatched unsolicited STATUS");
  Imap_eio.Client.close client

let test_discovery_capabilities () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "discovery-caps" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let unavailable capability = function
    | Error (Imap_eio.Error.Unsupported c)
      when Imap.Capability.equal c capability -> ()
    | Error e -> failwith ("wrong capability error: " ^
        Imap_eio.Client.error_to_string e)
    | Ok _ -> failwith "accepted unavailable discovery extension" in
  unavailable Imap.Capability.Namespace (Imap_eio.Client.namespace client);
  unavailable Imap.Capability.List_extended
    (Imap_eio.Client.list_extended client ~patterns:["*"]
      ~returns:[Imap.Command.Children] ());
  unavailable Imap.Capability.List_extended
    (Imap_eio.Client.list_extended client ~patterns:["*"]
      ~status:[Imap.Command.Messages] ());
  Imap_eio.Client.close client

let test_uidonly_partial_batches () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "uidonly-extensions" in
  let caps="IMAP4rev1 ENABLE UIDONLY PARTIAL UIDBATCHES UNSELECT" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* ENABLED UIDONLY\r\nA00000004 OK enabled\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 5] valid\r\n* OK [UIDNEXT 100] next\r\nA00000005 OK selected\r\n";
    `Return "* ESEARCH (TAG \"A00000006\") UID PARTIAL (1:2 99)\r\nA00000006 OK searched\r\n";
    `Return "* 99 UIDFETCH (FLAGS (\\Seen))\r\nA00000007 OK fetched\r\n";
    `Return "* UIDBATCHES (TAG \"A00000008\") 99:1\r\nA00000008 OK batches\r\n";
    `Return "A00000009 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ignore (ok (Imap_eio.Client.enable_uidonly client));
  ignore (ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected ->
      (match Imap_eio.Selected.uid_search selected "1:3" with
       | Error (Imap_eio.Error.State _) -> ()
       | _ -> failwith "UIDONLY accepted sequence SEARCH key");
      let page=ok (Imap_eio.Selected.uid_search_partial selected
        ~range:(1L,2L) ~criterion:"ALL") in
      if page.partial<>Some ("1:2",Some "99") then
        failwith "PARTIAL ESEARCH page lost";
      let rows=ok (Imap_eio.Selected.uid_fetch_partial selected
        ~set:"1:99" ~items:["FLAGS"] ~range:(1L,2L)) in
      (match rows with
       | [{uid=Some 99L;flags=Some ["\\Seen"];_}] -> ()
       | _ -> failwith "UIDFETCH page lost");
      let batches=ok (Imap_eio.Selected.uid_batches selected ~size:500L ()) in
      if batches.ranges<>[99L,1L] then failwith "UIDBATCHES lost";
      (match Imap_eio.Selected.uid_batches selected ~size:500L () with
       | Error (Imap_eio.Error.State _) -> ()
       | _ -> failwith "UIDBATCHES reissue was not gated");
      Ok ())));
  Imap_eio.Client.close client

let test_untagged_messagelimit () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "untagged-messagelimit" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 2] next\r\nA00000004 OK selected\r\n";
    `Return "* 1 FETCH (UID 1 FLAGS ())\r\n* NO [MESSAGELIMIT 1 1] partial\r\nA00000005 OK [EXPUNGEISSUED] completed\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.fetch_metadata_range selected
      ~first:1L ~last:1L ~modseq:false) with
  | Error (Imap_eio.Error.Limit _) -> ()
  | Error e -> failwith ("wrong partial completion error: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "untagged MESSAGELIMIT was accepted as complete"

let test_mutation_messagelimit_no () =
  let check ~name ~reply ~copy ~uncertain =
    Eio_mock.Backend.run @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let flow=Eio_mock.Flow.make name in
    let caps="IMAP4rev1 UNSELECT UIDPLUS MESSAGELIMIT=1" in
    Eio_mock.Flow.on_read flow [
      `Return "* OK ready\r\n";
      `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
      `Return "A00000002 OK logged in\r\n";
      `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
      `Return "* 2 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 3] next\r\nA00000004 OK [READ-WRITE] selected\r\n";
      `Return reply;
      `Return "A00000006 OK unselected\r\n";
    ];
    let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
      ~allow_insecure_transport:true () in
    let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
    let uid=match Imap.Proto.Uid.of_int64 1L with
      | Ok uid -> uid | Error message -> failwith message in
    let set=Imap.Proto.Uid_set.singleton uid in
    let outcome=Imap_eio.Client.with_mailbox client ~mode:`Read_write
      "INBOX" (fun selected ->
        if copy then
          Result.map (fun _ -> ())
            (Imap_eio.Selected.uid_copy selected ~set ~mailbox:"Archive")
        else Imap_eio.Selected.uid_expunge selected ~set) in
    (match outcome,uncertain with
     | Error (Imap_eio.Error.Uncertain reason),true ->
         if reason <> "server reported a partial mutation with MESSAGELIMIT" then
           failwith "partial mutation reason was discarded"
     | Error (Imap_eio.Error.Rejected _),false -> ()
     | Error error,_ -> failwith ("wrong MESSAGELIMIT result: " ^
         Imap_eio.Client.error_to_string error)
     | Ok (),_ -> failwith "MESSAGELIMIT mutation unexpectedly succeeded");
    Imap_eio.Client.close client in
  check ~name:"expunge-partial-no"
    ~reply:"* NO [MESSAGELIMIT 1 1] partial\r\nA00000005 NO stopped\r\n"
    ~copy:false ~uncertain:true;
  check ~name:"expunge-tagged-no"
    ~reply:"A00000005 NO [MESSAGELIMIT 1 1] partial\r\n"
    ~copy:false ~uncertain:true;
  check ~name:"copy-atomic-no"
    ~reply:"A00000005 NO [MESSAGELIMIT 1 1] too many\r\n"
    ~copy:true ~uncertain:false

let test_search_messagelimit_resume () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "search-messagelimit" in
  let caps="IMAP4rev1 UNSELECT MESSAGELIMIT=2" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 3 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 4] next\r\nA00000004 OK selected\r\n";
    `Return "* SEARCH 3 2\r\nA00000005 OK [MESSAGELIMIT 2 2] partial\r\n";
    `Return "* SEARCH 1\r\nA00000006 OK complete\r\n";
    `Return "A00000007 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ignore (ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected ->
      let first=ok (Imap_eio.Selected.uid_search_page selected "ALL") in
      if first.complete || first.uids<>[2L;3L] ||
         first.resume_before<>Some 2L then
        failwith "MESSAGELIMIT continuation lost";
      let second=ok (Imap_eio.Selected.uid_search_page selected
        ~before:2L "ALL") in
      if not second.complete || second.uids<>[1L] then
        failwith "MESSAGELIMIT continuation wrong";
      Ok ())));
  Imap_eio.Client.close client

let test_uidonly_rejects_sequence_updates () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "uidonly-invalid" in
  let caps="IMAP4rev1 ENABLE UIDONLY UNSELECT" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* ENABLED UIDONLY\r\nA00000004 OK enabled\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 5] valid\r\n* OK [UIDNEXT 10] next\r\nA00000005 OK selected\r\n";
    `Return "* 1 FETCH (UID 9 FLAGS ())\r\nA00000006 OK completed\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ignore (ok (Imap_eio.Client.enable_uidonly client));
  match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.fetch_metadata_range selected
      ~first:1L ~last:9L ~modseq:false) with
  | Error (Imap_eio.Error.Protocol _) -> ()
  | Error e -> failwith ("wrong UIDONLY mode error: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "accepted sequence-number FETCH in UIDONLY mode"

let test_preview () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "preview" in
  let caps="IMAP4rev1 UNSELECT PREVIEW" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 3 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 5] next\r\nA00000004 OK selected\r\n";
    `Return ("* 1 FETCH (UID 2 PREVIEW NIL)\r\n" ^
      "* 2 FETCH (UID 3 PREVIEW \"\")\r\n" ^
      "* 3 FETCH (UID 4 PREVIEW {7}\r\n");
    `Return "📧 hi)\r\nA00000005 OK fetched\r\n";
    `Return "* 1 FETCH (UID 2 PREVIEW \"ready\")\r\nA00000006 OK fetched\r\n";
    `Return "A00000007 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ignore (ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected ->
      let rows=ok (Imap_eio.Selected.uid_fetch_previews selected
        ~lazy_:true ~uids:[4L;2L;3L] ()) in
      (match rows with
       | [{uid=2L;preview=None}; {uid=3L;preview=Some ""};
          {uid=4L;preview=Some "📧 hi"}] -> ()
       | _ -> failwith "LAZY PREVIEW distinctions lost");
      let rows=ok (Imap_eio.Selected.uid_fetch_previews selected
        ~uids:[2L] ()) in
      (match rows with
       | [{uid=2L;preview=Some "ready"}] -> ()
       | _ -> failwith "non-LAZY PREVIEW missing");
      Ok ())));
  Imap_eio.Client.close client

let test_preview_rejects_nonlazy_nil () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "preview-nil" in
  let caps="IMAP4rev1 UNSELECT PREVIEW" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 3] next\r\nA00000004 OK selected\r\n";
    `Return "* 1 FETCH (UID 2 PREVIEW NIL)\r\nA00000005 OK fetched\r\n";
    `Return "A00000006 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_fetch_previews selected
      ~uids:[2L] ()) with
  | Error (Imap_eio.Error.Protocol _) -> ()
  | Error e -> failwith ("wrong PREVIEW NIL error: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "accepted non-LAZY PREVIEW NIL"

let test_preview_rejects_large_literal_early () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "preview-overlimit" in
  let caps="IMAP4rev1 UNSELECT PREVIEW" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 3] next\r\nA00000004 OK selected\r\n";
    `Return "* 1 FETCH (UID 2 PREVIEW {1025}\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_fetch_previews selected
      ~uids:[2L] ()) with
  | Error (Imap_eio.Error.Limit _) -> ()
  | Error e -> failwith ("wrong oversized PREVIEW error: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "accepted oversized PREVIEW literal"

let test_preview_requires_capability () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "preview-capability" in
  let caps="IMAP4rev1 UNSELECT" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 3] next\r\nA00000004 OK selected\r\n";
    `Return "A00000005 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw" ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  (match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_fetch_previews selected
      ~uids:[2L] ()) with
   | Error (Imap_eio.Error.Unsupported Imap.Capability.Preview) -> ()
   | Error e -> failwith ("wrong PREVIEW capability error: " ^
       Imap_eio.Client.error_to_string e)
   | Ok _ -> failwith "accepted PREVIEW without capability");
  Imap_eio.Client.close client

let test_objectid_fetch () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "objectid" in
  let caps="IMAP4rev1 UNSELECT OBJECTID" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 2 EXISTS\r\n* OK [UIDVALIDITY 7] valid\r\n* OK [UIDNEXT 9] next\r\n* OK [MAILBOXID (F_box)] id\r\nA00000004 OK selected\r\n";
    `Return "* 1 FETCH (UID 7 EMAILID (M_7) THREADID NIL)\r\n* 2 FETCH (UID 8 EMAILID (M_8) THREADID (T_8))\r\nA00000005 OK fetched\r\n";
    `Return "A00000006 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let rows=ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_fetch_object_ids selected
      ~uids:[8L;7L] ())) in
  let expected : Imap_eio.Selected.object_id_row list = [
    {uid=8L;email_id="M_8";thread_id=Some "T_8"};
    {uid=7L;email_id="M_7";thread_id=None}] in
  if rows<>expected then failwith "typed OBJECTID results differ";
  Imap_eio.Client.close client

let test_objectid_plus_is_separate () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "objectid-plus-only" in
  let caps="IMAP4rev1 UNSELECT OBJECTID+" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 7] valid\r\n* OK [UIDNEXT 8] next\r\nA00000004 OK selected\r\n";
    `Return "A00000005 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  (match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_fetch_object_ids selected
      ~uids:[7L] ()) with
   | Error (Imap_eio.Error.Unsupported Imap.Capability.Objectid) -> ()
   | Error error -> failwith ("wrong OBJECTID+ refusal: " ^
       Imap_eio.Client.error_to_string error)
   | Ok _ -> failwith "OBJECTID+ accepted as RFC 8474 OBJECTID");
  Imap_eio.Client.close client

let test_objectid_incomplete_row () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "objectid-incomplete" in
  let caps="IMAP4rev1 UNSELECT OBJECTID" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 7] valid\r\n* OK [UIDNEXT 8] next\r\n* OK [MAILBOXID (F_box)] id\r\nA00000004 OK selected\r\n";
    `Return "* 1 FETCH (UID 7 EMAILID (M_7))\r\nA00000005 OK fetched\r\n";
    `Return "A00000006 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  (match Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected -> Imap_eio.Selected.uid_fetch_object_ids selected
      ~uids:[7L] ()) with
   | Error (Imap_eio.Error.Protocol
       "incomplete OBJECTID FETCH row") -> ()
   | Error error -> failwith ("wrong incomplete OBJECTID result: " ^
       Imap_eio.Client.error_to_string error)
   | Ok _ -> failwith "incomplete OBJECTID row was accepted");
  Imap_eio.Client.close client

let test_objectid_plus_activation () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "objectid-plus" in
  let caps="IMAP4rev1 UNSELECT ENABLE OBJECTID+" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* ENABLED OBJECTID+\r\nA00000004 OK enabled\r\n";
    `Return "A00000005 OK [OBJECTID (ACCOUNTID u_account MAILBOXID F_created)] created\r\n";
    `Return "A00000006 OK [OBJECTID (ACCOUNTID u_account MAILBOXID F_created)] renamed\r\n";
    `Return "* STATUS INBOX (OBJECTID (ACCOUNTID u_account MAILBOXID F_box))\r\nA00000007 OK status\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 7] valid\r\n* OK [UIDNEXT 8] next\r\n* OK [OBJECTID (ACCOUNTID u_account MAILBOXID F_box)] identity\r\nA00000008 OK selected\r\n";
    `Return "* 1 FETCH (UID 7 OBJECTID (EMAILID M_7 THREADID T_7))\r\nA00000009 OK fetched\r\n";
    `Return "A00000010 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ok (Imap_eio.Client.enable_objectid_plus client);
  if not (Imap_eio.Client.is_enabled client Imap.Capability.Objectid_plus)
  then failwith "OBJECTID+ activation not retained";
  let created=ok (Imap_eio.Client.create_mailbox_objectid client "Draft") in
  (match created with
   | {account_id=Some "u_account";mailbox_id=Some "F_created";_} -> ()
   | _ -> failwith "CREATE compound receipt missing");
  let renamed=ok (Imap_eio.Client.rename_mailbox_objectid client
    ~old_name:"Draft" ~new_name:"Renamed") in
  (match renamed with
   | {account_id=Some "u_account";mailbox_id=Some "F_created";_} -> ()
   | _ -> failwith "RENAME compound receipt missing");
  let status=ok (Imap_eio.Client.status client ~mailbox:"INBOX"
    ~items:[Imap.Command.Objectid]) in
  (match status.objectid with
   | Some {account_id=Some "u_account";mailbox_id=Some "F_box";_} -> ()
   | _ -> failwith "STATUS compound identity missing");
  let rows=ok (Imap_eio.Client.with_mailbox client
    ~objectid:("u_account","F_box") ~mode:`Read_only "INBOX"
    (fun selected ->
      let info=ok (Imap_eio.Selected.info selected) in
      (match info.objectid with
       | Some {account_id=Some "u_account";
           mailbox_id=Some "F_box";_} -> ()
       | _ -> failwith "selected account-scoped OBJECTID missing");
      Imap_eio.Selected.uid_fetch_object_ids_plus selected ~uids:[7L] ())) in
  (match rows with
   | [{uid=7L;ids={email_id=Some "M_7";
       thread_id=Some "T_7";_}}] -> ()
   | _ -> failwith "typed OBJECTID+ FETCH result missing");
  Imap_eio.Client.close client

let test_objectid_plus_fallback_refused () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "objectid-plus-fallback" in
  let caps="IMAP4rev1 UNSELECT ENABLE OBJECTID+" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* ENABLED OBJECTID+\r\nA00000004 OK enabled\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 7] valid\r\n* OK [UIDNEXT 8] next\r\n* OK [OBJECTID (ACCOUNTID u_account MAILBOXID F_replacement)] fallback\r\nA00000005 OK selected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ok (Imap_eio.Client.enable_objectid_plus client);
  let called=ref false in
  (match Imap_eio.Client.with_mailbox client
    ~objectid:("u_account","F_expected") ~mode:`Read_only "INBOX"
    (fun _ -> called:=true; Ok ()) with
   | Error (Imap_eio.Error.State
       "OBJECTID+ SELECT fell back to another mailbox") -> ()
   | Error error -> failwith ("wrong fallback error: " ^
       Imap_eio.Client.error_to_string error)
   | Ok () -> failwith "OBJECTID+ fallback was accepted");
  if !called then failwith "OBJECTID+ fallback reached callback";
  Imap_eio.Client.close client

let test_objectid_plus_missing_mutation_receipt () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "objectid-plus-no-receipt" in
  let caps="IMAP4rev1 ENABLE OBJECTID+" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* ENABLED OBJECTID+\r\nA00000004 OK enabled\r\n";
    `Return "A00000005 OK created without a receipt\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ok (Imap_eio.Client.enable_objectid_plus client);
  (match Imap_eio.Client.create_mailbox_objectid client "Draft" with
   | Error (Imap_eio.Error.Uncertain _) -> ()
   | Error error -> failwith ("wrong missing receipt error: " ^
       Imap_eio.Client.error_to_string error)
   | Ok _ -> failwith "missing OBJECTID+ CREATE receipt was accepted");
  Imap_eio.Client.close client

let test_objectid_plus_pinned_append_guard () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "objectid-plus-append-guard" in
  let caps="IMAP4rev1 ENABLE OBJECTID+" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* ENABLED OBJECTID+\r\nA00000004 OK enabled\r\n";
    `Return "* STATUS INBOX (OBJECTID (ACCOUNTID u_account MAILBOXID F_replacement))\r\nA00000005 OK status\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  ok (Imap_eio.Client.enable_objectid_plus client);
  ok (Imap_eio.Client.pin_mailbox_objectid client ~mailbox:"INBOX"
    ~account_id:"u_account" ~mailbox_id:"F_expected");
  (match Imap_eio.Client.append_flow_receipt client ~mailbox:"INBOX"
    ~length:0L (Eio.Flow.string_source "") with
   | Error (Imap_eio.Error.State
       "APPEND destination differs from pinned OBJECTID+ identity") -> ()
   | Error error -> failwith ("wrong pinned APPEND error: " ^
       Imap_eio.Client.error_to_string error)
   | Ok _ -> failwith "APPEND to replaced pinned mailbox was accepted");
  Imap_eio.Client.close client

let lease_connection ~sw flow replies =
  Eio_mock.Flow.on_read flow ([
    `Return "* PREAUTH ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
    `Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 2] next\r\nA00000002 OK selected\r\n";
  ] @ replies);
  ok (Imap_eio.Client.of_flow ~sw flow)

let test_selected_serialization () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "serialized-lease" in
  let reading=ref false in
  let client=lease_connection ~sw flow [
    `Run (fun () ->
      reading:=true;
      Eio.Fiber.yield ();
      reading:=false;
      "* SEARCH 1\r\nA00000003 OK searched\r\n");
    `Run (fun () ->
      if !reading then failwith "two commands read the same connection";
      "* SEARCH 2\r\nA00000004 OK searched\r\n");
    `Return "A00000005 OK unselected\r\n";
  ] in
  ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected ->
      let results=ref [] in
      let search () =
        let uids=ok (Imap_eio.Selected.uid_search selected "ALL") in
        results:=uids::!results in
      Eio.Fiber.both search search;
      if List.sort compare !results <> [[1L];[2L]] then
        failwith "serialized searches lost results";
      Ok ()))

let test_escaped_selected_command () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "escaped-lease" in
  let entered,mark_entered=Eio.Promise.create () in
  let release,release_read=Eio.Promise.create () in
  let finished,mark_finished=Eio.Promise.create () in
  let queued,mark_queued=Eio.Promise.create () in
  let client=lease_connection ~sw flow [
    `Run (fun () ->
      Eio.Promise.resolve mark_entered ();
      Eio.Promise.await release;
      "* SEARCH 1\r\nA00000003 OK searched\r\n");
  ] in
  ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
    (fun selected ->
      Eio.Fiber.fork ~sw (fun () ->
        let result=Imap_eio.Selected.uid_search selected "ALL" in
        Eio.Promise.resolve mark_finished result);
      Eio.Promise.await entered;
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve mark_queued
          (Imap_eio.Selected.uid_search selected "ALL"));
      Ok ()));
  if Imap_eio.Client.is_open client then
    failwith "escaped selected command left connection reusable";
  Eio.Promise.resolve release_read ();
  (match Eio.Promise.await finished with
   | Error (Imap_eio.Error.State _) -> ()
   | _ -> failwith "expired in-flight lease returned success");
  (match Eio.Promise.await queued with
   | Error (Imap_eio.Error.State _) -> ()
   | _ -> failwith "queued command ran after lease expired")

let test_append_unsolicited_continuation () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "append-unsolicited" in
  Eio_mock.Flow.on_read flow [
    `Return "* PREAUTH ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UIDPLUS\r\nA00000001 OK done\r\n";
    `Return "* OK maintenance notice\r\n+ ready\r\n";
    `Return "A00000002 OK [APPENDUID 1 4] appended\r\n";
  ];
  let client=ok (Imap_eio.Client.of_flow ~sw flow) in
  match ok (Imap_eio.Client.append_flow_receipt client ~mailbox:"INBOX"
    ~length:3L (Eio.Flow.string_source "abc")) with
  | Some receipt when Imap.Proto.Uid.to_int64 receipt.uid=4L -> ()
  | _ -> failwith "APPEND lost receipt after unsolicited response"

let () =
  test_selected_serialization ();
  test_escaped_selected_command ();
  test_append_unsolicited_continuation ();
  test_login_requires_tls ();
  test_wrong_uid (); test_cleanly_missing_uid_keeps_session ();
  test_uidnotsticky_refuses_selected_callback ();
  test_uncertain_append (); test_cram_vector ();
  test_cram_exchange (); test_cram_rejection_no_fallback ();
  test_cram_not_advertised ();
  test_cram_bad_challenge (); test_idle_fragmented ();
  test_partial_fetch_never_completes_inventory ();
  test_metadata_fetch_messagelimit_resume ();
  test_changes_messagelimit_resume ();
  test_changes_messagelimit_missing_boundary ();
  test_typed_envelope_fetch ();
  test_typed_bodystructure_fetch ();
  test_metadata_fetch_missing_boundary ();
  test_extension_wrappers (); test_acl_mutation_uncertain ();
  test_selected_notify (); test_discovery (); test_discovery_capabilities ();
  test_uidonly_partial_batches (); test_untagged_messagelimit ();
  test_mutation_messagelimit_no ();
  test_search_messagelimit_resume ();
  test_uidonly_rejects_sequence_updates ();
  test_preview (); test_preview_rejects_nonlazy_nil ();
  test_preview_rejects_large_literal_early ();
  test_preview_requires_capability ();
  test_objectid_fetch ();
  test_objectid_plus_is_separate ();
  test_objectid_incomplete_row ();
  test_objectid_plus_activation ();
  test_objectid_plus_fallback_refused ();
  test_objectid_plus_missing_mutation_receipt ();
  test_objectid_plus_pinned_append_guard ()
