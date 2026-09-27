(* Internal session tests use small limits instead of allocating production-sized
   transcripts. The public client intentionally does not expose Session. *)
module Session = Imap_eio_core.Session
module Transport = Imap_eio_core.Transport

let with_session ?(max_responses=8) replies f =
  Eio_mock.Backend.run @@ fun () ->
  let flow=Eio_mock.Flow.make "session-budget" in
  Eio_mock.Flow.on_read flow (List.map (fun s -> `Return s) replies);
  let session=Session.create ~max_metadata:1024 ~max_responses
    ~max_command_metadata:1024 (Transport.of_flow flow) in
  f session

let notice = "* OK " ^ String.make 590 'x' ^ "\r\n"

let expect_limit session operation =
  match Session.protect session operation with
  | Error (Imap_eio_core.Error.Limit _) when session.closed -> ()
  | _ -> failwith "IDLE failed to enforce its aggregate budget and close"

let test_idle_bytes () =
  (* Each response fits the per-response limit. The two phases together do not. *)
  with_session [notice; "+ idle\r\n"; notice] (fun session ->
    expect_limit session (fun () -> Session.idle_once session))

let test_idle_count () =
  with_session ~max_responses:1 [
    "* 1 EXISTS\r\n"; "+ idle\r\n"; "* 2 EXISTS\r\n"
  ] (fun session ->
    expect_limit session (fun () -> Session.idle_once session))

(* A budget exceeded before the final CRLF leaves the APPEND unexecuted, so
   the limit is reported as such. After the final CRLF the outcome is
   unknown. *)
let test_append_budget () =
  let limit = function Imap_eio_core.Error.Limit _ -> true | _ -> false in
  let uncertain = function
    | Imap_eio_core.Error.Uncertain _ -> true | _ -> false in
  List.iter (fun (max_responses,replies,expected) ->
    with_session ~max_responses replies (fun session ->
      match Session.protect session (fun () ->
        Session.append session ~prefix:"APPEND INBOX {0}\r\n" ~length:0L
          (Eio.Flow.string_source "")) with
      | Error e when expected e && session.closed -> ()
      | _ -> failwith "APPEND exceeded budget without the right error or close"))
    [8,[notice;notice;"+ go\r\n";"A00000001 OK appended\r\n"],limit;
     8,[notice;"+ go\r\n";notice;"A00000001 OK appended\r\n"],uncertain;
     1,["* OK notice\r\n";"* OK notice\r\n";"+ go\r\n";
        "A00000001 OK appended\r\n"],limit;
     1,["* OK notice\r\n";"+ go\r\n";"* OK notice\r\n";
        "A00000001 OK appended\r\n"],uncertain]

let test_idle_control_literal () =
  with_session ["+ idle\r\n"; "* LIST () \"/\" {5}\r\nINBOX\r\n";
    "A00000001 OK done\r\n"] (fun session ->
    match Session.idle_once session with
    | [Imap.Response.Untagged (Imap.Response.List row)] when row.mailbox="INBOX" -> ()
    | _ -> failwith "IDLE lost literal mailbox name");
  with_session ["+ idle\r\n"; "* LIST () \"/\" {1100}\r\n" ^
    String.make 1100 'x' ^ "\r\n"] (fun session ->
      expect_limit session (fun () -> Session.idle_once session))

let test_multiappend_budget () =
  with_session ~max_responses:1 ["* OK notice\r\n";"+ first\r\n";
    "* OK notice\r\n";"+ second\r\n"] (fun session ->
      let part prefix : Session.append_part = {prefix;length=1L;synchronizing=true;
        read=Eio.Flow.single_read (Eio.Flow.string_source "a")} in
      match Session.protect session (fun () -> Session.append_many session
        [part "APPEND INBOX {1}\r\n";part " {1}\r\n"]) with
      | Error (Imap_eio_core.Error.Limit _) when session.closed -> ()
      | _ -> failwith "MULTIAPPEND reset response budget between literals")

let test_logout_budget () =
  with_session ~max_responses:1 ["* OK notice\r\n";"* BYE goodbye\r\n";
    "A00000001 OK logout\r\n"] (fun session ->
      match Session.protect session (fun () -> Session.logout session) with
      | Error (Imap_eio_core.Error.Limit _) when session.closed -> ()
      | _ -> failwith "LOGOUT did not enforce response budget")

let test_notification_flood () =
  let overflow="* OK [NOTIFICATIONOVERFLOW] dropped\r\n" in
  with_session ~max_responses:1 [overflow;overflow;"+ idling\r\n"]
    (fun session -> expect_limit session (fun () -> Session.idle_once session));
  with_session ~max_responses:1 ["+ idling\r\n";overflow;overflow;
    "A00000001 OK done\r\n"]
    (fun session -> expect_limit session (fun () -> Session.idle_once session))

let test_deferred_wire_error () =
  with_session ["* BYE going\r\nbad\n"] (fun session ->
    (match Session.read_response session with
     | [Imap.Wire.Text "* BYE going\r\n"; Imap.Wire.End_of_response] -> ()
     | _ -> failwith "lost the BYE framed before a wire error");
    match Session.read_response session with
    | exception Session.Failure (Imap_eio_core.Error.Protocol _) -> ()
    | _ -> failwith "lost the deferred wire error")

let () =
  test_deferred_wire_error ();
  test_notification_flood ();
  test_logout_budget ();
  test_multiappend_budget ();
  test_idle_control_literal ();
  test_idle_bytes ();
  test_idle_count ();
  test_append_budget ()
