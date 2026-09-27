(* Directed regressions for the implementation review of lib/eio. *)
module C = Imap_eio.Client
module S = Imap_eio.Selected
module E = Imap_eio.Error

let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let tag n = Printf.sprintf "A%08d" n
let expect label kind = function
  | Error e when kind e -> ()
  | Error e -> failwith (label ^ ": " ^ C.error_to_string e)
  | Ok _ -> failwith (label ^ ": unexpectedly succeeded")
let closed = function E.Closed -> true | _ -> false
let state = function E.State _ -> true | _ -> false

(* A PREAUTH server answers CAPABILITY as A00000001, so the first command a
   test issues is A00000002. *)
let preauth ~caps replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "review-fixes" in
  Eio_mock.Flow.on_read flow ([`Return "* PREAUTH ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ tag 1 ^ " OK caps\r\n")] @ replies);
  let client = ok (C.of_flow ~sw flow) in
  Fun.protect ~finally:(fun () -> C.close client) (fun () -> f client)

let selected n =
  "* 0 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 1] next\r\n" ^
  tag n ^ " OK selected\r\n"

let test_unselect_failure_keeps_outcome () =
  List.iter (fun unselect ->
    preauth ~caps:"IMAP4rev1 UNSELECT ENABLE UIDONLY"
      [`Return (selected 2); unselect] (fun client ->
      (match C.with_mailbox client ~mode:`Read_write "INBOX"
         (fun _ -> Ok "moved") with
       | Ok "moved" -> ()
       | _ -> failwith "failed UNSELECT dropped the callback outcome");
      if C.is_open client then failwith "failed UNSELECT left the connection open";
      expect "stale selection after failed UNSELECT" closed
        (C.enable_uidonly client)))
    [`Return (tag 3 ^ " NO busy\r\n"); `Raise End_of_file]

let test_selection_reset_after_exception () =
  preauth ~caps:"IMAP4rev1 UNSELECT ENABLE OBJECTID+" [`Return (selected 2)]
    (fun client ->
      (match C.with_mailbox client ~mode:`Read_only "INBOX"
         (fun _ -> failwith "callback bug") with
       | exception Failure message when message = "callback bug" -> ()
       | _ -> failwith "callback exception was relabelled");
      expect "stale selection after callback exception" closed
        (C.enable_objectid_plus client))

let test_enable_gating () =
  preauth ~caps:"IMAP4rev2 UIDONLY"
    [`Return ("* ENABLED UIDONLY\r\n" ^ tag 2 ^ " OK enabled\r\n")]
    (fun client ->
      ok (C.enable_uidonly client);
      if not (List.mem "UIDONLY" (C.enabled client)) then
        failwith "IMAP4rev2 ENABLE UIDONLY was not recorded");
  preauth ~caps:"IMAP4rev1 QRESYNC UTF8=ACCEPT"
    [`Return ("* OK still here\r\n" ^ tag 2 ^ " OK noop\r\n")]
    (fun client ->
      match ok (C.noop client) with
      | [_] when C.enabled client = [] -> ()
      | _ -> failwith "ENABLE was sent without ENABLE or IMAP4rev2")

let test_status_item_gating () =
  preauth ~caps:"IMAP4rev1" [] (fun client ->
    List.iter (fun item ->
      expect "ungated STATUS item" state
        (C.status client ~mailbox:"INBOX" ~items:[Imap.Command.Messages; item]))
      Imap.Command.[Highestmodseq; Mailboxid; Size; Deleted; Deleted_storage];
    if not (C.is_open client) then failwith "local STATUS refusal closed");
  preauth ~caps:"IMAP4rev1 CONDSTORE OBJECTID STATUS=SIZE QUOTA"
    [`Return ("* STATUS INBOX (HIGHESTMODSEQ 5 MAILBOXID (M1) SIZE 10 " ^
      "DELETED 0 DELETED-STORAGE 0)\r\n" ^ tag 2 ^ " OK status\r\n")]
    (fun client ->
      ignore (ok (C.status client ~mailbox:"INBOX"
        ~items:Imap.Command.[Highestmodseq; Mailboxid; Size; Deleted;
          Deleted_storage])));
  preauth ~caps:"IMAP4rev2"
    [`Return ("* STATUS INBOX (SIZE 10 DELETED 0)\r\n" ^ tag 2 ^ " OK status\r\n")]
    (fun client ->
      ignore (ok (C.status client ~mailbox:"INBOX"
        ~items:Imap.Command.[Size; Deleted])))

module Session = Imap_eio_core.Session
module Core_error = Imap_eio_core.Error

let with_session replies f =
  Eio_mock.Backend.run @@ fun () ->
  let flow = Eio_mock.Flow.make "review-session" in
  Eio_mock.Flow.on_read flow (List.map (fun s -> `Return s) replies);
  f (Session.create (Imap_eio_core.Transport.of_flow flow))

let test_unsent_command_is_known () =
  with_session [] (fun session ->
    match Session.protect session (fun () ->
      Session.command ~mutation:true session (String.make 70_000 'x')) with
    | Error (Core_error.Limit _) when not session.closed -> ()
    | _ -> failwith "an unsent mutation was not a plain limit")

let test_sent_mutation_keeps_cause () =
  with_session ["* BYE shutting down\r\n"] (fun session ->
    match Session.protect session (fun () ->
      Session.command ~mutation:true session "CREATE x") with
    | Error (Core_error.Uncertain text) when session.closed &&
        String.ends_with ~suffix:"server BYE: shutting down" text -> ()
    | _ -> failwith "sent mutation lost its cause or stayed open")

let test_append_known_failures () =
  List.iter (fun (replies, expected) ->
    with_session replies (fun session ->
      match Session.protect session (fun () ->
        Session.append session ~prefix:"APPEND INBOX {1}\r\n" ~length:1L
          (Eio.Flow.string_source "x")) with
      | Error e when expected e && session.closed -> ()
      | _ -> failwith "APPEND before its final CRLF was not a known failure"))
    [["* BYE going away\r\n"],
     (function Core_error.Protocol "server BYE: going away" -> true
      | _ -> false);
     ["A00000001 OK early\r\n"],
     (function Core_error.Protocol "expected APPEND continuation" -> true
      | _ -> false)]

let test_protect_reraises () =
  with_session [] (fun session ->
    match Session.protect session (fun () -> invalid_arg "bug") with
    | exception Invalid_argument message when message = "bug" ->
        if not session.closed then failwith "protect left a failed session open"
    | _ -> failwith "protect relabelled a programming error")

let test_idle_rejection_keeps_session () =
  with_session ["A00000001 NO [UNAVAILABLE] later\r\n"] (fun session ->
    match Session.protect session (fun () -> Session.idle_once session) with
    | Error (Core_error.Rejected _) when not session.closed -> ()
    | _ -> failwith "IDLE rejection closed the session")

let test_preview_limit_leading_zeros () =
  with_session ["* 1 FETCH (UID 1 PREVIEW {0002000}\r\n"] (fun session ->
    match Session.protect session (fun () ->
      Session.command session "UID FETCH 1 (UID PREVIEW)") with
    | Error (Core_error.Limit _) -> ()
    | _ -> failwith "zero-padded PREVIEW literal bypassed its limit")

let test_control_literals_bypass_sink () =
  let envelope = "ENVELOPE (\"d\" {3}\r\nsub NIL NIL NIL NIL NIL NIL NIL NIL)" in
  with_session [
    "* LIST () \"/\" {5}\r\nINBOX\r\n";
    "* 1 FETCH (UID 1 PREVIEW {5}\r\nhello BODY[] {3}\r\nabc " ^
      envelope ^ ")\r\n";
    "A00000001 OK done\r\n"] (fun session ->
    let sink = Buffer.create 8 and starts = ref [] in
    let responses = match Session.protect session (fun () ->
      Session.command session "UID FETCH 1 (UID PREVIEW BODY.PEEK[] ENVELOPE)"
        ~on_literal:(fun chunk -> Buffer.add_string sink chunk)
        ~on_literal_start:(fun n -> starts := n :: !starts)) with
      | Ok responses -> responses
      | Error e -> failwith (Core_error.to_string e) in
    if Buffer.contents sink <> "abc" || !starts <> [3L] then
      failwith "a control literal reached the body sink";
    match responses with
    | [Imap.Response.Untagged (Imap.Response.List list);
       Imap.Response.Untagged (Imap.Response.Fetch row)] ->
        if list.mailbox <> "INBOX" then failwith "LIST literal lost";
        if row.preview <> Some (Some "hello") then failwith "PREVIEW lost";
        if row.literals <> ["BODY[]", 3L] then failwith "body marker lost";
        (match Imap.Response.fetch_envelope row with
         | Ok (Some {subject = Some "sub"; _}) -> ()
         | _ -> failwith "ENVELOPE literal lost")
    | _ -> failwith "responses were not parsed")

(* Keeps the bytes the client writes, independently of mock traces. *)
module Recording = struct
  type t = {input : Eio_mock.Flow.t; written : Buffer.t}
  let read_methods = []
  let single_read t buffer = Eio.Flow.single_read t.input buffer
  let single_write t (buffers @ local) =
    let buffers = Cstruct.globalize_list buffers in
    List.iter (fun data ->
      Buffer.add_string t.written (Cstruct.to_string data)) buffers;
    Cstruct.lenv buffers
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _ = ()
  let close _ = ()
end
let recording_handler = Eio.Resource.handler (
  Eio.Resource.H (Eio.Resource.Close, Recording.close) ::
  Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Recording)))

let authenticate ~caps auth =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let input = Eio_mock.Flow.make "review-auth" in
  Eio_mock.Flow.on_read input [`Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ tag 1 ^ " OK caps\r\n")];
  let recording = {Recording.input; written = Buffer.create 64} in
  let result = C.of_flow ~sw ~auth (Eio.Resource.T (recording, recording_handler)) in
  Result.iter C.close result;
  result, Buffer.contents recording.written

let test_static_credentials_checked () =
  let rejects label f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> failwith (label ^ " was accepted") in
  rejects "NUL password" (fun () ->
    Imap_eio.Auth.password ~username:"u" ~password:"a\000b" ());
  rejects "CRAM-MD5 username with space" (fun () ->
    Imap_eio.Auth.password ~username:"a b" ~password:"p"
      ~mechanism:`Cram_md5 ());
  List.iter (fun token ->
    rejects ("bearer token " ^ token) (fun () ->
      Imap_eio.Auth.bearer ~username:"u" ~token ()))
    [""; "a=b"; "="; "a b"];
  ignore (Imap_eio.Auth.bearer ~username:"u" ~token:"abc==" ())

let test_provider_credentials_are_state () =
  let invalid label = function
    | Error (E.State "invalid credentials"), written
      when written = tag 1 ^ " CAPABILITY\r\n" -> ()
    | Error e, _ -> failwith (label ^ ": " ^ C.error_to_string e)
    | _ -> failwith (label ^ ": sent or accepted invalid credentials") in
  invalid "NUL provider password" (authenticate ~caps:"IMAP4rev1"
    (Imap_eio.Auth.refreshing ~username:"u" ~mechanism:`Login
      ~allow_insecure_transport:true (fun () -> "a\000b")));
  invalid "padded provider token" (authenticate
    ~caps:"IMAP4rev1 AUTH=OAUTHBEARER SASL-IR"
    (Imap_eio.Auth.refreshing_bearer ~username:"u"
      ~allow_insecure_transport:true (fun () -> "a=b")));
  invalid "CRAM-MD5 username checked before AUTHENTICATE" (authenticate
    ~caps:"IMAP4rev1 AUTH=CRAM-MD5"
    (Imap_eio.Auth.password ~username:"a b" ~password:"p" ()))

let test_plain_endpoint_names () =
  Eio_mock.Backend.run @@ fun () ->
  let net = Eio_mock.Net.make "review-net" in
  List.iter (fun host ->
    ignore (Imap_eio.Transport.v ~net ~host ~tls:`Plain ()))
    ["imap_test"; "fe80::1%eth0"]

let connect_and_close ~sw collected =
  let flow = Eio_mock.Flow.make "released" in
  Eio_mock.Flow.on_read flow [`Return "* PREAUTH ready\r\n";
    `Return ("* CAPABILITY IMAP4rev1\r\n" ^ tag 1 ^ " OK caps\r\n")];
  Gc.finalise (fun _ -> incr collected) flow;
  C.close (ok (C.of_flow ~sw flow))
[@@inline never]

let test_close_releases_switch_hook () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let collected = ref 0 in
  for _ = 1 to 4 do connect_and_close ~sw collected done;
  Gc.full_major (); Gc.full_major ();
  if !collected = 0 then
    failwith "closed clients stayed reachable from the switch"

let () =
  test_unselect_failure_keeps_outcome ();
  test_selection_reset_after_exception ();
  test_enable_gating ();
  test_status_item_gating ();
  test_close_releases_switch_hook ();
  test_unsent_command_is_known ();
  test_sent_mutation_keeps_cause ();
  test_append_known_failures ();
  test_protect_reraises ();
  test_idle_rejection_keeps_session ();
  test_preview_limit_leading_zeros ();
  test_control_literals_bypass_sink ();
  test_static_credentials_checked ();
  test_provider_credentials_are_state ();
  test_plain_endpoint_names ()
