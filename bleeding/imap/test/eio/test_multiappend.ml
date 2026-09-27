module C=Imap_eio.Client
module E=Imap_eio.Error
module R=Imap.Response
let ok=function Ok x -> x | Error e -> failwith (C.error_to_string e)
let expect label kind=function
  | Error e when kind e -> ()
  | Error e -> failwith (label ^ ": " ^ C.error_to_string e)
  | Ok _ -> failwith (label ^ ": unexpectedly succeeded")
let state=function E.State _ -> true | _ -> false
let unsupported c = function
  | E.Unsupported x -> Imap.Capability.equal x c | _ -> false
let uncertain=function E.Uncertain _ -> true | _ -> false
let tag n=Printf.sprintf "A%08d" n
let done_ n=tag n ^ " OK done\r\n"

(* Keep the actual bytes written by the client, independently of mock traces. *)
module Recording = struct
  type t={input:Eio_mock.Flow.t;written:Buffer.t;mutable closed:bool}
  let read_methods=[]
  let single_read t buffer=Eio.Flow.single_read t.input buffer
  let single_write t (buffers @ local)=
    let buffers=Cstruct.globalize_list buffers in
    List.iter (fun data -> Buffer.add_string t.written (Cstruct.to_string data)) buffers;
    Cstruct.lenv buffers
  let copy t ~src=Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _=()
  let close t=t.closed<-true
end
let recording_handler=Eio.Resource.handler (
  Eio.Resource.H (Eio.Resource.Close,Recording.close) ::
  Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Recording)))

let with_client ?(caps="IMAP4rev1 MULTIAPPEND UIDPLUS") replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let input=Eio_mock.Flow.make "multiappend-server" in
  Eio_mock.Flow.on_read input ([
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ done_ 1);
    `Return (done_ 2);
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ done_ 3)] @ replies);
  let transport={Recording.input;written=Buffer.create 128;closed=false} in
  let flow=Eio.Resource.T (transport,recording_handler) in
  let auth=Imap_eio.Auth.password ~username:"u" ~password:"p"
    ~mechanism:`Login ~allow_insecure_transport:true () in
  let client=ok (C.of_flow ~sw ~auth flow) in
  Buffer.clear transport.written;
  Fun.protect ~finally:(fun () -> C.close client) (fun () -> f client transport)

let message text=C.append_message ~length:(Int64.of_int (String.length text))
  (Eio.Flow.string_source text)
let replies completion=[`Return "+ first\r\n";`Return "+ second\r\n";
  `Return ("A00000004 " ^ completion ^ "\r\n")]
let test_wire () =
  with_client (replies "OK [APPENDUID 11 29,7] done") (fun client transport ->
    let source=Eio.Flow.string_source "abcTAIL" in
    let first=C.append_message ~length:3L ~flags:["\\Seen"] source in
    let second=message "defg" in
    (match ok (C.append_messages client ~mailbox:"INBOX" [first;second]) with
     | Some receipt when List.map Imap.Uid.to_int64 receipt.uids=[29L;7L] -> ()
     | _ -> failwith "receipt order lost");
    if Buffer.contents transport.written<>
      "A00000004 APPEND INBOX (\\Seen) {3}\r\nabc {4}\r\ndefg\r\n" then
      failwith "incorrect MULTIAPPEND wire";
    let rest=Cstruct.create 4 in Eio.Flow.read_exact source rest;
    if Cstruct.to_string rest<>"TAIL" then failwith "read beyond first message")
let test_preflight () =
  List.iter (fun (caps,kind,messages) ->
    with_client ~caps [] (fun client transport ->
    expect "preflight" kind
      (C.append_messages client ~mailbox:"INBOX" messages);
    if Buffer.length transport.written<>0 then failwith "preflight wrote bytes"))
    ["IMAP4rev1",unsupported Imap.Capability.Multiappend,
       [message "a";message "b"];
     "MULTIAPPEND",state,[];
     "MULTIAPPEND",state,[message "a";message ""];
     "MULTIAPPEND",state,[message "a";C.append_message ~flags:["bad flag"]
       ~length:1L (Eio.Flow.string_source "b")];
     "MULTIAPPEND",state,List.init 1001 (fun _ -> message "a")]
let test_rejection () =
  with_client [`Return "+ first\r\n";`Return "A00000004 NO [OVERQUOTA] full\r\n"]
    (fun client transport ->
      let second=Eio.Flow.string_source "SECOND" in
      expect "atomic rejection" (function E.Rejected _ -> true | _ -> false)
        (C.append_messages client ~mailbox:"INBOX"
          [message "first";C.append_message ~length:6L second]);
      if not (C.is_open client) then failwith "rejection closed usable session";
      if Buffer.contents transport.written<>"A00000004 APPEND INBOX {5}\r\nfirst {6}\r\n" then
        failwith "sent rejected literal";
      let rest=Cstruct.create 6 in Eio.Flow.read_exact second rest;
      if Cstruct.to_string rest<>"SECOND" then failwith "read rejected source")
let test_uncertain () =
  List.iter (fun completion -> with_client (replies completion) (fun client _ ->
    expect "invalid receipt" uncertain
      (C.append_messages client ~mailbox:"INBOX" [message "a";message "b"]);
    if C.is_open client then failwith "invalid receipt left connection open"))
    ["OK [APPENDUID 11 1] done";"OK [APPENDUID 11 1:3] done";
     "OK [APPENDUID 11 1:4294967295] done";"OK [APPENDUID 11 1,2,1] done"];
  with_client [`Return "+ first\r\n";`Return "+ second\r\n";`Raise End_of_file]
    (fun client _ -> expect "lost completion" uncertain
      (C.append_messages client ~mailbox:"INBOX" [message "a";message "b"]));
  with_client [`Return "+ first\r\n";`Return "+ second\r\n"]
    (fun client _ -> expect "short second source" state
      (C.append_messages client ~mailbox:"INBOX"
        [message "a";C.append_message ~length:5L (Eio.Flow.string_source "x")]);
      if C.is_open client then failwith "short second source kept session open")
let test_extra_continuation () =
  with_client [`Return "+ first\r\n";`Return "+ second\r\n";
    `Return "+ unexpected\r\nA00000004 OK [APPENDUID 11 1:2] done\r\n"]
    (fun client _ -> expect "extra continuation" uncertain
      (C.append_messages client ~mailbox:"INBOX" [message "a";message "b"]);
      if C.is_open client then failwith "extra continuation kept session open")
let test_partial_notice () =
  List.iter (fun completion ->
    with_client [`Return "* NO [MESSAGELIMIT 1 2] limited\r\n+ first\r\n";
      `Return "+ second\r\n";`Return ("A00000004 " ^ completion ^ "\r\n")]
      (fun client _ ->
        let expected=if String.starts_with ~prefix:"NO" completion then
          (function E.Rejected _ -> true | _ -> false) else uncertain in
        expect "partial notice" expected
          (C.append_messages client ~mailbox:"INBOX" [message "a";message "b"])))
    ["OK [APPENDUID 11 1:2] done";"NO rejected"]
let test_cancelled_second_source () =
  with_client [`Return "+ first\r\n";`Return "+ second\r\n"] (fun client transport ->
    let entered,mark_entered=Eio.Promise.create () in
    let source=Eio_mock.Flow.make "cancelled-second-message" in
    Eio_mock.Flow.on_read source [`Run (fun () ->
      Eio.Promise.resolve mark_entered (); Eio.Fiber.await_cancel ())];
    Eio.Fiber.first
      (fun () -> ignore (C.append_messages client ~mailbox:"INBOX"
        [message "a";C.append_message ~length:3L source]))
      (fun () -> Eio.Promise.await entered);
    if C.is_open client || not transport.closed then failwith "cancelled batch stayed open";
    if Buffer.contents transport.written<>"A00000004 APPEND INBOX {1}\r\na {3}\r\n" then
      failwith "cancelled batch replayed or added bytes")
let test_advertised_limits () =
  List.iter (fun capability ->
    with_client ~caps:("IMAP4rev1 MULTIAPPEND " ^ capability) [] (fun client transport ->
      expect "advertised batch limit" (function E.Limit _ -> true | _ -> false)
        (C.append_messages client ~mailbox:"INBOX" [message "a";message "b"]);
      if Buffer.length transport.written<>0 then failwith "over-limit batch dispatched"))
    ["SAVELIMIT=1";"MESSAGELIMIT=1";"MESSAGELIMIT=10 SAVELIMIT=1"];
  with_client ~caps:"IMAP4rev1 MULTIAPPEND SAVELIMIT=2"
    (replies "OK [APPENDUID 11 1:2] done") (fun client _ ->
      ignore (ok (C.append_messages client ~mailbox:"INBOX" [message "a";message "b"])));
  with_client ~caps:"IMAP4rev1 MULTIAPPEND SAVELIMIT=0" [] (fun client transport ->
    expect "invalid advertised limit" (function E.Protocol _ -> true | _ -> false)
      (C.append_messages client ~mailbox:"INBOX" [message "a";message "b"]);
    if Buffer.length transport.written<>0 then failwith "invalid limit allowed dispatch")
let test_empty_receipt () =
  with_client (replies "OK done") (fun client _ ->
    if ok (C.append_messages client ~mailbox:"INBOX" [message "a";message "b"])<>None then
      failwith "invented UID receipt")
let () = test_wire (); test_preflight (); test_rejection (); test_uncertain (); test_empty_receipt (); test_extra_continuation (); test_partial_notice (); test_cancelled_second_source (); test_advertised_limits ()
