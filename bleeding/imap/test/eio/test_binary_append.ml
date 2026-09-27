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

let with_client ?(caps="IMAP4rev1 BINARY UIDPLUS") replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let input=Eio_mock.Flow.make "binary-append-server" in
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

let test_exact_bytes () =
  with_client [`Return "+ ready\r\n";
    `Return "A00000004 OK [APPENDUID 11 27] appended\r\n"]
    (fun client transport ->
      let date=match Imap.Internal_date.of_string "12-Jan-2020 12:00:00 +0000" with
        | Ok date -> date | Error message -> failwith message in
      let source=Eio.Flow.string_source "a\000bTRAIL" in
      (match ok (C.append_binary_flow_receipt client ~mailbox:"INBOX"
        ~flags:["\\Seen"] ~internal_date:date ~length:3L source) with
       | Some receipt when Imap.Proto.Uidvalidity.to_int64 receipt.uidvalidity=11L &&
           Imap.Proto.Uid.to_int64 receipt.uid=27L -> ()
       | _ -> failwith "binary APPENDUID receipt lost");
      let expected="A00000004 APPEND INBOX (\\Seen) \"12-Jan-2020 12:00:00 +0000\" ~{3}\r\na\000b\r\n" in
      if Buffer.contents transport.written<>expected then
        failwith ("wrong binary APPEND wire: " ^ String.escaped (Buffer.contents transport.written));
      let remaining=Cstruct.create 5 in
      Eio.Flow.read_exact source remaining;
      if Cstruct.to_string remaining<>"TRAIL" then failwith "APPEND consumed bytes beyond declared size")

let test_empty_and_no_receipt () =
  with_client [`Return "+ ready\r\n";`Return (done_ 4)] (fun client transport ->
    if ok (C.append_binary_flow_receipt client ~mailbox:"INBOX" ~length:0L
      (Eio.Flow.string_source "remaining"))<>None then failwith "missing APPENDUID invented";
    if Buffer.contents transport.written<>"A00000004 APPEND INBOX ~{0}\r\n\r\n" then
      failwith "empty literal8 framing changed");
  with_client [`Return "+ ready\r\n";`Return (done_ 4)] (fun client _ ->
    ok (C.append_binary_flow client ~mailbox:"INBOX" ~length:1L
      (Eio.Flow.string_source "\255")))

let test_capability_refusal () =
  List.iter (fun caps ->
    with_client ~caps [] (fun client transport ->
      let source=Eio_mock.Flow.make "unread-binary-source" in
      let read=ref false in
      Eio_mock.Flow.on_read source [`Run (fun () -> read:=true; "abc")];
      expect "binary APPEND needs explicit BINARY"
        (unsupported Imap.Capability.Binary)
        (C.append_binary_flow client ~mailbox:"INBOX" ~length:3L source);
      if !read || Buffer.length transport.written<>0 then
        failwith "capability refusal dispatched or read source"))
    ["IMAP4rev1 UIDPLUS";"IMAP4rev2 UIDPLUS"]

let test_rejection () =
  List.iter (fun after_literal ->
    let prefix=if after_literal then [`Return "+ ready\r\n"] else [] in
    with_client (prefix @ [`Return "A00000004 NO [UNKNOWN-CTE] unsupported encoding\r\n"])
      (fun client transport ->
        let source=Eio.Flow.string_source "a\000b" in
        expect "typed UNKNOWN-CTE rejection"
          (function E.Rejected {code=Some R.Unknown_cte;_} -> true | _ -> false)
          (C.append_binary_flow client ~mailbox:"INBOX" ~length:3L source);
        let expected="A00000004 APPEND INBOX ~{3}\r\n" ^
          if after_literal then "a\000b\r\n" else "" in
        if Buffer.contents transport.written<>expected then failwith "rejected APPEND bytes wrong";
        if not after_literal then (
          let unread=Cstruct.create 3 in
          Eio.Flow.read_exact source unread;
          if Cstruct.to_string unread<>"a\000b" then failwith "source read before continuation")))
    [false;true]

let test_source_failures () =
  with_client [`Return "+ ready\r\n"] (fun client transport ->
    expect "truncated source is a known failure" state
      (C.append_binary_flow client ~mailbox:"INBOX" ~length:3L (Eio.Flow.string_source "a"));
    if C.is_open client || not transport.closed then failwith "truncated APPEND stayed open";
    if Buffer.contents transport.written<>"A00000004 APPEND INBOX ~{3}\r\na" then
      failwith "truncated APPEND replayed or added bytes");
  with_client [`Return "+ ready\r\n"] (fun client transport ->
    let source=Eio_mock.Flow.make "failed-binary-source" in
    Eio_mock.Flow.on_read source [`Raise (Failure "synthetic source failure")];
    expect "source exception is a known failure" state
      (C.append_binary_flow client ~mailbox:"INBOX" ~length:3L source);
    if C.is_open client || not transport.closed then failwith "failed source stayed open";
    if Buffer.contents transport.written<>"A00000004 APPEND INBOX ~{3}\r\n" then
      failwith "source failure triggered a replay");
  with_client [`Return "+ ready\r\n";`Raise End_of_file] (fun client transport ->
    expect "lost APPEND completion is uncertain" uncertain
      (C.append_binary_flow client ~mailbox:"INBOX" ~length:3L (Eio.Flow.string_source "a\000b"));
    if C.is_open client || not transport.closed then failwith "lost completion stayed open";
    if Buffer.contents transport.written<>"A00000004 APPEND INBOX ~{3}\r\na\000b\r\n" then
      failwith "lost completion triggered a replay")

let test_cancelled_source () =
  with_client [`Return "+ ready\r\n"] (fun client transport ->
    let entered,mark_entered=Eio.Promise.create () in
    let source=Eio_mock.Flow.make "cancelled-binary-source" in
    Eio_mock.Flow.on_read source [
      `Return "a";
      `Run (fun () -> Eio.Promise.resolve mark_entered (); Eio.Fiber.await_cancel ())];
    Eio.Fiber.first
      (fun () -> ignore (C.append_binary_flow client ~mailbox:"INBOX" ~length:3L source))
      (fun () -> Eio.Promise.await entered);
    if C.is_open client || not transport.closed then failwith "cancelled APPEND stayed open";
    if Buffer.contents transport.written<>"A00000004 APPEND INBOX ~{3}\r\na" then
      failwith "cancelled APPEND replayed or added bytes")

let test_multi_uid_receipt () =
  List.iter (fun binary ->
    with_client [`Return "+ ready\r\n";
      `Return "A00000004 OK [APPENDUID 11 27:28] appended\r\n"] (fun client transport ->
      let result=if binary then C.append_binary_flow_receipt client
          ~mailbox:"INBOX" ~length:1L (Eio.Flow.string_source "x")
        else C.append_flow_receipt client ~mailbox:"INBOX" ~length:1L
          (Eio.Flow.string_source "x") in
      expect "multiple UIDs for one APPEND is uncertain" uncertain result;
      if C.is_open client || not transport.closed then failwith "ambiguous APPENDUID stayed open"))
    [false;true]

let () =
  test_exact_bytes (); test_empty_and_no_receipt (); test_capability_refusal ();
  test_rejection (); test_source_failures (); test_cancelled_source ();
  test_multi_uid_receipt ()
