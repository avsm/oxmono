module C=Imap_eio.Client
module E=Imap_eio.Error
module R=Imap.Response
let ok=function Ok x -> x | Error e -> failwith (C.error_to_string e)
let expect label kind=function
  | Error e when kind e -> ()
  | Error e -> failwith (label ^ ": " ^ C.error_to_string e)
  | Ok _ -> failwith (label ^ ": unexpectedly succeeded")
let state=function E.State _ -> true | _ -> false
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


let test_boundary () =
  List.iter (fun caps -> List.iter (fun length ->
    let non_sync=length<=4096 && caps<>"IMAP4rev1" in
    let replies=(if non_sync then [] else [`Return "+ ready\r\n"]) @
      [`Return "A00000004 OK [APPENDUID 1 9] done\r\n"] in
    with_client ~caps replies (fun client transport ->
      let body=String.make length 'x' in
      ignore (ok (C.append client ~mailbox:"INBOX"
        (C.append_message ~length:(Int64.of_int length)
           (Eio.Flow.string_source body))));
      let expected=Printf.sprintf "A00000004 APPEND INBOX {%d%s}\r\n%s\r\n"
        length (if non_sync then "+" else "") body in
      if Buffer.contents transport.written<>expected then
        failwith "incorrect negotiated literal boundary")) [4095;4096;4097])
    ["IMAP4rev1";"IMAP4rev1 LITERAL-";"IMAP4rev1 LITERAL+";"IMAP4rev2"]
let test_revision_negotiation () =
  List.iter (fun enabled ->
    let enable=if enabled then "* ENABLED IMAP4rev2\r\nA00000004 OK enabled\r\n"
      else "A00000004 BAD not enabled\r\n" in
    let replies=[`Return enable] @
      (if enabled then [] else [`Return "+ ready\r\n"]) @
      [`Return "A00000005 OK [APPENDUID 1 9] done\r\n"] in
    with_client ~caps:"IMAP4rev1 IMAP4rev2" replies (fun client transport ->
      ok (Result.map ignore (C.append client ~mailbox:"INBOX"
        (C.append_message ~length:1L (Eio.Flow.string_source "x"))));
      let expected=if enabled then "A00000005 APPEND INBOX {1+}\r\nx\r\n"
        else "A00000005 APPEND INBOX {1}\r\nx\r\n" in
      if Buffer.contents transport.written<>expected then failwith "rev2 negotiation ignored"))
    [false;true]
let test_binary () =
  with_client ~caps:"IMAP4rev1 BINARY LITERAL-"
    [`Return "A00000004 OK [APPENDUID 1 9] done\r\n"] (fun client transport ->
      ok (Result.map ignore (C.append client ~mailbox:"INBOX" ~binary:true
        (C.append_message ~length:3L (Eio.Flow.string_source "a\000b"))));
      if Buffer.contents transport.written<>"A00000004 APPEND INBOX ~{3+}\r\na\000b\r\n" then
        failwith "binary non-synchronizing marker lost")
let test_mixed_batch () =
  with_client ~caps:"IMAP4rev1 MULTIAPPEND LITERAL-"
    [`Return "+ second\r\n";`Return "A00000004 OK [APPENDUID 1 9:10] done\r\n"]
    (fun client transport ->
      let first=String.make 4096 'a' and second=String.make 4097 'b' in
      let message text=C.append_message ~length:(Int64.of_int (String.length text))
        (Eio.Flow.string_source text) in
      ignore (ok (C.append_many client ~mailbox:"INBOX" [message first;message second]));
      if Buffer.contents transport.written<>
        "A00000004 APPEND INBOX {4096+}\r\n" ^ first ^ " {4097}\r\n" ^ second ^ "\r\n" then
        failwith "mixed batch framing changed")
let test_rejected () =
  with_client ~caps:"IMAP4rev1 LITERAL-"
    [`Return "A00000004 NO [OVERQUOTA] full\r\n"] (fun client _ ->
      expect "non-sync rejection" (function E.Rejected _ -> true | _ -> false)
        (Result.map ignore (C.append client ~mailbox:"INBOX"
          (C.append_message ~length:1L (Eio.Flow.string_source "x"))));
      if not (C.is_open client) then failwith "tagged rejection closed connection");
  with_client ~caps:"IMAP4rev1 LITERAL-"
    [`Return "+ invalid\r\n"] (fun client _ ->
      expect "illegal non-sync continuation" uncertain
        (Result.map ignore (C.append client ~mailbox:"INBOX"
          (C.append_message ~length:1L (Eio.Flow.string_source "x")))))
let () = test_boundary (); test_revision_negotiation (); test_binary (); test_mixed_batch (); test_rejected ()
