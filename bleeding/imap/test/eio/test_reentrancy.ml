(* A Client command on the connection a with_mailbox callback holds is a
   typed error, not a deadlock. *)
module C = Imap_eio.Client
module S = Imap_eio.Selected
module E = Imap_eio.Error

let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let tag n = Printf.sprintf "A%08d" n
let reentrant = "call inside with_mailbox on the same connection"

module Recording = struct
  type t = { input : Eio_mock.Flow.t; written : Buffer.t }
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

(* The PREAUTH handshake uses A00000001 and [written] starts empty after it,
   so SELECT is A00000002 and UNSELECT A00000003. *)
let with_lease replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let input = Eio_mock.Flow.make "reentrancy" in
  Eio_mock.Flow.on_read input ([`Return "* PREAUTH ready\r\n";
    `Return ("* CAPABILITY IMAP4rev1 UNSELECT\r\n" ^ tag 1 ^ " OK caps\r\n");
    `Return ("* 1 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n" ^
      "* OK [UIDNEXT 9] next\r\n" ^ tag 2 ^ " OK selected\r\n");
    `Return (tag 3 ^ " OK unselected\r\n")] @ replies);
  let transport = { Recording.input; written = Buffer.create 128 } in
  let client = ok (C.of_flow ~sw (Eio.Resource.T (transport,
    recording_handler))) in
  Buffer.clear transport.written;
  Fun.protect ~finally:(fun () -> C.close client) (fun () ->
    f client transport.written)

let refused label call written =
  let before = Buffer.length written in
  (match call () with
   | Error (E.State message) when message = reentrant -> ()
   | Error e -> failwith (label ^ ": " ^ C.error_to_string e)
   | Ok _ -> failwith (label ^ ": ran inside the lease"));
  if Buffer.length written <> before then
    failwith (label ^ ": wrote bytes")

let expect_wire label written expected =
  if Buffer.contents written <> expected then
    failwith (label ^ ": unexpected wire " ^
      String.escaped (Buffer.contents written))

let test_nested_with_mailbox () =
  with_lease [] (fun client written ->
    ok (C.with_mailbox client ~mode:`Read_only "INBOX" (fun _ ->
      refused "nested with_mailbox" (fun () ->
        C.with_mailbox client ~mode:`Read_only "Archive" (fun _ -> Ok ()))
        written;
      Ok ()));
    expect_wire "nested with_mailbox" written
      (tag 2 ^ " EXAMINE INBOX\r\n" ^ tag 3 ^ " UNSELECT\r\n");
    if not (C.is_open client) then
      failwith "nested with_mailbox closed the connection")

let test_noop_inside_callback () =
  with_lease [`Return (tag 4 ^ " OK noop\r\n")] (fun client written ->
    ok (C.with_mailbox client ~mode:`Read_only "INBOX" (fun _ ->
      refused "Client.noop" (fun () -> C.noop client) written;
      Ok ()));
    ignore (ok (C.noop client));
    expect_wire "Client.noop" written
      (tag 2 ^ " EXAMINE INBOX\r\n" ^ tag 3 ^ " UNSELECT\r\n" ^
       tag 4 ^ " NOOP\r\n"))

let () =
  test_nested_with_mailbox ();
  test_noop_inside_callback ()
