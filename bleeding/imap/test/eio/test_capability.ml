(* Typed capability gates, rev2 folding and the general ENABLE. *)
module C = Imap_eio.Client
module S = Imap_eio.Selected
module E = Imap_eio.Error
module Cap = Imap.Capability

let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let tag n = Printf.sprintf "A%08d" n
let expect label kind = function
  | Error e when kind e -> ()
  | Error e -> failwith (label ^ ": " ^ C.error_to_string e)
  | Ok _ -> failwith (label ^ ": unexpectedly succeeded")

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

(* A PREAUTH server answers CAPABILITY as A00000001, so the first command a
   test issues is A00000002. [written] starts empty after the handshake. *)
let preauth ~caps replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let input = Eio_mock.Flow.make "capability" in
  Eio_mock.Flow.on_read input ([`Return "* PREAUTH ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ tag 1 ^ " OK caps\r\n")]
    @ replies);
  let transport = { Recording.input; written = Buffer.create 128 } in
  let client = ok (C.of_flow ~sw (Eio.Resource.T (transport,
    recording_handler))) in
  Buffer.clear transport.written;
  Fun.protect ~finally:(fun () -> C.close client) (fun () ->
    f client transport.written)

let selected n =
  "* 1 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 9] next\r\n" ^
  tag n ^ " OK selected\r\n"

let uid_set wire = Result.get_ok (Imap.Uid_set.of_wire wire)

let test_move_needs_capability () =
  preauth ~caps:"IMAP4rev1 UNSELECT"
    [`Return (selected 2); `Return (tag 3 ^ " OK unselected\r\n")]
    (fun client written ->
      ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
        let before = Buffer.length written in
        (match S.uid_move selected ~set:(uid_set "1") ~mailbox:"Archive" with
         | Error (E.Unsupported Cap.Move) -> ()
         | Error e -> failwith ("rev1 MOVE: " ^ C.error_to_string e)
         | Ok _ -> failwith "rev1 MOVE without the capability was sent");
        if Buffer.length written <> before then
          failwith "refused MOVE wrote bytes";
        Ok ()));
      if not (C.is_open client) then failwith "refused MOVE closed")

let test_move_folded_into_rev2 () =
  preauth ~caps:"IMAP4rev2"
    [`Return (selected 2);
     `Return (tag 3 ^ " OK [COPYUID 1 1 5] moved\r\n");
     `Return (tag 4 ^ " OK unselected\r\n")]
    (fun client written ->
      if Cap.Set.mem Cap.Move (C.capabilities client) then
        failwith "fixture advertises MOVE";
      if not (C.has client Cap.Move) then
        failwith "effective IMAP4rev2 does not imply MOVE";
      if C.has client Cap.Binary then
        failwith "IMAP4rev2 implied BINARY";
      ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
        match S.uid_move selected ~set:(uid_set "1") ~mailbox:"Archive" with
        | Ok (Some _) -> Ok ()
        | Ok None -> failwith "COPYUID receipt lost"
        | Error e -> failwith ("rev2 MOVE: " ^ C.error_to_string e)));
      let wire = Buffer.contents written in
      if not (String.starts_with ~prefix:(tag 2 ^ " SELECT INBOX") wire) ||
         not (String.ends_with ~suffix:(tag 3 ^ " UID MOVE 1 Archive\r\n" ^
           tag 4 ^ " UNSELECT\r\n") wire) then
        failwith ("unexpected rev2 MOVE wire: " ^ String.escaped wire))

let test_qresync_not_enabled () =
  preauth ~caps:"IMAP4rev1 UNSELECT QRESYNC" [] (fun client written ->
    if not (C.has client Cap.Qresync) then failwith "QRESYNC not advertised";
    if C.is_enabled client Cap.Qresync then
      failwith "QRESYNC enabled without ENABLE";
    (match C.with_mailbox client ~qresync:(1L, 5L) ~mode:`Read_only "INBOX"
       (fun _ -> Ok ()) with
     | Error (E.Not_enabled Cap.Qresync) -> ()
     | Error e -> failwith ("QRESYNC gate: " ^ C.error_to_string e)
     | Ok () -> failwith "QRESYNC SELECT without ENABLE was sent");
    if Buffer.length written <> 0 then failwith "refused SELECT wrote bytes";
    if not (C.is_open client) then failwith "refused SELECT closed")

let test_enable_returns_confirmed () =
  preauth ~caps:"IMAP4rev1 ENABLE CONDSTORE X-FOO"
    [`Return ("* ENABLED CONDSTORE\r\n" ^ tag 2 ^ " OK enabled\r\n")]
    (fun client written ->
      let confirmed = ok (C.enable client [Cap.Condstore; Cap.Other "x-foo"])
      in
      if confirmed <> [Cap.Condstore] then
        failwith "enable did not return the confirmed list";
      if Buffer.contents written <> tag 2 ^ " ENABLE CONDSTORE x-foo\r\n" then
        failwith ("unexpected ENABLE wire: " ^
          String.escaped (Buffer.contents written));
      if not (C.is_enabled client Cap.Condstore) then
        failwith "confirmed capability not recorded";
      if C.is_enabled client (Cap.Other "X-FOO") then
        failwith "unconfirmed capability recorded";
      Buffer.clear written;
      if ok (C.enable client [Cap.Condstore]) <> [] then
        failwith "re-enable returned capabilities";
      if ok (C.enable client []) <> [] then
        failwith "empty enable returned capabilities";
      expect "unadvertised ENABLE"
        (function E.Unsupported Cap.Move -> true | _ -> false)
        (C.enable client [Cap.Move]);
      if Buffer.length written <> 0 then
        failwith "enable without a new capability wrote bytes");
  preauth ~caps:"IMAP4rev1 CONDSTORE" [] (fun client written ->
    expect "ENABLE not advertised"
      (function E.Unsupported Cap.Enable -> true | _ -> false)
      (C.enable client [Cap.Condstore]);
    if Buffer.length written <> 0 then
      failwith "ENABLE without ENABLE wrote bytes")

let () =
  test_move_needs_capability ();
  test_move_folded_into_rev2 ();
  test_qresync_not_enabled ();
  test_enable_returns_confirmed ()
