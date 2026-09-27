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
       | exception Failure _ -> ()
       | _ -> ());
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
  test_close_releases_switch_hook ()
