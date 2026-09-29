(* Strategy choice in Imap_eio.Mailbox, asserted on the exact commands. *)
module C = Imap_eio.Client
module S = Imap_eio.Selected
module M = Imap_eio.Mailbox
module E = Imap_eio.Error
module Cap = Imap.Capability

let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let tag n = Printf.sprintf "A%08d" n
let u n = match Imap.Uid.of_int64 n with Ok v -> v | Error e -> failwith e
let uid_set s =
  match Imap.Uid_set.of_wire s with Ok v -> v | Error e -> failwith e

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

(* A PREAUTH server answers CAPABILITY as A00000001, so SELECT is
   A00000002. [written] starts empty after the handshake, and the mock
   clock advances whenever every fiber is blocked on it. *)
let preauth ~caps replies f =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let input = Eio_mock.Flow.make "mailbox" in
  Eio_mock.Flow.on_read input ([`Return "* PREAUTH ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ tag 1 ^ " OK caps\r\n")]
    @ List.map (fun reply -> `Return reply) replies);
  let transport = { Recording.input; written = Buffer.create 128 } in
  let client = ok (C.of_flow ~sw (Eio.Resource.T (transport,
    recording_handler))) in
  Buffer.clear transport.written;
  Fun.protect ~finally:(fun () -> C.close client) (fun () ->
    f client transport.written env#clock)

let selected ?(uidnext = 9) n =
  Printf.sprintf "* 8 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n\
    * OK [UIDNEXT %d] next\r\n%s OK selected\r\n" uidnext (tag n)

let select = tag 2 ^ " SELECT INBOX\r\n"
let select_condstore = tag 2 ^ " SELECT INBOX (CONDSTORE)\r\n"

let wire label written expected =
  let got = Buffer.contents written in
  if got <> expected then
    failwith (Printf.sprintf "%s: expected\n%s\ngot\n%s" label
      (String.escaped expected) (String.escaped got))

(* Runs [f] on a read-write INBOX lease whose SELECT is A00000002 and whose
   UNSELECT follows the [replies]. *)
let with_lease ~caps ?uidnext replies f =
  preauth ~caps (selected ?uidnext 2 :: replies) (fun client written clock ->
    ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun s ->
      Ok (f (M.of_selected s) written clock))))

let seen = [Mail_flag.Imap_flag.system Seen]

let test_store_unconditional () =
  with_lease ~caps:"IMAP4rev1 UNSELECT"
    [tag 3 ^ " OK stored\r\n"; tag 4 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.store m ~set:(uid_set "1:3") ~operation:`Add ~flags:seen with
      | {strategy = `Unconditional; result = Ok _} ->
          wire "unconditional STORE" written
            (select ^ tag 3 ^ " UID STORE 1:3 +FLAGS (\\Seen)\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "unconditional STORE chose the wrong strategy")

let test_store_conditional () =
  with_lease ~caps:"IMAP4rev1 UNSELECT CONDSTORE"
    [tag 3 ^ " OK [MODIFIED 2] stored\r\n"; tag 4 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.store ~unchangedsince:5L m ~set:(uid_set "1:3")
              ~operation:`Add ~flags:seen with
      | {strategy = `Conditional; result = Ok receipt} ->
          if not (Imap.Uid_set.equal receipt.modified (uid_set "2")) then
            failwith "conditional STORE lost MODIFIED";
          wire "conditional STORE" written
            (select_condstore ^ tag 3 ^
             " UID STORE 1:3 (UNCHANGEDSINCE 5) +FLAGS (\\Seen)\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "conditional STORE chose the wrong strategy");
  with_lease ~caps:"IMAP4rev1 UNSELECT" [tag 3 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.store ~unchangedsince:5L m ~set:(uid_set "1:3")
              ~operation:`Add ~flags:seen with
      | {strategy = `Conditional;
         result = Error (E.Unsupported Cap.Condstore)} ->
          wire "refused conditional STORE" written select
      | _ -> failwith "conditional STORE without CONDSTORE was not refused")

let copyuid = "[COPYUID 7 1:2 11:12]"

let test_move_strategies () =
  with_lease ~caps:"IMAP4rev1 UNSELECT MOVE"
    [tag 3 ^ " OK " ^ copyuid ^ " moved\r\n"; tag 4 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.move m ~set:(uid_set "1:2") ~mailbox:"Archive" with
      | {strategy = `Move; result = Ok (Some _)} ->
          wire "MOVE" written (select ^ tag 3 ^ " UID MOVE 1:2 Archive\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "MOVE chose the wrong strategy");
  with_lease ~caps:"IMAP4rev1 UNSELECT UIDPLUS"
    [tag 3 ^ " OK " ^ copyuid ^ " copied\r\n"; tag 4 ^ " OK stored\r\n";
     tag 5 ^ " OK expunged\r\n"; tag 6 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.move m ~set:(uid_set "1:2") ~mailbox:"Archive" with
      | {strategy = `Copy_then_expunge; result = Ok (Some _)} ->
          wire "COPY then UID EXPUNGE" written
            (select ^ tag 3 ^ " UID COPY 1:2 Archive\r\n" ^
             tag 4 ^ " UID STORE 1:2 +FLAGS (\\Deleted)\r\n" ^
             tag 5 ^ " UID EXPUNGE 1:2\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "UIDPLUS move chose the wrong strategy");
  with_lease ~caps:"IMAP4rev1 UNSELECT"
    [tag 3 ^ " OK " ^ copyuid ^ " copied\r\n"; tag 4 ^ " OK stored\r\n";
     tag 5 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.move m ~set:(uid_set "1:2") ~mailbox:"Archive" with
      | {strategy = `Copy_then_flag; result = Ok (Some _)} ->
          wire "COPY then flag" written
            (select ^ tag 3 ^ " UID COPY 1:2 Archive\r\n" ^
             tag 4 ^ " UID STORE 1:2 +FLAGS (\\Deleted)\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "rev1 move chose the wrong strategy")

let test_move_partial () =
  with_lease ~caps:"IMAP4rev1 UNSELECT UIDPLUS"
    [tag 3 ^ " OK " ^ copyuid ^ " copied\r\n"; tag 4 ^ " NO no flags\r\n";
     tag 5 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.move m ~set:(uid_set "1:2") ~mailbox:"Archive" with
      | {strategy = `Copied (Some receipt);
         result = Error (E.Rejected {status = `No; _})} ->
          if not (Imap.Uid_set.equal receipt.destination (uid_set "11:12"))
          then failwith "partial move lost the COPYUID receipt";
          wire "partial move" written
            (select ^ tag 3 ^ " UID COPY 1:2 Archive\r\n" ^
             tag 4 ^ " UID STORE 1:2 +FLAGS (\\Deleted)\r\n")
      | _ -> failwith "failed STORE after COPY was not reported as Copied")

let modseq n = match Imap.Modseq.of_int64 n with
  | Ok m -> m | Error e -> failwith e

let flags_of = function
  | M.Flags (uid, flags) ->
      Printf.sprintf "F%Ld:%s" (Imap.Uid.to_int64 uid)
        (String.concat "," (List.map Mail_flag.Imap_flag.to_wire flags))
  | M.New uid -> Printf.sprintf "N%Ld" (Imap.Uid.to_int64 uid)
  | M.Vanished set -> "V" ^ Imap.Uid_set.to_wire set

let test_changes_condstore () =
  with_lease ~caps:"IMAP4rev1 UNSELECT CONDSTORE"
    ["* 1 FETCH (UID 2 FLAGS (\\Seen) MODSEQ (9))\r\n\
      * 2 FETCH (UID 8 FLAGS () MODSEQ (10))\r\n" ^ tag 3 ^ " OK done\r\n";
     tag 4 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.changes_since ~uidnext:(u 7L) m (Some (modseq 5L)) with
      | {strategy = `Condstore; result = Ok changes} ->
          if List.map flags_of changes <> ["F2:\\Seen"; "N8"] then
            failwith ("CONDSTORE changes: " ^
              String.concat " " (List.map flags_of changes));
          wire "CONDSTORE changes" written
            (select_condstore ^ tag 3 ^
             " UID FETCH 1:8 (UID FLAGS MODSEQ) (CHANGEDSINCE 5)\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "CONDSTORE changes chose the wrong strategy")

let test_changes_full () =
  with_lease ~caps:"IMAP4rev1 UNSELECT" ~uidnext:1500
    ["* 1 FETCH (UID 3 FLAGS (\\Flagged))\r\n" ^ tag 3 ^ " OK done\r\n";
     "* 2 FETCH (UID 1200 FLAGS ())\r\n" ^ tag 4 ^ " OK done\r\n";
     tag 5 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.changes_since m (Some (modseq 5L)) with
      | {strategy = `Full; result = Ok changes} ->
          if List.map flags_of changes <> ["F3:\\Flagged"; "F1200:"] then
            failwith ("full changes: " ^
              String.concat " " (List.map flags_of changes));
          wire "full changes" written
            (select ^ tag 3 ^ " UID FETCH 1:1000 (UID FLAGS)\r\n" ^
             tag 4 ^ " UID FETCH 1001:1499 (UID FLAGS)\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "full changes chose the wrong strategy")

let mailbox_names rows =
  List.map (fun ((entry : C.mailbox_entry), status) ->
    Result.get_ok entry.name.utf8 ^ "=" ^
    match status with
    | Some (s : Imap.Response.mailbox_status) ->
        Option.fold ~none:"?" ~some:Int64.to_string s.messages
    | None -> "none") rows

let test_list_with_status () =
  preauth ~caps:"IMAP4rev1 LIST-EXTENDED LIST-STATUS"
    ["* LIST () \"/\" INBOX\r\n* STATUS INBOX (MESSAGES 4)\r\n\
      * LIST (\\Noselect) \"/\" Folders\r\n" ^ tag 2 ^ " OK listed\r\n"]
    (fun client written _ ->
      match M.list_with_status client ~pattern:"*" [Imap.Status_item.Messages]
      with
      | {strategy = `List_status; result = Ok rows} ->
          if mailbox_names rows <> ["INBOX=4"; "Folders=none"] then
            failwith "LIST-STATUS rows";
          wire "LIST-STATUS" written
            (tag 2 ^ " LIST \"\" \"*\" RETURN (STATUS (MESSAGES))\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "LIST-STATUS chose the wrong strategy");
  preauth ~caps:"IMAP4rev1"
    ["* LIST () \"/\" INBOX\r\n* LIST (\\Noselect) \"/\" Folders\r\n\
      * LIST () \"/\" Gone\r\n" ^ tag 2 ^ " OK listed\r\n";
     "* STATUS INBOX (MESSAGES 4)\r\n" ^ tag 3 ^ " OK status\r\n";
     tag 4 ^ " NO no such mailbox\r\n"]
    (fun client written _ ->
      match M.list_with_status client ~pattern:"*" [Imap.Status_item.Messages]
      with
      | {strategy = `List_then_status; result = Ok rows} ->
          if mailbox_names rows <> ["INBOX=4"; "Folders=none"; "Gone=none"]
          then failwith "LIST then STATUS rows";
          wire "LIST then STATUS" written
            (tag 2 ^ " LIST \"\" \"*\"\r\n" ^
             tag 3 ^ " STATUS INBOX (MESSAGES)\r\n" ^
             tag 4 ^ " STATUS Gone (MESSAGES)\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "LIST then STATUS chose the wrong strategy")

let test_fetch_split () =
  let uids = List.init 1001 (fun i -> u (Int64.of_int (i + 1))) @ [u 5L] in
  with_lease ~caps:"IMAP4rev1 UNSELECT" ~uidnext:1002
    ["* 1 FETCH (UID 1 FLAGS ())\r\n* 2 FETCH (UID 1000 FLAGS ())\r\n" ^
     tag 3 ^ " OK done\r\n";
     "* 3 FETCH (UID 1001 FLAGS ())\r\n" ^ tag 4 ^ " OK done\r\n";
     tag 5 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.fetch m ~uids ~items:[] with
      | {strategy = `Fetch 2; result = Ok rows} ->
          if List.map (fun (r : S.row) -> Imap.Uid.to_int64 r.uid) rows
             <> [1L; 1000L; 1001L] then
            failwith "split FETCH lost request order";
          wire "split FETCH" written
            (select ^ tag 3 ^ " UID FETCH 1:1000 (UID FLAGS)\r\n" ^
             tag 4 ^ " UID FETCH 1001 (UID FLAGS)\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | {strategy = `Fetch n; _} ->
          failwith (Printf.sprintf "split FETCH took %d round trips" n))

let test_fetch_unsupported () =
  let preview = Imap.Fetch_item.Preview {lazy_ = false} in
  with_lease ~caps:"IMAP4rev1 UNSELECT"
    ["* 1 FETCH (UID 4 FLAGS ())\r\n" ^ tag 3 ^ " OK done\r\n";
     tag 4 ^ " OK unselected\r\n"]
    (fun m written _ ->
      (match M.fetch m ~uids:[u 4L] ~items:[preview] with
       | {strategy = `Fetch 0; result = Error (E.Unsupported Cap.Preview)} ->
           wire "refused PREVIEW" written select
       | _ -> failwith "unsupported PREVIEW was not refused");
      match M.fetch ~drop_unsupported:true m ~uids:[u 4L] ~items:[preview]
      with
      | {strategy = `Fetch 1; result = Ok [{preview = None; _}]} ->
          wire "dropped PREVIEW" written
            (select ^ tag 3 ^ " UID FETCH 4 (UID FLAGS)\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "dropped PREVIEW fetched the wrong rows")

let test_search_unsupported () =
  with_lease ~caps:"IMAP4rev1 UNSELECT" [tag 3 ^ " OK unselected\r\n"]
    (fun m written _ ->
      match M.search m ~criteria:(Imap.Search.Modseq (modseq 5L)) with
      | {strategy = `Search; result = Error (E.Unsupported Cap.Condstore)} ->
          wire "refused MODSEQ search" written select
      | _ -> failwith "MODSEQ search without CONDSTORE was not refused")

let test_wait_skips_keepalive () =
  with_lease ~caps:"IMAP4rev1 UNSELECT IDLE"
    ["+ idling\r\n* OK Still here\r\n"; tag 3 ^ " OK done\r\n";
     "+ idling\r\n* 9 EXISTS\r\n* OK Still here\r\n";
     tag 4 ^ " OK done\r\n"; tag 5 ^ " OK unselected\r\n"]
    (fun m written clock ->
      match M.wait m ~clock ~poll_seconds:60. with
      | {strategy = `Idle;
         result = Ok [Imap.Response.Untagged (Imap.Response.Exists 9L)]} ->
          wire "IDLE keepalive" written
            (select ^ tag 3 ^ " IDLE\r\nDONE\r\n" ^
             tag 4 ^ " IDLE\r\nDONE\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "IDLE woke for a keepalive or lost the EXISTS")

let test_wait_polls () =
  with_lease ~caps:"IMAP4rev1 UNSELECT"
    ["* OK Still here\r\n" ^ tag 3 ^ " OK done\r\n";
     "* 9 EXISTS\r\n* 1 RECENT\r\n" ^ tag 4 ^ " OK done\r\n";
     tag 5 ^ " OK unselected\r\n"]
    (fun m written clock ->
      let start = Eio.Time.now clock in
      match M.wait m ~clock ~poll_seconds:60. with
      | {strategy = `Poll;
         result = Ok Imap.Response.[Untagged (Exists 9L);
           Untagged (Recent 1L)]} ->
          if Eio.Time.now clock -. start <> 120. then
            failwith "poll did not sleep before each NOOP";
          wire "NOOP poll" written
            (select ^ tag 3 ^ " NOOP\r\n" ^ tag 4 ^ " NOOP\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "poll woke for a keepalive or lost the EXISTS")

let () =
  test_store_unconditional ();
  test_store_conditional ();
  test_move_strategies ();
  test_move_partial ();
  test_changes_condstore ();
  test_changes_full ();
  test_list_with_status ();
  test_fetch_split ();
  test_fetch_unsupported ();
  test_search_unsupported ();
  test_wait_skips_keepalive ();
  test_wait_polls ()
