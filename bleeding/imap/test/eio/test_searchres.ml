module S = Imap_eio.Selected
module C = Imap_eio.Client
module E = Imap_eio.Error
let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let expect label kind = function
  | Error e when kind e -> ()
  | Error e -> failwith (label ^ ": " ^ C.error_to_string e)
  | Ok _ -> failwith (label ^ ": unexpectedly succeeded")
let state = function E.State _ -> true | _ -> false
let unsupported c = function
  | E.Unsupported x -> Imap.Capability.equal x c | _ -> false
let protocol = function E.Protocol _ -> true | _ -> false
let uncertain = function E.Uncertain _ -> true | _ -> false
let rejected = function E.Rejected _ -> true | _ -> false
let tag n=Printf.sprintf "A%08d" n
let done_ n = tag n ^ " OK done\r\n"
let save n count = Printf.sprintf "* ESEARCH (TAG \"%s\") UID COUNT %Ld\r\n%s"
  (tag n) count (done_ n)
let selected n = "* 2 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n" ^
  "* OK [UIDNEXT 10] next\r\n" ^ done_ n
let with_client ?(caps="SEARCHRES UIDPLUS MOVE CONDSTORE PARTIAL") replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "searchres" in
  let caps="IMAP4rev1 UNSELECT " ^ caps in
  Eio_mock.Flow.on_read flow ([
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ done_ 1);
    `Return (done_ 2);
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ done_ 3)] @ replies);
  let auth=Imap_eio.Auth.password ~username:"u" ~password:"p"
    ~allow_insecure_transport:true () in
  let client=ok (C.of_flow ~sw ~auth flow) in
  Fun.protect ~finally:(fun () -> C.close client) (fun () -> f ~sw client)
module R = S.Searchres
let seen=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen
let search_save selected ~criteria =
  Result.bind (R.require selected) (fun searchres ->
    R.uid_search_save searchres ~criteria)
let store saved=R.uid_store_saved saved ~operation:`Add ~flags:[seen] ()
let fetch saved=R.uid_fetch_saved saved ~items:[Imap.Fetch_item.Flags] ()
let uids s = match Imap.Uid_set.of_wire s with
  | Ok set -> Imap.Search.Uid set
  | Error e -> failwith e

let test_operations () =
  with_client [
    `Return (selected 4); `Return (save 5 2L);
    `Return ("* 1 FETCH (UID 3 FLAGS ())\r\n* 2 FETCH (UID 7 FLAGS ())\r\n" ^ done_ 6);
    `Return ("* 1 FETCH (UID 3 FLAGS (\\Seen))\r\n" ^ done_ 7);
    `Return (tag 8 ^ " OK [COPYUID 2 3,7 20:21] copied\r\n");
    `Return ("* 1 EXPUNGE\r\n" ^ tag 9 ^ " OK [COPYUID 2 3 22] moved\r\n");
    `Return (done_ 10); `Return (done_ 11); `Return (done_ 12)]
    (fun ~sw:_ client ->
      let escaped=ref None in
      ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
        let saved=ok (search_save selected ~criteria:Imap.Search.All) in
        escaped:=Some saved;
        if R.saved_search_count saved<>2L then failwith "saved count lost";
        let rows=ok (fetch saved) in
        if List.map (fun (row:S.row) -> Imap.Uid.to_int64 row.uid) rows<>[3L;7L]
        then failwith "saved fetch UIDs lost";
        ignore (ok (store saved));
        (match ok (R.uid_copy_saved saved ~mailbox:"Archive") with
         | Some _ -> () | None -> failwith "saved COPYUID lost");
        ignore (ok (R.uid_move_saved saved ~mailbox:"Archive"));
        ok (R.uid_expunge_saved saved);
        if ok (fetch saved)<>[] then failwith "expunged saved set not empty";
        if R.saved_search_count saved<>2L then failwith "captured count mutated";
        Ok ()));
      expect "escaped saved lease" state (fetch (Option.get !escaped)))

let test_empty () =
  with_client ([`Return (selected 4);`Return (save 5 0L)] @
    List.init 6 (fun i -> `Return (done_ (6+i)))) (fun ~sw:_ client ->
    ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
      let saved=ok (search_save selected ~criteria:(uids "100:200")) in
      if R.saved_search_count saved<>0L || ok (fetch saved)<>[] then
        failwith "empty saved set changed";
      ignore (ok (store saved));
      ignore (ok (R.uid_copy_saved saved ~mailbox:"Archive"));
      ignore (ok (R.uid_move_saved saved ~mailbox:"Archive"));
      ok (R.uid_expunge_saved saved); Ok ())))

let test_replacement_and_raw_search () =
  with_client [`Return (selected 4);`Return (save 5 2L);`Return (save 6 1L);
    `Return ("* SEARCH 7\r\n" ^ done_ 7);`Return (done_ 8)] (fun ~sw:_ client ->
    ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
      let old=ok (search_save selected ~criteria:Imap.Search.All) in
      let current=ok (search_save selected ~criteria:(uids "7")) in
      expect "replacement SAVE" state (store old);
      ignore (ok (S.uid_search selected
        ~criteria:(Imap.Search.Raw "RETURN (SAVE) ALL")));
      expect "raw SAVE invalidates fetch" state (fetch current);
      expect "raw SAVE invalidates store" state (store current);
      expect "raw SAVE invalidates copy" state (R.uid_copy_saved current ~mailbox:"Archive");
      expect "raw SAVE invalidates move" state (R.uid_move_saved current ~mailbox:"Archive");
      expect "raw SAVE invalidates expunge" state (R.uid_expunge_saved current);
      Ok ())))

let test_rejected_search () =
  List.iter (fun response ->
    with_client [`Return (selected 4);`Return (save 5 2L);
      `Return (tag 6 ^ response ^ "\r\n");`Return (done_ 7)] (fun ~sw:_ client ->
      ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
        let saved=ok (search_save selected ~criteria:Imap.Search.All) in
        expect "SAVE rejection" rejected (search_save selected
          ~criteria:Imap.Search.All);
        expect "rejected SAVE invalidates prior handle" state (fetch saved);
        Ok ())))) [" NO [NOTSAVED] resource limit";" BAD bad criteria"];
  with_client [`Return (selected 4);`Return (save 5 2L);
    `Return ("* SEARCH 3 7\r\n" ^ done_ 6);`Return (done_ 7)] (fun ~sw:_ client ->
    ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
      let saved=ok (search_save selected ~criteria:Imap.Search.All) in
      ignore (ok (S.uid_search selected ~criteria:Imap.Search.All));
      expect "ordinary SEARCH conservative invalidation" state (fetch saved);
      Ok ())))

let test_invalid_save_results () =
  List.iter (fun reply ->
    with_client [`Return (selected 4);`Return (reply (tag 5) ^ done_ 5);
      `Return (done_ 6)] (fun ~sw:_ client ->
      expect "invalid SAVE COUNT" protocol
        (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
          search_save selected ~criteria:Imap.Search.All))))
    [(fun _ -> "");
     (fun _ -> "* ESEARCH UID COUNT 2\r\n");
     (fun tag -> "* ESEARCH (TAG \"" ^ tag ^ "\") UID\r\n");
     (fun tag -> "* ESEARCH (TAG \"" ^ tag ^ "\") COUNT 2\r\n");
     (fun tag -> "* ESEARCH (TAG \"" ^ tag ^ "\") UID COUNT 2 PARTIAL (1:1 7)\r\n");
     (fun tag -> let row="* ESEARCH (TAG \"" ^ tag ^ "\") UID COUNT 2\r\n" in row^row)]

let test_gates () =
  with_client ~caps:"" [`Return (selected 4);`Return (done_ 5)] (fun ~sw:_ client ->
    expect "SEARCHRES capability" (unsupported Imap.Capability.Searchres)
      (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
        search_save selected ~criteria:Imap.Search.All)));
  with_client ~caps:"SEARCHRES" [`Return (selected 4);`Return (save 5 2L);
    `Return (done_ 6)] (fun ~sw:_ client ->
    ok (C.with_mailbox client ~mode:`Read_only "INBOX" (fun selected ->
      let saved=ok (search_save selected ~criteria:Imap.Search.All) in
      expect "saved partial requires capability"
        (unsupported Imap.Capability.Partial)
        (R.uid_fetch_saved saved ~partial:(1L,2L)
          ~items:[Imap.Fetch_item.Flags] ());
      expect "saved MODSEQ requires capability"
        (unsupported Imap.Capability.Condstore)
        (R.uid_fetch_saved saved ~items:[Imap.Fetch_item.Modseq] ());
      expect "read-only saved STORE" state (store saved);
      expect "read-only saved MOVE" state (R.uid_move_saved saved ~mailbox:"Archive");
      expect "read-only saved EXPUNGE" state (R.uid_expunge_saved saved);
      Ok ())))

let test_identity_reset () =
  List.iter (fun code ->
    let notice="* OK [" ^ code ^ "] reset\r\n" in
    with_client [`Return (selected 4);`Return (notice ^ save 5 2L)] (fun ~sw:_ client ->
      expect "identity reset during SAVE" protocol
        (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
          search_save selected ~criteria:Imap.Search.All)));
    List.iter (fun (name,mutate) ->
      with_client [`Return (selected 4);`Return (save 5 2L);
        `Return (notice ^ done_ 6)] (fun ~sw:_ client ->
        expect (name ^ " identity reset must be uncertain") uncertain
          (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
            let saved=ok (search_save selected
              ~criteria:Imap.Search.All) in
            mutate saved))))
      ["STORE",(fun saved -> Result.map (fun _ -> ()) (store saved));
       "COPY",(fun saved -> Result.map (fun _ -> ()) (R.uid_copy_saved saved ~mailbox:"Archive"));
       "MOVE",(fun saved -> Result.map (fun _ -> ()) (R.uid_move_saved saved ~mailbox:"Archive"));
       "EXPUNGE",R.uid_expunge_saved]) ["UIDVALIDITY 2";"CLOSED"]

let test_concurrent_invalidation () =
  Eio_mock.Backend.run @@ fun () ->
  let entered,mark_entered=Eio.Promise.create () in
  let release,mark_release=Eio.Promise.create () in
  with_client [`Return (selected 4);`Return (save 5 2L);
    `Run (fun () -> Eio.Promise.resolve mark_entered ();
      Eio.Promise.await release; "* SEARCH 3\r\n" ^ done_ 6);
    `Return (done_ 7)] (fun ~sw client ->
    ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
      let saved=ok (search_save selected ~criteria:Imap.Search.All) in
      let searched,mark_searched=Eio.Promise.create () in
      let stored,mark_stored=Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () -> Eio.Promise.resolve mark_searched
        (S.uid_search selected
          ~criteria:(Imap.Search.Raw "RETURN (SAVE) UID 3")));
      Eio.Promise.await entered;
      Eio.Fiber.fork ~sw (fun () -> Eio.Promise.resolve mark_stored (store saved));
      Eio.Promise.resolve mark_release ();
      ignore (ok (Eio.Promise.await searched));
      expect "queued saved STORE invalidated before dispatch" state (Eio.Promise.await stored);
      Ok ())))

let test_saved_refinement () =
  with_client [`Return (selected 4);`Return (save 5 3L);
    `Return ("* ESEARCH (TAG \"A00000006\") UID COUNT 1 ALL 7\r\n" ^ done_ 6);
    `Return ("* ESEARCH (TAG \"A00000007\") UID COUNT 0\r\n" ^ done_ 7);
    `Return (done_ 8);`Return (done_ 9)] (fun ~sw:_ client ->
    ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
      let saved=ok (search_save selected ~criteria:Imap.Search.All) in
      let found=ok (R.uid_search_saved saved ~criteria:Imap.Search.Unseen) in
      if List.map Imap.Uid.to_int64 found<>[7L] then
        failwith "saved refinement lost subset";
      if ok (R.uid_search_saved saved ~criteria:(uids "100"))<>[] then
        failwith "empty saved refinement changed";
      expect "refinement grammar escape rejected" state
        (R.uid_search_saved saved
          ~criteria:(Imap.Search.Raw "ALL) RETURN (SAVE) ("));
      ignore (ok (store saved));
      Ok ())))

let test_invalid_refinement () =
  List.iter (fun fields ->
    with_client [`Return (selected 4);`Return (save 5 2L);
      `Return ("* ESEARCH (TAG \"A00000006\") UID " ^ fields ^ "\r\n" ^ done_ 6);
      `Return (done_ 7)] (fun ~sw:_ client ->
      expect "invalid saved refinement" protocol
        (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
          let saved=ok (search_save selected ~criteria:Imap.Search.All) in
          R.uid_search_saved saved ~criteria:Imap.Search.All))))
    ["COUNT 3 ALL 1:3";"COUNT 2 ALL 3,3";"COUNT 2";
     "ALL 3,7";"COUNT 0 ALL 3";"COUNT 2 PARTIAL (1:2 3,7)"]

let test_saved_failures () =
  List.iter (fun mutate ->
    with_client [`Return (selected 4);`Return (save 5 2L);`Raise End_of_file]
      (fun ~sw:_ client -> expect "lost saved mutation completion" uncertain
        (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
          mutate (ok (search_save selected ~criteria:Imap.Search.All))))))
    [(fun saved -> Result.map (fun _ -> ()) (store saved));
     (fun saved -> Result.map (fun _ -> ()) (R.uid_copy_saved saved ~mailbox:"Archive"));
     (fun saved -> Result.map (fun _ -> ()) (R.uid_move_saved saved ~mailbox:"Archive"));
     R.uid_expunge_saved];
  with_client ~caps:"SEARCHRES IDLE" [
    `Return (selected 4);`Return (save 5 2L);
    `Return "+ idling\r\n";`Return "* OK [UIDVALIDITY 2] changed\r\n"]
    (fun ~sw:_ client ->
      ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
        let saved=ok (search_save selected ~criteria:Imap.Search.All) in
        expect "IDLE epoch reset" protocol
          (Result.bind (S.Idle.require selected) S.Idle.wait_for_change);
        expect "IDLE reset expires saved handle" state (fetch saved);
        expect "IDLE reset expires lease info" state (S.info selected);
        Ok ())))

let test_mutation_receipts () =
  List.iter (fun (reply,mutate) ->
    with_client [`Return (selected 4);`Return (save 5 2L);`Return reply]
      (fun ~sw:_ client -> expect "invalid saved mutation receipt" uncertain
        (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
          mutate (ok (search_save selected ~criteria:Imap.Search.All))))))
    ["* OK [COPYUID 2 3 20] copied\r\n" ^ tag 6 ^
       " OK [COPYUID 2 3 20] copied\r\n",
       (fun saved -> Result.map (fun _ -> ()) (R.uid_copy_saved saved ~mailbox:"Archive"));
     tag 6 ^ " OK [COPYUID 2 3,7 20] copied\r\n",
       (fun saved -> Result.map (fun _ -> ()) (R.uid_move_saved saved ~mailbox:"Archive"));
     tag 6 ^ " OK [MODIFIED 0] stored\r\n",
       (fun saved -> Result.map (fun _ -> ()) (store saved))]

let test_truncated_save () =
  List.iter (fun partial ->
    with_client ~caps:"SEARCHRES MESSAGELIMIT=1"
      [`Return (selected 4);`Return (save 5 2L);`Return partial;
       `Return (done_ 7)] (fun ~sw:_ client ->
      ok (C.with_mailbox client ~mode:`Read_only "INBOX" (fun selected ->
        let previous=ok (search_save selected
          ~criteria:Imap.Search.All) in
        expect "truncated SAVE cannot mint a handle"
          (function E.Limit _ -> true | _ -> false)
          (search_save selected ~criteria:Imap.Search.All);
        expect "truncated SAVE invalidates previous handle" state (fetch previous);
        Ok ()))))
    ["* ESEARCH (TAG \"A00000006\") UID COUNT 1\r\n" ^
       tag 6 ^ " OK [MESSAGELIMIT 1 7] partial\r\n";
     "* ESEARCH (TAG \"A00000006\") UID COUNT 1\r\n" ^
       "* NO [MESSAGELIMIT 1 7] partial\r\n" ^ done_ 6];
  with_client ~caps:"SEARCHRES SAVELIMIT=1"
    [`Return (selected 4);`Return (save 5 2L);`Return (done_ 6)]
    (fun ~sw:_ client ->
      ok (C.with_mailbox client ~mode:`Read_only "INBOX" (fun selected ->
        let saved=ok (search_save selected ~criteria:Imap.Search.All) in
        if R.saved_search_count saved<>2L then failwith "SAVELIMIT truncated SEARCH";
        Ok ())))

let test_typed_criteria_gates () =
  let modseq=match Imap.Modseq.of_int64 5L with
    | Ok modseq -> modseq | Error e -> failwith e in
  with_client ~caps:"" [`Return (selected 4);`Return (done_ 5)]
    (fun ~sw:_ client ->
      ok (C.with_mailbox client ~mode:`Read_only "INBOX" (fun selected ->
        expect "MODSEQ criterion needs CONDSTORE"
          (unsupported Imap.Capability.Condstore)
          (S.uid_search selected ~criteria:(Modseq modseq));
        expect "saved criterion needs SEARCHRES"
          (unsupported Imap.Capability.Searchres)
          (S.uid_search selected ~criteria:(Not Saved));
        expect "non-ASCII criterion needs UTF-8" state
          (S.uid_search selected ~criteria:(Subject "caf\xc3\xa9"));
        expect "invalid typed criterion" state
          (S.uid_search selected ~criteria:(Larger (-1L)));
        Ok ())))

let () =
  test_truncated_save ();
  test_operations (); test_empty (); test_replacement_and_raw_search ();
  test_rejected_search (); test_invalid_save_results (); test_gates ();
  test_identity_reset (); test_concurrent_invalidation ();
  test_saved_refinement (); test_invalid_refinement (); test_saved_failures (); test_mutation_receipts ();
  test_typed_criteria_gates ()
