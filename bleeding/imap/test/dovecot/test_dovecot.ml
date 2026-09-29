(* [all_pages page id] concatenates the pages of 1,000 that [page] reads
   after the ID of the last row of the previous page. *)
let all_pages page id =
  let rec go after acc =
    let rows=page after in
    let acc=List.rev_append rows acc in
    if List.length rows<1000 then List.rev acc
    else go (Some (id (List.nth rows (List.length rows-1)))) acc in
  go None []
let all_pairs store ~scope =
  all_pages (fun after ->
    Imap_store.Journal.pairs_page store ~scope ?after ~limit:1000 ())
    (fun (p:Imap_store.Journal.pair) -> p.id)
let all_open_conflicts store ~scope =
  all_pages (fun after ->
    Imap_store.Journal.open_conflicts_page store ~scope ?after ~limit:1000 ())
    (fun (c:Imap_store.Journal.conflict) -> c.id)
let all_active_operations store ~scope =
  all_pages (fun after ->
    Imap_store.Journal.active_operations_page store ~scope ?after
      ~limit:1000 ())
    (fun (o:Imap_store.Journal.operation) -> o.id)

let unwrap = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Imap_eio.Client.error_to_string e)

(* Test conveniences over [Maildir]: a format or policy error fails the
   test, and each mutation takes its own writer. *)
module Md = struct
  include Maildir
  let ok = function
    | Ok x -> x
    | Error e -> Alcotest.failf "unexpected Maildir error: %a" pp_error e
  let open_dir path = ok (open_dir path)
  let scan m = ok (scan m)
  let find m ~id = ok (find m ~id)
  let append m ?id ~source ~length ~flags ?mtime () =
    ok (with_writer m (fun w -> append w ?id ~source ~length ~flags ?mtime ()))
  let set_flags m o flags = ok (with_writer m (fun w -> set_flags w o flags))
  let remove m o = with_writer m (fun w -> remove w o)
end

(* [sync_ctx ~client ~store ~scope ~mailbox ~spool_dir ?next_id ()] is the
   context of one sync call, and a refused context fails the test. The
   default [next_id] fails the test, since a call that took no ID source
   before contexts existed must not journal. *)
let sync_ctx ~client ~store ~scope ~mailbox ~spool_dir
    ?(next_id=fun () -> Alcotest.fail "sync call requested an ID") () =
  match Imap_sync.Ctx.v ~client ~store ~scope ~mailbox ~spool_dir ~next_id
  with
  | Ok ctx -> ctx
  | Error e -> Alcotest.failf "sync context refused: %a" Imap_sync.Error.pp e

let u n = match Imap.Uid.of_int64 n with
  | Ok uid -> uid | Error e -> Alcotest.fail e
let raw_uids = List.map Imap.Uid.to_int64
let uid_expunge selected ~set =
  Result.bind (Imap_eio.Selected.Uidplus.require selected) (fun uidplus ->
    Imap_eio.Selected.Uidplus.uid_expunge uidplus ~set)
let wires = List.map Mail_flag.Imap_flag.to_wire
let flag_of_wire name = match Mail_flag.Imap_flag.of_wire name with
  | Ok flag -> flag | Error message -> Alcotest.fail message

let mtime date = match Local_date.to_mtime date with
  | Ok mtime -> mtime | Error message -> Alcotest.fail message
let occurrence_date local = Result.to_option (Local_date.of_occurrence local)

let env name = match Sys.getenv_opt name with
  | Some s when s <> "" -> s
  | _ -> Alcotest.fail (name ^ " is unset")

let cli_job ~env argv =
  match Cmdliner.Cmd.eval_value' ~env ~argv Imap_cli.cmd with
  | `Ok job -> job
  | `Exit code -> Alcotest.failf "imap-sync command line exited %d" code

let configured () =
  if Sys.getenv_opt "IMAP_DOVECOT_HOST" = None then
    if Sys.getenv_opt "IMAP_DOVECOT_REQUIRED" = Some "1" then
      Alcotest.fail "IMAP_DOVECOT_REQUIRED=1 but IMAP_DOVECOT_HOST is unset"
    else Alcotest.skip ()

let connect env_io sw =
  let host = env "IMAP_DOVECOT_HOST" in
  let port = int_of_string (env "IMAP_DOVECOT_PORT") in
  let transport = Imap_eio.Transport.v
    ~net:(Eio.Stdenv.net env_io) ~host ~port ~tls:`Plain () in
  let auth = Imap_eio.Auth.password ~username:(env "IMAP_DOVECOT_USER")
    ~password:(env "IMAP_DOVECOT_PASSWORD") ~mechanism:`Cram_md5 () in
  transport, unwrap (Imap_eio.Client.connect ~sw ~auth transport)

let test_auth () =
  configured ();
  Eio_main.run @@ fun env_io ->
    Eio.Switch.run @@ fun sw ->
    let transport, client = connect env_io sw in
    (* Explicit CRAM-MD5 refuses to authenticate unless pre-auth CAPABILITY
       advertised it. Dovecot drops AUTH= after login, as expected. *)
    let boxes = unwrap (Imap_eio.Client.list client ~pattern:"INBOX" ()) in
    Alcotest.(check bool) "INBOX listed" true (boxes <> []);
    ignore (unwrap (Imap_eio.Client.noop client));
    unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
      (fun selected -> Result.map (fun _ -> ()) (Imap_eio.Selected.noop selected)));
    unwrap (Imap_eio.Client.logout client);
    Alcotest.(check bool) "LOGOUT released transport" false (Imap_eio.Client.is_open client);
    let wrong = Imap_eio.Auth.password ~username:(env "IMAP_DOVECOT_USER")
      ~password:"deliberately-wrong" ~mechanism:`Cram_md5 () in
    match Imap_eio.Client.connect ~sw ~auth:wrong transport with
    | Error (Imap_eio.Error.Rejected {code=Some Imap.Response.Authenticationfailed;
        text="authentication rejected"; _}) -> ()
    | Error e -> Alcotest.fail ("wrong rejection: " ^
        Imap_eio.Client.error_to_string e)
    | Ok c ->
        Imap_eio.Client.close c;
        Alcotest.fail "Dovecot accepted a wrong CRAM-MD5 password"

let test_bounded_pool () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let pool=Imap_eio.Pool.create ~sw ~max_connections:2
    ~connect:(fun ~sw ->
      let _,client=connect env_io sw in
      Ok client) in
  let list ()=unwrap (Imap_eio.Pool.use pool (fun client ->
    Eio.Fiber.yield ();
    Imap_eio.Client.list client ~pattern:"INBOX" ())) in
  let first,second=Eio.Fiber.pair list list in
  Alcotest.(check bool) "pooled concurrent INBOX discovery" true
    (first<>[] && second<>[]);
  unwrap (Imap_eio.Pool.use pool (fun client ->
    Imap_eio.Client.close client;
    Ok ()));
  Alcotest.(check bool) "pooled reconnect after close" true
    (list ()<>[])

let test_mailbox_management () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let old_name="Oxmono-Dovecot-" ^ nonce ^ "-Old" in
  let new_name="Oxmono-Dovecot-" ^ nonce ^ "-New" in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.unsubscribe_mailbox client ~mailbox:old_name);
    ignore (Imap_eio.Client.unsubscribe_mailbox client ~mailbox:new_name);
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox:old_name);
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox:new_name);
    Imap_eio.Client.close client) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox:old_name);
  unwrap (Imap_eio.Client.subscribe_mailbox client ~mailbox:old_name);
  let subscribed=unwrap (Imap_eio.Client.lsub client ~pattern:old_name ()) in
  Alcotest.(check bool) "LSUB includes subscribed mailbox" true
    (List.exists (fun (row:Imap_eio.Client.mailbox_entry) ->
      row.name.utf8=Ok old_name) subscribed);
  unwrap (Imap_eio.Client.unsubscribe_mailbox client ~mailbox:old_name);
  unwrap (Imap_eio.Client.rename_mailbox client ~old_name ~new_name);
  let renamed=unwrap (Imap_eio.Client.list client ~pattern:new_name ()) in
  Alcotest.(check bool) "RENAME exposes destination" true
    (List.exists (fun (row:Imap_eio.Client.mailbox_entry) ->
      row.name.utf8=Ok new_name) renamed);
  unwrap (Imap_eio.Client.subscribe_mailbox client ~mailbox:new_name);
  let subscribed=unwrap (Imap_eio.Client.lsub client ~pattern:new_name ()) in
  Alcotest.(check bool) "LSUB includes renamed subscription" true
    (List.exists (fun (row:Imap_eio.Client.mailbox_entry) ->
      row.name.utf8=Ok new_name) subscribed);
  unwrap (Imap_eio.Client.unsubscribe_mailbox client ~mailbox:new_name);
  unwrap (Imap_eio.Client.delete_mailbox client ~mailbox:new_name)

let test_binary_append () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let mailbox=Printf.sprintf "Oxmono-Dovecot-%d-%06x-BinaryAppend"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let decoded="binary\000payload\255\128\r\n" in
  let raw="From: binary@example.test\r\nSubject: binary append\r\n" ^
    "MIME-Version: 1.0\r\nContent-Type: application/octet-stream\r\n" ^
    "Content-Transfer-Encoding: binary\r\n\r\n" ^ decoded in
  let date=match Imap.Internal_date.of_string "26-Sep-2025 12:34:56 +0230" with
    | Ok date -> date | Error message -> Alcotest.fail message in
  let receipt=match unwrap (Imap_eio.Client.append client ~mailbox ~binary:true
    (Imap_eio.Client.append_message ~flags:[flag_of_wire "\\Flagged"]
       ~internal_date:date ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))) with
    | Some receipt -> receipt | None -> Alcotest.fail "binary APPEND omitted APPENDUID" in
  let uid=receipt.uid in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
    (fun selected ->
      let info=unwrap (Imap_eio.Selected.info selected) in
      Alcotest.(check bool) "binary APPEND destination epoch"
        true (info.uidvalidity=Imap.Uidvalidity.to_int64 receipt.uidvalidity);
      let output=Buffer.create 32 in
      let binary=unwrap (Imap_eio.Selected.Binary.require selected) in
      let length=unwrap (Imap_eio.Selected.Binary.fetch_binary_to binary ~uid
        ~section:[1] (Eio.Flow.buffer_sink output)) in
      Alcotest.(check (option int64)) "binary append decoded length"
        (Some (Int64.of_int (String.length decoded))) length;
      Alcotest.(check string) "binary append decoded octets" decoded (Buffer.contents output);
      let rows=unwrap (Imap_eio.Selected.fetch selected ~uids:[uid]
        ~items:[Imap.Fetch_item.Internal_date]) in
      Alcotest.(check bool) "binary append flags and date" true
        (match rows with
         | [{Imap_eio.Selected.flags=Some flags;internal_date=Some actual;_}] ->
             List.mem "\\Flagged" (wires flags) &&
             Imap.Internal_date.equal_instant date actual
         | _ -> false);
      Ok ()))

let test_rejection_codes () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let mailbox=Printf.sprintf "Oxmono-Dovecot-%d-%06x-Codes"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  let missing=mailbox ^ "-Missing" in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client) @@ fun () ->
  let rejected expected = function
    | Error (Imap_eio.Error.Rejected {status=`No;code=Some actual;_})
      when actual=expected -> ()
    | Error error -> Alcotest.fail ("unexpected rejection: " ^
        Imap_eio.Client.error_to_string error)
    | Ok _ -> Alcotest.fail "command unexpectedly succeeded" in
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  rejected Imap.Response.Alreadyexists
    (Imap_eio.Client.create_mailbox client ~mailbox);
  rejected Imap.Response.Nonexistent
    (Imap_eio.Client.with_mailbox client ~mode:`Read_only missing
      (fun _ -> Alcotest.fail "missing selection invoked callback"));
  rejected Imap.Response.Trycreate
    (Result.map ignore (Imap_eio.Client.append client ~mailbox:missing
      (Imap_eio.Client.append_message ~length:1L
         (Eio.Flow.string_source "x"))));
  let boxes=unwrap (Imap_eio.Client.list client ~pattern:mailbox ()) in
  Alcotest.(check bool) "connection usable after typed rejections" true
    (List.exists (fun (row:Imap_eio.Client.mailbox_entry) ->
      row.name.utf8=Ok mailbox) boxes)

let test_binary_sections () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let mailbox=Printf.sprintf "Oxmono-Dovecot-%d-%06x-Binary"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let decoded="\000\255hello\r\nbinary\128end" in
  let raw="From: binary@example.test\r\nSubject: decoded sections\r\n" ^
    "MIME-Version: 1.0\r\nContent-Type: multipart/mixed; boundary=oxbinary\r\n\r\n" ^
    "--oxbinary\r\nContent-Type: text/plain; charset=utf-8\r\n" ^
    "Content-Transfer-Encoding: quoted-printable\r\n\r\nhello=20world=21\r\n" ^
    "--oxbinary\r\nContent-Type: application/octet-stream\r\n" ^
    "Content-Transfer-Encoding: base64\r\n\r\nAP9oZWxsbw0KYmluYXJ5gGVuZA==\r\n" ^
    "--oxbinary--\r\n" in
  let uid=match unwrap (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message
       ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))) with
    | Some receipt -> receipt.uid
    | None -> Alcotest.fail "BINARY fixture requires APPENDUID" in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
    (fun selected ->
      let module S=Imap_eio.Selected in
      let binary=unwrap (S.Binary.require selected) in
      let fetch ?partial section expected =
        let output=Buffer.create 32 in
        let result=unwrap (S.Binary.fetch_binary_to binary ~uid ~section
          ?partial ~max_bytes:1024L (Eio.Flow.buffer_sink output)) in
        Alcotest.(check (option int64)) "decoded length"
          (Some (Int64.of_int (String.length expected))) result;
        Alcotest.(check string) "decoded exact octets" expected (Buffer.contents output) in
      fetch [2] decoded;
      fetch ~partial:(2L,5L) [2] "hello";
      fetch ~partial:(2L,100L) [2] (String.sub decoded 2 (String.length decoded-2));
      fetch ~partial:(1000L,10L) [2] "";
      fetch [1] "hello world!";
      let sizes=unwrap (S.fetch selected ~uids:[uid]
        ~items:[Imap.Fetch_item.Binary_size [2]]) in
      Alcotest.(check bool) "decoded BINARY.SIZE agrees with octets" true
        (match sizes with
         | [{S.uid=got;binary_sizes=[[2],size];_}] ->
             Imap.Uid.equal got uid &&
             size=Int64.of_int (String.length decoded)
         | _ -> false);
      let absent=Buffer.create 1 in
      (match S.Binary.fetch_binary_to binary ~uid:(u 4_294_967_295L)
          ~section:[2]
          (Eio.Flow.buffer_sink absent) with
       | Error (Imap_eio.Error.Missing_uid _) -> ()
       | Error error -> Alcotest.fail (Imap_eio.Client.error_to_string error)
       | Ok _ -> Alcotest.fail "BINARY fetch accepted nonexistent UID");
      Alcotest.(check int) "missing UID produced no body" 0 (Buffer.length absent);
      let archived=Buffer.create 256 in
      unwrap (S.fetch_to selected ~uid (Eio.Flow.buffer_sink archived));
      Alcotest.(check string) "raw BODY stays transfer-encoded" raw (Buffer.contents archived);
      let rows=unwrap (S.fetch_range selected ~first:uid ~last:uid ~items:[]) in
      Alcotest.(check bool) "BINARY.PEEK preserves unseen flag" true
        (match rows with
         | [{S.flags=Some flags;_}] -> not (List.mem "\\Seen" (wires flags))
         | _ -> false);
      Ok ()))

let test_saved_search () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let prefix=Printf.sprintf "Oxmono-Dovecot-%d-%06x-Saved"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  let source=prefix ^ "-Source" and copied=prefix ^ "-Copy"
  and moved=prefix ^ "-Move" in
  Fun.protect ~finally:(fun () ->
    List.iter (fun mailbox ->
      ignore (Imap_eio.Client.delete_mailbox client ~mailbox))
      [source;copied;moved];
    Imap_eio.Client.close client) @@ fun () ->
  List.iter (fun mailbox ->
    unwrap (Imap_eio.Client.create_mailbox client ~mailbox))
    [source;copied;moved];
  let append subject =
    let body="From: saved@example.test\r\nSubject: " ^ subject ^
      "\r\n\r\nSynthetic saved-search body\r\n" in
    match unwrap (Imap_eio.Client.append client ~mailbox:source
      (Imap_eio.Client.append_message
         ~length:(Int64.of_int (String.length body))
         (Eio.Flow.string_source body))) with
    | Some receipt -> Imap.Uid.to_int64 receipt.uid
    | None -> Alcotest.fail "SEARCHRES fixture requires APPENDUID" in
  let first=append "selected-first" in
  let second=append "selected-second" in
  let keeper=append "untouched" in
  let module S=Imap_eio.Selected in
  let module R=S.Searchres in
  let search_save selected ~criteria=
    Result.bind (R.require selected) (fun searchres ->
      R.uid_search_save searchres ~criteria) in
  let fetch saved=unwrap (R.uid_fetch_saved saved ~items:[Imap.Fetch_item.Flags]
    ()) in
  let uids rows=List.map (fun (row:S.row) -> Imap.Uid.to_int64 row.uid) rows
    |> List.sort Int64.compare in
  let stale name result=match result with
    | Error (Imap_eio.Error.State _) -> ()
    | Error error -> Alcotest.fail (name ^ ": " ^ Imap_eio.Client.error_to_string error)
    | Ok _ -> Alcotest.fail (name ^ " accepted stale saved result") in
  let escaped=unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write source
    (fun selected ->
      let saved=unwrap (search_save selected
        ~criteria:(Imap.Search.Subject "selected-")) in
      Alcotest.(check int64) "saved capture count" 2L (R.saved_search_count saved);
      Alcotest.(check (list int64)) "saved fetch selects exact fixture subset"
        [first;second] (uids (fetch saved));
      Alcotest.(check (list int64)) "search within saved subset" [first]
        (raw_uids (unwrap (R.uid_search_saved saved
          ~criteria:(Imap.Search.Subject "selected-first"))));
      Alcotest.(check (list int64)) "subset query preserves saved variable"
        [first;second] (uids (fetch saved));
      ignore (unwrap (R.uid_store_saved saved ~operation:`Add
        ~flags:[Mail_flag.Imap_flag.system Flagged] ()));
      let check_receipt label result=match unwrap result with
        | Some (receipt:S.copy_receipt) -> Alcotest.(check int64) label 2L
            (Imap.Uid_set.cardinality receipt.destination)
        | None -> Alcotest.fail "saved transfer omitted COPYUID" in
      check_receipt "saved COPY receipt" (R.uid_copy_saved saved ~mailbox:copied);
      check_receipt "saved MOVE receipt" (R.uid_move_saved saved ~mailbox:moved);
      Alcotest.(check (list int64)) "expunged saved members disappear" []
        (uids (fetch saved));
      Alcotest.(check int64) "capture count is not a live count" 2L
        (R.saved_search_count saved);
      unwrap (R.uid_expunge_saved saved);
      let replacement=unwrap (search_save selected
        ~criteria:Imap.Search.All) in
      stale "replaced saved FETCH"
        (R.uid_fetch_saved saved ~items:[Imap.Fetch_item.Flags] ());
      stale "replaced saved STORE" (R.uid_store_saved saved ~operation:`Add
        ~flags:[Mail_flag.Imap_flag.system Deleted] ());
      let rows=fetch replacement in
      Alcotest.(check (list int64)) "unmatched keeper survives" [keeper] (uids rows);
      Alcotest.(check bool) "stale STORE did not modify keeper" true
        (List.for_all (fun (row:S.row) -> row.flags=Some []) rows);
      ignore (unwrap (R.uid_store_saved replacement ~operation:`Add
        ~flags:[Mail_flag.Imap_flag.system Deleted] ()));
      unwrap (R.uid_expunge_saved replacement);
      Alcotest.(check (list int64)) "targeted saved EXPUNGE empties selection" []
        (uids (fetch replacement));
      let empty=unwrap (search_save selected ~criteria:Imap.Search.All) in
      Alcotest.(check int64) "empty saved count" 0L (R.saved_search_count empty);
      Alcotest.(check (list int64)) "empty saved FETCH" [] (uids (fetch empty));
      ignore (unwrap (S.uid_search selected
        ~criteria:(Imap.Search.Raw "RETURN (SAVE COUNT) ALL")));
      stale "raw SEARCH invalidates saved handle" (R.uid_fetch_saved empty
        ~items:[Imap.Fetch_item.Flags] ());
      let last=unwrap (search_save selected ~criteria:Imap.Search.All) in
      Ok last)) in
  stale "saved handle outlives selected lease"
    (R.uid_fetch_saved escaped ~items:[Imap.Fetch_item.Flags] ());
  List.iter (fun mailbox ->
    unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
      (fun selected ->
        let rows=unwrap (S.fetch_range selected ~first:(u 1L)
          ~last:(u 100L) ~items:[]) in
        Alcotest.(check int) "saved transfer destination count" 2 (List.length rows);
        Alcotest.(check bool) "saved STORE flags survived transfer" true
          (List.for_all (fun (row:S.row) ->
            Option.fold ~none:false
              ~some:(fun flags -> List.mem "\\Flagged" (wires flags))
              row.flags) rows);
        Ok ()))) [copied;moved]

let test_sort_thread () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let mailbox=Printf.sprintf "Oxmono-Dovecot-%d-%06x-SortThread"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let append ~id ~subject ~day ?parent () =
    let refs=match parent with None -> "" | Some parent ->
      "References: <" ^ parent ^ "@example.test>\r\n" ^
      "In-Reply-To: <" ^ parent ^ "@example.test>\r\n" in
    let body=Printf.sprintf
      "From: sender@example.test\r\nTo: recipient@example.test\r\nMessage-ID: <%s@example.test>\r\nDate: %02d Jan 2020 12:00:00 +0000\r\nSubject: %s\r\n%s\r\nSynthetic body %s\r\n"
      id day subject refs id in
    match unwrap (Imap_eio.Client.append client ~mailbox
      (Imap_eio.Client.append_message
         ~length:(Int64.of_int (String.length body))
         (Eio.Flow.string_source body))) with
    | Some receipt -> receipt.uid
    | None -> Alcotest.fail "SORT fixture requires APPENDUID" in
  let removed=append ~id:"removed" ~subject:"Removed" ~day:1 () in
  let root=append ~id:"root" ~subject:"Zebra" ~day:2 () in
  let first=append ~id:"first" ~subject:"Re: Zebra" ~day:3 ~parent:"root" () in
  let second=append ~id:"second" ~subject:"Re: Zebra" ~day:4 ~parent:"root" () in
  let solo=append ~id:"solo" ~subject:"Alpha" ~day:5 () in
  let raw=Imap.Uid.to_int64 in
  let root=raw root and first=raw first and second=raw second and solo=raw solo in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
    (fun selected ->
      let set=Imap.Uid_set.singleton removed in
      ignore (unwrap (Imap_eio.Selected.uid_store_flags selected ~set
        ~operation:`Add ~flags:[Mail_flag.Imap_flag.system Deleted]));
      unwrap (uid_expunge selected ~set);
      Ok ()));
  (* Removing the first occurrence makes sequence numbers differ from UIDs. *)
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
    (fun selected ->
      let top=Imap.Search.Uid (Imap.Uid_set.singleton (u 4294967295L)) in
      let sorting=unwrap (Imap_eio.Selected.Sort.require selected) in
      let esort=unwrap (Imap_eio.Selected.Esort.require selected) in
      let sort order criteria=raw_uids (unwrap (Imap_eio.Selected.Sort.uid_sort
        sorting ~keys:[Imap.Sort.Subject,order] ~charset:"UTF-8"
        ~criteria)) in
      Alcotest.(check (list int64)) "ascending subject, stable ties"
        [solo;root;first;second] (sort Imap.Sort.Ascending Imap.Search.All);
      Alcotest.(check (list int64)) "reverse subject, stable ties"
        [root;first;second;solo] (sort Imap.Sort.Descending Imap.Search.All);
      Alcotest.(check (list int64)) "empty sort" []
        (sort Imap.Sort.Ascending top);
      let extended returns order criteria =
        unwrap (Imap_eio.Selected.Esort.uid_sort_extended esort ~returns
          ~keys:[Imap.Sort.Subject,order] ~charset:"UTF-8" ~criteria) in
      let summary=extended [Imap.Sort.Min;Max;Count]
        Imap.Sort.Ascending Imap.Search.All in
      Alcotest.(check int64) "ESORT count" 4L summary.count;
      Alcotest.(check (option int64)) "ESORT first follows subject order"
        (Some solo) (Option.map raw summary.first);
      Alcotest.(check (option int64)) "ESORT last follows subject order"
        (Some second) (Option.map raw summary.last);
      Alcotest.(check bool) "summary avoids UID materialization" true
        (summary.uids=None && summary.range=None);
      let ordered=extended [] Imap.Sort.Descending Imap.Search.All in
      Alcotest.(check (option (list int64))) "ESORT default ALL preserves order"
        (Some [root;first;second;solo]) (Option.map raw_uids ordered.uids);
      Alcotest.(check int64) "ESORT default ALL count" 4L ordered.count;
      let empty=extended [Imap.Sort.All;Min;Max]
        Imap.Sort.Ascending top in
      Alcotest.(check int64) "ESORT empty count" 0L empty.count;
      Alcotest.(check (option (list int64))) "ESORT explicit empty UID result"
        (Some []) (Option.map raw_uids empty.uids);
      Alcotest.(check bool) "empty ESORT has no boundary UIDs" true
        (empty.first=None && empty.last=None);
      let references=unwrap
        (Imap_eio.Selected.Thread.require selected Imap.Thread.References) in
      let threads criteria=unwrap (Imap_eio.Selected.Thread.uid_thread
        references ~charset:"UTF-8" ~criteria) in
      let node uid children : Imap_eio.Selected.thread =
        {uid=Option.map u uid;children} in
      let expected=[node (Some root)
        [node (Some first) [];node (Some second) []];node (Some solo) []] in
      Alcotest.(check bool) "REFERENCES preserves sibling tree and UIDs" true
        (threads Imap.Search.All=expected);
      let by_subject=unwrap (Result.bind
        (Imap_eio.Selected.Thread.require selected Imap.Thread.Orderedsubject)
        (fun ordered -> Imap_eio.Selected.Thread.uid_thread ordered
          ~charset:"UTF-8" ~criteria:Imap.Search.All)) in
      Alcotest.(check bool) "ORDEREDSUBJECT preserves sibling tree and UIDs" true
        (by_subject=expected);
      Alcotest.(check bool) "filtered parent retained as dummy node" true
        (threads (Imap.Search.Uid (Imap.Uid_set.of_list [u first;u second]))=
         [node None [node (Some first) [];node (Some second) []]]);
      Alcotest.(check bool) "empty thread" true
        (threads top=[]);
      Ok ()))

let test_internal_date_roundtrip () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let mailbox=Printf.sprintf "Oxmono-Dovecot-%d-%06x-Date"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let date=match Imap.Internal_date.of_string
    "26-Sep-2025 12:34:56 +0000" with
    | Ok date -> date | Error e -> Alcotest.fail e in
  let message="From: date@example.test\r\nSubject: date\r\n\r\nBody\r\n" in
  let receipt=match unwrap (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message ~internal_date:date
       ~length:(Int64.of_int (String.length message))
       (Eio.Flow.string_source message))) with
    | Some receipt -> receipt
    | None -> Alcotest.fail "Dovecot omitted APPENDUID" in
  let uid=receipt.uid in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
    (fun selected ->
      match Imap_eio.Selected.fetch selected ~uids:[uid]
        ~items:[Imap.Fetch_item.Internal_date] with
      | Error _ as error -> error
      | Ok rows ->
          (match rows with
           | [row] when Imap.Uid.equal row.uid uid ->
               Alcotest.(check (option string)) "Dovecot INTERNALDATE"
                 (Some "26-Sep-2025 12:34:56 +0000")
                 (Option.map Imap.Internal_date.to_string row.internal_date)
           | _ -> Alcotest.fail "Dovecot omitted dated FETCH row");
          Ok ()))

let test_typed_mime_fetch () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let mailbox=Printf.sprintf "Oxmono-Dovecot-%d-%06x-MIME"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let message=String.concat "\r\n" [
    "From: Alice <alice@example.test>";
    "To: Bob <bob@example.test>";
    "Subject: multipart fixture";
    "Message-ID: <multipart-fixture@example.test>";
    "MIME-Version: 1.0";
    "Content-Type: multipart/mixed; boundary=oxmono-boundary";
    "";
    "--oxmono-boundary";
    "Content-Type: text/plain; charset=utf-8";
    "";
    "Hello from the text part.";
    "--oxmono-boundary";
    "Content-Type: application/octet-stream";
    "Content-Transfer-Encoding: base64";
    "Content-Disposition: attachment; filename=fixture.bin";
    "";
    "AQIDBA==";
    "--oxmono-boundary--";
    "";
  ] in
  let receipt=match unwrap (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message
       ~length:(Int64.of_int (String.length message))
       (Eio.Flow.string_source message))) with
    | Some receipt -> receipt
    | None -> Alcotest.fail "Dovecot omitted MIME APPENDUID" in
  let uid=receipt.uid in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
    (fun selected ->
      let ( let* ) result f=match result with
        | Ok value -> f value | Error _ as error -> error in
      let* envelopes=Imap_eio.Selected.fetch selected
        ~uids:[uid] ~items:[Imap.Fetch_item.Envelope] in
      (match envelopes with
       | [{envelope=Some {subject=Some "multipart fixture";
           message_id=Some "<multipart-fixture@example.test>";_};_}] -> ()
       | _ -> Alcotest.fail "Dovecot typed ENVELOPE mismatch");
      let* structures=Imap_eio.Selected.fetch selected
        ~uids:[uid] ~items:[Imap.Fetch_item.Bodystructure] in
      (match structures with
       | [{bodystructure=Some (Imap.Response.Multipart
           {parts=[Imap.Response.Single_part first;
                   Imap.Response.Single_part second];subtype;_});_}]
           when String.uppercase_ascii subtype="MIXED" &&
             String.uppercase_ascii first.media_type="TEXT" &&
             String.uppercase_ascii first.subtype="PLAIN" &&
             String.uppercase_ascii second.media_type="APPLICATION" &&
             String.uppercase_ascii second.subtype="OCTET-STREAM" -> ()
       | _ -> Alcotest.fail "Dovecot typed BODYSTRUCTURE mismatch");
      Ok ()))

let tls_authenticator path =
  let pem = In_channel.with_open_bin path In_channel.input_all in
  let ca = match X509.Certificate.decode_pem pem with
    | Ok ca -> ca
    | Error (`Msg message) -> Alcotest.fail message in
  let time () = Some (Ptime_clock.now ()) in
  X509.Authenticator.chain_of_trust_no_crl ~time [ca]

let test_compress () =
  configured ();
  let trusted=tls_authenticator (env "IMAP_DOVECOT_CA_CERT") in
  Eio_main.run @@ fun env_io ->
  List.iteri (fun index (port,tls) ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env_io) 30. (fun () ->
      Eio.Switch.run @@ fun sw ->
      let transport=Imap_eio.Transport.v ~net:(Eio.Stdenv.net env_io)
        ~host:(env "IMAP_DOVECOT_HOST") ~port:(int_of_string (env port))
        ~tls ~authenticator:trusted () in
      let auth=Imap_eio.Auth.password ~username:(env "IMAP_DOVECOT_USER")
        ~password:(env "IMAP_DOVECOT_PASSWORD") ~mechanism:`Cram_md5 () in
      let client=unwrap (Imap_eio.Client.connect ~sw ~auth transport) in
      let mailbox=Printf.sprintf "Oxmono-Dovecot-%d-%06x-Compress%d"
        (Unix.getpid ()) (Random.bits () land 0xffffff) index in
      Fun.protect ~finally:(fun () ->
        ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
        Imap_eio.Client.close client) @@ fun () ->
      let compress=unwrap (Imap_eio.Client.Compress.require client) in
      unwrap (Imap_eio.Client.Compress.activate compress);
      (match Imap_eio.Client.Compress.activate compress with
       | Error (Imap_eio.Error.State _) -> ()
       | _ -> Alcotest.fail "repeated COMPRESS was not refused locally");
      unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
      let raw="From: compress@example.test\r\nSubject: compressed literal\r\n\r\n" ^
        String.concat "" (List.init 16384 (fun _ ->
          "Repeated data across compressed APPEND and FETCH chunks.\r\n")) in
      let receipt=match unwrap (Imap_eio.Client.append client ~mailbox
        (Imap_eio.Client.append_message
           ~length:(Int64.of_int (String.length raw))
           (Eio.Flow.string_source raw))) with
        | Some receipt -> receipt | None -> Alcotest.fail "compressed APPEND omitted UID" in
      let uid=receipt.uid in
      unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
        (fun selected ->
          let output=Buffer.create (String.length raw) in
          unwrap (Imap_eio.Selected.fetch_to selected ~uid (Eio.Flow.buffer_sink output));
          Alcotest.(check bool) "compressed body roundtrip exact" true
            (Buffer.contents output=raw);
          Alcotest.(check (list int64)) "compressed SEARCH after literal"
            [Imap.Uid.to_int64 uid]
            (raw_uids (unwrap (Imap_eio.Selected.uid_search selected
              ~criteria:Imap.Search.All)));
          Ok ()));
      (match Imap_eio.Client.create_mailbox client ~mailbox with
       | Error (Imap_eio.Error.Rejected {code=Some Imap.Response.Alreadyexists;_}) -> ()
       | _ -> Alcotest.fail "compressed typed rejection lost");
      Alcotest.(check bool) "compressed connection still usable" true
        (unwrap (Imap_eio.Client.list client ~pattern:mailbox ())<>[])))
    ["IMAP_DOVECOT_PORT",`Plain;"IMAP_DOVECOT_TLS_PORT",`Implicit;
     "IMAP_DOVECOT_PORT",`Required_starttls]

let test_tls_cram () =
  configured ();
  let host = env "IMAP_DOVECOT_HOST" in
  let trusted = tls_authenticator (env "IMAP_DOVECOT_CA_CERT") in
  let untrusted = tls_authenticator (env "IMAP_DOVECOT_WRONG_CA_CERT") in
  let auth = Imap_eio.Auth.password ~username:(env "IMAP_DOVECOT_USER")
    ~password:(env "IMAP_DOVECOT_PASSWORD") ~mechanism:`Cram_md5 () in
  let login_auth = Imap_eio.Auth.password
    ~username:(env "IMAP_DOVECOT_USER")
    ~password:(env "IMAP_DOVECOT_PASSWORD") ~mechanism:`Login () in
  Eio_main.run @@ fun env_io ->
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env_io in
    List.iter (fun (name, port, tls) ->
      let transport authenticator = Imap_eio.Transport.v ~net ~host
        ~port:(int_of_string (env port)) ~tls ~authenticator () in
      let client = unwrap (Imap_eio.Client.connect ~sw ~auth
        (transport trusted)) in
      let boxes = unwrap (Imap_eio.Client.list client ~pattern:"INBOX" ()) in
      Alcotest.(check bool) (name ^ " INBOX listed") true (boxes <> []);
      Imap_eio.Client.close client;
      let login = unwrap (Imap_eio.Client.connect ~sw ~auth:login_auth
        (transport trusted)) in
      Alcotest.(check bool) (name ^ " LOGIN over TLS") true
        (unwrap (Imap_eio.Client.list login ~pattern:"INBOX" ()) <> []);
      Imap_eio.Client.close login;
      (match Imap_eio.Client.connect ~sw ~auth (transport untrusted) with
       | Error (Imap_eio.Error.Transport _) -> ()
       | Error e -> Alcotest.fail (name ^ " wrong-CA rejection: " ^
           Imap_eio.Client.error_to_string e)
       | Ok client ->
           Imap_eio.Client.close client;
           Alcotest.fail (name ^ " accepted an untrusted certificate"));
      let wrong_host = Imap_eio.Transport.v ~net ~host:"localhost"
        ~port:(int_of_string (env port)) ~tls ~authenticator:trusted () in
      match Imap_eio.Client.connect ~sw ~auth wrong_host with
      | Error (Imap_eio.Error.Transport _) -> ()
      | Error e -> Alcotest.fail (name ^ " wrong-host rejection: " ^
          Imap_eio.Client.error_to_string e)
      | Ok client ->
          Imap_eio.Client.close client;
          Alcotest.fail (name ^ " accepted a certificate for another host"))
      ["implicit TLS", "IMAP_DOVECOT_TLS_PORT", `Implicit;
       "required STARTTLS", "IMAP_DOVECOT_PORT", `Required_starttls];
    let plain = Imap_eio.Transport.v ~net ~host
      ~port:(int_of_string (env "IMAP_DOVECOT_PORT")) ~tls:`Plain () in
    (match Imap_eio.Client.connect ~sw ~auth:login_auth plain with
     | Error (Imap_eio.Error.State "LOGIN requires TLS") -> ()
     | Error e -> Alcotest.fail ("plaintext LOGIN policy: " ^
         Imap_eio.Client.error_to_string e)
     | Ok client ->
         Imap_eio.Client.close client;
         Alcotest.fail "Dovecot plaintext LOGIN was accepted")

let require_capability client capability =
  Alcotest.(check bool) capability true
    (Imap.Capability.Set.mem (Imap.Capability.of_wire capability)
      (Imap_eio.Client.capabilities client))

let uid_set = Imap.Uid_set.singleton

let ( let* ) result f = match result with
  | Ok x -> f x
  | Error _ as error -> error

let test_condstore_move_expunge () =
  configured ();
  Eio_main.run @@ fun env_io ->
    Eio.Switch.run @@ fun sw ->
    let _, client = connect env_io sw in
    List.iter (require_capability client)
      ["CONDSTORE"; "QRESYNC"; "MOVE"; "UIDPLUS"];
    let nonce = Printf.sprintf "%d-%06x" (Unix.getpid ())
      (Random.bits () land 0xffffff) in
    let source = "Oxmono-Dovecot-" ^ nonce ^ "-Source" in
    let destination = "Oxmono-Dovecot-" ^ nonce ^ "-Destination" in
    let created = ref [] in
    Fun.protect ~finally:(fun () ->
      List.iter (fun mailbox ->
        ignore (Imap_eio.Client.delete_mailbox client ~mailbox)) !created;
      Imap_eio.Client.close client) @@ fun () ->
    List.iter (fun mailbox ->
      unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
      created := mailbox :: !created) [source; destination];
    let append subject =
      let raw = "From: fixture@example.test\r\nSubject: " ^ subject ^
        "\r\nMessage-ID: <" ^ nonce ^ "-" ^ subject ^
        "@example.test>\r\n\r\nSynthetic body.\r\n" in
      unwrap (Result.map ignore (Imap_eio.Client.append client ~mailbox:source
        (Imap_eio.Client.append_message
           ~length:(Int64.of_int (String.length raw))
           (Eio.Flow.string_source raw)))) in
    append "move";
    append "expunge";
    let check_source selected =
      let* info = Imap_eio.Selected.info selected in
      Alcotest.(check int64) "EXISTS" 2L info.exists;
      Alcotest.(check bool) "UIDVALIDITY" true (info.uidvalidity > 0L);
      Alcotest.(check bool) "UIDNEXT" true (info.uidnext >= 3L);
      Alcotest.(check bool) "HIGHESTMODSEQ present" true
        (Option.is_some info.highestmodseq);
      let* uids = Imap_eio.Selected.uid_search selected
        ~criteria:Imap.Search.All in
      let move_uid, expunge_uid = match uids with
      | [first; second] -> first, second
      | _ -> Alcotest.failf "expected two source UIDs, found %d"
          (List.length uids) in
      let* rows = Imap_eio.Selected.fetch selected
        ~uids:[move_uid] ~items:[Imap.Fetch_item.Modseq] in
      let modseq = match rows with
      | [row] when Imap.Uid.equal row.uid move_uid ->
          (match row.modseq with
           | Some n -> Imap.Modseq.to_int64 n
           | None -> Alcotest.fail "FETCH omitted MODSEQ")
      | _ -> Alcotest.fail "FETCH omitted message metadata" in
      let seen = Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen in
      let* condstore = Imap_eio.Selected.Condstore.require selected in
      let* conflict = Imap_eio.Selected.Condstore.uid_store_flags condstore
        ~set:(uid_set move_uid) ~operation:`Add ~flags:[seen]
        ~unchangedsince:0L in
      Alcotest.(check bool) "MODIFIED identifies conflicting UID" true
        (Imap.Uid_set.mem move_uid conflict.modified);
      let* accepted = Imap_eio.Selected.Condstore.uid_store_flags condstore
        ~set:(uid_set move_uid) ~operation:`Add ~flags:[seen]
        ~unchangedsince:modseq in
      Alcotest.(check bool) "accepted store has no conflicts" true
        (Imap.Uid_set.is_empty accepted.modified);
      let* move = Imap_eio.Selected.Move.require selected in
      let* moved = Imap_eio.Selected.Move.uid_move move
        ~set:(uid_set move_uid) ~mailbox:destination in
      Alcotest.(check bool) "COPYUID receipt" true (Option.is_some moved);
      let deleted = Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted in
      let* _ = Imap_eio.Selected.uid_store_flags selected
        ~set:(uid_set expunge_uid) ~operation:`Add ~flags:[deleted] in
      let* () = uid_expunge selected
        ~set:(uid_set expunge_uid) in
      let* remaining = Imap_eio.Selected.uid_search selected
        ~criteria:Imap.Search.All in
      Alcotest.(check (list int64)) "source empty" [] (raw_uids remaining);
      Ok (info.uidvalidity, modseq) in
    let validity, checkpoint = unwrap
      (Imap_eio.Client.with_mailbox client ~mode:`Read_write
         source check_source) in
    unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only
      destination (fun selected ->
        let* uids = Imap_eio.Selected.uid_search selected
          ~criteria:Imap.Search.All in
        Alcotest.(check int) "one moved message" 1 (List.length uids);
        Ok ()));
    let qresync = match Imap.Uidvalidity.of_int64 validity,
        Imap.Modseq.of_int64 checkpoint with
      | Ok validity, Ok checkpoint -> (validity, checkpoint)
      | Error e, _ | _, Error e -> Alcotest.fail e in
    unwrap (Imap_eio.Client.with_mailbox client
      ~qresync ~mode:`Read_only source
      (fun selected ->
        let* info = Imap_eio.Selected.info selected in
        let* updates = Imap_eio.Selected.select_updates selected in
        let highest = match info.highestmodseq with
        | Some n -> n | None -> Alcotest.fail "QRESYNC omitted HIGHESTMODSEQ" in
        Alcotest.(check bool) "QRESYNC advanced MODSEQ" true
          (highest > checkpoint);
        let vanished = List.exists (function
          | Imap.Response.Untagged (Imap.Response.Vanished _) -> true
          | _ -> false) updates in
        Alcotest.(check bool) "QRESYNC reported vanished UIDs" true vanished;
        Ok ()))

let test_idle ~compress () =
  configured ();
  Eio_main.run @@ fun env_io ->
    Eio.Switch.run @@ fun sw ->
    let _, idle_client = connect env_io sw in
    let _, writer_client = connect env_io sw in
    if compress then (
      let activate client = Result.bind
        (Imap_eio.Client.Compress.require client)
        Imap_eio.Client.Compress.activate in
      unwrap (activate idle_client);
      unwrap (activate writer_client));
    require_capability idle_client "IDLE";
    let nonce = Printf.sprintf "%d-%06x" (Unix.getpid ())
      (Random.bits () land 0xffffff) in
    let mailbox = "Oxmono-Dovecot-" ^ nonce ^ "-Idle" in
    Fun.protect ~finally:(fun () ->
      Imap_eio.Client.close idle_client;
      ignore (Imap_eio.Client.delete_mailbox writer_client ~mailbox);
      Imap_eio.Client.close writer_client) @@ fun () ->
    unwrap (Imap_eio.Client.create_mailbox writer_client ~mailbox);
    let clock = Eio.Stdenv.clock env_io in
    let changes = Eio.Time.with_timeout_exn clock 10. (fun () ->
      unwrap (Imap_eio.Client.with_mailbox idle_client
        ~mode:`Read_only mailbox (fun selected ->
          let writer = Eio.Fiber.fork_promise ~sw (fun () ->
            Eio.Time.sleep clock 0.2;
            let raw = "From: fixture@example.test\r\nSubject: idle " ^
              nonce ^ "\r\n\r\nWake up.\r\n" in
            unwrap (Result.map ignore (Imap_eio.Client.append writer_client
              ~mailbox
              (Imap_eio.Client.append_message
                 ~length:(Int64.of_int (String.length raw))
                 (Eio.Flow.string_source raw))))) in
          let result = Result.bind (Imap_eio.Selected.Idle.require selected)
            (Imap_eio.Selected.Idle.wait_for_change ~clock ~timeout:60.) in
          Eio.Promise.await_exn writer;
          result))) in
    Alcotest.(check bool) "IDLE woke on EXISTS" true
      (List.exists (function
        | Imap.Response.Untagged (Imap.Response.Exists n) when n >= 1L -> true
        | _ -> false) changes)

exception Watch_done

let test_durable_watch ~gap () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _, writer = connect env_io sw in
  require_capability writer "IDLE";
  let nonce = Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox = "Oxmono-Dovecot-" ^ nonce ^ "-Watch" in
  let dbfile = Filename.temp_file "oxmono-imap-watch-" ".sqlite" in
  let cleanup () =
    ignore (Imap_eio.Client.delete_mailbox writer ~mailbox);
    Imap_eio.Client.close writer;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"] in
  Fun.protect ~finally:cleanup @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox writer ~mailbox);
  let mode=Imap_eio.Client.mailbox_mode writer in
  let raw_name=match Imap.Mailbox_name.encode ~mode mailbox with
    | Ok name -> name | Error message -> Alcotest.fail message in
  let scope : Imap.Mirror.scope = {
    endpoint=(env "IMAP_DOVECOT_HOST") ^ ":" ^ (env "IMAP_DOVECOT_PORT");
    account=env "IMAP_DOVECOT_USER"; mailbox_key=mailbox;
    raw_name;encoding=mode;mailbox_id=None} in
  Eio.Switch.run @@ fun store_sw ->
  let dbpath=Eio.Path.(Eio.Stdenv.fs env_io / dbfile) in
  let store=Imap_store.open_path ~sw:store_sw dbpath in
  let clock=Eio.Stdenv.clock env_io in
  let connect_watch ~sw =
    let transport=Imap_eio.Transport.v ~net:(Eio.Stdenv.net env_io)
      ~host:(env "IMAP_DOVECOT_HOST")
      ~port:(int_of_string (env "IMAP_DOVECOT_PORT")) ~tls:`Plain () in
    let auth=Imap_eio.Auth.password ~username:(env "IMAP_DOVECOT_USER")
      ~password:(env "IMAP_DOVECOT_PASSWORD") ~mechanism:`Cram_md5 () in
    match Imap_eio.Client.connect ~sw ~auth transport with
    | Error e -> Error (Imap_sync.Error.Client e)
    | Ok client ->
        (* A scan uses neither the spool directory nor an ID. *)
        Imap_sync.Ctx.v ~client ~store ~scope ~mailbox
          ~spool_dir:Eio.Path.(Eio.Stdenv.fs env_io /
            Filename.get_temp_dir_name ())
          ~next_id:(fun () -> Alcotest.fail "watch scan requested an ID") in
  let stage_number=ref 0 and publications=ref [] in
  let next_stage_id () =
    incr stage_number;
    Printf.sprintf "%s-stage-%d" nonce !stage_number in
  let on_publish (receipt : Imap_store.staged_receipt) =
    publications := receipt.row_count :: !publications;
    if List.length !publications=1 then (
      let write () =
        let raw="From: fixture@example.test\r\nSubject: watch " ^
          nonce ^ "\r\n\r\nWake up.\r\n" in
        unwrap (Result.map ignore (Imap_eio.Client.append writer ~mailbox
          (Imap_eio.Client.append_message
             ~length:(Int64.of_int (String.length raw))
             (Eio.Flow.string_source raw)))) in
      if gap then write ()
      else Eio.Fiber.fork ~sw (fun () ->
        Eio.Time.sleep clock 0.2;
        write ()))
    else if List.length !publications=2 then raise Watch_done in
  (try Eio.Time.with_timeout_exn clock 10. (fun () ->
    ignore (Imap_sync.Watch.run ~clock ~connect:connect_watch ~next_stage_id
      ~on_publish () : (unit, Imap_sync.Watch.error) result))
   with Watch_done -> ());
  Alcotest.(check (list int64)) "durable watch publications"
    [0L;1L] (List.rev !publications);
  let cursor=Imap_store.load_cursor store ~scope in
  Alcotest.(check int64) "watch persisted two revisions" 2L
    cursor.revision

let test_bridge_cram () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _, client=connect env_io sw in
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-Bridge" in
  let dbfile=Filename.temp_file "oxmono-dovecot-bridge-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  let fs=Eio.Stdenv.fs env_io in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let parse_date raw=match Imap.Internal_date.of_string raw with
    | Ok date -> date | Error error -> Alcotest.fail error in
  let remote_date=parse_date "26-Sep-2025 12:34:56 +0230" in
  let local_date=parse_date " 2-Jan-2024 03:04:05 -0700" in
  let raw="From: dovecot@example.test\r\nSubject: bridge " ^ nonce ^
    "\r\n\r\nRemote original\r\n" in
  unwrap (Result.map ignore (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message ~internal_date:remote_date
       ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))));
  let mode=Imap_eio.Client.mailbox_mode client in
  let raw_name=match Imap.Mailbox_name.encode ~mode mailbox with
    | Ok raw_name -> raw_name | Error e -> Alcotest.fail e in
  let scope : Imap.Mirror.scope = {
    endpoint=(env "IMAP_DOVECOT_HOST") ^ ":" ^ (env "IMAP_DOVECOT_PORT");
    account=env "IMAP_DOVECOT_USER";mailbox_key=mailbox;
    raw_name;encoding=mode;mailbox_id=None} in
  Eio.Switch.run @@ fun store_sw ->
  let store=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  let number=ref 0 in
  let next_id ()=incr number;Printf.sprintf "dovecot-%s-%d" nonce !number in
  let ctx ()=sync_ctx ~client ~store ~scope ~mailbox
    ~spool_dir:Eio.Path.(fs / spooldir) ~next_id () in
  let copy stage_id=match Imap_sync.Bridge.copy_once ~ctx:(ctx ()) ~maildir
    ~stage_id () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.fail (Format.asprintf "%a"
        Imap_sync.Error.pp error) in
  let imported=copy ("dovecot-import-" ^ nonce) in
  Alcotest.(check int) "CRAM-MD5 bridge import" 1
    imported.remote_to_local;
  let imported_occurrence=match Md.scan
      (Md.open_dir Eio.Path.(fs / maildir_path)) with
    | [occurrence] -> occurrence
    | _ -> Alcotest.fail "dated import missing" in
  Alcotest.(check bool) "imported instant survives Maildir reopen" true
    (match occurrence_date imported_occurrence with
     | Some saved -> Imap.Internal_date.equal_instant remote_date saved
     | None -> false);
  Alcotest.(check bool) "imported pair retains INTERNALDATE" true
    (match Imap_store.Journal.find_local store ~scope
        ~local_id:imported_occurrence.id with
     | Some {internal_date=Some saved;_} ->
         Imap.Internal_date.equal_instant remote_date saved
     | _ -> false);
  let imported_pair=Option.get (Imap_store.Journal.find_local store ~scope
    ~local_id:imported_occurrence.id) in
  Alcotest.(check bool) "import operation retains source INTERNALDATE" true
    (match Imap_store.Journal.find_operation store ~id:imported_pair.id with
     | Some {internal_date=Some saved;_} ->
         Imap.Internal_date.equal_instant remote_date saved
     | _ -> false);
  let local_bytes="From: local@example.test\r\nSubject: upload " ^ nonce ^
    "\r\n\r\nLocal original\r\n" in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source local_bytes)
    ~length:(Int64.of_int (String.length local_bytes)) ~flags:[]
    ~mtime:(mtime local_date) () in
  let plan_args=[|"imap-sync";"plan-sync";
    "--endpoint";scope.endpoint;"--account";scope.account;
    "--mailbox";mailbox;"--db";dbfile;
    "--maildir";maildir_path;"--max-inspect";"10"|] in
  let plan=cli_job ~env:Sys.getenv_opt plan_args in
  Alcotest.(check int) "read-only CLI sync plan" 0
    (Imap_cli.run plan ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt);
  Alcotest.(check bool) "plan did not pair local source" true
    (Imap_store.Journal.find_local store ~scope ~local_id:local.id=None);
  let uploaded=copy ("dovecot-upload-" ^ nonce) in
  Alcotest.(check int) "CRAM-MD5 bridge upload" 1
    uploaded.local_to_remote;
  Alcotest.(check bool) "Dovecot upload paired" true
    (Option.is_some (Imap_store.Journal.find_local store ~scope
      ~local_id:local.id));
  Alcotest.(check int) "Dovecot durable pairs" 2
    (List.length (all_pairs store ~scope));
  let pair=
    match Imap_store.Journal.find_local store ~scope ~local_id:local.id with
    | Some pair -> pair | None -> Alcotest.fail "upload pair missing" in
  Alcotest.(check bool) "uploaded pair retains INTERNALDATE" true
    (match pair.internal_date with
     | Some saved -> Imap.Internal_date.equal_instant local_date saved
     | None -> false);
  let remote_uid=match pair.remote_uid with
    | Some uid -> uid | None -> Alcotest.fail "upload UID missing" in
  let uploaded_date=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_only mailbox (fun selected ->
      match Imap_eio.Selected.fetch selected ~uids:[remote_uid]
        ~items:[Imap.Fetch_item.Internal_date] with
      | Error _ as error -> error
      | Ok [row] -> Ok row.internal_date
      | Ok _ -> Error (Imap_eio.Error.Protocol
          "uploaded date FETCH omitted UID"))) in
  Alcotest.(check bool) "upload preserved INTERNALDATE instant" true
    (match uploaded_date with
     | Some actual -> Imap.Internal_date.equal_instant local_date actual
     | None -> false);
  let flagged=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Flagged in
  let custom=match Mail_flag.Imap_flag.of_wire "$dovecot" with
    | Ok flag -> flag | Error e -> Alcotest.fail e in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
    (fun selected ->
      let* _=Imap_eio.Selected.uid_store_flags selected
        ~set:(Imap.Uid_set.singleton remote_uid)
        ~operation:`Add ~flags:[flagged] in Ok ()));
  ignore (Md.set_flags maildir local [custom]);
  let reconciled=copy ("dovecot-flags-" ^ nonce) in
  Alcotest.(check int) "Dovecot conditional flag merge" 1
    reconciled.flags_updated;
  let pair=match Imap_store.Journal.find_pair store ~id:pair.id with
    | Some pair -> pair | None -> Alcotest.fail "merged pair missing" in
  Alcotest.(check bool) "Dovecot common flags merged" true
    (List.mem flagged pair.common_flags &&
     List.mem custom pair.common_flags);
  let seen=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen in
  let changed_body=String.mapi
    (fun i c -> if i=0 then 'X' else c) local_bytes in
  let current=Option.get (Md.find maildir ~id:local.id) in
  Md.remove maildir current;
  ignore (Md.append maildir ~id:local.id
    ~source:(Eio.Flow.string_source changed_body)
    ~length:(Int64.of_int (String.length changed_body))
    ~flags:[seen] ~mtime:(mtime local_date) ());
  let sync_args=[|"imap-sync";"sync";
    "--host";env "IMAP_DOVECOT_HOST";
    "--port";env "IMAP_DOVECOT_PORT";"--tls";"plain";
    "--user";env "IMAP_DOVECOT_USER";"--auth";"cram-md5";
    "--password-env";"IMAP_DOVECOT_PASSWORD";
    "--endpoint";scope.endpoint;"--account";scope.account;
    "--mailbox";mailbox;"--db";dbfile;"--blob-dir";blobdir;
    "--maildir";maildir_path;"--spool-dir";spooldir|] in
  let sync_config=cli_job ~env:Sys.getenv_opt sync_args in
  Alcotest.(check int) "CLI reports paired body conflict" 4
    (Imap_cli.run sync_config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt);
  Alcotest.(check bool) "CLI persists content conflict" true
    (match all_open_conflicts store ~scope with
     | [{pair_id;kind=Imap_store.Journal.Content_conflict;_}] ->
         pair_id=pair.id
     | _ -> false);
  let current=Option.get (Md.find maildir ~id:local.id) in
  Md.remove maildir current;
  ignore (Md.append maildir ~id:local.id
    ~source:(Eio.Flow.string_source local_bytes)
    ~length:(Int64.of_int (String.length local_bytes))
    ~flags:pair.common_flags ~mtime:(mtime local_date) ());
  let restored=copy ("dovecot-content-restored-" ^ nonce) in
  Alcotest.(check int) "restored body needs no flag update" 0
    restored.flags_updated;
  Alcotest.(check int) "restored body clears content conflict" 0
    (List.length (all_open_conflicts store ~scope));
  let current=Option.get (Md.find maildir ~id:local.id) in
  Md.remove maildir current;
  ignore (Md.append maildir ~id:local.id
    ~source:(Eio.Flow.string_source changed_body)
    ~length:(Int64.of_int (String.length changed_body))
    ~flags:pair.common_flags ~mtime:(mtime local_date) ());
  let verify_args=[|"imap-sync";"verify-local";
    "--endpoint";scope.endpoint;"--account";scope.account;
    "--mailbox";mailbox;"--db";dbfile;"--maildir";maildir_path;
    "--max-inspect";"10"|] in
  let verify_config=cli_job ~env:Sys.getenv_opt verify_args in
  let verify ()=Imap_cli.run verify_config ~net:(Eio.Stdenv.net env_io)
      ~fs ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt in
  Alcotest.(check int) "offline scrub detects silent same-length edit" 4
    (verify ());
  Alcotest.(check bool) "silent edit persisted as content conflict" true
    (match all_open_conflicts store ~scope with
     | [{pair_id;kind=Imap_store.Journal.Content_conflict;_}] ->
         pair_id=pair.id
     | _ -> false);
  let current=Option.get (Md.find maildir ~id:local.id) in
  Md.remove maildir current;
  ignore (Md.append maildir ~id:local.id
    ~source:(Eio.Flow.string_source local_bytes)
    ~length:(Int64.of_int (String.length local_bytes))
    ~flags:pair.common_flags ~mtime:(mtime local_date) ());
  Alcotest.(check int) "offline scrub clears restored bytes" 0
    (verify ());
  let deleted=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
    (fun selected ->
      let* _=Imap_eio.Selected.uid_store_flags selected
        ~set:(Imap.Uid_set.singleton remote_uid)
        ~operation:`Add ~flags:[deleted] in Ok ()));
  let held=copy ("dovecot-deleted-hold-" ^ nonce) in
  Alcotest.(check int) "deleted flag held" 1 held.flags_held;
  let policy_conflicts ()=all_open_conflicts store ~scope
    |> List.filter (fun (x:Imap_store.Journal.conflict) ->
      x.kind=Imap_store.Journal.Policy_conflict) in
  let conflict=match policy_conflicts () with
    | [conflict] -> conflict
    | _ -> Alcotest.fail "deleted hold lacked one durable policy conflict" in
  Eio.Switch.run (fun inspect_sw ->
    let reopened=Imap_store.open_readonly ~sw:inspect_sw
      Eio.Path.(fs / dbfile) in
    Alcotest.(check (list string)) "policy hold visible after reopen"
      [conflict.id]
      (all_open_conflicts reopened ~scope
       |> List.map (fun (x:Imap_store.Journal.conflict) -> x.id)));
  let held_again=copy ("dovecot-deleted-hold-again-" ^ nonce) in
  Alcotest.(check int) "repeated deleted flag held" 1
    held_again.flags_held;
  Alcotest.(check (list string)) "policy conflict ID stable"
    [conflict.id] (List.map (fun (x:Imap_store.Journal.conflict) -> x.id)
      (policy_conflicts ()));
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
    (fun selected ->
      let* _=Imap_eio.Selected.uid_store_flags selected
        ~set:(Imap.Uid_set.singleton remote_uid)
        ~operation:`Remove ~flags:[deleted] in Ok ()));
  let cleared=copy ("dovecot-deleted-cleared-" ^ nonce) in
  Alcotest.(check int) "deleted flag hold cleared" 0
    cleared.flags_held;
  Alcotest.(check int) "policy conflict resolved after complete scan" 0
    (List.length (policy_conflicts ()));
  let imported_local=List.find (fun (x:Md.occurrence) ->
    x.id<>local.id) (Md.scan maildir) in
  let imported_pair=match Imap_store.Journal.find_local store ~scope
      ~local_id:imported_local.id with
    | Some pair -> pair | None -> Alcotest.fail "import pair missing" in
  Md.remove maildir imported_local;
  let deleted_remote=match Imap_sync.Bridge.copy_once
    ~deletion_policy:Imap.Sync_policy.Propagate ~ctx:(ctx ()) ~maildir
    ~stage_id:("dovecot-delete-remote-" ^ nonce) () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.fail (Format.asprintf "%a"
        Imap_sync.Error.pp error) in
  Alcotest.(check int) "Dovecot targeted remote deletion" 1
    deleted_remote.deletions;
  Alcotest.(check bool) "remote delete journal committed" true
    (match Imap_store.Journal.find_pair store ~id:imported_pair.id with
     | Some {remote_tombstone=Some
         {reason=Imap_store.Journal.Expunge_receipt;_};_} -> true
     | _ -> false);
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
    (fun selected ->
      let set=Imap.Uid_set.singleton remote_uid in
      let deleted=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted in
      let* _=Imap_eio.Selected.uid_store_flags selected ~set
        ~operation:`Add ~flags:[deleted] in
      uid_expunge selected ~set));
  let deleted_local=match Imap_sync.Bridge.copy_once
    ~deletion_policy:Imap.Sync_policy.Propagate ~ctx:(ctx ()) ~maildir
    ~stage_id:("dovecot-delete-local-" ^ nonce) () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.fail (Format.asprintf "%a"
        Imap_sync.Error.pp error) in
  Alcotest.(check int) "Dovecot local survivor deletion" 1
    deleted_local.deletions;
  Alcotest.(check bool) "local delete journal committed" true
    (match Imap_store.Journal.find_pair store ~id:pair.id with
     | Some {local_tombstone=Some
         {reason=Imap_store.Journal.Explicit_delete;_};_} -> true
     | _ -> false);
  let append_identical ()=Md.append maildir
    ~source:(Eio.Flow.string_source local_bytes)
    ~length:(Int64.of_int (String.length local_bytes)) ~flags:[] () in
  let first=append_identical () and second=append_identical () in
  List.iter (fun (local:Md.occurrence) ->
    let filename=Filename.concat
      (Filename.concat maildir_path "new") local.filename in
    Unix.utimes filename 1709164800. 1709164800.) [first;second];
  let twins=copy ("dovecot-identical-" ^ nonce) in
  Alcotest.(check int) "identical bytes retain two occurrences" 2
    twins.local_to_remote;
  let paired_uid local=match Imap_store.Journal.find_local store ~scope
      ~local_id:local.Md.id with
    | Some {remote_uid=Some uid;_} -> uid
    | _ -> Alcotest.fail "identical occurrence lacks a paired UID" in
  Alcotest.(check bool) "identical bytes have distinct remote UIDs" true
    (not (Imap.Uid.equal (paired_uid first) (paired_uid second)));
  let uploaded_mtime_date=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_only mailbox (fun selected ->
      match Imap_eio.Selected.fetch selected ~uids:[paired_uid first]
        ~items:[Imap.Fetch_item.Internal_date] with
      | Error _ as error -> error
      | Ok [row] -> Ok row.internal_date
      | Ok _ -> Error (Imap_eio.Error.Protocol
          "mtime upload FETCH omitted UID"))) in
  Alcotest.(check (option string)) "undated Maildir mtime uploaded as UTC"
    (Some "29-Feb-2024 00:00:00 +0000")
    (Option.map Imap.Internal_date.to_string uploaded_mtime_date);
  let again=copy ("dovecot-identical-again-" ^ nonce) in
  Alcotest.(check int) "identical occurrences do not replay" 0
    again.local_to_remote;
  let first_path=Filename.concat
    (Filename.concat maildir_path "new") first.filename in
  Unix.utimes first_path 1709164801. 1709164801.;
  let drift stage=match Imap_sync.Bridge.copy_once ~ctx:(ctx ()) ~maildir
      ~stage_id:stage () with
    | Error (Imap_sync.Error.Date_diverged _) -> ()
    | Error error -> Alcotest.failf "date drift: %a"
        Imap_sync.Error.pp error
    | Ok _ -> Alcotest.fail "paired date drift was accepted" in
  drift ("dovecot-date-drift-" ^ nonce);
  let date_conflicts ()=all_open_conflicts store ~scope
    |> List.filter (fun (x:Imap_store.Journal.conflict) ->
      x.kind=Imap_store.Journal.Identity_conflict) in
  let conflict=match date_conflicts () with
    | [conflict] -> conflict
    | _ -> Alcotest.fail "date drift lacked durable identity conflict" in
  drift ("dovecot-date-drift-again-" ^ nonce);
  Alcotest.(check (list string)) "date conflict ID survives rescan"
    [conflict.id]
    (List.map (fun (x:Imap_store.Journal.conflict) -> x.id)
      (date_conflicts ()));
  Unix.utimes first_path 1709164800. 1709164800.;
  ignore (copy ("dovecot-date-restored-" ^ nonce));
  Alcotest.(check int) "restored date resolves conflict" 0
    (List.length (date_conflicts ()))

let dovecot_scope client mailbox : Imap.Mirror.scope =
  let mode=Imap_eio.Client.mailbox_mode client in
  let raw_name=match Imap.Mailbox_name.encode ~mode mailbox with
    | Ok raw_name -> raw_name | Error e -> Alcotest.fail e in
  {endpoint=(env "IMAP_DOVECOT_HOST") ^ ":" ^ (env "IMAP_DOVECOT_PORT");
   account=env "IMAP_DOVECOT_USER";mailbox_key=mailbox;
   raw_name;encoding=mode;mailbox_id=None}

let test_shared_mailbox_bootstrap () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env_io in
  let _,client=connect env_io sw in
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-Bootstrap" in
  let dbfile=Filename.temp_file "oxmono-dovecot-bootstrap-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let raw="From: bootstrap@example.test\r\nSubject: same " ^ nonce ^
    "\r\n\r\nIdentical on both sides\r\n" in
  unwrap (Result.map ignore (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message
       ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))));
  let scope=dovecot_scope client mailbox in
  Eio.Switch.run @@ fun store_sw ->
  let store=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  ignore (Md.append maildir
    ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw)) ~flags:[] ());
  let number=ref 0 in
  let next_id ()=incr number;Printf.sprintf "bootstrap-%s-%d" nonce !number in
  let copy ?(allow_bootstrap_duplicates=false) stage_id=
    Imap_sync.Bridge.copy_once ~allow_bootstrap_duplicates
      ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir) ~next_id ())
      ~maildir ~stage_id () in
  (match copy ("bootstrap-refuse-" ^ nonce) with
   | Error Imap_sync.Error.Bootstrap_requires_pairing -> ()
   | Error error -> Alcotest.failf "bootstrap refusal: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "unpaired populated endpoints were merged");
  Alcotest.(check int) "refusal did not make local copies" 1
    (List.length (Md.scan maildir));
  Alcotest.(check int) "refusal did not journal mutations" 0
    (List.length (all_active_operations store ~scope));
  Alcotest.(check int) "refusal did not publish pairs" 0
    (List.length (all_pairs store ~scope));
  let server_uids ()=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_only mailbox (fun selected ->
      Imap_eio.Selected.uid_search selected ~criteria:Imap.Search.All)) in
  Alcotest.(check int) "refusal did not upload" 1
    (List.length (server_uids ()));
  let accepted=match copy ~allow_bootstrap_duplicates:true
      ("bootstrap-accept-" ^ nonce) with
    | Ok result -> result
    | Error error -> Alcotest.failf "explicit bootstrap: %a"
        Imap_sync.Error.pp error in
  Alcotest.(check int) "explicit remote import" 1
    accepted.remote_to_local;
  Alcotest.(check int) "explicit local upload" 1
    accepted.local_to_remote;
  Alcotest.(check int) "two occurrence pairs" 2
    (List.length (all_pairs store ~scope));
  Alcotest.(check int) "two remote UIDs" 2
    (List.length (server_uids ()));
  let stable=match copy ("bootstrap-stable-" ^ nonce) with
    | Ok result -> result
    | Error error -> Alcotest.failf "bootstrap replay: %a"
        Imap_sync.Error.pp error in
  Alcotest.(check int) "no import replay" 0 stable.remote_to_local;
  Alcotest.(check int) "no upload replay" 0 stable.local_to_remote

let append_crash_date () = match Imap.Internal_date.of_string
    "26-Sep-2025 12:34:56 +0000" with
  | Ok date -> date | Error message -> Alcotest.fail message

let append_crash_child dbfile mailbox local_id id =
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env_io in
  let _,client=connect env_io sw in
  let scope=dovecot_scope client mailbox in
  let store=Imap_store.open_path ~sw
    ~blob_dir:Eio.Path.(fs / (dbfile ^ "-blobs")) Eio.Path.(fs / dbfile) in
  let maildir=Md.open_dir Eio.Path.(fs / (dbfile ^ "-maildir")) in
  let local=match Md.find maildir ~id:local_id with
    | Some local -> local | None -> Alcotest.fail "crash child lost source" in
  let cursor=Imap_store.load_cursor store ~scope in
  let epoch=match cursor.uidvalidity with
    | Some epoch -> epoch | None -> Alcotest.fail "crash child has no epoch" in
  let blob=Eio.Switch.run @@ fun source_sw ->
    let source=Md.open_message maildir ~sw:source_sw local in
    Imap_store.Blob.put store ~source ~length:local.length () in
  let op : Imap_store.Journal.operation = {
    id;pair_id=None;local_id=Some local_id;scope;
    kind=Imap_store.Journal.Append;state=Imap_store.Journal.Prepared;
    source_uidvalidity=None;source_uid=None;destination=Some scope;
    destination_uidvalidity=Some epoch;
    blob_sha256=Some blob.sha256;blob_length=Some blob.length;
    desired_flags=Some [];
    internal_date=Some (append_crash_date ());
    append=Some {message_id=id;spool_ref=blob.sha256;
      pre_send_frontier=cursor.frontier};
    receipt=None;receipt_uidvalidity=None;
    receipt_uid=None} in
  Imap_store.Journal.prepare_operation
    ~local_source_mtime:local.mtime store op;
  Imap_store.Journal.mark_sent store ~id;
  let receipt=Eio.Switch.run @@ fun source_sw ->
    let source=Imap_store.Blob.open_in store ~sw:source_sw blob in
    unwrap (Imap_eio.Client.append client ~mailbox
      (Imap_eio.Client.append_message
         ~internal_date:(append_crash_date ()) ~length:blob.length source)) in
  let receipt=match receipt with
    | Some receipt -> receipt
    | None -> Alcotest.fail "Dovecot omitted APPENDUID" in
  let output=open_out (dbfile ^ "-appenduid") in
  Printf.fprintf output "%Ld %Ld\n"
    (Imap.Uidvalidity.to_int64 receipt.uidvalidity)
    (Imap.Uid.to_int64 receipt.uid);
  flush output;
  Unix.fsync (Unix.descr_of_out_channel output);
  close_out output;
  (* Server acceptance is real, but the receipt is only in the sidecar used
     as operator evidence. The journal does not record it before death. *)
  Unix._exit 77

let test_append_process_crash () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env_io in
  let _,client=connect env_io sw in
  require_capability client "UIDPLUS";
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-AppendCrash" in
  let dbfile=Filename.temp_file "oxmono-dovecot-append-crash-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm";dbfile ^ "-appenduid"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let scope=dovecot_scope client mailbox in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  let copy store stage_id = match Imap_sync.Bridge.copy_once
    ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir)
      ~next_id:(fun () -> "unexpected-" ^ Md.reserve_id ()) ())
    ~maildir ~stage_id () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "APPEND crash bridge: %a"
        Imap_sync.Error.pp error in
  Eio.Switch.run (fun store_sw ->
    let store=Imap_store.open_path ~sw:store_sw
      ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
    let initial=copy store ("append-crash-initial-" ^ nonce) in
    Alcotest.(check int) "initial mailbox empty" 0
      initial.remote_to_local);
  let raw="From: crash@example.test\r\nSubject: APPEND crash " ^ nonce ^
    "\r\nMessage-ID: <append-crash-" ^ nonce ^
    "@example.test>\r\n\r\nAccepted before process exit.\r\n" in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw)) ~flags:[]
    ~mtime:(mtime (append_crash_date ())) () in
  let id="append-crash-" ^ nonce in
  let executable=if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name in
  let pid=Unix.create_process executable
    [|executable;"--append-crash-child";dbfile;mailbox;local.id;id|]
    Unix.stdin Unix.stdout Unix.stderr in
  let _,status=Unix.waitpid [] pid in
  Alcotest.(check bool) "child exited after accepted APPEND" true
    (status=Unix.WEXITED 77);
  let input=open_in (dbfile ^ "-appenduid") in
  let receipt_line=input_line input in
  close_in input;
  let epoch_raw,uid_raw=match String.split_on_char ' ' receipt_line with
    | [epoch;uid] -> Int64.of_string epoch,Int64.of_string uid
    | _ -> Alcotest.fail "malformed child APPENDUID evidence" in
  let epoch=match Imap.Uidvalidity.of_int64 epoch_raw with
    | Ok epoch -> epoch | Error e -> Alcotest.fail e in
  let uid=match Imap.Uid.of_int64 uid_raw with
    | Ok uid -> uid | Error e -> Alcotest.fail e in
  let remote_uids ()=raw_uids (unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_only mailbox (fun selected ->
      Imap_eio.Selected.uid_search selected ~criteria:Imap.Search.All))) in
  Alcotest.(check (list int64)) "one server APPEND after child exit"
    [uid_raw] (remote_uids ());
  Eio.Switch.run (fun store_sw ->
    let store=Imap_store.open_path ~sw:store_sw
      ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
    Alcotest.(check bool) "journal lacks durable receipt" true
      (match Imap_store.Journal.find_operation store ~id with
       | Some {state=Imap_store.Journal.Sent;receipt_uid=None;_} -> true
       | _ -> false);
    (match Imap_sync.Bridge.copy_once
      ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir)
        ~next_id:(fun () -> Alcotest.fail "ambiguous APPEND replayed") ())
      ~maildir ~stage_id:("append-crash-hold-" ^ nonce) () with
     | Error (Imap_sync.Error.Pending_operations [pending])
       when pending=id -> ()
     | Error error -> Alcotest.failf "wrong APPEND hold: %a"
         Imap_sync.Error.pp error
     | Ok _ -> Alcotest.fail "unconfirmed APPEND was not held");
    Alcotest.(check (list int64)) "restart did not replay APPEND"
      [uid_raw] (remote_uids ());
    let same_instant=match Imap.Internal_date.of_string
      "26-Sep-2025 14:34:56 +0200" with
      | Ok date -> date | Error e -> Alcotest.fail e in
    let different_instant=match Imap.Internal_date.of_string
      "26-Sep-2025 12:34:57 +0000" with
      | Ok date -> date | Error e -> Alcotest.fail e in
    let extra=match unwrap (Imap_eio.Client.append client ~mailbox
      (Imap_eio.Client.append_message ~internal_date:same_instant
         ~length:(Int64.of_int (String.length raw))
         (Eio.Flow.string_source raw))) with
      | Some receipt -> receipt.uid
      | None -> Alcotest.fail "Dovecot omitted duplicate APPENDUID" in
    let wrong_date=match unwrap (Imap_eio.Client.append client ~mailbox
      (Imap_eio.Client.append_message ~internal_date:different_instant
         ~length:(Int64.of_int (String.length raw))
         (Eio.Flow.string_source raw))) with
      | Some receipt -> receipt.uid
      | None -> Alcotest.fail "Dovecot omitted other-date APPENDUID" in
    let inspect_ctx ()=sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir) () in
    (match Imap_sync.Repair.inspect_append_candidates ~max_uids:1
      ~ctx:(inspect_ctx ()) ~id () with
     | Error (Imap_sync.Error.Invalid_configuration _) -> ()
     | _ -> Alcotest.fail "candidate scan silently truncated UID range");
    (match Imap_sync.Repair.inspect_append_candidates
      ~max_body_bytes:(Int64.of_int (String.length raw))
      ~ctx:(inspect_ctx ()) ~id () with
     | Error (Imap_sync.Error.Invalid_configuration _) -> ()
     | _ -> Alcotest.fail "candidate scan exceeded aggregate body budget");
    let candidates=match Imap_sync.Repair.inspect_append_candidates
      ~max_body_bytes:(Int64.mul 2L (Int64.of_int (String.length raw)))
      ~ctx:(inspect_ctx ()) ~id () with
      | Ok report -> report
      | Error error -> Alcotest.failf "APPEND candidate inspection: %a"
          Imap_sync.Error.pp error in
    Alcotest.(check (list int64))
      "only identical bodies at the intended instant remain ambiguous"
      [uid_raw;Imap.Uid.to_int64 extra]
      (List.map Imap.Uid.to_int64 candidates.matching_uids);
    Alcotest.(check bool) "candidate inspection did not confirm journal"
      true (match Imap_store.Journal.find_operation store ~id with
        | Some {state=Imap_store.Journal.Sent;_} -> true | _ -> false);
    unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
      (fun selected ->
        let set=Imap.Uid_set.of_intervals
          [extra,extra;wrong_date,wrong_date] in
        let deleted=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted in
        let* _=Imap_eio.Selected.uid_store_flags selected ~set
          ~operation:`Add ~flags:[deleted] in
        uid_expunge selected ~set));
    Alcotest.(check (list int64)) "extra candidate removed before repair"
      [uid_raw] (remote_uids ());
    (match Imap_sync.Repair.record_appenduid ~store ~maildir ~scope
      ~id ~uidvalidity:epoch ~uid
      ~evidence:"Dovecot APPENDUID saved by crash witness" () with
     | Ok () -> ()
     | Error error -> Alcotest.failf "attest APPENDUID: %a"
         Imap_sync.Error.pp error);
    let completed=copy store ("append-crash-repair-" ^ nonce) in
    Alcotest.(check int) "attested APPEND not uploaded again" 0
      completed.local_to_remote;
    Alcotest.(check int) "one paired occurrence" 1
      (List.length (all_pairs store ~scope));
    Alcotest.(check bool) "local occurrence paired to witnessed UID" true
      (match Imap_store.Journal.find_local store ~scope ~local_id:local.id with
       | Some {remote_uid=Some paired_uid;_} -> paired_uid=uid
       | _ -> false));
  Alcotest.(check (list int64)) "repair left one server occurrence"
    [uid_raw] (remote_uids ())

let delete_crash_child dbfile mailbox pair_id operation_id =
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env_io in
  let _,client=connect env_io sw in
  let store=Imap_store.open_path ~sw
    ~blob_dir:Eio.Path.(fs / (dbfile ^ "-blobs")) Eio.Path.(fs / dbfile) in
  let pair=match Imap_store.Journal.find_pair store ~id:pair_id with
    | Some pair -> pair | None -> Alcotest.fail "delete child lost pair" in
  let epoch=Option.get pair.remote_uidvalidity in
  let uid=Option.get pair.remote_uid in
  let operation : Imap_store.Journal.operation = {
    id=operation_id;pair_id=Some pair_id;local_id=pair.local_id;
    scope=pair.scope;kind=Imap_store.Journal.Delete;
    state=Imap_store.Journal.Prepared;
    source_uidvalidity=Some epoch;source_uid=Some uid;
    destination=None;destination_uidvalidity=None;
    blob_sha256=pair.content_sha256;blob_length=pair.content_length;
    desired_flags=Some pair.common_flags;
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
    (fun selected ->
      let* info=Imap_eio.Selected.info selected in
      Alcotest.(check int64) "delete child epoch"
        (Imap.Uidvalidity.to_int64 epoch) info.uidvalidity;
      let* rows=Imap_eio.Selected.fetch selected ~uids:[uid]
        ~items:[Imap.Fetch_item.Modseq] in
      let modseq=match rows with
        | [row] when Imap.Uid.equal row.uid uid ->
            Imap.Modseq.to_int64 (Option.get row.modseq)
        | _ -> Alcotest.fail "delete child lost target metadata" in
      Imap_store.Journal.prepare_operation store operation;
      Imap_store.Journal.mark_sent store ~id:operation_id;
      let set=Imap.Uid_set.singleton uid in
      let deleted=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted in
      let* condstore=Imap_eio.Selected.Condstore.require selected in
      let* result=Imap_eio.Selected.Condstore.uid_store_flags condstore ~set
        ~operation:`Add ~flags:[deleted] ~unchangedsince:modseq in
      Alcotest.(check bool) "conditional delete accepted" true
        (Imap.Uid_set.is_empty result.modified);
      let* rows=Imap_eio.Selected.fetch selected ~uids:[uid]
        ~items:[Imap.Fetch_item.Modseq] in
      let after=match rows with
        | [row] when Imap.Uid.equal row.uid uid ->
            Some (Option.get row.flags,
              Option.map Imap.Modseq.to_int64 row.modseq)
        | _ -> None in
      Alcotest.(check bool) "delete child expunge preflight" true
        (Imap_sync.Deletion.expunge_preflight
           ~before_flags:pair.common_flags ~before_modseq:modseq after);
      let* ()=uid_expunge selected ~set in
      let* rows=Imap_eio.Selected.fetch selected ~uids:[uid] ~items:[] in
      Alcotest.(check int) "target absent before process exit" 0
        (List.length rows);
      Ok ()));
  (* The server accepted both mutations; SQLite has only the Sent operation. *)
  Unix._exit 77

let test_delete_process_crash () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env_io in
  let _,client=connect env_io sw in
  List.iter (require_capability client) ["CONDSTORE";"UIDPLUS"];
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-DeleteCrash" in
  let dbfile=Filename.temp_file "oxmono-dovecot-delete-crash-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let raw subject="From: delete@example.test\r\nSubject: " ^ subject ^
    " " ^ nonce ^ "\r\n\r\nKeep identities distinct.\r\n" in
  List.iter (fun subject ->
    let bytes=raw subject in
    unwrap (Result.map ignore (Imap_eio.Client.append client ~mailbox
      (Imap_eio.Client.append_message
         ~length:(Int64.of_int (String.length bytes))
         (Eio.Flow.string_source bytes))))) ["target";"unrelated"];
  let scope=dovecot_scope client mailbox in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  let open_store store_sw=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let copy store stage_id policy=match Imap_sync.Bridge.copy_once
    ~deletion_policy:policy
    ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir)
      ~next_id:(fun () -> "delete-import-" ^ Md.reserve_id ()) ())
    ~maildir ~stage_id () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "delete crash bridge: %a"
        Imap_sync.Error.pp error in
  let pair_id,target_uid,unrelated_uid=Eio.Switch.run (fun store_sw ->
    let store=open_store store_sw in
    let imported=copy store ("delete-crash-import-" ^ nonce)
      Imap.Sync_policy.Preserve in
    Alcotest.(check int) "two remote occurrences imported" 2
      imported.remote_to_local;
    let pairs=all_pairs store ~scope in
    let by_uid=List.sort (fun a b -> compare
      (Option.get a.Imap_store.Journal.remote_uid)
      (Option.get b.Imap_store.Journal.remote_uid)) pairs in
    let target,other=match by_uid with
      | [target;other] -> target,other
      | _ -> Alcotest.fail "expected two paired UIDs" in
    let target_local=Option.get target.local_id in
    Md.remove maildir
      (Option.get (Md.find maildir ~id:target_local));
    let held=copy store ("delete-crash-absence-" ^ nonce)
      Imap.Sync_policy.Preserve in
    Alcotest.(check int) "preserve policy holds deletion" 0
      held.deletions;
    Alcotest.(check int) "preserve policy reports one hold" 1
      held.deletions_held;
    let conflicts=all_open_conflicts store ~scope in
    Alcotest.(check int) "preserve hold is durable" 1
      (List.length (List.filter (fun (x:Imap_store.Journal.conflict) ->
        x.kind=Imap_store.Journal.Deletion_hold) conflicts));
    let hold_id=(List.hd conflicts).id in
    let held_again=copy store ("delete-crash-still-held-" ^ nonce)
      Imap.Sync_policy.Preserve in
    Alcotest.(check int) "repeated preserve hold" 1
      held_again.deletions_held;
    Alcotest.(check (list string)) "deletion hold ID stays stable"
      [hold_id]
      (List.map (fun (x:Imap_store.Journal.conflict) -> x.id)
        (all_open_conflicts store ~scope));
    let target=Option.get (Imap_store.Journal.find_pair store ~id:target.id) in
    Alcotest.(check bool) "local absence tombstone durable" true
      (match target.local_tombstone with
       | Some {reason=Imap_store.Journal.Local_absence;_} -> true
       | _ -> false);
    target.id,Imap.Uid.to_int64 (Option.get target.remote_uid),
      Imap.Uid.to_int64 (Option.get other.remote_uid)) in
  let operation_id="delete-crash-" ^ nonce in
  let executable=if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name in
  let pid=Unix.create_process executable
    [|executable;"--delete-crash-child";dbfile;mailbox;pair_id;
      operation_id|] Unix.stdin Unix.stdout Unix.stderr in
  let _,status=Unix.waitpid [] pid in
  Alcotest.(check bool) "child exited after accepted UID EXPUNGE" true
    (status=Unix.WEXITED 77);
  let remote_uids ()=raw_uids (unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_only mailbox (fun selected ->
      Imap_eio.Selected.uid_search selected ~criteria:Imap.Search.All))) in
  Alcotest.(check (list int64)) "only unrelated UID survives crash"
    [unrelated_uid] (remote_uids ());
  Eio.Switch.run (fun store_sw ->
    let store=open_store store_sw in
    Alcotest.(check bool) "delete intent remained Sent" true
      (match Imap_store.Journal.find_operation store ~id:operation_id with
       | Some {state=Imap_store.Journal.Sent;_} -> true | _ -> false);
    let recovered=copy store ("delete-crash-recovery-" ^ nonce)
      Imap.Sync_policy.Propagate in
    Alcotest.(check int) "no deletion mutation replayed" 0
      recovered.deletions;
    Alcotest.(check bool) "delete journal committed" true
      (match Imap_store.Journal.find_operation store ~id:operation_id with
       | Some {state=Imap_store.Journal.Committed;_} -> true | _ -> false);
    Alcotest.(check int) "deletion hold resolved after target absent" 0
      (List.length (all_open_conflicts store ~scope));
    Alcotest.(check bool) "target has inventory tombstone" true
      (match Imap_store.Journal.find_pair store ~id:pair_id with
       | Some {remote_tombstone=Some
           {reason=Imap_store.Journal.Inventory_absence;_};_} -> true
       | _ -> false));
  Alcotest.(check (list int64)) "recovery preserved unrelated UID"
    [unrelated_uid] (remote_uids ());
  Alcotest.(check bool) "expunged target was not resurrected" true
    (not (List.mem target_uid (remote_uids ())))

let test_flags_recovery () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env_io in
  let _,client=connect env_io sw in
  require_capability client "CONDSTORE";
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-FlagsRecovery" in
  let dbfile=Filename.temp_file "oxmono-dovecot-flags-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let raw="From: dovecot@example.test\r\nSubject: flags recovery " ^ nonce ^
    "\r\n\r\nBody\r\n" in
  let internal_date=match Imap.Internal_date.of_string
      "12-Jan-2020 12:00:00 +0000" with
    | Ok date -> date | Error message -> Alcotest.fail message in
  unwrap (Result.map ignore (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message ~internal_date
       ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))));
  let scope=dovecot_scope client mailbox in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  let open_store store_sw=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let next_id ()="flag-import-" ^ Md.reserve_id () in
  Eio.Switch.run @@ fun store_sw ->
  let store=open_store store_sw in
  (match Imap_sync.Bridge.copy_once
    ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir) ~next_id ())
    ~maildir ~stage_id:("flags-import-" ^ nonce) () with
   | Ok receipt -> Alcotest.(check int) "imported paired occurrence" 1
       receipt.remote_to_local
   | Error error -> Alcotest.failf "import: %a"
       Imap_sync.Error.pp error);
  let pair=match all_pairs store ~scope with
    | [pair] -> pair | _ -> Alcotest.fail "expected one pair" in
  let local_id=Option.get pair.local_id in
  let uid=Option.get pair.remote_uid in
  let epoch=Option.get pair.remote_uidvalidity in
  let flagged=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Flagged in
  let seen=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen in
  let draft=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Draft in
  let op id desired : Imap_store.Journal.operation = {
    id;pair_id=Some pair.id;local_id=Some local_id;scope;
    kind=Imap_store.Journal.Flags;state=Imap_store.Journal.Prepared;
    source_uidvalidity=Some epoch;source_uid=Some uid;
    destination=None;destination_uidvalidity=None;
    blob_sha256=None;blob_length=None;desired_flags=Some desired;
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
  let store_remote desired=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_write mailbox (fun selected ->
      let* _=Imap_eio.Selected.uid_store_flags selected
        ~set:(Imap.Uid_set.singleton uid)
        ~operation:`Replace ~flags:desired in Ok ())) in
  let first=op ("flags-recover-" ^ nonce) [flagged] in
  Imap_store.Journal.prepare_operation ~local_flags:[] store first;
  Imap_store.Journal.mark_sent store ~id:first.id;
  store_remote [flagged];
  (* Reopen the durable journal as the next process would.  No STORE is sent
     by recovery: only the local side may be finished. *)
  Eio.Switch.run @@ fun restart_sw ->
  let restarted=open_store restart_sw in
  let recover operation=Md.with_writer maildir (fun writer ->
    Imap_sync.Flags.recover_operation
      ~ctx:(sync_ctx ~client ~store:restarted ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir) ())
      ~writer ~operation ()) in
  let pending=Option.get (Imap_store.Journal.find_operation restarted
    ~id:first.id) in
  (match recover pending with
   | Ok (Imap_sync.Flags.Updated _) -> ()
   | Ok _ -> Alcotest.fail "one-sided FLAGS did not commit"
   | Error e -> Alcotest.failf "FLAGS recovery: %a"
       Imap_sync.Error.pp e);
  Alcotest.(check bool) "recovered local flag" true
    (List.mem flagged (Option.get (Md.find maildir
      ~id:local_id)).flags);
  Alcotest.(check bool) "journal committed after local write" true
    ((Option.get (Imap_store.Journal.find_operation restarted
      ~id:first.id)).state=Imap_store.Journal.Committed);
  let current=Option.get (Imap_store.Journal.find_pair restarted ~id:pair.id) in
  Alcotest.(check bool) "pair baseline advanced" true
    (List.mem flagged current.common_flags);
  let second=op ("flags-diverge-" ^ nonce) [flagged;seen] in
  Imap_store.Journal.prepare_operation ~local_flags:[flagged] restarted second;
  Imap_store.Journal.mark_sent restarted ~id:second.id;
  store_remote [flagged;seen];
  let local=Option.get (Md.find maildir ~id:local_id) in
  ignore (Md.set_flags maildir local [flagged;draft]);
  let pending=Option.get (Imap_store.Journal.find_operation restarted
    ~id:second.id) in
  (match recover pending with
   | Error (Imap_sync.Error.Pending_operations [id]) when id=second.id -> ()
   | _ -> Alcotest.fail "divergent local flags were overwritten");
  Alcotest.(check bool) "divergent local flag preserved" true
    (List.mem draft (Option.get (Md.find maildir
      ~id:local_id)).flags);
  Alcotest.(check bool) "divergent operation remains pending" true
    ((Option.get (Imap_store.Journal.find_operation restarted
      ~id:second.id)).state=Imap_store.Journal.Sent);
  let flag_conflicts ()=all_open_conflicts restarted ~scope
    |> List.filter (fun (conflict:Imap_store.Journal.conflict) ->
      conflict.kind=Imap_store.Journal.Flag_conflict) in
  let conflict=match flag_conflicts () with
    | [conflict] -> conflict
    | _ -> Alcotest.fail "pending FLAGS lacked a durable conflict" in
  (match recover pending with
   | Error (Imap_sync.Error.Pending_operations [id]) when id=second.id -> ()
   | _ -> Alcotest.fail "repeated divergent FLAGS recovery changed outcome");
  Alcotest.(check string) "flag conflict ID stays stable" conflict.id
    (List.hd (flag_conflicts ())).id;
  let local=Option.get (Md.find maildir ~id:local_id) in
  ignore (Md.set_flags maildir local [flagged]);
  (match recover pending with
   | Ok (Imap_sync.Flags.Updated _) -> ()
   | _ -> Alcotest.fail "restored FLAGS preimage did not recover");
  Alcotest.(check int) "flag conflict resolves with paired commit" 0
    (List.length (flag_conflicts ()));
  let third=op ("flags-settle-" ^ nonce) [seen] in
  let current=Option.get (Imap_store.Journal.find_pair restarted ~id:pair.id) in
  let paired_date=Option.get current.internal_date in
  let local=Option.get (Md.find maildir ~id:local_id) in
  let local_date=match Local_date.of_occurrence local with
    | Ok date -> date | Error message -> Alcotest.fail message in
  Alcotest.(check bool) "mtime preserves the paired instant" true
    (Imap.Internal_date.equal_instant local_date paired_date);
  Imap_store.Journal.prepare_operation ~local_flags:current.common_flags
    restarted third;
  Imap_store.Journal.mark_sent restarted ~id:third.id;
  store_remote [flagged;draft];
  let pending=Option.get (Imap_store.Journal.find_operation restarted
    ~id:third.id) in
  (match recover pending with
   | Error (Imap_sync.Error.Pending_operations [id]) when id=third.id -> ()
   | _ -> Alcotest.fail "divergent remote FLAGS was not held");
  let settle ?(scope=scope) evidence=Imap_sync.Repair.settle_flags
    ~ctx:(sync_ctx ~client ~store:restarted ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir) ())
    ~maildir ~id:third.id ~evidence () in
  (match settle "audit before endpoints agree" with
   | Error (Imap_sync.Error.Diverged _) -> ()
   | _ -> Alcotest.fail "divergent endpoints were settled");
  Alcotest.(check bool) "refused settlement remains pending" true
    ((Option.get (Imap_store.Journal.find_operation restarted
      ~id:third.id)).state=Imap_store.Journal.Sent);
  let local=Option.get (Md.find maildir ~id:local_id) in
  ignore (Md.set_flags maildir local [flagged;draft]);
  (match settle ~scope:{scope with account="foreign"} "foreign scope" with
   | Error _ -> () | Ok _ -> Alcotest.fail "foreign scope settled FLAGS");
  let getenv name=if name="IMAP_FLAGS_REPAIR_SECRET" then
      Some (env "IMAP_DOVECOT_PASSWORD") else None in
  let args=[|"imap-sync";"settle-flags";
    "--host";env "IMAP_DOVECOT_HOST";
    "--port";env "IMAP_DOVECOT_PORT";"--tls";"plain";
    "--user";env "IMAP_DOVECOT_USER";
    "--auth";"cram-md5";
    "--password-env";"IMAP_FLAGS_REPAIR_SECRET";
    "--endpoint";scope.endpoint;"--account";scope.account;
    "--mailbox";mailbox;"--db";dbfile;"--maildir";maildir_path;
    "--operation-id";third.id;
    "--evidence";"operator aligned Dovecot and Maildir flags"|] in
  let config=cli_job ~env:getenv args in
  Alcotest.(check int) "CLI settled matching endpoints" 0
    (Imap_cli.run config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io) ~env:getenv);
  let current=Option.get (Imap_store.Journal.find_pair restarted ~id:pair.id) in
  Alcotest.(check (list string)) "operator flags adopted as baseline"
    ["\\Draft";"\\Flagged"]
    (List.map Mail_flag.Imap_flag.to_wire current.common_flags
     |> List.sort String.compare);
  Alcotest.(check bool) "superseded FLAGS intent rejected" true
    ((Option.get (Imap_store.Journal.find_operation restarted
      ~id:third.id)).state=Imap_store.Journal.Rejected);
  Alcotest.(check int) "settlement resolved flag conflict" 0
    (List.length (flag_conflicts ()));
  Alcotest.(check int) "settlement cannot repeat" 9
    (Imap_cli.run config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io) ~env:getenv);
  Alcotest.(check int64) "repeat did not advance pair"
    current.revision
    (Option.get (Imap_store.Journal.find_pair restarted ~id:pair.id)).revision

let test_operator_local_delete_repair () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-LocalRepair" in
  let dbfile=Filename.temp_file "oxmono-dovecot-local-repair-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  let fs=Eio.Stdenv.fs env_io in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let raw="From: repair@example.test\r\nSubject: local repair " ^ nonce ^
    "\r\n\r\nUnchanged local survivor\r\n" in
  unwrap (Result.map ignore (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message
       ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))));
  let scope=dovecot_scope client mailbox in
  Eio.Switch.run @@ fun store_sw ->
  let store=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  let n=ref 0 in
  let next_id ()=incr n;Printf.sprintf "repair-%s-%d" nonce !n in
  let copy stage_id=match Imap_sync.Bridge.copy_once
    ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir) ~next_id ())
    ~maildir ~stage_id () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "local repair setup: %a"
        Imap_sync.Error.pp error in
  ignore (copy ("local-repair-import-" ^ nonce));
  let pair=match all_pairs store ~scope with
    | [pair] -> pair | _ -> Alcotest.fail "expected one imported pair" in
  let uid=Option.get pair.remote_uid in
  unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_write mailbox
    (fun selected ->
      let set=Imap.Uid_set.singleton uid in
      let deleted=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted in
      let* _=Imap_eio.Selected.uid_store_flags selected ~set
        ~operation:`Add ~flags:[deleted] in
      uid_expunge selected ~set));
  ignore (copy ("local-repair-absence-" ^ nonce));
  let pair=Option.get (Imap_store.Journal.find_pair store ~id:pair.id) in
  Alcotest.(check bool) "published remote tombstone" true
    (match pair.remote_tombstone with
     | Some {reason=Imap_store.Journal.Inventory_absence;_} -> true
     | _ -> false);
  let local_id=Option.get pair.local_id in
  let operation_id="local-repair-" ^ nonce in
  let operation : Imap_store.Journal.operation = {
    id=operation_id;pair_id=Some pair.id;local_id=Some local_id;scope;
    kind=Imap_store.Journal.Local_delete;state=Imap_store.Journal.Prepared;
    source_uidvalidity=pair.remote_uidvalidity;
    source_uid=pair.remote_uid;destination=None;
    destination_uidvalidity=None;
    blob_sha256=pair.content_sha256;
    blob_length=pair.content_length;
    desired_flags=Some pair.common_flags;
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
  Imap_store.Journal.prepare_operation store operation;
  Imap_store.Journal.mark_sent store ~id:operation_id;
  let repair ?(scope=scope) evidence =
    Imap_sync.Repair.local_delete
      ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir) ())
      ~maildir ~id:operation_id ~evidence () in
  (match repair "" with
   | Error _ -> () | Ok _ -> Alcotest.fail "empty evidence repaired");
  (match repair ~scope:{scope with account="foreign"} "audit" with
   | Error _ -> () | Ok _ -> Alcotest.fail "foreign scope repaired");
  let local=Option.get (Md.find maildir ~id:local_id) in
  let flagged=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Flagged in
  let changed=Md.set_flags maildir local [flagged] in
  (match repair "operator checked target" with
   | Error Imap_sync.Error.Identity_changed -> ()
   | _ -> Alcotest.fail "changed local flags repaired");
  Alcotest.(check bool) "local file retained after refusal" true
    (Option.is_some (Md.find maildir ~id:local_id));
  Alcotest.(check bool) "journal still Sent after refusal" true
    ((Option.get (Imap_store.Journal.find_operation store
      ~id:operation_id)).state=Imap_store.Journal.Sent);
  ignore (Md.set_flags maildir changed pair.common_flags);
  (match repair "Dovecot audit: UID absent and local bytes checked" with
   | Ok (Imap_sync.Deletion.Deleted _) -> ()
   | Error e -> Alcotest.failf "local deletion repair: %a"
       Imap_sync.Error.pp e
   | Ok _ -> Alcotest.fail "local deletion was not committed");
  Alcotest.(check bool) "exact local file removed" true
    (Md.find maildir ~id:local_id=None);
  Alcotest.(check bool) "journal committed" true
    ((Option.get (Imap_store.Journal.find_operation store
      ~id:operation_id)).state=Imap_store.Journal.Committed);
  Alcotest.(check bool) "local tombstone committed" true
    (match Imap_store.Journal.find_pair store ~id:pair.id with
     | Some {local_tombstone=Some
         {reason=Imap_store.Journal.Explicit_delete;_};_} -> true
     | _ -> false)

let test_operator_local_append_repair () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-AppendRepair" in
  let dbfile=Filename.temp_file "oxmono-dovecot-append-repair-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  let fs=Eio.Stdenv.fs env_io in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let raw="From: repair@example.test\r\nSubject: append repair " ^ nonce ^
    "\r\n\r\nExact remote bytes\r\n" in
  let date=match Imap.Internal_date.of_string
    "26-Sep-2025 12:34:56 +0000" with
    | Ok value -> value | Error error -> Alcotest.fail error in
  let receipt=match unwrap (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message ~internal_date:date
       ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))) with
    | Some value -> value | None -> Alcotest.fail "missing APPENDUID" in
  let scope=dovecot_scope client mailbox in
  Eio.Switch.run @@ fun store_sw ->
  let store=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  (match Imap_sync.Engine.scan_once
      ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir) ())
      ~stage_id:("append-repair-scan-" ^ nonce) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "repair setup scan: %a"
       Imap_sync.Error.pp error);
  let blob=Imap_store.Blob.put store ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw)) () in
  let local_id=Md.reserve_id () in
  let id="append-repair-" ^ nonce in
  let operation : Imap_store.Journal.operation = {
    id;pair_id=None;local_id=Some local_id;scope;
    kind=Imap_store.Journal.Local_append;state=Imap_store.Journal.Prepared;
    source_uidvalidity=Some receipt.uidvalidity;
    source_uid=Some receipt.uid;destination=None;
    destination_uidvalidity=None;blob_sha256=Some blob.sha256;
    blob_length=Some blob.length;desired_flags=Some [];
    internal_date=Some date;append=None;
    receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
  Imap_store.Journal.prepare_operation store operation;
  Imap_store.Journal.mark_sent store ~id;
  let copy stage_id=Imap_sync.Bridge.copy_once
    ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir)
      ~next_id:(fun () -> "unexpected-copy-" ^ nonce) ())
    ~maildir ~stage_id () in
  (match copy ("append-repair-pending-" ^ nonce) with
   | Error (Imap_sync.Error.Pending_operations [pending])
     when pending=id -> ()
   | Error error -> Alcotest.failf "wrong missing-file outcome: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "missing local append was replayed");
  let repair ?(scope=scope) evidence=Imap_sync.Repair.local_append
    ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir) ())
    ~maildir ~id ~evidence () in
  (match repair "" with Error _ -> () | Ok () -> Alcotest.fail "empty evidence accepted");
  (match repair ~scope:{scope with account="foreign"} "audit" with
   | Error _ -> () | Ok () -> Alcotest.fail "foreign scope accepted");
  let flagged=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Flagged in
  let set_flags flags=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_write mailbox (fun selected ->
      let set=Imap.Uid_set.singleton receipt.uid in
      let* _=Imap_eio.Selected.uid_store_flags selected ~set
        ~operation:`Replace ~flags in
      Ok ())) in
  set_flags [flagged];
  (match repair "audit" with
   | Error (Imap_sync.Error.Flags_diverged _) -> ()
   | Error error -> Alcotest.failf "wrong changed-flags refusal: %a"
       Imap_sync.Error.pp error
   | Ok () -> Alcotest.fail "changed flags repaired");
  Alcotest.(check bool) "no local file after refusal" true
    (Md.find maildir ~id:local_id=None);
  set_flags [];
  (match repair "operator verified Dovecot source" with
   | Ok () -> ()
   | Error error -> Alcotest.failf "local append repair: %a"
       Imap_sync.Error.pp error);
  let local=Option.get (Md.find maildir ~id:local_id) in
  Alcotest.(check string) "repaired exact bytes" blob.sha256
    (Md.sha256 maildir local);
  Alcotest.(check (option string)) "repaired INTERNALDATE"
    (Some (Imap.Internal_date.to_string date))
    (Option.map Imap.Internal_date.to_string (occurrence_date local));
  Alcotest.(check bool) "repair committed" true
    ((Option.get (Imap_store.Journal.find_operation store ~id)).state=
      Imap_store.Journal.Committed);
  Alcotest.(check int) "one pair" 1
    (List.length (all_pairs store ~scope));
  (match copy ("append-repair-resume-" ^ nonce) with
   | Ok result -> Alcotest.(check int) "no duplicate import" 0
       result.remote_to_local
   | Error error -> Alcotest.failf "post-repair sync: %a"
       Imap_sync.Error.pp error)

let test_deletion_grace_live () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env_io in
  let _,client=connect env_io sw in
  List.iter (require_capability client) ["CONDSTORE";"UIDPLUS"];
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-Grace" in
  let dbfile=Filename.temp_file "oxmono-dovecot-grace-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let raw="From: grace@example.test\r\nSubject: grace " ^ nonce ^
    "\r\n\r\nOriginal body\r\n" in
  unwrap (Result.map ignore (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message
       ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))));
  let scope=dovecot_scope client mailbox in
  Eio.Switch.run @@ fun store_sw ->
  let store=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  let n=ref 0 in
  let next_id ()=incr n;Printf.sprintf "grace-%s-%d" nonce !n in
  let copy ?(grace=0) stage_id=match Imap_sync.Bridge.copy_once
    ~min_absence_scans:grace ~deletion_policy:Imap.Sync_policy.Propagate
    ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir) ~next_id ())
    ~maildir ~stage_id () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "grace bridge: %a"
        Imap_sync.Error.pp error in
  ignore (copy ("grace-import-" ^ nonce));
  let pair=match all_pairs store ~scope with
    | [pair] -> pair | _ -> Alcotest.fail "expected one imported pair" in
  Md.remove maildir
    (Option.get (Md.find maildir
      ~id:(Option.get pair.local_id)));
  let server_uids ()=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_only mailbox (fun selected ->
      Imap_eio.Selected.uid_search selected ~criteria:Imap.Search.All)) in
  let first=copy ~grace:1 ("grace-first-" ^ nonce) in
  Alcotest.(check int) "first absence held" 1 first.deletions_held;
  Alcotest.(check int) "first absence did not delete" 0 first.deletions;
  Alcotest.(check int) "remote message survives first scan" 1
    (List.length (server_uids ()));
  let changed=String.sub raw 0 (String.length raw-1) ^ "!" in
  let wrong=Md.append maildir
    ~id:(Option.get pair.local_id)
    ~source:(Eio.Flow.string_source changed)
    ~length:(Int64.of_int (String.length changed))
    ~flags:pair.common_flags ?mtime:(Option.map mtime pair.internal_date)
    () in
  ignore (copy ~grace:1 ("grace-changed-" ^ nonce));
  let content_conflicts ()=all_open_conflicts store ~scope
    |> List.filter (fun (x:Imap_store.Journal.conflict) ->
      x.kind=Imap_store.Journal.Content_conflict) in
  Alcotest.(check int) "changed restoration creates content conflict" 1
    (List.length (content_conflicts ()));
  Md.remove maildir wrong;
  ignore (copy ~grace:1 ("grace-changed-absent-" ^ nonce));
  Alcotest.(check int) "content conflict survives renewed absence" 1
    (List.length (content_conflicts ()));
  Alcotest.(check int) "remote survives content conflict" 1
    (List.length (server_uids ()));
  let restored=Md.append maildir
    ~id:(Option.get pair.local_id)
    ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw))
    ~flags:pair.common_flags ?mtime:(Option.map mtime pair.internal_date)
    () in
  let seen=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen in
  let restored=Md.set_flags maildir restored [seen] in
  let present=copy ~grace:1 ("grace-present-" ^ nonce) in
  Alcotest.(check int) "restored local file prevents deletion" 0
    present.deletions;
  Alcotest.(check int) "reactivated pair resumes flag reconciliation" 1
    present.flags_updated;
  Alcotest.(check bool) "local absence tombstone cleared" true
    ((Option.get (Imap_store.Journal.find_pair store ~id:pair.id))
       .local_tombstone=None);
  Alcotest.(check int) "exact restoration resolves content conflict" 0
    (List.length (content_conflicts ()));
  Alcotest.(check (option int64)) "local presence recorded"
    (Some present.cursor.generation)
    (Imap_store.Journal.last_presence_generation store ~pair_id:pair.id
      ~side:`Local);
  Md.remove maildir restored;
  let again=copy ~grace:1 ("grace-absent-again-" ^ nonce) in
  Alcotest.(check int) "new first absence held" 1 again.deletions_held;
  Alcotest.(check int) "remote survives new first absence" 1
    (List.length (server_uids ()));
  let second=copy ~grace:1 ("grace-second-" ^ nonce) in
  Alcotest.(check bool) "later complete scan advanced" true
    (second.cursor.generation>again.cursor.generation);
  Alcotest.(check int) "later scan deleted survivor" 1 second.deletions;
  Alcotest.(check int) "targeted delete removed remote message" 0
    (List.length (server_uids ()))

let test_reject_unchanged_remote_delete () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-DeleteReject" in
  let dbfile=Filename.temp_file "oxmono-dovecot-delete-reject-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  let fs=Eio.Stdenv.fs env_io in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let raw="From: reject@example.test\r\nSubject: delete " ^ nonce ^
    "\r\n\r\nOriginal remote body\r\n" in
  unwrap (Result.map ignore (Imap_eio.Client.append client ~mailbox
    (Imap_eio.Client.append_message
       ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))));
  let scope=dovecot_scope client mailbox in
  Eio.Switch.run @@ fun store_sw ->
  let store=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let maildir=Md.open_dir Eio.Path.(fs / maildir_path) in
  let n=ref 0 in
  let next_id ()=incr n;Printf.sprintf "delete-reject-%s-%d" nonce !n in
  let copy stage_id=match Imap_sync.Bridge.copy_once
    ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
      ~spool_dir:Eio.Path.(fs / spooldir) ~next_id ())
    ~maildir ~stage_id () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "delete reject setup: %a"
        Imap_sync.Error.pp error in
  ignore (copy ("delete-reject-import-" ^ nonce));
  let pair=match all_pairs store ~scope with
    | [pair] -> pair | _ -> Alcotest.fail "expected one imported pair" in
  let uid=Option.get pair.remote_uid in
  let local_id=Option.get pair.local_id in
  Md.remove maildir
    (Option.get (Md.find maildir ~id:local_id));
  ignore (copy ("delete-reject-absence-" ^ nonce));
  let pair=Option.get (Imap_store.Journal.find_pair store ~id:pair.id) in
  let operation_id="remote-delete-reject-" ^ nonce in
  let operation:Imap_store.Journal.operation={
    id=operation_id;pair_id=Some pair.id;local_id=Some local_id;scope;
    kind=Imap_store.Journal.Delete;state=Imap_store.Journal.Prepared;
    source_uidvalidity=pair.remote_uidvalidity;source_uid=pair.remote_uid;
    destination=None;destination_uidvalidity=None;
    blob_sha256=pair.content_sha256;blob_length=pair.content_length;
    desired_flags=Some pair.common_flags;
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
  Imap_store.Journal.prepare_operation store operation;
  Imap_store.Journal.mark_sent store ~id:operation_id;
  let reject ?(scope=scope) evidence =
    Imap_sync.Repair.reject_remote_delete
      ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir) ())
      ~maildir ~id:operation_id ~evidence () in
  (match reject ~scope:{scope with account="foreign"} "audit" with
   | Error _ -> () | Ok () -> Alcotest.fail "foreign scope rejected delete");
  let flagged=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Flagged in
  let set=Imap.Uid_set.singleton uid in
  let edit operation=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_write mailbox (fun selected ->
      let* _=Imap_eio.Selected.uid_store_flags selected ~set ~operation
        ~flags:[flagged] in Ok ())) in
  edit `Add;
  (match reject "operator verified unchanged UID" with
   | Error Imap_sync.Error.Identity_changed -> ()
   | Error error -> Alcotest.failf "wrong changed-flags refusal: %a"
       Imap_sync.Error.pp error
   | Ok () -> Alcotest.fail "changed flags rejected pending delete");
  Alcotest.(check bool) "changed remote target stays pending" true
    ((Option.get (Imap_store.Journal.find_operation store
      ~id:operation_id)).state=Imap_store.Journal.Sent);
  edit `Remove;
  let cli_args=[|"imap-sync";"reject-remote-delete";
    "--host";env "IMAP_DOVECOT_HOST";
    "--port";env "IMAP_DOVECOT_PORT";"--tls";"plain";
    "--user";env "IMAP_DOVECOT_USER";
    "--password-env";"IMAP_DOVECOT_PASSWORD";
    "--auth";"cram-md5";"--endpoint";scope.endpoint;
    "--account";scope.account;"--mailbox";mailbox;
    "--db";dbfile;"--maildir";maildir_path;
    "--spool-dir";spooldir;"--operation-id";operation_id;
    "--evidence";"operator verified unchanged UID"|] in
  let cli_config=cli_job ~env:Sys.getenv_opt cli_args in
  Alcotest.(check int) "CLI rejects verified unchanged intent" 0
    (Imap_cli.run cli_config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt);
  Alcotest.(check bool) "unchanged remote intent rejected" true
    ((Option.get (Imap_store.Journal.find_operation store
      ~id:operation_id)).state=Imap_store.Journal.Rejected);
  Alcotest.(check int64) "pair revision unchanged" pair.revision
    (Option.get (Imap_store.Journal.find_pair store ~id:pair.id)).revision;
  let remote=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_only mailbox (fun selected ->
      Imap_eio.Selected.uid_search selected ~criteria:Imap.Search.All)) in
  Alcotest.(check (list int64)) "remote UID preserved"
    [Imap.Uid.to_int64 uid] (raw_uids remote);
  let finish_id="remote-delete-finish-" ^ nonce in
  Imap_store.Journal.prepare_operation store {operation with id=finish_id};
  Imap_store.Journal.mark_sent store ~id:finish_id;
  let deleted=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted in
  let edit_deleted operation=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_write mailbox (fun selected ->
      let* _=Imap_eio.Selected.uid_store_flags selected ~set ~operation
        ~flags:[deleted] in Ok ())) in
  edit_deleted `Add;
  edit `Add;
  (match Imap_sync.Repair.finish_remote_delete
      ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir) ())
      ~maildir ~id:finish_id
      ~evidence:"operator verified marked target" () with
   | Error Imap_sync.Error.Identity_changed -> ()
   | Error error -> Alcotest.failf "wrong extra-flags refusal: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "extra remote flag was expunged");
  Alcotest.(check bool) "refusal did not attest operation" true
    ((Option.get (Imap_store.Journal.find_operation store
      ~id:finish_id)).state=Imap_store.Journal.Sent);
  edit `Remove;
  let finish_args=Array.mapi (fun i value ->
    if i=1 then "finish-remote-delete"
    else if value=operation_id then finish_id
    else if value="operator verified unchanged UID" then
      "operator verified marked target"
    else value) cli_args in
  let finish_config=cli_job ~env:Sys.getenv_opt finish_args in
  Alcotest.(check int) "CLI targeted EXPUNGE succeeds" 0
    (Imap_cli.run finish_config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt);
  let finished=Option.get (Imap_store.Journal.find_operation store
    ~id:finish_id) in
  Alcotest.(check bool) "targeted EXPUNGE journal committed" true
    (finished.state=Imap_store.Journal.Committed);
  Alcotest.(check (option string)) "operator evidence in committed receipt"
    (Some "operator targeted UID EXPUNGE: operator verified marked target; UID FETCH absent")
    finished.receipt;
  let remote=unwrap (Imap_eio.Client.with_mailbox client
    ~mode:`Read_only mailbox (fun selected ->
      Imap_eio.Selected.uid_search selected ~criteria:Imap.Search.All)) in
  Alcotest.(check (list int64)) "targeted UID absent" [] (raw_uids remote);
  Alcotest.(check bool) "pair has expunge receipt" true
    (match Imap_store.Journal.find_pair store ~id:pair.id with
     | Some {remote_tombstone=Some
         {reason=Imap_store.Journal.Expunge_receipt;_};_} -> true
     | _ -> false)

let test_bounded_hydration () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let nonce=Printf.sprintf "%d-%06x" (Unix.getpid ())
    (Random.bits () land 0xffffff) in
  let mailbox="Oxmono-Dovecot-" ^ nonce ^ "-Hydrate" in
  let dbfile=Filename.temp_file "oxmono-dovecot-hydrate-" ".sqlite" in
  let blobdir=dbfile ^ "-blobs" and spooldir=dbfile ^ "-spool" in
  let maildir_path=dbfile ^ "-maildir" in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  let fs=Eio.Stdenv.fs env_io in
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox);
    Imap_eio.Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile;dbfile ^ "-wal";dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true
      Eio.Path.(fs / path)) [blobdir;spooldir;maildir_path]) @@ fun () ->
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  let append subject =
    let raw="From: hydrate@example.test\r\nSubject: " ^ subject ^
      "\r\n\r\nExact body " ^ nonce ^ "\r\n" in
    let receipt=unwrap (Imap_eio.Client.append client ~mailbox
      (Imap_eio.Client.append_message
         ~length:(Int64.of_int (String.length raw))
         (Eio.Flow.string_source raw))) in
    (Option.get receipt,raw) in
  let first,raw_first=append "first" in
  let second,raw_second=append "second" in
  let scope=dovecot_scope client mailbox in
  Eio.Switch.run @@ fun store_sw ->
  let store=Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  (match Imap_sync.Engine.scan_once
      ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir) ())
      ~stage_id:("hydrate-scan-" ^ nonce) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "hydration setup scan: %a"
       Imap_sync.Error.pp error);
  let cursor=Imap_store.load_cursor store ~scope in
  let epoch=Option.get cursor.uidvalidity in
  Alcotest.(check bool) "no body before hydration" true
    (Imap_store.Blob.find store ~scope ~uidvalidity:epoch ~uid:first.uid=None);
  let serial=ref 0 in
  let next_spool_id ()=incr serial;nonce ^ "-" ^ string_of_int !serial in
  let hydrate ?(max_messages=1) ?(max_total_bytes=1024L) () =
    match Imap_sync.Engine.hydrate_once ~max_messages ~max_total_bytes
      ~ctx:(sync_ctx ~client ~store ~scope ~mailbox
        ~spool_dir:Eio.Path.(fs / spooldir) ~next_id:next_spool_id ())
      () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "hydration: %a"
        Imap_sync.Error.pp error in
  let blocked=hydrate ~max_total_bytes:1L () in
  Alcotest.(check int) "budget preflight does not fetch" 0 blocked.hydrated;
  Alcotest.(check bool) "budget reports pending body" true blocked.more;
  let cli_args=[|"imap-sync";"hydrate";
    "--host";env "IMAP_DOVECOT_HOST";
    "--port";env "IMAP_DOVECOT_PORT";"--tls";"plain";
    "--user";env "IMAP_DOVECOT_USER";"--auth";"cram-md5";
    "--password-env";"IMAP_DOVECOT_PASSWORD";
    "--endpoint";scope.endpoint;"--account";scope.account;
    "--mailbox";mailbox;"--db";dbfile;
    "--blob-dir";blobdir;"--spool-dir";spooldir;
    "--max-transfers";"1";"--max-total-bytes";"1024"|] in
  let cli_config=cli_job ~env:Sys.getenv_opt cli_args in
  Alcotest.(check int) "CLI bounded pass reports more" 2
    (Imap_cli.run cli_config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt);
  Alcotest.(check bool) "first body durably attached" true
    (Option.is_some (Imap_store.Blob.find store ~scope
      ~uidvalidity:epoch ~uid:first.uid));
  Alcotest.(check bool) "second body still pending" true
    (Imap_store.Blob.find store ~scope
      ~uidvalidity:epoch ~uid:second.uid=None);
  let two=hydrate () in
  Alcotest.(check int) "second bounded pass" 1 two.hydrated;
  Alcotest.(check bool) "all bodies attached" false two.more;
  let done_pass=hydrate () in
  Alcotest.(check int) "idempotent hydration" 0 done_pass.hydrated;
  Alcotest.(check bool) "no remaining body" false done_pass.more;
  let sync_args=Array.append
    (Array.mapi (fun i arg ->
      if i=1 then "sync"
      else if i>0 && cli_args.(i-1)="--max-transfers" then "10"
      else arg) cli_args)
    [|"--maildir";maildir_path;"--hydrate-bodies"|] in
  let sync_config=cli_job ~env:Sys.getenv_opt sync_args in
  Alcotest.(check int) "sync schedules hydration after bridge" 0
    (Imap_cli.run sync_config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt);
  Alcotest.(check int) "bridge imported both occurrences" 2
    (List.length (Md.scan
      (Md.open_dir Eio.Path.(fs / maildir_path))));
  let raw_db=Sqlite3.db_open dbfile in
  let evicted=Fun.protect ~finally:(fun () ->
    ignore (Sqlite3.db_close raw_db : bool)) (fun () ->
    Sqlite3.exec raw_db "DELETE FROM blob_refs"=Sqlite3.Rc.OK) in
  Alcotest.(check bool) "evict only cache references" true
    evicted;
  Alcotest.(check bool) "both bodies need reattachment" true
    (Imap_store.Blob.find store ~scope ~uidvalidity:epoch ~uid:first.uid=None &&
     Imap_store.Blob.find store ~scope ~uidvalidity:epoch ~uid:second.uid=None);
  Alcotest.(check int) "sync rehydrates existing pairs" 0
    (Imap_cli.run sync_config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt);
  Alcotest.(check int) "rehydration did not duplicate Maildir messages" 2
    (List.length (Md.scan
      (Md.open_dir Eio.Path.(fs / maildir_path))));
  let second_blob=Option.get (Imap_store.Blob.find store ~scope
    ~uidvalidity:epoch ~uid:second.uid) in
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(fs / blobdir / ("sha256-" ^ second_blob.sha256))
    (String.make (String.length raw_second) 'x');
  Alcotest.(check bool) "corrupt cached body detected" false
    (Imap_store.Blob.verify store second_blob);
  let audit ?after_uid ?(max_messages=1) ?(max_total_bytes=1024L) () =
    match Imap_sync.Engine.audit_cache_once ?after_uid ~max_messages
      ~max_total_bytes ~store ~scope () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "cache audit: %a"
        Imap_sync.Error.pp error in
  let blocked_audit=audit ~max_total_bytes:1L () in
  Alcotest.(check int) "audit preflight respects bytes" 0
    blocked_audit.checked;
  Alcotest.(check bool) "audit reports remaining references" true
    blocked_audit.more;
  let first_audit=audit () in
  Alcotest.(check int) "first audit page" 1 first_audit.checked;
  Alcotest.(check int) "first blob is healthy" 0 first_audit.invalidated;
  Alcotest.(check bool) "second audit page remains" true first_audit.more;
  Alcotest.(check bool) "changed audit revision rejected" true
    (Imap_sync.Engine.audit_cache_once ~after_uid:first.uid
      ~expected_revision:(Int64.sub first_audit.cursor.revision 1L)
      ~store ~scope ()=Error Imap_sync.Error.Store_stale_revision);
  let second_audit=audit ?after_uid:first_audit.last_uid () in
  Alcotest.(check int) "second audit page" 1 second_audit.checked;
  Alcotest.(check int) "corrupt reference invalidated" 1
    second_audit.invalidated;
  Alcotest.(check bool) "audit page complete" false second_audit.more;
  Alcotest.(check bool) "corrupt reference removed" true
    (Imap_store.Blob.find store ~scope
      ~uidvalidity:epoch ~uid:second.uid=None);
  let repaired=hydrate () in
  Alcotest.(check int) "invalidated body fetched again" 1 repaired.hydrated;
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(fs / blobdir / ("sha256-" ^ second_blob.sha256))
    (String.make (String.length raw_second) 'y');
  let audit_args=[|"imap-sync";"audit-cache";
    "--endpoint";scope.endpoint;"--account";scope.account;
    "--mailbox";mailbox;"--db";dbfile;"--blob-dir";blobdir;
    "--max-transfers";"1";"--max-total-bytes";"1024";
    "--after-uid";Int64.to_string (Imap.Uid.to_int64 first.uid);
    "--expected-revision";
    Int64.to_string (Imap_store.load_cursor store ~scope).revision|] in
  let audit_config=cli_job ~env:Sys.getenv_opt audit_args in
  Alcotest.(check int) "offline CLI invalidates corrupt cache" 0
    (Imap_cli.run audit_config ~net:(Eio.Stdenv.net env_io) ~fs
      ~random:(Eio.Stdenv.secure_random env_io)
      ~env:Sys.getenv_opt);
  Alcotest.(check bool) "offline audit removed bad reference" true
    (Imap_store.Blob.find store ~scope
      ~uidvalidity:epoch ~uid:second.uid=None);
  Alcotest.(check int) "CLI-audited body fetched again" 1
    (hydrate ()).hydrated;
  List.iter (fun ((receipt:Imap_eio.Client.append_receipt),raw) ->
    let blob=Option.get (Imap_store.Blob.find store ~scope
      ~uidvalidity:epoch ~uid:receipt.uid) in
    Alcotest.(check int64) "exact message length"
      (Int64.of_int (String.length raw)) blob.length;
    Alcotest.(check string) "exact message digest"
      (Digestif.SHA256.digest_string raw |> Digestif.SHA256.to_hex)
      blob.sha256;
    Alcotest.(check bool) "durable blob verifies" true
      (Imap_store.Blob.verify store blob))
    [first,raw_first;second,raw_second];
  Alcotest.(check (list string)) "provisional spools removed" []
    (Eio.Path.read_dir Eio.Path.(fs / spooldir))

let test_multiappend () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let _,client=connect env_io sw in
  let mailbox=Printf.sprintf "Oxmono-Multiappend-%d-%06x"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  unwrap (Imap_eio.Client.create_mailbox client ~mailbox);
  Fun.protect ~finally:(fun () ->
    ignore (Imap_eio.Client.delete_mailbox client ~mailbox))
    (fun () ->
      let date=match Imap.Internal_date.of_string "26-Sep-2025 12:34:56 +0230" with
        | Ok date -> date | Error message -> Alcotest.fail message in
      let bodies=["From: a@example.test\r\nSubject: First\r\n\r\none\r\n";
        "From: b@example.test\r\nSubject: Second\r\n\r\ntwo\r\n"] in
      let flags=[["\\Seen";"batch-keyword"];["\\Flagged"]] in
      let messages=List.map2 (fun body flags ->
        Imap_eio.Client.append_message ~length:(Int64.of_int (String.length body))
          ~flags:(List.map flag_of_wire flags) ~internal_date:date
          (Eio.Flow.string_source body)) bodies flags in
      let multiappend=unwrap (Imap_eio.Client.Multiappend.require client) in
      let receipt=match unwrap (Imap_eio.Client.Multiappend.append_many
          multiappend ~mailbox messages) with
        | Some receipt -> receipt | None -> Alcotest.fail "Dovecot omitted batch UID receipt" in
      if List.length receipt.uids<>2 then Alcotest.fail "wrong batch UID cardinality";
      unwrap (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox (fun selected ->
        List.iter2 (fun uid (body,expected_flags) ->
          let rows=unwrap (Imap_eio.Selected.fetch selected ~uids:[uid]
            ~items:[Imap.Fetch_item.Internal_date]) in
          let row=match rows with [row] -> row | _ -> Alcotest.fail "missing batch row" in
          if not (Option.fold ~none:false ~some:(Imap.Internal_date.equal_instant date)
            row.internal_date) then Alcotest.fail "batch date changed";
          let normalize flags=List.map String.lowercase_ascii flags |>
            List.filter ((<>) "\\recent") |> List.sort_uniq String.compare in
          Alcotest.(check (list string)) "per-message flags" (normalize expected_flags)
            (normalize (wires (Option.get row.flags)));
          let buffer=Buffer.create 128 in
          unwrap (Imap_eio.Selected.fetch_to selected ~uid (Eio.Flow.buffer_sink buffer));
          Alcotest.(check string) "UID corresponds to input message" body (Buffer.contents buffer))
          receipt.uids (List.combine bodies flags);
        Ok ())))

let test_shared_maildir () =
  configured ();
  let path=match Sys.getenv_opt "IMAP_DOVECOT_SHARED_MAILDIR" with
    | Some path -> path | None -> Alcotest.skip () in
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let module D=Md in
  let module S=Imap_eio.Selected in
  let flag name=match Mail_flag.Imap_flag.of_wire name with
    | Ok flag -> flag | Error message -> Alcotest.fail message in
  let date=match Imap.Internal_date.of_string "26-Sep-2025 12:34:56 +0230" with
    | Ok date -> date | Error message -> Alcotest.fail message in
  let maildir=D.open_dir Eio.Path.(Eio.Stdenv.fs env_io / path) in
  let id=D.reserve_id () in
  let body="From: shared@example.test\r\nSubject: " ^ id ^
    "\r\nMessage-ID: <" ^ id ^ "@example.test>\r\n\r\nshared body\r\n" in
  let initial=[flag "\\Seen";flag "local-keyword"] in
  let occurrence=D.append maildir ~id ~source:(Eio.Flow.string_source body)
    ~length:(Int64.of_int (String.length body)) ~flags:initial
    ~mtime:(mtime date) () in
  let _,client=connect env_io sw in
  let with_selected mode f=unwrap (Imap_eio.Client.with_mailbox client
    ~mode "INBOX" (fun selected -> Ok (f selected))) in
  let uid=with_selected `Read_only (fun selected ->
    match unwrap (S.uid_search selected ~criteria:(Imap.Search.Header
      ("Message-ID", "<" ^ id ^ "@example.test>"))) with
    | [uid] -> uid | _ -> Alcotest.fail "Dovecot did not discover shared message") in
  let check_remote expected=with_selected `Read_only (fun selected ->
    let rows=unwrap (S.fetch selected ~uids:[uid]
      ~items:[Imap.Fetch_item.Internal_date]) in
    let row=match rows with [row] -> row | _ -> Alcotest.fail "missing shared metadata" in
    let actual=match row.flags with
      | Some flags -> wires flags | None -> Alcotest.fail "missing flags" in
    let normalize flags=List.map String.lowercase_ascii flags |> List.filter
      ((<>) "\\recent") |> List.sort_uniq String.compare in
    Alcotest.(check (list string)) "Dovecot sees file flags"
      (normalize (List.map Mail_flag.Imap_flag.to_wire expected)) (normalize actual);
    if not (Option.fold ~none:false ~some:(Imap.Internal_date.equal_instant date)
      row.internal_date) then Alcotest.fail "Dovecot changed file INTERNALDATE";
    let buffer=Buffer.create 128 in
    unwrap (S.fetch_to selected ~uid (Eio.Flow.buffer_sink buffer));
    Alcotest.(check string) "Dovecot sees exact body" body (Buffer.contents buffer)) in
  check_remote initial;
  let remote_flags=[flag "\\Answered";flag "remote-keyword"] in
  with_selected `Read_write (fun selected ->
    ignore (unwrap (S.uid_store_flags selected ~set:(Imap.Uid_set.singleton uid)
      ~operation:`Replace ~flags:remote_flags)));
  let current=match D.find maildir ~id with
    | Some current -> current | None -> Alcotest.fail "Dovecot renamed occurrence identity" in
  let normalize=List.sort_uniq Mail_flag.Imap_flag.compare in
  if normalize current.flags<>normalize remote_flags then
    Alcotest.fail "Maildir reader missed Dovecot keyword/filename update";
  Alcotest.(check (float 0.0)) "Dovecot preserved mtime" occurrence.mtime current.mtime;
  let final_flags=[flag "\\Flagged";flag "local-keyword";flag "remote-keyword"] in
  ignore (D.set_flags maildir current final_flags);
  check_remote final_flags;
  with_selected `Read_write (fun selected ->
    let set=Imap.Uid_set.singleton uid in
    ignore (unwrap (S.uid_store_flags selected ~set ~operation:`Add
      ~flags:[flag "\\Deleted"]));
    unwrap (uid_expunge selected ~set));
  if D.find maildir ~id<>None then Alcotest.fail "expunged file remains in shared Maildir"

let () = if Array.length Sys.argv=6 &&
  Sys.argv.(1)="--append-crash-child" then
  append_crash_child Sys.argv.(2) Sys.argv.(3) Sys.argv.(4) Sys.argv.(5)
else if Array.length Sys.argv=6 &&
  Sys.argv.(1)="--delete-crash-child" then
  delete_crash_child Sys.argv.(2) Sys.argv.(3) Sys.argv.(4) Sys.argv.(5)
else Alcotest.run "Dovecot IMAP" ["live", [
  Alcotest.test_case "CRAM-MD5 auth and list" `Quick test_auth;
  Alcotest.test_case "bounded CRAM-MD5 connection pool" `Quick
    test_bounded_pool;
  Alcotest.test_case "bounded durable body hydration" `Quick
    test_bounded_hydration;
  Alcotest.test_case "mailbox rename and subscriptions" `Quick
    test_mailbox_management;
  Alcotest.test_case "DEFLATE streaming over plain and TLS" `Quick test_compress;
  Alcotest.test_case "DEFLATE IDLE wakeup" `Quick (test_idle ~compress:true);
  Alcotest.test_case "binary APPEND decoded preservation" `Quick test_binary_append;
  Alcotest.test_case "structured rejection codes" `Quick test_rejection_codes;
  Alcotest.test_case "BINARY decoded sections and raw archival" `Quick test_binary_sections;
  Alcotest.test_case "SEARCHRES saved leases and mutations" `Quick test_saved_search;
  Alcotest.test_case "UID SORT and REFERENCES threading" `Quick test_sort_thread;
  Alcotest.test_case "INTERNALDATE APPEND and FETCH" `Quick
    test_internal_date_roundtrip;
  Alcotest.test_case "typed ENVELOPE and BODYSTRUCTURE" `Quick
    test_typed_mime_fetch;
  Alcotest.test_case "verified TLS auth and peer identity" `Quick test_tls_cram;
  Alcotest.test_case "CONDSTORE QRESYNC MOVE UID EXPUNGE" `Quick test_condstore_move_expunge;
  Alcotest.test_case "IDLE wakeup" `Quick (test_idle ~compress:false);
  Alcotest.test_case "durable IDLE watch" `Quick (test_durable_watch ~gap:false);
  Alcotest.test_case "watch scan-to-IDLE gap" `Quick (test_durable_watch ~gap:true);
  Alcotest.test_case "MULTIAPPEND ordered receipts and metadata" `Quick test_multiappend;
  Alcotest.test_case "shared filesystem Maildir interoperability" `Quick test_shared_maildir;
  Alcotest.test_case "CRAM-MD5 durable bridge" `Quick test_bridge_cram;
  Alcotest.test_case "populated bootstrap requires explicit choice" `Quick
    test_shared_mailbox_bootstrap;
  Alcotest.test_case "APPEND process crash and repair" `Quick
    test_append_process_crash;
  Alcotest.test_case "UID EXPUNGE process crash recovery" `Quick
    test_delete_process_crash;
  Alcotest.test_case "deletion grace on live Dovecot" `Quick
    test_deletion_grace_live;
  Alcotest.test_case "one-sided FLAGS recovery and divergence" `Quick
    test_flags_recovery;
  Alcotest.test_case "operator local delete repair" `Quick
    test_operator_local_delete_repair;
  Alcotest.test_case "resolve uncertain remote delete" `Quick
    test_reject_unchanged_remote_delete;
  Alcotest.test_case "operator local append repair" `Quick
    test_operator_local_append_repair;
]]
