module Client = Imap_eio.Client
module Selected = Imap_eio.Selected

let unwrap = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Client.error_to_string e)

let env name = match Sys.getenv_opt name with
  | Some s when s <> "" -> s
  | _ -> Alcotest.fail (name ^ " is unset")

let configured () =
  if Sys.getenv_opt "IMAP_STALWART_HOST" = None then
    if Sys.getenv_opt "IMAP_STALWART_REQUIRED" = Some "1" then
      Alcotest.fail "IMAP_STALWART_REQUIRED=1 but IMAP_STALWART_HOST is unset"
    else Alcotest.skip ()

let transport env_io =
  let tls, authenticator = match Sys.getenv_opt "IMAP_STALWART_CA_CERT" with
    | None -> `Plain, None
    | Some path ->
        let pem = In_channel.with_open_bin path In_channel.input_all in
        let ca = match X509.Certificate.decode_pem pem with
          | Ok ca -> ca | Error (`Msg message) -> Alcotest.fail message in
        let time () = Some (Ptime_clock.now ()) in
        let hash = `SHA256 in
        let fingerprint = X509.Certificate.fingerprint hash ca in
        `Implicit, Some (X509.Authenticator.cert_fingerprint ~time ~hash
          ~fingerprint)
  in
  Imap_eio.Transport.v ~net:(Eio.Stdenv.net env_io)
    ~host:(env "IMAP_STALWART_HOST")
    ~port:(int_of_string (env "IMAP_STALWART_PORT")) ~tls
    ?authenticator ()

let connect env_io sw =
  let transport = transport env_io in
  (* The pinned release does not advertise CRAM-MD5. The v0.16 fixture
     uses verified implicit TLS; v0.15 uses isolated loopback plaintext. *)
  let auth = Imap_eio.Auth.password ~username:(env "IMAP_STALWART_USER")
    ~password:(env "IMAP_STALWART_PASSWORD") ~mechanism:`Plain
    ~allow_insecure_transport:true () in
  unwrap (Client.connect ~sw ~auth transport)

let ( let* ) r f = match r with Ok x -> f x | Error _ as error -> error

let nonce () = Printf.sprintf "%d-%06x" (Unix.getpid ())
  (Random.bits () land 0xffffff)

let raw nonce part =
  "From: fixture@example.test\r\nSubject: stalwart " ^ part ^ " " ^ nonce ^
  "\r\nMessage-ID: <" ^ nonce ^ "-" ^ part ^ "@example.test>\r\n" ^
  "\r\nExact Stalwart body " ^ part ^ ".\r\n"

let advertised client capability =
  Imap.Capability.Set.mem capability (Client.capabilities client)

let require_capability client capability =
  Alcotest.(check bool) capability true
    (advertised client (Imap.Capability.of_wire capability))

let test_protocol () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let client = connect env_io sw in
  let mailbox = "Oxmono-Stalwart-" ^ nonce () ^ "-Protocol" in
  Fun.protect ~finally:(fun () ->
    ignore (Client.delete_mailbox client mailbox); Client.close client) @@ fun () ->
  Alcotest.(check bool) "Stalwart does not advertise CRAM-MD5" false
    (advertised client (Imap.Capability.Auth "CRAM-MD5"));
  let transport = transport env_io in
  let cram = Imap_eio.Auth.password ~username:(env "IMAP_STALWART_USER")
    ~password:(env "IMAP_STALWART_PASSWORD") ~mechanism:`Cram_md5 () in
  (match Client.connect ~sw ~auth:cram transport with
   | Error (Imap_eio.Error.Unsupported (Imap.Capability.Auth "CRAM-MD5")) ->
       ()
   | Error e -> Alcotest.fail ("unexpected CRAM-MD5 rejection: " ^
       Client.error_to_string e)
   | Ok unexpected -> Client.close unexpected;
       Alcotest.fail "Stalwart unexpectedly accepted CRAM-MD5");
  List.iter (require_capability client)
    ["UIDPLUS"; "CONDSTORE"; "QRESYNC"];
  let objectid_plus=advertised client Imap.Capability.Objectid_plus in
  if Sys.getenv_opt "IMAP_STALWART_OBJECTID_PLUS_REQUIRED"=Some "1" &&
      not objectid_plus then
    Alcotest.fail "OBJECTID+ required but not advertised";
  if objectid_plus then unwrap (Client.enable_objectid_plus client);
  let objectid=if objectid_plus then (
    let ids=unwrap (Client.create_mailbox_objectid client mailbox) in
    match ids with
    | {account_id=Some account_id;mailbox_id=Some mailbox_id;_} ->
        Some (account_id,mailbox_id)
    | _ -> Alcotest.fail "OBJECTID+ CREATE omitted account/mailbox context")
    else (unwrap (Client.create_mailbox client mailbox); None) in
  if objectid_plus then (
    let status=unwrap (Client.status client ~mailbox
      ~items:[Imap.Command.Objectid]) in
    match status.objectid with
    | Some {account_id=Some account_id;mailbox_id=Some mailbox_id;_}
      when objectid=Some (account_id,mailbox_id) -> ()
    | _ -> Alcotest.fail "OBJECTID+ STATUS omitted account/mailbox context");
  if objectid_plus then (
    let source=mailbox ^ "-rename-source" in
    let target=mailbox ^ "-rename-target" in
    ignore (unwrap (Client.create_mailbox_objectid client source));
    let renamed=unwrap (Client.rename_mailbox_objectid client
      ~old_name:source ~new_name:target) in
    let status=unwrap (Client.status client ~mailbox:target
      ~items:[Imap.Command.Objectid]) in
    (match status.objectid with
     | Some ids when ids.account_id=renamed.account_id &&
         ids.mailbox_id=renamed.mailbox_id -> ()
     | _ -> Alcotest.fail "OBJECTID+ RENAME receipt differs from STATUS");
    unwrap (Client.delete_mailbox client target));
  let body = raw (nonce ()) "protocol" in
  let receipt = unwrap (Client.append_flow_receipt client ~mailbox
    ~length:(Int64.of_int (String.length body))
    (Eio.Flow.string_source body)) in
  let receipt = match receipt with Some r -> r | None ->
    Alcotest.fail "advertised UIDPLUS did not return APPENDUID" in
  let receipt_uid = Imap.Proto.Uid.to_int64 receipt.uid in
  let validity, checkpoint = unwrap (Client.with_mailbox client ?objectid
    ~mode:`Read_write mailbox (fun selected ->
      let* info = Selected.info selected in
      if objectid_plus then (
        match info.objectid with
        | Some {account_id=Some _;mailbox_id=Some _;_} -> ()
        | _ -> Alcotest.fail "OBJECTID+ omitted account/mailbox context");
      Alcotest.(check int64) "EXISTS" 1L info.exists;
      Alcotest.(check bool) "APPENDUID UIDVALIDITY" true
        (info.uidvalidity = Imap.Proto.Uidvalidity.to_int64 receipt.uidvalidity);
      let* uids = Selected.uid_search selected "ALL" in
      Alcotest.(check (list int64)) "APPENDUID UID"
        [receipt_uid] uids;
      let* ()=if objectid_plus then
        let* objects=Selected.uid_fetch_object_ids_plus selected
          ~uids:[receipt_uid] () in
        (match objects with
         | [{uid;ids={email_id=Some _;thread_id=Some _;_}}]
             when uid=receipt_uid -> Ok ()
         | _ -> Alcotest.fail "OBJECTID+ omitted message identifiers")
        else Ok () in
      let output = Buffer.create (String.length body) in
      let* () = Selected.fetch_to selected ~uid:receipt_uid
        (Eio.Flow.buffer_sink output) in
      Alcotest.(check string) "exact RFC822 bytes" body
        (Buffer.contents output);
      let* rows = Selected.fetch_metadata_range selected
        ~first:receipt_uid ~last:receipt_uid ~modseq:true in
      let modseq = match rows with
        | [row] when row.uid = Some receipt_uid ->
            (match row.modseq with Some n -> n | None ->
              Alcotest.fail "CONDSTORE omitted MODSEQ")
        | _ -> Alcotest.fail "missing CONDSTORE metadata" in
      let set = Imap.Proto.Uid_set.singleton receipt.uid in
      let seen = Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen in
      let* conflict = Selected.uid_store_flags selected ~set
        ~operation:`Add ~flags:[seen] ~unchangedsince:0L () in
      Alcotest.(check bool) "CONDSTORE conflict" true
        (Imap.Proto.Uid_set.mem receipt.uid conflict.modified);
      let* accepted = Selected.uid_store_flags selected ~set
        ~operation:`Add ~flags:[seen] ~unchangedsince:modseq () in
      Alcotest.(check string) "conditional STORE accepted" ""
        (Imap.Proto.Uid_set.to_wire accepted.modified);
      Ok (info.uidvalidity, modseq))) in
  unwrap (Client.with_mailbox client ?objectid ~qresync:(validity, checkpoint)
    ~mode:`Read_only mailbox (fun selected ->
      let* info = Selected.info selected in
      Alcotest.(check bool) "QRESYNC selected MODSEQ" true
        (Option.is_some info.highestmodseq);
      Alcotest.(check bool) "QRESYNC advanced checkpoint" true
        (match info.highestmodseq with Some n -> n > checkpoint | None -> false);
      let* uids = Selected.uid_search selected "ALL" in
      Alcotest.(check (list int64)) "QRESYNC preserved UID"
        [receipt_uid] uids;
      Ok ()))

let test_bridge () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let client = connect env_io sw in
  let n = nonce () in
  let mailbox = "Oxmono-Stalwart-" ^ n ^ "-Bridge" in
  let dbfile = Filename.temp_file "oxmono-stalwart-bridge-" ".sqlite" in
  let blobdir = dbfile ^ "-blobs" and spooldir = dbfile ^ "-spool" in
  let maildir_path = dbfile ^ "-maildir" in
  let fs = Eio.Stdenv.fs env_io in
  Unix.mkdir blobdir 0o700;
  Unix.mkdir spooldir 0o700;
  Fun.protect ~finally:(fun () ->
    ignore (Client.delete_mailbox client mailbox); Client.close client;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile; dbfile ^ "-wal"; dbfile ^ "-shm"];
    List.iter (fun path -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / path))
      [blobdir; spooldir; maildir_path]) @@ fun () ->
  unwrap (Client.create_mailbox client mailbox);
  let remote = raw n "remote" in
  ignore (unwrap (Client.append_flow_receipt client ~mailbox
    ~length:(Int64.of_int (String.length remote))
    (Eio.Flow.string_source remote)));
  let mode = Client.mailbox_mode client in
  let raw_name = match Imap.Mailbox_name.encode ~mode mailbox with
    | Ok raw_name -> raw_name | Error e -> Alcotest.fail e in
  let scope : Imap.Mirror.scope = {
    endpoint = env "IMAP_STALWART_HOST" ^ ":" ^ env "IMAP_STALWART_PORT";
    account = env "IMAP_STALWART_USER"; mailbox_key = mailbox;
    raw_name; encoding = mode; mailbox_id = None } in
  Eio.Switch.run @@ fun store_sw ->
  let store = Imap_store.open_path ~sw:store_sw
    ~blob_dir:Eio.Path.(fs / blobdir) Eio.Path.(fs / dbfile) in
  let maildir = Imap_maildir.open_dir Eio.Path.(fs / maildir_path) in
  let counter = ref 0 in
  let next_id () = incr counter; Printf.sprintf "stalwart-%s-%d" n !counter in
  let copy stage_id = match Imap_sync.Bridge.copy_once ~client ~store ~maildir
    ~scope ~mailbox ~stage_id ~next_id ~spool_dir:Eio.Path.(fs / spooldir)
    () with
    | Ok receipt -> receipt
    | Error e -> Alcotest.fail (Format.asprintf "%a" Imap_sync.Bridge.pp_error e) in
  let imported = copy ("stalwart-import-" ^ n) in
  Alcotest.(check int) "remote imported" 1 imported.remote_to_local;
  let imported_local = match Imap_maildir.scan maildir with
    | [x] -> x | _ -> Alcotest.fail "expected one imported local message" in
  let imported_bytes = Buffer.create (String.length remote) in
  Eio.Switch.run @@ fun read_sw ->
  Eio.Flow.copy (Imap_maildir.open_message maildir ~sw:read_sw imported_local)
    (Eio.Flow.buffer_sink imported_bytes);
  Alcotest.(check string) "import exact bytes" remote
    (Buffer.contents imported_bytes);
  let local_body = raw n "local" in
  let local = Imap_maildir.append maildir
    ~source:(Eio.Flow.string_source local_body)
    ~length:(Int64.of_int (String.length local_body)) ~flags:[] () in
  let uploaded = copy ("stalwart-upload-" ^ n) in
  Alcotest.(check int) "local uploaded" 1 uploaded.local_to_remote;
  let pair = match Imap_store.Journal.find_local store ~scope ~local_id:local.id with
    | Some pair -> pair | None -> Alcotest.fail "upload pair missing" in
  let uid = match pair.remote_uid with Some uid -> Imap.Proto.Uid.to_int64 uid | None ->
    Alcotest.fail "upload UIDPLUS receipt missing" in
  unwrap (Client.with_mailbox client ~mode:`Read_only mailbox
    (fun selected ->
      let output = Buffer.create (String.length local_body) in
      let* () = Selected.fetch_to selected ~uid
        (Eio.Flow.buffer_sink output) in
      Alcotest.(check string) "upload exact bytes" local_body
        (Buffer.contents output);
      Ok ()));
  Alcotest.(check int) "durable pairs" 2
    (List.length (Imap_store.Journal.pairs store ~scope))

let test_objectid_binding () =
  configured ();
  Eio_main.run @@ fun env_io ->
  Eio.Switch.run @@ fun sw ->
  let client = connect env_io sw in
  if not (advertised client Imap.Capability.Objectid_plus) then (
    Client.close client;
    Alcotest.skip ());
  let mutator = connect env_io sw in
  let n = nonce () in
  let mailbox = "Oxmono-Stalwart-" ^ n ^ "-Identity" in
  let renamed = mailbox ^ "-Moved" in
  let dbfile = Filename.temp_file "oxmono-stalwart-objectid-" ".sqlite" in
  let fs = Eio.Stdenv.fs env_io in
  Fun.protect ~finally:(fun () ->
    ignore (Client.delete_mailbox mutator mailbox);
    ignore (Client.delete_mailbox mutator renamed);
    Client.close client;
    Client.close mutator;
    List.iter (fun path -> try Unix.unlink path with _ -> ())
      [dbfile; dbfile ^ "-wal"; dbfile ^ "-shm"]) @@ fun () ->
  unwrap (Client.create_mailbox mutator mailbox);
  let body = raw n "identity" in
  ignore (unwrap (Client.append_flow_receipt mutator ~mailbox
    ~length:(Int64.of_int (String.length body))
    (Eio.Flow.string_source body)));
  let mode = Client.mailbox_mode client in
  let raw_name = match Imap.Mailbox_name.encode ~mode mailbox with
    | Ok name -> name | Error message -> Alcotest.fail message in
  let scope : Imap.Mirror.scope = {
    endpoint = env "IMAP_STALWART_HOST" ^ ":" ^ env "IMAP_STALWART_PORT";
    account = env "IMAP_STALWART_USER"; mailbox_key = mailbox;
    raw_name; encoding = mode; mailbox_id = None } in
  Eio.Switch.run @@ fun store_sw ->
  let store = Imap_store.open_path ~sw:store_sw Eio.Path.(fs / dbfile) in
  let scan stage_id = Imap_sync.Engine.run_once_staged ~client ~store ~scope
    ~mailbox ~stage_id () in
  let first = match scan ("identity-first-" ^ n) with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "initial identity scan: %a"
        Imap_sync.Engine.pp_error error in
  Alcotest.(check bool) "OBJECTID+ bound in SQLite" true
    (match Imap_store.object_identity store ~scope with
     | `Bound _ -> true | `Unbound | `Conflict -> false);
  unwrap (Client.enable_objectid_plus mutator);
  ignore (unwrap (Client.rename_mailbox_objectid mutator
    ~old_name:mailbox ~new_name:renamed));
  unwrap (Client.create_mailbox mutator mailbox);
  (match scan ("identity-replaced-" ^ n) with
   | Error (Imap_sync.Engine.Invalid_scope _) -> ()
   | Error error -> Alcotest.failf "wrong replacement error: %a"
       Imap_sync.Engine.pp_error error
   | Ok _ -> Alcotest.fail "replacement mailbox was scanned");
  let current = Imap_store.load_cursor store ~scope in
  Alcotest.(check int64) "replacement did not publish"
    first.cursor.revision current.revision;
  unwrap (Client.with_mailbox mutator ~mode:`Read_only mailbox
    (fun selected ->
      let* uids = Selected.uid_search selected "ALL" in
      Alcotest.(check (list int64)) "replacement remains empty" [] uids;
      Ok ()));
  unwrap (Client.with_mailbox mutator ~mode:`Read_only renamed
    (fun selected ->
      let* uids = Selected.uid_search selected "ALL" in
      Alcotest.(check int) "renamed original retains message" 1
        (List.length uids);
      Ok ()))

let () = Alcotest.run "Stalwart IMAP" ["live", [
  Alcotest.test_case "UIDPLUS CONDSTORE QRESYNC exact bytes" `Quick test_protocol;
  Alcotest.test_case "durable bridge import upload" `Quick test_bridge;
  Alcotest.test_case "OBJECTID+ durable replacement guard" `Quick
    test_objectid_binding;
]]
