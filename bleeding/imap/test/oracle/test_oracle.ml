module Client = Imap_eio.Client
module Selected = Imap_eio.Selected
module Transport = Imap_eio.Transport

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

let getenv name default =
  match Sys.getenv_opt name with Some s when s <> "" -> s | _ -> default

let configured () =
  match Sys.getenv_opt "IMAP_ORACLE_HOST" with
  | Some s when s <> "" -> true
  | _ -> false

let endpoint env =
  let host = getenv "IMAP_ORACLE_HOST" "127.0.0.1" in
  let port =
    match int_of_string_opt (getenv "IMAP_ORACLE_PORT" "18143") with
    | Some n when n > 0 && n <= 65535 -> n
    | _ -> Alcotest.fail "IMAP_ORACLE_PORT must be in 1..65535"
  in
  let tls =
    match getenv "IMAP_ORACLE_TLS" "plain-test" with
    | "plain-test" -> `Plain
    | "starttls" -> `Required_starttls
    | "implicit" -> `Implicit
    | s -> Alcotest.failf "unknown IMAP_ORACLE_TLS %S" s
  in
  Transport.v ~net:(Eio.Stdenv.net env) ~host ~port ~tls ()

let unwrap = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Client.error_to_string e)

let bind r f = match r with Ok x -> f x | Error _ as e -> e
let ( let* ) = bind

let contains s sub =
  let rec scan i =
    i + String.length sub <= String.length s &&
    (String.sub s i (String.length sub) = sub || scan (i + 1))
  in
  scan 0

let read_one selected message nonce =
  let subject =
    match message.Corpus.name with
    | "plain" -> "Plain IMAP oracle"
    | "literal" -> "Literal IMAP oracle"
    | "mime" -> "MIME IMAP oracle"
    | _ -> assert false
  in
  let criteria = Imap.Search.Header ("Subject", subject ^ " " ^ nonce) in
  let* uids = Selected.uid_search selected ~criteria in
  match uids with
  | [ uid ] ->
      let body = Buffer.create (String.length message.raw) in
      let* () = Selected.fetch_to selected ~uid (Eio.Flow.buffer_sink body) in
      let* rows = Selected.fetch selected ~uids:[ uid ] ~items:[] in
      let metadata =
        List.map
          (fun (row : Selected.row) ->
            String.concat " "
              (List.map Mail_flag.Imap_flag.to_wire
                 (Option.value ~default:[] row.flags)))
          rows
      in
      Ok (message.name, message.raw, Buffer.contents body, metadata)
  | _ ->
      Alcotest.failf "expected one UID for %s, got %d" message.name
        (List.length uids)

let round_trip () =
  if not (configured ()) then
    if getenv "IMAP_ORACLE_REQUIRED" "0" = "1" then
      Alcotest.fail "IMAP_ORACLE_REQUIRED=1 but IMAP_ORACLE_HOST is unset"
    else Alcotest.skip ()
  else
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let auth =
      Imap_eio.Auth.password
        ~username:(getenv "IMAP_ORACLE_USER" "user1")
        ~password:(getenv "IMAP_ORACLE_PASSWORD" "x")
        ~allow_insecure_transport:true ()
    in
    let client = unwrap (Client.connect ~sw ~auth (endpoint env)) in
    let nonce =
      Printf.sprintf "oxmono-%d-%d" (Unix.getpid ())
        (int_of_float (Unix.gettimeofday ()))
    in
    let mailbox = "Oxmono oracle 📬 " ^ nonce in
    unwrap (Client.create_mailbox client ~mailbox);
    Fun.protect ~finally:(fun () ->
      unwrap (Client.delete_mailbox client ~mailbox))
    @@ fun () ->
    let mailboxes = unwrap (Client.list client ~pattern:mailbox ()) in
    Alcotest.(check bool) "LIST contains test mailbox" true
      (List.exists
         (fun (entry : Client.mailbox_entry) -> entry.name.utf8 = Ok mailbox)
         mailboxes);
    let advertised capability =
      Imap.Capability.Set.mem capability (Client.capabilities client) in
    if advertised Imap.Capability.Namespace ||
       advertised Imap.Capability.Imap4rev2 then (
      let namespaces = unwrap (Client.namespace client) in
      Alcotest.(check bool) "NAMESPACE personal response" true
        (Option.is_some namespaces.personal));
    let messages = Corpus.messages ~nonce in
    List.iter
      (fun (message : Corpus.message) ->
        unwrap
          (Result.map ignore (Client.append client ~mailbox
            (Client.append_message
               ~length:(Int64.of_int (String.length message.raw))
               (Eio.Flow.string_source message.raw)))))
      messages;
    if advertised Imap.Capability.List_extended ||
       advertised Imap.Capability.Imap4rev2 then (
      let discovery = unwrap (Client.list_extended client
        ~patterns:[mailbox]
        ~returns:[Imap.Mailbox_list.Children]
        ?status:(if advertised Imap.Capability.List_status then
          Some [Imap.Status_item.Messages; Imap.Status_item.Uidnext;
                Imap.Status_item.Uidvalidity] else None) ()) in
      let discovered = List.filter (fun
          ((entry : Client.mailbox_entry), _) ->
        entry.name.utf8 = Ok mailbox) discovery.mailboxes in
      Alcotest.(check int) "LIST-EXTENDED found test mailbox" 1
        (List.length discovered);
      if advertised Imap.Capability.List_status then
        Alcotest.(check bool) "LIST-STATUS paired mailbox" true
          (match discovered with [_, Some status] ->
            status.messages=Some 3L | _ -> false));
    let fetched =
      unwrap
        (Client.with_mailbox client ~mode:`Read_only mailbox (fun selected ->
             let rec gather acc = function
               | [] -> Ok (List.rev acc)
               | message :: rest ->
                   let* row = read_one selected message nonce in
                   gather (row :: acc) rest
             in
             gather [] messages))
    in
    List.iter
      (fun (name, expected, actual, metadata) ->
        Alcotest.(check string) (name ^ " BODY.PEEK[] bytes") expected actual;
        Alcotest.(check bool) (name ^ " has UID metadata") true
          (metadata <> []);
        Alcotest.(check bool) (name ^ " remains unread") false
          (List.exists
             (fun s -> contains (String.uppercase_ascii s) "\\SEEN")
             metadata))
      fetched;
    let dbfile = Filename.temp_file "oxmono-imap-oracle-" ".sqlite" in
    let blobdir = dbfile ^ "-blobs" in
    let maildir_path = dbfile ^ "-maildir" in
    let spooldir = dbfile ^ "-spool" in
    Unix.mkdir blobdir 0o700;
    Unix.mkdir spooldir 0o700;
    Fun.protect ~finally:(fun () ->
      List.iter (fun path -> try Unix.unlink path with Unix.Unix_error _ -> ())
        [dbfile; dbfile ^ "-wal"; dbfile ^ "-shm"];
      Eio.Path.rmtree ~missing_ok:true
        Eio.Path.(Eio.Stdenv.fs env / blobdir);
      Eio.Path.rmtree ~missing_ok:true
        Eio.Path.(Eio.Stdenv.fs env / maildir_path);
      Eio.Path.rmtree ~missing_ok:true
        Eio.Path.(Eio.Stdenv.fs env / spooldir))
    @@ fun () ->
    let encoding = Client.mailbox_mode client in
    let raw_name = match Imap.Mailbox_name.encode ~mode:encoding mailbox with
      | Ok s -> s | Error e -> Alcotest.fail e in
    let scope : Imap.Mirror.scope = {
      endpoint=Printf.sprintf "%s:%s"
        (getenv "IMAP_ORACLE_HOST" "127.0.0.1")
        (getenv "IMAP_ORACLE_PORT" "18143");
      account=getenv "IMAP_ORACLE_USER" "user1";
      mailbox_key="oracle-" ^ nonce;
      raw_name; encoding; mailbox_id=None
    } in
    let dbpath = Eio.Path.(Eio.Stdenv.fs env / dbfile) in
    let blobpath = Eio.Path.(Eio.Stdenv.fs env / blobdir) in
    Eio.Switch.run @@ fun store_sw ->
    let store = Imap_store.open_path ~sw:store_sw ~blob_dir:blobpath dbpath in
    let scan stage_id =
      match Imap_sync.Engine.scan_once ~client ~store ~scope ~mailbox
        ~stage_id () with
      | Ok receipt -> receipt
      | Error error -> Alcotest.fail (Format.asprintf "%a"
          Imap_sync.Engine.pp_error error) in
    let published_rows () =
      let cursor = Imap_store.load_cursor store ~scope in
      match Imap_store.snapshot_page store ~scope ~cursor ~limit:10_000 () with
      | `Rows rows -> rows
      | `Stale_revision -> Alcotest.fail "published snapshot changed" in
    let first = scan "initial" in
    let first_rows = published_rows () in
    Alcotest.(check int64) "initial mirror rows" 3L first.row_count;
    let advertised capability =
      Imap.Capability.Set.mem capability (Client.capabilities client) in
    if advertised Imap.Capability.Condstore then
      Alcotest.(check bool) "opening MODSEQ checkpoint" true
        (Option.is_some first.cursor.anchor);
    if advertised Imap.Capability.Qresync then
      Alcotest.(check bool) "QRESYNC enabled for next scan" true
        (Client.is_enabled client Imap.Capability.Qresync);
    let first_uid = (List.hd first_rows).uid in
    let spool = Eio.Path.(Eio.Stdenv.fs env / (dbfile ^ ".spool")) in
    let archived = match Imap_sync.Engine.archive_uid ~client ~store ~scope
      ~mailbox ~uid:first_uid ~spool () with
      | Ok blob -> blob
      | Error error -> Alcotest.fail (Format.asprintf "%a"
          Imap_sync.Engine.pp_error error) in
    let archived_bytes = Buffer.create 256 in
    Eio.Flow.copy
      (Imap_store.Blob.open_in store ~sw:store_sw archived)
      (Eio.Flow.buffer_sink archived_bytes);
    Alcotest.(check string) "archived server octets"
      (List.hd messages).raw (Buffer.contents archived_bytes);
    let flag = Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen in
    unwrap (Client.with_mailbox client ~mode:`Read_write mailbox
      (fun selected ->
        let* uids = Selected.uid_search selected ~criteria:Imap.Search.All in
        match uids with
        | uid :: _ ->
            let set = Imap.Uid_set.singleton uid in
            let* _ = Selected.uid_store_flags selected ~set
              ~operation:`Add ~flags:[flag] in
            Ok ()
        | [] -> Alcotest.fail "no UID to flag"));
    let second = scan "after-flags" in
    let second_rows = published_rows () in
    let changed = List.filter (fun (row : Imap.Mirror.row) ->
      match List.find_opt (fun (before : Imap.Mirror.row) ->
          Imap.Uid.equal before.uid row.uid) first_rows with
      | Some before ->
          not (Mail_flag.Imap_flag.equal_durable before.flags row.flags)
      | None -> false) second_rows in
    Alcotest.(check int) "flag delta" 1 (List.length changed);
    let persisted = Imap_store.load_cursor store ~scope in
    Alcotest.(check int64) "durable revision" 2L persisted.revision;
    let duplicate = List.hd messages in
    let blob = Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source duplicate.raw)
      ~length:(Int64.of_int (String.length duplicate.raw)) () in
    let probe_id = "probe-" ^ nonce in
    let probe : Imap_store.intent = {
      id=probe_id; scope; state=Imap_store.Prepared;
      kind=Imap_store.Append {
        message_id="probe"; content_digest=blob.sha256;
        spool_ref=blob.sha256;
        pre_send_uid_frontier=Some second.cursor.frontier;
        expected_length=Some blob.length; expected_flags=Some [];
        expected_internal_date=None};
      uidvalidity=second.cursor.uidvalidity; uid=None} in
    Imap_store.prepare_intent store probe;
    Imap_store.set_intent_state store ~id:probe_id Imap_store.Sent;
    Imap_store.Journal.prepare_operation store {
      id=probe_id; pair_id=None; local_id=None; scope;
      kind=Imap_store.Journal.Append; state=Imap_store.Journal.Prepared;
      source_uidvalidity=None; source_uid=None; destination=Some scope;
      destination_uidvalidity=second.cursor.uidvalidity;
      blob_sha256=Some blob.sha256; blob_length=Some blob.length;
      desired_flags=Some []; receipt=None; receipt_uidvalidity=None;
      receipt_uid=None};
    Imap_store.Journal.mark_sent store ~id:probe_id;
    let outcome = match Imap_sync.Engine.append_blob_journaled
      ~client ~store ~scope ~mailbox ~id:("append-" ^ nonce)
      ~message_id:("duplicate-" ^ nonce) blob with
      | Ok x -> x
      | Error error -> Alcotest.fail (Format.asprintf "%a"
          Imap_sync.Engine.pp_error error) in
    let receipt = match outcome with
      | Imap_sync.Engine.Identified x -> x
      | Imap_sync.Engine.Needs_reconciliation ->
          Alcotest.fail "Cyrus omitted APPENDUID" in
    let intent = match Imap_store.find_intent store ~id:("append-" ^ nonce) with
      | Some intent -> intent
      | None -> Alcotest.fail "APPEND intent missing after confirmation" in
    (match intent.kind with
     | Imap_store.Append metadata ->
         Alcotest.(check (option int64)) "journaled pre-send frontier"
           (Some second.cursor.frontier) metadata.pre_send_uid_frontier;
         Alcotest.(check (option int64)) "journaled byte length"
           (Some blob.length) metadata.expected_length;
         Alcotest.(check bool) "known empty APPEND flags" true
           (metadata.expected_flags = Some [])
     | Imap_store.Other _ -> Alcotest.fail "wrong APPEND intent kind");
    let evidence = match Imap_sync.Bridge.inspect_append_candidates
      ~client ~store ~scope ~mailbox ~id:probe_id
      ~spool_dir:Eio.Path.(Eio.Stdenv.fs env / spooldir) () with
      | Ok report -> report
      | Error error -> Alcotest.fail (Format.asprintf "%a"
          Imap_sync.Bridge.pp_error error) in
    (* The inspection admits only candidates whose flags equal the journaled
       flags, so one match also proves the candidate's wire flags. *)
    Alcotest.(check (list int64)) "uncertain APPEND candidate"
      [Imap.Uid.to_int64 receipt.uid]
      (List.map Imap.Uid.to_int64 evidence.matching_uids);
    Alcotest.(check bool) "inspection leaves intent pending" true
      (List.exists (fun (x : Imap_store.intent) -> x.id=probe_id)
        (Imap_store.pending_intents store ~scope));
    Alcotest.(check bool) "inspection leaves operation pending" true
      (match Imap_store.Journal.find_operation store ~id:probe_id with
       | Some {state=Imap_store.Journal.Sent;_} -> true
       | _ -> false);
    Imap_store.set_intent_state store ~id:probe_id Imap_store.Rejected;
    Imap_store.Journal.reject_operation store ~id:probe_id
      ~receipt:"oracle probe inspected";
    let before_append = published_rows () in
    let third = scan "after-journaled-append" in
    let added = List.filter (fun (row : Imap.Mirror.row) ->
      not (List.exists (fun (before : Imap.Mirror.row) ->
        Imap.Uid.equal before.uid row.uid) before_append))
      (published_rows ()) in
    Alcotest.(check int) "journaled APPEND added one UID" 1
      (List.length added);
    Imap_store.Blob.attach store ~scope ~uidvalidity:receipt.uidvalidity
      ~uid:receipt.uid blob;
    Alcotest.(check bool) "archived body hash and length" true
      (Imap_store.Blob.verify store blob);
    let staged = match Imap_sync.Engine.scan_once ~client ~store ~scope
      ~mailbox ~stage_id:("disk-stage-" ^ nonce) () with
      | Ok receipt -> receipt
      | Error error -> Alcotest.fail (Format.asprintf "%a"
          Imap_sync.Engine.pp_error error) in
    Alcotest.(check int64) "disk-staged mirror rows" 4L staged.row_count;
    Alcotest.(check int64) "disk-staged revision"
      (Int64.succ third.cursor.revision) staged.cursor.revision;
    Alcotest.(check bool) "staged publication kept blob reference" true
      (Option.is_some (Imap_store.Blob.find store ~scope
        ~uidvalidity:receipt.uidvalidity ~uid:receipt.uid));
    Alcotest.(check (list string)) "no abandoned stage" []
      (Imap_store.abandoned_stages store);
    Alcotest.(check bool) "no pending confirmed APPEND" true
      (Imap_store.pending_intents store ~scope = []);
    let maildir = Md.open_dir
      Eio.Path.(Eio.Stdenv.fs env / maildir_path) in
    let spool_dir = Eio.Path.(Eio.Stdenv.fs env / spooldir) in
    let sequence = ref 0 in
    let next_id () = incr sequence;
      Printf.sprintf "bridge-%s-%d" nonce !sequence in
    let bootstrap = Md.append maildir
      ~source:(Eio.Flow.string_source "bootstrap\r\n")
      ~length:11L ~flags:[] () in
    (match Imap_sync.Bridge.copy_once ~client ~store ~maildir ~scope ~mailbox
      ~stage_id:("bridge-bootstrap-" ^ nonce) ~next_id ~spool_dir () with
     | Error Imap_sync.Bridge.Bootstrap_requires_pairing -> ()
     | Error error -> Alcotest.fail (Format.asprintf
         "unexpected bootstrap error: %a" Imap_sync.Bridge.pp_error error)
     | Ok _ -> Alcotest.fail "unpaired populated endpoints must be held");
    Md.remove maildir bootstrap;
    let copy stage_id = match Imap_sync.Bridge.copy_once ~client ~store ~maildir
      ~scope ~mailbox ~stage_id ~next_id ~spool_dir () with
      | Ok receipt -> receipt
      | Error error -> Alcotest.fail
          (Format.asprintf "%a" Imap_sync.Bridge.pp_error error) in
    let imported = copy ("bridge-import-" ^ nonce) in
    Alcotest.(check int) "remote occurrences imported" 4
      imported.remote_to_local;
    Alcotest.(check int) "Maildir occurrences" 4
      (List.length (Md.scan maildir));
    Alcotest.(check int) "durable occurrence pairs" 4
      (List.length (Imap_store.Journal.pairs store ~scope));
    let local_bytes = "From: local@example.test\r\nSubject: Local bridge " ^ nonce ^
      "\r\n\r\nUnique local payload\r\n" in
    let local = Md.append maildir
      ~source:(Eio.Flow.string_source local_bytes)
      ~length:(Int64.of_int (String.length local_bytes)) ~flags:[] () in
    let uploaded = copy ("bridge-upload-" ^ nonce) in
    Alcotest.(check int) "local occurrence uploaded" 1
      uploaded.local_to_remote;
    Alcotest.(check int) "durable pair after upload" 5
      (List.length (Imap_store.Journal.pairs store ~scope));
    Alcotest.(check bool) "local upload paired" true
      (Option.is_some (Imap_store.Journal.find_local store ~scope
        ~local_id:local.id));
    Alcotest.(check bool) "no pending bridge operations" true
      (Imap_store.Journal.active_operations store ~scope = []);
    let stable = copy ("bridge-stable-" ^ nonce) in
    Alcotest.(check int) "stable remote copies" 0 stable.remote_to_local;
    Alcotest.(check int) "stable local copies" 0 stable.local_to_remote;
    let flag_local = List.find (fun (item:Md.occurrence) ->
      item.id<>local.id) (Md.scan maildir) in
    let flag_pair = match Imap_store.Journal.find_local store ~scope
      ~local_id:flag_local.id with
      | Some pair -> pair
      | None -> Alcotest.fail "flag test occurrence lacks pair" in
    let flag_uid = match flag_pair.remote_uid with
      | Some uid -> uid | None -> Alcotest.fail "flag pair lacks UID" in
    let flagged = Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Flagged in
    let local_keyword = match Mail_flag.Imap_flag.of_wire "$local" with
      | Ok flag -> flag | Error e -> Alcotest.fail e in
    unwrap (Client.with_mailbox client ~mode:`Read_write mailbox
      (fun selected ->
        let set=Imap.Uid_set.singleton flag_uid in
        let* _ = Selected.uid_store_flags selected ~set ~operation:`Add
          ~flags:[flagged] in Ok ()));
    ignore (Md.set_flags maildir flag_local
      (local_keyword::flag_local.flags));
    let after_flags = copy ("bridge-three-way-flags-" ^ nonce) in
    Alcotest.(check int) "three-way flags updated" 1
      after_flags.flags_updated;
    let flag_pair =
      match Imap_store.Journal.find_pair store ~id:flag_pair.id with
      | Some pair -> pair | None -> Alcotest.fail "flag pair vanished" in
    Alcotest.(check bool) "remote and local additions merged" true
      (List.mem flagged flag_pair.common_flags &&
       List.mem local_keyword flag_pair.common_flags);
    let flag_local = match Md.find maildir ~id:flag_local.id with
      | Some local -> local | None -> Alcotest.fail "flag local vanished" in
    Alcotest.(check bool) "Maildir has merged flags" true
      (List.mem flagged flag_local.flags &&
       List.mem local_keyword flag_local.flags);
    let remote_flag_wires = unwrap (Client.with_mailbox client
      ~mode:`Read_only mailbox (fun selected ->
        let* rows=Selected.fetch selected ~uids:[flag_uid] ~items:[] in
        match rows with
        | [row] -> Ok (List.map Mail_flag.Imap_flag.to_wire
            (Option.value ~default:[] row.flags))
        | _ -> Alcotest.fail "flag UID disappeared")) in
    Alcotest.(check bool) "server has merged flags" true
      (List.mem "\\Flagged" remote_flag_wires &&
       List.mem "$local" remote_flag_wires);
    let deleted = Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted in
    unwrap (Client.with_mailbox client ~mode:`Read_write mailbox
      (fun selected ->
        let set=Imap.Uid_set.singleton flag_uid in
        let* _ = Selected.uid_store_flags selected ~set ~operation:`Add
          ~flags:[deleted] in Ok ()));
    let held_deleted = copy ("bridge-deleted-held-" ^ nonce) in
    Alcotest.(check int) "Deleted requires policy" 0
      held_deleted.flags_updated;
    Alcotest.(check bool) "Deleted flag hold is visible" true
      (held_deleted.flags_held>0 &&
       List.mem flag_pair.id held_deleted.held_pair_ids);
    let held_pair =
      match Imap_store.Journal.find_pair store ~id:flag_pair.id with
      | Some pair -> pair | None -> Alcotest.fail "held pair vanished" in
    Alcotest.(check bool) "Deleted absent from common flags" false
      (List.mem deleted held_pair.common_flags);
    let held_local = match Md.find maildir ~id:flag_local.id with
      | Some local -> local | None -> Alcotest.fail "held local vanished" in
    Alcotest.(check bool) "Deleted absent from Maildir" false
      (List.mem deleted held_local.flags);
    let recovered_bytes = "From: recovery@example.test\r\nSubject: Recovery " ^
      nonce ^ "\r\n\r\nDurable local receipt\r\n" in
    let recovered_length = Int64.of_int (String.length recovered_bytes) in
    let remote_receipt = match unwrap (Client.append client ~mailbox
      (Client.append_message ~length:recovered_length
         (Eio.Flow.string_source recovered_bytes))) with
      | Some receipt -> receipt
      | None -> Alcotest.fail "Cyrus omitted recovery APPENDUID" in
    let recovered_blob = Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source recovered_bytes)
      ~length:recovered_length () in
    let recovery_id = next_id () in
    let recovery_local_id = Md.reserve_id () in
    let pending : Imap_store.Journal.operation = {
      id=recovery_id; pair_id=None;local_id=Some recovery_local_id;
      scope; kind=Imap_store.Journal.Local_append;
      state=Imap_store.Journal.Prepared;
      source_uidvalidity=Some remote_receipt.uidvalidity;
      source_uid=Some remote_receipt.uid;
      destination=None;destination_uidvalidity=None;
      blob_sha256=Some recovered_blob.sha256;
      blob_length=Some recovered_length;desired_flags=Some [];
      receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
    Imap_store.Journal.prepare_operation store pending;
    Imap_store.Journal.mark_sent store ~id:recovery_id;
    ignore (Md.append maildir ~id:recovery_local_id
      ~source:(Eio.Flow.string_source recovered_bytes)
      ~length:recovered_length ~flags:[] ());
    let recovered = copy ("bridge-recover-" ^ nonce) in
    Alcotest.(check int) "recovered write not duplicated" 0
      recovered.remote_to_local;
    Alcotest.(check int) "recovered pair count" 6
      (List.length (Imap_store.Journal.pairs store ~scope));
    Alcotest.(check bool) "recovered operation committed" true
      (match Imap_store.Journal.find_operation store ~id:recovery_id with
       | Some {state=Imap_store.Journal.Committed;_} -> true
       | _ -> false);
    let append_bytes = "From: append-recovery@example.test\r\nSubject: " ^
      "Append recovery " ^ nonce ^ "\r\n\r\nReceipt survived\r\n" in
    let append_length = Int64.of_int (String.length append_bytes) in
    let append_local = Md.append maildir
      ~source:(Eio.Flow.string_source append_bytes)
      ~length:append_length ~flags:[] () in
    let append_blob = Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source append_bytes)
      ~length:append_length () in
    let append_id = next_id () in
    let append_pending : Imap_store.Journal.operation = {
      id=append_id;pair_id=None;local_id=Some append_local.id;
      scope;kind=Imap_store.Journal.Append;state=Imap_store.Journal.Prepared;
      source_uidvalidity=None;source_uid=None;
      destination=Some scope;
      destination_uidvalidity=staged.cursor.uidvalidity;
      blob_sha256=Some append_blob.sha256;
      blob_length=Some append_length;desired_flags=Some [];
      receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
    Imap_store.Journal.prepare_operation
      ~local_source_mtime:append_local.mtime store append_pending;
    Imap_store.Journal.mark_sent store ~id:append_id;
    (match Imap_sync.Engine.append_blob_journaled ~client ~store ~scope
      ~mailbox ~id:append_id ~message_id:append_id ~flags:[] append_blob with
     | Ok (Imap_sync.Engine.Identified _) -> ()
     | Ok Imap_sync.Engine.Needs_reconciliation ->
         Alcotest.fail "Cyrus omitted APPENDUID for recovery"
     | Error error -> Alcotest.fail (Format.asprintf "%a"
         Imap_sync.Engine.pp_error error));
    let recovered_append = copy ("bridge-append-recover-" ^ nonce) in
    Alcotest.(check int) "confirmed APPEND not duplicated" 0
      recovered_append.local_to_remote;
    Alcotest.(check int) "APPEND recovery pair count" 7
      (List.length (Imap_store.Journal.pairs store ~scope));
    Alcotest.(check bool) "APPEND recovery committed" true
      (match Imap_store.Journal.find_operation store ~id:append_id with
       | Some {state=Imap_store.Journal.Committed;_} -> true
       | _ -> false);
    let removed_local = match Md.find maildir ~id:local.id with
      | Some occurrence -> occurrence
      | None -> Alcotest.fail "uploaded local occurrence vanished" in
    let removed_pair = match Imap_store.Journal.find_local store ~scope
      ~local_id:removed_local.id with
      | Some pair -> pair
      | None -> Alcotest.fail "local occurrence lacks pair" in
    Md.remove maildir removed_local;
    let after_local_absence = copy ("bridge-local-absence-" ^ nonce) in
    Alcotest.(check int) "local disappearance not recopied" 0
      after_local_absence.remote_to_local;
    Alcotest.(check bool) "local absence tombstone committed" true
      (match Imap_store.Journal.find_pair store ~id:removed_pair.id with
       | Some {local_tombstone=Some
           {reason=Imap_store.Journal.Local_absence;_};_} -> true
       | _ -> false);
    let expunged_uid =
      match Imap_store.Journal.find_pair store ~id:append_id with
      | Some {remote_uid=Some uid;_} -> uid
      | _ -> Alcotest.fail "APPEND recovery pair lacks remote UID" in
    unwrap (Client.with_mailbox client ~mode:`Read_write mailbox
      (fun selected ->
        let set = Imap.Uid_set.singleton expunged_uid in
        let* _ = Selected.uid_store_flags selected ~set ~operation:`Add
          ~flags:[Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted]
        in
        let* uidplus = Selected.Uidplus.require selected in
        Selected.Uidplus.uid_expunge uidplus ~set));
    let after_remote_absence = copy ("bridge-remote-absence-" ^ nonce) in
    Alcotest.(check int) "remote disappearance not reuploaded" 0
      after_remote_absence.local_to_remote;
    Alcotest.(check bool) "inventory-proven remote tombstone" true
      (match Imap_store.Journal.find_pair store ~id:append_id with
       | Some {remote_tombstone=Some
           {reason=Imap_store.Journal.Inventory_absence;
            generation=Some _;_};_} -> true
       | _ -> false);
    let changed_survivor=Md.set_flags maildir append_local
      [local_keyword] in
    let propagated = match Imap_sync.Bridge.copy_once
      ~deletion_policy:Imap.Sync_policy.Propagate ~client ~store ~maildir
      ~scope ~mailbox ~stage_id:("bridge-propagate-delete-" ^ nonce)
      ~next_id ~spool_dir () with
      | Ok receipt -> receipt
      | Error error -> Alcotest.fail
          (Format.asprintf "delete propagation: %a"
            Imap_sync.Bridge.pp_error error) in
    Alcotest.(check int) "unchanged remote survivor deleted" 1
      propagated.deletions;
    Alcotest.(check bool) "changed survivor deletion hold is visible" true
      (propagated.deletions_held>0 &&
       List.mem append_id propagated.held_pair_ids);
    Alcotest.(check bool) "remote survivor targeted for deletion" true
      (match Imap_store.Journal.find_pair store ~id:removed_pair.id with
       | Some {remote_tombstone=Some
           {reason=Imap_store.Journal.Expunge_receipt;_};_} -> true
       | _ -> false);
    Alcotest.(check bool) "changed local survivor held" true
      (Option.is_some (Md.find maildir ~id:append_local.id));
    ignore (Md.set_flags maildir changed_survivor []);
    let propagated_local = match Imap_sync.Bridge.copy_once
      ~deletion_policy:Imap.Sync_policy.Propagate ~client ~store ~maildir
      ~scope ~mailbox ~stage_id:("bridge-propagate-local-" ^ nonce)
      ~next_id ~spool_dir () with
      | Ok receipt -> receipt
      | Error error -> Alcotest.fail
          (Format.asprintf "local delete propagation: %a"
            Imap_sync.Bridge.pp_error error) in
    Alcotest.(check int) "restored survivor deleted" 1
      propagated_local.deletions;
    Alcotest.(check bool) "local survivor removed" true
      (match Imap_store.Journal.find_pair store ~id:append_id with
       | Some {local_tombstone=Some
           {reason=Imap_store.Journal.Explicit_delete;_};_} -> true
       | _ -> false);
    Alcotest.(check bool) "other deleted UID not expunged" true
      (unwrap (Client.with_mailbox client ~mode:`Read_only mailbox
        (fun selected ->
          let* rows=Selected.fetch selected ~uids:[flag_uid] ~items:[] in
          Ok (List.exists (fun (row:Selected.row) ->
            Imap.Uid.equal row.uid flag_uid) rows))));
    Alcotest.(check bool) "no pending deletion operations" true
      (Imap_store.Journal.active_operations store ~scope = []);
    let ambiguous_bytes = "From: ambiguous@example.test\r\nSubject: " ^
      "Ambiguous " ^ nonce ^ "\r\n\r\nMust not replay\r\n" in
    let ambiguous_length = Int64.of_int (String.length ambiguous_bytes) in
    let ambiguous_local = Md.append maildir
      ~source:(Eio.Flow.string_source ambiguous_bytes)
      ~length:ambiguous_length ~flags:[] () in
    let ambiguous_blob = Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source ambiguous_bytes)
      ~length:ambiguous_length () in
    let ambiguous_id = next_id () in
    let current = Imap_store.load_cursor store ~scope in
    let ambiguous_op : Imap_store.Journal.operation = {
      id=ambiguous_id;pair_id=None;local_id=Some ambiguous_local.id;
      scope;kind=Imap_store.Journal.Append;state=Imap_store.Journal.Prepared;
      source_uidvalidity=None;source_uid=None;
      destination=Some scope;destination_uidvalidity=current.uidvalidity;
      blob_sha256=Some ambiguous_blob.sha256;
      blob_length=Some ambiguous_length;desired_flags=Some [];
      receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
    Imap_store.Journal.prepare_operation
      ~local_source_mtime:ambiguous_local.mtime store ambiguous_op;
    Imap_store.Journal.mark_sent store ~id:ambiguous_id;
    let legacy : Imap_store.intent = {
      id=ambiguous_id;scope;state=Imap_store.Prepared;
      kind=Imap_store.Append {
        message_id=ambiguous_id;content_digest=ambiguous_blob.sha256;
        spool_ref=ambiguous_blob.sha256;
        pre_send_uid_frontier=Some current.frontier;
        expected_length=Some ambiguous_length;
        expected_flags=Some [];expected_internal_date=None};
      uidvalidity=current.uidvalidity;uid=None} in
    Imap_store.prepare_intent store legacy;
    Imap_store.set_intent_state store ~id:ambiguous_id Imap_store.Sent;
    let ambiguous_receipt=match unwrap (Client.append client ~mailbox
      (Client.append_message ~length:ambiguous_length
         (Eio.Flow.string_source ambiguous_bytes))) with
      | Some receipt -> receipt
      | None -> Alcotest.fail "Cyrus omitted operator APPENDUID" in
    let count_remote () = unwrap (Client.with_mailbox client
      ~mode:`Read_only mailbox (fun selected ->
        let* uids = Selected.uid_search selected ~criteria:Imap.Search.All in
        Ok (List.length uids))) in
    let count_before = count_remote () in
    (match Imap_sync.Bridge.copy_once ~client ~store ~maildir ~scope ~mailbox
      ~stage_id:("bridge-ambiguous-" ^ nonce) ~next_id ~spool_dir () with
     | Error (Imap_sync.Bridge.Pending_operations [id]) when id=ambiguous_id -> ()
     | Error error -> Alcotest.fail (Format.asprintf
         "unexpected ambiguous result: %a" Imap_sync.Bridge.pp_error error)
     | Ok _ -> Alcotest.fail "ambiguous APPEND was not held");
    Alcotest.(check int) "ambiguous APPEND not replayed"
      count_before (count_remote ());
    Alcotest.(check int) "ambiguous APPEND not paired" 7
      (List.length (Imap_store.Journal.pairs store ~scope));
    (match Imap_sync.Bridge.record_appenduid_evidence ~store ~maildir ~scope
      ~id:ambiguous_id ~uidvalidity:ambiguous_receipt.uidvalidity
      ~uid:ambiguous_receipt.uid
      ~evidence:"Cyrus APPENDUID retained by operator" () with
     | Ok () -> ()
     | Error error -> Alcotest.fail (Format.asprintf
         "operator APPENDUID: %a" Imap_sync.Bridge.pp_error error));
    let repaired=copy ("bridge-repaired-append-" ^ nonce) in
    Alcotest.(check int) "operator repair made no duplicate upload" 0
      repaired.local_to_remote;
    Alcotest.(check bool) "operator APPENDUID pair verified" true
      (Option.is_some (Imap_store.Journal.find_local store ~scope
        ~local_id:ambiguous_local.id));
    Alcotest.(check bool) "operator APPENDUID committed" true
      (match Imap_store.Journal.find_operation store ~id:ambiguous_id with
       | Some {state=Imap_store.Journal.Committed;_} -> true
       | _ -> false)

let objectid_round_trip () =
  if not (configured ()) then
    if getenv "IMAP_ORACLE_REQUIRED" "0"="1" then
      Alcotest.fail "IMAP_ORACLE_REQUIRED=1 but IMAP_ORACLE_HOST is unset"
    else Alcotest.skip ()
  else Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let auth=Imap_eio.Auth.password
    ~username:(getenv "IMAP_ORACLE_USER" "user1")
    ~password:(getenv "IMAP_ORACLE_PASSWORD" "x")
    ~allow_insecure_transport:true () in
  let client=unwrap (Client.connect ~sw ~auth (endpoint env)) in
  let mailbox=Printf.sprintf "Oxmono ObjectID %d-%06x"
    (Unix.getpid ()) (Random.bits () land 0xffffff) in
  Fun.protect ~finally:(fun () ->
    ignore (Client.delete_mailbox client ~mailbox);
    Client.close client) @@ fun () ->
  Alcotest.(check bool) "Cyrus advertises OBJECTID" true
    (Imap.Capability.Set.mem Imap.Capability.Objectid
      (Client.capabilities client));
  unwrap (Client.create_mailbox client ~mailbox);
  let raw="From: objectid@example.test\r\nSubject: identity\r\n\r\nExact content\r\n" in
  let receipt=match unwrap (Client.append client ~mailbox
    (Client.append_message ~length:(Int64.of_int (String.length raw))
       (Eio.Flow.string_source raw))) with
    | Some receipt -> receipt | None -> Alcotest.fail "missing APPENDUID" in
  let uid=receipt.uid in
  unwrap (Client.with_mailbox client ~mode:`Read_only mailbox
    (fun selected ->
      let* info=Selected.info selected in
      Alcotest.(check bool) "selected MAILBOXID" true
        (Option.is_some info.mailbox_id);
      let* rows=Selected.fetch selected ~uids:[uid]
        ~items:[Imap.Fetch_item.Emailid; Threadid] in
      (match rows with
       | [{uid=observed;email_id=Some email_id;thread_id=Some _;_}]
         when Imap.Uid.equal observed uid ->
           Alcotest.(check bool) "EMAILID nonempty" true (email_id<>"")
       | _ -> Alcotest.fail "missing typed OBJECTID row");
      Ok ()))

let () =
  Alcotest.run "imap-oracle"
    [ "cyrus", [
      Alcotest.test_case "APPEND LIST SEARCH FETCH" `Quick round_trip;
      Alcotest.test_case "RFC 8474 OBJECTID" `Quick objectid_round_trip;
    ] ]
