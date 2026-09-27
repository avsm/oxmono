module M = Imap.Mirror
module Store = Imap_store

let ok = function Ok x -> x | Error _ -> Alcotest.fail "unexpected error"
let uid n = ok (Imap.Uid.of_int64 n)
let epoch n = ok (Imap.Uidvalidity.of_int64 n)
let modseq n = ok (Imap.Modseq.of_int64 n)
let flag s = ok (Mail_flag.Imap_flag.of_wire s)
let scope : M.scope = {
  endpoint="imap.example"; account="alice"; mailbox_key="inbox";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1; mailbox_id=None
}
let row n flags : M.row = { uid=uid n; flags; modseq=Some (modseq 17L) }
let transition cursor published ~stage ~epoch_value rows =
  let selected : M.selected = {
    uidvalidity=epoch epoch_value; uidnext=5L;
    highestmodseq=Some (modseq 17L); nomodseq=false } in
  let action=ok (M.plan cursor ~stage_id:stage selected) in
  let completed : M.completed = {
    action_id=action.id; uidvalidity=action.uidvalidity;
    covered_upper=action.upper_uid; inventory_complete=true;
    commands_complete=true; rows; explicit_highestmodseq=Some (modseq 17L);
    nomodseq=false } in
  let staged=ok (M.complete cursor action completed) in
  ok (M.publish cursor ~published staged)

let uids snap =
  M.rows snap |> List.map (fun (r:M.row) -> Imap.Uid.to_int64 r.uid)
let test_object_identity env =
  let path=Filename.temp_file "imap-object-id-" ".db" in
  let cleanup ()=List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
    [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup (fun () ->
    let fs=Eio.Stdenv.fs env in
    let db_path=Eio.Path.(fs / path) in
    let identity:Store.object_identity={account_id="u_account";
      mailbox_id="F_box"} in
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw db_path in
      let rival=Store.open_path ~sw db_path in
      Alcotest.(check bool) "unbound" true
        (Store.object_identity db ~scope=`Unbound);
      Alcotest.(check bool) "first binding" true
        (Store.observe_object_identity db ~scope identity=`Bound);
      Alcotest.(check bool) "cross-handle exact match" true
        (Store.observe_object_identity rival ~scope identity=`Matched);
      Alcotest.(check bool) "changed identity held" true
        (Store.observe_object_identity rival ~scope
          {identity with mailbox_id="F_other"}=`Conflict);
      let other_scope={scope with mailbox_key="other";raw_name="Other"} in
      Alcotest.(check bool) "identity cannot bind two scopes" true
        (Store.observe_object_identity rival ~scope:other_scope identity=
          `Conflict));
    Eio.Switch.run (fun sw ->
      let db=Store.open_readonly ~sw db_path in
      Alcotest.(check bool) "binding survives reopen" true
        (Store.object_identity db ~scope=`Bound identity)))

let test_flag_settlement_transaction env =
  let module J=Store.Journal in
  let path=Filename.temp_file "imap-flag-settlement-" ".db" in
  let cleanup ()=List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
    [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup @@ fun () ->
  let fs=Eio.Stdenv.fs env in
  Eio.Switch.run (fun sw ->
    let db=Store.open_path ~sw Eio.Path.(fs / path) in
    let pair:J.pair={
      id="settle-pair";scope;remote_uidvalidity=Some (epoch 7L);
      remote_uid=Some (uid 3L);local_id=Some "settle-local";
      content_sha256=Some (String.make 64 'a');content_length=Some 9L;
      internal_date=None;common_flags=[flag "\\Seen"];
      remote_tombstone=None;local_tombstone=None;revision=0L} in
    let pair=match J.put_pair db ~expected_revision:None pair with
      | `Committed pair -> pair
      | `Stale_revision -> Alcotest.fail "new settlement pair stale" in
    let operation:J.operation={
      id="settle-op";pair_id=Some pair.id;local_id=pair.local_id;scope;
      kind=Flags;state=Prepared;
      source_uidvalidity=pair.remote_uidvalidity;
      source_uid=pair.remote_uid;destination=None;
      destination_uidvalidity=None;blob_sha256=None;blob_length=None;
      desired_flags=Some [flag "\\Flagged"];receipt=None;
      receipt_uidvalidity=None;receipt_uid=None} in
    J.prepare_operation ~local_flags:[flag "\\Seen"] db operation;
    Alcotest.(check bool) "prepared cannot be settled" true
      (J.settle_flag_operation db ~id:operation.id pair
        ~flags:[flag "\\Flagged"] ~evidence:"manual"=`Invalid_operation);
    J.mark_sent db ~id:operation.id;
    J.mark_ambiguous db ~id:operation.id;
    ignore (J.ensure_open_conflict db ~pair ~kind:Flag_conflict
      ~id:"settle-conflict" ~evidence:"unverified remote flags");
    let stale={pair with revision=Int64.pred pair.revision} in
    Alcotest.(check bool) "stale pair cannot settle" true
      (J.settle_flag_operation db ~id:operation.id stale
        ~flags:[flag "\\Flagged"] ~evidence:"manual"=`Stale_revision);
    (match J.settle_flag_operation db ~id:operation.id pair
        ~flags:[flag "\\Flagged"] ~evidence:"operator aligned both sides" with
     | `Settled updated ->
         Alcotest.(check int64) "settlement advances pair"
           (Int64.succ pair.revision) updated.revision
     | _ -> Alcotest.fail "verified flag settlement failed");
    Alcotest.(check bool) "superseded intent rejected" true
      ((Option.get (J.find_operation db ~id:operation.id)).state=Rejected);
    Alcotest.(check int) "flag conflict resolved atomically" 0
      (List.length (J.open_conflicts db ~scope)));
  Eio.Switch.run (fun sw ->
    let db=Store.open_readonly ~sw Eio.Path.(fs / path) in
    let pair=Option.get (J.find_pair db ~id:"settle-pair") in
    Alcotest.(check (list string)) "adopted flags survive restart"
      ["\\Flagged"]
      (List.map Mail_flag.Imap_flag.to_wire pair.common_flags);
    let op=Option.get (J.find_operation db ~id:"settle-op") in
    Alcotest.(check bool) "operator evidence survives restart" true
      (Option.is_some op.receipt &&
       String.length (Option.get op.receipt)>0))

let test_reopen env =
  let path=Filename.temp_file "imap-store-" ".db" in
  let cleanup () =
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup (fun () ->
    let first = Eio.Switch.run (fun sw ->
      let fs=Eio.Stdenv.fs env in
      let db=Store.open_path ~sw Eio.Path.(fs / path) in
      let rival=Store.open_path ~sw Eio.Path.(fs / path) in
      let initial=Store.load db ~scope in
      let rival_initial=Store.load rival ~scope in
      Alcotest.(check int64) "initial revision" 0L initial.cursor.revision;
      let next=transition initial.cursor initial.snapshot ~stage:"first"
        ~epoch_value:5L [row 1L [flag "\\Seen";flag "custom"];
                         row 2L [flag "\\Flagged"]] in
      Alcotest.(check bool) "committed" true
        (Store.publish db next=`Committed);
      let rival_next=transition rival_initial.cursor rival_initial.snapshot
        ~stage:"rival" ~epoch_value:5L [row 4L []] in
      Alcotest.(check bool) "cross-connection CAS" true
        (Store.publish rival rival_next=`Stale_revision);
      let altered_scope = {scope with raw_name="Different"} in
      let c=next.cursor in
      let altered_cursor = ok (M.restore ~schema_version:c.schema_version
        ~scope:altered_scope ~phase:c.phase ~uidvalidity:c.uidvalidity
        ~generation:c.generation ~revision:c.revision ~anchor:c.anchor
        ~frontier:c.frontier ~inventory_ref:c.inventory_ref ~mode:c.mode) in
      let altered=transition altered_cursor (Some next.snapshot)
        ~stage:"wrong-scope" ~epoch_value:5L [row 1L []] in
      Alcotest.(check bool) "same-revision scope mismatch" true
        (Store.publish rival altered=`Stale_revision);
      Alcotest.(check bool) "CAS rejects replay" true
        (Store.publish db next=`Stale_revision);
      let intent : Store.intent = {
        id="append-1";scope;
        kind=Append {message_id="<a@x>";content_digest=String.make 64 'a';
                     spool_ref="/spool/a";
                     pre_send_uid_frontier=Some 2L;
                     expected_length=Some 47L;
                     expected_flags=Some [flag "\\Seen";flag "Seen"];
                     expected_internal_date=Some "26-Sep-2026 12:00:00 +0000"};
        state=Prepared;uidvalidity=Some (epoch 5L);uid=None } in
      Store.prepare_intent db intent;
      (try Store.prepare_intent db intent;
           Alcotest.fail "duplicate intent accepted"
       with Sqlite3.SqliteError _ | Sqlite3.Error _ -> ());
      Store.set_intent_state db ~id:intent.id Sent;
      next) in
    Eio.Switch.run (fun sw ->
      let fs=Eio.Stdenv.fs env in
      let db=Store.open_path ~sw Eio.Path.(fs / path) in
      let loaded=Store.load db ~scope in
      Alcotest.(check int64) "revision survives reopen" 1L loaded.cursor.revision;
      Alcotest.(check (list int64)) "UIDs survive reopen" [1L;2L]
        (uids (Option.get loaded.snapshot));
      let flags=(List.hd (M.rows (Option.get loaded.snapshot))).flags in
      Alcotest.(check (list string)) "flags survive reopen"
        ["\\Seen";"custom"] (List.map Mail_flag.Imap_flag.to_wire flags);
      let pending=Store.pending_intents db ~scope in
      Alcotest.(check int) "pending APPEND survives reopen" 1
        (List.length pending);
      Alcotest.(check bool) "sent state" true
        ((List.hd pending).state=Sent);
      (match (List.hd pending).kind with
       | Append metadata ->
         Alcotest.(check (option int64)) "pre-send frontier" (Some 2L)
           metadata.pre_send_uid_frontier;
         Alcotest.(check (option int64)) "expected length" (Some 47L)
           metadata.expected_length;
         Alcotest.(check (option (list string))) "exact flags"
           (Some ["\\Seen";"Seen"])
           (Option.map (List.map Mail_flag.Imap_flag.to_wire)
             metadata.expected_flags);
         Alcotest.(check (option string)) "expected internal date"
           (Some "26-Sep-2026 12:00:00 +0000")
           metadata.expected_internal_date
       | _ -> Alcotest.fail "APPEND decoded as another intent");
      let second=transition loaded.cursor loaded.snapshot ~stage:"epoch"
        ~epoch_value:6L [row 1L []] in
      Alcotest.(check bool) "epoch published" true
        (Store.publish db second=`Committed);
      Alcotest.(check bool) "old transition stale" true
        (Store.publish db first=`Stale_revision);
      Store.set_intent_state db ~id:"append-1" Ambiguous;
      Store.confirm_intent db ~id:"append-1"
        ~uidvalidity:(Some (epoch 5L)) ~uid:(Some (uid 3L));
      let receipt=Option.get (Store.find_intent db ~id:"append-1") in
      Alcotest.(check bool) "APPENDUID receipt is durable" true
        (receipt.state=Confirmed && receipt.uidvalidity=Some (epoch 5L)
         && receipt.uid=Some (uid 3L));
      Alcotest.(check int) "resolved intents hidden" 0
        (List.length (Store.pending_intents db ~scope));
      (try Store.set_intent_state db ~id:"append-1" Sent;
           Alcotest.fail "illegal reverse state accepted"
       with Invalid_argument _ -> ()));
    Eio.Switch.run (fun sw ->
      let fs=Eio.Stdenv.fs env in
      let db=Store.open_path ~sw Eio.Path.(fs / path) in
      let receipt=Option.get (Store.find_intent db ~id:"append-1") in
      Alcotest.(check bool) "APPENDUID survives reopen" true
        (receipt.state=Confirmed && receipt.uidvalidity=Some (epoch 5L)
         && receipt.uid=Some (uid 3L))));
  ()

let test_blobs env =
  let path=Filename.temp_file "imap-blob-db-" ".db" in
  let dir_path=Filename.temp_file "imap-blobs-" "" in
  Sys.remove dir_path;
  let fs=Eio.Stdenv.fs env in
  let dir=Eio.Path.(fs / dir_path) in
  Eio.Path.mkdir ~perm:0o700 dir;
  let cleanup () =
    List.iter (fun name ->
      try Eio.Path.unlink Eio.Path.(dir / name) with _ -> ())
      (try Eio.Path.read_dir dir with _ -> []);
    (try Eio.Path.rmdir dir with _ -> ());
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup (fun () ->
    let content="From: a@example.test\r\nSubject: exact\r\n\r\nHello\000world\r\n" in
    let digest=Digestif.SHA256.(to_hex (digest_string content)) in
    let saved=Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw ~blob_dir:dir Eio.Path.(fs / path) in
      let initial=Store.load db ~scope in
      let first=transition initial.cursor initial.snapshot ~stage:"blob"
        ~epoch_value:19L [row 1L []] in
      Alcotest.(check bool) "message published" true
        (Store.publish db first=`Committed);
      let blob=Store.Blob.put db ~source:(Eio.Flow.string_source content)
        ~length:(Int64.of_int (String.length content))
        ~expected_sha256:digest () in
      Alcotest.(check string) "SHA-256" digest blob.sha256;
      Alcotest.(check bool) "verified after fsync" true
        (Store.Blob.verify db blob);
      (try ignore (Store.Blob.put db ~source:(Eio.Flow.string_source "short")
        ~length:6L () : Store.Blob.blob);
           Alcotest.fail "truncated source accepted"
       with End_of_file -> ());
      (try ignore (Store.Blob.put db ~source:(Eio.Flow.string_source content)
        ~length:(Int64.of_int (String.length content))
        ~expected_sha256:(String.make 64 '0') () : Store.Blob.blob);
           Alcotest.fail "wrong digest accepted"
       with Store.Blob.Digest_mismatch -> ());
      (try Store.Blob.attach db ~scope ~uidvalidity:(epoch 20L)
        ~uid:(uid 1L) blob;
           Alcotest.fail "wrong epoch attachment accepted"
       with Invalid_argument _ -> ());
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 19L) ~uid:(uid 1L)
        blob;
      Alcotest.(check (list string)) "failed writes leave no temp files" []
        (Store.Blob.orphan_candidates db);
      let pending=Store.Blob.put db ~source:(Eio.Flow.string_source "pending")
        ~length:7L () in
      Store.prepare_intent db {id="gc-pending-intent";scope;
        kind=Store.Append {message_id="gc-pending";content_digest=pending.sha256;
          spool_ref="sha256-"^pending.sha256;pre_send_uid_frontier=None;
          expected_length=Some 7L;expected_flags=None;expected_internal_date=None};
        state=Store.Prepared;uidvalidity=None;uid=None};
      Store.set_intent_state db ~id:"gc-pending-intent" Store.Sent;
      let pending_op=Store.Blob.put db ~source:(Eio.Flow.string_source "operation")
        ~length:9L () in
      Store.Journal.prepare_operation db {id="gc-pending-operation";
        pair_id=None;local_id=None;scope;kind=Store.Journal.Append;
        state=Store.Journal.Prepared;
        source_uidvalidity=None;source_uid=None;destination=Some scope;
        destination_uidvalidity=None;receipt_uidvalidity=None;receipt_uid=None;
        blob_sha256=Some pending_op.sha256;blob_length=Some 9L;
        desired_flags=None;receipt=None};
      let orphan=Store.Blob.put db ~source:(Eio.Flow.string_source "orphan")
        ~length:6L () in
      Eio.Path.save ~create:(`Exclusive 0o600)
        Eio.Path.(dir / ".tmp-crash") "partial";
      blob,orphan) in
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw ~blob_dir:dir Eio.Path.(fs / path) in
      let blob,orphan=saved in
      Alcotest.(check bool) "reference survives reopen" true
        (Store.Blob.find db ~scope ~uidvalidity:(epoch 19L) ~uid:(uid 1L)
         = Some blob);
      Alcotest.(check bool) "blob rehash after reopen" true
        (Store.Blob.verify db blob);
      let input=Store.Blob.open_in db ~sw blob in
      let out=Buffer.create 64 in
      Eio.Flow.copy input (Eio.Flow.buffer_sink out);
      Alcotest.(check string) "exact octets" content (Buffer.contents out);
      Eio.Path.save ~create:(`Or_truncate 0o600)
        Eio.Path.(dir / ("sha256-"^blob.sha256)) "damaged";
      Alcotest.(check bool) "corruption detected" false
        (Store.Blob.verify db blob);
      let repaired=Store.Blob.put db ~source:(Eio.Flow.string_source content)
        ~length:(Int64.of_int (String.length content))
        ~expected_sha256:digest () in
      Alcotest.(check bool) "repair retains content address" true
        (repaired=blob && Store.Blob.verify db blob);
      let names=Store.Blob.orphan_candidates db in
      Alcotest.(check bool) "unreferenced final detected" true
        (List.mem ("sha256-"^orphan.sha256) names);
      Alcotest.(check bool) "crashed temp detected" true
        (List.mem ".tmp-crash" names);
      Alcotest.(check bool) "referenced file excluded" false
        (List.mem ("sha256-"^blob.sha256) names);
      let loaded=Store.load db ~scope in
      let refresh=transition loaded.cursor loaded.snapshot ~stage:"refresh"
        ~epoch_value:19L [row 1L []] in
      Alcotest.(check bool) "refresh committed" true
        (Store.publish db refresh=`Committed);
      Alcotest.(check bool) "reference retained across refresh" true
        (Store.Blob.find db ~scope ~uidvalidity:(epoch 19L) ~uid:(uid 1L)
         = Some blob);
      let removed=transition refresh.cursor (Some refresh.snapshot)
        ~stage:"removed" ~epoch_value:19L [] in
      Alcotest.(check bool) "removal committed" true
        (Store.publish db removed=`Committed);
      Alcotest.(check bool) "reference pruned with UID" true
        (Store.Blob.find db ~scope ~uidvalidity:(epoch 19L) ~uid:(uid 1L)
         = None);
      let reaped=Store.Blob.reap_orphans db in
      List.iter (fun content ->
        let sha256=Digestif.SHA256.(to_hex (digest_string content)) in
        Alcotest.(check string) "journal body survives restart and collection" content
          (Eio.Path.load Eio.Path.(dir / ("sha256-"^sha256))))
        ["pending";"operation"];
      Alcotest.(check bool) "formerly referenced blob reaped" true
        (List.mem ("sha256-"^blob.sha256) reaped);
      Alcotest.(check bool) "crashed temp reaped" true
        (List.mem ".tmp-crash" reaped);
      Alcotest.(check (list string)) "no remaining candidates" []
        (Store.Blob.orphan_candidates db);
      Alcotest.(check bool) "missing blob fails verification" false
        (Store.Blob.verify db blob)));
  ()

let test_missing_blob_pages env =
  let path=Filename.temp_file "imap-missing-blob-db-" ".db" in
  let dir_path=Filename.temp_file "imap-missing-blobs-" "" in
  Sys.remove dir_path;
  let fs=Eio.Stdenv.fs env in
  let dir=Eio.Path.(fs / dir_path) in
  Eio.Path.mkdir ~perm:0o700 dir;
  let cleanup () =
    List.iter (fun name -> try Eio.Path.unlink Eio.Path.(dir / name)
      with _ -> ()) (try Eio.Path.read_dir dir with _ -> []);
    (try Eio.Path.rmdir dir with _ -> ());
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup (fun () ->
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw ~blob_dir:dir Eio.Path.(fs / path) in
      let initial=Store.load_cursor db ~scope in
      let page cursor ?after_uid ~limit () =
        match Store.Blob.missing_page db ~scope ~cursor ?after_uid ~limit () with
        | `Uids uids -> List.map Imap.Uid.to_int64 uids
        | `Stale_revision -> Alcotest.fail "unexpected stale blob page" in
      Alcotest.(check (list int64)) "new mailbox missing page" []
        (page initial ~limit:2 ());
      let first=transition initial None ~stage:"missing-blob-first"
        ~epoch_value:91L [row 1L [];row 2L [];row 3L [];row 4L []] in
      Alcotest.(check bool) "published inventory" true
        (Store.publish db first=`Committed);
      Alcotest.(check (list int64)) "first bounded page" [1L;2L]
        (page first.cursor ~limit:2 ());
      Alcotest.(check (list int64)) "second bounded page" [3L;4L]
        (page first.cursor ~after_uid:(uid 2L) ~limit:2 ());
      let blob=Store.Blob.put db ~source:(Eio.Flow.string_source "body")
        ~length:4L () in
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 91L)
        ~uid:(uid 2L) blob;
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 91L)
        ~uid:(uid 4L) blob;
      Alcotest.(check (list int64)) "references filter missing page"
        [1L;3L] (page first.cursor ~limit:2 ());
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 91L)
        ~uid:(uid 3L) blob;
      Alcotest.(check (list int64)) "attachment shrinks later page" []
        (page first.cursor ~after_uid:(uid 1L) ~limit:2 ());
      let second=transition first.cursor (Some first.snapshot)
        ~stage:"missing-blob-second" ~epoch_value:91L
        [row 1L [];row 2L [];row 3L [];row 4L []] in
      Alcotest.(check bool) "same-epoch revision published" true
        (Store.publish db second=`Committed);
      Alcotest.(check bool) "stale revision refused" true
        (Store.Blob.missing_page db ~scope ~cursor:first.cursor
          ~limit:2 ()=`Stale_revision);
      Alcotest.(check (list int64)) "same epoch references survive"
        [1L] (page second.cursor ~limit:2 ());
      let third=transition second.cursor (Some second.snapshot)
        ~stage:"missing-blob-third" ~epoch_value:92L
        [row 1L [];row 2L []] in
      Alcotest.(check bool) "new epoch published" true
        (Store.publish db third=`Committed);
      Alcotest.(check bool) "stale epoch refused" true
        (Store.Blob.missing_page db ~scope ~cursor:second.cursor
          ~limit:2 ()=`Stale_revision);
      Alcotest.(check (list int64)) "new epoch has no blob refs"
        [1L;2L] (page third.cursor ~limit:2 ());
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 92L)
        ~uid:(uid 1L) blob;
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 92L)
        ~uid:(uid 2L) blob;
      let refs cursor ?after_uid ~limit () =
        match Store.Blob.referenced_page db ~scope ~cursor ?after_uid
          ~limit () with
        | `Refs rows -> List.map (fun (uid,_) -> Imap.Uid.to_int64 uid) rows
        | `Stale_revision -> Alcotest.fail "unexpected stale reference page" in
      Alcotest.(check (list int64)) "first bounded reference page" [1L]
        (refs third.cursor ~limit:1 ());
      Alcotest.(check (list int64)) "second bounded reference page" [2L]
        (refs third.cursor ~after_uid:(uid 1L) ~limit:1 ());
      Alcotest.(check bool) "stale reference page refused" true
        (Store.Blob.referenced_page db ~scope ~cursor:second.cursor
          ~limit:1 ()=`Stale_revision);
      Alcotest.(check bool) "stale detach refused" true
        (Store.Blob.detach_if_matches db ~scope ~cursor:second.cursor
          ~uid:(uid 1L) blob=`Stale_revision);
      let replacement=Store.Blob.put db
        ~source:(Eio.Flow.string_source "new body") ~length:8L () in
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 92L)
        ~uid:(uid 1L) replacement;
      Alcotest.(check bool) "changed reference retained" true
        (Store.Blob.detach_if_matches db ~scope ~cursor:third.cursor
          ~uid:(uid 1L) blob=`Unchanged);
      Alcotest.(check bool) "exact corrupt reference detached" true
        (Store.Blob.detach_if_matches db ~scope ~cursor:third.cursor
          ~uid:(uid 2L) blob=`Detached);
      Alcotest.(check (list int64)) "detached reference is missing"
        [2L] (page third.cursor ~limit:2 ());
      Alcotest.(check (list int64)) "replacement reference remains"
        [1L] (refs third.cursor ~limit:2 ());
      (try ignore (Store.Blob.missing_page db ~scope
        ~cursor:third.cursor ~limit:0 ());
       Alcotest.fail "zero page limit accepted" with Invalid_argument _ -> ());
      (try ignore (Store.Blob.missing_page db
        ~scope:{scope with raw_name="other"}
        ~cursor:third.cursor ~limit:1 ());
       Alcotest.fail "cursor/scope mismatch accepted"
       with Invalid_argument _ -> ())))

let test_schema_upgrade env ~from_version =
  let path=Filename.temp_file "imap-upgrade-" ".db" in
  let fs=Eio.Stdenv.fs env in
  let cleanup () =
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup (fun () ->
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw Eio.Path.(fs / path) in
      if List.mem from_version [5;7;8;9;10;11;12] then (
        let module J=Store.Journal in
        let pair : J.pair = {
          id="legacy-pair";scope;remote_uidvalidity=Some (epoch 5L);
          remote_uid=Some (uid 1L);local_id=Some "legacy-local";
          content_sha256=None;content_length=None;internal_date=None;
          common_flags=[];remote_tombstone=None;local_tombstone=None;
          revision=0L} in
        let pair=match J.put_pair db ~expected_revision:None pair with
          | `Committed pair -> pair
          | `Stale_revision -> Alcotest.fail "legacy pair create failed" in
        if from_version=5 || from_version=7 then (
        let operation : J.operation = {
          id="legacy-flags";pair_id=Some pair.id;
          local_id=pair.local_id;scope;kind=Flags;state=Prepared;
          source_uidvalidity=pair.remote_uidvalidity;
          source_uid=pair.remote_uid;destination=None;
          destination_uidvalidity=None;blob_sha256=None;blob_length=None;
          desired_flags=Some [flag "\\Seen"];receipt=None;
          receipt_uidvalidity=None;receipt_uid=None} in
        J.prepare_operation db operation;
        J.mark_sent db ~id:operation.id;
        J.observe_operation db ~id:operation.id ~receipt:"legacy observed"
          ~destination_uidvalidity:None ~destination_uid:None)));
    Eio.Switch.run (fun sw ->
      let db=Sqlite3_eio.open_path ~sw Eio.Path.(fs / path) in
      Sqlite3.Rc.check (Sqlite3_eio.exec db
        "DROP TABLE sync_pair_presence");
      if from_version<=11 then Sqlite3.Rc.check (Sqlite3_eio.exec db
        "DROP TABLE mailbox_object_ids");
      if from_version<=10 then Sqlite3.Rc.check (Sqlite3_eio.exec db
        "DROP TABLE sync_operation_source_dates");
      if from_version<=9 then Sqlite3.Rc.check (Sqlite3_eio.exec db
        "ALTER TABLE sync_pairs DROP COLUMN internal_date");
      if from_version<=8 then Sqlite3.Rc.check (Sqlite3_eio.exec db
        "DROP TABLE sync_operation_local_sources");
      if from_version<=7 then List.iter (fun table ->
        Sqlite3.Rc.check (Sqlite3_eio.exec db ("DROP TABLE "^table)))
        ["sync_operation_local_preimage_flags";
         "sync_operation_local_preimages"];
      if from_version<=5 then
        Sqlite3.Rc.check (Sqlite3_eio.exec db
          "DROP TABLE sync_operation_preconditions");
      if from_version=5 || from_version=6 then (
        Sqlite3.Rc.check (Sqlite3_eio.exec db
          "ALTER TABLE sync_pairs DROP COLUMN content_sha256");
        Sqlite3.Rc.check (Sqlite3_eio.exec db
          "ALTER TABLE sync_pairs DROP COLUMN content_length"));
      if from_version<5 then List.iter (fun table ->
        Sqlite3.Rc.check (Sqlite3_eio.exec db ("DROP TABLE "^table)))
        ["sync_operation_flags";"sync_operations";"sync_conflicts";
         "sync_pair_flags";"sync_pairs"];
      if from_version<4 then List.iter (fun table ->
        Sqlite3.Rc.check (Sqlite3_eio.exec db ("DROP TABLE "^table)))
        ["scan_flags";"scan_rows";"scan_stages"];
      if from_version<3 then (
        if from_version=1 then
          Sqlite3.Rc.check (Sqlite3_eio.exec db "DROP TABLE blob_refs");
        Sqlite3.Rc.check (Sqlite3_eio.exec db "DROP TABLE intent_flags");
        Sqlite3.Rc.check (Sqlite3_eio.exec db "DROP TABLE intents");
        Sqlite3.Rc.check (Sqlite3_eio.exec db
          "CREATE TABLE intents (id TEXT PRIMARY KEY, endpoint TEXT NOT NULL, \
           account TEXT NOT NULL, mailbox_key TEXT NOT NULL, \
           raw_name TEXT NOT NULL, encoding TEXT NOT NULL, mailbox_id TEXT, \
           kind TEXT NOT NULL, message_id TEXT, digest TEXT, spool_ref TEXT, \
           state TEXT NOT NULL, uidvalidity INTEGER, uid INTEGER)");
        Sqlite3.Rc.check (Sqlite3_eio.exec db
          "INSERT INTO intents VALUES ('legacy-append','imap.example','alice', \
           'inbox','INBOX','mutf7',NULL,'append','<legacy@x>', \
           'sha256:old','/spool/old','ambiguous',5,NULL)"));
      Sqlite3.Rc.check (Sqlite3_eio.exec db
        ("PRAGMA user_version=" ^ string_of_int from_version)));
    (if from_version>=8 then Eio.Switch.run (fun sw ->
      let db=Store.open_readonly ~sw Eio.Path.(fs / path) in
      if from_version=12 then Alcotest.(check (option int64))
        "v12 has no presence witness table" None
        (Store.Journal.last_presence_generation db ~pair_id:"legacy-pair"
          ~side:`Local);
      Alcotest.(check bool) "older pair readable before migration" true
        (match Store.Journal.find_pair db ~id:"legacy-pair" with
         | Some pair -> pair.internal_date=None
         | None -> false);
      Alcotest.(check bool) "older source date is unknown" true
        (Store.Journal.operation_source_date db ~id:"legacy-flags"=None)));
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw Eio.Path.(fs / path) in
      Alcotest.(check int64) "pre-blob cursor still loads" 0L
        (Store.load db ~scope).cursor.revision;
      if from_version>=8 then Alcotest.(check bool)
        "migrated pair retains unknown date" true
        (match Store.Journal.find_pair db ~id:"legacy-pair" with
         | Some pair -> pair.internal_date=None
         | None -> false);
      if from_version=5 then (
        let module J=Store.Journal in
        let pair=Option.get (J.find_pair db ~id:"legacy-pair") in
        Alcotest.(check bool) "v5 pending operation lacks safe precondition"
          true (J.commit_operation_with_pair db ~id:"legacy-flags"
            ~expected_pair_revision:(Some pair.revision)
            {pair with common_flags=[flag "\\Seen"]}=`Stale_revision));
      if from_version=7 then (
        let module J=Store.Journal in
        Alcotest.(check bool) "v7 FLAGS preimage remains unknown" true
          (J.local_flags_preimage db ~id:"legacy-flags"=None);
        Alcotest.(check (option int64)) "v7 pair precondition retained"
          (Some 1L) (J.operation_pair_revision db ~id:"legacy-flags"));
      if from_version<3 then (
      let old=Option.get (Store.find_intent db ~id:"legacy-append") in
      (match old.kind with
       | Append metadata ->
         Alcotest.(check (option int64)) "legacy frontier unknown" None
           metadata.pre_send_uid_frontier;
         Alcotest.(check (option int64)) "legacy length unknown" None
           metadata.expected_length;
         Alcotest.(check (option (list string))) "legacy flags unknown" None
           (Option.map (List.map Mail_flag.Imap_flag.to_wire)
             metadata.expected_flags);
         Alcotest.(check (option string)) "legacy date unknown" None
           metadata.expected_internal_date
       | _ -> Alcotest.fail "legacy APPEND changed kind"))));
  ()

let test_disk_stage env =
  let path=Filename.temp_file "imap-stage-" ".db" in
  let fs=Eio.Stdenv.fs env in
  let db_path=Eio.Path.(fs / path) in
  let cleanup ()=
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"] in
  let count=100_001L in
  let action cursor id =
    let selected : M.selected = {
      uidvalidity=epoch 42L;uidnext=Int64.succ count;
      highestmodseq=Some (modseq 73L);nomodseq=false} in
    ok (M.plan cursor ~stage_id:id selected) in
  let window first last =
    List.init (Int64.to_int (Int64.succ (Int64.sub last first)))
      (fun index -> Int64.add first (Int64.of_int index)) in
  Fun.protect ~finally:cleanup (fun () ->
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw db_path in
      let cursor=Store.load_cursor db ~scope in
      let partial=action cursor "crashed-stage" in
      Store.begin_stage db ~cursor ~action:partial;
      Store.stage_rows db ~stage_id:partial.id ~first:1L ~last:1000L
        (List.map (fun n -> row n []) (window 1L 1000L));
      Alcotest.(check int64) "partial stage cannot publish" 0L
        (Store.load_cursor db ~scope).revision;
      (try ignore (Store.publish_stage db ~cursor ~action:partial
        ~explicit_highestmodseq:(Some (modseq 73L)) ~nomodseq:false);
        Alcotest.fail "incomplete stage published"
       with Invalid_argument _ -> ()));
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw db_path in
      Alcotest.(check (list string)) "crash stage survives restart"
        ["crashed-stage"] (Store.abandoned_stages db);
      Alcotest.(check int64) "crash did not advance cursor" 0L
        (Store.load_cursor db ~scope).revision;
      Store.discard_stage db ~stage_id:"crashed-stage";
      let cursor=Store.load_cursor db ~scope in
      let planned=action cursor "large-stage" in
      Store.begin_stage db ~cursor ~action:planned;
      let rec fetch first=
        if first>count then () else
        let last=Int64.min count (Int64.add first 999L) in
        Store.stage_rows db ~stage_id:planned.id ~first ~last
          (List.map (fun n -> row n [flag "\\Seen"]) (window first last));
        fetch (Int64.succ last) in
      fetch 1L;
      (try ignore (Store.publish_stage db ~cursor ~action:planned
        ~explicit_highestmodseq:(Some (modseq 73L)) ~nomodseq:false);
        Alcotest.fail "stage published before SEARCH coverage"
       with Invalid_argument _ -> ());
      let rec search first=
        if first>count then () else
        let last=Int64.min count (Int64.add first 999L) in
        Store.stage_membership db ~stage_id:planned.id ~first ~last
          (List.map uid (window first last));
        search (Int64.succ last) in
      search 1L;
      let receipt=match Store.publish_stage db ~cursor ~action:planned
        ~explicit_highestmodseq:(Some (modseq 73L)) ~nomodseq:false with
        | `Committed receipt -> receipt
        | `Stale_revision -> Alcotest.fail "fresh stage was stale" in
      Alcotest.(check int64) "more than old row limit published"
        count receipt.row_count;
      Alcotest.(check int64) "cursor CAS advanced" 1L
        (Store.load_cursor db ~scope).revision;
      Alcotest.(check (list string)) "stage removed atomically" []
        (Store.abandoned_stages db);
      let stale=action cursor "stale-stage" in
      Store.begin_stage db ~cursor ~action:stale;
      let first=row 1L [] in
      Store.stage_rows db ~stage_id:stale.id ~first:1L ~last:count [first];
      Store.stage_membership db ~stage_id:stale.id
        ~first:1L ~last:count [uid 1L];
      Alcotest.(check bool) "stale CAS leaves published data" true
        (Store.publish_stage db ~cursor ~action:stale
          ~explicit_highestmodseq:(Some (modseq 73L)) ~nomodseq:false
         = `Stale_revision);
      Store.discard_stage db ~stage_id:stale.id;
      Alcotest.(check int64) "published cursor remains" 1L
        (Store.load_cursor db ~scope).revision));
  ()

let test_stage_blob_refs env =
  let path=Filename.temp_file "imap-stage-ref-" ".db" in
  let dir_path=Filename.temp_file "imap-stage-blobs-" "" in
  Sys.remove dir_path;
  let fs=Eio.Stdenv.fs env in
  let dir=Eio.Path.(fs / dir_path) in
  Eio.Path.mkdir ~perm:0o700 dir;
  let cleanup ()=
    List.iter (fun name ->
      try Eio.Path.unlink Eio.Path.(dir / name) with _ -> ())
      (try Eio.Path.read_dir dir with _ -> []);
    (try Eio.Path.rmdir dir with _ -> ());
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup (fun () ->
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw ~blob_dir:dir Eio.Path.(fs / path) in
      let initial=Store.load db ~scope in
      let first=transition initial.cursor initial.snapshot ~stage:"blob-stage-base"
        ~epoch_value:56L [row 1L [];row 2L []] in
      Alcotest.(check bool) "base published" true
        (Store.publish db first=`Committed);
      let blob=Store.Blob.put db
        ~source:(Eio.Flow.string_source "same-content") ~length:12L () in
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 56L) ~uid:(uid 1L) blob;
      Store.Blob.attach db ~scope ~uidvalidity:(epoch 56L) ~uid:(uid 2L) blob;
      let cursor=Store.load_cursor db ~scope in
      let selected : M.selected = {
        uidvalidity=epoch 56L;uidnext=5L;
        highestmodseq=Some (modseq 18L);nomodseq=false} in
      let action=ok (M.plan cursor ~stage_id:"blob-stage-next" selected) in
      Store.begin_stage db ~cursor ~action;
      Store.stage_rows db ~stage_id:action.id ~first:1L ~last:4L
        [row 2L []];
      Store.stage_membership db ~stage_id:action.id ~first:1L ~last:4L
        [uid 2L];
      (match Store.publish_stage db ~cursor ~action
        ~explicit_highestmodseq:(Some (modseq 18L)) ~nomodseq:false with
       | `Committed _ -> ()
       | `Stale_revision -> Alcotest.fail "blob stage unexpectedly stale");
      Alcotest.(check bool) "expunged UID reference removed" true
        (Store.Blob.find db ~scope ~uidvalidity:(epoch 56L)
          ~uid:(uid 1L)=None);
      Alcotest.(check bool) "surviving UID reference retained" true
        (Store.Blob.find db ~scope ~uidvalidity:(epoch 56L)
          ~uid:(uid 2L)=Some blob)));
  ()

let test_sync_journal env =
  let module J = Store.Journal in
  let path=Filename.temp_file "imap-sync-journal-" ".db" in
  let fs=Eio.Stdenv.fs env in
  let db_path=Eio.Path.(fs / path) in
  let cleanup ()=List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
    [path;path^"-wal";path^"-shm"] in
  let pair id uid_value local_id : J.pair = {
    id;scope;remote_uidvalidity=Some (epoch 67L);remote_uid=Some (uid uid_value);
    local_id=Some local_id;content_sha256=None;content_length=None;
    internal_date=None;
    common_flags=[flag "\\Seen"];
    remote_tombstone=None;local_tombstone=None;revision=0L } in
  let date=match Imap.Internal_date.of_string
      "26-Sep-2025 12:34:56 +0230" with
    | Ok date -> date | Error error -> Alcotest.fail error in
  Fun.protect ~finally:cleanup (fun () ->
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw db_path in
      let initial=Store.load db ~scope in
      let published=transition initial.cursor initial.snapshot
        ~stage:"inventory-1" ~epoch_value:67L
        [row 1L [];row 2L []] in
      Alcotest.(check bool) "initial inventory" true
        (Store.publish db published=`Committed);
      let page=match Store.snapshot_page db ~scope ~cursor:published.cursor
          ~limit:1 () with
        | `Rows rows -> rows | `Stale_revision -> Alcotest.fail "fresh page stale" in
      Alcotest.(check (list int64)) "bounded snapshot first page" [1L]
        (List.map (fun (r:M.row) -> Imap.Uid.to_int64 r.uid) page);
      let page=match Store.snapshot_page db ~scope ~cursor:published.cursor
          ~after_uid:(uid 1L) ~limit:1 () with
        | `Rows rows -> rows | `Stale_revision -> Alcotest.fail "fresh page stale" in
      Alcotest.(check (list int64)) "bounded snapshot second page" [2L]
        (List.map (fun (r:M.row) -> Imap.Uid.to_int64 r.uid) page);
      Alcotest.(check bool) "indexed published UID membership" true
        (Store.snapshot_contains_uid db ~scope ~cursor:published.cursor
          ~uid:(uid 2L) = `Present true);
      Alcotest.(check bool) "indexed published UID absence" true
        (Store.snapshot_contains_uid db ~scope ~cursor:published.cursor
          ~uid:(uid 3L) = `Present false);
      let first=match J.put_pair db ~expected_revision:None
          {(pair "occ-1" 1L "maildir-base-a") with
           internal_date=Some date} with
        | `Committed p -> p | _ -> Alcotest.fail "first pair stale" in
      (try ignore (J.put_pair db ~expected_revision:None
        {(pair "bad-flags" 3L "maildir-base-c") with
         common_flags=[flag "\\Seen";flag "\\sEeN"]});
       Alcotest.fail "duplicate semantic flags accepted"
       with Invalid_argument _ -> ());
      let second=match J.put_pair db ~expected_revision:None
          (pair "occ-2" 2L "maildir-base-b") with
        | `Committed p -> p | _ -> Alcotest.fail "second pair stale" in
      Alcotest.(check bool) "duplicate content has distinct occurrences" true
        (first.id<>second.id && first.local_id<>second.local_id);
      Alcotest.(check (list string)) "pair page first" ["occ-1"]
        (List.map (fun (p:J.pair) -> p.id)
          (J.pairs_page db ~scope ~limit:1 ()));
      Alcotest.(check (list string)) "pair page second" ["occ-2"]
        (List.map (fun (p:J.pair) -> p.id)
          (J.pairs_page db ~scope ~after:"occ-1" ~limit:1 ()));
      (try ignore (J.put_pair db ~expected_revision:None
        (pair "occ-3" 1L "maildir-base-c"));
       Alcotest.fail "duplicate remote occurrence accepted"
       with Sqlite3.SqliteError _ | Sqlite3.Error _ -> ());
      Alcotest.(check string) "lookup remote" "occ-1"
        (Option.get (J.find_remote db ~scope ~uidvalidity:(epoch 67L)
          ~uid:(uid 1L))).id;
      Alcotest.(check string) "lookup local" "occ-2"
        (Option.get (J.find_local db ~scope ~local_id:"maildir-base-b")).id;
      let updated={first with common_flags=[flag "\\Flagged"]} in
      let updated=match J.put_pair db ~expected_revision:(Some first.revision)
          updated with
        | `Committed p -> p | _ -> Alcotest.fail "pair update stale" in
      (try ignore (J.put_pair db
        ~expected_revision:(Some updated.revision)
        {updated with internal_date=Some
          (match Imap.Internal_date.of_string
            "27-Sep-2025 12:34:56 +0230" with
           | Ok date -> date | Error error -> Alcotest.fail error)});
       Alcotest.fail "established pair date changed"
       with Invalid_argument _ -> ());
      Alcotest.(check bool) "old pair CAS rejected" true
        (J.put_pair db ~expected_revision:(Some first.revision) first
         = `Stale_revision);
      (try ignore (J.put_pair db ~expected_revision:(Some updated.revision)
        {updated with local_id=Some "changed-occurrence"});
       Alcotest.fail "identity changed"
       with Invalid_argument _ -> ());
      let conflict : J.conflict = {id="conf-1";pair_id=updated.id;
        kind=Flag_conflict;evidence="both ends changed Seen";
        pair_revision=updated.revision;resolved=false} in
      J.record_conflict db conflict;
      let policy=match J.ensure_open_conflict db ~pair:updated
        ~kind:Policy_conflict ~id:"policy-1"
        ~evidence:"deleted flag needs policy" with
        | `Open conflict -> conflict
        | `Stale_revision -> Alcotest.fail "fresh policy hold was stale" in
      let repeated=match J.ensure_open_conflict db ~pair:updated
        ~kind:Policy_conflict ~id:"policy-2"
        ~evidence:"deleted flag still needs policy" with
        | `Open conflict -> conflict
        | `Stale_revision -> Alcotest.fail "repeated policy hold was stale" in
      Alcotest.(check string) "policy hold ID is stable" policy.id
        repeated.id;
      Alcotest.(check string) "policy evidence refreshed"
        "deleted flag still needs policy" repeated.evidence;
      Alcotest.(check bool) "stale pair cannot record policy hold" true
        (J.ensure_open_conflict db ~pair:first ~kind:Policy_conflict
          ~id:"stale-policy" ~evidence:"stale" = `Stale_revision);
      Alcotest.(check bool) "policy resolution CAS" true
        (J.resolve_open_conflicts db ~pair:first
          ~kind:Policy_conflict = `Stale_revision);
      Alcotest.(check bool) "policy hold resolved" true
        (J.resolve_open_conflicts db ~pair:updated
          ~kind:Policy_conflict = `Resolved 1);
      (try J.record_conflict db {conflict with id="stale-conf";
        pair_revision=first.revision};
       Alcotest.fail "stale conflict accepted"
       with Invalid_argument _ -> ());
      let operation : J.operation = {
        id="op-1";pair_id=Some first.id;
        local_id=Some "maildir-base-a";scope;kind=Flags;state=Prepared;
        source_uidvalidity=Some (epoch 67L);source_uid=Some (uid 1L);
        destination=None;destination_uidvalidity=None;
        blob_sha256=None;blob_length=None;
        desired_flags=Some [flag "\\Flagged"];receipt=None;
        receipt_uidvalidity=None;receipt_uid=None} in
      J.prepare_operation ~local_flags:[] db operation;
      Alcotest.(check bool) "empty local FLAGS preimage is known" true
        (J.local_flags_preimage db ~id:operation.id=Some []);
      Alcotest.(check (option int64)) "FLAGS pair revision captured"
        (Some updated.revision)
        (J.operation_pair_revision db ~id:operation.id);
      J.mark_sent db ~id:operation.id;
      let append : J.operation = {operation with id="op-append";
        pair_id=None;kind=Append;source_uidvalidity=None;source_uid=None;
        destination=Some scope;destination_uidvalidity=Some (epoch 67L);
        blob_sha256=Some (String.make 64 'a');
        blob_length=Some 123L;desired_flags=Some []} in
      J.prepare_operation ~local_source_mtime:1709164800.125 db append;
      Alcotest.(check (option bool)) "APPEND source mtime captured"
        (Some true)
        (Option.map ((=) 1709164800.125)
          (J.operation_source_mtime db ~id:append.id));
      J.mark_sent db ~id:append.id;
      J.mark_ambiguous ~reason:"APPEND lacks an attributable UID"
        db ~id:append.id;
      let local_append : J.operation = {operation with
        id="op-local-append";kind=Local_append;
        blob_sha256=Some (String.make 64 'b');blob_length=Some 123L} in
      J.prepare_operation ~source_internal_date:date db local_append;
      (try J.prepare_operation ~source_internal_date:date db
        {append with id="bad-upload-date"};
       Alcotest.fail "source date accepted for an upload"
       with Invalid_argument _ -> ());
      let local_delete : J.operation = {operation with
        id="op-local-delete";kind=Local_delete;
        desired_flags=None} in
      J.prepare_operation db local_delete;
      (try J.prepare_operation db {local_append with id="bad-local-append";
        local_id=None};
       Alcotest.fail "local append without reserved occurrence ID accepted"
       with Invalid_argument _ -> ());
      (try J.commit_operation db ~id:append.id;
       Alcotest.fail "ambiguous operation committed"
       with Invalid_argument _ -> ());
      let vanished=transition published.cursor (Some published.snapshot)
        ~stage:"inventory-2" ~epoch_value:67L [row 1L []] in
      Alcotest.(check bool) "new complete inventory" true
        (Store.publish db vanished=`Committed);
      Alcotest.(check bool) "stale snapshot page rejected" true
        (Store.snapshot_page db ~scope ~cursor:published.cursor ~limit:1 ()
         = `Stale_revision);
      Alcotest.(check bool) "stale indexed membership rejected" true
        (Store.snapshot_contains_uid db ~scope ~cursor:published.cursor
          ~uid:(uid 1L) = `Stale_revision);
      let tombstone : J.tombstone = {reason=Inventory_absence;
        evidence="inventory-2";generation=Some vanished.cursor.generation} in
      (try ignore (J.put_pair db ~expected_revision:(Some updated.revision)
        {updated with remote_tombstone=Some tombstone});
       Alcotest.fail "live UID tombstoned"
       with Invalid_argument _ -> ());
      (try ignore (J.put_pair db ~expected_revision:(Some second.revision)
        {second with remote_tombstone=Some {tombstone with evidence="partial"}});
       Alcotest.fail "unpublished inventory tombstone accepted"
       with Invalid_argument _ -> ());
      ignore (match J.put_pair db ~expected_revision:(Some second.revision)
        {second with remote_tombstone=Some tombstone} with
       | `Committed p -> p | _ -> Alcotest.fail "verified tombstone stale"));
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw db_path in
      Alcotest.(check (option string)) "pair date survives restart"
        (Some "26-Sep-2025 12:34:56 +0230")
        (Option.map Imap.Internal_date.to_string
          (Option.get (J.find_pair db ~id:"occ-1")).internal_date);
      Alcotest.(check (option bool)) "APPEND source mtime survives restart"
        (Some true)
        (Option.map ((=) 1709164800.125)
          (J.operation_source_mtime db ~id:"op-append"));
      Alcotest.(check (option string)) "local append date survives restart"
        (Some "26-Sep-2025 12:34:56 +0230")
        (Option.map Imap.Internal_date.to_string
          (J.operation_source_date db ~id:"op-local-append"));
      Alcotest.(check int) "pairs survive reopen" 2
        (List.length (J.pairs db ~scope));
      Alcotest.(check bool) "verified tombstone survives" true
        (Option.is_some (Option.get (J.find_pair db ~id:"occ-2")).remote_tombstone);
      Alcotest.(check int) "open conflict survives" 1
        (List.length (J.open_conflicts db ~scope));
      Alcotest.(check int) "active operations survive" 4
        (List.length (J.active_operations db ~scope));
      Alcotest.(check bool) "typed local deletion survives restart" true
        ((Option.get (J.find_operation db ~id:"op-local-delete")).kind
         = Local_delete);
      Alcotest.(check bool) "typed local append survives restart" true
        ((Option.get (J.find_operation db ~id:"op-local-append")).kind
         = Local_append);
      Alcotest.(check bool) "ambiguous APPEND stays ambiguous" true
        ((Option.get (J.find_operation db ~id:"op-append")).state=Ambiguous);
      Alcotest.(check (option string)) "ambiguous reason survives restart"
        (Some "APPEND lacks an attributable UID")
        (Option.get (J.find_operation db ~id:"op-append")).receipt;
      Alcotest.(check bool) "local FLAGS preimage survives restart" true
        (J.local_flags_preimage db ~id:"op-1"=Some []);
      Alcotest.(check bool) "legacy FLAGS without preimage stays unknown" true
        (J.local_flags_preimage db ~id:"op-local-delete"=None);
      Alcotest.(check bool) "immutable expected destination epoch" true
        ((Option.get (J.find_operation db ~id:"op-append")).destination_uidvalidity
         = Some (epoch 67L));
      J.observe_operation db ~id:"op-append" ~receipt:"APPENDUID 67 3"
        ~destination_uidvalidity:(Some (epoch 67L))
        ~destination_uid:(Some (uid 3L));
      J.commit_operation db ~id:"op-append";
      Alcotest.(check bool) "typed receipt survives state transition" true
        ((Option.get (J.find_operation db ~id:"op-append")).receipt_uid
         = Some (uid 3L));
      Alcotest.(check string) "reserved Maildir ID survives restart"
        "maildir-base-a"
        (Option.get (Option.get (J.find_operation db ~id:"op-append")).local_id);
      J.observe_operation db ~id:"op-1" ~receipt:"MODIFIED none"
        ~destination_uidvalidity:None ~destination_uid:None;
      (try J.commit_operation db ~id:"op-1";
       Alcotest.fail "paired operation bypassed pair CAS"
       with Invalid_argument _ -> ());
      let current=Option.get (J.find_pair db ~id:"occ-1") in
      Alcotest.(check bool) "stale pair cannot commit operation" true
        (J.commit_operation_with_pair db ~id:"op-1"
          ~expected_pair_revision:(Some 1L) current=`Stale_revision);
      Alcotest.(check bool) "stale commit remains observable" true
        ((Option.get (J.find_operation db ~id:"op-1")).state=Observed);
      (match J.commit_operation_with_pair db ~id:"op-1"
        ~expected_pair_revision:(Some current.revision) current with
       | `Committed _ -> () | `Stale_revision -> Alcotest.fail "fresh commit stale");
      Alcotest.(check bool) "pair and receipt committed" true
        ((Option.get (J.find_operation db ~id:"op-1")).state=Committed);
      let current=Option.get (J.find_pair db ~id:"occ-1") in
      let stale_op : J.operation = {
        id="op-stale-precondition";pair_id=Some current.id;
        local_id=current.local_id;scope;kind=Flags;state=Prepared;
        source_uidvalidity=current.remote_uidvalidity;
        source_uid=current.remote_uid;destination=None;
        destination_uidvalidity=None;blob_sha256=None;blob_length=None;
        desired_flags=Some [flag "\\Answered"];receipt=None;
        receipt_uidvalidity=None;receipt_uid=None} in
      J.prepare_operation db stale_op;
      J.mark_sent db ~id:stale_op.id;
      J.observe_operation db ~id:stale_op.id ~receipt:"verified"
        ~destination_uidvalidity:None ~destination_uid:None;
      let advanced=match J.put_pair db
        ~expected_revision:(Some current.revision)
        {current with common_flags=[flag "\\Draft"]} with
        | `Committed pair -> pair
        | `Stale_revision -> Alcotest.fail "pair advance failed" in
      Alcotest.(check bool) "operation precondition survives pair advance"
        true (J.commit_operation_with_pair db ~id:stale_op.id
          ~expected_pair_revision:(Some advanced.revision)
          {advanced with common_flags=[flag "\\Answered"]}
          = `Stale_revision);
      Alcotest.(check bool) "stale intent remains visible" true
        ((Option.get (J.find_operation db ~id:stale_op.id)).state=Observed);
      let with_content=match J.put_pair db
        ~expected_revision:(Some advanced.revision)
        {advanced with content_sha256=Some (String.make 64 'a');
          content_length=Some 123L} with
        | `Committed pair -> pair
        | `Stale_revision -> Alcotest.fail "content evidence backfill stale" in
      Alcotest.(check (option int64)) "content evidence survives pair CAS"
        (Some 123L)
        (Option.get (J.find_pair db ~id:with_content.id)).content_length;
      (try ignore (J.put_pair db
        ~expected_revision:(Some with_content.revision)
        {with_content with content_sha256=Some (String.make 64 'b')});
       Alcotest.fail "immutable content digest changed"
       with Invalid_argument _ -> ());
      Alcotest.(check int) "paired FLAGS commit resolved conflict" 0
        (List.length (J.open_conflicts db ~scope))));
  ()

let test_active_operation_pages env =
  let path=Filename.temp_file "imap-store-operations-" ".db" in
  let cleanup () =
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup (fun () ->
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
      let other={scope with account="bob"} in
      let create ~scope id : Store.Journal.operation = {
        id;pair_id=None;local_id=None;scope;
        kind=Flags;state=Prepared;
        source_uidvalidity=Some (epoch 9L);source_uid=Some (uid 1L);
        destination=None;destination_uidvalidity=None;
        blob_sha256=None;blob_length=None;desired_flags=Some [];
        receipt=None;receipt_uidvalidity=None;receipt_uid=None;
      } in
      let ids=["z-prepared";"c-rejected";"e-ambiguous";
               "d-committed";"b-sent";"a-observed"] in
      List.iter (fun id -> Store.Journal.prepare_operation db
        (create ~scope id)) ids;
      Store.Journal.prepare_operation db (create ~scope:other "f-other");
      Store.Journal.mark_sent db ~id:"b-sent";
      Store.Journal.mark_sent db ~id:"e-ambiguous";
      Store.Journal.mark_ambiguous db ~id:"e-ambiguous";
      Store.Journal.mark_sent db ~id:"a-observed";
      Store.Journal.observe_operation db ~id:"a-observed" ~receipt:"verified"
        ~destination_uidvalidity:None ~destination_uid:None;
      Store.Journal.mark_sent db ~id:"d-committed";
      Store.Journal.observe_operation db ~id:"d-committed" ~receipt:"verified"
        ~destination_uidvalidity:None ~destination_uid:None;
      Store.Journal.commit_operation db ~id:"d-committed";
      Store.Journal.reject_operation db ~id:"c-rejected" ~receipt:"not sent";
      let page ?after ~limit () =
        Store.Journal.active_operations_page db ~scope ?after ~limit ()
        |> List.map (fun (x:Store.Journal.operation) -> x.id) in
      Alcotest.(check (list string)) "first ordered page"
        ["a-observed";"b-sent"] (page ~limit:2 ());
      Alcotest.(check (list string)) "terminal rows skipped"
        ["e-ambiguous";"z-prepared"]
        (page ~after:"b-sent" ~limit:2 ());
      Alcotest.(check (list string)) "end of pages" []
        (page ~after:"z-prepared" ~limit:2 ());
      Alcotest.(check (list string)) "all active states"
        ["a-observed";"b-sent";"e-ambiguous";"z-prepared"]
        (page ~limit:100 ());
      Store.Journal.reject_operation db ~id:"e-ambiguous"
        ~receipt:"reconciled";
      Alcotest.(check (list string)) "state transition between pages"
        ["z-prepared"] (page ~after:"b-sent" ~limit:2 ());
      let pair : Store.Journal.pair = {
        id="paired-page";scope;remote_uidvalidity=Some (epoch 9L);
        remote_uid=Some (uid 1L);local_id=Some "local-page";
        content_sha256=None;content_length=None;internal_date=None;
        common_flags=[];
        remote_tombstone=None;local_tombstone=None;revision=0L;
      } in
      (match Store.Journal.put_pair db ~expected_revision:None pair with
       | `Committed _ -> () | `Stale_revision -> Alcotest.fail "pair stale");
      List.iter (fun id ->
        Store.Journal.prepare_operation db
          {(create ~scope id) with pair_id=Some pair.id;
            local_id=pair.local_id}) ["pair-z";"pair-a"];
      let active_pair () =
        Store.Journal.active_operation_for_pair db ~pair_id:pair.id
        |> Option.map (fun (x:Store.Journal.operation) -> x.id) in
      Alcotest.(check (option string)) "first active pair operation"
        (Some "pair-a") (active_pair ());
      Store.Journal.reject_operation db ~id:"pair-a" ~receipt:"resolved";
      Alcotest.(check (option string)) "next active pair operation"
        (Some "pair-z") (active_pair ());
      Store.Journal.reject_operation db ~id:"pair-z" ~receipt:"resolved";
      Alcotest.(check (option string)) "no pending pair operation"
        None (active_pair ());
      List.iter (fun limit ->
        try ignore (page ~limit ());
            Alcotest.fail "invalid active operation page limit accepted"
        with Invalid_argument _ -> ()) [0;10_001]));
  ()

let test_readonly_and_conflict_pages env =
  let path=Filename.temp_file "imap-store-readonly-" ".db" in
  let missing=path^"-missing" in
  let old=path^"-old" in
  let malformed=path^"-malformed" in
  let cleanup () =
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm";missing;old;old^"-wal";old^"-shm";
       malformed;malformed^"-wal";malformed^"-shm"] in
  Fun.protect ~finally:cleanup (fun () ->
    let fs=Eio.Stdenv.fs env in
    Eio.Switch.run (fun sw ->
      let db=Store.open_path ~sw Eio.Path.(fs / path) in
      for n=1 to 5 do
        let id=Printf.sprintf "%02d" n in
        let pair : Store.Journal.pair = {
          id="pair-"^id;scope;remote_uidvalidity=None;remote_uid=None;
          local_id=Some ("local-"^id);content_sha256=None;
          content_length=None;internal_date=None;
          common_flags=[];remote_tombstone=None;
          local_tombstone=None;revision=0L} in
        let pair=
          match Store.Journal.put_pair db ~expected_revision:None pair with
          | `Committed pair -> pair
          | `Stale_revision -> Alcotest.fail "new pair stale" in
        let conflict : Store.Journal.conflict = {
          id="conflict-"^id;pair_id=pair.id;kind=Identity_conflict;
          evidence="test";pair_revision=pair.revision;resolved=false} in
        Store.Journal.record_conflict db conflict
      done;
      Store.Journal.resolve_conflict db ~id:"conflict-03");
    let legacy_v7=Sqlite3.db_open path in
    List.iter (fun name ->
      Alcotest.(check bool) (name ^ " dropped") true
        (Sqlite3.exec legacy_v7 ("DROP INDEX " ^ name)=Sqlite3.Rc.OK))
      ["sync_operations_scope_id";"sync_operations_pair_id"];
    ignore (Sqlite3.db_close legacy_v7 : bool);
    let before=Digest.file path in
    Eio.Switch.run (fun sw ->
      let db=Store.open_readonly ~sw Eio.Path.(fs / path) in
      let page ?after ()=Store.Journal.open_conflicts_page db ~scope ?after
        ~limit:2 () |> List.map (fun (c:Store.Journal.conflict) -> c.id) in
      Alcotest.(check (list string)) "first conflict page"
        ["conflict-01";"conflict-02"] (page ());
      Alcotest.(check (list string)) "second conflict page skips resolved"
        ["conflict-04";"conflict-05"]
        (page ~after:"conflict-02" ());
      Alcotest.(check (list string)) "last conflict page empty" []
        (page ~after:"conflict-05" ());
      List.iter (fun limit ->
        try ignore (Store.Journal.open_conflicts_page db ~scope ~limit ());
          Alcotest.fail "bad conflict page limit accepted"
        with Invalid_argument _ -> ()) [0;10_001];
      (try Store.Journal.resolve_conflict db ~id:"conflict-01";
           Alcotest.fail "read-only connection accepted write"
       with Sqlite3.SqliteError _ | Sqlite3.Error _ -> ()));
    Alcotest.(check string) "database unchanged by inspection"
      before (Digest.file path);
    Alcotest.(check bool) "missing database remains absent" false
      (Sys.file_exists missing);
    (try Eio.Switch.run (fun sw ->
       ignore (Store.open_readonly ~sw Eio.Path.(fs / missing)));
       Alcotest.fail "missing database opened read-only"
     with Eio.Exn.Io _ | Sqlite3.Error _ -> ());
    Alcotest.(check bool) "missing database not created" false
      (Sys.file_exists missing);
    let old_db=Sqlite3.db_open old in
    ignore (Sqlite3.exec old_db "PRAGMA user_version=6" : Sqlite3.Rc.t);
    ignore (Sqlite3.db_close old_db : bool);
    let old_before=Digest.file old in
    (try Eio.Switch.run (fun sw ->
       ignore (Store.open_readonly ~sw Eio.Path.(fs / old)));
       Alcotest.fail "older schema opened read-only"
     with Failure _ -> ());
    Alcotest.(check string) "older schema not migrated" old_before
      (Digest.file old);
    let malformed_db=Sqlite3.db_open malformed in
    ignore (Sqlite3.exec malformed_db "PRAGMA user_version=7" : Sqlite3.Rc.t);
    ignore (Sqlite3.db_close malformed_db : bool);
    let malformed_before=Digest.file malformed in
    (try Eio.Switch.run (fun sw ->
       ignore (Store.open_readonly ~sw Eio.Path.(fs / malformed)));
       Alcotest.fail "malformed v7 schema opened read-only"
     with Failure _ -> ());
    Alcotest.(check string) "malformed schema not modified"
      malformed_before (Digest.file malformed));
  ()

let killed_stage_action cursor =
  let selected : M.selected = {
    uidvalidity=epoch 42L;uidnext=1002L;
    highestmodseq=Some (modseq 73L);nomodseq=false} in
  ok (M.plan cursor ~stage_id:"killed-stage" selected)

let stage_crash_child path =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let db=Store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
  let cursor=Store.load_cursor db ~scope in
  let action=killed_stage_action cursor in
  Store.begin_stage db ~cursor ~action;
  Store.stage_rows db ~stage_id:action.id ~first:1L ~last:1000L
    (List.init 1000 (fun n -> row (Int64.of_int (n+1)) []));
  Unix._exit 78

let test_stage_process_crash env =
  let path=Filename.temp_file "imap-stage-killed-" ".db" in
  let cleanup ()=List.iter (fun p ->
    try Sys.remove p with Sys_error _ -> ())
    [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup @@ fun () ->
  let executable=if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name in
  let pid=Unix.create_process executable
    [|executable;"--stage-crash-child";path|]
    Unix.stdin Unix.stdout Unix.stderr in
  let _,status=Unix.waitpid [] pid in
  Alcotest.(check bool) "child exited after stage fsync" true
    (status=Unix.WEXITED 78);
  Eio.Switch.run @@ fun sw ->
  let db=Store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
  let cursor=Store.load_cursor db ~scope in
  Alcotest.(check int64) "incomplete stage did not publish cursor" 0L
    cursor.revision;
  Alcotest.(check (list string)) "stage survived process exit"
    ["killed-stage"] (Store.abandoned_stages db);
  let action=killed_stage_action cursor in
  (try ignore (Store.publish_stage db ~cursor ~action
    ~explicit_highestmodseq:(Some (modseq 73L)) ~nomodseq:false);
    Alcotest.fail "incomplete killed stage was published"
   with Invalid_argument _ -> ());
  Store.discard_stage db ~stage_id:action.id;
  Alcotest.(check (list string)) "abandoned stage discarded" []
    (Store.abandoned_stages db)

let test_seeded_stage_modseq_and_membership env =
  let path=Filename.temp_file "imap-stage-seed-" ".db" in
  let cleanup ()=List.iter (fun p ->
    try Sys.remove p with Sys_error _ -> ())
    [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let db=Store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
  let initial=Store.load db ~scope in
  let first=transition initial.cursor initial.snapshot ~stage:"seed-base"
    ~epoch_value:5L
    [row 1L [flag "\\Seen"];row 2L [];row 3L []] in
  Alcotest.(check bool) "seed base committed" true
    (Store.publish db first=`Committed);
  let cursor=Store.load_cursor db ~scope in
  let selected : M.selected = {
    uidvalidity=epoch 5L;uidnext=5L;
    highestmodseq=Some (modseq 18L);nomodseq=false} in
  let action=ok (M.plan cursor ~stage_id:"seed-delta" selected) in
  Store.begin_stage db ~cursor ~action;
  Alcotest.(check bool) "published rows seeded" true
    (Store.seed_stage_from_published db ~cursor ~action=`Seeded);
  let delta raw_uid n flags : M.row =
    {uid=uid raw_uid;modseq=Some (modseq n);flags} in
  Store.stage_rows db ~stage_id:action.id ~first:1L ~last:4L
    ~preserve_newer:true
    [delta 1L 16L [];delta 2L 18L [flag "\\Flagged"]];
  Store.stage_membership db ~stage_id:action.id ~first:1L ~last:4L
    [uid 1L;uid 2L];
  (match Store.publish_stage db ~cursor ~action
    ~explicit_highestmodseq:(Some (modseq 18L)) ~nomodseq:false with
   | `Committed _ -> ()
   | `Stale_revision -> Alcotest.fail "seeded stage was stale");
  let snapshot=Option.get (Store.load db ~scope).snapshot in
  let rows=M.rows snapshot in
  Alcotest.(check (list int64)) "SEARCH pruned vanished UID" [1L;2L]
    (uids snapshot);
  let row1=List.hd rows and row2=List.nth rows 1 in
  Alcotest.(check (list string)) "older MODSEQ did not replace flags"
    ["\\Seen"] (List.map Mail_flag.Imap_flag.to_wire row1.flags);
  Alcotest.(check int64) "older MODSEQ did not replace checkpoint" 17L
    (Imap.Modseq.to_int64 (Option.get row1.modseq));
  Alcotest.(check (list string)) "newer MODSEQ updated flags"
    ["\\Flagged"] (List.map Mail_flag.Imap_flag.to_wire row2.flags)

let seed_crash_action cursor =
  let selected : M.selected = {
    uidvalidity=epoch 5L;uidnext=5L;
    highestmodseq=Some (modseq 18L);nomodseq=false} in
  ok (M.plan cursor ~stage_id:"seed-crash" selected)

let seed_crash_child path =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let db=Store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
  let cursor=Store.load_cursor db ~scope in
  let action=seed_crash_action cursor in
  Store.begin_stage db ~cursor ~action;
  (match Store.seed_stage_from_published db ~cursor ~action with
   | `Seeded -> ()
   | `Stale_revision -> Alcotest.fail "seed crash stage was stale");
  let changed : M.row = {
    uid=uid 1L;modseq=Some (modseq 18L);flags=[flag "\\Flagged"]} in
  Store.stage_rows db ~stage_id:action.id ~first:1L ~last:4L
    ~preserve_newer:true [changed];
  Unix._exit 79

let test_seed_stage_process_crash env =
  let path=Filename.temp_file "imap-stage-seed-killed-" ".db" in
  let cleanup ()=List.iter (fun p ->
    try Sys.remove p with Sys_error _ -> ())
    [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup @@ fun () ->
  Eio.Switch.run (fun sw ->
    let db=Store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
    let initial=Store.load db ~scope in
    let first=transition initial.cursor initial.snapshot
      ~stage:"seed-crash-base" ~epoch_value:5L
      [row 1L [flag "\\Seen"]] in
    Alcotest.(check bool) "base published before seed crash" true
      (Store.publish db first=`Committed));
  let executable=if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name in
  let pid=Unix.create_process executable
    [|executable;"--seed-crash-child";path|]
    Unix.stdin Unix.stdout Unix.stderr in
  let _,status=Unix.waitpid [] pid in
  Alcotest.(check bool) "child exited after seeded delta fsync" true
    (status=Unix.WEXITED 79);
  Eio.Switch.run @@ fun sw ->
  let db=Store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
  let cursor=Store.load_cursor db ~scope in
  Alcotest.(check int64) "seeded crash retained old revision" 1L
    cursor.revision;
  let snapshot=Option.get (Store.load db ~scope).snapshot in
  Alcotest.(check (list string)) "seeded crash retained old flags"
    ["\\Seen"] (List.map Mail_flag.Imap_flag.to_wire
      (List.hd (M.rows snapshot)).flags);
  Alcotest.(check (list string)) "seeded stage is inert" ["seed-crash"]
    (Store.abandoned_stages db);
  let action=seed_crash_action cursor in
  (try ignore (Store.publish_stage db ~cursor ~action
    ~explicit_highestmodseq:(Some (modseq 18L)) ~nomodseq:false);
    Alcotest.fail "seeded stage without SEARCH was published"
   with Invalid_argument _ -> ());
  Store.discard_stage db ~stage_id:action.id;
  Alcotest.(check (list string)) "seeded crash stage discarded" []
    (Store.abandoned_stages db)

let () =
  if Array.length Sys.argv=3 && Sys.argv.(1)="--stage-crash-child" then
    stage_crash_child Sys.argv.(2)
  else if Array.length Sys.argv=3 &&
    Sys.argv.(1)="--seed-crash-child" then
    seed_crash_child Sys.argv.(2)
  else
  Eio_main.run @@ fun env ->
  Alcotest.run "IMAP SQLite store" ["durability", [
    Alcotest.test_case "OBJECTID+ identity survives restart" `Quick
      (fun () -> test_object_identity env);
    Alcotest.test_case "FLAGS operator settlement is atomic" `Quick
      (fun () -> test_flag_settlement_transaction env);
    Alcotest.test_case "reopen, CAS, epoch and intent" `Quick
      (fun () -> test_reopen env);
    Alcotest.test_case "blob fsync, integrity, references and orphans" `Quick
      (fun () -> test_blobs env);
    Alcotest.test_case "paged missing blobs, CAS and epoch" `Quick
      (fun () -> test_missing_blob_pages env);
    Alcotest.test_case "v1 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:1);
    Alcotest.test_case "v2 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:2);
    Alcotest.test_case "v3 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:3);
    Alcotest.test_case "v4 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:4);
    Alcotest.test_case "v5 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:5);
    Alcotest.test_case "v6 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:6);
    Alcotest.test_case "v7 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:7);
    Alcotest.test_case "v8 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:8);
    Alcotest.test_case "v9 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:9);
    Alcotest.test_case "v10 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:10);
    Alcotest.test_case "v11 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:11);
    Alcotest.test_case "v12 to v13 schema migration" `Quick
      (fun () -> test_schema_upgrade env ~from_version:12);
    Alcotest.test_case "sync occurrence journal and restart" `Quick
      (fun () -> test_sync_journal env);
    Alcotest.test_case "active operation pages and mixed states" `Quick
      (fun () -> test_active_operation_pages env);
    Alcotest.test_case "read-only inspection and conflict pages" `Quick
      (fun () -> test_readonly_and_conflict_pages env);
    Alcotest.test_case "process exit during staged scan" `Quick
      (fun () -> test_stage_process_crash env);
    Alcotest.test_case "seeded stage preserves newer MODSEQ" `Quick
      (fun () -> test_seeded_stage_modseq_and_membership env);
    Alcotest.test_case "process exit during seeded stage" `Quick
      (fun () -> test_seed_stage_process_crash env);
    Alcotest.test_case "disk stage, restart, >100k rows, CAS" `Slow
      (fun () -> test_disk_stage env);
    Alcotest.test_case "staged expunge prunes blob reference" `Quick
      (fun () -> test_stage_blob_refs env)]]
