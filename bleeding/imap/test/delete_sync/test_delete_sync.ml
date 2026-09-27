module M = Imap.Mirror
module P = Imap.Proto
module J = Imap_store.Journal

let ok = function Ok x -> x | Error _ -> Alcotest.fail "unexpected error"
let uid n = ok (P.Uid.of_int64 n)
let epoch n = ok (P.Uidvalidity.of_int64 n)
let scope : M.scope = {
  endpoint="delete.test";account="alice";mailbox_key="INBOX";
  raw_name="INBOX";encoding=Imap.Mailbox_name.Rev1;mailbox_id=None;
}
let body="From: alice@example.test\r\n\r\nExact bytes\r\n"
let length=Int64.of_int (String.length body)
let digest=Digestif.SHA256.(to_hex (digest_string body))

let rec remove_tree path =
  if Sys.is_directory path then (
    Sys.readdir path |> Array.iter (fun name ->
      remove_tree (Filename.concat path name));
    Unix.rmdir path)
  else Sys.remove path

let with_fixture f =
  let root=Filename.temp_file "imap-delete-sync-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove_tree root) @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let store=Imap_store.open_path ~sw Eio.Path.(fs / root / "state.db") in
  let maildir=Imap_maildir.open_dir Eio.Path.(fs / root / "maildir") in
  f store maildir

let publish store ~stage rows =
  let loaded=Imap_store.load store ~scope in
  let selected : M.selected = {
    uidvalidity=epoch 11L;uidnext=2L;
    highestmodseq=None;nomodseq=true;
  } in
  let action=ok (M.plan loaded.cursor ~stage_id:stage selected) in
  let completed : M.completed = {
    action_id=action.id;uidvalidity=action.uidvalidity;
    covered_upper=action.upper_uid;inventory_complete=true;
    commands_complete=true;rows;explicit_highestmodseq=None;
    nomodseq=true;
  } in
  let staged=ok (M.complete loaded.cursor action completed) in
  let transition=ok (M.publish loaded.cursor ~published:loaded.snapshot staged) in
  Alcotest.(check bool) "published" true
    (Imap_store.publish store transition=`Committed);
  (Imap_store.load store ~scope).cursor

let pair ~id ~local_id ~remote_tombstone ~local_tombstone : J.pair = {
  id;scope;remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
  local_id=Some local_id;content_sha256=Some digest;
  content_length=Some length;internal_date=None;common_flags=[];
  remote_tombstone;local_tombstone;revision=0L;
}

let put_pair store pair =
  match J.put_pair store ~expected_revision:None pair with
  | `Committed pair -> pair
  | `Stale_revision -> Alcotest.fail "pair create was stale"

let operation (pair:J.pair) ~id ~kind : J.operation = {
  id;pair_id=Some pair.J.id;local_id=pair.local_id;scope;
  kind;state=J.Prepared;
  source_uidvalidity=pair.remote_uidvalidity;
  source_uid=pair.remote_uid;destination=None;
  destination_uidvalidity=None;blob_sha256=pair.content_sha256;
  blob_length=pair.content_length;desired_flags=Some [];
  receipt=None;receipt_uidvalidity=None;receipt_uid=None;
}

let outcome = function
  | Ok (Imap_sync.Deletion.Deleted pair) -> pair
  | Ok _ -> Alcotest.fail "expected completed deletion"
  | Error e -> Alcotest.fail (Format.asprintf "%a"
      Imap_sync.Deletion.pp_error e)

let test_local_sent_recovery_after_new_scan () =
  with_fixture @@ fun store maildir ->
  let cursor=publish store ~stage:"empty-1" [] in
  let local=Imap_maildir.append maildir
    ~source:(Eio.Flow.string_source body) ~length ~flags:[] () in
  let remote_tombstone=Some {
    J.reason=J.Inventory_absence;
    evidence=Option.get cursor.inventory_ref;
    generation=Some cursor.generation;
  } in
  let pair=put_pair store (pair ~id:"pair-local" ~local_id:local.id
    ~remote_tombstone ~local_tombstone:None) in
  let op=operation pair ~id:"op-local" ~kind:J.Local_delete in
  J.prepare_operation store op;
  J.mark_sent store ~id:op.id;
  Imap_maildir.remove maildir local;
  let cursor=publish store ~stage:"empty-2" [] in
  let op=Option.get (J.find_operation store ~id:op.id) in
  Imap_maildir.with_inventory_pages maildir (fun local_inventory ->
    let pair=outcome (Imap_sync.Deletion.recover_operation ~store ~maildir
      ~cursor ~local_inventory ~operation:op ()) in
    Alcotest.(check bool) "local tombstone" true
      (match pair.local_tombstone with
       | Some {reason=J.Explicit_delete;_} -> true | _ -> false);
    Alcotest.(check bool) "remote absence retained" true
      (pair.remote_tombstone=remote_tombstone);
    Alcotest.(check bool) "operation committed" true
      ((Option.get (J.find_operation store ~id:op.id)).state=J.Committed))

let test_local_sent_without_unlink_is_held () =
  with_fixture @@ fun store maildir ->
  let cursor=publish store ~stage:"empty-pending" [] in
  let local=Imap_maildir.append maildir
    ~source:(Eio.Flow.string_source body) ~length ~flags:[] () in
  let pair=put_pair store (pair ~id:"pair-local-pending"
    ~local_id:local.id
    ~remote_tombstone:(Some {J.reason=J.Inventory_absence;
      evidence=Option.get cursor.inventory_ref;
      generation=Some cursor.generation}) ~local_tombstone:None) in
  let op=operation pair ~id:"op-local-pending" ~kind:J.Local_delete in
  J.prepare_operation store op;
  J.mark_sent store ~id:op.id;
  Imap_maildir.with_inventory_pages maildir (fun local_inventory ->
    (match Imap_sync.Deletion.recover_operation ~store ~maildir ~cursor
      ~local_inventory ~operation:op () with
     | Error (Imap_sync.Deletion.Pending_operation id) when id=op.id -> ()
     | _ -> Alcotest.fail "uncertain local unlink was replayed"));
  Alcotest.(check bool) "local occurrence remains present" true
    (Option.is_some (Imap_maildir.find maildir ~id:local.id));
  Alcotest.(check bool) "journal remains Sent" true
    ((Option.get (J.find_operation store ~id:op.id)).state=J.Sent)

let test_remote_ambiguous_needs_complete_absence () =
  with_fixture @@ fun store maildir ->
  let row : M.row = {uid=uid 1L;flags=[];modseq=None} in
  let cursor=publish store ~stage:"present" [row] in
  let pair=put_pair store (pair ~id:"pair-remote" ~local_id:"reserved-id"
    ~remote_tombstone:None
    ~local_tombstone:(Some {J.reason=J.Local_absence;
      evidence="local-stage";generation=None})) in
  let op=operation pair ~id:"op-remote" ~kind:J.Delete in
  J.prepare_operation store op;
  J.mark_sent store ~id:op.id;
  J.mark_ambiguous store ~id:op.id;
  let op=Option.get (J.find_operation store ~id:op.id) in
  Imap_maildir.with_inventory_pages maildir (fun local_inventory ->
    Alcotest.(check bool) "still pending while UID present" true
      (match Imap_sync.Deletion.recover_operation ~store ~maildir
        ~cursor ~local_inventory ~operation:op () with
       | Error (Imap_sync.Deletion.Pending_operation _) -> true
       | _ -> false));
  let cursor=publish store ~stage:"absent" [] in
  Imap_maildir.with_inventory_pages maildir (fun local_inventory ->
    let pair=outcome (Imap_sync.Deletion.recover_operation ~store ~maildir
      ~cursor ~local_inventory ~operation:op ()) in
    Alcotest.(check bool) "inventory-proven remote tombstone" true
      (match pair.remote_tombstone with
       | Some {reason=J.Inventory_absence;generation=Some _;_} -> true
       | _ -> false);
    Alcotest.(check bool) "operation committed" true
      ((Option.get (J.find_operation store ~id:op.id)).state=J.Committed))

let test_prepared_never_dispatched () =
  with_fixture @@ fun store maildir ->
  let cursor=publish store ~stage:"prepared" [] in
  let pair=put_pair store (pair ~id:"pair-prepared" ~local_id:"reserved-id"
    ~remote_tombstone:(Some {J.reason=J.Inventory_absence;
      evidence=Option.get cursor.inventory_ref;
      generation=Some cursor.generation})
    ~local_tombstone:None) in
  let op=operation pair ~id:"op-prepared" ~kind:J.Local_delete in
  J.prepare_operation store op;
  Imap_maildir.with_inventory_pages maildir (fun local_inventory ->
    Alcotest.(check bool) "prepared rejected" true
      (match Imap_sync.Deletion.recover_operation ~store ~maildir
        ~cursor ~local_inventory ~operation:op () with
       | Ok Imap_sync.Deletion.Unchanged -> true | _ -> false);
    Alcotest.(check bool) "terminal rejection" true
      ((Option.get (J.find_operation store ~id:op.id)).state=J.Rejected))

let test_prepared_stale_pair_is_rejected () =
  with_fixture @@ fun store maildir ->
  let cursor=publish store ~stage:"stale-prepared" [] in
  let pair=put_pair store (pair ~id:"pair-stale-prepared"
    ~local_id:"reserved-id"
    ~remote_tombstone:(Some {J.reason=J.Inventory_absence;
      evidence=Option.get cursor.inventory_ref;
      generation=Some cursor.generation}) ~local_tombstone:None) in
  let op=operation pair ~id:"op-stale-prepared" ~kind:J.Local_delete in
  J.prepare_operation store op;
  (match J.put_pair store ~expected_revision:(Some pair.revision) pair with
   | `Committed _ -> ()
   | `Stale_revision -> Alcotest.fail "pair advance was stale");
  Imap_maildir.with_inventory_pages maildir (fun local_inventory ->
    (match Imap_sync.Deletion.recover_operation ~store ~maildir ~cursor
      ~local_inventory ~operation:op () with
     | Ok Imap_sync.Deletion.Unchanged -> ()
     | _ -> Alcotest.fail "unsent deletion was blocked by stale pair"));
  Alcotest.(check bool) "unsent operation rejected" true
    ((Option.get (J.find_operation store ~id:op.id)).state=J.Rejected)

let test_prepared_only_rejection_guard () =
  with_fixture @@ fun store _maildir ->
  let cursor=publish store ~stage:"guard" [] in
  let pair=put_pair store (pair ~id:"pair-guard" ~local_id:"reserved-id"
    ~remote_tombstone:(Some {J.reason=J.Inventory_absence;
      evidence=Option.get cursor.inventory_ref;
      generation=Some cursor.generation}) ~local_tombstone:None) in
  let op=operation pair ~id:"op-guard" ~kind:J.Local_delete in
  J.prepare_operation store op;
  J.mark_sent store ~id:op.id;
  (match J.reject_prepared_operation store ~id:op.id
    ~receipt:"incorrectly assumed unsent" with
   | exception Invalid_argument _ -> ()
   | _ -> Alcotest.fail "dispatched operation was rejected as prepared");
  Alcotest.(check bool) "sent operation retained" true
    ((Option.get (J.find_operation store ~id:op.id)).state=J.Sent)

let test_expunge_preflight () =
  let module F = Mail_flag.Imap_flag in
  let seen=F.system F.Seen and deleted=F.system F.Deleted in
  let check label expected after =
    Alcotest.(check bool) label expected
      (Imap_sync.Deletion.expunge_preflight ~before_flags:[seen]
        ~before_modseq:15L after) in
  check "expected new Deleted with later MODSEQ" true
    (Some ([seen;deleted],Some 16L));
  check "no Deleted is unsafe" false (Some ([seen],Some 16L));
  check "concurrent flag edit is unsafe" false
    (Some ([deleted],Some 17L));
  check "unchanged MODSEQ is unsafe for new Deleted" false
    (Some ([seen;deleted],Some 15L));
  check "missing MODSEQ is unsafe" false
    (Some ([seen;deleted],None));
  check "vanished target needs inventory recovery" false None;
  let keyword wire=match F.of_wire wire with
    | Ok flag -> flag | Error message -> Alcotest.fail message in
  Alcotest.(check bool) "server keyword capitalization is not a concurrent edit" true
    (Imap_sync.Deletion.expunge_preflight
      ~before_flags:[keyword "$Label"] ~before_modseq:15L
      (Some ([keyword "$LABEL";deleted],Some 16L)));
  Alcotest.(check bool) "already Deleted may preserve MODSEQ" true
    (Imap_sync.Deletion.expunge_preflight
      ~before_flags:[seen;deleted] ~before_modseq:15L
      (Some ([deleted;seen],Some 15L)))

let () =
  Alcotest.run "imap-delete-sync" [
    "recovery",[
      Alcotest.test_case "local Sent, later scan" `Quick
        test_local_sent_recovery_after_new_scan;
      Alcotest.test_case "local Sent without unlink is held" `Quick
        test_local_sent_without_unlink_is_held;
      Alcotest.test_case "remote Ambiguous requires absence" `Quick
        test_remote_ambiguous_needs_complete_absence;
      Alcotest.test_case "Prepared is not sent" `Quick
        test_prepared_never_dispatched;
      Alcotest.test_case "stale pair cannot wedge Prepared" `Quick
        test_prepared_stale_pair_is_rejected;
      Alcotest.test_case "Prepared-only transition guard" `Quick
        test_prepared_only_rejection_guard;
    ];
    "remote mutation",[
      Alcotest.test_case "pre-EXPUNGE flags and MODSEQ" `Quick
        test_expunge_preflight;
    ];
  ]
