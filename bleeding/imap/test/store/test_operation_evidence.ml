module J = Imap_store.Journal
let value = function Ok x -> x | Error _ -> failwith "invalid fixture"
let uid n = value (Imap.Uid.of_int64 n)
let epoch n = value (Imap.Uidvalidity.of_int64 n)
let flag x = value (Mail_flag.Imap_flag.of_wire x)
let scope : Imap.Mirror.scope = {
  endpoint="imap.example"; account="alice"; mailbox_key="inbox";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1; mailbox_id=None }
let date = value (Imap.Internal_date.of_string "26-Sep-2025 12:34:56 +0230")
let pair : J.pair = {
  id="pair"; scope; remote_uidvalidity=Some (epoch 67L); remote_uid=Some (uid 2L);
  local_id=Some "message"; content_sha256=Some (String.make 64 'a');
  content_length=Some 42L; internal_date=Some date; common_flags=[flag "\\Seen"];
  remote_tombstone=None; local_tombstone=None; revision=0L }
let append : J.operation = {
  id="append"; pair_id=None; local_id=pair.local_id; scope; kind=Append;
  state=Prepared; source_uidvalidity=None; source_uid=None;
  destination=Some scope; destination_uidvalidity=Some (epoch 67L);
  blob_sha256=pair.content_sha256; blob_length=pair.content_length;
  desired_flags=Some pair.common_flags; internal_date=None;
  append=Some {message_id="<append@x>"; spool_ref="spool-append";
    pre_send_frontier=0L};
  receipt=None; receipt_uidvalidity=None; receipt_uid=None }
let invalid label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith (label ^ " accepted")
let observed db (op:J.operation) epoch uid =
  J.mark_sent db ~id:op.id;
  J.observe_operation db ~id:op.id ~receipt:"verified"
    ~destination_uidvalidity:epoch ~destination_uid:uid
let committed = function
  | `Committed pair -> pair | `Stale_revision -> failwith "unexpected stale pair"
let run env =
  let path=Filename.temp_file "imap-operation-evidence-" ".db" in
  Fun.protect ~finally:(fun () -> List.iter (fun path ->
    try Sys.remove path with Sys_error _ -> ()) [path;path^"-wal";path^"-shm"])
    (fun () -> Eio.Switch.run (fun sw ->
      let db=Imap_store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
      J.prepare_operation db append;
      observed db append pair.remote_uidvalidity pair.remote_uid;
      let commit candidate=J.commit_operation_with_pair db ~id:append.id
        ~expected_pair_revision:None candidate in
      List.iter (fun (label,candidate) ->
        invalid label (fun () -> commit candidate);
        if (Option.get (J.find_operation db ~id:append.id)).state<>Observed then
          failwith "rejected evidence changed journal";
        if J.find_pair db ~id:pair.id<>None then failwith "rejected evidence published pair")
        ["wrong UID",{pair with remote_uid=Some (uid 3L)};
         "wrong epoch",{pair with remote_uidvalidity=Some (epoch 68L)};
         "wrong local ID",{pair with local_id=Some "other"};
         "wrong digest",{pair with content_sha256=Some (String.make 64 'b')};
         "wrong length",{pair with content_length=Some 43L};
         "wrong flags",{pair with common_flags=[]};
         "wrong scope",{pair with scope={scope with account="other"}}];
      let mismatch={append with id="receipt-epoch";local_id=Some "epoch-message"} in
      J.prepare_operation db mismatch;
      observed db mismatch (Some (epoch 68L)) (Some (uid 8L));
      invalid "receipt changed expected epoch" (fun () -> J.commit_operation_with_pair db
        ~id:mismatch.id ~expected_pair_revision:None
        {pair with id=mismatch.id;local_id=mismatch.local_id;
          remote_uidvalidity=Some (epoch 68L);remote_uid=Some (uid 8L)});
      let missing={append with id="missing-receipt"} in
      J.prepare_operation db missing;
      observed db missing None None;
      invalid "missing receipt identity" (fun () -> J.commit_operation_with_pair db
        ~id:missing.id ~expected_pair_revision:None {pair with id=missing.id});
      let current=committed (commit pair) in
      let flags : J.operation = {append with id="flags"; pair_id=Some current.id;
        append=None;
        kind=Flags; destination=None; destination_uidvalidity=None;
        source_uidvalidity=current.remote_uidvalidity; source_uid=current.remote_uid;
        blob_sha256=None; blob_length=None; desired_flags=Some [flag "\\Flagged"]} in
      List.iter (fun (label,op) ->
        invalid label (fun () -> J.prepare_operation db op);
        if J.find_operation db ~id:op.J.id<>None then failwith "invalid intent persisted")
        ["source UID",{flags with id="bad-uid";source_uid=Some (uid 9L)};
         "source epoch",{flags with id="bad-epoch";source_uidvalidity=Some (epoch 68L)};
         "missing local ID",{flags with id="bad-local";local_id=None};
         "content mismatch",{flags with id="bad-content";
           blob_sha256=Some (String.make 64 'b');blob_length=Some 42L};
         "incomplete content",{flags with id="bad-length";blob_length=Some 42L};
         "invalid digest",{flags with id="bad-digest";blob_sha256=Some "abc";blob_length=Some 42L}];
      J.prepare_operation db flags;
      observed db flags None None;
      invalid "wrong merged flags" (fun () -> J.commit_operation_with_pair db
        ~id:flags.id ~expected_pair_revision:(Some current.revision) current);
      let next=committed (J.commit_operation_with_pair db ~id:flags.id
        ~expected_pair_revision:(Some current.revision)
        {current with common_flags=Option.get flags.desired_flags}) in
      let deletion={flags with id="delete";kind=J.Delete;
        desired_flags=Some next.common_flags} in
      J.prepare_operation db deletion;
      observed db deletion None None;
      invalid "deletion without tombstone" (fun () -> J.commit_operation_with_pair db
        ~id:deletion.id ~expected_pair_revision:(Some next.revision) next);
      let tombstone : J.tombstone = {reason=Expunge_receipt;evidence="UID EXPUNGE";generation=None} in
      invalid "unrelated local tombstone" (fun () -> J.commit_operation_with_pair db
        ~id:deletion.id ~expected_pair_revision:(Some next.revision)
        {next with remote_tombstone=Some tombstone;
          local_tombstone=Some {tombstone with reason=Explicit_delete}});
      ignore (committed (J.commit_operation_with_pair db ~id:deletion.id
        ~expected_pair_revision:(Some next.revision) {next with remote_tombstone=Some tombstone}));
      let local={append with id="local";kind=J.Local_append;local_id=Some "local";
        append=None;
        destination=None;destination_uidvalidity=None;
        source_uidvalidity=Some (epoch 67L);source_uid=Some (uid 4L)} in
      J.prepare_operation db {local with internal_date=Some date};
      observed db local None None;
      let candidate={pair with id="local";local_id=local.local_id;remote_uid=local.source_uid} in
      invalid "missing source date" (fun () -> J.commit_operation_with_pair db ~id:local.id
        ~expected_pair_revision:None {candidate with internal_date=None});
      let utc=value (Imap.Internal_date.of_string "26-Sep-2025 10:04:56 +0000") in
      ignore (committed (J.commit_operation_with_pair db ~id:local.id
        ~expected_pair_revision:None {candidate with internal_date=Some utc}))))
let () = Eio_main.run run
