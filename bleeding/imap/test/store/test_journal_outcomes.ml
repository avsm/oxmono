(* Directed checks for journal outcomes: flag-set equality, stale pairs in
   operator repairs, tombstone replacement, caller errors and the indexed
   open-conflict page. *)
module J = Imap_store.Journal
module P = Imap.Proto

let value = function Ok x -> x | Error _ -> failwith "invalid fixture"
let check condition message = if not condition then failwith message
let uid n = value (P.Uid.of_int64 n)
let epoch n = value (P.Uidvalidity.of_int64 n)
let flag x = value (Mail_flag.Imap_flag.of_wire x)
let scope : Imap.Mirror.scope = {
  endpoint="imap.example"; account="alice"; mailbox_key="inbox";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1; mailbox_id=None }
let absence : J.tombstone =
  {reason=Local_absence; evidence="scan"; generation=None}

let pair id : J.pair = {
  id; scope; remote_uidvalidity=Some (epoch 7L);
  remote_uid=Some (uid (Int64.of_int (Hashtbl.hash id land 0xffff + 1)));
  local_id=Some ("local-" ^ id); content_sha256=Some (String.make 64 'a');
  content_length=Some 9L; internal_date=None;
  common_flags=[flag "\\Seen"; flag "\\Flagged"; flag "Custom"];
  remote_tombstone=None; local_tombstone=None; revision=0L }

let delete (p:J.pair) id desired_flags : J.operation = {
  id; pair_id=Some p.id; local_id=p.local_id; scope; kind=Delete;
  state=Prepared; source_uidvalidity=p.remote_uidvalidity;
  source_uid=p.remote_uid; destination=None; destination_uidvalidity=None;
  blob_sha256=p.content_sha256; blob_length=p.content_length;
  desired_flags; receipt=None; receipt_uidvalidity=None; receipt_uid=None }

let committed = function
  | `Committed p -> p
  | `Stale_revision -> failwith "unexpected stale pair"

let raises_from who f =
  match f () with
  | exception Invalid_argument message ->
    check (String.starts_with ~prefix:who message)
      (Printf.sprintf "%s reported as %S" who message)
  | _ -> failwith (who ^ " accepted a caller error")

(* The DELETE preimage is spelled in another order and keyword case than
   the pair, and a second DELETE carries no flag preimage at all. *)
let delete_repairs db =
  let p=committed (J.put_pair db ~expected_revision:None
    {(pair "delete") with local_tombstone=Some absence}) in
  let reordered=delete p "delete-reordered"
    (Some [flag "custom"; flag "\\Flagged"; flag "\\Seen"]) in
  J.prepare_operation db reordered;
  J.mark_sent db ~id:reordered.id;
  let stale={p with revision=Int64.pred p.revision} in
  check (J.reject_unchanged_delete_operation db ~id:reordered.id stale
    ~evidence:"verified"=`Stale_revision) "stale pair not reported stale";
  check (J.attest_targeted_expunge db ~id:reordered.id stale
    ~evidence:"verified"=`Stale_revision) "stale attestation not stale";
  check (J.reject_unchanged_delete_operation db ~id:reordered.id p
    ~evidence:"verified"=`Rejected) "reordered flag preimage refused";
  let unflagged=delete p "delete-unflagged" None in
  J.prepare_operation db unflagged;
  J.mark_ambiguous db ~id:unflagged.id ~reason:(String.make 4000 'r');
  raises_from "Imap_store.Journal.attest_targeted_expunge" (fun () ->
    J.attest_targeted_expunge db ~id:unflagged.id p
      ~evidence:(String.make 200 'e'));
  check ((Option.get (J.find_operation db ~id:unflagged.id)).receipt
    =Some (String.make 4000 'r')) "overflowing attestation changed receipt";
  let settle_wrong_kind=J.settle_flag_operation db ~id:unflagged.id p
    ~flags:[] ~evidence:"verified" in
  check (settle_wrong_kind=`Invalid_operation) "wrong kind not invalid";
  J.reject_operation db ~id:unflagged.id ~receipt:"cleared";
  let bare=delete p "delete-bare" None in
  J.prepare_operation db bare;
  J.mark_sent db ~id:bare.id;
  check (J.attest_targeted_expunge db ~id:bare.id p ~evidence:"verified"
    =`Attested) "DELETE without flag preimage refused"

let settle_messages db =
  let p=committed (J.put_pair db ~expected_revision:None (pair "settle")) in
  let op : J.operation = {(delete p "settle-op" (Some p.common_flags))
    with kind=Flags} in
  J.prepare_operation ~local_flags:p.common_flags db op;
  J.mark_sent db ~id:op.id;
  raises_from "Imap_store.Journal.settle_flag_operation" (fun () ->
    J.settle_flag_operation db ~id:op.id p ~flags:[flag "\\Recent"]
      ~evidence:"verified")

let tombstones db =
  let p=committed (J.put_pair db ~expected_revision:None
    {(pair "retained") with local_tombstone=Some
      {reason=Retention; evidence="policy"; generation=None}}) in
  let put candidate=J.put_pair db ~expected_revision:(Some p.revision)
    candidate in
  raises_from "Imap_store.Journal.put_pair" (fun () ->
    put {p with local_tombstone=Some absence});
  raises_from "Imap_store.Journal.put_pair" (fun () ->
    put {p with local_tombstone=None});
  raises_from "Imap_store.Journal.put_pair" (fun () ->
    put {p with scope={scope with raw_name="Renamed"}});
  let p=committed (put {p with local_tombstone=Some
    {reason=Explicit_delete; evidence="operator"; generation=None}}) in
  check (p.local_tombstone<>None) "escalation lost the tombstone";
  let renewed=committed (J.put_pair db ~expected_revision:None
    {(pair "renewed") with local_tombstone=Some absence}) in
  ignore (committed (J.put_pair db ~expected_revision:(Some renewed.revision)
    {renewed with local_tombstone=Some {absence with evidence="again"}}))

let caller_errors db =
  let local_only={(pair "local-only") with
    remote_uidvalidity=None; remote_uid=None} in
  let local_only=committed (J.put_pair db ~expected_revision:None
    local_only) in
  raises_from "Imap_store.Journal.note_presence" (fun () ->
    J.note_presence db ~pair:local_only ~side:`Remote ~generation:0L);
  let p=committed (J.put_pair db ~expected_revision:None (pair "commit")) in
  let op : J.operation = {(delete p "commit-op" (Some p.common_flags))
    with kind=Flags} in
  J.prepare_operation db op;
  J.mark_sent db ~id:op.id;
  J.observe_operation db ~id:op.id ~receipt:"seen"
    ~destination_uidvalidity:None ~destination_uid:None;
  raises_from "Imap_store.Journal.commit_operation_with_pair" (fun () ->
    J.commit_operation_with_pair db ~id:op.id ~expected_pair_revision:None p);
  let keyword={p with common_flags=[flag "\\seen"; flag "\\FLAGGED";
    flag "CUSTOM"]} in
  ignore (committed (J.commit_operation_with_pair db ~id:op.id
    ~expected_pair_revision:(Some p.revision) keyword))

let conflicts db path =
  let p=committed (J.put_pair db ~expected_revision:None (pair "conflict")) in
  List.iter (fun id -> ignore (J.ensure_open_conflict db ~pair:p
    ~kind:(if id="c1" then Flag_conflict else Content_conflict) ~id
    ~evidence:"seen")) ["c1";"c2"];
  check (J.resolve_open_conflicts db ~pair:p ~kind:Flag_conflict
    =`Resolved 1) "resolved count";
  check (J.resolve_open_conflicts db ~pair:p ~kind:Flag_conflict
    =`Resolved 0) "second resolution count";
  let raw=Sqlite3.db_open ~mode:`READONLY path in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close raw)) (fun () ->
    let plan=ref [] in
    Sqlite3.Rc.check (Sqlite3.exec raw ~cb:(fun row _ ->
      Array.iter (Option.iter (fun cell -> plan := cell :: !plan)) row)
      "EXPLAIN QUERY PLAN SELECT c.id FROM sync_conflicts AS c CROSS JOIN \
       sync_pairs AS p ON p.id=c.pair_id WHERE c.resolved=0 \
       AND p.endpoint='e' AND p.account='a' AND p.mailbox_key='k' \
       AND c.id>'x' ORDER BY c.id LIMIT 10");
    check (List.exists (fun line ->
      List.mem "sync_conflicts_open_id" (String.split_on_char ' ' line))
      !plan) "open conflict page does not use sync_conflicts_open_id")

let () =
  Eio_main.run (fun env ->
    let path=Filename.temp_file "imap-journal-outcomes-" ".db" in
    Fun.protect ~finally:(fun () -> List.iter (fun path ->
      try Sys.remove path with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"])
      (fun () -> Eio.Switch.run (fun sw ->
        let db=Imap_store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
        delete_repairs db;
        settle_messages db;
        tombstones db;
        caller_errors db;
        conflicts db path)))
