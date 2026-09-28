(* Compile-time probes of the kind and mode claims in the store facade.
   Each abbreviation in [Kinds] compiles only when its kind holds. Each
   probe is a closure bound at portable mode, as in [let (f @ portable) =
   fun () -> ...]. [records] captures module-level records and reads them,
   so it compiles only when their types cross portability and contention.
   [lookup] captures an open store and calls store functions, so it
   compiles only when [Imap_store.t] crosses both and the functions are
   portable. It runs here and in a second domain. *)

module J = Imap_store.Journal

module Kinds = struct
  type store : value mod portable contended = Imap_store.t
  type object_identity : immutable_data = Imap_store.object_identity
  type staged_receipt : immutable_data = Imap_store.staged_receipt
  type tombstone_reason : immutable_data = J.tombstone_reason
  type tombstone : immutable_data = J.tombstone
  type pair : immutable_data = J.pair
  type conflict_kind : immutable_data = J.conflict_kind
  type conflict : immutable_data = J.conflict
  type operation_kind : immutable_data = J.operation_kind
  type operation_state : immutable_data = J.operation_state
  type append : immutable_data = J.append
  type operation : immutable_data = J.operation
  type blob : immutable_data = Imap_store.Blob.blob
end

let get = function Ok x -> x | Error e -> failwith e

let scope : Imap.Mirror.scope = {
  endpoint = "imap.example"; account = "alice"; mailbox_key = "inbox";
  raw_name = "INBOX"; encoding = Imap.Mailbox_name.Rev1; mailbox_id = None }
let uid = get (Imap.Uid.of_int 7)
let identity : Imap_store.object_identity =
  { account_id = "a1"; mailbox_id = "m1" }
let receipt : Imap_store.staged_receipt =
  { cursor = Imap.Mirror.initial scope; row_count = 3L }
let tombstone : J.tombstone =
  { reason = Expunge_receipt; evidence = "op-1"; generation = Some 2L }
let pair : J.pair = {
  id = "p1"; scope; remote_uidvalidity = Some (get (Imap.Uidvalidity.of_int 1));
  remote_uid = Some uid; local_id = None; content_sha256 = None;
  content_length = None; internal_date = None;
  common_flags = [ get (Mail_flag.Imap_flag.of_wire "\\Seen") ];
  remote_tombstone = Some tombstone; local_tombstone = None; revision = 4L }
let conflict : J.conflict = {
  id = "c1"; pair_id = "p1"; kind = Flag_conflict; evidence = "e";
  pair_revision = 4L; resolved = false }
let operation : J.operation = {
  id = "o1"; pair_id = Some "p1"; local_id = None; scope; kind = Append;
  state = Sent; source_uidvalidity = None; source_uid = None;
  destination = Some scope; destination_uidvalidity = None;
  blob_sha256 = None; blob_length = Some 10L; desired_flags = None;
  internal_date = None;
  append = Some { message_id = "<m@x>"; spool_ref = "s";
                  pre_send_frontier = 6L };
  receipt = None; receipt_uidvalidity = None; receipt_uid = None }

let (records @ portable) = fun () ->
  let frontier = match operation.append with
    | Some a -> a.pre_send_frontier | None -> 0L in
  let reason = match pair.remote_tombstone with
    | Some { reason = Expunge_receipt; _ } -> "expunge" | _ -> "other" in
  identity.account_id, receipt.row_count, reason,
  Option.map Imap.Uid.to_int pair.remote_uid,
  (match conflict.kind with Flag_conflict -> "flag" | _ -> "other"),
  (match operation.kind, operation.state with
   | Append, Sent -> "append sent" | _ -> "other"), frontier

let test_records () =
  let account, rows, reason, remote_uid, conflict, operation, frontier =
    records () in
  Alcotest.(check string) "identity" "a1" account;
  Alcotest.(check int64) "receipt" 3L rows;
  Alcotest.(check string) "tombstone" "expunge" reason;
  Alcotest.(check (option int)) "pair" (Some 7) remote_uid;
  Alcotest.(check string) "conflict" "flag" conflict;
  Alcotest.(check string) "operation" "append sent" operation;
  Alcotest.(check int64) "append" 6L frontier

let shared_store env store =
  let created = match J.put_pair store ~expected_revision:None
      { pair with remote_tombstone = None; revision = 0L } with
    | `Committed p -> p
    | `Stale_revision -> failwith "pair not created" in
  let (lookup @ portable) = fun () ->
    (Imap_store.load_cursor store ~scope).revision,
    J.find_pair store ~id:"p1" in
  let here = lookup () in
  let there =
    Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) lookup in
  Alcotest.(check bool) "same in both domains" true (here = there);
  Alcotest.(check int64) "initial revision" 0L (fst here);
  Alcotest.(check bool) "pair found" true (snd here = Some created)

let test_store () =
  Eio_main.run @@ fun env ->
  let path = Filename.temp_file "imap-store-modes-" ".db" in
  Fun.protect ~finally:(fun () -> List.iter (fun path ->
      try Sys.remove path with Sys_error _ -> ())
      [ path; path ^ "-wal"; path ^ "-shm" ])
    (fun () -> Eio.Switch.run @@ fun sw ->
      shared_store env
        (Imap_store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path)))

let () =
  Alcotest.run "Imap_store kinds and modes" [
    "portable", [ Alcotest.test_case "records" `Quick test_records;
                  Alcotest.test_case "store" `Quick test_store ] ]
