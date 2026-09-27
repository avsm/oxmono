(* Stage 100,000 rows in one FETCH and one SEARCH window and publish them
   to a temporary database. *)

module M = Imap.Mirror

let count = 100_000
let ok = function Ok x -> x | Error _ -> failwith "invalid value"
let uid n = ok (Imap.Uid.of_int64 (Int64.of_int n))
let modseq n = ok (Imap.Modseq.of_int64 (Int64.of_int n))
let seen = ok (Mail_flag.Imap_flag.of_wire "\\Seen")

let scope : M.scope = {
  endpoint = "bench.local"; account = "bench"; mailbox_key = "inbox";
  raw_name = "INBOX"; encoding = Imap.Mailbox_name.Rev1; mailbox_id = None }

let rows = List.init count (fun i ->
  { M.uid = uid (i + 1); flags = [ seen ]; modseq = Some (modseq (i + 1)) })
let uids = List.map (fun (r : M.row) -> r.uid) rows

let () =
  let path = Filename.temp_file "imap-bench-store-" ".db" in
  Fun.protect ~finally:(fun () ->
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [ path; path ^ "-wal"; path ^ "-shm" ]) @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let db = Imap_store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
  let cursor = M.initial scope in
  let selected : M.selected = {
    uidvalidity = ok (Imap.Uidvalidity.of_int64 1L);
    uidnext = Int64.of_int (count + 1);
    highestmodseq = Some (modseq count); nomodseq = false } in
  let action = ok (M.plan cursor ~stage_id:"bench" selected) in
  Imap_store.begin_stage db ~cursor ~action;
  let last = Int64.of_int count in
  let published = Bench_measure.run "store stage and publish 100000 rows"
    (fun () ->
      Imap_store.stage_rows db ~stage_id:action.id ~first:1L ~last rows;
      Imap_store.stage_membership db ~stage_id:action.id ~first:1L ~last
        uids;
      Imap_store.publish_stage db ~cursor ~action
        ~explicit_highestmodseq:(Some (modseq count)) ~nomodseq:false) in
  match published with
  | `Committed (r : Imap_store.staged_receipt) when r.row_count = last -> ()
  | `Committed _ -> failwith "row count"
  | `Stale_revision -> failwith "stale"
