(* Cursor publication through the SQLite stage path: inventory
   replacement, epoch changes, anchors, NOMODSEQ, coverage and CAS. *)
module M = Imap.Mirror
module Store = Imap_store

let ok = function Ok x -> x | Error _ -> Alcotest.fail "unexpected error"
let uid n = ok (Imap.Uid.of_int64 n)
let validity n = ok (Imap.Uidvalidity.of_int64 n)
let modseq n = ok (Imap.Modseq.of_int64 n)
let flag s = ok (Mail_flag.Imap_flag.of_wire s)
let row ?(flags=[]) ?modseq:seq n : M.row = {uid=uid n;flags;modseq=seq}
let scope : M.scope = {
  endpoint="imap.example";account="alice";mailbox_key="inbox";
  raw_name="INBOX";encoding=Imap.Mailbox_name.Rev1;mailbox_id=None
}
let selected ?(nomodseq=false) ?highest uidvalidity uidnext : M.selected = {
  uidvalidity=validity uidvalidity;uidnext;
  highestmodseq=Option.map modseq highest;nomodseq
}

let with_store env f =
  let path=Filename.temp_file "imap-stage-publication-" ".db" in
  let cleanup ()=List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
    [path;path^"-wal";path^"-shm"] in
  Fun.protect ~finally:cleanup @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  f (Store.open_path ~sw Eio.Path.(Eio.Stdenv.fs env / path))

(* [fill db action rows] stages [rows] as one FETCH and one SEARCH window
   over the whole planned UID range. *)
let fill db (action:M.action) rows =
  let last=action.upper_uid in
  if last>0L then (
    Store.stage_rows db ~stage_id:action.id ~first:1L ~last rows;
    Store.stage_membership db ~stage_id:action.id ~first:1L ~last
      (List.map (fun (r:M.row) -> r.uid) rows))

let stage db cursor ~stage_id sel rows =
  let action=ok (M.plan cursor ~stage_id sel) in
  Store.begin_stage db ~cursor ~action;
  fill db action rows;
  action

let publish ?explicit ?(nomodseq=false) db cursor action =
  match Store.publish_stage db ~cursor ~action
    ~explicit_highestmodseq:(Option.map modseq explicit) ~nomodseq with
  | `Committed (receipt:Store.staged_receipt) -> receipt
  | `Stale_revision -> Alcotest.fail "fresh stage was stale"

let snapshot db (cursor:M.cursor) =
  match Store.snapshot_page db ~scope:cursor.scope ~cursor ~limit:10_000 ()
  with
  | `Rows rows -> rows
  | `Stale_revision -> Alcotest.fail "snapshot page stale"

let uids rows = List.map (fun (r:M.row) -> Imap.Uid.to_int64 r.uid) rows
let present db cursor n =
  Store.snapshot_contains_uid db ~scope ~cursor ~uid:(uid n)=`Present true

(* [changed before after] is the UIDs of [after] whose durable flags
   differ from the same UID in [before]. *)
let changed before after =
  List.filter_map (fun (now:M.row) ->
    match List.find_opt (fun (old:M.row) -> old.uid=now.uid) before with
    | Some old when not (Mail_flag.Imap_flag.equal_durable old.flags
                           now.flags) -> Some (Imap.Uid.to_int64 now.uid)
    | _ -> None) after

let contains ~needle haystack =
  let n=String.length needle in
  let rec from k = k+n<=String.length haystack &&
    (String.sub haystack k n=needle || from (k+1)) in
  from 0

let rejects what ?(needle="") f =
  match f () with
  | exception Invalid_argument message when contains ~needle message -> ()
  | exception Invalid_argument message -> Alcotest.failf "%s: %s" what message
  | _ -> Alcotest.fail what

let baseline db =
  let cursor=Store.load_cursor db ~scope in
  let action=stage db cursor ~stage_id:"first"
    (selected ~highest:98L 5L 4L) [row 1L;row 2L;row 3L] in
  (publish ~explicit:98L db cursor action).cursor

let test_same_count_different_uids db =
  let first=baseline db in
  let action=stage db first ~stage_id:"second"
    (selected ~highest:100L 5L 5L) [row 1L;row 3L;row 4L] in
  let receipt=publish ~explicit:100L db first action in
  let next=receipt.cursor in
  Alcotest.(check (list int64)) "published inventory" [1L;3L;4L]
    (uids (snapshot db next));
  Alcotest.(check bool) "new UID" true (present db next 4L);
  Alcotest.(check bool) "expunged UID" false (present db next 2L);
  Alcotest.(check int64) "same count" 3L receipt.row_count

let test_epoch_change db =
  let first=baseline db in
  let action=stage db first ~stage_id:"new-epoch"
    (selected ~highest:1L 6L 2L) [row 1L] in
  Alcotest.(check bool) "restart reason" true
    (action.restart=Some Uidvalidity_changed);
  let next=(publish ~explicit:1L db first action).cursor in
  Alcotest.(check bool) "invalidated" true
    (next.uidvalidity=Some (validity 6L));
  Alcotest.(check (list int64)) "new epoch inventory" [1L]
    (uids (snapshot db next));
  (* The old epoch's rows are retained, not deleted, until dropped. *)
  Alcotest.(check bool) "no deletion propagation" true
    (Store.forget_epochs db ~scope ~cursor:next=`Dropped 1)

let test_anchor_and_interruption db =
  let first=baseline db in
  let sel=selected ~highest:104L 5L 5L in
  let rows=[row ~modseq:(modseq 103L) 1L;row ~modseq:(modseq 104L) 2L;
            row 3L;row 4L] in
  let action=stage db first ~stage_id:"catch-up" sel rows in
  (* The staging write can be interrupted. No cursor changes until publish. *)
  Alcotest.(check (option int64)) "old durable anchor" (Some 98L)
    (Option.map Imap.Modseq.to_int64 (Store.load_cursor db ~scope).anchor);
  ignore (publish ~explicit:99L db first action);
  let next=Store.load_cursor db ~scope in
  Alcotest.(check (option int64)) "explicit lower anchor wins" (Some 99L)
    (Option.map Imap.Modseq.to_int64 next.anchor);
  Alcotest.(check int64) "revision advanced at publication" 2L
    next.revision;
  let replay=stage db first ~stage_id:"catch-up-replay" sel rows in
  (match Store.publish_stage db ~cursor:first ~action:replay
     ~explicit_highestmodseq:(Some (modseq 99L)) ~nomodseq:false with
   | `Stale_revision -> ()
   | `Committed _ -> Alcotest.fail "stale stage was accepted")

let test_incomplete_and_regression db =
  let first=baseline db in
  let sel=selected ~highest:104L 5L 5L in
  let publish_rejects what ~needle ~explicit (action:M.action) =
    rejects what ~needle (fun () ->
      Store.publish_stage db ~cursor:first ~action
        ~explicit_highestmodseq:(Some (modseq explicit)) ~nomodseq:false) in
  let fetch_gap=ok (M.plan first ~stage_id:"incomplete-fetch" sel) in
  Store.begin_stage db ~cursor:first ~action:fetch_gap;
  Store.stage_rows db ~stage_id:fetch_gap.id ~first:1L ~last:1L [row 1L];
  Store.stage_membership db ~stage_id:fetch_gap.id ~first:1L ~last:1L
    [uid 1L];
  publish_rejects "partial FETCH inventory accepted"
    ~needle:"incomplete range coverage" ~explicit:104L fetch_gap;
  let search_gap=ok (M.plan first ~stage_id:"incomplete-search" sel) in
  Store.begin_stage db ~cursor:first ~action:search_gap;
  Store.stage_rows db ~stage_id:search_gap.id ~first:1L
    ~last:search_gap.upper_uid [row 1L];
  publish_rejects "partial SEARCH inventory accepted"
    ~needle:"incomplete range coverage" ~explicit:104L search_gap;
  let action=stage db first ~stage_id:"regression" sel [row 1L] in
  publish_rejects "regressing completed anchor accepted"
    ~needle:"MODSEQ regression" ~explicit:97L action

let test_stale_before_coverage db =
  let first=baseline db in
  let sel=selected ~highest:104L 5L 5L in
  let gap=ok (M.plan first ~stage_id:"stale-gap" sel) in
  Store.begin_stage db ~cursor:first ~action:gap;
  Store.stage_rows db ~stage_id:gap.id ~first:1L ~last:1L [row 1L];
  Store.stage_membership db ~stage_id:gap.id ~first:1L ~last:1L [uid 1L];
  let action=stage db first ~stage_id:"advance" sel [row 1L;row 2L] in
  ignore (publish ~explicit:104L db first action);
  match Store.publish_stage db ~cursor:first ~action:gap
    ~explicit_highestmodseq:(Some (modseq 104L)) ~nomodseq:false with
  | `Stale_revision -> ()
  | `Committed _ -> Alcotest.fail "stale incomplete stage was published"
  | exception Invalid_argument message ->
      Alcotest.failf "stale incomplete stage raised: %s" message

let test_presence_after_publication db =
  let module J = Store.Journal in
  let first=baseline db in
  let pair : J.pair = {
    id="presence";scope;remote_uidvalidity=Some (validity 5L);
    remote_uid=Some (uid 1L);local_id=Some "presence-local";
    content_sha256=None;content_length=None;internal_date=None;
    common_flags=[];remote_tombstone=None;local_tombstone=None;
    revision=0L} in
  let pair=match J.put_pair db ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new presence pair stale" in
  let note generation=J.note_presence db ~pair ~side:`Remote ~generation in
  Alcotest.(check bool) "current generation recorded" true
    (note first.generation=`Recorded);
  let action=stage db first ~stage_id:"moved-on"
    (selected ~highest:100L 5L 4L) [row 1L;row 2L;row 3L] in
  let next=(publish ~explicit:100L db first action).cursor in
  Alcotest.(check bool) "replaced generation is stale" true
    (note first.generation=`Stale_revision);
  rejects "future generation accepted" ~needle:"unpublished generation"
    (fun () -> note (Int64.succ next.generation))

let test_flag_delta db =
  let first=baseline db in
  let before=snapshot db first in
  let action=stage db first ~stage_id:"flags" (selected ~highest:99L 5L 4L)
    [row ~flags:[flag "\\Seen"] 1L;row 2L;row 3L] in
  let next=(publish ~explicit:99L db first action).cursor in
  Alcotest.(check (list int64)) "changed UID" [1L]
    (changed before (snapshot db next))

let test_duplicate_flag_membership db =
  let cursor=Store.load_cursor db ~scope in
  let action=stage db cursor ~stage_id:"old" (selected ~highest:10L 5L 2L)
    [row ~flags:[flag "\\Seen";flag "\\Seen"] 1L] in
  let first=(publish ~explicit:10L db cursor action).cursor in
  let before=snapshot db first in
  let action=stage db first ~stage_id:"new" (selected ~highest:11L 5L 2L)
    [row ~flags:[flag "\\Seen";flag "\\Flagged"] 1L] in
  let next=(publish ~explicit:11L db first action).cursor in
  Alcotest.(check int) "membership change" 1
    (List.length (changed before (snapshot db next)))

let test_mid_cycle_nomodseq db =
  let first=baseline db in
  let action=stage db first ~stage_id:"nomodseq"
    (selected ~highest:99L 5L 4L) [row 1L;row 2L;row 3L] in
  let next=(publish ~nomodseq:true db first action).cursor in
  Alcotest.(check bool) "baseline mode" true (next.mode=Baseline);
  Alcotest.(check bool) "anchor cleared" true (next.anchor=None);
  let probe=ok (M.plan first ~stage_id:"nomodseq-probe"
    (selected ~nomodseq:true ~highest:99L 5L 4L)) in
  Alcotest.(check bool) "restart reason" true (probe.restart=Some Nomodseq)

let test_anchor_needs_explicit_highestmodseq db =
  let first=baseline db in
  (* The highest-MODSEQ message was expunged, so the largest row MODSEQ is
     below the previous anchor. That is not a regression. *)
  let action=stage db first ~stage_id:"no-explicit"
    (selected ~highest:99L 5L 4L)
    [row ~modseq:(modseq 50L) 1L;row ~modseq:(modseq 60L) 2L] in
  let next=(publish db first action).cursor in
  Alcotest.(check (option int64)) "no anchor without HIGHESTMODSEQ" None
    (Option.map Imap.Modseq.to_int64 next.anchor)

let test_restart_reason_kept db =
  let first=baseline db in
  let action=stage db first ~stage_id:"epoch-nomodseq"
    (selected ~nomodseq:true ~highest:5L 6L 2L) [row 1L] in
  Alcotest.(check bool) "UIDVALIDITY reason survives NOMODSEQ" true
    (action.restart=Some Uidvalidity_changed);
  let next=(publish ~nomodseq:true db first action).cursor in
  Alcotest.(check bool) "NOMODSEQ epoch published in baseline mode" true
    (next.mode=Baseline && next.uidvalidity=Some (validity 6L))

let test_over_coverage db =
  let first=baseline db in
  let action=ok (M.plan first ~stage_id:"over"
    (selected ~highest:99L 5L 4L)) in
  let beyond=Int64.succ action.upper_uid in
  Store.begin_stage db ~cursor:first ~action;
  rejects "FETCH over-coverage not reported as invalid" (fun () ->
    Store.stage_rows db ~stage_id:action.id ~first:1L ~last:beyond
      [row 1L]);
  Store.stage_rows db ~stage_id:action.id ~first:1L ~last:action.upper_uid
    [row 1L];
  rejects "SEARCH over-coverage not reported as invalid" (fun () ->
    Store.stage_membership db ~stage_id:action.id ~first:1L ~last:beyond
      [uid 1L])

let test_recent_only_change db =
  let first=baseline db in
  let before=snapshot db first in
  let action=stage db first ~stage_id:"recent" (selected ~highest:99L 5L 4L)
    [row ~flags:[flag "\\Recent"] 1L;row 2L;row 3L] in
  let next=(publish ~explicit:99L db first action).cursor in
  Alcotest.(check int) "Recent is not a durable change" 0
    (List.length (changed before (snapshot db next)))

let test_cross_scope_action db =
  let source=Store.load_cursor db ~scope in
  let other=Store.load_cursor db
    ~scope:{scope with mailbox_key="archive";raw_name="Archive"} in
  let action=ok (M.plan source ~stage_id:"same-id"
    (selected ~highest:1L 5L 2L)) in
  rejects "cross-scope completion accepted" ~needle:"mismatch" (fun () ->
    Store.begin_stage db ~cursor:other ~action);
  Store.begin_stage db ~cursor:source ~action;
  fill db action [row 1L];
  rejects "cross-scope publication accepted" ~needle:"mismatch" (fun () ->
    Store.publish_stage db ~cursor:other ~action
      ~explicit_highestmodseq:(Some (modseq 1L)) ~nomodseq:false)

let test_restore_cursor db =
  ignore (baseline db);
  let c=Store.load_cursor db ~scope in
  let restored=ok (M.restore ~schema_version:c.schema_version ~scope:c.scope
    ~phase:c.phase ~uidvalidity:c.uidvalidity ~generation:c.generation
    ~revision:c.revision ~anchor:c.anchor ~frontier:c.frontier
    ~inventory_ref:c.inventory_ref ~mode:c.mode) in
  Alcotest.(check bool) "round trip" true (restored=c);
  (match M.restore ~schema_version:c.schema_version ~scope:c.scope
    ~phase:c.phase ~uidvalidity:c.uidvalidity ~generation:c.generation
    ~revision:(Int64.succ c.revision) ~anchor:c.anchor ~frontier:c.frontier
    ~inventory_ref:c.inventory_ref ~mode:c.mode with
   | Error (Invalid _) -> ()
   | _ -> Alcotest.fail "accepted inconsistent persisted counters")

let () =
  Eio_main.run @@ fun env ->
  let case name f =
    Alcotest.test_case name `Quick (fun () -> with_store env f) in
  Alcotest.run "IMAP stage publication"
    ["reconciliation", [
      case "same count, different UIDs" test_same_count_different_uids;
      case "UIDVALIDITY change" test_epoch_change;
      case "lower explicit MODSEQ and interruption"
        test_anchor_and_interruption;
      case "incomplete and regression" test_incomplete_and_regression;
      case "staleness before coverage" test_stale_before_coverage;
      case "presence after a later publication"
        test_presence_after_publication;
      case "flag delta" test_flag_delta;
      case "duplicate flag membership" test_duplicate_flag_membership;
      case "mid-cycle NOMODSEQ" test_mid_cycle_nomodseq;
      case "anchor needs explicit HIGHESTMODSEQ"
        test_anchor_needs_explicit_highestmodseq;
      case "restart reason kept" test_restart_reason_kept;
      case "over-coverage" test_over_coverage;
      case "Recent-only change" test_recent_only_change;
      case "cross-scope action" test_cross_scope_action;
      case "restored cursor" test_restore_cursor]]
