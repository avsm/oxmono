open Imap.Mirror

let ok = function
  | Ok x -> x
  | Error _ -> Alcotest.fail "unexpected mirror error"

let uid n = ok (Imap.Proto.Uid.of_int64 n)
let validity n = ok (Imap.Proto.Uidvalidity.of_int64 n)
let modseq n = ok (Imap.Proto.Modseq.of_int64 n)
let flag s = ok (Mail_flag.Imap_flag.of_wire s)
let row ?(flags=[]) ?modseq:seq n = {uid=uid n;flags;modseq=seq}
let scope = {
  endpoint="imap.example";account="alice";mailbox_key="inbox";
  raw_name="INBOX";encoding=Imap.Mailbox_name.Rev1;mailbox_id=None
}
let selected ?(nomodseq=false) ?highest uidvalidity uidnext = {
  uidvalidity=validity uidvalidity;uidnext;
  highestmodseq=Option.map modseq highest;nomodseq
}
let done_ action rows ?explicit () = {
  action_id=action.id;uidvalidity=action.uidvalidity;
  covered_upper=action.upper_uid;inventory_complete=true;
  commands_complete=true;rows;
  explicit_highestmodseq=Option.map modseq explicit;nomodseq=false
}
let uids rows = List.map (fun row -> Imap.Proto.Uid.to_int64 row.uid) rows

let baseline () =
  let cursor=initial scope in
  let action=ok (plan cursor ~stage_id:"first"
    (selected ~highest:98L 5L 4L)) in
  let staged=ok (complete cursor action
    (done_ action [row 1L;row 2L;row 3L] ~explicit:98L ())) in
  ok (publish cursor ~published:None staged)

let test_same_count_different_uids () =
  let first=baseline () in
  let action=ok (plan first.cursor ~stage_id:"second"
    (selected ~highest:100L 5L 5L)) in
  let staged=ok (complete first.cursor action
    (done_ action [row 1L;row 3L;row 4L] ~explicit:100L ())) in
  let next=ok (publish first.cursor ~published:(Some first.snapshot) staged) in
  Alcotest.(check (list int64)) "new UID" [4L] (uids next.added);
  Alcotest.(check (list int64)) "expunged UID" [2L]
    (List.map Imap.Proto.Uid.to_int64 next.removed);
  Alcotest.(check int) "same count" 3 (List.length (rows next.snapshot))

let test_epoch_change () =
  let first=baseline () in
  let action=ok (plan first.cursor ~stage_id:"new-epoch"
    (selected ~highest:1L 6L 2L)) in
  Alcotest.(check bool) "restart reason" true
    (action.restart=Some Uidvalidity_changed);
  let staged=ok (complete first.cursor action
    (done_ action [row 1L] ~explicit:1L ())) in
  let next=ok (publish first.cursor ~published:(Some first.snapshot) staged) in
  Alcotest.(check bool) "invalidated" true next.invalidated_epoch;
  Alcotest.(check int) "no deletion propagation" 0 (List.length next.removed);
  Alcotest.(check (list int64)) "new epoch inventory" [1L] (uids next.added)

let test_anchor_and_interruption () =
  let first=baseline () in
  let action=ok (plan first.cursor ~stage_id:"catch-up"
    (selected ~highest:104L 5L 5L)) in
  let staged=ok (complete first.cursor action
    (done_ action [row ~modseq:(modseq 103L) 1L;
                   row ~modseq:(modseq 104L) 2L;
                   row 3L;row 4L] ~explicit:99L ())) in
  (* The staging write can be interrupted. No cursor changes until publish. *)
  Alcotest.(check (option int64)) "old durable anchor" (Some 98L)
    (Option.map Imap.Proto.Modseq.to_int64 first.cursor.anchor);
  let next=ok (publish first.cursor ~published:(Some first.snapshot) staged) in
  Alcotest.(check (option int64)) "explicit lower anchor wins" (Some 99L)
    (Option.map Imap.Proto.Modseq.to_int64 next.cursor.anchor);
  Alcotest.(check int64) "revision advanced at publication" 2L
    next.cursor.revision;
  (match publish next.cursor ~published:(Some next.snapshot) staged with
   | Error Stale_revision -> ()
   | _ -> Alcotest.fail "stale stage was accepted")

let test_incomplete_and_regression () =
  let first=baseline () in
  let action=ok (plan first.cursor ~stage_id:"incomplete"
    (selected ~highest:104L 5L 5L)) in
  let partial={(done_ action [row 1L] ()) with inventory_complete=false} in
  (match complete first.cursor action partial with
   | Error Incomplete_coverage -> ()
   | _ -> Alcotest.fail "partial inventory accepted");
  (match complete first.cursor action
    (done_ action [row 1L] ~explicit:97L ()) with
   | Error Modseq_regression -> ()
   | _ -> Alcotest.fail "regressing completed anchor accepted")

let test_flag_delta () =
  let first=baseline () in
  let action=ok (plan first.cursor ~stage_id:"flags"
    (selected ~highest:99L 5L 4L)) in
  let staged=ok (complete first.cursor action
    (done_ action [row ~flags:[flag "\\Seen"] 1L;
                   row 2L;row 3L] ~explicit:99L ())) in
  let next=ok (publish first.cursor ~published:(Some first.snapshot) staged) in
  Alcotest.(check (list int64)) "changed UID"
    [1L] (List.map (fun x -> Imap.Proto.Uid.to_int64 x.after.uid) next.changed)

let test_duplicate_flag_membership () =
  let cursor=initial scope in
  let action=ok (plan cursor ~stage_id:"old"
    (selected ~highest:10L 5L 2L)) in
  let staged=ok (complete cursor action
    (done_ action [row ~flags:[flag "\\Seen";flag "\\Seen"] 1L]
      ~explicit:10L ())) in
  let first=ok (publish cursor ~published:None staged) in
  let action=ok (plan first.cursor ~stage_id:"new"
    (selected ~highest:11L 5L 2L)) in
  let staged=ok (complete first.cursor action
    (done_ action [row ~flags:[flag "\\Seen";flag "\\Flagged"] 1L]
      ~explicit:11L ())) in
  let next=ok (publish first.cursor ~published:(Some first.snapshot) staged) in
  Alcotest.(check int) "membership change" 1 (List.length next.changed)

let test_mid_cycle_nomodseq () =
  let first=baseline () in
  let action=ok (plan first.cursor ~stage_id:"nomodseq"
    (selected ~highest:99L 5L 4L)) in
  let completed={(done_ action [row 1L;row 2L;row 3L] ()) with nomodseq=true} in
  let staged=ok (complete first.cursor action completed) in
  let next=ok (publish first.cursor ~published:(Some first.snapshot) staged) in
  Alcotest.(check bool) "baseline mode" true (next.cursor.mode=Baseline);
  Alcotest.(check bool) "anchor cleared" true (next.cursor.anchor=None);
  Alcotest.(check bool) "restart reason" true (next.restart=Some Nomodseq)

let test_cross_scope_action () =
  let source=initial scope in
  let other=initial {scope with mailbox_key="archive";raw_name="Archive"} in
  let action=ok (plan source ~stage_id:"same-id"
    (selected ~highest:1L 5L 2L)) in
  let receipt=done_ action [row 1L] ~explicit:1L () in
  (match complete other action receipt with
   | Error Wrong_action -> ()
   | _ -> Alcotest.fail "cross-scope completion accepted");
  let staged=ok (complete source action receipt) in
  (match publish other ~published:None staged with
   | Error Wrong_action -> ()
   | _ -> Alcotest.fail "cross-scope publication accepted")

let test_restore_cursor () =
  let published=baseline () in
  let c=published.cursor in
  let restored=ok (restore ~schema_version:c.schema_version ~scope:c.scope
    ~phase:c.phase ~uidvalidity:c.uidvalidity ~generation:c.generation
    ~revision:c.revision ~anchor:c.anchor ~frontier:c.frontier
    ~inventory_ref:c.inventory_ref ~mode:c.mode) in
  Alcotest.(check bool) "round trip" true (restored=c);
  (match restore ~schema_version:c.schema_version ~scope:c.scope
    ~phase:c.phase ~uidvalidity:c.uidvalidity ~generation:c.generation
    ~revision:(Int64.succ c.revision) ~anchor:c.anchor ~frontier:c.frontier
    ~inventory_ref:c.inventory_ref ~mode:c.mode with
   | Error (Invalid _) -> ()
   | _ -> Alcotest.fail "accepted inconsistent persisted counters")

let () =
  Alcotest.run "IMAP mirror"
    ["reconciliation", [
      Alcotest.test_case "same count, different UIDs" `Quick
        test_same_count_different_uids;
      Alcotest.test_case "UIDVALIDITY change" `Quick test_epoch_change;
      Alcotest.test_case "lower explicit MODSEQ and interruption" `Quick
        test_anchor_and_interruption;
      Alcotest.test_case "incomplete and regression" `Quick
        test_incomplete_and_regression;
      Alcotest.test_case "flag delta" `Quick test_flag_delta;
      Alcotest.test_case "duplicate flag membership" `Quick
        test_duplicate_flag_membership;
      Alcotest.test_case "mid-cycle NOMODSEQ" `Quick test_mid_cycle_nomodseq;
      Alcotest.test_case "cross-scope action" `Quick test_cross_scope_action;
      Alcotest.test_case "restored cursor" `Quick test_restore_cursor]]
