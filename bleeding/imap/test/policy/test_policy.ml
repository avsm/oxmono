module F = Mail_flag.Imap_flag
module P = Imap.Sync_policy

let flag text = match F.of_wire text with
  | Ok flag -> flag | Error message -> Alcotest.fail message
let wires flags = List.map F.to_wire flags
let check_flags name expected actual =
  Alcotest.(check (list string)) name expected (wires actual)

let test_disjoint_changes () =
  let base=[flag "\\Seen";flag "$Old"] in
  let remote=[flag "\\Seen";flag "$Remote"] in
  let local=[flag "\\Seen";flag "$Old";flag "$Local"] in
  let plan=P.reconcile_flags ~base ~remote ~local () in
  check_flags "merged" ["\\Seen";"$Local";"$Remote"] plan.merged;
  check_flags "remote adds" ["$Local"] plan.to_remote.add;
  check_flags "remote removes" [] plan.to_remote.remove;
  check_flags "local adds" ["$Remote"] plan.to_local.add;
  check_flags "local removes" ["$Old"] plan.to_local.remove

let test_wire_semantics () =
  let base=[flag "Seen"] in
  let remote=[flag "SEEN";flag "\\Forwarded";flag "\\Recent"] in
  let local=[flag "Seen";flag "\\Seen"] in
  let plan=P.reconcile_flags ~base ~remote ~local () in
  check_flags "keeps keyword distinct and remote spelling"
    ["\\Seen";"SEEN";"\\Forwarded"] plan.merged;
  check_flags "remote adds system flag" ["\\Seen"] plan.to_remote.add;
  check_flags "local adds extension" ["\\Forwarded"] plan.to_local.add

let test_deleted_gate () =
  let remote=[flag "\\Deleted"] in
  let held=P.reconcile_flags ~base:[] ~remote ~local:[] () in
  Alcotest.(check bool) "deleted held without policy" true held.deleted_held;
  check_flags "held deleted keeps base" [] held.merged;
  check_flags "no local deleted add" [] held.to_local.add;
  check_flags "no remote deleted removal" [] held.to_remote.remove;
  let plan=P.reconcile_flags ~propagate_deleted:true
    ~base:[] ~remote ~local:[] () in
  Alcotest.(check bool) "explicit policy" false plan.deleted_held;
  check_flags "local deleted add" ["\\Deleted"] plan.to_local.add

let test_deleted_hold_is_per_flag () =
  let seen=flag "\\Seen" and flagged=flag "\\Flagged" in
  let deleted=flag "\\Deleted" in
  let plan=P.reconcile_flags ~base:[seen] ~remote:[seen;deleted;flagged]
    ~local:[] () in
  Alcotest.(check bool) "deleted held" true plan.deleted_held;
  check_flags "other flags merge" ["\\Flagged"] plan.merged;
  check_flags "local gets flagged, not deleted" ["\\Flagged"]
    plan.to_local.add;
  check_flags "remote loses seen, keeps deleted" ["\\Seen"]
    plan.to_remote.remove;
  let agreed=P.reconcile_flags ~base:[] ~remote:[deleted] ~local:[deleted] ()
  in
  Alcotest.(check bool) "identical deleted adds are not held" false
    agreed.deleted_held;
  check_flags "agreed deleted merges" ["\\Deleted"] agreed.merged;
  check_flags "nothing to send remote" [] agreed.to_remote.add;
  check_flags "nothing to send local" [] agreed.to_local.add

let test_deletion_guards () =
  let plan ?(policy=P.Propagate) ?(paired=true) ?(local_retained=false)
      ?(remote_complete=true) ?(survivor_unchanged=true) () =
    P.plan_disappearance ~policy ~paired ~remote_present:false
      ~remote_complete ~local_present:true ~local_complete:true
      ~local_retained
      ~survivor_unchanged in
  Alcotest.(check bool) "default preserve" true
    (plan ~policy:P.Preserve () =
      P.Hold_deletion P.Preservation_policy);
  Alcotest.(check bool) "missing incomplete" true
    (plan ~remote_complete:false () =
      P.Hold_deletion P.Incomplete_inventory);
  Alcotest.(check bool) "unpaired" true
    (plan ~paired:false () = P.Hold_deletion P.Unpaired_identity);
  Alcotest.(check bool) "survivor changed" true
    (plan ~survivor_unchanged:false () =
      P.Hold_deletion P.Survivor_changed);
  Alcotest.(check bool) "explicit complete deletion" true
    (plan () = P.Delete_local);
  Alcotest.(check bool) "remote-only propagation" true
    (plan ~policy:P.Propagate_remote () = P.Delete_local);
  Alcotest.(check bool) "wrong direction held" true
    (plan ~policy:P.Propagate_local () =
      P.Hold_deletion P.Direction_policy);
  let local_missing policy retained = P.plan_disappearance ~policy
    ~paired:true ~remote_present:true ~remote_complete:true
    ~local_present:false ~local_complete:true ~local_retained:retained
    ~survivor_unchanged:true in
  Alcotest.(check bool) "local-only propagation" true
    (local_missing P.Propagate_local false = P.Delete_remote);
  Alcotest.(check bool) "retention protects remote" true
    (local_missing P.Propagate true =
      P.Hold_deletion P.Retention_policy)

let test_flag_truth_table () =
  let seen=flag "\\Seen" in
  let as_list = function true -> [seen] | false -> [] in
  let apply present (delta:P.flag_delta) =
    let present=present || List.exists (F.equal seen) delta.add in
    present && not (List.exists (F.equal seen) delta.remove) in
  List.iter (fun base -> List.iter (fun remote -> List.iter (fun local ->
    let plan=P.reconcile_flags ~base:(as_list base)
      ~remote:(as_list remote) ~local:(as_list local) () in
    let expected=if base then remote && local else remote || local in
    let merged=List.exists (F.equal seen) plan.merged in
    Alcotest.(check bool) "merged truth table" expected merged;
    Alcotest.(check bool) "remote delta converges" merged
      (apply remote plan.to_remote);
    Alcotest.(check bool) "local delta converges" merged
      (apply local plan.to_local)) [false;true]) [false;true])
    [false;true]

let test_absence_grace () =
  let mature current first scans=P.absence_mature
    ~last_present_generation:None ~current_generation:current ~first_generation:first
    ~min_scans:scans in
  Alcotest.(check bool) "first observation held" false
    (mature 12L (Some 12L) 1);
  Alcotest.(check bool) "next complete scan permits" true
    (mature 13L (Some 12L) 1);
  Alcotest.(check bool) "legacy unknown age held" false
    (mature 13L None 1);
  Alcotest.(check bool) "zero grace preserves old policy" true
    (mature 12L None 0);
  Alcotest.(check bool) "legacy absence with presence witness, zero grace"
    true (P.absence_mature ~last_present_generation:(Some 9L)
      ~current_generation:12L ~first_generation:None ~min_scans:0);
  Alcotest.(check bool) "legacy absence with presence witness, grace" false
    (P.absence_mature ~last_present_generation:(Some 9L)
      ~current_generation:12L ~first_generation:None ~min_scans:1);
  Alcotest.(check bool) "later presence supersedes old absence" false
    (P.absence_mature ~last_present_generation:(Some 14L)
      ~current_generation:16L ~first_generation:(Some 12L)
      ~min_scans:1);
  Alcotest.(check bool) "later presence also gates zero grace" false
    (P.absence_mature ~last_present_generation:(Some 14L)
      ~current_generation:16L ~first_generation:(Some 12L)
      ~min_scans:0);
  let plan ~mature=P.plan_disappearance_with_grace
    ~absence_mature:mature ~policy:P.Propagate ~paired:true
    ~remote_present:true ~remote_complete:true
    ~local_present:false ~local_complete:true
    ~local_retained:false ~survivor_unchanged:true in
  Alcotest.(check bool) "immature deletion held" true
    (plan ~mature:false=P.Hold_deletion P.Grace_period);
  Alcotest.(check bool) "mature deletion planned" true
    (plan ~mature:true=P.Delete_remote)

let () = Alcotest.run "imap-policy" ["flags", [
  Alcotest.test_case "disjoint changes" `Quick test_disjoint_changes;
  Alcotest.test_case "wire semantics" `Quick test_wire_semantics;
  Alcotest.test_case "deleted gate" `Quick test_deleted_gate;
  Alcotest.test_case "deleted hold is per flag" `Quick
    test_deleted_hold_is_per_flag;
  Alcotest.test_case "deletion guards" `Quick test_deletion_guards;
  Alcotest.test_case "absence grace" `Quick test_absence_grace;
  Alcotest.test_case "flag truth table" `Quick test_flag_truth_table]]
