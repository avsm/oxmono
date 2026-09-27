module S = Imap_sync.Flags
module F = Mail_flag.Imap_flag

let seen=F.system F.Seen
let flagged=F.system F.Flagged
let deleted=F.system F.Deleted
let keyword=match F.of_wire "customKey" with
  | Ok flag -> flag | Error e -> failwith e

let apply = function
  | Ok {S.plan=S.Apply flags;_} -> flags
  | Ok {plan=S.No_change;_} -> Alcotest.fail "expected flag changes"
  | Error e -> Alcotest.fail (Format.asprintf "%a" S.pp_error e)

let test_remote_add_to_local () =
  let flags=S.plan_flags ~base:[] ~remote:[seen;keyword] ~local:[]
    ~condstore:false ~remote_modseq:None () |> apply in
  Alcotest.(check int) "remote additions copied locally" 2
    (List.length flags)

let test_local_add_requires_modseq () =
  (match S.plan_flags ~base:[] ~remote:[] ~local:[flagged]
    ~condstore:false ~remote_modseq:None () with
   | Error S.Conditional_store_unavailable -> ()
   | _ -> Alcotest.fail "must hold an unguarded remote write");
  (match S.plan_flags ~base:[] ~remote:[] ~local:[flagged]
    ~condstore:true ~remote_modseq:None () with
   | Error S.Conditional_store_unavailable -> ()
   | _ -> Alcotest.fail "must hold when FETCH omitted MODSEQ");
  (match S.plan_flags ~base:[] ~remote:[] ~local:[flagged]
    ~condstore:true ~remote_modseq:(Some 0L) () with
   | Error S.Conditional_store_unavailable -> ()
   | _ -> Alcotest.fail "must hold invalid zero MODSEQ");
  let flags=S.plan_flags ~base:[] ~remote:[] ~local:[flagged]
    ~condstore:true ~remote_modseq:(Some 4L) () |> apply in
  Alcotest.(check bool) "flag included" true (List.mem flagged flags)

let test_deleted_hold () =
  (match S.plan_flags ~base:[] ~remote:[deleted] ~local:[]
    ~condstore:true ~remote_modseq:(Some 1L) () with
   | Ok {plan=S.No_change;deleted_held=true} -> ()
   | _ -> Alcotest.fail "\\Deleted must be held by default");
  (match S.plan_flags ~base:[] ~remote:[deleted;seen] ~local:[flagged]
    ~condstore:true ~remote_modseq:(Some 1L) () with
   | Ok {plan=S.Apply merged;deleted_held=true} ->
       Alcotest.(check bool) "other flags merge while \\Deleted is held" true
         (List.mem seen merged && List.mem flagged merged &&
          not (List.mem deleted merged))
   | _ -> Alcotest.fail "held \\Deleted blocked the other flags");
  let flags=S.plan_flags ~propagate_deleted:true ~base:[]
    ~remote:[deleted] ~local:[] ~condstore:false ~remote_modseq:None ()
    |> apply in
  Alcotest.(check bool) "explicit policy propagates" true
    (List.mem deleted flags)

let test_three_way_and_recent () =
  let recent=match F.of_wire "\\Recent" with
    | Ok flag -> flag | Error e -> failwith e in
  let flags=S.plan_flags ~base:[seen] ~remote:[seen;keyword;recent]
    ~local:[flagged] ~condstore:true ~remote_modseq:(Some 8L) ()
    |> apply in
  Alcotest.(check bool) "removed seen remains removed" false
    (List.mem seen flags);
  Alcotest.(check bool) "remote keyword retained" true
    (List.mem keyword flags);
  Alcotest.(check bool) "local flagged retained" true
    (List.mem flagged flags);
  Alcotest.(check bool) "recent excluded" false (List.mem recent flags)

let test_no_change () =
  match S.plan_flags ~base:[seen] ~remote:[seen]
    ~local:[seen] ~condstore:false ~remote_modseq:None () with
  | Ok {plan=S.No_change;deleted_held=false} -> ()
  | _ -> Alcotest.fail "equal observations should be a no-op"

let test_common_baseline_advances () =
  let merged=S.plan_flags ~base:[] ~remote:[seen]
    ~local:[seen] ~condstore:false ~remote_modseq:None () |> apply in
  Alcotest.(check bool) "joint change commits new common baseline" true
    (List.mem seen merged)

let test_permanent_flags () =
  (match S.validate_permanent_flags ~available:(Some ["\\Seen";"\\*"])
    ~defined:None ~remote:[seen] ~merged:[seen;keyword] with
   | Ok () -> () | _ -> Alcotest.fail "wildcard should permit new keyword");
  (match S.validate_permanent_flags ~available:(Some ["\\Seen";"\\*"])
    ~defined:None ~remote:[seen;keyword] ~merged:[seen] with
   | Error (S.Permanent_flag_unavailable flag) when F.equal flag keyword -> ()
   | _ -> Alcotest.fail "wildcard does not authorize keyword removal");
  (match S.validate_permanent_flags ~available:(Some ["\\Seen"])
    ~defined:None ~remote:[seen] ~merged:[seen;flagged] with
   | Error (S.Permanent_flag_unavailable flag) when F.equal flag flagged -> ()
   | _ -> Alcotest.fail "unlisted system flag must be held");
  (match S.validate_permanent_flags ~available:None ~defined:None
    ~remote:[] ~merged:[seen;keyword] with
   | Ok () -> ()
   | _ -> Alcotest.fail "missing PERMANENTFLAGS means every flag is permanent");
  (match S.validate_permanent_flags ~available:(Some ["\\Seen";"\\*"])
    ~defined:(Some ["\\Seen";"customKey"]) ~remote:[seen]
    ~merged:[seen;keyword] with
   | Error (S.Permanent_flag_unavailable flag) when F.equal flag keyword -> ()
   | _ -> Alcotest.fail "wildcard licenses only keywords absent from FLAGS")

let test_case_only_is_unchanged () =
  let flag wire = match F.of_wire wire with
    | Ok flag -> flag | Error error -> failwith error in
  match S.plan_flags ~base:[flag "$Label";flag "\\X-Custom"]
    ~remote:[flag "$LABEL";flag "\\x-custom"]
    ~local:[flag "$label";flag "\\X-CUSTOM"]
    ~condstore:false ~remote_modseq:None () with
  | Ok {plan=S.No_change;_} -> ()
  | _ -> Alcotest.fail "case-only differences must not create FLAGS operations"

let () = Alcotest.run "imap flag sync" ["planning",[
  Alcotest.test_case "remote add needs only local write" `Quick
    test_remote_add_to_local;
  Alcotest.test_case "remote write requires conditional STORE" `Quick
    test_local_add_requires_modseq;
  Alcotest.test_case "deleted policy" `Quick test_deleted_hold;
  Alcotest.test_case "three-way flags and recent" `Quick
    test_three_way_and_recent;
  Alcotest.test_case "no-op" `Quick test_no_change;
  Alcotest.test_case "case-only no-op" `Quick test_case_only_is_unchanged;
  Alcotest.test_case "joint changes advance baseline" `Quick
    test_common_baseline_advances;
  Alcotest.test_case "permanent flag preflight" `Quick
    test_permanent_flags]]
