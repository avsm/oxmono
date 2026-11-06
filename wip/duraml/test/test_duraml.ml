module D = Duraml
let time s = Option.get (Ptime.of_float_s s)
let span s = Option.get (Ptime.Span.of_float_s s)
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let spec ?key ?not_before ?lane ?(payload="input") ?(queue="work") () =
  D.Spec.v ~queue ~kind:"example" ~version:1 ~payload ?key ?not_before ?lane
    ()
let inserted = function D.Inserted id -> id | _ -> Alcotest.fail
  "Expected insert"
let claim q = match D.try_take q ~queue:"work" ~lease:(span 10.) with
  | D.Ready c -> c | _ -> Alcotest.fail "Expected claim"
let with_queue backend f = Eio_main.run (fun env ->
  let path=Filename.temp_file "duraml-" ".db" in Unix.unlink path;
  Fun.protect ~finally:(fun () -> List.iter (fun suffix ->
    let p=path^suffix in if Sys.file_exists p then Sys.remove p)
      ["";"-wal";"-shm"])
    (fun () -> Eio.Switch.run (fun sw ->
      let clock=Eio_mock.Clock.make () in
      let q=if backend="memory" then
        Duraml_memory.create ~sw ~clock ~random:env#secure_random ()
      else Duraml_sqlite.open_ ~sw ~clock ~random:env#secure_random
        Eio.Path.(env#fs / path) in
      f env clock q)))
let dedup backend () = with_queue backend (fun _ _ q ->
  let s=spec ~key:"stable" () in
  let id=inserted (D.enqueue q s) in
  check "same key" true (D.enqueue q s=D.Existing id);
  check "conflicting payload" true
    (D.enqueue q (spec ~key:"stable" ~payload:"changed" ())=D.Conflict id);
  ignore (D.enqueue q (spec ~key:"stable" ~queue:"other" ()));
  let c=claim q in
  check "completion" true (D.settle q c (D.complete (D.Succeeded
    "ok"))=D.Applied);
  check "key survives completion" true (D.enqueue q s=D.Existing id);
  count "filtered count" 1 (D.count q (D.query ~queue:"work" ~limit:1 ()));
  let newest=D.tail ~limit:2 q in
  count "tail page" 2 (List.length newest);
  check "tail is newest first" true (D.Event.sequence (List.hd newest)
    > D.Event.sequence (List.nth newest 1));
  check "tail cursor excludes" true (List.for_all (fun e ->
    D.Event.sequence e < D.Event.sequence (List.nth newest 1))
    (D.tail ~before:(D.Event.sequence (List.nth newest 1)) q));
  count "filtered query" 1 (List.length (D.list q (D.query ~queue:"work"
    ~state:D.Done ())));
  count "exclusive query cursor" 1 (List.length (D.list q (D.query ~after:id
    ())));
  check "no payload in events" true
    (List.for_all (fun (e:D.event) -> (D.Event.detail e)<>"input" &&
      (D.Event.detail e)<>"ok")
      (D.events q)))
let fencing backend () = with_queue backend (fun _ clock q ->
  let id=inserted (D.enqueue q (spec ())) in
  let a=claim q in
  Eio_mock.Clock.set_time clock 4.;
  check "renew" true (D.renew q a ~lease:(span 10.));
  Eio_mock.Clock.set_time clock 11.;
  check "renewed claim still owns" true
    (match D.try_take q ~queue:"work" ~lease:(span 10.) with D.Await _ -> true
      | _ ->
      false);
  Eio_mock.Clock.set_time clock 14.;
  check "expired renewal refused" false (D.renew q a ~lease:(span 10.));
  let b=claim q in
  count "new generation" 2 (D.Claim.attempt b);
  check "stale completion fenced" true
    (D.settle q a (D.complete (D.Succeeded "old"))=D.Lost_claim);
  check "new completion" true
    (D.settle q b (D.complete (D.Succeeded "new"))=D.Applied);
  check "result retained" true ((D.Job.outcome (Option.get (D.find q id)))=Some
    (D.Succeeded "new"));
  check "lease expiry recorded" true
    (List.exists (fun (e:D.event) -> (D.Event.kind e)="lease_expired")
      (D.events q)))
let atomic_successors backend () = with_queue backend (fun _ clock q ->
  let source=inserted (D.enqueue q (spec ())) in
  let c=claim q in
  let existing=inserted (D.enqueue q (spec ~queue:"convert" ~key:"occupied"
    ())) in
  let before=D.events q in
  let followups=[spec ~queue:"convert" ~key:"new" ();
    spec ~queue:"convert" ~key:"occupied" ~payload:"different" ()] in
  check "successor conflict" true
    (D.settle q c (D.complete ~followups (D.Succeeded "ok"))=
      D.Followup_conflict existing);
  check "events rollback" true (D.events q=before);
  count "insert rollback" 2 (List.length (D.list q (D.query ())));
  check "claim remains live" true ((D.Job.state (Option.get (D.find q
    source)))=D.Running);
  let finish=D.complete ~followups:[spec ~queue:"convert" ~key:"new" ()]
    (D.Succeeded "ok") in
  check "settle succeeds" true (D.settle q c finish=D.Applied);
  Eio_mock.Clock.set_time clock 100.;
  check "repeat after expiry" true (D.settle q c finish=D.Already_applied);
  check "different repeat refused" true
    (D.settle q c (D.complete (D.Succeeded "changed"))=D.Settlement_conflict);
  let source=D.Job.id (List.hd (D.list q (D.query ()))) in
  let successors=D.list q (D.query ~parent:source ()) in
  count "successor lineage" 1 (List.length successors);
  check "successor parent" true
    (D.Job.parent (List.hd successors)=Some source);
  check "direct work has no parent" true
    (List.for_all (fun (j:D.job) -> D.Job.parent j=None)
      (D.list q (D.query ~queue:"work" ())));
  count "only one successor" 3 (List.length (D.list q (D.query ()))))
let retry_cancel backend () = with_queue backend (fun _ clock q ->
  let id=inserted (D.enqueue q (spec ())) in
  let c=claim q in
  let retry=D.complete (D.Retry {at=time 20.;reason="busy"}) in
  check "retry" true (D.settle q c retry=D.Applied);
  check "delayed" true (match D.try_take q ~queue:"work" ~lease:(span 10.) with
    | D.Await w -> D.Waiting.next_at w=Some (time 20.) | _ -> false);
  Eio_mock.Clock.set_time clock 20.;
  let next=claim q in
  check "prior settlement remains idempotent" true (D.settle q c
    retry=D.Already_applied);
  check "cancel running" true (D.cancel q id);
  check "revocation fences handler" true
    (D.settle q next (D.complete (D.Succeeded "late"))=D.Lost_claim);
  check "second cancellation" false (D.cancel q id);
  let id=inserted (D.enqueue q (spec ())) in
  check "cancel queued" true (D.cancel q id))
let waits backend () = with_queue backend (fun env clock q ->
  let w=match D.try_take q ~queue:"work" ~lease:(span 10.) with
    | D.Await w -> w | _ -> assert false in
  ignore (D.enqueue q (spec ()));
  Eio.Time.with_timeout_exn env#clock 1. (fun () -> D.Pi.await q w);
  let c=claim q in ignore (D.settle q c (D.complete (D.Succeeded "")));
  let pending=inserted (D.enqueue q (spec ~not_before:(time 30.) ())) in
  Eio.Time.with_timeout_exn env#clock 1. (fun () -> Eio.Fiber.both
    (fun () -> check "delayed take wakes" true
      (D.Claim.id (D.take q ~queue:"work" ~lease:(span 10.))=pending))
    (fun () -> Eio.Fiber.yield (); Eio_mock.Clock.set_time clock 30.));
  check "empty take cancels" true (try
    Eio.Time.with_timeout_exn env#clock 0.01 (fun () ->
      ignore (D.take q ~queue:"empty" ~lease:(span 10.))); false
    with Eio.Time.Timeout -> true);
  ignore (D.enqueue q (spec ~queue:"empty" ()));
  ignore (D.take q ~queue:"empty" ~lease:(span 10.)))
let concurrency backend () = with_queue backend (fun _ _ q ->
  let ids=Eio.Fiber.List.map ~max_fibers:8 (fun _ ->
    D.enqueue q (spec ~key:"one" ())) (List.init 16 Fun.id) in
  count "one insertion" 1 (List.length (List.filter
    (function D.Inserted _ -> true | _ -> false) ids));
  let claims=Eio.Fiber.List.map ~max_fibers:8 (fun _ ->
    D.try_take q ~queue:"work" ~lease:(span 10.)) (List.init 16 Fun.id) in
  count "one claimant" 1 (List.length (List.filter
    (function D.Ready _ -> true | _ -> false) claims)))
let uncertain backend () = with_queue backend (fun _ _ q ->
  let exception Response_lost in
  let after={Duraml_mock.observe=(fun _ -> raise Response_lost)} in
  let mock=Duraml_mock.wrap ~after q in
  let s=spec ~key:"reconcile" () in
  (try ignore (D.enqueue mock s) with Response_lost -> ());
  check "enqueue committed despite response loss" true
    (match D.enqueue q s with D.Existing _ -> true | _ -> false);
  let c=claim q in
  let result=D.complete ~followups:[spec ~queue:"next" ~key:"next" ()]
    (D.Succeeded "done") in
  (try ignore (D.settle mock c result) with Response_lost -> ());
  check "settlement reconciled" true (D.settle q c result=D.Already_applied);
  count "successor not duplicated" 2 (List.length (D.list q (D.query ()))))
let closed backend () = Eio_main.run (fun env ->
  let path=Filename.temp_file "duraml-closed-" ".db" in Unix.unlink path;
  let q=Eio.Switch.run (fun sw ->
    if backend="memory" then
      Duraml_memory.create ~sw ~clock:env#clock ~random:env#secure_random ()
    else Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
      Eio.Path.(env#fs / path)) in
  check "closed resource" true (try ignore (D.list q (D.query ()));false
    with Eio.Io (D.E D.Closed,_) -> true);
  List.iter (fun suffix -> let p=path^suffix in
    if Sys.file_exists p then Sys.remove p) ["";"-wal";"-shm"])
let validation backend () = with_queue backend (fun env _ q ->
  let rejects f = try f ();false with Invalid_argument _ -> true in
  check "empty queue refused" true (rejects (fun () -> ignore (spec ~queue:""
    ())));
  check "negative version refused" true (rejects (fun () -> ignore
    (D.Spec.v ~queue:"q" ~kind:"k" ~version:0 ~payload:"" ())));
  check "payload budget" true (rejects (fun () -> ignore
    (spec ~payload:(String.make (1048576+1) 'a') ())));
  check "invalid time refused" true (rejects (fun () -> ignore
    (spec ~not_before:(time (-1.)) ())));
  check "invalid lease refused" true (rejects (fun () -> ignore
    (D.try_take q ~queue:"work" ~lease:(span 0.))));
  let id=inserted (D.enqueue q (spec ())) in
  let c=claim q in
  check "retry cannot spawn successors" true (rejects (fun () -> ignore
    (D.settle q c (D.complete ~followups:[spec ()]
      (D.Retry {at=time 20.;reason="retry"})))));
  Eio.Switch.run (fun sw ->
    let other=Duraml_memory.create ~sw ~clock:env#clock
      ~random:env#secure_random () in
    ignore (D.enqueue other (spec ()));
    ignore (D.take other ~queue:"work" ~lease:(span 10.));
    check "cross-store claim refused" true
      (D.settle other c (D.complete (D.Succeeded "wrong"))=D.Lost_claim));
  check "original still running" true ((D.Job.state (Option.get (D.find q
    id)))=D.Running);
  let page=D.events ~limit:1 q in
  count "event limit" 1 (List.length page);
  count "exclusive event cursor" 1
    (List.length (D.events ~after:(D.Event.sequence (List.hd page)) q)))
let precise_times backend () = with_queue backend (fun _ clock q ->
  let base=time 1_700_000_000. in
  let early=Option.get (Ptime.add_span base (Ptime.Span.v (0,1L))) in
  let later=Option.get (Ptime.add_span base (Ptime.Span.v (0,2L))) in
  Eio_mock.Clock.set_time clock 1_700_000_000.;
  let second=inserted (D.enqueue q (spec ~not_before:later ())) in
  let first=inserted (D.enqueue q (spec ~not_before:early ())) in
  check "stored timestamp preserves picoseconds" true
    (D.Job.available_at (Option.get (D.find q second))=later);
  check "no rounding into early eligibility" true
    (match D.try_take q ~queue:"work" ~lease:(span 1.) with
     | D.Await w -> D.Waiting.next_at w=Some early | _ -> false);
  let wake=D.Pi.clock_deadline later in
  check "clock boundary rounds upward" true
    (Ptime.compare (D.Pi.time_of_float_s wake) later>=0);
  Eio_mock.Clock.set_time clock wake;
  (* Both became eligible with the same creation time, so the schedule
     no longer orders them and allocation order decides. *)
  check "equal creation time sorts by allocation ID" true
    (D.Claim.id (claim q)=second);
  check "later allocation follows" true (D.Claim.id (claim q)=first);
  let rejects f = try f ();false with Invalid_argument _ -> true in
  check "invalid completion rejected at construction" true
    (rejects (fun () -> ignore (D.complete (D.Retry
      {at=time (-1.);reason="pre-epoch"}))));
  List.iter (fun reading ->
    check "invalid clock reading refused" true
      (rejects (fun () -> ignore (D.Pi.time_of_float_s reading))))
    [Float.nan;infinity;neg_infinity;-1.];
  Eio_mock.Clock.set_time clock 253_402_300_798.;
  ignore (D.enqueue q (spec ()));
  let before=D.list q (D.query ()) in
  check "overflowing lease rejected" true
    (rejects (fun () -> ignore
      (D.try_take q ~queue:"work" ~lease:(span 100.))));
  check "overflow rolls back claim" true (D.list q (D.query ())=before))
(* Dispatch is oldest-first within a lane and round-robin across lanes, so
   retries and recovered claims keep their age and a small submission is
   interleaved with a bulk one. *)
let fairness backend () = with_queue backend (fun _ clock q ->
  let at t = Eio_mock.Clock.set_time clock t in
  let settle c outcome = check "settled" true
    (D.settle q c (D.complete outcome)=D.Applied) in
  at 0.; let a=inserted (D.enqueue q (spec ~key:"a" ())) in
  at 1.; let b=inserted (D.enqueue q (spec ~key:"b" ())) in
  let ca=claim q in
  check "oldest first" true (D.Claim.id ca=a);
  settle ca (D.Retry {at=time 5.;reason="busy"});
  at 2.; ignore (D.enqueue q (spec ~key:"c" ()));
  at 5.;
  let ca=claim q in
  check "retry keeps its age" true (D.Claim.id ca=a);
  let cb=claim q in
  check "then the next oldest" true (D.Claim.id cb=b);
  settle ca (D.Succeeded "ok"); settle cb (D.Succeeded "ok");
  at 6.; let cc=claim q in
  check "no more eligible" true
    (match D.try_take q ~queue:"work" ~lease:(span 10.) with
     | D.Await _ -> true | D.Ready _ -> false);
  at 7.; let young=inserted (D.enqueue q (spec ~key:"young" ())) in
  (* The claim of c expires at 16; it is older than the young job. *)
  at 16.;
  let cc=D.Claim.id cc and recovered=claim q in
  check "expired claim precedes younger work" true (D.Claim.id recovered=cc);
  let cy=claim q in
  check "younger work follows" true (D.Claim.id cy=young);
  settle recovered (D.Succeeded "ok"); settle cy (D.Succeeded "ok");
  (* Lanes: a bulk batch of three and a single ad hoc request. *)
  at 40.;
  let bulk=List.map (fun k -> inserted (D.enqueue q
    (spec ~key:k ~lane:"bulk" ()))) ["b1";"b2";"b3"] in
  at 41.; let adhoc=inserted (D.enqueue q (spec ~key:"x1" ~lane:"adhoc" ()))
  in
  let c1=claim q in
  check "unserved lanes tie by name" true (D.Claim.id c1=adhoc);
  settle c1 (D.Succeeded "ok");
  let c2=claim q in
  check "bulk lane next" true (D.Claim.id c2=List.nth bulk 0);
  at 42.; let adhoc2=inserted (D.enqueue q (spec ~key:"x2" ~lane:"adhoc" ()))
  in
  let c3=claim q in
  check "new ad hoc work interleaves" true (D.Claim.id c3=adhoc2);
  settle c3 (D.Succeeded "ok");
  let c4=claim q in
  check "bulk resumes in age order" true (D.Claim.id c4=List.nth bulk 1);
  (* Successors inherit the lane unless they name their own. *)
  check "successors published" true (D.settle q c2 (D.complete ~followups:[
    spec ~queue:"convert" ~key:"inherit" ();
    spec ~queue:"convert" ~key:"explicit" ~lane:"own" ()] (D.Succeeded "ok"))
    =D.Applied);
  settle c4 (D.Succeeded "ok");
  let c5=claim q in
  check "bulk drains" true (D.Claim.id c5=List.nth bulk 2);
  settle c5 (D.Succeeded "ok");
  let lanes=List.map (fun j -> D.Spec.key (D.Job.spec j),
    D.Spec.lane (D.Job.spec j)) (D.list q (D.query ~queue:"convert" ())) in
  check "successor lanes" true (List.sort compare lanes=
    [Some "explicit","own";Some "inherit","bulk"]);
  (* Lanes schedule; they do not identify work. *)
  let id=inserted (D.enqueue q (spec ~key:"keyed" ~lane:"one" ())) in
  check "keyed repeat in another lane reconciles" true
    (D.enqueue q (spec ~key:"keyed" ~lane:"two" ())=D.Existing id);
  check "stored lane kept" true
    (D.Spec.lane (D.Job.spec (Option.get (D.find q id)))="one");
  check "lane validated" true (try
    ignore (spec ~lane:"bad\000" ()); false with Invalid_argument _ -> true))
(* A lane that gains work joins the back of the rotation, so lanes created
   on demand cannot keep preempting a lane already waiting. *)
let rotation backend () = with_queue backend (fun _ clock q ->
  let at t = Eio_mock.Clock.set_time clock t in
  let done_ c = check "settled" true
    (D.settle q c (D.complete (D.Succeeded "ok"))=D.Applied) in
  at 0.;
  let z1=inserted (D.enqueue q (spec ~key:"z1" ~lane:"z" ())) in
  let z2=inserted (D.enqueue q (spec ~key:"z2" ~lane:"z" ())) in
  let c=claim q in check "first lane served" true (D.Claim.id c=z1); done_ c;
  at 1.; let a=inserted (D.enqueue q (spec ~key:"a" ~lane:"a" ())) in
  let c=claim q in check "new lane ties by name" true (D.Claim.id c=a);
  done_ c;
  at 2.; let b=inserted (D.enqueue q (spec ~key:"b" ~lane:"b" ())) in
  let c=claim q in
  check "waiting lane precedes a newer lane" true (D.Claim.id c=z2); done_ c;
  let c=claim q in check "newer lane follows" true (D.Claim.id c=b); done_ c;
  (* An emptied lane is forgotten and rejoins at the back. *)
  at 3.; let z3=inserted (D.enqueue q (spec ~key:"z3" ~lane:"z" ())) in
  let a2=inserted (D.enqueue q (spec ~key:"a2" ~lane:"a" ())) in
  let c=claim q in check "rejoined lanes tie by name" true (D.Claim.id c=a2);
  done_ c;
  let c=claim q in check "then the other" true (D.Claim.id c=z3); done_ c)
let core_validation backend () = with_queue backend (fun _ _ q ->
  let rejects f = try f (); false with Invalid_argument _ -> true in
  List.iter (fun s -> check ("rejects ID " ^ s) true
    (Result.is_error (D.Id.of_string s)))
    ["0x1";"+1";"0b1";"1_0";"01";"0";"-1";"";"9223372036854775808"];
  check "accepts canonical ID" true
    (Result.map D.Id.to_string (D.Id.of_string "42")=Ok "42");
  check "conflicting successors rejected" true (rejects (fun () ->
    ignore (D.complete ~followups:[spec ~key:"k" ~payload:"a" ();
      spec ~key:"k" ~payload:"b" ()] (D.Succeeded ""))));
  check "identical successors allowed" true (not (rejects (fun () ->
    ignore (D.complete ~followups:[spec ~key:"k" ();spec ~key:"k" ()]
      (D.Succeeded "")))));
  let huge=Option.get (Ptime.Span.of_d_ps (365*1_000_000, 0L)) in
  check "oversized lease rejected on an empty queue" true (rejects (fun () ->
    ignore (D.try_take q ~queue:"work" ~lease:huge))))
(* Names of only Unicode white space and an explicit empty lane are refused,
   a page stops at its byte budget, and counts follow state changes. *)
let pages backend () = with_queue backend (fun _ _ q ->
  let rejects f = try f (); false with Invalid_argument _ -> true in
  check "non-breaking space queue rejected" true
    (rejects (fun () -> ignore (spec ~queue:"\xc2\xa0\xe3\x80\x80" ())));
  check "empty lane rejected" true (rejects (fun () -> ignore (spec ~lane:"" ())));
  check "name with a space accepted" true
    (not (rejects (fun () -> ignore (spec ~queue:"a\xc2\xa0b" ()))));
  let big=String.make 1048576 'x' in
  for _=1 to 20 do ignore (D.enqueue q (spec ~payload:big ())) done;
  ignore (D.enqueue q (spec ()));
  let first=D.list q (D.query ~limit:100 ()) in
  count "page ends at 16 MiB" 16 (List.length first);
  let last=D.Job.id (List.nth first 15) in
  count "rest follows" 5 (List.length (D.list q (D.query ~after:last ())));
  count "queued counted" 21 (D.count q (D.query ~state:D.Queued ()));
  let c=claim q in
  ignore (D.settle q c (D.complete (D.Succeeded "")));
  count "queued after settle" 20 (D.count q (D.query ~state:D.Queued ()));
  count "done after settle" 1
    (D.count q (D.query ~queue:"work" ~state:D.Done ()));
  count "other queue empty" 0 (D.count q (D.query ~queue:"other" ()));
  count "all jobs" 21 (D.count q (D.query ())))
let () = Alcotest.run "Duraml backends" (List.map (fun backend -> backend,
  List.map (fun (name,test) -> Alcotest.test_case name `Quick (test backend))
    ["deduplication and queries",dedup;"lease fencing",fencing;
     "atomic successors",atomic_successors;
     "retry and cancellation",retry_cancel;
     "waiting and cancellation",waits;"competing fibers",concurrency;
     "uncertain commit",uncertain;"switch lifetime",closed;
     "validation and isolation",validation;
     "typed time precision and bounds",precise_times;
     "lane fairness and age order",fairness;
     "lane rotation",rotation;"identifiers and bounds",core_validation;
     "pages and counts",pages])
    ["memory";"sqlite"])
