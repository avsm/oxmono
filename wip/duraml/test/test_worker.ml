module D = Duraml
let time s = Option.get (Ptime.of_float_s s)
let span s = Option.get (Ptime.Span.of_float_s s)
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let random () = Eio.Flow.string_source
  (String.init 65536 (fun i -> Char.chr (i mod 251)))
let spec () = D.Spec.v ~queue:"work" ~kind:"test" ~version:1 ~payload:"" ()
let setup f = Eio_mock.Backend.run_full (fun env -> Eio.Switch.run (fun sw ->
  let q=Duraml_memory.create ~sw ~clock:env#clock ~random:(random ()) () in
  f env q))
let renewing () = setup (fun env q ->
  ignore (D.enqueue q (spec ()));
  let done_,resolve=Eio.Promise.create () in
  let calls=ref 0 in
  Eio.Fiber.first
    (fun () -> Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"work"
      ~concurrency:1 ~lease:(span 3.) ~on_settled:(fun _ s ->
        check "worker settled" true (s=D.Applied);Eio.Promise.resolve resolve
          ())
      q (fun ~sw:_ _ -> incr calls; Eio.Time.sleep env#clock 20.;
        D.complete (D.Succeeded "ok")))
    (fun () -> Eio.Promise.await done_);
  count "long handler not reclaimed" 1 !calls;
  check "finished" true ((D.Job.state (List.hd (D.list q (D.query
    ()))))=D.Done))
let retries () = setup (fun env q ->
  ignore (D.enqueue q (spec ()));
  let done_,resolve=Eio.Promise.create () in
  let calls=ref 0 in
  Eio.Fiber.first
    (fun () -> Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"work"
      ~concurrency:1 ~max_attempts:3
      ~on_settled:(fun _ _ -> if (D.Job.state (List.hd (D.list q (D.query
        ()))))=D.Failed_job
        then Eio.Promise.resolve resolve ()) q (fun ~sw:_ _ ->
        incr calls; D.complete (D.Retry {at=time (Eio.Time.now
          env#clock+.1.);reason="busy"})))
    (fun () -> Eio.Promise.await done_);
  count "bounded attempts" 3 !calls)
let revoked () = setup (fun env q ->
  let id=match D.enqueue q (spec ()) with D.Inserted id -> id | _ -> assert
    false in
  let started,start=Eio.Promise.create () and cleaned,clean=Eio.Promise.create
    () in
  Eio.Fiber.first
    (fun () -> Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"work"
      ~concurrency:1 ~lease:(span 3.) q (fun ~sw:_ _ ->
        Fun.protect ~finally:(fun () -> Eio.Promise.resolve clean ())
          (fun () -> Eio.Promise.resolve start (); Eio.Fiber.await_cancel ())))
    (fun () -> Eio.Promise.await started; ignore (D.cancel q id);
      Eio.Promise.await cleaned);
  check "cancelled remains terminal" true ((D.Job.state (Option.get (D.find q
    id)))=D.Cancelled))
let concurrency () = setup (fun env q ->
  for _=1 to 8 do ignore (D.enqueue q (spec ())) done;
  let active=ref 0 and peak=ref 0 and finished=ref 0 in
  let done_,resolve=Eio.Promise.create () in
  Eio.Fiber.first
    (fun () -> Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"work"
      ~concurrency:3 ~on_settled:(fun _ _ -> incr finished;
        if !finished=8 then Eio.Promise.resolve resolve ()) q (fun ~sw:_ _ ->
        incr active;peak:=max !peak !active;
        Eio.Time.sleep env#clock 2.;decr active;
        D.complete (D.Succeeded "")))
    (fun () -> Eio.Promise.await done_);
  count "bounded concurrency" 3 !peak;
  count "all completed" 8 !finished)
let handler_failure () = setup (fun env q ->
  ignore (D.enqueue q (spec ()));
  ignore (D.enqueue q (spec ()));
  let started=ref 0 and cleaned=ref 0 in
  let exception Handler_failed in
  check "unexpected failure propagated" true (try
    Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"work" ~concurrency:2
      q (fun ~sw:_ _ ->
        incr started;
        Fun.protect ~finally:(fun () -> incr cleaned) (fun () ->
          if !started=1 then (Eio.Time.sleep env#clock 1.;raise Handler_failed)
          else Eio.Fiber.await_cancel ()));
    false
  with Handler_failed -> true);
  count "sibling handlers cleaned" 2 !cleaned;
  check "uncertain effects retain claims" true (List.for_all
    (fun (j:D.job) -> (D.Job.state j)=D.Running) (D.list q (D.query ()))))
let draining () = setup (fun env q ->
  for _=1 to 3 do ignore (D.enqueue q (spec ())) done;
  let stop,request_stop=Eio.Promise.create () in
  let calls=ref 0 in
  Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"work"
    ~concurrency:1 ~stop ~drain:(span 10.) q (fun ~sw:_ _ ->
      incr calls; Eio.Promise.resolve request_stop ();
      Eio.Time.sleep env#clock 5.; D.complete (D.Succeeded "done"));
  count "no new handlers after stop" 1 !calls;
  count "active work drained" 1 (List.length (D.list q (D.query ~state:D.Done
    ())));
  count "remaining work queued" 2 (List.length (D.list q (D.query
    ~state:D.Queued ()))))
let drain_timeout () = setup (fun env q ->
  ignore (D.enqueue q (spec ()));
  let stop,request_stop=Eio.Promise.create () in
  let cleaned=ref false in
  Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"work"
    ~concurrency:1 ~stop ~drain:(span 2.) q (fun ~sw:_ _ ->
      Eio.Promise.resolve request_stop ();
      Fun.protect ~finally:(fun () -> cleaned:=true)
        (fun () -> Eio.Fiber.await_cancel ()));
  check "drain timeout joins cleanup" true !cleaned;
  check "unfinished claim expires later" true
    ((D.Job.state (List.hd (D.list q (D.query ()))))=D.Running))
let scripted () =
  let seen=ref [] in
  let call : type a. a D.Pi.request -> a = fun request ->
    seen:=D.Pi.operation request::!seen;
    match request with D.Pi.Find _ -> None
    | _ -> failwith "Unexpected scripted call" in
  let q=Duraml_mock.make ~await:(fun _ -> failwith "Unexpected wait") {call} in
  check "scripted result" true (D.find q (Result.get_ok (D.Id.of_string
    "1"))=None);
  check "operation recorded" true (!seen=["find"])
let shortened_lease () = setup (fun env q ->
  ignore (D.enqueue q (spec ()));
  let c=D.take q ~queue:"work" ~lease:(span 100.) in
  let start=Eio.Time.now env#clock in
  Eio.Fiber.both
    (fun () ->
      let next=D.take q ~queue:"work" ~lease:(span 10.) in
      count "expired attempt reclaimed" 2 (D.Claim.attempt next);
      check "consumer observes shortened deadline" true
        (Eio.Time.now env#clock <= start+.2.))
    (fun () ->
      Eio.Time.sleep env#clock 1.;
      let before=D.events q in
      check "shorten live lease" true (D.renew q c ~lease:(span 1.));
      check "renewal does not add audit noise" true (D.events q=before)))
let invalid_final_retry () = setup (fun env q ->
  ignore (D.enqueue q (spec ()));
  let stop,resolve=Eio.Promise.create () in
  check "invalid retry fails even on last attempt" true (try
    Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"work"
      ~concurrency:1 ~max_attempts:1 ~stop
      ~on_settled:(fun _ _ -> Eio.Promise.resolve resolve ())
      q (fun ~sw:_ _ ->
        D.complete ~followups:[spec ()]
          (D.Retry {at=time 0.;reason="invalid successors"}));
    false
  with Invalid_argument _ -> true);
  check "invalid result does not settle" true
    ((D.Job.state (List.hd (D.list q (D.query ()))))=D.Running))
let () = Alcotest.run "Duraml workers" ["worker", List.map (fun (n,f) ->
  Alcotest.test_case n `Quick f) ["renewal",renewing;"bounded retries",retries;
    "revocation cleanup",revoked;"concurrency",concurrency;
    "scripted mock",scripted;"failure cancels siblings",handler_failure;
    "graceful drain",draining;"bounded drain",drain_timeout;
    "shortened lease wakes consumer",shortened_lease;
    "invalid final retry",invalid_final_retry]]
