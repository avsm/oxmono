module D = Duraml
module C = Duraml_client
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let integer = C.Codec.v ~encode:string_of_int ~decode:(fun s ->
  match int_of_string_opt s with Some n -> Ok n | None -> Error "Not an int")
let binding ?(name="numbers") ?(version=1) store = C.Queue.v store ~name
  ~kind:"square" ~version ~request:integer ~result:integer
let added = function C.Queue.Added job -> job | _ -> Alcotest.fail "Not added"
let with_store backend f = Eio_main.run (fun env ->
  let path=Filename.temp_file "duraml-client-" ".db" in Unix.unlink path;
  Fun.protect ~finally:(fun () -> List.iter (fun suffix ->
    let p=path^suffix in if Sys.file_exists p then Sys.remove p)
    ["";"-wal";"-shm"]) (fun () -> Eio.Switch.run (fun sw ->
      let open_store () = if backend="memory" then
        Duraml_memory.create ~sw ~clock:env#clock ~random:env#secure_random ()
      else Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
        Eio.Path.(env#fs / path) in
      let store=open_store () in f env sw store open_store)))
let run_one env queue handler =
  let stop,resolve=Eio.Promise.create () in
  C.Queue.run ~mono_clock:env#mono_clock ~concurrency:1 ~stop
    ~on_settled:(fun job _ -> match C.Job.inspect job with
      | C.Job.Queued | Running -> ()
      | _ -> Eio.Promise.resolve resolve ()) queue handler
let roundtrip backend () = with_store backend (fun env _ store reopen ->
  let queue=binding store in
  let job=added (C.Queue.submit ~key:"one" queue 7) in
  check "inspect queued" true (C.Job.inspect job=C.Job.Queued);
  check "deduplicate" true (match C.Queue.submit ~key:"one" queue 7 with
    | Existing old -> C.Job.id old=C.Job.id job | _ -> false);
  check "different payload conflicts" true
    (C.Queue.submit ~key:"one" queue 8=C.Queue.Conflict (C.Job.id job));
  check "no context outside handlers" true (C.attempt ()=None);
  let seen=ref None in
  run_one env queue (fun ~sw n ->
    seen:=C.attempt ();
    Eio.Fiber.fork ~sw (fun () ->
      if C.attempt ()<> !seen then Alcotest.fail "Context not inherited");
    C.Completion.succeed (n*n));
  check "handler context names the attempt" true
    (!seen=Some (C.Job.id job,1));
  check "typed result" true (C.Job.inspect job=C.Job.Done 49);
  let store=if backend="sqlite" then reopen () else store in
  let restored=C.Queue.find (binding store) (C.Job.id job) in
  check "restore result capability" true
    (Option.map C.Job.inspect restored=Some (C.Job.Done 49));
  check "wrong schema cannot restore handle" true
    (C.Queue.find (binding ~version:2 store) (C.Job.id job)=None);
  check "wrong queue cannot restore handle" true
    (C.Queue.find (binding ~name:"other" store) (C.Job.id job)=None))
let successors backend () = with_store backend (fun env _ store _ ->
  let first=binding store in
  let second=binding ~name:"next" store in
  let staged=C.Queue.stage ~key:"successor" second 8 in
  count "staging has no durable effect" 0
    (List.length (D.list store (D.query ())));
  let job=added (C.Queue.submit first 2) in
  run_one env first (fun ~sw:_ n ->
    C.Completion.succeed ~followups:[staged] (n*n));
  check "source done" true (C.Job.inspect job=C.Job.Done 4);
  let child=match C.Queue.submit ~key:"successor" second 8 with
    | C.Queue.Existing job -> job | _ -> Alcotest.fail "Missing successor" in
  run_one env second (fun ~sw:_ n -> C.Completion.succeed (n*n));
  check "successor decoded and processed" true (C.Job.inspect child=C.Job.Done
    64))
let retry_failure backend () = with_store backend (fun env _ store _ ->
  let queue=binding store in
  let job=added (C.Queue.submit queue 1) in
  let calls=ref 0 in
  run_one env queue (fun ~sw:_ _ -> incr calls;
    if !calls=1 then C.Completion.retry ~at:Ptime.epoch ~reason:"busy"
    else C.Completion.fail ~code:"not_found" ~detail:"No document" ());
  count "retry handler called twice" 2 !calls;
  check "typed failure" true
    (C.Job.inspect job=C.Job.Failed {code="not_found";detail="No document"});
  let other=added (C.Queue.submit queue 2) in
  check "cancel through handle" true (C.Job.cancel other);
  check "typed cancellation" true (C.Job.inspect other=C.Job.Cancelled))
let malformed backend () = with_store backend (fun env _ store _ ->
  let queue=binding store in
  let id=match D.enqueue store (D.Spec.v ~queue:"numbers" ~kind:"square"
    ~version:1 ~payload:"not-an-int" ()) with
    | D.Inserted id -> id | _ -> assert false in
  run_one env queue (fun ~sw:_ _ -> Alcotest.fail "Decoded malformed input");
  let job=Option.get (C.Queue.find queue id) in
  check "decode failure settled" true (C.Job.inspect job=C.Job.Failed
    {code="invalid_payload";detail="Request decoding failed"});
  let bad=C.Codec.v ~encode:string_of_int
    ~decode:(fun _ -> Error "secret decoder diagnostic") in
  let queue=C.Queue.v store ~name:"bad" ~kind:"k" ~version:1
    ~request:bad ~result:integer in
  ignore (C.Queue.submit queue 1);
  run_one env queue (fun ~sw:_ _ -> Alcotest.fail "Handler invoked");
  check "audit omits decoder diagnostic" true (List.for_all
    (fun e -> D.Event.detail e<>"secret decoder diagnostic") (D.events store)))
let schema backend () = with_store backend (fun env _ store _ ->
  let old=binding store in
  let job=added (C.Queue.submit old 2) in
  let newer=binding ~version:2 store in
  check "schema mismatch stops runner" true (try
    C.Queue.run ~mono_clock:env#mono_clock ~concurrency:1 newer
      (fun ~sw:_ _ -> Alcotest.fail "Wrong schema handler invoked"); false
    with C.Queue.Schema_mismatch id -> id=C.Job.id job);
  check "incompatible work retained" true (C.Job.inspect job=C.Job.Running))
let conflicts backend () = with_store backend (fun env sw store _ ->
  let queue=binding store and next=binding ~name:"next" store in
  ignore (C.Queue.submit ~key:"occupied" next 1);
  let fresh=C.Queue.stage ~key:"fresh" next 3 in
  let conflict=C.Queue.stage ~key:"occupied" next 2 in
  let job=added (C.Queue.submit queue 4) in
  check "successor conflict stops runner" true (try
    run_one env queue (fun ~sw:_ n ->
      C.Completion.succeed ~followups:[fresh;conflict] n); false
    with Eio.Io (D.E (D.Storage _),_) -> true);
  check "source not settled" true (C.Job.inspect job=C.Job.Running);
  count "earlier successor rolled back" 2
    (List.length (D.list store (D.query ())));
  let other=Duraml_memory.create ~sw ~clock:env#clock
    ~random:env#secure_random () in
  let foreign=C.Queue.stage (binding other) 9 in
  let another=binding ~name:"another" store in
  let job=added (C.Queue.submit another 8) in
  check "cross-store successors refused" true (try
    run_one env another (fun ~sw:_ n ->
      C.Completion.succeed ~followups:[foreign] n); false
    with Invalid_argument _ -> true);
  check "cross-store refusal preserves source" true
    (C.Job.inspect job=C.Job.Running);
  count "foreign store untouched" 0 (List.length (D.list other (D.query ()))))
let result_errors backend () = with_store backend (fun env _ store _ ->
  let queue=binding store in
  let job=added (C.Queue.submit queue 3) in
  let c=D.take store ~queue:"numbers" ~lease:(Ptime.Span.of_int_s 60) in
  ignore (D.settle store c (D.complete (D.Succeeded "not an integer")));
  check "invalid stored result reported" true
    (C.Job.inspect job=C.Job.Invalid_result "Not an int");
  let oversized=C.Codec.v ~encode:(fun _ -> String.make 1048577 'a')
    ~decode:(fun _ -> Ok 0) in
  let queue=C.Queue.v store ~name:"oversized" ~kind:"k" ~version:1
    ~request:integer ~result:oversized in
  let job=added (C.Queue.submit queue 2) in
  check "encoded result budget enforced" true (try
    run_one env queue (fun ~sw:_ n -> C.Completion.succeed n);false
    with Invalid_argument _ -> true);
  check "failed encoding leaves claim recoverable" true
    (C.Job.inspect job=C.Job.Running))
let structured backend () = with_store backend (fun env _ store _ ->
  let queue=binding store in
  let job=added (C.Queue.submit queue 5) in
  let started,start=Eio.Promise.create () and
    stop,request_stop=Eio.Promise.create
    () in
  let cleaned=ref false in
  Eio.Switch.run (fun sw ->
    C.Queue.start ~sw ~mono_clock:env#mono_clock ~concurrency:1 ~stop
      ~drain:Ptime.Span.zero queue (fun ~sw:_ _ ->
        Fun.protect ~finally:(fun () -> cleaned:=true) (fun () ->
          Eio.Promise.resolve start (); Eio.Fiber.await_cancel ()));
    Eio.Promise.await started; Eio.Promise.resolve request_stop ());
  check "switch joins handler cleanup" true !cleaned;
  check "interrupted work recoverable" true (C.Job.inspect job=C.Job.Running))
let json backend () = with_store backend (fun env _ store _ ->
  let queue=C.Queue.v store ~name:"json" ~kind:"sum" ~version:1
    ~request:(Duraml_jsont.codec (Jsont.list Jsont.int))
    ~result:(Duraml_jsont.codec Jsont.int) in
  let job=added (C.Queue.submit queue [1;2;3]) in
  run_one env queue (fun ~sw:_ xs ->
    C.Completion.succeed (List.fold_left (+) 0 xs));
  check "optional JSON adapter" true (C.Job.inspect job=C.Job.Done 6))
let () = Alcotest.run "Duraml typed client" (List.map (fun backend ->
  backend,List.map (fun (name,test) -> Alcotest.test_case name `Quick
    (test backend)) ["typed submission and reopening",roundtrip;
    "atomic typed successors",successors;"retry and failure",retry_failure;
    "decode failures",malformed;"schema mismatch retains work",schema;
    "successor rollback and isolation",conflicts;"result errors",result_errors;
    "switch-owned workers",structured;"JSON adapter",json]) ["memory";"sqlite"])
