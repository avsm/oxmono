module S = Siftbot.Store
module M = Siftbot_model

let check name ok = if not ok then failwith name

let response =
  {|[{"dimension":"triage","value":"relevant","score":0.8,"explanation":"Evidence"},{"dimension":"delivery","value":"direct","score":0.8,"explanation":"Evidence"},{"dimension":"attention","value":"fyi","score":0.8,"explanation":"Evidence"}]|}

let reply () = Agentkit.Chat.response (Some response)

let seed ~sw count =
  let db = S.memory ~sw () in
  let base = List.hd Siftbot.Demo.messages in
  let messages =
    List.init count (fun i ->
        let id = Printf.sprintf "parallel-%02d" i in
        { base with source = { base.source with id }; subject = id })
  in
  S.commit_batch db ~job:"fixture" ~expected:None ~cursor:"1" messages [];
  (db, messages)

let () =
  Eio_main.run (fun env ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
          Eio.Switch.run (fun sw ->
              let db, _ = seed ~sw 8 in
              let ready, release = Eio.Promise.create () in
              let saved, release_first = Eio.Promise.create () in
              let active = ref 0 and peak = ref 0 and calls = ref 0 in
              let first_finished = ref false and results = ref [] in
              let complete _ =
                incr calls;
                let n = !calls in
                incr active;
                peak := max !peak !active;
                Fun.protect
                  ~finally:(fun () -> decr active)
                  (fun () ->
                    if n = 4 then Eio.Promise.resolve release ();
                    Eio.Promise.await ready;
                    if n = 1 then (
                      Eio.Promise.await saved;
                      first_finished := true);
                    reply ())
              in
              let on_classified ~subject _ =
                if !results = [] then (
                  check "out-of-order completions save immediately"
                    (not !first_finished);
                  Eio.Promise.resolve release_first ());
                results := subject :: !results;
                Eio.Fiber.yield ()
              in
              let counts = ref [] in
              check "parallel batch completes"
                (Siftbot.Classifier.run ~jobs:4 db ~model:"test" ~complete
                   ~on_classified ~on_progress:(fun p ->
                     counts := p.Siftbot.Classifier.completed :: !counts)
                = 8);
              check "exactly four model calls in flight"
                (!peak = 4 && !active = 0);
              check "every result saved exactly once"
                (List.length (List.sort_uniq String.compare !results) = 8);
              check "progress is monotonic despite yielding callbacks"
                (List.rev !counts = List.init 9 Fun.id);
              check "parallel results remain incremental"
                (Siftbot.Classifier.run ~jobs:4 db ~model:"test"
                   ~complete:(fun _ -> failwith "Cache miss")
                = 0);
              let before = (S.snapshot db ~query:"").assignments in
              let previous = List.hd before in
              let revision = S.revision db in
              check "invalid prediction rolls back the entire result"
                (try
                   ignore
                     (S.save_classification db ~expected_revision:revision
                        ~source:previous.target
                        ~revision:previous.source_revision ~kind:"rejected"
                        ~model:"test" ~prompt_version:"test"
                        ~question:"synthetic" ~answer:"synthetic"
                        [
                          {
                            previous with
                            dimension = Area;
                            value = "unapproved";
                          };
                        ]);
                   false
                 with Invalid_argument _ -> true);
              check "rollback retains old predictions and audit revision"
                ((S.snapshot db ~query:"").assignments = before
                && S.revision db = revision
                && S.cached db ~source:previous.target ~kind:"rejected" = None);

              let db, _ = seed ~sw 4 in
              let ready, release = Eio.Promise.create () in
              let saved, release_failure = Eio.Promise.create () in
              let started = ref 0 and active = ref 0 in
              let complete _ =
                incr started;
                let n = !started in
                incr active;
                Fun.protect
                  ~finally:(fun () -> decr active)
                  (fun () ->
                    if n = 4 then Eio.Promise.resolve release ();
                    Eio.Promise.await ready;
                    if n = 1 then reply ()
                    else if n = 2 then (
                      Eio.Promise.await saved;
                      failwith "Provider failed")
                    else Eio.Fiber.await_cancel ())
              in
              check "failed worker cancels other requests"
                (try
                   ignore
                     (Siftbot.Classifier.run ~jobs:4 db ~model:"test" ~complete
                        ~on_classified:(fun ~subject:_ _ ->
                          Eio.Promise.resolve release_failure ()));
                   false
                 with Failure s -> s = "Provider failed");
              check "cancelled requests have ended" (!active = 0);
              check "retry resumes only unsaved work"
                (Siftbot.Classifier.run ~jobs:4 db ~model:"test"
                   ~complete:(fun _ -> reply ())
                = 3);

              let db, messages = seed ~sw 4 in
              let ready, release = Eio.Promise.create () in
              let started = ref 0 in
              let complete _ =
                incr started;
                let n = !started in
                if n = 4 then Eio.Promise.resolve release ();
                Eio.Promise.await ready;
                if n = 1 then (
                  let m = List.hd messages in
                  S.commit_batch db ~job:"fixture" ~expected:(Some "1")
                    ~cursor:"2"
                    [ { m with revision = "changed" } ]
                    [];
                  reply ())
                else Eio.Fiber.await_cancel ()
              in
              check "external changes still reject stale inference"
                (try
                   ignore
                     (Siftbot.Classifier.run ~jobs:4 db ~model:"test" ~complete);
                   false
                 with S.Conflict -> true);
              check "conflicting batch saved no predictions"
                ((S.snapshot db ~query:"").assignments = []);
              print_endline
                "Parallel inference limits, progress, cancellation and \
                 conflicts passed")))
