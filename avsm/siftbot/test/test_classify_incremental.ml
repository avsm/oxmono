module S = Siftbot.Store
module M = Siftbot_model

let check name ok = if not ok then failwith name

let response =
  {|[{"dimension":"triage","value":"relevant","score":0.8,"explanation":"Evidence"},{"dimension":"delivery","value":"direct","score":0.8,"explanation":"Evidence"},{"dimension":"attention","value":"fyi","score":0.8,"explanation":"Evidence"}]|}

let () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let db = S.memory ~sw () in
          let base = List.hd Siftbot.Demo.messages in
          let message id = { base with source = { base.source with id } } in
          let first = message "first"
          and second = message "second"
          and third = message "third" in
          S.commit_batch db ~job:"fixture" ~expected:None ~cursor:"1"
            [ first; second; third ] [];
          let calls = ref 0 and saved = ref [] and progress = ref [] in
          let complete _ =
            incr calls;
            Agentkit.Chat.response (Some response)
          in
          let on_classified ~subject assignments =
            check "saved subject" (subject = base.subject);
            let source = (List.hd assignments).M.target in
            check "callback runs after saving the active cache"
              (Option.is_some
                 (S.cached db ~source ~kind:"classification/active"));
            saved := source :: !saved
          in
          let run ?max_messages () =
            saved := [];
            progress := [];
            Siftbot.Classifier.run ?max_messages db ~complete ~model:"test"
              ~on_classified ~on_progress:(fun p -> progress := p :: !progress)
          in
          check "batch remains bounded" (run ~max_messages:1 () = 1);
          check "all drains the remainder" (run () = 2 && !calls = 3);
          check "overall progress includes all pending work"
            (List.rev !progress
            = Siftbot.Classifier.
                [
                  { completed = 0; total = 2; remaining = 2 };
                  { completed = 1; total = 2; remaining = 1 };
                  { completed = 2; total = 2; remaining = 0 };
                ]);
          check "repeat pass makes no model calls"
            (run () = 0 && !calls = 3 && !saved = []);
          let changed = { first with revision = "changed"; text = "Updated" } in
          let fresh = message "fresh" in
          S.commit_batch db ~job:"fixture" ~expected:(Some "1") ~cursor:"2"
            [ changed; second; fresh ] [];
          check "only changed and new mail needs inference"
            (run () = 2
            && !calls = 5
            && List.sort compare !saved
               = List.sort compare [ first.source; fresh.source ]);
          S.commit_batch db ~job:"fixture" ~expected:(Some "2") ~cursor:"3"
            [ message "resume-a"; message "resume-b" ]
            [];
          let interrupted ~subject assignments =
            on_classified ~subject assignments;
            raise Sys.Break
          in
          check "interruption propagates after saving a result"
            (try
               ignore
                 (Siftbot.Classifier.run db ~complete ~model:"test"
                    ~on_classified:interrupted);
               false
             with Sys.Break -> true);
          check "resume classifies only the unsaved result"
            (run () = 1 && !calls = 7);
          check "model suggestions are not confirmed examples"
            (S.confirmed_assignments db ~limit:20 = []);
          let decision : M.assignment =
            {
              target = changed.source;
              conversation = false;
              dimension = Triage;
              value = "relevant";
              origin = Admin;
              version = "review";
              source_revision = changed.revision;
              explanation = "Confirm an example";
              evidence = [ changed.source ];
              score = None;
            }
          in
          S.edit db ~revision:(S.revision db) (Assign decision)
            ~explanation:"Confirm an example";
          check "current human decision is classifier context"
            (S.confirmed_assignments db ~limit:20 = [ decision ]);
          check "changed confirmed context refreshes all current results"
            (run () = 6 && !calls = 13);
          S.commit_batch db ~job:"fixture" ~expected:(Some "3") ~cursor:"4"
            [ { changed with revision = "changed-again" } ]
            [];
          check "stale human decisions are excluded from examples"
            (S.confirmed_assignments db ~limit:20 = []);
          print_endline
            "Full classification, incremental updates and resume passed"))
