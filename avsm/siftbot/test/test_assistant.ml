module M = Siftbot_model
module S = Siftbot.Store
module A = Agentkit

let check name b = if not b then failwith name

let () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let db = S.memory ~sw () in
          Siftbot.Demo.seed db;
          let before = (S.snapshot db ~query:"").assignments in
          let calls = ref 0 in
          let complete (r : A.Chat.request) =
            incr calls;
            check "bounded request" (A.Chat.text_bytes r <= 65536);
            if !calls = 1 then
              A.Chat.response
                ~calls:
                  [
                    A.Agent.
                      { id = "bad"; name = "commit_rule"; arguments = "{}" };
                    A.Agent.
                      {
                        id = "search";
                        name = "search";
                        arguments = {|{"query":"Geotessera"}|};
                      };
                  ]
                ~finish:Tool_calls None
            else (
              check "source data permission boundary"
                (match A.Chat.system_text r.messages with
                | Some s -> String.length s > 100
                | None -> false);
              A.Chat.response ~finish:Stop
                (Some "Review the Geotessera evidence."))
          in
          let answer =
            Siftbot.Assistant.discuss db ~complete ~model:"fake/v1"
              ~selected:None "Compare the teaching and research evidence"
          in
          check "answer" (answer = "Review the Geotessera evidence.");
          check "model cannot commit rules"
            ((S.snapshot db ~query:"").assignments = before);
          check "answer persisted"
            ((S.snapshot db ~query:"").discussion = answer);
          let complete _ = failwith "model unavailable" in
          check "failed turn propagates"
            (match
               Siftbot.Assistant.discuss db ~complete ~model:"fake/v1"
                 ~selected:None "Question"
             with
            | _ -> false
            | exception Failure _ -> true);
          check "failed turn leaves decisions"
            ((S.snapshot db ~query:"").assignments = before);
          check "failed turn audited"
            ((S.snapshot db ~query:"").discussion
           = "Discussion failed. No review decisions were changed.");
          let inference_calls = ref 0 in
          let complete _ =
            incr inference_calls;
            A.Chat.response
              (Some
                 {|[{"dimension":"triage","value":"relevant","score":0.8,"explanation":"Supervision request"},{"dimension":"delivery","value":"direct","score":0.7,"explanation":"Personal request"},{"dimension":"attention","value":"needs-reply","score":0.8,"explanation":"Please review"}]|})
          in
          check "bounded model classification"
            (Siftbot.Classifier.run db ~complete ~model:"fake/v1"
               ~max_messages:1
            = 1);
          let first_calls = !inference_calls in
          ignore
            (Siftbot.Classifier.run db ~complete ~model:"fake/v1"
               ~max_messages:0);
          check "zero model budget" (!inference_calls = first_calls);
          let db2 = S.memory ~sw () in
          let message = List.hd Siftbot.Demo.messages in
          S.commit_batch db2 ~job:"fixture" ~expected:None ~cursor:"1"
            [ message ] [];
          ignore
            (Siftbot.Classifier.run db2 ~complete ~model:"fake/v1"
               ~max_messages:1);
          check "unchanged inference cache"
            (Siftbot.Classifier.run db2 ~complete ~model:"fake/v1"
               ~max_messages:1
            = 0);
          ignore
            (Siftbot.Classifier.run db2 ~complete ~model:"fake/v2"
               ~max_messages:1);
          let calls_before = !inference_calls in
          ignore
            (Siftbot.Classifier.run db2 ~complete ~model:"fake/v1"
               ~max_messages:1);
          check "returning to cached provider reuses inference"
            (!inference_calls = calls_before);
          check "provider suggestions replace superseded predictions"
            (List.length (S.snapshot db2 ~query:"").assignments = 3);
          print_endline "Agentkit evidence boundary tests passed"))
