module S = Siftbot.Store
module M = Siftbot_model

let check name ok = if not ok then failwith name

let input_codec =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem "message" M.message_jsont ~enc:Fun.id
  |> Jsont.Object.finish

let response =
  {|[{"dimension":"triage","value":"relevant","score":0.8,"explanation":"Evidence"},{"dimension":"delivery","value":"direct","score":0.8,"explanation":"Evidence"},{"dimension":"attention","value":"fyi","score":0.8,"explanation":"Evidence"}]|}

let () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let base = List.hd Siftbot.Demo.messages in
          let headers =
            List.init 138 (fun _ -> ("Received", String.make 350 'x'))
            @ [
                ("List-Id", "synthetic.example");
                ("Auto-Submitted", "auto-generated");
              ]
          in
          List.iter
            (fun (m : M.message) ->
              let db = S.memory ~sw () in
              S.commit_batch db ~job:"fixture" ~expected:None ~cursor:"1" [ m ]
                [];
              let area : M.area =
                {
                  id = "work";
                  name = "Work";
                  definition = String.make 29000 'x';
                  exclusions = "";
                  parent = None;
                  status = "proposed";
                  revision = 1;
                }
              in
              S.propose db [ area ] [];
              S.edit db ~revision:(S.revision db) (Approve "work")
                ~explanation:"Approve";
              let calls = ref 0 in
              let complete (request : Agentkit.Chat.request) =
                incr calls;
                check "encoded request and retry fit the limit"
                  (Agentkit.Chat.text_bytes request <= 65536);
                let input =
                  List.find_map
                    (function
                      | Agentkit.Chat.User value -> Some value | _ -> None)
                    request.messages
                  |> Option.get
                in
                let sent = Jmap_eio.Codec.decode_exn input_codec input in
                check "truncation is explicit" sent.truncated;
                check "identity retained" (sent.source = m.source);
                check "important headers survive long transport headers"
                  (List.assoc_opt "List-Id" sent.headers
                   = Some "synthetic.example"
                  && List.assoc_opt "Auto-Submitted" sent.headers
                     = Some "auto-generated");
                Agentkit.Chat.response
                  (Some (if !calls = 1 then "invalid" else response))
              in
              check "oversized mail classifies including a retry"
                (Siftbot.Classifier.run db ~model:"test" ~complete = 1
                && !calls = 2);
              check "stored email is unchanged" (S.get db m.source = Some m);
              check "saved result reused"
                (Siftbot.Classifier.run db ~model:"test" ~complete:(fun _ ->
                     failwith "Cache miss")
                = 0))
            [
              {
                base with
                headers;
                original = String.make 8192 'x';
                text = String.make 8192 'y';
              };
              {
                base with
                headers;
                recipients = List.init 2000 (fun _ -> String.make 1024 'r');
                subject = String.make 32768 's';
                original = String.make 8192 '\000';
                text = String.concat "" (List.init 8192 (fun _ -> "é\n\""));
              };
            ];
          print_endline
            "Classification input, escaped bytes, retry budget and source \
             preservation passed"))
