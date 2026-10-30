module M = Siftbot_model
module S = Siftbot.Store

let check name value = if not value then failwith name

let () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let db = S.memory ~sw () in
          let template = List.hd Siftbot.Demo.messages in
          let body = String.make 65536 'x' in
          let messages =
            List.init 100 (fun i ->
                {
                  template with
                  M.source =
                    { template.source with id = Printf.sprintf "email-%03d" i };
                  original = body;
                  text = body;
                  headers = [ ("received", body) ];
                })
          in
          S.commit_batch db ~job:"fixture" ~expected:None ~cursor:"1" messages
            [];
          check "old full-body snapshot exceeds worker limit"
            (try
               ignore (Siftbot_protocol.response (Ok (S.snapshot db ~query:"")));
               false
             with Invalid_argument _ -> true);
          let view = Siftbot.Review.snapshot db ~query:"" () in
          let raw = Siftbot_protocol.response (Ok view) in
          check "all 100 messages remain listed"
            (List.length view.messages = 100);
          check "one body crosses worker boundary"
            (List.length
               (List.filter
                  (fun (m : M.message) -> m.original <> "")
                  view.messages)
            = 1);
          check "compact response fits"
            (String.length raw < Siftbot_protocol.max_bytes);
          check "compact response round trips"
            (Siftbot_protocol.parse_response raw = Ok (Ok view));
          let unrelated =
            List.init 15000 (fun i ->
                {
                  template with
                  M.source =
                    { template.source with id = Printf.sprintf "old-%05d" i };
                  thread = Printf.sprintf "old-thread-%05d" i;
                  date = "1900-01-01";
                  original = "";
                  text = "";
                  headers = [];
                })
          in
          S.commit_batch db ~job:"older" ~expected:None ~cursor:"1" unrelated [];
          let label (m : M.message) : M.assignment =
            {
              target = m.source;
              conversation = false;
              dimension = Triage;
              value = "relevant";
              origin = Model;
              version = "test";
              source_revision = m.revision;
              explanation = "Test evidence";
              evidence = [ m.source ];
              score = None;
            }
          in
          let conversation =
            {
              (label (List.hd messages)) with
              target = { template.source with id = template.thread };
              conversation = true;
            }
          in
          S.propose db [] (conversation :: List.map label (messages @ unrelated));
          let view = Siftbot.Review.snapshot db ~query:"" () in
          check "review excludes assignments outside its message window"
            (List.length view.assignments = 101);
          check "review retains conversation defaults"
            (List.mem conversation view.assignments);
          check "classified mailbox stays within worker limit"
            (String.length (Siftbot_protocol.response (Ok view))
            < Siftbot_protocol.max_bytes);
          let selected = (List.nth messages 50).source in
          let view = Siftbot.Review.snapshot db ~query:"" ~selected () in
          check "selection loads the requested body"
            (List.exists
               (fun (m : M.message) -> m.source = selected && m.original = body)
               view.messages);
          check "stored source unchanged"
            ((Option.get (S.get db selected)).headers = [ ("received", body) ]);
          let large =
            {
              template with
              source = (List.hd messages).source;
              original = String.make ((256 * 1024) - 1) 'a' ^ "é";
              revision = "large";
            }
          in
          S.commit_batch db ~job:"fixture" ~expected:(Some "1") ~cursor:"2"
            [ large ] [];
          let view =
            Siftbot.Review.snapshot db ~query:"" ~selected:large.source ()
          in
          let message =
            List.find
              (fun (m : M.message) -> m.source = large.source)
              view.messages
          in
          check "large display body is marked truncated" message.truncated;
          check "truncation respects UTF-8"
            (String.is_valid_utf_8 message.original);
          ignore (Siftbot_protocol.response (Ok view))));
  print_endline
    "Review lists large corpora and loads selected bodies within worker limits"
