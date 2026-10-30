module S = Siftbot.Store
module C = Agentkit.Chat

let check name ok = if not ok then failwith name

let triage =
  {|{"dimension":"triage","value":"relevant","score":0.8,"explanation":"Personal request"}|}

let delivery =
  {|{"dimension":"delivery","value":"direct","score":0.8,"explanation":"Personal request"}|}

let attention =
  {|{"dimension":"attention","value":"needs-reply","score":0.8,"explanation":"Question"}|}

let array rows = "[" ^ String.concat "," rows ^ "]"
let valid = array [ triage; delivery; attention ]
let reply text = C.response ~finish:Stop (Some text)

let field name = function
  | Jsont.Object (fields, _) ->
      fields
      |> List.find_map (fun ((key, _), value) ->
             if name = key then Some value else None)
      |> Option.get
  | _ -> failwith "Expected a schema object"

let strings = function
  | Jsont.Array (values, _) ->
      List.map
        (function Jsont.String (s, _) -> s | _ -> failwith "Expected string")
        values
  | _ -> failwith "Expected array"

let () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let schema_db = S.memory ~sw () in
          let schema = Siftbot.Classifier.schema schema_db in
          check "empty policy forbids all area predictions"
            (match field "items" schema with
            | Jsont.Bool (false, _) -> true
            | _ -> false);
          check "schema requires exactly the three base dimensions in order"
            (match field "prefixItems" schema with
            | Jsont.Array (items, _) ->
                List.map
                  (fun item ->
                    match
                      item |> field "properties" |> field "dimension"
                      |> field "const"
                    with
                    | Jsont.String (s, _) -> s
                    | _ -> failwith "Expected dimension")
                  items
                = [ "triage"; "delivery"; "attention" ]
            | _ -> false);
          let area id : Siftbot_model.area =
            {
              id;
              name = id;
              definition = "Work";
              exclusions = "";
              parent = None;
              status = "proposed";
              revision = 1;
            }
          in
          S.propose schema_db [ area "approved"; area "candidate" ] [];
          S.edit schema_db ~revision:(S.revision schema_db) (Approve "approved")
            ~explanation:"Test approval";
          let schema = Siftbot.Classifier.schema schema_db in
          check "schema permits only approved area IDs"
            (schema |> field "items" |> field "properties" |> field "value"
           |> field "enum" |> strings = [ "approved" ]);
          let run replies =
            let db = S.memory ~sw () in
            let message = List.hd Siftbot.Demo.messages in
            S.commit_batch db ~job:"fixture" ~expected:None ~cursor:"1"
              [ message ] [];
            let requests = ref [] and replies = ref replies in
            let complete (request : C.request) =
              requests := request :: !requests;
              check "tool-free request with reasoning disabled"
                (request.tools = [] && request.reasoning = Some "none");
              check "bounded request"
                (C.text_bytes request <= 65536 && request.max_tokens = Some 4096);
              (match List.rev !requests with
              | first :: _ ->
                  check "no conversational synthesis instruction"
                    (List.length first.messages = 2)
              | [] -> assert false);
              match !replies with
              | response :: rest ->
                  replies := rest;
                  response
              | [] -> failwith "Unexpected model request"
            in
            let progress = ref [] in
            let result =
              try
                Ok
                  (Siftbot.Classifier.run db ~complete ~model:"test"
                     ~max_messages:1 ~on_progress:(fun p ->
                       progress := p :: !progress))
              with Failure reason -> Error reason
            in
            (match result with
            | Ok 1 ->
                check "three saved assignments"
                  (List.length (S.snapshot db ~query:"").assignments = 3);
                check "success advances once" (List.length !progress = 2);
                check "saved results are cached"
                  (Siftbot.Classifier.run db ~model:"test" ~max_messages:1
                     ~complete:(fun _ -> failwith "Cache miss")
                  = 0)
            | Error _ ->
                check "failure saves no assignments"
                  ((S.snapshot db ~query:"").assignments = []);
                check "failure does not advance" (List.length !progress = 1);
                check "failure is not cached"
                  (S.cached db ~source:message.source
                     ~kind:"classification/active"
                  = None)
            | Ok _ -> failwith "Unexpected classification count");
            (result, List.rev !requests)
          in
          List.iter
            (fun text ->
              let result, requests = run [ reply text ] in
              check "bare or fenced JSON accepted"
                (result = Ok 1 && List.length requests = 1))
            [
              valid;
              "```json\n" ^ valid ^ "\n```";
              "```\r\n" ^ valid ^ "\r\n```";
            ];
          List.iter
            (fun response ->
              let result, requests = run [ response; reply valid ] in
              check "invalid response retried once"
                (result = Ok 1 && List.length requests = 2);
              check "retry contains validation guidance"
                (List.length (List.nth requests 1).messages = 3))
            [
              reply "not JSON";
              reply ("Here is the result: " ^ valid);
              reply ("```json\n" ^ valid ^ "\n```\nextra prose");
              reply "";
              reply (String.make 16385 'x');
              reply (array [ triage; triage; delivery; attention ]);
              reply (array [ triage; delivery ]);
              reply
                (array
                   [
                     {|{"dimension":"Triage","value":"relevant","score":0.8,"explanation":"Evidence"}|};
                     delivery;
                     attention;
                   ]);
              reply
                (array
                   [
                     triage;
                     delivery;
                     attention;
                     {|{"dimension":"area","value":"unknown","score":0.8,"explanation":"Evidence"}|};
                   ]);
              C.response ~finish:Length (Some valid);
              C.response ~finish:(Other "content_filter") (Some valid);
              C.response ~finish:Tool_calls
                ~calls:
                  [
                    {
                      Agentkit.Agent.id = "x";
                      name = "write";
                      arguments = "{}";
                    };
                  ]
                (Some valid);
            ];
          let result, requests = run [ reply "PRIVATE"; reply "PRIVATE" ] in
          check "persistent failure is bounded and content-free"
            (List.length requests = 2
            &&
            match result with
            | Error reason ->
                String.starts_with
                  ~prefix:"Model returned invalid classification JSON" reason
                && not (String.contains reason 'P')
            | Ok _ -> false);
          print_endline
            "Classification JSON, validation, retry and cache tests passed"))
