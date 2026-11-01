let check name value = if not value then failwith name

let member name = function
  | Jsont.Object (fields, _) ->
      List.find_map
        (fun ((key, _), value) -> if key = name then Some value else None)
        fields
  | _ -> None

let () =
  Unix.putenv "SIFTBOT_OPENROUTER_API_KEY" "hosted-only-fixture";
  Unix.putenv "SIFTBOT_CHAT_API_KEY" "";
  Eio_main.run (fun _ ->
      let profile =
        {
          (Siftbot.Profile.defaults ~name:"test" ~jmap_profile:"test") with
          chat_model = Some "Qwen/Qwen3.8-27B-FP8";
          chat_base_url = Some "http://sequoia:8000/v1";
        }
      in
      let calls = ref 0 in
      let fetch =
        Fetch_mock.client (fun request ->
            incr calls;
            let url = Fetch.Middleware.Url.to_string request.url in
            if !calls = 1 then (
              check "custom endpoint"
                (url = "http://sequoia:8000/v1/chat/completions");
              check "hosted key never sent to Sequoia"
                (Http.Header.get request.headers "authorization" = None))
            else (
              check "default hosted endpoint"
                (url = "https://openrouter.ai/api/v1/chat/completions");
              check "hosted credential retained"
                (Http.Header.get request.headers "authorization"
                = Some "Bearer hosted-only-fixture"));
            let body =
              match request.body with
              | Fetch.String body -> body
              | _ -> assert false
            in
            let json = Jmap_eio.Codec.decode_exn Jsont.json body in
            check "classification schema reaches the configured endpoint"
              (if !calls = 1 then
                 match
                   Option.bind
                     (Option.bind
                        (member "response_format" json)
                        (member "json_schema"))
                     (fun schema ->
                       Option.bind (member "schema" schema) (member "type"))
                 with
                 | Some (Jsont.String ("array", _)) -> true
                 | _ -> false
               else member "response_format" json = None);
            let model =
              match json with
              | Jsont.Object (fields, _) ->
                  List.find_map
                    (fun ((key, _), value) ->
                      if key = "model" then
                        match value with
                        | Jsont.String (model, _) -> Some model
                        | _ -> None
                      else None)
                    fields
              | _ -> None
            in
            check "wire model name unchanged" (model = profile.chat_model);
            Fetch_mock.respond
              ~headers:
                (Http.Header.of_list [ ("content-type", "application/json") ])
              {|{"id":"chat-1","model":"Qwen/Qwen3.8-27B-FP8","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"OK"}}]}|}
              request)
      in
      let identity, complete =
        Siftbot.Assistant.completion
          ~json_schema:
            (Jmap_eio.Codec.decode_exn Jsont.json {|{"type":"array"}|})
          ~fetch profile
      in
      check "cache identity includes endpoint"
        (identity = "http://sequoia:8000/v1#Qwen/Qwen3.8-27B-FP8");
      let request =
        Agentkit.Chat.request [ User "Synthetic connection test" ]
      in
      check "anonymous compatible completion"
        ((complete request).text = Some "OK");
      let hosted, complete =
        Siftbot.Assistant.completion ~fetch
          { profile with chat_base_url = None }
      in
      check "hosted cache identity retained" (hosted = "Qwen/Qwen3.8-27B-FP8");
      ignore (complete request);
      List.iter
        (fun base_url ->
          check "invalid base rejected before network"
            (try
               ignore
                 (Siftbot.Assistant.completion ~fetch
                    { profile with chat_base_url = Some base_url });
               false
             with Invalid_argument _ -> true))
        [
          "http://user:password@sequoia:8000/v1";
          "http://sequoia:8000/v1?key=secret";
          "http://sequoia:8000/v1#fragment";
        ];
      check "only expected requests sent" (!calls = 2));
  print_endline
    "Sift custom model endpoint, credential isolation and adapter tests passed"
