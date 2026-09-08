module O = Openrouter

let check name value = Alcotest.(check bool) name true value
let tree text = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json text)

let field name = function
  | Jsont.Object (fields, _) ->
      List.find_map
        (fun ((key, _), value) -> if key = name then Some value else None)
        fields
  | _ -> None

let json ?(status = 200) body request =
  Fetch_mock.respond ~status
    ~headers:(Http.Header.of_list [ ("Content-Type", "application/json") ])
    body request

let body request =
  match request.Fetch.Middleware.body with
  | Fetch.String text -> tree text
  | _ -> Alcotest.fail "expected JSON request body"

let response =
  {|{"id":"chat-1","model":"test/model","created":1,
    "object":"chat.completion","choices":[{"index":0,
    "finish_reason":"stop","message":{"role":"assistant","content":"OK",
    "audio":null,"tool_calls":null,"reasoning":null}}],
    "usage":{"prompt_tokens":2,"completion_tokens":1,"total_tokens":3}}|}

let chunk =
  {|{"id":"chat-1","model":"test/model","created":1,
    "object":"chat.completion.chunk","system_fingerprint":null,
    "choices":[{"index":0,"finish_reason":null,
    "delta":{"content":"OK","reasoning":null}}],"usage":null}|}

let final_chunk =
  {|{"id":"chat-1","model":"test/model","created":1,
    "object":"chat.completion.chunk","choices":[{"index":0,
    "finish_reason":"stop","delta":{}}],
    "usage":{"prompt_tokens":2,"completion_tokens":1,"total_tokens":3}}|}

let request () =
  O.Chat.request ~model:"test/model" ~max_tokens:16 ~temperature:0.
    ~messages:[ O.Message.system "Be brief"; O.Message.user "Hello" ]
    ()

let client ?max_response_bytes handler =
  O.of_fetch ~base_url:"https://example.test/v1/" ?max_response_bytes
    (Fetch_mock.client handler)

let expect_fetch f =
  match f () with
  | _ -> Alcotest.fail "expected Fetch failure"
  | exception Eio.Io (Fetch.E _, _) -> ()

let expect_error f =
  match f () with
  | _ -> Alcotest.fail "expected Openrouter error"
  | exception Eio.Io (O.E error, _) -> error

let test_complete () =
  let c =
    O.of_fetch ~base_url:"https://example.test/v1/" ~api_key:"test-key"
      ~app_title:"test app"
      (Fetch_mock.client (fun req ->
           check "POST" (req.meth = `POST);
           check "API prefix"
             (Fetch.Middleware.Url.path_and_query req.url
             = "/v1/chat/completions");
           check "scoped bearer"
             (Http.Header.get req.headers "authorization"
             = Some "Bearer test-key");
           check "app header"
             (Http.Header.get req.headers "x-openrouter-title"
             = Some "test app");
           check "non-streaming flag"
             (field "stream" (body req) = Some (Jsont.Json.bool false));
           check "native token budget"
             (field "max_completion_tokens" (body req)
             = Some (Jsont.Json.int 16));
           json response req))
  in
  let result = O.Chat.complete c (request ()) in
  check "native completion"
    (result.id = "chat-1" && result.model = "test/model");
  check "native choice"
    (match result.choices with
    | [ { text = Some "OK"; finish_reason = Some O.Chat.Stop; _ } ] -> true
    | _ -> false);
  check "native usage"
    (Option.map (fun (u : O.Chat.usage) -> u.total_tokens) result.usage
    = Some 3)

let test_models () =
  List.iter
    (fun (body, context) ->
      let models = O.Models.list (client (json body)) in
      check "native model list"
        (match models with
        | [ { id = "model"; context_length; _ } ] ->
            context_length = Some context
        | _ -> false))
    [
      ( {|{"data":[{"id":"model","name":"Model","context_length":8192}]}|},
        8192 );
      ( {|{"object":"list","data":[{"id":"model","max_model_len":4096}]}|},
        4096 );
    ];
  expect_fetch (fun () ->
      O.Models.list (client ~max_response_bytes:1 (json {|{"data":[]}|})))

let scripted ?(status = 200) ?(media = "text/event-stream") wire closed req
    =
  Fetch.Middleware.Pi.response ~status ~version:`HTTP_1_1
    ~headers:(Http.Header.of_list [ ("Content-Type", media) ])
    ~body:(Eio.Flow.string_source wire)
    ~close:(fun () -> incr closed)
    ~url:req.Fetch.Middleware.url ()

let sse chunks =
  ": keepalive\n\n"
  ^ String.concat ""
      (List.map (fun data -> "data: " ^ data ^ "\n\n") chunks)

let compact text =
  Result.get_ok (Jsont_bytesrw.encode_string Jsont.json (tree text))

let stream_wire =
  sse [ compact chunk; compact final_chunk; "[DONE]"; "invalid" ]

let test_stream () =
  let closed = ref 0 and seen = ref [] in
  let c =
    client (fun req ->
        check "stream flag chosen by client"
          (field "stream" (body req) = Some (Jsont.Json.bool true));
        check "stream accept"
          (Http.Header.get req.headers "accept" = Some "text/event-stream");
        scripted stream_wire closed req)
  in
  let result =
    O.Chat.stream c (request ()) ~on_event:(fun event ->
        Eio.Fiber.yield ();
        seen := event :: !seen;
        `Continue)
  in
  check "completion sentinel" (result = `Complete);
  check "semantic events and late usage"
    (match List.rev !seen with
    | [
     O.Chat.Started _;
     Text { text = "OK"; _ };
     Finished { reason = Stop; _ };
     Usage { total_tokens = 3; _ };
    ] ->
        true
    | _ -> false);
  Alcotest.(check int) "closed once" 1 !closed

let test_stream_lifetimes () =
  let closed = ref 0 in
  let c = client (scripted stream_wire closed) in
  check "consumer stop"
    (O.Chat.stream c (request ()) ~on_event:(fun _ -> `Stop) = `Stopped);
  (match O.Chat.stream c (request ()) ~on_event:(fun _ -> raise Exit) with
  | _ -> Alcotest.fail "lost callback exception"
  | exception Exit -> ());
  (match
     O.Chat.stream c (request ()) ~on_event:(fun _ ->
         raise (Eio.Cancel.Cancelled Exit))
   with
  | _ -> Alcotest.fail "lost cancellation"
  | exception Eio.Cancel.Cancelled Exit -> ());
  Alcotest.(check int) "every response closed" 3 !closed

let test_stream_failures () =
  let consume ?max_event c =
    O.Chat.stream ?max_event c (request ()) ~on_event:(fun _ -> `Continue)
  in
  List.iter
    (fun wire ->
      let closed = ref 0 in
      (match
         expect_error (fun () -> consume (client (scripted wire closed)))
       with
      | O.Protocol_error _ -> ()
      | _ -> Alcotest.fail "wrong protocol error");
      Alcotest.(check int) "protocol failure closed" 1 !closed)
    [ sse [ compact chunk ]; sse [ "invalid" ]; sse [ "{}" ] ];
  let closed = ref 0 in
  (match
     expect_error (fun () ->
         consume
           (client
              (scripted
                 (sse [ {|{"error":{"code":429,"message":"busy"}}|} ])
                 closed)))
   with
  | O.Stream_error { code = Some "429"; message = "busy" } -> ()
  | _ -> Alcotest.fail "wrong stream error");
  expect_fetch (fun () ->
      consume ~max_event:16 (client (scripted stream_wire closed)));
  expect_fetch (fun () ->
      consume (client (scripted ~media:"text/html" stream_wire closed)))

let test_errors_and_policy () =
  (match
     expect_error (fun () ->
         O.Chat.complete
           (client
              (json ~status:401
                 {|{"error":{"message":"no key","code":401}}|}))
           (request ()))
   with
  | O.Http_error { status = 401; message = "no key"; _ } -> ()
  | _ -> Alcotest.fail "wrong HTTP error");
  expect_fetch (fun () ->
      O.Chat.complete
        (client ~max_response_bytes:1 (json response))
        (request ()));
  let calls = ref 0 in
  let c =
    client (fun req ->
        incr calls;
        Fetch_mock.respond ~status:307
          ~headers:
            (Http.Header.of_list
               [ ("Location", "https://elsewhere.test/") ])
          "" req)
  in
  ignore (expect_error (fun () -> O.Chat.complete c (request ())));
  Alcotest.(check int) "POST not redirected" 1 !calls;
  expect_fetch (fun () -> O.Models.list c);
  Alcotest.(check int) "GET redirect cannot escape prefix" 2 !calls;
  let c =
    O.of_fetch ~base_url:"http://example.test/v1" ~api_key:"test-key"
      (Fetch_mock.client (fun _ -> Alcotest.fail "insecure bearer sent"))
  in
  expect_fetch (fun () -> O.Chat.complete c (request ()))

let test_tools () =
  let call : O.Tool.call =
    {
      id = "call-1";
      name = "weather";
      arguments = {|{"city":"Cambridge"}|};
    }
  in
  check "typed tool arguments"
    (Result.is_ok (O.Tool.arguments Jsont.json call));
  check "bad arguments remain detectable"
    (Result.is_error
       (O.Tool.arguments Jsont.json { call with arguments = "{" }));
  let tool =
    O.Tool.v ~name:"weather"
      ~parameters:
        (tree {|{"type":"object","properties":{"city":{"type":"string"}}}|})
      ()
  in
  let request =
    O.Chat.request ~model:"test/model" ~tools:[ tool ]
      ~tool_choice:(O.Chat.Function "weather")
      ~messages:
        [
          O.Message.user "Weather?";
          O.Message.assistant ~tool_calls:[ call ] "";
          O.Message.tool_result ~tool_call_id:call.id "Sunny";
        ]
      ()
  in
  ignore
    (O.Chat.complete
       (client (fun req ->
            check "tools encoded"
              (Option.is_some (field "tools" (body req)));
            json response req))
       request)

let test_invalid_requests () =
  List.iter
    (fun make ->
      match make () with
      | _ -> Alcotest.fail "invalid request accepted"
      | exception Invalid_argument _ -> ())
    [
      (fun () ->
        O.Chat.request ~model:"" ~messages:[ O.Message.user "hi" ] ());
      (fun () -> O.Chat.request ~model:"m" ~messages:[] ());
      (fun () ->
        O.Chat.request ~model:"m"
          ~messages:[ O.Message.user "hi" ]
          ~temperature:nan ());
      (fun () ->
        O.Chat.request ~model:"m"
          ~messages:[ O.Message.user "hi" ]
          ~max_tokens:0 ());
    ]

let test_tool_responses () =
  let response =
    {|{"id":"chat-2","model":"test/model","created":2,
      "object":"chat.completion","choices":[{"index":0,
      "finish_reason":"tool_calls","message":{"role":"assistant",
      "content":null,"reasoning_content":"Check the weather",
      "tool_calls":[{"id":"call-1","type":"function",
      "function":{"name":"weather",
      "arguments":"{\"city\":\"Cambridge\"}"}}]}}]}|}
  in
  let result = O.Chat.complete (client (json response)) (request ()) in
  check "native completed tool call"
    (match result.choices with
    | [
     {
       text = None;
       reasoning = Some "Check the weather";
       tool_calls = [ { id = "call-1"; name = "weather"; arguments } ];
       finish_reason = Some O.Chat.Tool_calls;
       _;
     };
    ] ->
        arguments = {|{"city":"Cambridge"}|}
    | _ -> false);
  let chunk choices =
    compact
      (Printf.sprintf
         {|{"id":"chat-2","model":"test/model","created":2,
      "object":"chat.completion.chunk","choices":%s}|}
         choices)
  in
  let wire =
    sse
      [
        chunk
          {|[{"index":0,"delta":{"reasoning_content":"Checking",
      "tool_calls":[{"index":0,"id":"call-1","type":"function",
      "function":{"name":"weather","arguments":"{\"city\":"}}]}}]|};
        chunk
          {|[{"index":0,"delta":{"tool_calls":[{"index":0,
      "function":{"arguments":"\"Cambridge\"}"}}]},
      "finish_reason":"tool_calls"}]|};
        "[DONE]";
      ]
  in
  let seen = ref [] and closed = ref 0 in
  ignore
    (O.Chat.stream
       (client (scripted wire closed))
       (request ())
       ~on_event:(fun e ->
         seen := e :: !seen;
         `Continue));
  check "tool and reasoning fragments"
    (match List.rev !seen with
    | [
     O.Chat.Started _;
     Reasoning { choice = 0; text = "Checking" };
     Tool_call
       {
         choice = 0;
         index = 0;
         id = Some "call-1";
         name = Some "weather";
         arguments = Some first;
       };
     Tool_call
       {
         choice = 0;
         index = 0;
         id = None;
         name = None;
         arguments = Some last;
       };
     Finished { choice = 0; reason = Tool_calls };
    ] ->
        first ^ last = {|{"city":"Cambridge"}|}
    | _ -> false);
  Alcotest.(check int) "tool response closed" 1 !closed

let test_images () =
  let inline =
    O.Image.of_flow ~max_bytes:3 ~format:Png ~detail:Low
      (Eio.Flow.string_source "abc")
  in
  let url =
    O.Image.of_url ~detail:Original
      "https://images.example.test/photo.png?signature=abc%2fdef"
  in
  let message =
    O.Message.user_parts
      [
        O.Content.text "Compare";
        O.Content.image inline;
        O.Content.text "with";
        O.Content.image url;
      ]
  in
  let request =
    O.Chat.request ~model:"vision/model" ~messages:[ message ] ()
  in
  ignore
    (O.Chat.complete
       (client (fun req ->
            let expected =
              tree
                {|[
      {"role":"user","content":[
        {"type":"text","text":"Compare"},
        {"type":"image_url","image_url":{
          "url":"data:image/png;base64,YWJj","detail":"low"}},
        {"type":"text","text":"with"},
        {"type":"image_url","image_url":{
          "url":"https://images.example.test/photo.png?signature=abc%2fdef",
          "detail":"original"}}]}]|}
            in
            check "ordered image content"
              (match field "messages" (body req) with
              | Some actual -> Jsont.Json.equal actual expected
              | None -> false);
            json response req))
       request);
  List.iter
    (fun f ->
      match expect_error f with
      | O.Image_too_large { limit = 3 } -> ()
      | _ -> Alcotest.fail "wrong image limit error")
    [
      (fun () -> O.Image.of_string ~max_bytes:3 ~format:Png "abcd");
      (fun () ->
        O.Image.of_flow ~max_bytes:3 ~format:Png
          (Eio.Flow.string_source "abcd"));
    ];
  List.iter
    (fun url ->
      match O.Image.of_url url with
      | _ -> Alcotest.fail "invalid image URL accepted"
      | exception Invalid_argument _ -> ())
    [
      "file:///etc/passwd";
      "https://user:password@example.test/image";
      "data:image/png;base64,YWJj";
    ];
  match O.Image.of_string ~format:Jpeg "" with
  | _ -> Alcotest.fail "empty image accepted"
  | exception Invalid_argument _ -> ()

let () =
  Alcotest.run "Openrouter native Fetch client"
    [
      ( "client",
        List.map
          (fun (name, test) ->
            ( name,
              `Quick,
              fun () -> Eio_mock.Backend.run_full (fun _ -> test ()) ))
          [
            ("completion", test_complete);
            ("models", test_models);
            ("stream events", test_stream);
            ("stream lifetimes", test_stream_lifetimes);
            ("stream failures", test_stream_failures);
            ("errors and capabilities", test_errors_and_policy);
            ("tools", test_tools);
            ("tool responses", test_tool_responses);
            ("images", test_images);
            ("input validation", test_invalid_requests);
          ] );
    ]
