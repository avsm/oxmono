module Chat = Agentkit.Chat
module Turn = Agentkit.Turn
module Summary = Agentkit.Summary

let check name value = Alcotest.(check bool) name true value
let call ?(id = "1") name arguments = { Agentkit.Agent.id; name; arguments }

let tool name =
  Agentkit.Agent.Tool.v ~name ~description:name
    ~parameters:(Jsont.Json.object' [])

let ok_guard = Turn.unguarded "test tools have no authority"
let dispatch (c : Agentkit.Agent.tool_call) = Ok ("ran " ^ c.name)

(* A scripted model: each request gets the next reply, and requests are kept. *)
let script replies =
  let replies = ref replies and seen = ref [] in
  let complete (r : Chat.request) =
    seen := r :: !seen;
    match !replies with
    | reply :: rest ->
        replies := rest;
        reply
    | [] -> Alcotest.fail "unexpected model request"
  in
  (complete, fun () -> List.rev !seen)

let calls_reply n =
  Chat.response ~finish:Chat.Tool_calls
    ~calls:(List.init n (fun i -> call ~id:(string_of_int i) "t" "{}"))
    None

let last_user (r : Chat.request) =
  match List.rev r.messages with Chat.User s :: _ -> Some s | _ -> None

let test_answer () =
  let complete, seen =
    script [ calls_reply 1; Chat.response ~finish:Chat.Stop (Some "done") ]
  in
  let answer =
    Turn.run ~complete ~tools:[ tool "t" ] ~guard:ok_guard ~dispatch
      [ Chat.System "sys"; Chat.User "hi" ]
  in
  check "answer" (answer = "done");
  match seen () with
  | [ first; second ] ->
      check "tools offered" (List.length first.tools = 1);
      check "result follows call"
        (match List.rev second.messages with
        | Chat.Tool_result { id = "0"; content = "ran t" }
          :: Chat.Assistant { calls = [ _ ]; _ }
          :: _ ->
            true
        | _ -> false)
  | _ -> check "two requests" false

let test_synthesis_notice () =
  let complete, seen =
    script
      [
        calls_reply 2; calls_reply 1; Chat.response (Some "summary answer");
      ]
  in
  let answer =
    Turn.run ~budget:3 ~complete ~tools:[ tool "t" ] ~guard:ok_guard ~dispatch
      [ Chat.System "sys"; Chat.User "hi" ]
  in
  check "synthesis answer" (answer = "summary answer");
  let terminal = List.nth (seen ()) 2 in
  check "terminal request has no tools" (terminal.tools = []);
  check "instruction is the final user message"
    (match last_user terminal with
    | Some s -> String.starts_with ~prefix:"Runtime notice" s
    | None -> false);
  check "system prompt kept first"
    (match terminal.messages with
    | Chat.System s :: _ -> String.starts_with ~prefix:"sys" s
    | _ -> false)

let test_recovery_and_fallback () =
  let complete, _ =
    script [ Chat.response None; Chat.response (Some "recovered") ]
  in
  let events = ref [] in
  let answer =
    Turn.run ~complete ~tools:[] ~guard:ok_guard ~dispatch
      ~on_event:(fun e -> events := e :: !events)
      [ Chat.User "hi" ]
  in
  check "empty reply retried" (answer = "recovered");
  check "recovery reported" (List.mem Turn.Recovered !events);
  let complete, _ = script [ Chat.response None; Chat.response (Some " ") ] in
  let answer =
    Turn.run ~complete ~tools:[] ~guard:ok_guard ~dispatch
      ~fallback:(fun ~tools_used -> if tools_used then "x" else "nothing")
      [ Chat.User "hi" ]
  in
  check "fallback never blank" (answer = "nothing")

let test_guard_fails_closed () =
  let ran = ref 0 in
  let dispatch c =
    incr ran;
    dispatch c
  in
  let guard (c : Agentkit.Agent.tool_call) =
    if c.id = "0" then Error "not yours" else failwith "guard bug"
  in
  let complete, seen =
    script [ calls_reply 2; Chat.response (Some "ok") ]
  in
  ignore
    (Turn.run ~complete ~tools:[ tool "t" ] ~guard ~dispatch
       [ Chat.User "hi" ]);
  check "denied calls never dispatch" (!ran = 0);
  check "refusals reach the model"
    (match List.rev (List.nth (seen ()) 1).messages with
    | Chat.Tool_result { content = "Error: Tool call refused."; _ }
      :: Chat.Tool_result { content = "Error: not yours"; _ }
      :: _ ->
        true
    | _ -> false)

let test_budget_and_bounds () =
  let audited = ref 0 in
  let around _ f =
    incr audited;
    match f () with Ok s -> s | Error e -> "Error: " ^ e
  in
  let complete, _ = script [ calls_reply 7 ] in
  (match
     Turn.run ~complete ~tools:[ tool "t" ] ~guard:ok_guard ~dispatch ~around
       [ Chat.User "hi" ]
   with
  | _ -> check "over-budget raises" false
  | exception Turn.Budget_exceeded -> ());
  check "every refused call audited" (!audited = 7);
  let big _ = Ok (String.make 100 'x') in
  let complete, seen = script [ calls_reply 1; Chat.response (Some "ok") ] in
  ignore
    (Turn.run ~complete ~tools:[ tool "t" ] ~guard:ok_guard ~dispatch:big
       ~max_result_bytes:10 [ Chat.User "hi" ]);
  check "results clipped"
    (match List.rev (List.nth (seen ()) 1).messages with
    | Chat.Tool_result { content; _ } :: _ ->
        content = String.make 10 'x' ^ "\n[truncated]"
    | _ -> false)

let test_cut_off () =
  let complete, _ =
    script [ Chat.response ~finish:Chat.Length (Some "partial") ]
  in
  let answer =
    Turn.run ~complete ~tools:[] ~guard:ok_guard ~dispatch [ Chat.User "hi" ]
  in
  check "cut-off answer marked"
    (answer = "partial\n\n[Reply cut off at the token limit.]")

let test_bind () =
  let tools =
    Turn.bind ~guard:(fun _ -> Error "no") ~dispatch [ tool "t" ]
  in
  check "bound tools are guarded"
    (Agentkit.Agent.Tool.invoke (List.hd tools) (call "t" "{}") = "Error: no")

let summarise replies =
  let complete, seen = script replies in
  let retries = ref [] in
  let result =
    try
      Ok
        (Summary.run ~complete
           ~instructions:(fun ~words -> Printf.sprintf "about %d words" words)
           ~limit:1000
           ~on_retry:(fun f ~words -> retries := (f, words) :: !retries)
           "input")
    with Summary.Failed f -> Error f
  in
  (result, seen (), List.rev !retries)

let test_summary () =
  let result, seen, _ =
    summarise
      [ Chat.response (Some "Sure:\n```json\n{\"summary\":\"short\"}\n```") ]
  in
  check "fenced JSON accepted" (result = Ok "short");
  let first = List.hd seen in
  check "reasoning disabled and words asked"
    (first.reasoning = Some "none"
    && first.tools = []
    && Chat.system_text first.messages = Some "about 100 words");
  let result, _, retries =
    summarise
      [
        Chat.response ~finish:Chat.Length (Some "{\"summary\":\"cut");
        Chat.response (Some "{\"summary\":\"ok\"}");
      ]
  in
  check "cut-off retried with half the words"
    (result = Ok "ok" && retries = [ (Summary.Length, 50) ]);
  let result, _, _ =
    summarise
      [
        Chat.response (Some ("{\"summary\":\"" ^ String.make 1001 'x' ^ "\"}"));
        Chat.response (Some "");
      ]
  in
  check "oversized then empty fails" (result = Error Summary.Json);
  let result, seen, _ = summarise [ calls_reply 1 ] in
  check "tool requests are not retried"
    (result = Error Summary.Tools && List.length seen = 1)

(* Streamed tool calls carry the id and name only on their first fragment. *)
let sse chunks =
  String.concat "" (List.map (fun d -> "data: " ^ d ^ "\n\n") chunks)
  ^ "data: [DONE]\n\n"

let delta ?(finish = "null") body =
  Printf.sprintf
    {|{"id":"c","model":"m","created":1,"object":"chat.completion.chunk","choices":[{"index":0,"finish_reason":%s,"delta":%s}]}|}
    finish body

let fragmented =
  sse
    [
      delta
        {|{"tool_calls":[{"index":1,"id":"b","type":"function","function":{"name":"echo","arguments":""}}]}|};
      delta
        {|{"tool_calls":[{"index":0,"id":"a","type":"function","function":{"name":"echo","arguments":""}}]}|};
      delta {|{"tool_calls":[{"index":0,"function":{"arguments":"{\"text\":"}}]}|};
      delta {|{"tool_calls":[{"index":1,"function":{"arguments":"{\"text\":\"two\"}"}}]}|};
      delta {|{"tool_calls":[{"index":0,"function":{"arguments":"\"one\"}"}}]}|};
      delta ~finish:{|"tool_calls"|} "{}";
    ]

let answer =
  sse
    [ delta {|{"content":"fin"}|}; delta ~finish:{|"length"|} "{}" ]

let test_stream () =
  let wires = ref [ fragmented; answer ] in
  let fetch =
    Fetch_mock.client (fun req ->
        match !wires with
        | wire :: rest ->
            wires := rest;
            Fetch.Middleware.Pi.response ~status:200 ~version:`HTTP_1_1
              ~headers:
                (Http.Header.of_list [ ("Content-Type", "text/event-stream") ])
              ~body:(Eio.Flow.string_source wire)
              ~close:(fun () -> ())
              ~url:req.Fetch.Middleware.url ()
        | [] -> Alcotest.fail "unexpected request")
  in
  let client = Openrouter.of_fetch ~base_url:"https://example.test/v1" fetch in
  let echo =
    Agentkit.Agent.Tool.with_invoke (tool "echo") (fun c -> c.arguments)
  in
  let agent =
    Agentkit_openrouter.Agent.create ~client ~model:"m" ~max_tokens:5
      ~tools:[ echo ] ()
  in
  let events = ref [] in
  Agentkit_openrouter.Agent.send agent
    ~on_event:(fun e -> events := e :: !events)
    "go";
  let calls =
    List.filter_map
      (function Agentkit.Agent.Tool_call c -> Some c | _ -> None)
      (List.rev !events)
  in
  check "fragments joined by index, in index order"
    (List.map (fun (c : Agentkit.Agent.tool_call) -> (c.id, c.arguments)) calls
    = [ ("a", {|{"text":"one"}|}); ("b", {|{"text":"two"}|}) ]);
  check "tools executed"
    (List.exists
       (function
         | Agentkit.Agent.Tool_result ("echo", {|{"text":"two"}|}) -> true
         | _ -> false)
       !events);
  check "cut-off reported"
    (List.exists
       (function Agentkit.Agent.Cut_off { tokens = 5; _ } -> true | _ -> false)
       !events)

let png = "\x89PNG\r\n\x1a\n" ^ String.make 16 'x'

let test_images () =
  let format data = Option.map (fun (i : Chat.image) -> i.format)
      (Chat.image_of_string data) in
  check "formats come from the bytes"
    (format png = Some Chat.Png
    && format "\xff\xd8\xff\xe0rest" = Some Chat.Jpeg
    && format "GIF89a..." = Some Chat.Gif
    && format "RIFF\x00\x00\x00\x00WEBPVP8 " = Some Chat.Webp
    && format "<html>not an image" = None
    && format "" = None);
  let seen = ref "" in
  let fetch =
    Fetch_mock.client (fun req ->
        (match req.Fetch.Middleware.body with
        | Fetch.String s -> seen := s
        | _ -> ());
        Fetch_mock.respond
          ~headers:
            (Http.Header.of_list [ ("Content-Type", "application/json") ])
          {|{"id":"c","model":"m","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"red"}}]}|}
          req)
  in
  let client = Openrouter.of_fetch ~base_url:"https://example.test/v1" fetch in
  let image = Option.get (Chat.image_of_string png) in
  let r =
    Agentkit_openrouter.complete client ~model:"m"
      (Chat.request
         [ Chat.User_images { text = "What colour?"; images = [ image ] } ])
  in
  let contains part =
    let rec loop i =
      i + String.length part <= String.length !seen
      && (String.sub !seen i (String.length part) = part || loop (i + 1))
    in
    loop 0
  in
  check "images reach the wire as data URLs after the text"
    (r.text = Some "red"
    && contains {|"type":"text"|}
    && contains "data:image/png;base64,")

let () =
  Alcotest.run "agentkit core"
    [
      ( "turn",
        [
          ("answer", `Quick, test_answer);
          ("synthesis notice", `Quick, test_synthesis_notice);
          ("recovery and fallback", `Quick, test_recovery_and_fallback);
          ("guard fails closed", `Quick, test_guard_fails_closed);
          ("budget and bounds", `Quick, test_budget_and_bounds);
          ("cut-off", `Quick, test_cut_off);
          ("bind", `Quick, test_bind);
        ] );
      ("summary", [ ("summary", `Quick, test_summary) ]);
      ( "images",
        [
          ( "images",
            `Quick,
            fun () -> Eio_mock.Backend.run_full (fun _ -> test_images ()) );
        ] );
      ( "openrouter",
        [
          ( "stream",
            `Quick,
            fun () -> Eio_mock.Backend.run_full (fun _ -> test_stream ()) );
        ] );
    ]
