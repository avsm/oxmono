(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Event = Agentkit.Agent
module Tool = Agentkit_apple_fm.Tool

let fail message = failwith message
let failures = ref 0

let check label condition =
  if condition then Printf.printf "ok   - %s\n" label
  else (
    incr failures;
    Printf.printf "FAIL - %s\n" label)

let codec tool_name =
  let open Apple_fm.Codec in
  Invoke.map tool_name (fun path limit -> (path, limit))
  |> Invoke.param ~enc:fst "path" string
  |> Invoke.param ~enc:snd ~default:10 "limit" int
  |> Invoke.seal

let () =
  let module _ : Agentkit.Agent.S = Agentkit_apple_fm.Agent in
  let driver =
    Agentkit_apple_fm.driver
      ~create:(fun _ -> fail "model constructor ran while listing")
      ()
  in
  check "Apple driver lists its qualified default model"
    (match Agentkit.Driver.models (Agentkit.Driver.merge [ driver ]) with
    | [ { Agentkit.Driver.name = "apple/default"; _ } ] -> true
    | _ -> false);
  let events = ref [] in
  let tool =
    Tool.v ~description:"Read a path." (codec "read") (fun (path, limit) ->
        Printf.sprintf "%s:%d" path limit)
  in
  let output =
    Tool.invoke
      ~on_event:(fun event -> events := event :: !events)
      tool {|{"path":"a"}|}
  in
  check "the adapter keeps the tool name" (Tool.name tool = "read");
  check "the wrapped tool returns its complete result" (output = "a:10");
  check "a defaulted argument is canonical in the call event"
    (match List.rev !events with
    | [ Event.Tool_call call; Event.Tool_result ("read", "a:10") ] ->
        call.name = "read" && call.arguments = {|{"path":"a","limit":10}|}
    | _ -> false);
  let events = ref [] in
  let failing =
    Tool.v ~description:"Fail." (codec "fail") (fun _ -> fail "broken")
  in
  let output =
    Tool.invoke
      ~on_event:(fun event -> events := event :: !events)
      failing {|{"path":"a","limit":2}|}
  in
  check "an ordinary handler exception is an explicit result"
    (String.starts_with ~prefix:"Error: Failure(\"broken\")" output);
  check "the failed call still has one result event"
    (match List.rev !events with
    | [ Event.Tool_call _; Event.Tool_result ("fail", result) ] ->
        result = output
    | _ -> false);
  let ran = ref false in
  let callback_failed =
    let tool =
      Tool.v ~description:"Do not run." (codec "guarded") (fun _ ->
          ran := true;
          "ran")
    in
    match
      Tool.invoke
        ~on_event:(fun _ -> fail "journal failed")
        tool {|{"path":"a"}|}
    with
    | exception Failure message -> message = "journal failed"
    | _ -> false
  in
  check "an event callback failure escapes tool invocation" callback_failed;
  check "a failed call event prevents the tool side effect" (not !ran);
  let events = ref [] in
  let output =
    Tool.invoke
      ~on_event:(fun event -> events := event :: !events)
      tool {|{"path":3}|}
  in
  check "invalid arguments are returned to the model"
    (String.starts_with ~prefix:"Error:" output);
  check "a call Apple cannot decode is not invented in the event stream"
    (!events = []);
  if !failures <> 0 then exit 1
