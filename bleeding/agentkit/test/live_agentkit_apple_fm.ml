(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Agent = Agentkit_apple_fm.Agent
module Event = Agentkit.Agent

let failures = ref 0

let check label condition =
  if condition then Printf.printf "ok   - %s\n%!" label
  else (
    incr failures;
    Printf.printf "FAIL - %s\n%!" label)

let available = function
  | `Available -> true
  | `Device_not_eligible | `Apple_intelligence_not_enabled | `Model_not_ready
  | `Unavailable _ ->
      false

let run env =
  if not (available (Apple_fm.Availability.get ())) then
    failwith "Apple Foundation Models is not available";
  Eio.Switch.run @@ fun sw ->
  let options =
    Apple_fm.Generation.options ~sampling:`Greedy ~maximum_response_tokens:32 ()
  in
  let agent = Agent.create ~sw ~instructions:"Answer briefly." ~options [] in
  let events = ref [] in
  Agent.send agent
    ~on_event:(fun event -> events := event :: !events)
    "Reply with the single word OK.";
  let events = List.rev !events in
  let content =
    List.filter_map
      (function Event.Content text -> Some text | _ -> None)
      events
    |> String.concat ""
  in
  check "Apple content streams through the common event type" (content <> "");
  check "Apple accounting reports the model context"
    (List.exists
       (function Event.Stats stats -> stats.ctx_size > 0 | _ -> false)
       events);
  check "Done is the final successful event"
    (match List.rev events with Event.Done :: _ -> true | _ -> false);
  let compacted = ref None in
  Agent.compact agent ~reason:"live adapter test" ~on_event:(function
    | Event.Compacted value -> compacted := Some value
    | _ -> ());
  check "Apple transcript compaction reports its summary"
    (match !compacted with
    | Some value -> String.trim value.summary <> ""
    | None -> false);
  let transcript = Agent.transcript agent in
  Agent.replace_transcript agent transcript;
  check "an Apple transcript can replace the active session" true;
  Agent.close agent;
  ignore env

let () =
  match Sys.getenv_opt "APPLE_FM_LIVE" with
  | None ->
      print_endline
        "SKIP - live Apple adapter test (set APPLE_FM_LIVE=1 to run)"
  | Some _ ->
      Eio_main.run run;
      if !failures <> 0 then exit 1
