(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The fold from agent events to journal kinds, with no engine and no model.

   What matters here is where a boundary falls. A reply arrives a token at a
   time and must reach the account as one record, at the point it is complete
   and before anything it explains happens, so the tests are about which events
   flush and in what order. The rest is the pairing: a turn asks for every tool
   call before any is made, so a result belongs to the oldest call waiting, and
   a result answering no call must still be recorded rather than dropped.

   The clock is a counter this file advances, since a duration measured against
   the real one is not a value a test can state. *)

module Agent = Agentkit.Agent
module Journal = Agentkit.Journal
module Trace = Agentkit.Trace

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

(* A clock that advances one second per reading, so a call's duration is the
   number of readings between its request and its answer. *)
let ticking () =
  let t = ref 0. in
  fun () ->
    t := !t +. 1.;
    !t

(* A trace over a list that collects what it emitted, in order. *)
let collecting ?tool_result_limit ?now () =
  let got = ref [] in
  let t =
    Trace.create ?tool_result_limit ?now ~emit:(fun k -> got := k :: !got) ()
  in
  (t, fun () -> List.rev !got)

let call name arguments = Agent.Tool_call { Agent.name; arguments }

let stats ctx_used =
  Agent.Stats
    {
      Agent.ctx_used;
      ctx_size = 32768;
      prompt_tokens = 100;
      generated = 10;
      generate_seconds = 1.;
      prefill_seconds = 1.;
      tool_calls = 1;
      turns = 1;
      drafted = 0;
      total_generated = 10;
      total_generate_seconds = 1.;
    }

let names got = List.map Journal.kind_name got

(* Text. Each block is one record, whatever the tokens were, and reasoning
   precedes content because that is the order the model produced them in. *)

let () =
  let t, got = collecting () in
  List.iter (Trace.event t)
    [
      Agent.Reasoning "I will ";
      Agent.Reasoning "look.";
      Agent.Content "Looking";
      Agent.Content " now.";
      Agent.Done;
    ];
  check "a turn's text is two records, reasoning first"
    (got ()
    = [ Journal.Reasoning "I will look."; Journal.Content "Looking now." ]);
  let t, got = collecting () in
  List.iter (Trace.event t)
    [ Agent.Content "one"; Agent.Done; Agent.Content "two"; Agent.Done ];
  check "each block flushes on its own"
    (got () = [ Journal.Content "one"; Journal.Content "two" ])

let () =
  let t, got = collecting () in
  List.iter (Trace.event t)
    [ Agent.Content "about to read"; call "read" "{}"; Agent.Done ];
  check "a tool call flushes the text that precedes it"
    (names (got ()) = [ "content"; "tool_call" ]);
  let t, got = collecting () in
  List.iter (Trace.event t) [ Agent.Content "done"; stats 900; Agent.Done ];
  check "stats flush the text before the stats record"
    (names (got ()) = [ "content"; "stats" ]);
  let t, got = collecting ~now:(ticking ()) () in
  List.iter (Trace.event t)
    [
      call "read" "{}";
      Agent.Content "after";
      Agent.Tool_result ("read", "A");
      Agent.Done;
    ];
  check "a result is not a boundary, so the text after the call follows it"
    (names (got ()) = [ "tool_call"; "tool_result"; "content" ]);
  let t, got = collecting () in
  List.iter (Trace.event t) [ Agent.Content "cut off"; Agent.Squeezed 12 ];
  check "an event that ends nothing leaves the text buffered"
    (names (got ()) = [ "squeezed" ]);
  Trace.event t Agent.Done;
  check "the end of the exchange flushes it"
    (names (got ()) = [ "squeezed"; "content" ])

(* Pairing. Every call of a turn is asked for before any is answered, so the
   results come back against the oldest call outstanding. *)

let () =
  let t, got = collecting ~now:(ticking ()) () in
  List.iter (Trace.event t)
    [
      call "read" "{\"path\":\"a\"}";
      call "read" "{\"path\":\"b\"}";
      Agent.Tool_result ("read", "A");
      Agent.Tool_result ("read", "B");
      Agent.Done;
    ];
  let calls =
    List.filter_map
      (function
        | Journal.Tool_call c -> Some (c.Journal.call, c.Journal.arguments)
        | _ -> None)
      (got ())
  in
  let results =
    List.filter_map
      (function
        | Journal.Tool_result r -> Some (r.Journal.call, r.Journal.output)
        | _ -> None)
      (got ())
  in
  check "calls are numbered from 1 in the order they were asked for"
    (calls = [ (1, "{\"path\":\"a\"}"); (2, "{\"path\":\"b\"}") ]);
  check "a result takes the oldest call still waiting"
    (results = [ (1, "A"); (2, "B") ])

let () =
  let t, got = collecting ~now:(ticking ()) () in
  List.iter (Trace.event t) [ Agent.Tool_result ("read", "A"); Agent.Done ];
  match got () with
  | [ Journal.Tool_result r ] ->
      check "a result answering no call is recorded with call 0"
        (r.Journal.call = 0);
      check "and is timed from when it was seen" (r.Journal.seconds = 1.)
  | _ -> check "a result answering no call is recorded with call 0" false

let () =
  let t, got = collecting ~now:(ticking ()) () in
  List.iter (Trace.event t)
    [ call "bash" "{}"; Agent.Tool_result ("bash", "out"); Agent.Done ];
  match got () with
  | [ _; Journal.Tool_result r ] ->
      (* One reading of the clock when the call was queued, one when its result
         arrived. *)
      check "a call is timed from when it was asked for" (r.Journal.seconds = 1.)
  | _ -> check "a call is timed from when it was asked for" false

(* The ids count over the whole run, so a reader following an account across the
   turns of a session sees each call once. Neither the stats that end a turn nor
   the end of an exchange starts the count again. *)

let () =
  let t, got = collecting ~now:(ticking ()) () in
  List.iter (Trace.event t)
    [
      call "read" "{}";
      Agent.Tool_result ("read", "A");
      stats 900;
      call "read" "{}";
      Agent.Tool_result ("read", "B");
      Agent.Done;
    ];
  let ids =
    List.filter_map
      (function
        | Journal.Tool_call c -> Some c.Journal.call
        | Journal.Tool_result r -> Some r.Journal.call
        | _ -> None)
      (got ())
  in
  check "call ids carry on across the turns of a run" (ids = [ 1; 1; 2; 2 ])

(* A caller writing to a journal that will not take a record must stop, so the
   fold lets the failure out rather than going on with an account that reads as
   complete. *)

let () =
  let raising () = Trace.create ~emit:(fun _ -> raise Exit) () in
  let escaped f =
    try
      f ();
      false
    with Exit -> true
  in
  let t = raising () in
  check "an exception out of emit escapes the event that emitted"
    (escaped (fun () -> Trace.event t (call "read" "{}")));
  let t = raising () in
  Trace.event t (Agent.Content "buffered");
  check "and escapes the event that flushed"
    (escaped (fun () -> Trace.event t Agent.Done))

(* Truncation. The flag says the model saw less than the record holds, so it
   follows the limit the agent was given rather than the length of the record. *)

let () =
  let t, got = collecting ~tool_result_limit:4 () in
  List.iter (Trace.event t)
    [
      call "read" "{}";
      Agent.Tool_result ("read", "1234");
      call "read" "{}";
      Agent.Tool_result ("read", "12345");
      Agent.Done;
    ];
  let flags =
    List.filter_map
      (function
        | Journal.Tool_result r -> Some (r.Journal.truncated, r.Journal.output)
        | _ -> None)
      (got ())
  in
  check "a result at the limit is not truncated"
    (flags = [ (false, "1234"); (true, "12345") ]);
  (* A limit of zero is no limit, so the model saw the whole result and the
     account must not say otherwise. *)
  let t, got = collecting ~tool_result_limit:0 () in
  List.iter (Trace.event t)
    [
      call "read" "{}";
      Agent.Tool_result ("read", String.make 100000 'x');
      Agent.Done;
    ];
  let flags =
    List.filter_map
      (function Journal.Tool_result r -> Some r.Journal.truncated | _ -> None)
      (got ())
  in
  check "no limit truncates nothing" (flags = [ false ])

(* The context events pass straight through, since neither ends a block. *)

let () =
  let t, got = collecting () in
  List.iter (Trace.event t) [ Agent.Expanded 65536; Agent.Squeezed 40 ];
  check "expanded and squeezed are emitted as they arrive"
    (got () = [ Journal.Expanded 65536; Journal.Squeezed 40 ])

(* A cut ends the text of its turn, so the text has to be on the record before
   the reason it stopped. An account that says the reply was cut off and then
   shows the reply reads as though something followed the cut. *)

let () =
  let t, got = collecting () in
  List.iter (Trace.event t)
    [
      Agent.Content "half a thought";
      Agent.Cut_off { Agent.tokens = 2048; tool_call = true };
    ];
  check "a cut flushes the text of its turn before it"
    (got ()
    = [
        Journal.Content "half a thought";
        Journal.Cut_off { Journal.tokens = 2048; tool_call = true };
      ])

let () =
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
