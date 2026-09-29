(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The brief a wake-up starts from. It is pure, so what goes into a context is
   checked here rather than against an engine.

   The property that matters is the separation. A scheduled task is an
   instruction from a person, in a file numpty cannot write, and an open item is
   numpty's own note that something is unfinished. A brief that ran the two
   together would let the agent edit its own orders, so each has to be in its own
   section and said to be what it is.

   The digest is the other one. A tool result runs to kilobytes and a reply to
   paragraphs, and a digest that carried either whole would cost the context the
   work needs, so a record becomes one line and a long one is cut with its size
   named. *)

module Brief = Numpty_daemon.Brief
module Journal = Agentkit.Journal
module Memory = Agentkit.Memory

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

(* Where in the brief a passage falls, so that two passages can be told to be in
   different sections rather than merely both present. *)
let index s sub =
  let n = String.length s and m = String.length sub in
  let rec at i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else at (i + 1)
  in
  at 0

let entry ?(tags = []) id kind title body : Memory.entry =
  { Memory.id; kind; title; body; tags; created = 1; updated = 1 }

let entries =
  [
    entry "ds4-upstream" Memory.Fact "the upstream tag moved"
      "upstream is at v0.9 as of yesterday" ~tags:[ "ds4" ];
    entry "how-to-build" Memory.Procedure "building the vendored engine"
      "run csrc/vendor.sh and then dune build";
    entry "finish-the-digest" Memory.Open_item "the digest is half done"
      "three feeds are left, and the fourth was the one that timed out";
    entry "changes-url" Memory.Reference "where the changelog is"
      "https://example.invalid/CHANGES.md";
  ]

let record seq kind : Journal.record =
  { Journal.v = 1; seq; time = "2026-08-08T09:14:07Z"; run = 9; kind }

(* The system prompt. A model that is not told its context dies writes nothing
   down and loses a day's work, so the paragraph has to say it. *)

let () =
  let s = Brief.system_prompt in
  check "the system prompt says the conversation does not survive"
    (contains s "does not survive");
  check "and that memory is what does" (contains s "memory does survive");
  check "and names the tool that writes it" (contains s "memory_write");
  check "and says which kind unfinished work goes in" (contains s "open_item");
  check "and that a wake-up can end because the context filled"
    (contains s "filled up");
  check "the handover prompt asks what should outlast the wake-up"
    (contains Brief.handover_prompt "outlast")

(* The four parts, and the separation between the third and the second. *)

let () =
  let b =
    Brief.assemble ~version:17 ~entries ~task:"feeds"
      ~prompt:"Check the feeds in memory and write down what moved." ~session:1
      ~history:[ record 400 (Journal.Content "I fetched two of the four.") ]
  in
  let u = b.Brief.user in
  check "the brief names the memory version it came from"
    (contains u "memory version 17" && b.Brief.version = 17);
  check "a fact is there in full"
    (contains u "upstream is at v0.9 as of yesterday");
  check "a procedure is there in full"
    (contains u "run csrc/vendor.sh and then dune build");
  check "an open item is there in full" (contains u "three feeds are left");
  check "a reference is there as a pointer"
    (contains u "https://example.invalid/CHANGES.md");
  check "the task's prompt is there"
    (contains u "Check the feeds in memory and write down what moved.");
  check "the task is said to be a person's instruction"
    (contains u "not yours to change");
  check "the open items are counted" (b.Brief.open_items = 1);
  check "the byte count covers both messages"
    (b.Brief.bytes
    = String.length Brief.system_prompt + String.length b.Brief.user);
  check "the system prompt is the one that was written"
    (b.Brief.system = Brief.system_prompt);
  (* The separation. The open items are their own section, above the orders, and
     each says which it is. *)
  check "the unfinished work is in a section of its own"
    (contains u "## Unfinished work");
  check "the orders are in a section of their own"
    (contains u "## What you have been asked to do");
  check "and the open item falls outside the section holding the orders"
    (match
       (index u "finish-the-digest", index u "What you have been asked")
     with
    | Some item, Some orders -> item < orders
    | _ -> false);
  check "the open item is not presented as a task"
    (contains u "your own note to yourself");
  check "the digest of the journal is there"
    (contains u "I fetched two of the four.")

(* A first session says nothing about continuing, and a later one says where it
   came from, since a model that thinks it is starting will start again. *)

let () =
  let assemble session =
    (Brief.assemble ~version:2 ~entries ~task:"feeds" ~prompt:"go" ~session
       ~history:[])
      .Brief.user
  in
  check "a first session is not told it is carrying on"
    (not (contains (assemble 1) "This is session"));
  check "a later session is told which it is"
    (contains (assemble 3) "This is session 3");
  check "and told to carry on from the open items"
    (contains (assemble 3) "Carry on from the open items")

(* An empty memory says so rather than presenting an empty section. *)

let () =
  let u =
    (Brief.assemble ~version:0 ~entries:[] ~task:"probe" ~prompt:"go" ~session:1
       ~history:[])
      .Brief.user
  in
  check "an empty memory says it holds nothing"
    (contains u "Memory holds nothing yet");
  check "and that nothing was left unfinished"
    (contains u "left nothing unfinished")

(* The digest: one line per record, outcomes rather than text. *)

let () =
  check "an empty history says so"
    (contains (Brief.digest []) "Nothing has happened");
  let long =
    String.concat "\n" (List.init 400 (fun i -> Printf.sprintf "line %d" i))
  in
  let d =
    Brief.digest
      [
        record 1
          (Journal.Wake
             {
               Journal.task = "feeds";
               due = "2026-08-08T09:00:00Z";
               why = "due";
               serial = None;
             });
        record 2
          (Journal.Tool_call
             {
               Journal.call = 1;
               name = "fetch";
               arguments = {|{"url":"https://x/"}|};
             });
        record 3
          (Journal.Tool_result
             {
               Journal.call = 1;
               name = "fetch";
               output = long;
               seconds = 1.5;
               truncated = true;
             });
        record 4
          (Journal.Memory_write
             {
               Journal.from = 1;
               to_ = 2;
               entry = "a";
               why = "recorded the tag";
             });
        record 5 (Journal.Prompt "the whole brief, which is not a digest line");
        record 6
          (Journal.Stats
             Agentkit.Agent.
               {
                 ctx_used = 1;
                 ctx_size = 2;
                 prompt_tokens = 0;
                 generated = 0;
                 generate_seconds = 0.;
                 prefill_seconds = 0.;
                 tool_calls = 0;
                 turns = 0;
                 drafted = 0;
                 total_generated = 0;
                 total_generate_seconds = 0.;
               });
      ]
  in
  check "a wake is one line naming the task and why"
    (contains d "woke for task feeds (due)");
  check "a tool call keeps the adapter's canonical arguments"
    (contains d {|called fetch {"url":"https://x/"}|});
  check "a tool result is cut to its first line"
    (contains d "fetch answered: line 0" && not (contains d "line 399"));
  check "and says how large the whole of it was"
    (contains d (string_of_int (String.length long)));
  check "a memory write says which version and why"
    (contains d "memory version 2: recorded the tag");
  check
    "a prompt is not a digest line, the record of it being the prompt itself"
    (not (contains d "which is not a digest line"));
  check "and neither is a stats record" (not (contains d "ctx_used"));
  check "each record is one line"
    (List.length (String.split_on_char '\n' (String.trim d)) = 4)

(* A digest of more than it will carry keeps the recent end and says how many
   lines it left out. *)

let () =
  let many =
    List.init 300 (fun i ->
        record i (Journal.Content (Printf.sprintf "said %d" i)))
  in
  let d = Brief.digest many in
  check "an over-long digest keeps the recent end"
    (contains d "said 299" && not (contains d "said 0\n"));
  check "and says how many lines it left out"
    (contains d "earlier lines left out")

(* [since_handover] against a journal a test writes, since what a wake-up
   inherits is what came after the last handover and nothing before it. *)

let () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_file "ds4-numpty-brief" "" in
  Sys.remove tmp;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" tmp)))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let j =
        Journal.create ~sw ~clock:(Eio.Stdenv.clock env) ~run:1 ~seq:1 dir
      in
      ignore (Journal.append j (Journal.Content "before the handover"));
      ignore (Journal.append j (Journal.Handover 4));
      ignore (Journal.append j (Journal.Content "after the handover"));
      ignore (Journal.append j (Journal.Content "and after that"));
      let since = Brief.since_handover dir in
      check "only what came after the last handover is inherited"
        (List.map
           (fun (r : Journal.record) ->
             match r.Journal.kind with Journal.Content c -> c | _ -> "?")
           since
        = [ "after the handover"; "and after that" ]))

let () =
  if !failures > 0 then begin
    Printf.printf "\n%d failure(s)\n" !failures;
    exit 1
  end
