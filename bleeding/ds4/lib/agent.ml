(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A tool-using agent loop over the DeepSeek-V4 engine.

   Dsml renders the initial prompt and parses the model's replies. The exact
   token transcript is extended after that. V4.Session keeps the KV cache
   across turns, so each round prefills only its new tokens. *)

type stats = {
  ctx_used : int;
  ctx_size : int;
  prompt_tokens : int;
  generated : int;
  generate_seconds : float;
  prefill_seconds : float;
  tool_calls : int;
  turns : int;
  drafted : int;
  total_generated : int;
  total_generate_seconds : float;
}

type cut = { tokens : int; tool_call : bool }
type compaction = { before : int; after : int; summary : string }

type event =
  | Reasoning of string
  | Content of string
  | Tool_call of Dsml.tool_call
  | Tool_result of string * string
  | Stats of stats
  | Expanded of int
  | Cut_off of cut
  | Squeezed of int
  | Compacted of compaction
  | Done

exception Context_exhausted of { needed : int; ctx : int }
exception Tool_call_cut_off of { tokens : int; attempts : int }
exception Empty_reply of { attempts : int }
exception Malformed_tool_call of { message : string; attempts : int }

(* How many turns in a row may end with a tool call the token ceiling cut in
   half before the exchange is given up on. The model is told each time and can
   answer with a smaller call, so a run of them is a model that will not, and
   every one of them costs a whole reply's worth of generation. *)
let max_cut_attempts = 3
let max_empty_attempts = 3
let max_tool_error_attempts = 3

let () =
  Printexc.register_printer (function
    | Context_exhausted { needed; ctx } ->
        Some
          (Printf.sprintf
             "conversation needs %d tokens but the context is %d. Use --ctx to \
              raise it, or start a new session."
             needed ctx)
    | Tool_call_cut_off { tokens; attempts } ->
        Some
          (Printf.sprintf
             "the model reached its %d token ceiling while writing a tool \
              call, on %d turns running, so nothing was done. Ask for the work \
              in smaller steps."
             tokens attempts)
    | Empty_reply { attempts } ->
        Some
          (Printf.sprintf
             "the model ended %d turns without a reply or a tool call. Start a \
              new session or ask for the work in a smaller step."
             attempts)
    | Malformed_tool_call { message; attempts } ->
        Some
          (Printf.sprintf
             "the model wrote malformed tool syntax on %d turns running: %s"
             attempts message)
    | _ -> None)
  [@alert "-unsafe_multidomain"]

(* The counters for one [send], which spans several turns when the model calls
   tools. They are always read together and always reset together, so they are
   one value rather than six fields. *)
type exchange = {
  mutable prompt : int;
  mutable generated : int;
  mutable generate_seconds : float;
  mutable prefill_seconds : float;
  mutable tool_calls : int;
  mutable turns : int;
  mutable drafted : int;
}

let new_exchange () =
  {
    prompt = 0;
    generated = 0;
    generate_seconds = 0.;
    prefill_seconds = 0.;
    tool_calls = 0;
    turns = 0;
    drafted = 0;
  }

type t = {
  engine : V4.engine;
  mutable session : V4.Session.t;
  mutable closed : bool;
  seed : int64;
  max_ctx_size : int;
  speculative : bool;
  thinking : Dsml.thinking_mode;
  dialect : Dsml.dialect;
  temperature : float;
  top_p : float;
  min_p : float;
  max_tokens : int;
  tool_result_limit : int;
  now : unit -> float;
  tools : Tool.t list;
  mutable messages : Dsml.message list; (* oldest first; head is system *)
  mutable transcript : V4.Transcript.t;
  system_length : int; (* tokens up to the end of the system prompt *)
  mutable pending : int; (* the transcript's length before its open turn *)
  mutable next_call_id : int;
  mutable exchange : exchange; (* the current or most recent [send] *)
  (* These run for the life of the agent. *)
  mutable total_generated : int;
  mutable total_generate_seconds : float;
}

(* Keep a tool result under [limit] characters by removing its middle. The
   start of a file and the end of a command output are both worth keeping.

   What is removed this way cannot be asked for again, since nothing in the
   answer says what was in it, so the note says to ask for less rather than
   leaving the model to guess that it may. A tool that can bound its own answer
   should do so instead, and say where it stopped.

   This runs before the result is added to the conversation. The session reuses
   the KV prefix that still matches, so adding to the conversation is cheap
   while editing it later would force everything after the edit to be prefilled
   again. *)
let elide ~limit s =
  let n = String.length s in
  if limit <= 0 || n <= limit then s
  else begin
    (* Favour the start, which usually says what the output is. *)
    let head = limit * 2 / 3 in
    let tail = limit - head in
    Printf.sprintf
      "%s\n\
       … %d characters elided from the middle of this result. Ask again for \
       the part of it you need, such as a narrower range of lines.\n\
       %s"
      (String.sub s 0 head) (n - limit)
      (String.sub s (n - tail) tail)
  end

let think_of = function Dsml.Chat -> `None | Dsml.Thinking -> `High
let think_mode t = think_of t.thinking

let append_assistant_prefix t transcript =
  V4.Transcript.append_assistant_prefix transcript ~think:(think_mode t)
    ~ctx_size:(V4.Session.ctx t.session)

(* Open the assistant's turn, remembering where the conversation stood before
   it, which is where a compaction cuts. *)
let open_turn t =
  t.pending <- V4.Transcript.length t.transcript;
  append_assistant_prefix t t.transcript

let fits_context t candidate =
  let transcript = V4.Transcript.copy t.transcript in
  V4.Transcript.append_message transcript ~role:"tool" candidate;
  append_assistant_prefix t transcript;
  let prompt_tokens = V4.Transcript.length transcript in
  prompt_tokens + t.max_tokens + 1 <= t.max_ctx_size

let fit_tool_result t result =
  let result = Dsml.neutralise_specials ~dialect:t.dialect result in
  let fits candidate =
    (t.tool_result_limit <= 0 || String.length candidate <= t.tool_result_limit)
    && fits_context t candidate
  in
  if fits result then result
  else
    let rec search low high best =
      if low > high then best
      else
        let mid = low + ((high - low) / 2) in
        let candidate = elide ~limit:mid result in
        if fits candidate then search (mid + 1) high candidate
        else search low (mid - 1) best
    in
    let fallback =
      "Error: this tool result does not fit the remaining context. Ask for a \n\
       smaller range or a more specific result."
    in
    search 1 (String.length result) fallback

let create ?(system = "You are a helpful assistant.") ?(thinking = Dsml.Chat)
    ?(ctx_size = 32768) ?temperature ?top_p ?min_p ?(seed = 0x2545F4914F6CDD1DL)
    ?(max_tokens = 2048) ?(tool_result_limit = 4000) ?(max_ctx_size = 262144)
    ?(now = Unix.gettimeofday) ?(tools = []) engine =
  let session = V4.Session.create engine ~ctx_size ~seed in
  let family = V4.family engine in
  let temperature, top_p, min_p =
    match family with
    | `Deepseek | `Deepseek41 | `Qwen ->
        ( Option.value temperature ~default:1.0,
          Option.value top_p ~default:1.0,
          Option.value min_p ~default:0.05 )
    | `Glm ->
        ( Option.value temperature ~default:1.0,
          Option.value top_p ~default:0.95,
          Option.value min_p ~default:0.0 )
  in
  (* Advertise the tools in the system prompt. *)
  let tool_objs =
    List.map
      (fun t ->
        Dsml.Tool.v ~name:(Tool.name t) ~description:(Tool.description t)
          ~parameters:(Tool.schema t) ())
      tools
  in
  let dialect =
    match family with
    | `Deepseek -> Dsml.Deepseek
    | `Deepseek41 -> Dsml.Deepseek41
    | `Glm -> Dsml.Glm
    | `Qwen -> Dsml.Qwen
  in
  let transcript = V4.Transcript.create engine in
  V4.Transcript.append_think_prefix transcript ~think:(think_of thinking)
    ~ctx_size;
  (* The tool prompt is trusted control text. A DSML prompt is tokenised as
     rendered chat so that its markers become the model's reserved tokens, and
     V4.1 wants the system marker before it. GLM and Qwen spell their examples
     in ordinary text and take the prompt as a system message. *)
  if tool_objs <> [] then begin
    let prompt = Dsml.tool_prompt ~dialect tool_objs in
    match dialect with
    | Dsml.Glm | Dsml.Qwen ->
        V4.Transcript.append_message transcript ~role:"system" prompt
    | Dsml.Deepseek -> V4.Transcript.append_rendered transcript prompt
    | Dsml.Deepseek41 ->
        V4.Transcript.append_message transcript ~role:"system" "";
        V4.Transcript.append_rendered transcript prompt
  end;
  if system <> "" then
    V4.Transcript.append_message transcript ~role:"system" system;
  {
    engine;
    session;
    closed = false;
    seed;
    max_ctx_size = max max_ctx_size ctx_size;
    speculative = V4.draft_tokens engine > 0;
    thinking;
    dialect;
    temperature;
    top_p;
    min_p;
    max_tokens;
    tool_result_limit;
    now;
    tools;
    messages = [ Dsml.system ~tools:tool_objs system ];
    system_length = V4.Transcript.length transcript;
    pending = V4.Transcript.length transcript;
    transcript;
    next_call_id = 0;
    exchange = new_exchange ();
    total_generated = 0;
    total_generate_seconds = 0.;
  }

(* Every entry point that reaches the session passes through here. A closed
   session's cache has been freed, so using it would fault in the C stubs
   instead of saying which call was made too late. *)
let check_open t what =
  if t.closed then
    invalid_arg (Printf.sprintf "Agent.%s: the agent is closed" what)

let close t =
  if not t.closed then begin
    (* Mark first, so that a session that refuses to close still leaves an
       agent nobody can drive. *)
    t.closed <- true;
    V4.Session.close t.session
  end

let stats t =
  check_open t "stats";
  {
    (* Taken from the session, which is the engine's own count, so it stays
       correct even after a turn fails partway. *)
    ctx_used = V4.Session.pos t.session;
    ctx_size = V4.Session.ctx t.session;
    prompt_tokens = t.exchange.prompt;
    generated = t.exchange.generated;
    generate_seconds = t.exchange.generate_seconds;
    prefill_seconds = t.exchange.prefill_seconds;
    tool_calls = t.exchange.tool_calls;
    turns = t.exchange.turns;
    drafted = t.exchange.drafted;
    total_generated = t.total_generated;
    total_generate_seconds = t.total_generate_seconds;
  }

let prefill_progress t = V4.Session.prefill_progress t.session

let cancel t =
  check_open t "cancel";
  V4.Session.cancel t.session

(* A prompt of [needed] tokens followed by a reply of [max_tokens] fills that
   many tokens of the window, and generation must stop before the window is
   full, so a full reply needs one token more again. *)
let full_ctx ~max_tokens ~needed = needed + max_tokens + 1

let has_room ~squeeze ~max_tokens ~needed ~ctx =
  if squeeze then needed < ctx else full_ctx ~max_tokens ~needed <= ctx

let grow_to ~max_tokens ~max_ctx_size ~needed ~ctx =
  let wanted = max (2 * ctx) (full_ctx ~max_tokens ~needed) in
  let ctx_size = min max_ctx_size wanted in
  (* Refuse a size the turn would raise at anyway. The demand here is the one
     [run_turn] makes of an unsqueezed turn, so the two cannot drift apart and
     leave a retry raising the moment it is made. *)
  if
    ctx_size <= ctx
    || not (has_room ~squeeze:false ~max_tokens ~needed ~ctx:ctx_size)
  then None
  else Some ctx_size

(* Move to a larger context without disturbing the conversation, which lives in
   [messages] rather than in the session. The session is only a KV cache, so a
   bigger one can be built on the same engine and the model is not reloaded.

   The engine cannot resize a session, and a new one shares no prefix with the
   old, so the next sync prefills the whole conversation. Doubling rather than
   nudging is what keeps that affordable: each step at least halves the number
   still to come, so the prefills total about twice the last one. *)
let expand t ~needed =
  match
    grow_to ~max_tokens:t.max_tokens ~max_ctx_size:t.max_ctx_size ~needed
      ~ctx:(V4.Session.ctx t.session)
  with
  | None -> None
  | Some ctx_size ->
      (* Release the old cache before allocating the larger one. Holding both
         is worst exactly here, where the reason for growing is that memory is
         already tight.

         That leaves nothing to fall back on if the larger one cannot be had,
         so try again at the size that was working. Should even that fail, the
         agent is left holding a closed session, which reports itself rather
         than being used. *)
      let previous = V4.Session.ctx t.session in
      V4.Session.close t.session;
      (try t.session <- V4.Session.create t.engine ~ctx_size ~seed:t.seed
       with e ->
         t.session <- V4.Session.create t.engine ~ctx_size:previous ~seed:t.seed;
         raise e);
      Some ctx_size

(* Give every call an id so that results pair with calls when the conversation
   is rendered again. *)
let with_ids t calls =
  List.map
    (fun (tc : Dsml.tool_call) ->
      match tc.id with
      | Some _ -> tc
      | None ->
          let id = Printf.sprintf "call_%Lx_%d" t.seed t.next_call_id in
          t.next_call_id <- t.next_call_id + 1;
          { tc with id = Some id })
    calls

let prompt_tokens t = V4.Transcript.tokens t.transcript

(* Bring the session to [transcript]. A cache that is not a prefix of the
   prompt would be prefilled again from scratch, so it is first cut back to
   what the two share. *)
let sync_session t transcript =
  let pos = V4.Session.pos t.session in
  let common =
    V4.Session.common_prefix t.session (V4.Transcript.tokens transcript)
  in
  if common < pos then V4.Session.rewind t.session common;
  V4.Transcript.sync t.session transcript

(* Run one model turn and return the tool calls it made. A turn that is not
   [squeeze]d demands room for a whole reply, so [send] grows the context while
   there is still something to grow for. *)
let run_turn t ~squeeze ~on_event =
  let tokens = prompt_tokens t in
  let transcript_start = Array.length tokens in
  (* Check the length before syncing. The engine rejects a prompt with no room
     to generate, and reporting that here gives a clear error instead of a bare
     failure from the C stubs after the session has been disturbed. *)
  let ctx = V4.Session.ctx t.session in
  let needed = Array.length tokens in
  if not (has_room ~squeeze ~max_tokens:t.max_tokens ~needed ~ctx) then
    raise (Context_exhausted { needed; ctx });
  t.exchange.turns <- t.exchange.turns + 1;
  t.exchange.prompt <- needed;
  (* Time prefill and generation apart, since a slow turn can be either. *)
  let t_sync = t.now () in
  sync_session t t.transcript;
  let t_decode = t.now () in
  t.exchange.prefill_seconds <-
    t.exchange.prefill_seconds +. (t_decode -. t_sync);
  (* Never let generation reach the context bound. *)
  let room = V4.Session.ctx t.session - V4.Session.pos t.session in
  let budget = if room <= 1 then 0 else min t.max_tokens (room - 1) in
  (* Only a turn at [max_ctx_size] reaches this with less than a reply's room.
     Say so, since the alternative is a reply that stops in the middle for no
     reason the caller can see, or, at a budget of zero, no reply at all. *)
  if budget < t.max_tokens then on_event (Squeezed budget);
  let dec = Dsml.Stream.create ~dialect:t.dialect t.thinking in
  let content = Buffer.create 256 and reasoning = Buffer.create 256 in
  let calls = ref [] in
  let tool_error = ref None in
  let produced = ref 0 in
  let handle = function
    | Dsml.Stream.Content c ->
        Buffer.add_string content c;
        on_event (Content c)
    | Dsml.Stream.Reasoning r ->
        Buffer.add_string reasoning r;
        on_event (Reasoning r)
    | Dsml.Stream.Tool_call tc ->
        calls := tc :: !calls;
        on_event (Tool_call tc)
    | Dsml.Stream.Tool_error { message; generated } ->
        tool_error := Some message;
        let report = Printf.sprintf "Tool error: %s\n%s" message generated in
        Buffer.add_string content report;
        on_event (Content report)
    | Dsml.Stream.Done -> ()
  in
  (* The model stopping of its own accord and this loop stopping it are the
     same silence to everything downstream unless the difference is carried out
     of here. *)
  let rec loop n =
    if !calls <> [] then `Tool
    else if !tool_error <> None then `Tool_error
    else if n >= budget then `Ceiling
    else begin
      if V4.Session.is_cancelled t.session then raise V4.Session_interrupted;
      let temperature, top_p, min_p =
        match Dsml.Stream.sampling_mode dec with
        | `Configured -> (t.temperature, t.top_p, t.min_p)
        | `Greedy -> (0.0, 1.0, 0.0)
      in
      let tok = V4.Session.sample t.session ~temperature ~top_p ~min_p in
      if V4.token_is_stop t.engine tok then `Eos
      else begin
        let committed =
          if t.speculative then
            V4.Session.eval_speculative t.session tok ~max:(budget - n)
              ~temperature ~top_p ~min_p
          else begin
            V4.Session.eval t.session tok;
            [| tok |]
          end
        in
        t.exchange.drafted <- t.exchange.drafted + Array.length committed - 1;
        (* Drafted tokens are taken in order, and taking them stops where
           sampling one at a time would have stopped: at a stop token, or once
           the decoder has a whole tool call or a refusal. What the cache holds
           beyond that is rewound when the turn ends. *)
        let rec take i n =
          if i >= Array.length committed then loop n
          else
            let tok = committed.(i) in
            if i > 0 && V4.token_is_stop t.engine tok then `Eos
            else begin
              V4.Transcript.append_token t.transcript tok;
              incr produced;
              let text = V4.token_text t.engine tok in
              List.iter handle (Dsml.Stream.feed dec text);
              if !calls <> [] || !tool_error <> None then loop (n + 1)
              else take (i + 1) (n + 1)
            end
        in
        take 0 n
      end
    end
  in
  let stopped = loop 0 in
  (* Asked before finishing, which clears the decoder. *)
  let open_call = Dsml.Stream.in_tool_call dec in
  List.iter
    (fun ev ->
      match ev with
      | Dsml.Stream.Content c when open_call ->
          (* Reported but not recorded. An account of the turn should hold
             everything the model wrote, where a conversation carrying markup
             that never closed would hand the model its own wreckage back as
             an answer. The note that follows says what became of it. *)
          on_event (Content c)
      | Dsml.Stream.Tool_error { generated; _ }
        when open_call && stopped = `Ceiling ->
          on_event (Content generated)
      | ev -> handle ev)
    (Dsml.Stream.finish dec);
  if stopped = `Ceiling && open_call then
    V4.Transcript.truncate t.transcript transcript_start;
  V4.Transcript.finish_assistant t.transcript;
  (* The cache may hold what the transcript does not: drafted tokens past the
     end of the turn, or a tool call the ceiling cut and the transcript
     dropped. A sync to a prompt the cache is not a prefix of prefills the
     whole conversation again, so the cache is cut back to what the two share.
     A cache that merely stops short, such as one without the closing token
     the transcript adds, is left alone. *)
  let pos = V4.Session.pos t.session in
  let common =
    V4.Session.common_prefix t.session (V4.Transcript.tokens t.transcript)
  in
  if common < pos then V4.Session.rewind t.session common;
  let elapsed = t.now () -. t_decode in
  t.exchange.generated <- t.exchange.generated + !produced;
  t.exchange.generate_seconds <- t.exchange.generate_seconds +. elapsed;
  t.total_generated <- t.total_generated + !produced;
  t.total_generate_seconds <- t.total_generate_seconds +. elapsed;
  (* Reported before the statistics, being what ended the turn they count. *)
  let cut =
    match stopped with
    | `Eos | `Tool | `Tool_error -> None
    | `Ceiling ->
        let c = { tokens = budget; tool_call = open_call } in
        on_event (Cut_off c);
        Some c
  in
  (* Report after every turn, not only when the exchange ends. A request that
     makes the model work through several rounds of tool calls would otherwise
     show nothing until all of them were done. *)
  on_event (Stats (stats t));
  let calls = with_ids t (List.rev !calls) in
  (* Semantic messages serve events, journals and rollback. The inference
     transcript already holds the exact sampled tokens. Decoded text is made
     inert here so another consumer cannot render quoted control syntax as a
     new turn. *)
  t.messages <-
    t.messages
    @ [
        Dsml.assistant
          ~content:
            (Dsml.neutralise_specials ~dialect:t.dialect
               (Buffer.contents content))
          ~reasoning_content:
            (Dsml.neutralise_specials ~dialect:t.dialect
               (Buffer.contents reasoning))
          ~tool_calls:calls ();
      ];
  let tool_error =
    match (stopped, open_call) with `Ceiling, true -> None | _ -> !tool_error
  in
  (calls, cut, Buffer.length content > 0, tool_error)

(* ---- Compaction --------------------------------------------------------- *)

(* What the model is asked for when its conversation no longer fits. The
   summary replaces everything but the system prompt and a recent tail, so it
   has to carry what the rest carried: the goal, what was learnt and done, and
   where to look again rather than the bulk of what was looked at. *)
let summary_request =
  "The conversation above no longer fits the context, so it is about to be \
   replaced by a summary you write now. Write it for yourself, to carry on \
   from without the conversation. Include the user's goals and constraints, \
   the files you read or changed with their exact paths, the tool calls you \
   made and what they showed, the decisions you took and the approaches you \
   rejected and why, what remains to be done next, and where to reload any \
   bulky data, with exact paths and line ranges. Do not do any of the \
   remaining work and do not call a tool. Write only the summary."

let summary_intro =
  "The earlier part of this conversation was replaced by this summary, which \
   you wrote to carry on from:\n\n"

let summary_outro =
  "\n\n\
   The conversation continues from here. Carry on with the work where it \
   stopped, reading again whatever you need."

let max_summary_tokens = 4096
let min_summary_tokens = 256
let max_tail_tokens = 50000

(* Tokens the summarisation request costs beyond the conversation it
   summarises: its instruction text and the assistant prefix that opens the
   reply. Reserved up front so the cut point can be chosen without building
   the request just to measure it. A dialect whose actual cost runs over this
   only fails {!summarise}'s own check, which is safe, not unsafe. *)
let summary_overhead = 256

(* Generate the summary of the conversation up to [upto] greedily, without
   reasoning, in at most [budget] tokens. *)
let summarise t ~upto ~budget =
  let s = V4.Transcript.copy t.transcript in
  V4.Transcript.truncate s upto;
  V4.Transcript.append_message s ~role:"user" summary_request;
  V4.Transcript.append_assistant_prefix s ~think:`None
    ~ctx_size:(V4.Session.ctx t.session);
  let ctx = V4.Session.ctx t.session in
  let budget = min budget (ctx - V4.Transcript.length s - 1) in
  if budget < min_summary_tokens then None
  else begin
    sync_session t s;
    let dec = Dsml.Stream.create ~dialect:t.dialect Dsml.Chat in
    let buf = Buffer.create 4096 in
    let called = ref false in
    let take = function
      | Dsml.Stream.Content c -> Buffer.add_string buf c
      | Dsml.Stream.Tool_call _ | Dsml.Stream.Tool_error _ -> called := true
      | Dsml.Stream.Reasoning _ | Dsml.Stream.Done -> ()
    in
    let rec loop n =
      if n < budget && (not !called) && not (Dsml.Stream.in_tool_call dec) then begin
        if V4.Session.is_cancelled t.session then raise V4.Session_interrupted;
        let tok =
          V4.Session.sample t.session ~temperature:0.0 ~top_p:1.0 ~min_p:0.0
        in
        if not (V4.token_is_stop t.engine tok) then begin
          V4.Session.eval t.session tok;
          List.iter take (Dsml.Stream.feed dec (V4.token_text t.engine tok));
          loop (n + 1)
        end
      end
    in
    loop 0;
    List.iter take (Dsml.Stream.finish dec);
    let summary = String.trim (Buffer.contents buf) in
    if summary = "" then None
    else Some (Dsml.neutralise_specials ~dialect:t.dialect summary)
  end

(* Where the summary stops and the verbatim tail begins. It never reaches
   into [upto], the start of the turn in progress, which is always kept
   whole, and never asks for more than a session of [ctx] can hold while it
   generates the summary, which [upto] alone may already exceed. Within those
   two hard limits, it keeps as much recent history as [tail_budget] allows,
   and less only where the limits leave no choice. *)
let cut_point t ~upto ~before ~budget =
  let ctx = V4.Session.ctx t.session in
  let tail_budget = min max_tail_tokens (ctx / 10) in
  let safe = ctx - budget - summary_overhead in
  let upper = max t.system_length (min upto safe) in
  let lower = max t.system_length (before - tail_budget) in
  if lower <= upper then lower else upper

(* Replace the conversation before [cut_point]'s cut with a summary, keeping
   whatever follows it, [upto] included, as it is. Returns false, changing
   nothing, where there is no room to write a summary, where doing so would
   not shrink anything, or where the transcript holds an image, whose
   position a rebuilt transcript cannot carry. *)
let compact t ~upto ~on_event =
  if V4.Transcript.has_images t.transcript then false
  else
    let ctx = V4.Session.ctx t.session in
    let budget = min max_summary_tokens (ctx / 8) in
    let before = V4.Transcript.length t.transcript in
    let off = cut_point t ~upto ~before ~budget in
    (* A summary costs a fixed wrapper of its own on top of whatever the
       model writes, so compacting barely any history can leave the
       conversation larger than it found it. This is what a tool result too
       big for a small, mostly-empty conversation looks like: [upto] sits
       just past the system prompt, [cut_point] correctly refuses to reach
       into it, and there is nothing worth summarising as a result. Leave it
       to the caller's own fallback rather than pay for a summary that would
       not shrink anything. *)
    if off - t.system_length < min_summary_tokens then false
    else
      match summarise t ~upto:off ~budget with
      | None -> false
      | Some summary ->
          let tokens = V4.Transcript.tokens t.transcript in
          let rebuilt =
            V4.Transcript.of_tokens t.engine
              (Array.sub tokens 0 t.system_length)
          in
          V4.Transcript.append_message rebuilt ~role:"user"
            (summary_intro ^ summary ^ summary_outro);
          let base = V4.Transcript.length rebuilt in
          V4.Transcript.append_tokens rebuilt
            (Array.sub tokens off (before - off));
          (* The conversation before the summary is gone, and nothing
             outside this module reads [messages], so it is rebuilt bare.
             Where [off] reaches into [t.pending] (always true when [upto] is
             [t.pending] itself, never when a tool result's [upto] runs past
             it) the translated value is exact; the other case is overwritten
             by the next {!open_turn} before anything reads it. *)
          t.messages <-
            [
              List.hd t.messages;
              Dsml.user (summary_intro ^ summary ^ summary_outro);
            ];
          t.pending <- base + max 0 (t.pending - off);
          t.transcript <- rebuilt;
          on_event
            (Compacted { before; after = V4.Transcript.length rebuilt; summary });
          true

(* What the model is told when the ceiling cut its call in half. It cannot see
   the ceiling, and an exchange that ended here would report work as finished
   that was never started, so the turn is spent saying what happened and what
   to do about it. *)
let cut_note ~tokens =
  Printf.sprintf
    "Your reply reached its ceiling of %d tokens while you were writing a tool \
     call, so the call was discarded and nothing was done. The conversation is \
     as it was before it. Make the next call small enough to finish inside \
     that ceiling, splitting the work across several calls where one of them \
     will not hold it."
    tokens

let empty_note =
  "Your last turn ended without a reply or a tool call. Answer the request or \
   make one small tool call."

let tool_error_note message =
  Printf.sprintf
    "Your last tool call was refused because its syntax was malformed: %s. \
     Write one complete tool call using the syntax in the system prompt. A \
     value that contains its own closing tag must write that tag's < as &lt;, \
     as the system prompt says."
    message

let send t ~on_event prompt =
  check_open t "send";
  V4.Session.clear_cancel t.session;
  (* A failed turn would otherwise leave a user message the model never
     answered, and one that overran the context would overrun it again on every
     later turn. Restoring the conversation keeps the session usable. *)
  let committed_messages = ref t.messages in
  let committed_transcript = ref (V4.Transcript.copy t.transcript) in
  t.messages <- t.messages @ [ Dsml.user prompt ];
  V4.Transcript.append_message t.transcript ~role:"user" prompt;
  open_turn t;
  (* [stats] reports the exchange just run, so the counters start from zero and
     accumulate across its turns. *)
  t.exchange <- new_exchange ();
  (* A turn that called something has moved, whatever else it cut short, so
     only the turns that called nothing count towards the limit. *)
  let rec turn ~squeeze ~cuts ~empty ~tool_errors () =
    let calls, cut, has_content, tool_error = run_turn t ~squeeze ~on_event in
    let cut_call =
      match cut with Some c when c.tool_call -> Some c.tokens | _ -> None
    in
    match (calls, cut_call, tool_error) with
    | _, _, Some message when tool_errors + 1 >= max_tool_error_attempts ->
        raise (Malformed_tool_call { message; attempts = tool_errors + 1 })
    | _, _, Some message ->
        let note = tool_error_note message in
        t.messages <- t.messages @ [ Dsml.tool ~id:"" note ];
        V4.Transcript.append_message t.transcript ~role:"tool" note;
        open_turn t;
        turn ~squeeze ~cuts:0 ~empty:0 ~tool_errors:(tool_errors + 1) ()
    | [], None, None when has_content -> on_event Done
    | [], None, None when empty + 1 >= max_empty_attempts ->
        raise (Empty_reply { attempts = empty + 1 })
    | [], None, None ->
        t.messages <- t.messages @ [ Dsml.user empty_note ];
        V4.Transcript.append_message t.transcript ~role:"user" empty_note;
        open_turn t;
        turn ~squeeze ~cuts:0 ~empty:(empty + 1) ~tool_errors:0 ()
    | [], Some tokens, None when cuts + 1 >= max_cut_attempts ->
        raise (Tool_call_cut_off { tokens; attempts = cuts + 1 })
    | [], Some tokens, None ->
        let note = cut_note ~tokens in
        t.messages <- t.messages @ [ Dsml.user note ];
        V4.Transcript.append_message t.transcript ~role:"user" note;
        open_turn t;
        turn ~squeeze ~cuts:(cuts + 1) ~empty:0 ~tool_errors:0 ()
    | calls, _, None ->
        let results =
          List.mapi
            (fun index (tc : Dsml.tool_call) ->
              let result =
                match
                  List.find_opt (fun tl -> Tool.name tl = tc.name) t.tools
                with
                | Some tl -> (
                    match Tool.invoke_result tl tc with
                    | result -> result
                    | exception e ->
                        Tool.text ("Error: tool failed: " ^ Printexc.to_string e)
                    )
                | None -> Tool.text ("Error: unknown tool " ^ tc.name)
              in
              t.exchange.tool_calls <- t.exchange.tool_calls + 1;
              (* Report the whole result, but store a shortened copy, so that a
               large output costs display space rather than context.  The
               stored copy is made inert for the same reason a reply is: a tool
               that reads a file describing DSML, this repository's own sources
               among them, would otherwise put real markers into the next
               prompt. *)
              on_event (Tool_result (tc.name, result.text));
              let text =
                Printf.sprintf "Tool result %d (%s):\n%s%s" (index + 1) tc.name
                  result.text
                  (if
                     result.text <> ""
                     && result.text.[String.length result.text - 1] <> '\n'
                   then "\n"
                   else "")
              in
              (text, result.images))
            calls
        in
        let full =
          String.concat "" (List.map (fun (text, _) -> text) results)
        in
        (* A result that does not fit is better kept whole after a compaction
           than cut in the middle, since what is cut cannot be asked for
           again. *)
        if
          (not
             (fits_context t (Dsml.neutralise_specials ~dialect:t.dialect full)))
          && compact t ~upto:(V4.Transcript.length t.transcript) ~on_event
        then begin
          committed_messages := t.messages;
          committed_transcript := V4.Transcript.copy t.transcript
        end;
        let stored = fit_tool_result t full in
        let id =
          match calls with
          | (tc : Dsml.tool_call) :: _ -> Option.value tc.id ~default:""
          | [] -> ""
        in
        t.messages <- t.messages @ [ Dsml.tool ~id stored ];
        let images = List.concat_map snd results in
        if images = [] then
          V4.Transcript.append_message t.transcript ~role:"tool" stored
        else begin
          let remaining = ref stored in
          let parts = ref [] in
          List.iter
            (fun _ ->
              parts := !remaining :: !parts;
              remaining := "")
            images;
          V4.Transcript.append_multimodal_message t.transcript ~role:"tool"
            ~text_parts:(List.rev (!remaining :: !parts))
            ~images
        end;
        committed_messages := t.messages;
        committed_transcript := V4.Transcript.copy t.transcript;
        open_turn t;
        turn ~squeeze ~cuts:0 ~empty:0 ~tool_errors:0 ()
  in
  (* Growing keeps the whole conversation, so it comes first. Once the context
     cannot grow, the conversation is compacted, once per turn, and only a turn
     that still does not fit runs squeezed. *)
  let rec attempt ~squeeze ~compacted () =
    try turn ~squeeze ~cuts:0 ~empty:0 ~tool_errors:0 ()
    with Context_exhausted { needed; ctx } as e -> (
      match expand t ~needed with
      | Some ctx_size ->
          on_event (Expanded ctx_size);
          attempt ~squeeze ~compacted ()
      | None when (not compacted) && compact t ~upto:t.pending ~on_event ->
          (* [compact] already carries the open turn's assistant prefix into
             the rebuilt transcript and moves [t.pending] to sit just before
             it, so the turn is not reopened here: doing so would append a
             second prefix after the one already there. *)
          committed_messages := t.messages;
          committed_transcript := V4.Transcript.copy t.transcript;
          attempt ~squeeze:false ~compacted:true ()
      (* The context is at its ceiling and the prompt still fits. Generate in
         the room that is left rather than refuse the turn. *)
      | None when (not squeeze) && needed < ctx ->
          attempt ~squeeze:true ~compacted ()
      | None -> raise e)
  in
  try attempt ~squeeze:false ~compacted:false ()
  with e ->
    t.messages <- !committed_messages;
    t.transcript <- !committed_transcript;
    V4.Session.clear_cancel t.session;
    raise e
