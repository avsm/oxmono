(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Agent = Ds4.Agent
module Driver = Agentkit.Driver
module Common_agent = Agentkit.Agent
module Journal = Agentkit.Journal
module Memory = Agentkit.Memory
module Trace = Agentkit.Trace

type config = {
  ctx_size : int;
  max_ctx_size : int;
  handover_at : float;
  max_sessions : int;
  thinking : Dsml.thinking_mode;
  seed : int64;
  tool_result_limit : int;
}

let default_config =
  {
    ctx_size = 32768;
    max_ctx_size = 262144;
    handover_at = 0.75;
    max_sessions = 5;
    thinking = Dsml.Chat;
    seed = 1L;
    tool_result_limit = 4000;
  }

type outcome = Finished | Faulted of string

exception Stop_requested

let run ~cancel_on_stop ~clock ~store ~create ~config ~fault ~stop ~publish
    ~prefill ~task ~prompt =
  let journal = Store.journal store in
  let memory = Store.memory store in
  let journal_dir = Store.journal_dir (Store.root store) in
  let append k = ignore (Journal.append journal k) in
  (* What the control fiber reads. It is published at every state change rather
     than asked for, because [Agent.stats] waits for the engine and a turn holds
     the engine for minutes. Each publication is a whole immutable record, so a
     reader never sees half of one. *)
  let started = Journal.rfc3339 (Unix.gettimeofday ()) in
  let calls = ref 0 and turns = ref 0 and ctx_used = ref 0 in
  let job = ref None in
  let update f =
    job := Option.map f !job;
    publish !job
  in
  (* The status is derived from the same kinds the journal takes, so what a
     person is shown while a wake-up runs and what they read afterwards cannot
     disagree. The counts are the wake-up's rather than the session's: a turn
     count comes from here and not from the stats, which start again with each
     session on the task. *)
  let emit (k : Journal.kind) =
    (match k with
    | Journal.Tool_call { call; name; _ } ->
        calls := call;
        update (fun j -> { j with Status.tool = Some name; tool_calls = call })
    | Journal.Tool_result _ -> update (fun j -> { j with Status.tool = None })
    | Journal.Stats s ->
        ctx_used := s.Common_agent.ctx_used;
        incr turns;
        update (fun j ->
            {
              j with
              Status.ctx_used = s.Common_agent.ctx_used;
              ctx_size = s.Common_agent.ctx_size;
              turns = !turns;
            })
    | Journal.Expanded n -> update (fun j -> { j with Status.ctx_size = n })
    | _ -> ());
    append k
  in
  (* One trace for the whole wake-up, so the call ids run on across a session
     that handed over and continued. *)
  let trace =
    Trace.create ~tool_result_limit:config.tool_result_limit
      ~now:(fun () -> Eio.Time.now clock)
      ~emit ()
  in
  let on_event event = Trace.event trace event in
  let send ?(interruptible = true) agent text =
    append (Journal.Prompt text);
    let run () = Driver.send agent ~on_event text in
    let rec watch_stop () =
      if stop () then begin
        Driver.cancel agent;
        raise Stop_requested
      end;
      Eio.Time.sleep clock 0.1;
      watch_stop ()
    in
    if interruptible && cancel_on_stop then Eio.Fiber.first run watch_stop
    else run ()
  in
  (* The handover is what makes the wake-up survivable, so a context that has
     no room left for it is reported and the version it did reach is still
     named. Losing the record as well as the writes would leave the next brief
     unable to say where this one got to. *)
  let handover agent =
    (try send ~interruptible:false agent Brief.handover_prompt with
    | Agent.Context_exhausted { needed; ctx } ->
        append
          (Journal.Error
             {
               Journal.where = "handover";
               what =
                 Printf.sprintf
                   "the handover prompt needed %d tokens in a context of %d, \
                    so it was not sent and nothing more was written to memory"
                   needed ctx;
             })
    | Agent.Tool_call_cut_off { tokens; attempts } ->
        append
          (Journal.Error
             {
               Journal.where = "handover";
               what =
                 Printf.sprintf
                   "the model could not write a tool call inside its %d token \
                    reply, on %d turns running, so the handover wrote nothing \
                    more to memory"
                   tokens attempts;
             })
    | Agent.Empty_reply { attempts } ->
        append
          (Journal.Error
             {
               Journal.where = "handover";
               what =
                 Printf.sprintf
                   "the model ended %d turns without a reply or a tool call, \
                    so the handover wrote nothing more to memory"
                   attempts;
             })
    | Agent.Malformed_tool_call { message; attempts } ->
        append
          (Journal.Error
             {
               Journal.where = "handover";
               what =
                 Printf.sprintf
                   "the model wrote malformed tool syntax on %d turns, so the \
                    handover stopped: %s"
                   attempts message;
             })
    | Failure msg ->
        append (Journal.Error { Journal.where = "handover"; what = msg }));
    append (Journal.Handover (Memory.version memory))
  in
  let rec session n =
    let brief =
      Brief.assemble ~version:(Memory.version memory)
        ~entries:(Memory.entries memory) ~task ~prompt ~session:n
        ~history:(Brief.since_handover journal_dir)
    in
    append
      (Journal.Brief
         {
           Journal.version = brief.Brief.version;
           open_items = brief.Brief.open_items;
           bytes = brief.Brief.bytes;
         });
    let agent = create ~system:brief.Brief.system in
    job :=
      Some
        {
          Status.task;
          started;
          session = n;
          ctx_used = !ctx_used;
          ctx_size = config.ctx_size;
          turns = !turns;
          tool_calls = !calls;
          tool = None;
          prefilled = 0;
          prefill_total = 0;
        };
    publish !job;
    (* The one live call the control fiber may make. It reads two atomics the
       engine's progress hook writes and does not wait for the engine, which is
       what lets a status answered during a four minute prefill say how far it
       has got. *)
    prefill (fun () -> Driver.prefill_progress agent);
    (* A wake-up that could not do its work still hands over, so the failure
       is carried rather than raised: the memory writes it did make and the
       record of why it stopped are worth more than the exception. *)
    let exhausted =
      match send agent brief.Brief.user with
      | () -> None
      | exception Agent.Context_exhausted { needed; ctx } ->
          Some
            (Printf.sprintf
               "the brief needed %d tokens in a context of %d, so this wake-up \
                did nothing. Memory has grown past what a context holds."
               needed ctx)
      | exception Agent.Tool_call_cut_off { tokens; attempts } ->
          Some
            (Printf.sprintf
               "the model could not write a tool call inside its %d token \
                reply, on %d turns running, so nothing those turns asked for \
                was done. The work wants smaller steps."
               tokens attempts)
      | exception Agent.Empty_reply { attempts } ->
          Some
            (Printf.sprintf
               "the model ended %d turns without a reply or a tool call, so \
                this wake-up stopped. The work wants a smaller step."
               attempts)
      | exception Agent.Malformed_tool_call { message; attempts } ->
          Some
            (Printf.sprintf
               "the model wrote malformed tool syntax on %d turns, so this \
                wake-up stopped: %s"
               attempts message)
      | exception Stop_requested -> None
    in
    (* The turn boundary. Everything decided here is decided once the turn and
       its tool calls are over, since that is the granularity [Agent.send]
       works at. *)
    let fault = fault () in
    let filling =
      float_of_int !ctx_used
      >= config.handover_at *. float_of_int config.max_ctx_size
    in
    (match exhausted with
    | Some what ->
        append (Journal.Error { Journal.where = "brief"; what });
        append (Journal.Handover (Memory.version memory))
    | None -> handover agent);
    Driver.close agent;
    prefill (fun () -> (0, 0));
    match (fault, exhausted) with
    | Some why, _ -> Faulted why
    | None, Some _ -> Finished
    | None, None ->
        (* A stop asked for during the turn ends the wake-up here, the handover
           having been taken above, rather than starting a session nobody is
           waiting for. *)
        if (not filling) || stop () then Finished
        else if n >= config.max_sessions then begin
          append
            (Journal.Error
               {
                 Journal.where = "wake-up";
                 what =
                   Printf.sprintf
                     "the context filled in each of %d sessions on this task, \
                      so it was left for the next wake-up. What it got to is \
                      in memory."
                     n;
               });
          Finished
        end
        else begin
          append
            (Journal.Continued { Journal.task; session = n + 1; previous = n });
          session (n + 1)
        end
  in
  let outcome = session 1 in
  job := None;
  publish None;
  outcome
