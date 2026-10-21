(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let stats (stats : Ds4.Agent.stats) : Agentkit.Agent.stats =
  {
    ctx_used = stats.ctx_used;
    ctx_size = stats.ctx_size;
    prompt_tokens = stats.prompt_tokens;
    generated = stats.generated;
    generate_seconds = stats.generate_seconds;
    prefill_seconds = stats.prefill_seconds;
    tool_calls = stats.tool_calls;
    turns = stats.turns;
    drafted = stats.drafted;
    total_generated = stats.total_generated;
    total_generate_seconds = stats.total_generate_seconds;
  }

let compaction (compaction : Ds4.Agent.compaction) : Agentkit.Agent.compaction =
  {
    before = compaction.before;
    after = compaction.after;
    summary = compaction.summary;
  }

let event : Ds4.Agent.event -> Agentkit.Agent.event = function
  | Reasoning text -> Agentkit.Agent.Reasoning text
  | Content text -> Agentkit.Agent.Content text
  | Tool_call call ->
      Agentkit.Agent.Tool_call
        { id = Option.value call.Dsml.id ~default:""; name = call.name; arguments = call.arguments }
  | Tool_result (name, output) -> Agentkit.Agent.Tool_result (name, output)
  | Stats value -> Agentkit.Agent.Stats (stats value)
  | Expanded tokens -> Agentkit.Agent.Expanded tokens
  | Cut_off cut ->
      Agentkit.Agent.Cut_off
        { tokens = cut.Ds4.Agent.tokens; tool_call = cut.tool_call }
  | Squeezed tokens -> Agentkit.Agent.Squeezed tokens
  | Compacted value -> Agentkit.Agent.Compacted (compaction value)
  | Done -> Agentkit.Agent.Done

let tools generic =
  List.map
    (fun tool ->
      Ds4.Tool.raw ~name:(Agentkit.Agent.Tool.name tool)
        ~description:(Agentkit.Agent.Tool.description tool)
        ~schema:(Agentkit.Agent.Tool.parameters tool)
        (fun (call : Dsml.tool_call) ->
          Agentkit.Agent.Tool.invoke tool
            {
              Agentkit.Agent.id = Option.value ~default:"" call.id;
              name = call.name;
              arguments = call.arguments;
            }))
    generic

let messages ~dialect ~tools history =
  let neutralise = Dsml.neutralise_specials ~dialect in
  let tools =
    List.map
      (fun tool ->
        Dsml.Tool.v ~name:(Agentkit.Agent.Tool.name tool)
          ~description:(Agentkit.Agent.Tool.description tool)
          ~parameters:(Agentkit.Agent.Tool.parameters tool) ())
      tools
  in
  let system, history =
    match history with
    | Agentkit.Chat.System text :: rest -> (text, rest)
    | history -> ("", history)
  in
  Dsml.system ~tools (neutralise system)
  :: List.map
       (function
         | Agentkit.Chat.System text -> Dsml.system (neutralise text)
         | User text -> Dsml.user (neutralise text)
         | User_images { text; images = [] } -> Dsml.user (neutralise text)
         | User_images _ ->
             invalid_arg "Agentkit_ds4.complete: image messages are not supported"
         | Assistant { text; calls } ->
             let tool_calls =
               List.map
                 (fun (call : Agentkit.Agent.tool_call) ->
                   Dsml.tool_call ~id:call.id ~name:call.name
                     ~arguments:(neutralise call.arguments) ())
                 calls
             in
             Dsml.assistant ~content:(neutralise text) ~tool_calls ()
         | Tool_result { id; content } -> Dsml.tool ~id (neutralise content))
       history

let transcript history =
  Dsml.encode_messages Dsml.Chat
    (messages ~dialect:Dsml.Deepseek ~tools:[] history)

let reasoning = function
  | None | Some "none" -> (Dsml.Chat, None)
  | Some "high" -> (Dsml.Thinking, Some Dsml.High)
  | Some "max" -> (Dsml.Thinking, Some Dsml.Max)
  | Some _ ->
      invalid_arg "Agentkit_ds4.complete: reasoning must be none, high or max"

let complete engine ~ctx_size ?max_tokens () =
  let next_id = Atomic.make 0 in
  fun (r : Agentkit.Chat.request) ->
  let max_tokens =
    match r.max_tokens with Some _ as n -> n | None -> max_tokens
  in
  let thinking, reasoning_effort = reasoning r.reasoning in
  let history = messages ~dialect:(Ds4.Completion.dialect engine) ~tools:r.tools r.messages in
  let response =
    Ds4.Completion.complete engine ~ctx_size ?max_tokens ~thinking
      ?reasoning_effort history
  in
  let calls =
    List.map
      (fun (call : Dsml.tool_call) ->
        { Agentkit.Agent.id = "ds4-call-" ^ string_of_int (Atomic.fetch_and_add next_id 1);
          name = call.name; arguments = call.arguments })
      response.message.tool_calls
  in
  let finish = match response.finish with
    | Ds4.Completion.Stop -> Agentkit.Chat.Stop
    | Length -> Agentkit.Chat.Length
    | Tool_calls -> Agentkit.Chat.Tool_calls
  in
  Agentkit.Chat.response ~calls ~finish
    (if response.message.content = "" then None else Some response.message.content)

module Agent = struct
  type t = Ds4.Agent.t

  let send agent ~on_event prompt =
    Ds4.Agent.send agent prompt ~on_event:(fun value -> on_event (event value))

  let stats agent = stats (Ds4.Agent.stats agent)
  let cancel = Ds4.Agent.cancel
  let close = Ds4.Agent.close
end

let models ~dir () =
  {
    Agentkit.Driver.name = "auto";
    description = "Preferred downloaded DS4 model";
  }
  :: (List.map
        (fun (model : Ds4_cli.Model.t) ->
          {
            Agentkit.Driver.name = model.name;
            description =
              (model.descr
              ^
              if Ds4_cli.Model.present ~dir model then " [downloaded]"
              else " [download required]");
          })
        Ds4_cli.Model.all
     @ List.map
         (fun name ->
           {
             Agentkit.Driver.name = "local/" ^ name;
             description = "Local GGUF file [downloaded]";
           })
         (Ds4_cli.Model.others ~dir))

let model_path ~dir name =
  if String.starts_with ~prefix:"local/" name then
    let file = String.sub name 6 (String.length name - 6) in
    if List.mem file (Ds4_cli.Model.others ~dir) then Filename.concat dir file
    else failwith ("ds4/local/" ^ file ^ " is not a local GGUF file")
  else
    Ds4_cli.Cli.resolve_model ~dir (if name = "auto" then None else Some name)

let driver ~models ~create =
  Agentkit.Driver.v ~name:"ds4" ~models ~create:(fun model ->
      let agent = create model in
      Agentkit.Driver.session
        ~prefill_progress:(fun () -> Ds4.Agent.prefill_progress agent)
        (module Agent)
        agent)
