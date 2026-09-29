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
        { id = ""; name = call.Dsml.name; arguments = call.Dsml.arguments }
  | Tool_result (name, output) -> Agentkit.Agent.Tool_result (name, output)
  | Stats value -> Agentkit.Agent.Stats (stats value)
  | Expanded tokens -> Agentkit.Agent.Expanded tokens
  | Cut_off cut ->
      Agentkit.Agent.Cut_off
        { tokens = cut.Ds4.Agent.tokens; tool_call = cut.tool_call }
  | Squeezed tokens -> Agentkit.Agent.Squeezed tokens
  | Compacted value -> Agentkit.Agent.Compacted (compaction value)
  | Done -> Agentkit.Agent.Done

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
