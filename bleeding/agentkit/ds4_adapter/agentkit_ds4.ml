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

let transcript messages =
  let messages =
    match messages with Agentkit.Chat.System _ :: rest -> rest | m -> m
  in
  List.map
    (function
      | Agentkit.Chat.System s -> "System: " ^ s
      | User s -> "User: " ^ s
      | User_images { text; images } ->
          Printf.sprintf "User: %s\n[%d image(s) omitted: DS4 reads text only]"
            text (List.length images)
      | Assistant { text; calls } ->
          "Assistant: " ^ text
          ^ String.concat ""
              (List.map
                 (fun (c : Agentkit.Agent.tool_call) ->
                   "\n[called " ^ c.name ^ " " ^ c.arguments ^ "]")
                 calls)
      | Tool_result { id; content } -> "Tool result " ^ id ^ ": " ^ content)
    messages
  |> String.concat "\n\n"

let complete engine ~ctx_size ?max_tokens () (r : Agentkit.Chat.request) =
  let max_tokens =
    match r.max_tokens with Some _ as n -> n | None -> max_tokens
  in
  let agent =
    Ds4.Agent.create ?system:(Agentkit.Chat.system_text r.messages) ~ctx_size
      ?max_tokens ~tools:(tools r.tools) engine
  in
  let output = Buffer.create 256 and cut = ref false in
  Fun.protect
    ~finally:(fun () -> Ds4.Agent.close agent)
    (fun () ->
      Ds4.Agent.send agent (transcript r.messages) ~on_event:(function
        | Ds4.Agent.Content text -> Buffer.add_string output text
        | Ds4.Agent.Cut_off { tool_call = false; _ } -> cut := true
        | _ -> ()));
  Agentkit.Chat.response
    ~finish:(if !cut then Agentkit.Chat.Length else Agentkit.Chat.Stop)
    (if Buffer.length output = 0 then None else Some (Buffer.contents output))

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
