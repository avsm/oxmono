module Tool = struct
  type t = Agentkit.Agent.Tool.t
  let to_openrouter tool =
    Openrouter.Tool.v ~name:(Agentkit.Agent.Tool.name tool)
      ~description:(Agentkit.Agent.Tool.description tool)
      ~parameters:(Agentkit.Agent.Tool.parameters tool) ()
end

module Common = Agentkit.Agent

let zero_stats () =
  {
    Common.ctx_used = 0;
    ctx_size = 0;
    prompt_tokens = 0;
    generated = 0;
    generate_seconds = 0.;
    prefill_seconds = 0.;
    tool_calls = 0;
    turns = 0;
    drafted = 0;
    total_generated = 0;
    total_generate_seconds = 0.;
  }

module Agent = struct
  type t = {
    client : Openrouter.t;
    model : string;
    max_tokens : int option;
    mutable messages : Openrouter.Message.t list;
    mutable stats : Common.stats;
    mutable closed : bool;
  }

  let create ~client ~model ?system ?max_tokens () =
    if String.trim model = "" then invalid_arg "Agentkit_openrouter.Agent.create: empty model";
    let messages = Option.fold ~none:[] ~some:(fun s -> [ Openrouter.Message.system s ]) system in
    { client; model; max_tokens; messages; stats = zero_stats (); closed = false }

  let send t ~on_event prompt =
    if t.closed then invalid_arg "Agentkit_openrouter.Agent.send: closed";
    if String.trim prompt = "" then invalid_arg "Agentkit_openrouter.Agent.send: empty prompt";
    let user = Openrouter.Message.user prompt in
    let request =
      Openrouter.Chat.request ?max_tokens:t.max_tokens ~model:t.model
        ~messages:(t.messages @ [ user ]) ()
    in
    let text = Buffer.create 128 in
    let reasoning = Buffer.create 64 in
    let tool_calls = Hashtbl.create 4 in
    let usage = ref None in
    let emit event =
      match event with
      | Openrouter.Chat.Started _ -> `Continue
      | Openrouter.Chat.Text { text = part; _ } ->
          Buffer.add_string text part;
          on_event (Common.Content part);
          `Continue
      | Openrouter.Chat.Reasoning { text = part; _ } ->
          Buffer.add_string reasoning part;
          on_event (Common.Reasoning part);
          `Continue
      | Openrouter.Chat.Refusal { text = part; _ } ->
          Buffer.add_string text part;
          on_event (Common.Content part);
          `Continue
      | Openrouter.Chat.Tool_call { index; id; name; arguments; _ } ->
          let id = Option.value id ~default:(string_of_int index) in
          let name = Option.value name ~default:"" in
          let previous =
            match Hashtbl.find_opt tool_calls id with
            | Some (old_name, old_args) -> (if old_name = "" then name else old_name), old_args
            | None -> name, ""
          in
          let args = Option.value arguments ~default:"" in
          Hashtbl.replace tool_calls id (fst previous, snd previous ^ args);
          `Continue
      | Openrouter.Chat.Finished _ -> `Continue
      | Openrouter.Chat.Usage value ->
          usage := Some value;
          `Continue
    in
    ignore (Openrouter.Chat.stream ?max_event:None ~on_event:emit t.client request);
    let reply = Buffer.contents text in
    let calls =
      Hashtbl.fold
        (fun id (name, arguments) acc ->
          if name = "" then acc
          else (({ Common.id = id; name; arguments } : Common.tool_call) :: acc))
        tool_calls []
      |> List.rev
    in
    List.iter (fun call -> on_event (Common.Tool_call call)) calls;
    let assistant = Openrouter.Message.assistant ~tool_calls:(List.map (fun c -> { Openrouter.Tool.id = c.Common.id; name = c.Common.name; arguments = c.arguments }) calls) reply in
    t.messages <- t.messages @ [ user; assistant ];
    let generated, prompt_tokens =
      match !usage with
      | Some value -> value.completion_tokens, value.prompt_tokens
      | None -> (if reply = "" then 0 else String.length reply), 0
    in
    t.stats <-
      { (zero_stats ()) with
        prompt_tokens;
        generated;
        total_generated = t.stats.total_generated + generated;
        turns = 1;
        tool_calls = List.length calls };
    on_event (Common.Stats t.stats);
    on_event Common.Done

  let stats t = t.stats
  let cancel _ = ()
  let close t = t.closed <- true
end

let models client () =
  Openrouter.Models.list client
  |> List.map (fun m ->
      {
        Agentkit.Driver.name = m.Openrouter.Models.id;
        description = Option.value m.name ~default:"OpenRouter model";
      })

let driver ~models ~create () =
  Agentkit.Driver.v ~name:"openrouter" ~models ~create:(fun model ->
      Agentkit.Driver.session (module Agent) (create model))
