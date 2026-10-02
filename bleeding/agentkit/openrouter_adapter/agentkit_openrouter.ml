module Common = Agentkit.Agent
module Chat = Agentkit.Chat

module Tool = struct
  type t = Common.Tool.t

  let to_openrouter tool =
    Openrouter.Tool.v ~name:(Common.Tool.name tool)
      ~description:(Common.Tool.description tool)
      ~parameters:(Common.Tool.parameters tool) ()
end

let wire_calls calls =
  List.map
    (fun (c : Common.tool_call) ->
      { Openrouter.Tool.id = c.id; name = c.name; arguments = c.arguments })
    calls

let messages ms =
  List.map
    (function
      | Chat.System s -> Openrouter.Message.system s
      | Chat.User s -> Openrouter.Message.user s
      | Chat.User_images { text; images } ->
          let format = function
            | Chat.Png -> Openrouter.Image.Png
            | Jpeg -> Openrouter.Image.Jpeg
            | Webp -> Openrouter.Image.Webp
            | Gif -> Openrouter.Image.Gif
          in
          Openrouter.Message.user_parts
            (Openrouter.Content.text text
            :: List.map
                 (fun (i : Chat.image) ->
                   Openrouter.Content.image
                     (Openrouter.Image.of_string ~format:(format i.format)
                        i.data))
                 images)
      | Chat.Assistant { text; calls } ->
          Openrouter.Message.assistant ~tool_calls:(wire_calls calls) text
      | Chat.Tool_result { id; content } ->
          Openrouter.Message.tool_result ~tool_call_id:id content)
    ms

let finish = function
  | Openrouter.Chat.Stop -> Chat.Stop
  | Length -> Chat.Length
  | Tool_calls -> Chat.Tool_calls
  | Content_filter -> Chat.Other "content_filter"
  | Other s -> Chat.Other s

let wire_request ~model (r : Chat.request) =
  let tools = List.map Tool.to_openrouter r.tools in
  Openrouter.Chat.request ~model ?max_tokens:r.max_tokens
    ~messages:(messages r.messages)
    ?tools:(if tools = [] then None else Some tools)
    ?parallel_tool_calls:(if tools = [] then None else Some false)
    ?reasoning_effort:r.reasoning ()

let complete client ~model (r : Chat.request) =
  let result = Openrouter.Chat.complete client (wire_request ~model r) in
  match
    List.find_opt
      (fun (c : Openrouter.Chat.choice) -> c.index = 0)
      result.choices
  with
  | None -> failwith "Agentkit_openrouter.complete: no model choice"
  | Some choice ->
      Chat.response
        ~calls:
          (List.map
             (fun (call : Openrouter.Tool.call) ->
               { Common.id = call.id; name = call.name;
                 arguments = call.arguments })
             choice.tool_calls)
        ?finish:(Option.map finish choice.finish_reason)
        choice.text

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

(* A streamed call arrives as fragments sharing an [index]. Only the first
   fragment carries the id and name, so fragments are joined by index. *)
type partial = {
  mutable id : string option;
  mutable name : string;
  arguments : Buffer.t;
}

module Agent = struct
  type t = {
    client : Openrouter.t;
    model : string;
    max_tokens : int option;
    tools : Common.Tool.t list;
    max_rounds : int;
    mutable messages : Chat.message list;
    mutable stats : Common.stats;
    mutable closed : bool;
  }

  let create ~client ~model ?system ?max_tokens ?(tools = [])
      ?(max_rounds = 8) () =
    if String.trim model = "" then
      invalid_arg "Agentkit_openrouter.Agent.create: empty model";
    if max_rounds < 1 then
      invalid_arg "Agentkit_openrouter.Agent.create: max_rounds below 1";
    let messages =
      Option.fold ~none:[] ~some:(fun s -> [ Chat.System s ]) system
    in
    {
      client;
      model;
      max_tokens;
      tools;
      max_rounds;
      messages;
      stats = zero_stats ();
      closed = false;
    }

  let stream t ~on_event ~tools =
    let request =
      wire_request ~model:t.model
        (Chat.request ~tools ?max_tokens:t.max_tokens t.messages)
    in
    let text = Buffer.create 128 in
    let partials = Hashtbl.create 4 in
    let reason = ref None and usage = ref None in
    let emit = function
      | Openrouter.Chat.Text { choice = 0; text = part }
      | Openrouter.Chat.Refusal { choice = 0; text = part } ->
          Buffer.add_string text part;
          on_event (Common.Content part);
          `Continue
      | Openrouter.Chat.Reasoning { choice = 0; text = part } ->
          on_event (Common.Reasoning part);
          `Continue
      | Openrouter.Chat.Tool_call { choice = 0; index; id; name; arguments } ->
          let p =
            match Hashtbl.find_opt partials index with
            | Some p -> p
            | None ->
                let p =
                  { id = None; name = ""; arguments = Buffer.create 64 }
                in
                Hashtbl.replace partials index p;
                p
          in
          if p.id = None then p.id <- id;
          Option.iter (fun n -> p.name <- p.name ^ n) name;
          Option.iter (fun a -> Buffer.add_string p.arguments a) arguments;
          `Continue
      | Openrouter.Chat.Finished { choice = 0; reason = r } ->
          reason := Some r;
          `Continue
      | Openrouter.Chat.Usage u ->
          usage := Some u;
          `Continue
      | _ -> `Continue
    in
    ignore (Openrouter.Chat.stream ~on_event:emit t.client request);
    let calls =
      Hashtbl.fold (fun index p acc -> (index, p) :: acc) partials []
      |> List.sort (fun (a, _) (b, _) -> compare a b)
      |> List.filter_map (fun (index, p) ->
          if p.name = "" then None
          else
            Some
              {
                Common.id =
                  Option.value p.id ~default:("call-" ^ string_of_int index);
                name = p.name;
                arguments = Buffer.contents p.arguments;
              })
    in
    (Buffer.contents text, calls, !reason, !usage)

  let invoke t (call : Common.tool_call) =
    match
      List.find_opt (fun tool -> Common.Tool.name tool = call.name) t.tools
    with
    | Some tool -> Common.Tool.invoke tool call
    | None -> "Error: unknown tool " ^ call.name

  let send t ~on_event prompt =
    if t.closed then invalid_arg "Agentkit_openrouter.Agent.send: closed";
    if String.trim prompt = "" then
      invalid_arg "Agentkit_openrouter.Agent.send: empty prompt";
    t.messages <- t.messages @ [ Chat.User prompt ];
    let generated = ref 0 and prompt_tokens = ref 0 and calls_made = ref 0 in
    let rec round n =
      let tools = if n >= t.max_rounds then [] else t.tools in
      let text, calls, reason, usage = stream t ~on_event ~tools in
      Option.iter
        (fun (u : Openrouter.Chat.usage) ->
          generated := !generated + u.completion_tokens;
          prompt_tokens := u.prompt_tokens)
        usage;
      let cut = reason = Some Openrouter.Chat.Length in
      (* A call cut off mid-arguments is unusable, so it is dropped. *)
      let calls = if cut then [] else calls in
      if cut then
        on_event
          (Common.Cut_off
             {
               tokens = Option.value t.max_tokens ~default:0;
               tool_call = calls <> [];
             });
      t.messages <- t.messages @ [ Chat.Assistant { text; calls } ];
      List.iter
        (fun call ->
          incr calls_made;
          on_event (Common.Tool_call call);
          let result = invoke t call in
          on_event (Common.Tool_result (call.name, result));
          t.messages <-
            t.messages
            @ [ Chat.Tool_result { id = call.id; content = result } ])
        calls;
      if calls <> [] then round (n + 1) else n
    in
    let turns = round 1 in
    t.stats <-
      {
        (zero_stats ()) with
        prompt_tokens = !prompt_tokens;
        generated = !generated;
        total_generated = t.stats.total_generated + !generated;
        turns;
        tool_calls = !calls_made;
      };
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
