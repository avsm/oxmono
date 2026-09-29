(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Common = Agentkit.Agent

type observer = {
  mutable callback : (Common.event -> unit) option;
  mutable failure : exn option;
  mutable calls : int;
}

let report observer event =
  match (observer.failure, observer.callback) with
  | Some _, _ -> false
  | None, None -> true
  | None, Some callback -> (
      try
        callback event;
        true
      with exn ->
        observer.failure <- Some exn;
        false)

module Tool = struct
  type t =
    | Tool : {
        includes_schema_in_instructions : bool;
        description : string;
        codec : 'a Apple_fm.Codec.t;
        handler : 'a -> string;
      }
        -> t

  let v ?(includes_schema_in_instructions = true) ~description codec handler =
    Tool { includes_schema_in_instructions; description; codec; handler }

  let name (Tool tool) = Apple_fm.Codec.name tool.codec

  let compile observer (Tool tool) =
    let handler value =
      let arguments =
        match Apple_fm.Codec.encode_arguments tool.codec value with
        | Ok arguments -> arguments
        | Error message ->
            invalid_arg
              (Printf.sprintf "cannot encode %s tool arguments: %s"
                 (Apple_fm.Codec.name tool.codec)
                 message)
      in
      let name = Apple_fm.Codec.name tool.codec in
      observer.calls <- observer.calls + 1;
      if not (report observer (Common.Tool_call { id = ""; name; arguments })) then
        "Error: the agent event callback failed"
      else
        let output =
          try tool.handler value with
          | (Eio.Cancel.Cancelled _ | Out_of_memory | Stack_overflow) as exn ->
              raise exn
          | exn -> "Error: " ^ Printexc.to_string exn
        in
        ignore (report observer (Common.Tool_result (name, output)));
        output
    in
    Apple_fm.Tool.v
      ~includes_schema_in_instructions:tool.includes_schema_in_instructions
      ~description:tool.description tool.codec handler

  let invoke ?on_event tool arguments =
    let observer = { callback = on_event; failure = None; calls = 0 } in
    let output = Apple_fm.Tool.invoke (compile observer tool) arguments in
    Option.iter raise observer.failure;
    output
end

module Event_sink = struct
  type t = observer

  let single_write observer (buffers @ local) =
    let length = Cstruct.lenv buffers in
    let text = buffers |> Cstruct.globalize_list |> List.map Cstruct.to_string |> String.concat "" in
    (match observer.failure with
    | Some exn -> raise exn
    | None -> (
        match observer.callback with
        | None -> ()
        | Some callback -> callback (Common.Content text)));
    length

  let copy observer ~src = Eio.Flow.Pi.simple_copy ~single_write observer ~src
end

let event_sink_handler = Eio.Flow.Pi.sink (module Event_sink)
let event_sink observer = Eio.Resource.T (observer, event_sink_handler)

let zero_stats ctx_size =
  {
    Common.ctx_used = 0;
    ctx_size;
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

let unsupported = function
  | Eio.Io (Apple_fm.Error.E (`Unsupported_version _), _) -> true
  | _ -> false

module Agent = struct
  type t = {
    sw : Eio.Switch.t;
    model : Apple_fm.Model.t;
    tools : Apple_fm.Tool.t list;
    options : Apple_fm.Generation.options;
    context : Apple_fm.Context.t option;
    compact_at : int;
    compact_tokens : int;
    response_reserve : int;
    context_size : int;
    observer : observer;
    lock : Eio.Mutex.t;
    mutable session : Apple_fm.Session.t;
    mutable stats : Common.stats;
    mutable closed : bool;
  }

  let check_compaction compact_at compact_tokens response_reserve =
    if compact_at <> 0 && (compact_at < 10 || compact_at > 95) then
      invalid_arg "Agent.create: compact_at must be zero or from 10 through 95";
    if compact_tokens < 64 then
      invalid_arg "Agent.create: compact_tokens must be at least 64";
    if response_reserve < 1 then
      invalid_arg "Agent.create: response_reserve must be positive"

  let create_session t ?transcript ?instructions () =
    Apple_fm.Session.create ~sw:t.sw ~model:t.model ?transcript ?instructions
      t.tools

  let create ~sw ?(model = Apple_fm.Model.default) ?instructions ?transcript
      ?(options = Apple_fm.Generation.options ()) ?context ?(compact_at = 0)
      ?compact_tokens ?(response_reserve = 1_024) tools =
    if Option.is_some instructions && Option.is_some transcript then
      invalid_arg
        "Agent.create: instructions and transcript are mutually exclusive";
    let context_size = (Apple_fm.Model.info ~model ()).context_size in
    let compact_tokens =
      Option.value compact_tokens
        ~default:(max 256 (min 1_024 (context_size / 8)))
    in
    check_compaction compact_at compact_tokens response_reserve;
    let observer = { callback = None; failure = None; calls = 0 } in
    let tools = List.map (Tool.compile observer) tools in
    let session =
      Apple_fm.Session.create ~sw ~model ?instructions ?transcript tools
    in
    {
      sw;
      model;
      tools;
      options;
      context;
      compact_at;
      compact_tokens;
      response_reserve;
      context_size;
      observer;
      lock = Eio.Mutex.create ();
      session;
      stats = zero_stats context_size;
      closed = false;
    }

  let ensure_open t operation =
    if t.closed then invalid_arg (operation ^ ": agent is closed")

  let replace_session t transcript =
    let replacement = create_session t ~transcript () in
    let old = t.session in
    t.session <- replacement;
    Apple_fm.Session.close old

  let transcript_tokens t transcript =
    try Apple_fm.Model.count_transcript_tokens ~model:t.model transcript
    with exn when unsupported exn -> 0

  let text_tokens t text =
    try Apple_fm.Model.count_text_tokens ~model:t.model text
    with exn when unsupported exn -> 0

  let compaction_prompt reason =
    Printf.sprintf
      "Internal agent context compaction request. This is not a user request.\n\
       Write a durable task-state summary of the conversation so far. Preserve \
       the user goals, constraints, work completed, important results, known \
       faults, and pending steps. Do not call tools. Return only the summary.\n\n\
       Compaction reason: %s"
      reason

  let compact_locked t ~on_event ~reason =
    ensure_open t "Agent.compact";
    let before = Apple_fm.Session.transcript t.session in
    let before_tokens = transcript_tokens t before in
    let summary_options =
      Apple_fm.Generation.options ~sampling:`Greedy
        ~maximum_response_tokens:t.compact_tokens ~tool_calling:`Disallowed ()
    in
    let fallback_options =
      Apple_fm.Generation.options ~sampling:`Greedy
        ~maximum_response_tokens:t.compact_tokens ()
    in
    let restore () = replace_session t before in
    match
      let summary =
        try
          Apple_fm.Session.respond ~options:summary_options t.session
            (compaction_prompt reason)
        with Eio.Io (Apple_fm.Error.E (`Unsupported_version _), _) ->
          Apple_fm.Session.respond ~options:fallback_options t.session
            (compaction_prompt reason)
      in
      if String.trim summary = "" then failwith "empty compaction summary";
      let rec retain turns =
        let transcript =
          Apple_fm.Model.compact_transcript ~keep_last_turns:turns ~summary
            before
        in
        if turns = 0 then transcript
        else
          let used = transcript_tokens t transcript in
          if used = 0 || used + t.response_reserve + 128 < t.context_size then
            transcript
          else retain (turns - 1)
      in
      let compacted = retain 2 in
      replace_session t compacted;
      let after_tokens = transcript_tokens t compacted in
      on_event
        (Common.Compacted
           { before = before_tokens; after = after_tokens; summary })
    with
    | () -> ()
    | exception exn ->
        Eio.Cancel.protect (fun () -> restore ());
        raise exn

  let should_compact t prompt =
    if t.compact_at = 0 || t.context_size <= 0 then false
    else
      let used = transcript_tokens t (Apple_fm.Session.transcript t.session) in
      let incoming = text_tokens t prompt in
      let limit = t.context_size * t.compact_at / 100 in
      used > 0 && incoming > 0
      && used + incoming + t.response_reserve >= max 1 limit

  let usage t = Apple_fm.Session.usage t.session

  let generated_since (before : Apple_fm.Context.usage option)
      (after : Apple_fm.Context.usage option) =
    match (before, after) with
    | Some before, Some after ->
        max 0
          (after.output_tokens + after.reasoning_tokens - before.output_tokens
         - before.reasoning_tokens)
    | _ -> 0

  let send t ~on_event prompt =
    Eio.Mutex.use_ro t.lock @@ fun () ->
    ensure_open t "Agent.send";
    if should_compact t prompt then
      compact_locked t ~on_event ~reason:"context pressure before the next turn";
    t.observer.callback <- Some on_event;
    t.observer.failure <- None;
    t.observer.calls <- 0;
    Fun.protect ~finally:(fun () -> t.observer.callback <- None) @@ fun () ->
    let before_usage = usage t in
    ignore
      (Apple_fm.Session.respond_stream ~options:t.options ?context:t.context
         ~output:(event_sink t.observer) t.session prompt);
    Option.iter raise t.observer.failure;
    let after_usage = usage t in
    let generated = generated_since before_usage after_usage in
    let ctx_used =
      transcript_tokens t (Apple_fm.Session.transcript t.session)
    in
    let stats =
      {
        Common.ctx_used;
        ctx_size = t.context_size;
        prompt_tokens = 0;
        generated;
        generate_seconds = 0.;
        prefill_seconds = 0.;
        tool_calls = t.observer.calls;
        turns = 0;
        drafted = 0;
        total_generated = t.stats.total_generated + generated;
        total_generate_seconds = 0.;
      }
    in
    t.stats <- stats;
    on_event (Common.Stats stats);
    on_event Common.Done

  let stats t = t.stats
  let cancel t = if not t.closed then Apple_fm.Session.cancel t.session

  let close t =
    Eio.Mutex.use_ro t.lock @@ fun () ->
    if not t.closed then (
      t.closed <- true;
      Apple_fm.Session.close t.session)

  let compact t ~on_event ~reason =
    Eio.Mutex.use_ro t.lock @@ fun () -> compact_locked t ~on_event ~reason

  let transcript t =
    Eio.Mutex.use_ro t.lock @@ fun () ->
    ensure_open t "Agent.transcript";
    Apple_fm.Session.transcript t.session

  let replace_transcript t transcript =
    Eio.Mutex.use_ro t.lock @@ fun () ->
    ensure_open t "Agent.replace_transcript";
    replace_session t transcript
end

let driver
    ?(models =
      fun () ->
        [
          {
            Agentkit.Driver.name = "default";
            description = "Apple system model";
          };
        ]) ~create () =
  Agentkit.Driver.v ~name:"apple" ~models ~create:(fun model ->
      Agentkit.Driver.session (module Agent) (create model))
