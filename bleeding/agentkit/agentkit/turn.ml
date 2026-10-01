type guard = Agent.tool_call -> (unit, string) result

let unguarded (_ : string) _ = Ok ()

type event =
  | Request of { round : int; budget : int }
  | Budget_exceeded of { calls : int; budget : int }
  | Empty of { error : exn option }
  | Recovered
  | Fallback
  | Cut_off

exception Budget_exceeded

let clip ~bytes text =
  if String.length text <= bytes then text
  else begin
    let last = ref (max 0 bytes) in
    while !last > 0 && Char.code text.[!last] land 0xc0 = 0x80 do
      decr last
    done;
    String.sub text 0 !last ^ "\n[truncated]"
  end

let render _ f = match f () with Ok s -> s | Error e -> "Error: " ^ e

let execute ~guard ~dispatch ~around ~max_result_bytes call =
  around call (fun () ->
      let decision =
        try guard call with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | _ -> Error "Tool call refused."
      in
      match decision with
      | Error _ as e -> e
      | Ok () ->
          Result.map (clip ~bytes:max_result_bytes) (dispatch call))

let bind ~guard ~dispatch ?(around = render) ?(max_result_bytes = 32768) tools
    =
  List.map
    (fun tool ->
      Agent.Tool.with_invoke tool
        (execute ~guard ~dispatch ~around ~max_result_bytes))
    tools

let synthesis =
  "The tool-call allowance for this turn is exhausted. Give the user a concise \
   answer now using the tool results already present. State any missing \
   information or failed searches plainly. Do not request more tools. Return \
   visible answer text even if the task is incomplete."

let retry =
  "The last completion did not produce an answer. Return a visible, concise \
   answer to the user's request using the results already in this \
   conversation. Acknowledge uncertainty or unfinished work. No more tool \
   calls. Do not claim that an action succeeded unless a tool result says so."

let cut_marker = "\n\n[Reply cut off at the token limit.]"

let default_fallback ~tools_used =
  if tools_used then
    "I couldn't turn the tool results into an answer. The tool activity is \
     saved in the log."
  else "I couldn't produce an answer this time. Please try again."

(* Some providers reject a system message after the first, so the instruction
   replaces the opening one. The model also needs it last, where its attention
   is, so it is repeated as a user message marked as coming from the runtime. *)
let instruct instruction messages =
  let notice =
    Chat.User ("Runtime notice, not from the user: " ^ instruction)
  in
  match messages with
  | Chat.System s :: rest ->
      (Chat.System (s ^ "\n\n" ^ instruction) :: rest) @ [ notice ]
  | messages -> messages @ [ notice ]

let run ~complete ~tools ~guard ~dispatch ?(around = render)
    ?(check = fun () -> ()) ?(on_event = fun _ -> ()) ?(budget = 6) ?max_tokens
    ?(max_result_bytes = 32768) ?(max_answer_bytes = 12000)
    ?(fallback = default_fallback) messages =
  if budget < 0 then invalid_arg "Agentkit.Turn.run: negative budget";
  let round = ref 0 and tools_used = ref false in
  let answer = clip ~bytes:max_answer_bytes in
  let finished (r : Chat.response) =
    match Chat.text_of_response r with
    | None -> None
    | Some text when r.finish = Some Chat.Length ->
        on_event Cut_off;
        Some (answer text ^ cut_marker)
    | Some text -> Some (answer text)
  in
  let recover ?error messages =
    on_event (Empty { error });
    check ();
    incr round;
    let result =
      try
        Some
          (complete (Chat.request ?max_tokens (instruct retry messages)))
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> None
    in
    check ();
    match
      Option.bind result (fun (r : Chat.response) ->
          if r.calls = [] then finished r else None)
    with
    | Some text ->
        on_event Recovered;
        text
    | None ->
        on_event Fallback;
        fallback ~tools_used:!tools_used
  in
  let run_call call =
    check ();
    tools_used := true;
    execute ~guard ~dispatch ~around ~max_result_bytes call
  in
  let rec loop budget messages =
    incr round;
    check ();
    on_event (Request { round = !round; budget });
    let request =
      if budget = 0 then Chat.request ?max_tokens (instruct synthesis messages)
      else Chat.request ~tools ?max_tokens messages
    in
    match complete request with
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception exn when budget = 0 -> recover ~error:exn messages
    | (r : Chat.response) -> (
        let n = List.length r.calls in
        if n > budget then begin
          on_event (Budget_exceeded { calls = n; budget });
          List.iter
            (fun call ->
              tools_used := true;
              ignore
                (around call (fun () -> Error "Tool-call budget exceeded.")))
            r.calls;
          raise Budget_exceeded
        end;
        match r.calls with
        | [] -> (
            match finished r with
            | Some text -> text
            | None -> recover messages)
        | calls ->
            let results =
              List.map
                (fun (call : Agent.tool_call) ->
                  Chat.Tool_result { id = call.id; content = run_call call })
                calls
            in
            let text = clip ~bytes:4096 (Option.value ~default:"" r.text) in
            loop (budget - n)
              (messages @ (Chat.Assistant { text; calls } :: results)))
  in
  loop budget messages
