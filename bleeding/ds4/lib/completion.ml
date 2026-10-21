type finish = Stop | Length | Tool_calls

type t = { message : Dsml.parsed_message; finish : finish }

let dialect engine =
  match V4.family engine with
  | `Deepseek -> Dsml.Deepseek
  | `Deepseek41 -> Dsml.Deepseek41
  | `Glm -> Dsml.Glm
  | `Qwen -> Dsml.Qwen

let complete engine ~ctx_size ?(max_tokens = 2048) ?(thinking = Dsml.Chat)
    ?reasoning_effort messages =
  if max_tokens < 1 then invalid_arg "Completion.complete: nonpositive max_tokens";
  if messages = [] then invalid_arg "Completion.complete: empty transcript";
  let dialect = dialect engine in
  let reasoning_effort =
    match reasoning_effort with
    | Some Dsml.Max when ctx_size < 393216 -> Some Dsml.High
    | effort -> effort
  in
  let tokens =
    Dsml.encode_messages ~dialect ?reasoning_effort thinking messages
    |> V4.Session.tokenize engine
  in
  if Array.length tokens >= ctx_size then
    raise (Agent.Context_exhausted { needed = Array.length tokens; ctx = ctx_size });
  let session = V4.Session.create engine ~ctx_size ~seed:0x2545F4914F6CDD1DL in
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> V4.Session.close session))
    (fun () ->
      V4.Session.sync session tokens;
      let budget = min max_tokens (V4.Session.ctx session - V4.Session.pos session - 1) in
      let decoder = Dsml.Stream.create ~dialect thinking in
      let content = Buffer.create 256 and reasoning = Buffer.create 256 in
      let calls = ref [] in
      let consume = function
        | Dsml.Stream.Content text -> Buffer.add_string content text
        | Dsml.Stream.Reasoning text -> Buffer.add_string reasoning text
        | Dsml.Stream.Tool_call call -> calls := call :: !calls
        | Dsml.Stream.Tool_error { message; _ } -> raise (Dsml.Parse_error message)
        | Dsml.Stream.Done -> ()
      in
      let rec loop n =
        Eio.Fiber.check ();
        if !calls <> [] then Tool_calls
        else if n >= budget then Length
        else
          let temperature, top_p, min_p =
            match Dsml.Stream.sampling_mode decoder with
            | `Greedy -> (0., 1., 0.)
            | `Configured when dialect = Dsml.Glm -> (1., 0.95, 0.)
            | `Configured -> (1., 1., 0.05)
          in
          let token = V4.Session.sample session ~temperature ~top_p ~min_p in
          if V4.token_is_stop engine token then Stop
          else begin
            V4.Session.eval session token;
            Dsml.Stream.feed decoder (V4.token_text engine token) |> List.iter consume;
            loop (n + 1)
          end
      in
      let finish = loop 0 in
      (* A partial call is not an executable response, even if its prefix
         happens to contain valid JSON. *)
      if finish <> Length || not (Dsml.Stream.in_tool_call decoder) then
        List.iter consume (Dsml.Stream.finish decoder);
      {
        message = {
          Dsml.content = Buffer.contents content;
          reasoning_content = Buffer.contents reasoning;
          tool_calls = if finish = Length then [] else List.rev !calls;
        };
        finish;
      })
