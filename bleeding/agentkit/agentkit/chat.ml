type message =
  | System of string
  | User of string
  | Assistant of { text : string; calls : Agent.tool_call list }
  | Tool_result of { id : string; content : string }

type finish = Stop | Length | Tool_calls | Other of string

type request = {
  messages : message list;
  tools : Agent.Tool.t list;
  max_tokens : int option;
  reasoning : string option;
}

let request ?(tools = []) ?max_tokens ?reasoning messages =
  if messages = [] then invalid_arg "Agentkit.Chat.request: empty transcript";
  Option.iter
    (fun n ->
      if n <= 0 then
        invalid_arg "Agentkit.Chat.request: nonpositive max_tokens")
    max_tokens;
  { messages; tools; max_tokens; reasoning }

type response = {
  text : string option;
  calls : Agent.tool_call list;
  finish : finish option;
}

let response ?(calls = []) ?finish text = { text; calls; finish }

type complete = request -> response

let system_text = function System s :: _ -> Some s | _ -> None

let text_of_response r =
  match r.text with Some s when String.trim s <> "" -> Some s | _ -> None
