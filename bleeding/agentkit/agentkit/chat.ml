type image_format = Png | Jpeg | Webp | Gif
type image = { format : image_format; data : string }

let image_of_string data =
  let starts p = String.starts_with ~prefix:p data in
  let format =
    if starts "\x89PNG\r\n\x1a\n" then Some Png
    else if starts "\xff\xd8\xff" then Some Jpeg
    else if starts "GIF87a" || starts "GIF89a" then Some Gif
    else if
      String.length data >= 12
      && starts "RIFF"
      && String.sub data 8 4 = "WEBP"
    then Some Webp
    else None
  in
  Option.map (fun format -> { format; data }) format

type message =
  | System of string
  | User of string
  | User_images of { text : string; images : image list }
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

let text_bytes (r : request) =
  let call_bytes (c : Agent.tool_call) =
    String.length c.id + String.length c.name + String.length c.arguments
  in
  let messages = List.fold_left (fun n -> function
      | System s | User s -> n + String.length s
      | User_images { text; _ } -> n + String.length text
      | Assistant { text; calls } ->
          n + String.length text + List.fold_left
            (fun n c -> n + call_bytes c) 0 calls
      | Tool_result { id; content } -> n + String.length id + String.length content)
      0 r.messages in
  List.fold_left (fun n tool ->
      let schema = match Jsont_bytesrw.encode_string Jsont.json
          (Agent.Tool.parameters tool) with
        | Ok schema -> schema
        | Error error -> invalid_arg ("Chat.text_bytes: " ^ error) in
      n + String.length (Agent.Tool.name tool)
        + String.length (Agent.Tool.description tool) + String.length schema)
    messages r.tools
