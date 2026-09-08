module Api = Openrouter_api

type t = { api : Api.t; runtime : Openapi.Runtime.Client.t }

type error =
  | Http_error of { status : int; code : string option; message : string }
  | Stream_error of { code : string option; message : string }
  | Protocol_error of string
  | Image_too_large of { limit : int }

type Eio.Exn.err += E of error

let fail error = raise (Eio.Exn.create (E error))

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | E (Http_error { status; message; _ }) ->
        Format.fprintf ppf "Openrouter HTTP %d: %s" status message;
        true
    | E (Stream_error { message; _ }) ->
        Format.fprintf ppf "Openrouter stream: %s" message;
        true
    | E (Protocol_error message) ->
        Format.fprintf ppf "Openrouter protocol: %s" message;
        true
    | E (Image_too_large { limit }) ->
        Format.fprintf ppf "Openrouter image exceeds %d bytes" limit;
        true
    | _ -> false)

let object_ fields =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> ((name, Jsont.Meta.none), value))
       fields)

let member name = function
  | Jsont.Object (fields, _) ->
      List.find_map
        (fun ((key, _), value) -> if key = name then Some value else None)
        fields
  | _ -> None

let string = function Jsont.String (value, _) -> Some value | _ -> None
let string_member name json = Option.bind (member name json) string

let error_details json =
  let json = Option.value (member "error" json) ~default:json in
  let message =
    Option.value
      (string_member "message" json)
      ~default:"server returned an error"
  in
  let code =
    Option.bind (member "code" json) (function
      | Jsont.String (code, _) -> Some code
      | Jsont.Number (code, _) -> Some (Printf.sprintf "%.17g" code)
      | _ -> None)
  in
  (code, message)

let with_errors f =
  try f ()
  with Openapi.Runtime.Api_error error ->
    let code, message =
      match Fetch.Media.decode (Fetch.Json.v Jsont.json) error.body with
      | Ok json -> error_details json
      | Error _ -> (None, "server returned a non-JSON error")
    in
    fail (Http_error { status = error.status; code; message })

let of_fetch ?(base_url = "https://openrouter.ai/api/v1") ?api_key ?app_url
    ?app_title ?max_response_bytes fetch =
  let base_url = Api.base_url (Api.of_fetch ~base_url fetch) in
  let scope = [ base_url ] in
  let fetch = Fetch.restrict ~under:scope fetch in
  let fetch =
    match api_key with
    | None -> fetch
    | Some key ->
        Fetch.with_credentials ~scope [ Fetch.Credential.bearer key ] fetch
  in
  let headers =
    List.fold_right
      (fun (name, value) headers ->
        match value with
        | None -> headers
        | Some value ->
            let cell = Fetch.Header.raw name value in
            Fetch.Header.(cell :: headers))
      [ ("HTTP-Referer", app_url); ("X-OpenRouter-Title", app_title) ]
      Fetch.Header.[]
  in
  let fetch = Fetch.with_headers ~scope ~mode:`If_absent headers fetch in
  {
    api = Api.of_fetch ?max_response_bytes ~base_url fetch;
    runtime =
      Openapi.Runtime.Client.of_fetch ?max_response_bytes ~base_url fetch;
  }

module Tool = struct
  type t = Api.ChatFunctionTool.T.t

  let v ~name ?description ?strict ~parameters () =
    if name = "" then invalid_arg "Openrouter.Tool.v: empty name";
    let fields =
      [ ("name", Jsont.Json.string name); ("parameters", parameters) ]
      @ Option.to_list
          (Option.map
             (fun s -> ("description", Jsont.Json.string s))
             description)
      @ Option.to_list
          (Option.map (fun b -> ("strict", Jsont.Json.bool b)) strict)
    in
    let json =
      object_
        [
          ("type", Jsont.Json.string "function");
          ("function", object_ fields);
        ]
    in
    match Jsont.Json.decode Api.ChatFunctionTool.T.jsont json with
    | Ok tool -> tool
    | Error error -> invalid_arg error

  type call = { id : string; name : string; arguments : string }

  let arguments codec call =
    Result.map_error Fetch.Media.error_to_string
      (Fetch.Media.decode (Fetch.Json.v codec) call.arguments)

  let function_jsont =
    Jsont.Object.(
      map (fun name arguments -> (name, arguments))
      |> mem "name" Jsont.string ~enc:fst
      |> mem "arguments" Jsont.string ~enc:snd
      |> finish)

  let call_jsont =
    Jsont.Object.(
      map (fun id (name, arguments) () -> { id; name; arguments })
      |> mem "id" Jsont.string ~enc:(fun c -> c.id)
      |> mem "function" function_jsont ~enc:(fun c -> (c.name, c.arguments))
      |> mem "type" (Jsont.enum [ ("function", ()) ]) ~enc:(fun _ -> ())
      |> finish)

  let to_wire call =
    Api.ChatToolCall.T.v ~id:call.id ~type_:"function"
      ~function_:
        (object_
           [
             ("name", Jsont.Json.string call.name);
             ("arguments", Jsont.Json.string call.arguments);
           ])
      ()
end

module Image = struct
  type format = Png | Jpeg | Webp | Gif
  type detail = Auto | Low | High | Original
  type t = { url : string; detail : detail option }

  let of_url ?detail url =
    (match Fetch.Middleware.Url.of_string url with
    | Ok _ -> ()
    | Error message -> invalid_arg ("Openrouter.Image.of_url: " ^ message));
    { url; detail }

  let check_limit limit =
    if limit <= 0 || limit >= Sys.max_string_length then
      invalid_arg "Openrouter.Image: invalid byte limit"

  let of_string ?(max_bytes = 20 * 1024 * 1024) ?detail ~format bytes =
    check_limit max_bytes;
    if String.length bytes > max_bytes then
      fail (Image_too_large { limit = max_bytes });
    if bytes = "" then invalid_arg "Openrouter.Image: empty image";
    let media =
      match format with
      | Png -> "image/png"
      | Jpeg -> "image/jpeg"
      | Webp -> "image/webp"
      | Gif -> "image/gif"
    in
    {
      url = "data:" ^ media ^ ";base64," ^ Base64.encode_string bytes;
      detail;
    }

  let of_flow ?(max_bytes = 20 * 1024 * 1024) ?detail ~format flow =
    check_limit max_bytes;
    let bytes =
      try
        Eio.Buf_read.take_all
          (Eio.Buf_read.of_flow ~max_size:(max_bytes + 1) flow)
      with Eio.Buf_read.Buffer_limit_exceeded ->
        fail (Image_too_large { limit = max_bytes })
    in
    of_string ~max_bytes ?detail ~format bytes

  let to_wire t =
    let detail =
      Option.to_list
        (Option.map
           (fun detail ->
             ( "detail",
               Jsont.Json.string
                 (match detail with
                 | Auto -> "auto"
                 | Low -> "low"
                 | High -> "high"
                 | Original -> "original") ))
           t.detail)
    in
    object_
      [
        ("type", Jsont.Json.string "image_url");
        ("image_url", object_ (("url", Jsont.Json.string t.url) :: detail));
      ]
end

module Content = struct
  type t = Jsont.json

  let text text =
    object_
      [
        ("type", Jsont.Json.string "text"); ("text", Jsont.Json.string text);
      ]

  let image = Image.to_wire
end

module Message = struct
  type t = Api.ChatMessages.T.t

  let system content =
    Api.ChatMessages.T.SystemMessage
      (Api.ChatSystemMessage.T.v ~role:"system"
         ~content:(Jsont.Json.string content)
         ())

  let developer content =
    Api.ChatMessages.T.DeveloperMessage
      (Api.ChatDeveloperMessage.T.v ~role:"developer"
         ~content:(Jsont.Json.string content)
         ())

  let user content =
    Api.ChatMessages.T.UserMessage
      (Api.ChatUserMessage.T.v ~role:"user"
         ~content:(Jsont.Json.string content)
         ())

  let user_parts parts =
    if parts = [] then
      invalid_arg "Openrouter.Message.user_parts: empty parts";
    Api.ChatMessages.T.UserMessage
      (Api.ChatUserMessage.T.v ~role:"user" ~content:(Jsont.Json.list parts)
         ())

  let assistant ?tool_calls content =
    Api.ChatMessages.T.AssistantMessage
      (Api.ChatAssistantMessage.T.v ~role:"assistant"
         ?tool_calls:(Option.map (List.map Tool.to_wire) tool_calls)
         ~content:(Jsont.Json.string content)
         ())

  let tool_result ~tool_call_id content =
    Api.ChatMessages.T.ToolMessage
      (Api.ChatToolMessage.T.v ~role:"tool" ~tool_call_id
         ~content:(Jsont.Json.string content)
         ())
end

module Models = struct
  type model = {
    id : string;
    name : string option;
    context_length : int option;
  }

  let model_jsont =
    Jsont.Object.(
      map (fun id name context_length max_model_len ->
          {
            id;
            name;
            context_length =
              (match context_length with
              | Some _ -> context_length
              | None -> max_model_len);
          })
      |> mem "id" Jsont.string ~enc:(fun m -> m.id)
      |> opt_mem "name" Jsont.string ~enc:(fun m -> m.name)
      |> mem "context_length"
           (Jsont.option Openapi.Runtime.int_jsont)
           ~dec_absent:(fun () -> None)
           ~enc:(fun m -> m.context_length)
      |> mem "max_model_len"
           (Jsont.option Openapi.Runtime.int_jsont)
           ~dec_absent:(fun () -> None)
           ~enc:(fun _ -> None)
      |> finish)

  let jsont =
    Jsont.Object.(
      map Fun.id
      |> mem "data" (Jsont.list model_jsont) ~enc:Fun.id
      |> finish)

  let list client =
    with_errors @@ fun () ->
    Openapi.Runtime.Client.call ~operation:"list_models" ~path:"/models"
      ~query:""
      ~decode:(fun ~limit response ->
        Fetch.decode ~limit (Fetch.Json.v jsont) response)
      client.runtime `GET
end

module Chat = struct
  type tool_choice = Auto | None_ | Required | Function of string

  type request = {
    model : string;
    messages : Message.t list;
    max_tokens : int option;
    temperature : float option;
    top_p : float option;
    seed : int option;
    stop : string list option;
    tools : Tool.t list option;
    tool_choice : tool_choice option;
    parallel_tool_calls : bool option;
  }

  let request ?max_tokens ?temperature ?top_p ?seed ?stop ?tools
      ?tool_choice ?parallel_tool_calls ~model ~messages () =
    let invalid message =
      invalid_arg ("Openrouter.Chat.request: " ^ message)
    in
    if String.trim model = "" then invalid "empty model";
    if messages = [] then invalid "empty messages";
    Option.iter
      (fun n -> if n <= 0 then invalid "max_tokens must be positive")
      max_tokens;
    List.iter
      (fun (name, upper, value) ->
        Option.iter
          (fun n ->
            if (not (Float.is_finite n)) || n < 0. || n > upper then
              invalid name)
          value)
      [ ("temperature", 2., temperature); ("top_p", 1., top_p) ];
    Option.iter
      (fun values ->
        if List.length values > 4 then invalid "at most four stop sequences")
      stop;
    {
      model;
      messages;
      max_tokens;
      temperature;
      top_p;
      seed;
      stop;
      tools;
      tool_choice;
      parallel_tool_calls;
    }

  let to_wire ~stream request =
    let tool_choice =
      Option.map
        (function
          | Auto -> Jsont.Json.string "auto"
          | None_ -> Jsont.Json.string "none"
          | Required -> Jsont.Json.string "required"
          | Function name ->
              object_
                [
                  ("type", Jsont.Json.string "function");
                  ("function", object_ [ ("name", Jsont.Json.string name) ]);
                ])
        request.tool_choice
    in
    Api.Chat.Request.v ~model:request.model ~messages:request.messages
      ~stream
      ?max_completion_tokens:(Option.map Option.some request.max_tokens)
      ?temperature:(Option.map Option.some request.temperature)
      ?top_p:(Option.map Option.some request.top_p)
      ?seed:(Option.map Option.some request.seed)
      ?stop:
        (Option.map
           (fun xs ->
             Jsont.Json.list (List.map (fun s -> Jsont.Json.string s) xs))
           request.stop)
      ?tools:request.tools ?tool_choice
      ?parallel_tool_calls:
        (Option.map Option.some request.parallel_tool_calls)
      ?stream_options:
        (if stream then
           Some (Some (object_ [ ("include_usage", Jsont.Json.bool true) ]))
         else None)
      ()

  type usage = {
    prompt_tokens : int;
    completion_tokens : int;
    total_tokens : int;
    cost : float option;
  }

  let int_jsont = Openapi.Runtime.int_jsont

  let usage_jsont =
    Jsont.Object.(
      map (fun prompt_tokens completion_tokens total_tokens cost ->
          { prompt_tokens; completion_tokens; total_tokens; cost })
      |> mem "prompt_tokens" int_jsont ~enc:(fun u -> u.prompt_tokens)
      |> mem "completion_tokens" int_jsont ~enc:(fun u ->
          u.completion_tokens)
      |> mem "total_tokens" int_jsont ~enc:(fun u -> u.total_tokens)
      |> mem "cost"
           (Jsont.option Jsont.number)
           ~dec_absent:(fun () -> None)
           ~enc:(fun u -> u.cost)
      |> finish)

  type finish_reason =
    | Stop
    | Length
    | Tool_calls
    | Content_filter
    | Other of string

  let finish_reason = function
    | "stop" -> Stop
    | "length" -> Length
    | "tool_calls" -> Tool_calls
    | "content_filter" -> Content_filter
    | other -> Other other

  let finish_reason_string = function
    | Stop -> "stop"
    | Length -> "length"
    | Tool_calls -> "tool_calls"
    | Content_filter -> "content_filter"
    | Other other -> other

  type choice = {
    index : int;
    text : string option;
    reasoning : string option;
    refusal : string option;
    tool_calls : Tool.call list;
    finish_reason : finish_reason option;
  }

  type completion = {
    id : string;
    model : string;
    created : int;
    choices : choice list;
    usage : usage option;
  }

  let prefer a b = match a with Some _ -> a | None -> b

  let optional name codec enc map =
    Jsont.Object.mem name (Jsont.option codec)
      ~dec_absent:(fun () -> None)
      ~enc map

  let message_jsont =
    Jsont.Object.(
      map (fun text reasoning legacy refusal calls () ->
          ( text,
            prefer reasoning legacy,
            refusal,
            Option.value ~default:[] calls ))
      |> optional "content" Jsont.string (fun (v, _, _, _) -> v)
      |> optional "reasoning" Jsont.string (fun (_, v, _, _) -> v)
      |> optional "reasoning_content" Jsont.string (fun _ -> None)
      |> optional "refusal" Jsont.string (fun (_, _, v, _) -> v)
      |> optional "tool_calls" (Jsont.list Tool.call_jsont)
           (fun (_, _, _, calls) -> Some calls)
      |> mem "role" (Jsont.enum [ ("assistant", ()) ]) ~enc:(fun _ -> ())
      |> finish)

  let choice_jsont =
    Jsont.Object.(
      map (fun index reason (text, reasoning, refusal, tool_calls) ->
          {
            index;
            text;
            reasoning;
            refusal;
            tool_calls;
            finish_reason = Option.map finish_reason reason;
          })
      |> mem "index" int_jsont ~enc:(fun c -> c.index)
      |> mem "finish_reason" (Jsont.option Jsont.string) ~enc:(fun c ->
          Option.map finish_reason_string c.finish_reason)
      |> mem "message" message_jsont ~enc:(fun c ->
          (c.text, c.reasoning, c.refusal, c.tool_calls))
      |> finish)

  let completion_jsont =
    Jsont.Object.(
      map (fun id model created choices usage () ->
          { id; model; created; choices; usage })
      |> mem "id" Jsont.string ~enc:(fun c -> c.id)
      |> mem "model" Jsont.string ~enc:(fun c -> c.model)
      |> mem "created" int_jsont ~enc:(fun c -> c.created)
      |> mem "choices" (Jsont.list choice_jsont) ~enc:(fun c -> c.choices)
      |> optional "usage" usage_jsont (fun c -> c.usage)
      |> mem "object"
           (Jsont.enum [ ("chat.completion", ()) ])
           ~enc:(fun _ -> ())
      |> finish)

  let complete client request =
    with_errors @@ fun () ->
    let headers, body =
      Fetch.encode
        (Fetch.Json.v Api.Chat.Request.jsont)
        (to_wire ~stream:false request)
    in
    Openapi.Runtime.Client.call ~headers ~body ~operation:"chat_complete"
      ~path:"/chat/completions" ~query:""
      ~decode:(fun ~limit response ->
        Fetch.decode ~limit (Fetch.Json.v completion_jsont) response)
      client.runtime `POST

  type event =
    | Started of { id : string; model : string; created : int }
    | Text of { choice : int; text : string }
    | Reasoning of { choice : int; text : string }
    | Refusal of { choice : int; text : string }
    | Tool_call of {
        choice : int;
        index : int;
        id : string option;
        name : string option;
        arguments : string option;
      }
    | Finished of { choice : int; reason : finish_reason }
    | Usage of usage

  module Wire = struct
    type tool = {
      index : int;
      id : string option;
      name : string option;
      arguments : string option;
    }

    type delta = {
      text : string option;
      reasoning : string option;
      refusal : string option;
      tools : tool list;
    }

    type choice = { index : int; delta : delta; finish : string option }

    type chunk = {
      id : string;
      model : string;
      created : int;
      choices : choice list;
      usage : usage option;
    }

    let function_jsont =
      Jsont.Object.(
        map (fun name arguments -> (name, arguments))
        |> optional "name" Jsont.string fst
        |> optional "arguments" Jsont.string snd
        |> finish)

    let tool_jsont =
      Jsont.Object.(
        map (fun index id fn ->
            let name, arguments = Option.value ~default:(None, None) fn in
            { index; id; name; arguments })
        |> mem "index" int_jsont ~enc:(fun (t : tool) -> t.index)
        |> optional "id" Jsont.string (fun (t : tool) -> t.id)
        |> optional "function" function_jsont (fun (t : tool) ->
            Some (t.name, t.arguments))
        |> finish)

    let delta_jsont =
      Jsont.Object.(
        map (fun text reasoning legacy refusal tools ->
            {
              text;
              reasoning = prefer reasoning legacy;
              refusal;
              tools = Option.value ~default:[] tools;
            })
        |> optional "content" Jsont.string (fun d -> d.text)
        |> optional "reasoning" Jsont.string (fun d -> d.reasoning)
        |> optional "reasoning_content" Jsont.string (fun _ -> None)
        |> optional "refusal" Jsont.string (fun d -> d.refusal)
        |> optional "tool_calls" (Jsont.list tool_jsont) (fun d ->
            Some d.tools)
        |> finish)

    let choice_jsont =
      Jsont.Object.(
        map (fun index delta finish -> { index; delta; finish })
        |> mem "index" int_jsont ~enc:(fun (c : choice) -> c.index)
        |> mem "delta" delta_jsont ~enc:(fun c -> c.delta)
        |> optional "finish_reason" Jsont.string (fun c -> c.finish)
        |> finish)

    let chunk_jsont =
      Jsont.Object.(
        map (fun id model created choices usage () ->
            { id; model; created; choices; usage })
        |> mem "id" Jsont.string ~enc:(fun (c : chunk) -> c.id)
        |> mem "model" Jsont.string ~enc:(fun c -> c.model)
        |> mem "created" int_jsont ~enc:(fun c -> c.created)
        |> mem "choices" (Jsont.list choice_jsont) ~enc:(fun c -> c.choices)
        |> optional "usage" usage_jsont (fun c -> c.usage)
        |> mem "object"
             (Jsont.enum [ ("chat.completion.chunk", ()) ])
             ~enc:(fun _ -> ())
        |> finish)
  end

  let stream ?max_event ~on_event client request =
    Option.iter
      (fun n ->
        if n <= 0 then
          invalid_arg "Openrouter.Chat.stream: max_event must be positive")
      max_event;
    with_errors @@ fun () ->
    let started = ref false and done_ = ref false in
    let exception Stop_callback in
    let emit event =
      match on_event event with
      | `Continue -> ()
      | `Stop -> raise Stop_callback
    in
    let on_wire_event (event : Fetch.Sse.event) =
      if String.trim event.data = "[DONE]" then begin
        done_ := true;
        `Stop
      end
      else begin
        let json =
          match Fetch.Media.decode (Fetch.Json.v Jsont.json) event.data with
          | Ok json -> json
          | Error error ->
              fail (Protocol_error (Fetch.Media.error_to_string error))
        in
        (match member "error" json with
        | (None | Some (Jsont.Null _)) when event.name <> "error" -> ()
        | _ ->
            let code, message = error_details json in
            fail (Stream_error { code; message }));
        let chunk =
          match Jsont.Json.decode Wire.chunk_jsont json with
          | Ok chunk -> chunk
          | Error error -> fail (Protocol_error error)
        in
        if not !started then begin
          started := true;
          emit
            (Started
               {
                 id = chunk.id;
                 model = chunk.model;
                 created = chunk.created;
               })
        end;
        List.iter
          (fun (item : Wire.choice) ->
            let choice = item.index and delta = item.delta in
            Option.iter
              (fun text -> emit (Reasoning { choice; text }))
              delta.reasoning;
            Option.iter
              (fun text -> emit (Text { choice; text }))
              delta.text;
            Option.iter
              (fun text -> emit (Refusal { choice; text }))
              delta.refusal;
            List.iter
              (fun (call : Wire.tool) ->
                emit
                  (Tool_call
                     {
                       choice;
                       index = call.index;
                       id = call.id;
                       name = call.name;
                       arguments = call.arguments;
                     }))
              delta.tools;
            Option.iter
              (fun reason ->
                emit (Finished { choice; reason = finish_reason reason }))
              item.finish)
          chunk.choices;
        Option.iter (fun value -> emit (Usage value)) chunk.usage;
        `Continue
      end
    in
    match
      Api.Chat.send_chat_completion_request_stream ?max_event
        ~on_event:on_wire_event
        ~body:(to_wire ~stream:true request)
        client.api ()
    with
    | `Stopped when !done_ -> `Complete
    | `Eof | `Stopped ->
        fail (Protocol_error "chat stream ended before [DONE]")
    | exception Stop_callback -> `Stopped
end
