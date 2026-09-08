let ( let* ) = Result.bind

module Id = Zulip.Id.Scheduled_message

type destination =
  | Channel of Zulip.Id.Channel.t
  | Direct of Zulip.Id.User.t list

type t = {
  id : Id.t;
  destination : destination;
  topic : string option;
  content : string;
  rendered_content : string;
  scheduled_delivery_timestamp : int;
  failed : bool;
  raw : Jsont.json;
}

let mem name value = ((name, Jsont.Meta.none), value)

let json_encode codec value =
  match Jsont.Json.encode' codec value with
  | Ok json -> json
  | Error error -> raise (Jsont.Error error)

let destination_to_wire = function
  | Channel id -> ("channel", string_of_int (Zulip.Id.Channel.to_int id))
  | Direct ids -> (
      match
        Jsont_bytesrw.encode_string' (Jsont.list Zulip.Id.User.jsont) ids
      with
      | Ok value -> ("direct", value)
      | Error error -> raise (Jsont.Error error))

let destination_json = function
  | Channel id -> Jsont.Json.int (Zulip.Id.Channel.to_int id)
  | Direct ids ->
      Jsont.Json.list
        (List.map (fun id -> Jsont.Json.int (Zulip.Id.User.to_int id)) ids)

let destination_of_json type_ json =
  match (type_, json) with
  | ("stream" | "channel"), Jsont.Number (number, meta) ->
      if
        (not (Float.is_finite number))
        || Float.floor number <> number
        || number < 0.
        || number >= 9_007_199_254_740_992.
      then
        Jsont.Error.msgf meta
          "scheduled-message channel ID must be an exact nonnegative JSON \
           integer"
      else Channel (Zulip.Id.Channel.of_int (int_of_float number))
  | ("private" | "direct"), Jsont.Array (values, _) ->
      let users =
        List.map
          (fun json ->
            match Jsont.Json.decode' Zulip.Id.User.jsont json with
            | Ok id -> id
            | Error error -> raise (Jsont.Error error))
          values
      in
      Direct users
  | ("stream" | "channel"), json ->
      Jsont.Json.error_sort ~exp:Jsont.Sort.Number json
  | ("private" | "direct"), json ->
      Jsont.Json.error_sort ~exp:Jsont.Sort.Array json
  | value, _ ->
      Jsont.Error.msgf Jsont.Meta.none "unknown scheduled message type %S" value

let unknown_without known = function
  | Jsont.Object (members, meta) ->
      Jsont.Object
        ( List.filter (fun ((name, _), _) -> not (List.mem name known)) members,
          meta )
  | _ -> Jsont.Json.object' []

let jsont =
  let make id type_ to_ topic content rendered_content
      scheduled_delivery_timestamp failed unknown =
    let destination = destination_of_json type_ to_ in
    let known =
      [
        mem "scheduled_message_id" (Jsont.Json.int (Id.to_int id));
        mem "type" (Jsont.Json.string type_);
        mem "to" to_;
        mem "content" (Jsont.Json.string content);
        mem "rendered_content" (Jsont.Json.string rendered_content);
        mem "scheduled_delivery_timestamp"
          (Jsont.Json.int scheduled_delivery_timestamp);
        mem "failed" (Jsont.Json.bool failed);
      ]
      @ Option.fold ~none:[]
          ~some:(fun value -> [ mem "topic" (Jsont.Json.string value) ])
          topic
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    {
      id;
      destination;
      topic;
      content;
      rendered_content;
      scheduled_delivery_timestamp;
      failed;
      raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
    }
  in
  Jsont.Object.map ~kind:"Zulip scheduled message" make
  |> Jsont.Object.mem "scheduled_message_id" Id.jsont ~enc:(fun (m : t) -> m.id)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun (m : t) ->
      match m.destination with Channel _ -> "stream" | Direct _ -> "private")
  |> Jsont.Object.mem "to" Jsont.json ~enc:(fun (m : t) ->
      destination_json m.destination)
  |> Jsont.Object.opt_mem "topic" Jsont.string ~enc:(fun (m : t) -> m.topic)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun (m : t) -> m.content)
  |> Jsont.Object.mem "rendered_content" Jsont.string ~enc:(fun (m : t) ->
      m.rendered_content)
  |> Jsont.Object.mem "scheduled_delivery_timestamp" Jsont.int
       ~enc:(fun (m : t) -> m.scheduled_delivery_timestamp)
  |> Jsont.Object.mem "failed" Jsont.bool ~enc:(fun (m : t) -> m.failed)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (m : t) ->
      unknown_without
        [
          "scheduled_message_id";
          "type";
          "to";
          "topic";
          "content";
          "rendered_content";
          "scheduled_delivery_timestamp";
          "failed";
        ]
        m.raw)
  |> Jsont.Object.finish

type page = { scheduled_messages : t list; raw : Jsont.json }

let page_jsont =
  let make scheduled_messages unknown =
    let known =
      [
        mem "scheduled_messages"
          (Jsont.Json.list (List.map (json_encode jsont) scheduled_messages));
      ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    {
      scheduled_messages;
      raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
    }
  in
  Jsont.Object.map ~kind:"Zulip scheduled-messages response" make
  |> Jsont.Object.mem "scheduled_messages" (Jsont.list jsont)
       ~enc:(fun (p : page) -> p.scheduled_messages)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (p : page) ->
      unknown_without [ "scheduled_messages" ] p.raw)
  |> Jsont.Object.finish

type create_result = { id : Id.t; raw : Jsont.json }

let create_result_jsont =
  let make id unknown =
    let known =
      [ mem "scheduled_message_id" (Jsont.Json.int (Id.to_int id)) ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    { id; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) }
  in
  Jsont.Object.map ~kind:"Zulip create-scheduled-message response" make
  |> Jsont.Object.mem "scheduled_message_id" Id.jsont
       ~enc:(fun (r : create_result) -> r.id)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : create_result) ->
      unknown_without [ "scheduled_message_id" ] r.raw)
  |> Jsont.Object.finish

type update = {
  destination : destination option;
  topic : string option;
  content : string option;
  scheduled_delivery_timestamp : int option;
}

let destination_params destination =
  let type_, to_ = destination_to_wire destination in
  [ ("type", type_); ("to", to_) ]

let create_detailed client ~destination ?topic ?(read_by_sender = true) ~content
    ~scheduled_delivery_timestamp () =
  match (destination, topic) with
  | Channel _, None ->
      Error (Error.Invalid_request "scheduled channel messages require a topic")
  | Direct _, Some _ ->
      Error
        (Error.Invalid_request "scheduled direct messages cannot have a topic")
  | _ ->
      let params =
        destination_params destination
        @ [
            ("content", content);
            ( "scheduled_delivery_timestamp",
              string_of_int scheduled_delivery_timestamp );
            ("read_by_sender", string_of_bool read_by_sender);
          ]
        @ Option.fold ~none:[] ~some:(fun value -> [ ("topic", value) ]) topic
      in
      Client.request_typed client ~method_:`POST
        ~path:"/api/v1/scheduled_messages" ~params ~codec:create_result_jsont ()

let create client ~destination ?topic ?read_by_sender ~content
    ~scheduled_delivery_timestamp () =
  let* result =
    create_detailed client ~destination ?topic ?read_by_sender ~content
      ~scheduled_delivery_timestamp ()
  in
  Ok result.id

let list client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/scheduled_messages"
    ~codec:page_jsont ()

let update client ~scheduled_message_id update =
  if
    match (update.destination, update.topic) with
    | Some (Direct _), Some _ -> true
    | _ -> false
  then
    Error
      (Error.Invalid_request "scheduled direct messages cannot have a topic")
  else
    let params =
      Option.fold ~none:[] ~some:destination_params update.destination
      @ Option.fold ~none:[]
          ~some:(fun value -> [ ("topic", value) ])
          update.topic
      @ Option.fold ~none:[]
          ~some:(fun value -> [ ("content", value) ])
          update.content
      @ Option.fold ~none:[]
          ~some:(fun value ->
            [ ("scheduled_delivery_timestamp", string_of_int value) ])
          update.scheduled_delivery_timestamp
    in
    if params = [] then
      Error (Error.Invalid_request "scheduled-message update has no changes")
    else
      Client.request client ~method_:`PATCH
        ~path:
          ("/api/v1/scheduled_messages/"
          ^ string_of_int (Id.to_int scheduled_message_id))
        ~params ()
      |> Result.map (Fun.const ())

let delete client ~scheduled_message_id =
  Client.request client ~method_:`DELETE
    ~path:
      ("/api/v1/scheduled_messages/"
      ^ string_of_int (Id.to_int scheduled_message_id))
    ()
  |> Result.map (Fun.const ())
