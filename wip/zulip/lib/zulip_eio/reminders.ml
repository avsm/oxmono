let ( let* ) = Result.bind

module Id = Zulip.Id.Reminder

type t = {
  id : Id.t;
  recipients : Zulip.Id.User.t list;
  content : string;
  rendered_content : string;
  scheduled_delivery_timestamp : int;
  failed : bool;
  target_message_id : Zulip.Id.Message.t;
  raw : Jsont.json;
}

let mem name value = ((name, Jsont.Meta.none), value)

let json_encode codec value =
  match Jsont.Json.encode' codec value with
  | Ok json -> json
  | Error error -> raise (Jsont.Error error)

let unknown_without known = function
  | Jsont.Object (members, meta) ->
      Jsont.Object
        ( List.filter (fun ((name, _), _) -> not (List.mem name known)) members,
          meta )
  | _ -> Jsont.Json.object' []

let jsont =
  let make id type_ recipients content rendered_content
      scheduled_delivery_timestamp failed target_message_id unknown =
    if type_ <> "private" then
      Jsont.Error.msgf Jsont.Meta.none "unknown reminder type %S" type_;
    let known =
      [
        mem "reminder_id" (Jsont.Json.int (Id.to_int id));
        mem "type" (Jsont.Json.string type_);
        mem "to"
          (Jsont.Json.list
             (List.map
                (fun id -> Jsont.Json.int (Zulip.Id.User.to_int id))
                recipients));
        mem "content" (Jsont.Json.string content);
        mem "rendered_content" (Jsont.Json.string rendered_content);
        mem "scheduled_delivery_timestamp"
          (Jsont.Json.int scheduled_delivery_timestamp);
        mem "failed" (Jsont.Json.bool failed);
        mem "reminder_target_message_id"
          (Jsont.Json.int (Zulip.Id.Message.to_int target_message_id));
      ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    {
      id;
      recipients;
      content;
      rendered_content;
      scheduled_delivery_timestamp;
      failed;
      target_message_id;
      raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
    }
  in
  Jsont.Object.map ~kind:"Zulip reminder" make
  |> Jsont.Object.mem "reminder_id" Id.jsont ~enc:(fun (r : t) -> r.id)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun _ -> "private")
  |> Jsont.Object.mem "to" (Jsont.list Zulip.Id.User.jsont) ~enc:(fun (r : t) ->
      r.recipients)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun (r : t) -> r.content)
  |> Jsont.Object.mem "rendered_content" Jsont.string ~enc:(fun (r : t) ->
      r.rendered_content)
  |> Jsont.Object.mem "scheduled_delivery_timestamp" Jsont.int
       ~enc:(fun (r : t) -> r.scheduled_delivery_timestamp)
  |> Jsont.Object.mem "failed" Jsont.bool ~enc:(fun (r : t) -> r.failed)
  |> Jsont.Object.mem "reminder_target_message_id" Zulip.Id.Message.jsont
       ~enc:(fun (r : t) -> r.target_message_id)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : t) ->
      unknown_without
        [
          "reminder_id";
          "type";
          "to";
          "content";
          "rendered_content";
          "scheduled_delivery_timestamp";
          "failed";
          "reminder_target_message_id";
        ]
        r.raw)
  |> Jsont.Object.finish

type page = { reminders : t list; raw : Jsont.json }

let page_jsont =
  let make reminders unknown =
    let known =
      [
        mem "reminders"
          (Jsont.Json.list (List.map (json_encode jsont) reminders));
      ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    { reminders; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) }
  in
  Jsont.Object.map ~kind:"Zulip reminders response" make
  |> Jsont.Object.mem "reminders" (Jsont.list jsont) ~enc:(fun (p : page) ->
      p.reminders)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (p : page) ->
      unknown_without [ "reminders" ] p.raw)
  |> Jsont.Object.finish

type create_result = { id : Id.t; raw : Jsont.json }

let create_result_jsont =
  let make id unknown =
    let known = [ mem "reminder_id" (Jsont.Json.int (Id.to_int id)) ] in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    { id; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) }
  in
  Jsont.Object.map ~kind:"Zulip create-reminder response" make
  |> Jsont.Object.mem "reminder_id" Id.jsont ~enc:(fun (r : create_result) ->
      r.id)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : create_result) ->
      unknown_without [ "reminder_id" ] r.raw)
  |> Jsont.Object.finish

let list client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/reminders"
    ~codec:page_jsont ()

let create_detailed client ~message_id ~scheduled_delivery_timestamp ?note () =
  let params =
    [
      ("message_id", string_of_int (Zulip.Id.Message.to_int message_id));
      ( "scheduled_delivery_timestamp",
        string_of_int scheduled_delivery_timestamp );
    ]
    @ Option.fold ~none:[] ~some:(fun value -> [ ("note", value) ]) note
  in
  Client.request_typed client ~method_:`POST ~path:"/api/v1/reminders" ~params
    ~codec:create_result_jsont ()

let create client ~message_id ~scheduled_delivery_timestamp ?note () =
  let* result =
    create_detailed client ~message_id ~scheduled_delivery_timestamp ?note ()
  in
  Ok result.id

let delete client ~reminder_id =
  Client.request client ~method_:`DELETE
    ~path:("/api/v1/reminders/" ^ string_of_int (Id.to_int reminder_id))
    ()
  |> Result.map (Fun.const ())
