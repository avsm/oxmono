let ( let* ) = Result.bind

module Id = Zulip.Id.Draft

let target_id_jsont =
  Jsont.map ~kind:"Zulip draft target ID"
    ~dec:(fun number ->
      if
        (not (Float.is_finite number))
        || Float.floor number <> number
        || number < 0.
        || number >= 9_007_199_254_740_992.
      then
        Jsont.Error.msgf Jsont.Meta.none
          "draft target ID must be an exact nonnegative JSON integer";
      int_of_float number)
    ~enc:Float.of_int Jsont.number

type destination =
  | Unaddressed
  | Channel of Zulip.Id.Channel.t
  | Direct of Zulip.Id.User.t list

type new_ = {
  destination : destination;
  topic : string;
  content : string;
  timestamp : int option;
}

type t = {
  id : Id.t;
  destination : destination;
  topic : string;
  content : string;
  timestamp : int option;
  raw : Jsont.json;
}

let mem name value = ((name, Jsont.Meta.none), value)

let json_encode codec value =
  match Jsont.Json.encode' codec value with
  | Ok json -> json
  | Error error -> raise (Jsont.Error error)

let destination_to_wire = function
  | Unaddressed -> ("", [])
  | Channel id -> ("stream", [ Zulip.Id.Channel.to_int id ])
  | Direct ids -> ("private", List.map Zulip.Id.User.to_int ids)

let destination_of_wire type_ to_ =
  match (type_, to_) with
  | "", _ -> Unaddressed
  | "stream", [ id ] -> Channel (Zulip.Id.Channel.of_int id)
  | "stream", _ ->
      Jsont.Error.msgf Jsont.Meta.none
        "a channel draft must have exactly one target channel"
  | "private", ids -> Direct (List.map Zulip.Id.User.of_int ids)
  | value, _ ->
      Jsont.Error.msgf Jsont.Meta.none "unknown Zulip draft type %S" value

let raw type_ to_ topic content timestamp unknown =
  let known =
    [
      mem "type" (Jsont.Json.string type_);
      mem "to" (Jsont.Json.list (List.map Jsont.Json.int to_));
      mem "topic" (Jsont.Json.string topic);
      mem "content" (Jsont.Json.string content);
    ]
    @ Option.fold ~none:[]
        ~some:(fun value -> [ mem "timestamp" (Jsont.Json.int value) ])
        timestamp
  in
  let unknown =
    match unknown with Jsont.Object (members, _) -> members | _ -> []
  in
  Jsont.Object (known @ unknown, Jsont.Meta.none)

let unknown raw =
  let known = [ "id"; "type"; "to"; "topic"; "content"; "timestamp" ] in
  match raw with
  | Jsont.Object (members, meta) ->
      Jsont.Object
        ( List.filter (fun ((name, _), _) -> not (List.mem name known)) members,
          meta )
  | _ -> Jsont.Json.object' []

let jsont =
  let make id type_ to_ topic content timestamp unknown =
    {
      id;
      destination = destination_of_wire type_ to_;
      topic;
      content;
      timestamp;
      raw =
        Jsont.Object
          ( mem "id" (Jsont.Json.int (Id.to_int id))
            ::
            (match raw type_ to_ topic content timestamp unknown with
            | Jsont.Object (members, _) -> members
            | _ -> []),
            Jsont.Meta.none );
    }
  in
  Jsont.Object.map ~kind:"Zulip draft" make
  |> Jsont.Object.mem "id" Id.jsont ~enc:(fun (d : t) -> d.id)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun (d : t) ->
      fst (destination_to_wire d.destination))
  |> Jsont.Object.mem "to" (Jsont.list target_id_jsont) ~enc:(fun (d : t) ->
      snd (destination_to_wire d.destination))
  |> Jsont.Object.mem "topic" Jsont.string ~enc:(fun (d : t) -> d.topic)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun (d : t) -> d.content)
  |> Jsont.Object.opt_mem "timestamp" Jsont.int ~enc:(fun (d : t) ->
      d.timestamp)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (d : t) ->
      unknown d.raw)
  |> Jsont.Object.finish

let new_jsont =
  Jsont.Object.map ~kind:"new Zulip draft"
    (fun type_ to_ topic content timestamp ->
      { destination = destination_of_wire type_ to_; topic; content; timestamp })
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun (d : new_) ->
      fst (destination_to_wire d.destination))
  |> Jsont.Object.mem "to" (Jsont.list target_id_jsont) ~enc:(fun (d : new_) ->
      snd (destination_to_wire d.destination))
  |> Jsont.Object.mem "topic" Jsont.string ~enc:(fun (d : new_) -> d.topic)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun (d : new_) -> d.content)
  |> Jsont.Object.opt_mem "timestamp" Jsont.int ~enc:(fun (d : new_) ->
      d.timestamp)
  |> Jsont.Object.finish

type page = { count : int; drafts : t list; raw : Jsont.json }

let page_jsont =
  let make count drafts unknown =
    let known =
      [
        mem "count" (Jsont.Json.int count);
        mem "drafts" (Jsont.Json.list (List.map (json_encode jsont) drafts));
      ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    { count; drafts; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) }
  in
  Jsont.Object.map ~kind:"Zulip drafts response" make
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun (p : page) -> p.count)
  |> Jsont.Object.mem "drafts" (Jsont.list jsont) ~enc:(fun (p : page) ->
      p.drafts)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (p : page) ->
      match p.raw with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            ( List.filter
                (fun ((name, _), _) -> name <> "count" && name <> "drafts")
                members,
              meta )
      | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

let list client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/drafts"
    ~codec:page_jsont ()

type create_result = { ids : Id.t list; raw : Jsont.json }

let ids_jsont =
  Jsont.Object.map ~kind:"Zulip create-drafts response" (fun ids unknown ->
      let known =
        [
          mem "ids"
            (Jsont.Json.list
               (List.map (fun id -> Jsont.Json.int (Id.to_int id)) ids));
        ]
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { ids; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "ids" (Jsont.list Id.jsont)
       ~enc:(fun (r : create_result) -> r.ids)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : create_result) ->
      match r.raw with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            (List.filter (fun ((name, _), _) -> name <> "ids") members, meta)
      | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

let create_detailed client drafts =
  let* drafts = Codec.encode (Jsont.list new_jsont) drafts in
  Client.request_typed client ~method_:`POST ~path:"/api/v1/drafts"
    ~params:[ ("drafts", drafts) ]
    ~codec:ids_jsont ()

let create client drafts =
  let* result = create_detailed client drafts in
  Ok result.ids

let edit client ~draft_id draft =
  let* draft = Codec.encode new_jsont draft in
  Client.request client ~method_:`PATCH
    ~path:("/api/v1/drafts/" ^ string_of_int (Id.to_int draft_id))
    ~params:[ ("draft", draft) ]
    ()
  |> Result.map (Fun.const ())

let delete client ~draft_id =
  Client.request client ~method_:`DELETE
    ~path:("/api/v1/drafts/" ^ string_of_int (Id.to_int draft_id))
    ()
  |> Result.map (Fun.const ())
