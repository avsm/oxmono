let ( let* ) = Result.bind
let unit_result = Result.map (Fun.const ())

let storage_jsont =
  let dec = function
    | Jsont.Object (members, _) ->
        let seen = Hashtbl.create (List.length members) in
        List.map
          (fun ((name, meta), value) ->
            if Hashtbl.mem seen name then
              Jsont.Error.msgf meta "duplicate bot-storage key %S" name;
            Hashtbl.add seen name ();
            match value with
            | Jsont.String (value, _) -> (name, value)
            | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.String json)
          members
    | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json
  in
  let enc entries =
    let seen = Hashtbl.create (List.length entries) in
    let member (name, value) =
      if Hashtbl.mem seen name then
        Jsont.Error.msgf Jsont.Meta.none "duplicate bot-storage key %S" name;
      Hashtbl.add seen name ();
      ((name, Jsont.Meta.none), Jsont.String (value, Jsont.Meta.none))
    in
    Jsont.Object (List.map member entries, Jsont.Meta.none)
  in
  Jsont.map ~kind:"Zulip bot storage" ~dec ~enc Jsont.json

let response_jsont =
  Jsont.Object.map ~kind:"Zulip bot storage response" Fun.id
  |> Jsont.Object.mem "storage" storage_jsont ~enc:Fun.id
  |> Jsont.Object.finish

let keys_param = function
  | None -> Ok []
  | Some keys ->
      let* encoded = Codec.encode (Jsont.list Jsont.string) keys in
      Ok [ ("keys", encoded) ]

let get client ?keys () =
  let* params = keys_param keys in
  Client.request_typed client ~method_:`GET ~path:"/api/v1/bot_storage" ~params
    ~codec:response_jsont ()

let set client entries =
  let* storage = Codec.encode storage_jsont entries in
  Client.request client ~method_:`PUT ~path:"/api/v1/bot_storage"
    ~params:[ ("storage", storage) ]
    ()
  |> unit_result

let remove client ?keys () =
  let* params = keys_param keys in
  Client.request client ~method_:`DELETE ~path:"/api/v1/bot_storage" ~params ()
  |> unit_result
