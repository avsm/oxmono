type status = Active | Idle | Offline | Other of string

let status_to_string = function
  | Active -> "active"
  | Idle -> "idle"
  | Offline -> "offline"
  | Other value -> value

let status_of_string = function
  | "active" -> Active
  | "idle" -> Idle
  | "offline" -> Offline
  | value -> Other value

let status_jsont =
  Jsont.map ~kind:"Zulip presence status" ~dec:status_of_string
    ~enc:status_to_string Jsont.string

type client_presence = {
  client : string;
  status : status;
  timestamp : float;
  pushable : bool;
}

type user_presence = {
  active_timestamp : float option;
  idle_timestamp : float option;
  clients : client_presence list;
}

type presence_map = (string * user_presence) list

type user_response = {
  server_timestamp : float option;
  presence : user_presence;
  extensions : Jsont.json;
}

type realm_response = {
  server_timestamp : float;
  presences : presence_map;
  extensions : Jsont.json;
}

type update_response = {
  presence_last_update_id : int option;
  server_timestamp : float option;
  presences : presence_map option;
  extensions : Jsont.json;
}

type wire_presence = { status : status; timestamp : float; pushable : bool }

let wire_presence_jsont =
  Jsont.Object.map ~kind:"Zulip client presence"
    (fun status timestamp pushable -> { status; timestamp; pushable })
  |> Jsont.Object.mem "status" status_jsont
       ~enc:(fun (presence : wire_presence) -> presence.status)
  |> Jsont.Object.mem "timestamp" Jsont.number
       ~enc:(fun (presence : wire_presence) -> presence.timestamp)
  |> Jsont.Object.mem "pushable" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (presence : wire_presence) -> presence.pushable)
  |> Jsont.Object.finish

let client_presence_jsont =
  Jsont.Object.map ~kind:"Zulip named client presence"
    (fun client status timestamp pushable ->
      ({ client; status; timestamp; pushable } : client_presence))
  |> Jsont.Object.mem "client" Jsont.string
       ~enc:(fun (presence : client_presence) -> presence.client)
  |> Jsont.Object.mem "status" status_jsont
       ~enc:(fun (presence : client_presence) -> presence.status)
  |> Jsont.Object.mem "timestamp" Jsont.number
       ~enc:(fun (presence : client_presence) -> presence.timestamp)
  |> Jsont.Object.mem "pushable" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (presence : client_presence) -> presence.pushable)
  |> Jsont.Object.finish

let user_presence_jsont =
  let dec = function
    | Jsont.Object (members, _) ->
        let timestamp name =
          match Jsont.Json.find_mem name members with
          | None -> None
          | Some (_, value) -> (
              match Jsont.Json.decode' Jsont.number value with
              | Ok value -> Some value
              | Error error -> raise (Jsont.Error error))
        in
        let clients =
          List.filter_map
            (fun ((client, _), value) ->
              if client = "active_timestamp" || client = "idle_timestamp" then
                None
              else
                match Jsont.Json.decode' wire_presence_jsont value with
                | Ok presence ->
                    Some
                      {
                        client;
                        status = presence.status;
                        timestamp = presence.timestamp;
                        pushable = presence.pushable;
                      }
                | Error error -> raise (Jsont.Error error))
            members
        in
        {
          active_timestamp = timestamp "active_timestamp";
          idle_timestamp = timestamp "idle_timestamp";
          clients;
        }
    | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json
  in
  let enc presence =
    let member (client : client_presence) =
      let wire =
        {
          status = client.status;
          timestamp = client.timestamp;
          pushable = client.pushable;
        }
      in
      match Jsont.Json.encode' wire_presence_jsont wire with
      | Ok value -> ((client.client, Jsont.Meta.none), value)
      | Error error -> raise (Jsont.Error error)
    in
    let timestamp name = function
      | None -> []
      | Some value ->
          [ ((name, Jsont.Meta.none), Jsont.Number (value, Jsont.Meta.none)) ]
    in
    Jsont.Object
      ( timestamp "active_timestamp" presence.active_timestamp
        @ timestamp "idle_timestamp" presence.idle_timestamp
        @ List.map member presence.clients,
        Jsont.Meta.none )
  in
  Jsont.map ~kind:"Zulip user presence" ~dec ~enc Jsont.json

let presence_map_jsont =
  let dec = function
    | Jsont.Object (members, _) ->
        List.map
          (fun ((key, _), json) ->
            match Jsont.Json.decode' user_presence_jsont json with
            | Ok presence -> (key, presence)
            | Error error -> raise (Jsont.Error error))
          members
    | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json
  in
  let enc presences =
    let member (key, presence) =
      match Jsont.Json.encode' user_presence_jsont presence with
      | Ok value -> ((key, Jsont.Meta.none), value)
      | Error error -> raise (Jsont.Error error)
    in
    Jsont.Object (List.map member presences, Jsont.Meta.none)
  in
  Jsont.map ~kind:"Zulip presence map" ~dec ~enc Jsont.json

let user_response_jsont =
  Jsont.Object.map ~kind:"Zulip user presence response"
    (fun server_timestamp presence extensions ->
      { server_timestamp; presence; extensions })
  |> Jsont.Object.opt_mem "server_timestamp" Jsont.number
       ~enc:(fun (response : user_response) -> response.server_timestamp)
  |> Jsont.Object.mem "presence" user_presence_jsont
       ~enc:(fun (response : user_response) -> response.presence)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (response : user_response) -> response.extensions)
  |> Jsont.Object.finish

let realm_response_jsont =
  Jsont.Object.map ~kind:"Zulip organization presence response"
    (fun server_timestamp presences extensions ->
      { server_timestamp; presences; extensions })
  |> Jsont.Object.mem "server_timestamp" Jsont.number
       ~enc:(fun (response : realm_response) -> response.server_timestamp)
  |> Jsont.Object.mem "presences" presence_map_jsont
       ~enc:(fun (response : realm_response) -> response.presences)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (response : realm_response) -> response.extensions)
  |> Jsont.Object.finish

let update_response_jsont =
  Jsont.Object.map ~kind:"Zulip update-presence response"
    (fun presence_last_update_id server_timestamp presences extensions ->
      { presence_last_update_id; server_timestamp; presences; extensions })
  |> Jsont.Object.opt_mem "presence_last_update_id" Jsont.int
       ~enc:(fun (response : update_response) ->
         response.presence_last_update_id)
  |> Jsont.Object.opt_mem "server_timestamp" Jsont.number
       ~enc:(fun (response : update_response) -> response.server_timestamp)
  |> Jsont.Object.opt_mem "presences" presence_map_jsont
       ~enc:(fun (response : update_response) -> response.presences)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (response : update_response) -> response.extensions)
  |> Jsont.Object.finish

let get_user_detailed client ~user_id =
  Client.request_typed client ~method_:`GET
    ~path:
      ("/api/v1/users/"
      ^ string_of_int (Zulip.Id.User.to_int user_id)
      ^ "/presence")
    ~codec:user_response_jsont ()

let get_user client ~user_id =
  Result.map
    (fun response -> response.presence)
    (get_user_detailed client ~user_id)

let get_user_by_email_detailed client ~email =
  Client.request_typed client ~method_:`GET
    ~path:("/api/v1/users/" ^ Client.path_segment email ^ "/presence")
    ~codec:user_response_jsont ()

let get_user_by_email client ~email =
  Result.map
    (fun response -> response.presence)
    (get_user_by_email_detailed client ~email)

let get_all_detailed client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/realm/presence"
    ~codec:realm_response_jsont ()

let get_all client =
  Result.map
    (fun (response : realm_response) -> response.presences)
    (get_all_detailed client)

let update client ~status ?last_update_id ?history_limit_days ?ping_only
    ?new_user_input ?slim_presence () =
  let status =
    match status with
    | Active -> Ok "active"
    | Idle -> Ok "idle"
    | Offline | Other _ ->
        Error
          (Error.Invalid_request
             "presence updates accept only Active or Idle status")
  in
  let history_limit_days =
    match history_limit_days with
    | Some value when value < 0 ->
        Error (Error.Invalid_request "history_limit_days must not be negative")
    | value -> Ok value
  in
  let ( let* ) = Result.bind in
  let* status = status in
  let* history_limit_days = history_limit_days in
  let int_param name = function
    | None -> []
    | Some value -> [ (name, string_of_int value) ]
  in
  let bool_param name = function
    | None -> []
    | Some value -> [ (name, string_of_bool value) ]
  in
  let params =
    [ ("status", status) ]
    @ int_param "last_update_id" last_update_id
    @ int_param "history_limit_days" history_limit_days
    @ bool_param "ping_only" ping_only
    @ bool_param "new_user_input" new_user_input
    @ bool_param "slim_presence" slim_presence
  in
  Client.request_typed client ~method_:`POST ~path:"/api/v1/users/me/presence"
    ~params ~codec:update_response_jsont ()

let pp_status ppf status = Format.pp_print_string ppf (status_to_string status)

let pp_user_presence ppf presence =
  Format.fprintf ppf "[@[%a@]]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
       (fun ppf presence ->
         Format.fprintf ppf "%s:%a" presence.client pp_status presence.status))
    presence.clients
