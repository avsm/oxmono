module User_id = Matrix_proto.Id.User_id
module Device_id = Matrix_proto.Id.Device_id

type recipient = All | Device of Device_id.t
type messages = (User_id.t * (recipient * Jsont.json) list) list
type request = { messages : (string * (string * Jsont.json) list) list }

let recipient_to_string = function
  | All -> "*"
  | Device d -> Device_id.to_string d

let string_map_jsont = Matrix_proto.Json.Codec.string_map
let send_route = Route.v "/sendToDevice/{event_type}/{transaction_id}"

let request_jsont =
  Jsont.Object.(
    map (fun messages -> { messages })
    |> mem "messages"
         (string_map_jsont (string_map_jsont Matrix_proto.Json.Codec.json))
         ~enc:(fun t -> t.messages)
    |> finish)

let send client ~event_type ~txn_id messages =
  let messages =
    List.filter_map
      (fun (user_id, devices) ->
        match devices with
        | [] -> None
        | _ ->
            Some
              ( User_id.to_string user_id,
                List.map (fun (r, j) -> (recipient_to_string r, j)) devices ))
      messages
  in
  match Client.Http.encode_body request_jsont { messages } with
  | Error e -> Error e
  | Ok body -> (
      let path =
        Route.expand_exn send_route
          [ ("event_type", event_type); ("transaction_id", txn_id) ]
      in
      match Client.Http.put client ~path ~body () with
      | Error e -> Error e
      | Ok _ -> Ok ())

let send_with_new_txn client ~event_type messages =
  let txn_id = Random.txn_id (Client.random client) in
  send client ~event_type ~txn_id messages
