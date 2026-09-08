open Result.Syntax

type presence_state = Online | Offline | Unavailable

let presence_state_to_string = function
  | Online -> "online"
  | Offline -> "offline"
  | Unavailable -> "unavailable"

let presence_state_of_string = function
  | "online" -> Ok Online
  | "offline" -> Ok Offline
  | "unavailable" -> Ok Unavailable
  | s -> Error (`Msg (Printf.sprintf "unknown presence state %S" s))

let presence_state_jsont =
  Jsont.of_of_string ~kind:"presence_state" ~enc:presence_state_to_string
    (fun s ->
      match presence_state_of_string s with
      | Ok v -> Ok v
      | Error (`Msg e) -> Error e)

type presence = {
  presence : presence_state;
  status_msg : string option;
  last_active_ago : int option;
  currently_active : bool option;
}

let presence_jsont =
  Jsont.Object.(
    map (fun presence status_msg last_active_ago currently_active ->
        { presence; status_msg; last_active_ago; currently_active })
    |> mem "presence" presence_state_jsont
    |> mem "status_msg"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.status_msg)
    |> opt_mem "last_active_ago" Matrix_proto.Json.Codec.int ~enc:(fun t ->
        t.last_active_ago)
    |> opt_mem "currently_active" Jsont.bool ~enc:(fun t -> t.currently_active)
    |> finish)

let status_path_route = Route.v "/presence/{user_id}/status"

let status_path user_id =
  Route.expand_exn status_path_route
    [ ("user_id", Matrix_proto.Id.User_id.to_string user_id) ]

let get_presence client ~user_id =
  let* body = Client.Http.get client ~path:(status_path user_id) () in
  Client.Http.decode_response presence_jsont body

type set_presence_request = {
  presence : presence_state;
  status_msg : string option;
}

let set_presence_request_jsont =
  Jsont.Object.(
    map (fun presence status_msg -> { presence; status_msg })
    |> mem "presence" presence_state_jsont ~enc:(fun t -> t.presence)
    |> opt_mem "status_msg" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.status_msg)
    |> finish)

let sync_presence_state = function
  | Online -> (`Online : Client.sync_presence)
  | Offline -> `Offline
  | Unavailable -> `Unavailable

(* The endpoint is scoped to a user ID and a client may only set its own. *)
let set_presence client ~presence ?status_msg ?(immediate = true) () =
  Client.set_sync_presence client (sync_presence_state presence);
  if not immediate then Ok ()
  else
    match Client.session client with
    | None -> Error Error.No_session
    | Some { Client.user_id; _ } ->
        let* body =
          Client.Http.encode_body set_presence_request_jsont
            { presence; status_msg }
        in
        let+ _ = Client.Http.put client ~path:(status_path user_id) ~body () in
        ()
