open Result.Syntax
module Id = Matrix_proto.Id
module Timestamp = Matrix_proto.Event.Timestamp

type connection = {
  ip : string option;
  last_seen : Timestamp.t option;
  user_agent : string option;
}

type connection_info = connection
type session = { connections : connection list }
type session_info = session
type device = { sessions : session list }
type device_info = device

type response = {
  user_id : Id.User_id.t option;
  devices : (string * device) list;
}

(* ruma's Option<T> fields accept either null or an omitted member.  [mem]
   with [Jsont.option], unlike [opt_mem], also accepts explicit null; the
   [dec_absent] values cover serde's omitted-field behaviour exactly. *)
let connection_jsont =
  Jsont.Object.(
    map ~kind:"admin_connection" (fun ip last_seen user_agent ->
        { ip; last_seen; user_agent })
    |> mem "ip" (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
    |> mem "last_seen" (Jsont.option Timestamp.jsont) ~dec_absent:(fun () ->
        None)
    |> mem "user_agent" (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
    |> finish)

let session_jsont =
  Jsont.Object.(
    map ~kind:"admin_session" (fun connections -> { connections })
    |> mem "connections" (Jsont.list connection_jsont) ~dec_absent:(fun () ->
        [])
    |> finish)

let device_jsont =
  Jsont.Object.(
    map ~kind:"admin_device" (fun sessions -> { sessions })
    |> mem "sessions" (Jsont.list session_jsont) ~dec_absent:(fun () -> [])
    |> finish)

let response_jsont =
  Jsont.Object.(
    map ~kind:"admin_whois" (fun user_id devices -> { user_id; devices })
    |> mem "user_id" (Jsont.option Id.User_id.jsont) ~dec_absent:(fun () ->
        None)
    |> mem "devices" (Json_codec.string_map device_jsont) ~dec_absent:(fun () ->
        [])
    |> finish)

let user_id_path_route = Route.v "/admin/whois/{user_id}"

let user_id_path user_id =
  Route.expand_exn user_id_path_route
    [ ("user_id", Id.User_id.to_string user_id) ]

let whois client ~user_id =
  let* body = Client.Http.get client ~path:(user_id_path user_id) () in
  Client.Http.decode_response response_jsont body

let get_user_info = whois
