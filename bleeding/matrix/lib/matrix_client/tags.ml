open Result.Syntax

let src = Logs.Src.create "matrix.tags" ~doc:"Matrix room tags"

module Log = (val Logs.src_log src : Logs.LOG)

let favourite = "m.favourite"
let low_priority = "m.lowpriority"
let server_notice = "m.server_notice"

let order_jsont =
  Jsont.Object.(
    map ~kind:"tag" Fun.id
    |> opt_mem "order" Matrix_proto.Json.Codec.number ~enc:Fun.id
    |> skip_unknown |> finish)

let tags_response_jsont =
  Jsont.Object.(
    map ~kind:"tags_response" Fun.id
    |> mem "tags"
         (Json_codec.string_map order_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:Fun.id
    |> finish)

let tags_path_route = Route.v "/user/{user_id}/rooms/{room_id}/tags"
let tag_path_route = Route.v "/user/{user_id}/rooms/{room_id}/tags/{tag}"

let tags_path ~user_id ~room_id =
  Route.expand_exn tags_path_route
    [
      ("user_id", Matrix_proto.Id.User_id.to_string user_id);
      ("room_id", Matrix_proto.Id.Room_id.to_string room_id);
    ]

let tag_path ~user_id ~room_id ~tag =
  Route.expand_exn tag_path_route
    [
      ("user_id", Matrix_proto.Id.User_id.to_string user_id);
      ("room_id", Matrix_proto.Id.Room_id.to_string room_id);
      ("tag", tag);
    ]

let get client ~user_id ~room_id =
  let path = tags_path ~user_id ~room_id in
  let* body = Client.Http.get client ~path () in
  Client.Http.decode_response tags_response_jsont body

let set client ~user_id ~room_id ~tag ?order () =
  let path = tag_path ~user_id ~room_id ~tag in
  let* body = Client.Http.encode_body order_jsont order in
  let+ _ = Client.Http.put client ~path ~body () in
  Log.debug (fun m -> m "Tagged room with %s" tag)

let remove client ~user_id ~room_id ~tag =
  let path = tag_path ~user_id ~room_id ~tag in
  let+ _ = Client.Http.delete client ~path () in
  Log.debug (fun m -> m "Removed tag %s" tag)

(* The endpoint is scoped to a user id, and a client may only tag rooms for
   itself. *)
let toggle client ~room_id ~tag ~on ?order () =
  match Client.session client with
  | None -> Error Error.No_session
  | Some { Client.user_id; _ } ->
      if on then set client ~user_id ~room_id ~tag ?order ()
      else remove client ~user_id ~room_id ~tag

let set_favourite client ~room_id ~favourite:on ?order () =
  toggle client ~room_id ~tag:favourite ~on ?order ()

let set_low_priority client ~room_id ~low_priority:on ?order () =
  toggle client ~room_id ~tag:low_priority ~on ?order ()
