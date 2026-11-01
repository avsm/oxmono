open Result.Syntax

let src = Logs.Src.create "matrix.report" ~doc:"Matrix content reporting"

module Log = (val Logs.src_log src : Logs.LOG)

type request = { reason : string option }

let request_jsont =
  Jsont.Object.(
    map ~kind:"report_request" (fun reason -> { reason })
    |> opt_mem "reason" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.reason)
    |> finish)

let post client ~path ~reason =
  let* body = Client.Http.encode_body request_jsont { reason } in
  let+ _ = Client.Http.post client ~path ~body () in
  Log.info (fun m -> m "Reported %s" path)

let event_route = Route.v "/rooms/{room_id}/report/{event_id}"
let stable_room_route = Route.v "/_matrix/client/v3/rooms/{room_id}/report"

let unstable_room_route =
  Route.v "/_matrix/client/unstable/org.matrix.msc4151/rooms/{room_id}/report"

let stable_user_route = Route.v "/_matrix/client/v3/users/{user_id}/report"

let unstable_user_route =
  Route.v "/_matrix/client/unstable/org.matrix.msc4260/users/{user_id}/report"

let event client ~room_id ~event_id ?reason ?score () =
  (* [score] was part of an older report API, but the current Matrix request
     content contains only [reason]. Keep accepting it for source compatibility
     while deliberately never putting it on the wire. *)
  ignore score;
  let path =
    Route.expand_exn event_route
      [
        ("room_id", Matrix_proto.Id.Room_id.to_string room_id);
        ("event_id", Matrix_proto.Id.Event_id.to_string event_id);
      ]
  in
  post client ~path ~reason

let room client ~room_id ?reason () =
  let* versions = Server.get_versions client in
  let route =
    if Server.supports_version_at_least versions ~major:1 ~minor:13 then
      stable_room_route
    else unstable_room_route
  in
  let path =
    Route.expand_exn route
      [ ("room_id", Matrix_proto.Id.Room_id.to_string room_id) ]
  in
  let* body = Client.Http.encode_body request_jsont { reason } in
  let+ _ = Client.Http.post_absolute client ~path ~body () in
  Log.info (fun m -> m "Reported %s" path)

let user client ~user_id ?reason () =
  let* versions = Server.get_versions client in
  let route =
    if Server.supports_version_at_least versions ~major:1 ~minor:14 then
      stable_user_route
    else unstable_user_route
  in
  let path =
    Route.expand_exn route
      [ ("user_id", Matrix_proto.Id.User_id.to_string user_id) ]
  in
  let* body = Client.Http.encode_body request_jsont { reason } in
  let+ _ = Client.Http.post_absolute client ~path ~body () in
  Log.info (fun m -> m "Reported %s" path)
