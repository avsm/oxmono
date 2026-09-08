open Result.Syntax

type typing_request = { typing : bool; timeout : int option }

let typing_request_jsont =
  Jsont.Object.(
    map (fun typing timeout -> { typing; timeout })
    |> mem "typing" Jsont.bool ~enc:(fun t -> t.typing)
    |> opt_mem "timeout" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.timeout)
    |> finish)

type typing_content = { user_ids : Matrix_proto.Id.User_id.t list }

let typing_content_jsont =
  Jsont.Object.(
    map (fun user_ids -> { user_ids })
    |> mem "user_ids" (Jsont.list Matrix_proto.Id.User_id.jsont)
    |> finish)

let users_of_content (json : Jsont.json) =
  match Jsont.Json.decode typing_content_jsont json with
  | Ok { user_ids } -> Ok user_ids
  | Error msg -> Error (Error.Json_error msg)

let status_route = Route.v "/rooms/{room_id}/typing/{user_id}"

(* The endpoint is scoped to a user ID and a client may only report its
   own. *)
let set_typing client ~room_id ~typing ?timeout () =
  match Client.session client with
  | None -> Error Error.No_session
  | Some { Client.user_id; _ } ->
      let path =
        Route.expand_exn status_route
          [
            ("room_id", Matrix_proto.Id.Room_id.to_string room_id);
            ("user_id", Matrix_proto.Id.User_id.to_string user_id);
          ]
      in
      let* body =
        Client.Http.encode_body typing_request_jsont { typing; timeout }
      in
      let+ _ = Client.Http.put client ~path ~body () in
      ()
