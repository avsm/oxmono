module M = Matrix_eio
module E = Matrix_proto.Event
module S = Matrix_proto.Sync

let getenv name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      Printf.eprintf "missing environment variable %s\n" name;
      exit 1

let print_messages (room_id, (room : S.Joined_room.t)) =
  match room.timeline with
  | None -> ()
  | Some timeline ->
      List.iter
        (fun (event : E.Raw_event.t) ->
          match event.type_ with
          | E.Event_type.Room_message -> (
              match
                Jsont.Json.decode E.Text_message_content.jsont event.content
              with
              | Ok content ->
                  Printf.printf "[%s] %s: %s\n%!" room_id
                    (Matrix_proto.Id.User_id.to_string event.sender)
                    (E.Text_message_content.body content)
              | Error _ -> ())
          | _ -> ())
        timeline.events

let on_sync (response : S.Response.t) =
  (match response.rooms with
  | None -> ()
  | Some rooms -> List.iter print_messages rooms.join);
  M.Sync.Continue

let () =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  M.run_sync ~sw ~env client ~on_sync ()
