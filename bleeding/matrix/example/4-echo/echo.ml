module M = Matrix_eio
module E = Matrix_proto.Event
module S = Matrix_proto.Sync

let getenv name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      Printf.eprintf "missing environment variable %s\n" name;
      exit 1

let echo client ~self (room_id, (room : S.Joined_room.t)) =
  match room.timeline with
  | None -> ()
  | Some timeline ->
      List.iter
        (fun (event : E.Raw_event.t) ->
          if
            (not (Matrix_proto.Id.User_id.equal event.sender self))
            && E.Event_type.equal event.type_ E.Event_type.Room_message
          then
            match
              Jsont.Json.decode E.Text_message_content.jsont event.content
            with
            | Ok content
              when E.Text_message_content.msgtype content = E.Msgtype.Text ->
                let room_id = Matrix_proto.Id.Room_id.of_string_exn room_id in
                let body = E.Text_message_content.body content in
                ignore (M.Messages.send_notice client ~room_id ~body)
            | _ -> ())
        timeline.events

let () =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  let self = M.Client.user_id client in
  let first_response = ref true in
  let on_sync (response : S.Response.t) =
    (if !first_response then first_response := false
     else
       match response.rooms with
       | None -> ()
       | Some rooms -> List.iter (echo client ~self) rooms.join);
    M.Sync.Continue
  in
  M.run_sync ~sw ~env client ~on_sync ()
