module M = Matrix_eio
module E = Matrix_proto.Event

let getenv name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      Printf.eprintf "missing environment variable %s\n" name;
      exit 1

let () =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  let invitee = Matrix_proto.Id.User_id.of_string_exn Sys.argv.(1) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  let room_id =
    M.Rooms.create client ~name:"Tutorial room" ~topic:"Created by 5-rooms"
      ~preset:M.Rooms.Private_chat ()
  in
  Printf.printf "Created %s\n%!" (Matrix_proto.Id.Room_id.to_string room_id);
  M.Rooms.invite client ~room_id ~user_id:invitee ();
  Printf.printf "Invited %s\n%!" (Matrix_proto.Id.User_id.to_string invitee);
  let topic_event =
    M.State.set_topic client ~room_id ~topic:"Set after creation"
  in
  Printf.printf "Topic set (%s)\n%!"
    (Matrix_proto.Id.Event_id.to_string topic_event);
  Printf.printf "Joined rooms: %d\n%!"
    (List.length (M.Rooms.get_joined_rooms client));
  List.iter
    (fun (user_id, (m : M.Rooms.joined_member)) ->
      Printf.printf "  joined: %s (%s)\n%!"
        (Matrix_proto.Id.User_id.to_string user_id)
        (Option.value m.display_name ~default:"no display name"))
    (M.Rooms.get_joined_members client ~room_id);
  List.iter
    (fun (m : M.Rooms.member) ->
      Printf.printf "  member: %s is %s\n%!"
        (Matrix_proto.Id.User_id.to_string m.user_id)
        (E.Membership.to_string m.membership))
    (M.Rooms.get_members client ~room_id ());
  M.Rooms.leave client ~room_id ()
