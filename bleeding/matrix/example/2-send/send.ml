module M = Matrix_eio

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
  let room_id = Matrix_proto.Id.Room_id.of_string_exn Sys.argv.(1) in
  let body = Sys.argv.(2) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  let event_id = M.Messages.send_text client ~room_id ~body () in
  Printf.printf "Sent %s\n%!" (Matrix_proto.Id.Event_id.to_string event_id)
