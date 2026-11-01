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
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  let user_id = M.Auth.whoami client in
  let device_id = M.Client.device_id client in
  Printf.printf "Logged in as %s on device %s\n%!"
    (Matrix_proto.Id.User_id.to_string user_id)
    (Matrix_proto.Id.Device_id.to_string device_id);
  M.Auth.logout client
