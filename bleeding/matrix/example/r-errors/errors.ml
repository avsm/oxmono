module M = Matrix_client
module Id = Matrix_proto.Id

let getenv name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      Printf.eprintf "missing environment variable %s\n" name;
      exit 1

let ok = function
  | Ok v -> v
  | Error e ->
      Format.eprintf "unexpected error: %a\n%!" M.Error.pp e;
      exit 1

(* Only these two kinds of failure are worth trying again: the request never
   reached the server, or the server asked the caller to slow down. *)
let is_retryable = function
  | M.Error.Network_error _ -> true
  | M.Error.Matrix_error { errcode = M.Error.M_LIMIT_EXCEEDED; _ } -> true
  | _ -> false

let retry_after_ms = function
  | M.Error.Matrix_error { retry_after_ms; _ } -> retry_after_ms
  | _ -> None

let rec attempt client n =
  match M.Auth.whoami client with
  | Ok user_id -> Printf.printf "whoami: %s\n" (Id.User_id.to_string user_id)
  | Error e when (not (is_retryable e)) || n >= 3 ->
      Printf.printf "gave up after attempt %d: %s\n" n (M.Error.to_string e)
  | Error e ->
      (match retry_after_ms e with
      | Some ms -> Printf.printf "attempt %d failed, retrying after %dms\n" n ms
      | None -> Printf.printf "attempt %d failed, retrying\n" n);
      attempt client (n + 1)

let () =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  Eio_main.run @@ fun env ->
  let fetch = Fetch_httpz.std env in
  let random = M.Random.of_env env in
  let config = M.Client.config ~homeserver () in
  let client = M.Client.create ~config ~fetch ~random in
  let session = ok (M.Auth.login_password client ~user ~password ()) in
  let client = M.Client.with_session client session in

  let missing_alias = Id.Room_alias.of_string_exn "#does-not-exist:localhost" in
  (match
     M.Rooms.join client ~room_id_or_alias:(`Room_alias missing_alias) ()
   with
  | Error (M.Error.Matrix_error { errcode = M.Error.M_NOT_FOUND; _ }) ->
      Printf.printf "matrix error: room not found\n"
  | Error e -> Printf.printf "unexpected error: %s\n" (M.Error.to_string e)
  | Ok _ -> Printf.printf "unexpectedly joined\n");

  let anonymous = M.Client.create ~config ~fetch ~random in
  (match M.Presence.set_presence anonymous ~presence:M.Presence.Online () with
  | Error M.Error.No_session -> Printf.printf "no session, as expected\n"
  | Error e -> Printf.printf "unexpected error: %s\n" (M.Error.to_string e)
  | Ok () -> Printf.printf "unexpectedly answered\n");

  let closed_port =
    M.Client.config ~homeserver:(Uriz.of_string_exn "http://127.0.0.1:1") ()
  in
  let unreachable = M.Client.create ~config:closed_port ~fetch ~random in
  (match M.Auth.whoami unreachable with
  | Error (M.Error.Network_error msg) -> Printf.printf "network error: %s\n" msg
  | Error e -> Printf.printf "unexpected error: %s\n" (M.Error.to_string e)
  | Ok _ -> Printf.printf "unexpectedly answered\n");

  attempt unreachable 1
