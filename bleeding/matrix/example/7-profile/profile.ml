module M = Matrix_eio
module Profile_store = Matrix_client.Profile_store
module Session = Matrix_client.Session

let getenv name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      Printf.eprintf "missing environment variable %s\n" name;
      exit 1

let profile = match Sys.argv with [| _; p |] -> p | _ -> "default"

let login_and_save ~sw ~env store =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  let session = Option.get (M.Client.session client) in
  let now = Ptime_clock.now () in
  let file : Session.Session_file.t =
    {
      server = { homeserver; user_id = session.user_id };
      auth =
        {
          access_token = session.access_token;
          device_id = session.device_id;
          refresh_token = session.refresh_token;
          access_token_expires_at = None;
          method_ = Session.Auth.Matrix;
        };
      sync = { next_batch = None; filter_id = None };
      metadata =
        { created_at = now; last_used_at = now; client_name = "7-profile" };
    }
  in
  (match Profile_store.save_session store file with
  | Ok () -> ()
  | Error e ->
      Format.eprintf "cannot save session: %a\n%!" Matrix_client.Error.pp e;
      exit 1);
  Printf.printf "Logged in and saved session to profile %S\n%!" profile;
  client

let client_of_session (session : Session.Session_file.t) ~sw ~env =
  let client =
    M.Client.create ~sw ~env ~homeserver:session.server.homeserver ()
  in
  M.Client.with_session client
    {
      user_id = session.server.user_id;
      device_id = session.auth.device_id;
      access_token = session.auth.access_token;
      refresh_token = session.auth.refresh_token;
    }

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Profile_store.create ~xdg ~profile in
  let client =
    match Profile_store.load_session store with
    | Ok (Some session) ->
        Printf.printf "Reusing session for profile %S\n%!" profile;
        client_of_session session ~sw ~env
    | Ok None -> login_and_save ~sw ~env store
    | Error e ->
        Format.eprintf "session for profile %S is unreadable: %a\n%!" profile
          Matrix_client.Error.pp e;
        exit 1
  in
  let who = M.Auth.whoami client in
  Printf.printf "Logged in as %s\n%!" (Matrix_proto.Id.User_id.to_string who)
