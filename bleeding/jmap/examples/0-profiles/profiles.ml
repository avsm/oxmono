module Auth = Jmap_eio.Auth
module Cli = Jmap_eio.Cli
module Client = Jmap_eio.Client
module Profile = Jmap_eio.Profile
module Proto = Jmap.Proto

let ( let* ) = Result.bind
let profile_result result = Result.map_error Profile.error_to_string result

let list_profiles () =
  Eio_main.run @@ fun env ->
  let* store = profile_result (Profile.xdg_store env) in
  let* profiles = profile_result (Profile.list store) in
  List.iter
    (fun profile ->
      Fmt.pr "%-20s %s@." (Profile.name profile)
        (Cli.terminal_text (Profile.session_url profile)))
    profiles;
  if profiles = [] then Fmt.pr "No saved profiles.@.";
  Ok ()

let save_profile config name =
  Eio_main.run @@ fun env ->
  let* config = Cli.resolve env config in
  let* key =
    match config.api_key_file with
    | None -> Ok config.api_key
    | Some path -> Auth.read_secret_file ~fs:(Eio.Stdenv.fs env) path
  in
  let* credential =
    match config.auth with
    | Auth.Bearer -> Ok (Profile.Bearer key)
    | Auth.Basic -> (
        match String.index_opt key ':' with
        | None -> Error "a Basic key must be user:password"
        | Some split ->
            let user = String.sub key 0 split in
            let password =
              String.sub key (split + 1) (String.length key - split - 1)
            in
            Ok (Profile.Basic { user; password }))
  in
  let* profile =
    profile_result (Profile.v ~name ~session_url:config.session_url credential)
  in
  let* store = profile_result (Profile.xdg_store env) in
  let* () = profile_result (Profile.save store profile) in
  Fmt.pr "Saved profile %s.@." (Profile.name profile);
  Ok ()

let connect_profile name allow_insecure =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let* client =
    profile_result
      (Profile.connect_name ~sw ~timeout:30. ~allow_insecure env name)
  in
  let session = Client.session client in
  Fmt.pr "Connected as %s.@." (Cli.terminal_text session.username);
  match Proto.Session.primary_account_for Proto.Capability.mail session with
  | None -> Error "the session has no primary mail account"
  | Some account_id ->
      Fmt.pr "Primary mail account: %a@." Proto.Id.pp account_id;
      Ok ()

let () =
  let open Cmdliner in
  let name = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME") in
  let selected_profile =
    let require = function
      | Some name -> Ok name
      | None -> Error "select --profile NAME or set JMAP_PROFILE"
    in
    Term.(term_result' ~usage:true (const require $ Cli.profile_term))
  in
  let listing = Term.(term_result' (const list_profiles $ const ())) in
  let list_cmd =
    Cmd.v (Cmd.info "list" ~doc:"List saved profile names and URLs") listing
  in
  let save_cmd =
    Cmd.v
      (Cmd.info "save"
         ~doc:
           "Create or replace a profile from the connection settings (offline)")
      Term.(term_result' (const save_profile $ Cli.config_term $ name))
  in
  let connect_cmd =
    Cmd.v
      (Cmd.info "connect" ~doc:"Connect using a saved profile")
      Term.(
        term_result'
          (const connect_profile $ selected_profile $ Cli.allow_insecure_term))
  in
  exit
    (Cmd.eval
       (Cmd.group ~default:listing
          (Cmd.info "profiles" ~doc:"Save, list and use shared JMAP logins")
          [ list_cmd; save_cmd; connect_cmd ]))
