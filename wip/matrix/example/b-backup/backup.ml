module M = Matrix_eio
module Backup = M.Backup
module Cli = Matrix_cli

let encryption env ~profile client =
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Matrix_client.Crypto_store.create ~xdg ~profile in
  let enc =
    M.Encryption.of_env env ~user_id:(M.Client.user_id client)
      ~device_id:(M.Client.device_id client)
      ~store ()
  in
  M.Encryption.execute_requests enc client (M.Encryption.outgoing_requests enc);
  enc

let enable_run () homeserver username password profile =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    M.login_password ~sw ~env ~homeserver ~user:username ~password ()
  in
  let enc = encryption env ~profile client in
  (* Send one message to ourselves so there is a room key to back up: sending
     creates an outbound Megolm session, which is kept as an inbound one too
     so this device can read its own history back. *)
  let room_id = M.Rooms.create client ~encrypted:true () in
  let members = List.map fst (M.Rooms.get_joined_members client ~room_id) in
  ignore
    (M.Encryption.send_encrypted_text enc client room_id ~body:"backup me"
       ~members);
  let random = Matrix_client.Client.random (M.Client.base client) in
  let key = Backup.Decryption_key.generate ~random in
  (* Signing the auth data with this device's Ed25519 key lets another device
     tell which device made the backup, before deciding to trust it. *)
  let auth_data =
    M.Encryption.sign enc
      (Backup.auth_data_to_json
         { public_key = Backup.Decryption_key.public key; signatures = [] })
  in
  let version =
    M.Room_keys.create_version client ~algorithm:Backup.backup_algorithm
      ~auth_data
  in
  M.Encryption.enable_backup enc ~version ~decryption_key:key
    (Backup.Decryption_key.public key);
  let uploaded = M.Encryption.backup_pending enc client in
  M.Encryption.save enc;
  Logs.app (fun m -> m "Backup version %s created" version);
  Logs.app (fun m ->
      m "%d room key%s uploaded" uploaded (if uploaded = 1 then "" else "s"));
  Logs.app (fun m -> m "Recovery key: %s" (Backup.Recovery_key.encode key));
  Logs.app (fun m ->
      m
        "Write this down: it is the only thing that can read the backup, and \
         it is never stored on the server.")

let restore_run () homeserver username password profile recovery_key =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    M.login_password ~sw ~env ~homeserver ~user:username ~password ()
  in
  let enc = encryption env ~profile client in
  let key = Backup.Recovery_key.decode recovery_key in
  let info = M.Room_keys.get_current_version client in
  M.Encryption.enable_backup enc ~version:info.version ~decryption_key:key
    (Backup.Decryption_key.public key);
  let imported = M.Encryption.restore_from_backup enc client in
  M.Encryption.save enc;
  Logs.app (fun m ->
      m "Imported %d room key%s from backup version %s" imported
        (if imported = 1 then "" else "s")
        info.version)

let recovery_key_term =
  let doc = "The recovery key printed by $(b,enable)." in
  Cmdliner.Arg.(
    required & pos 0 (some string) None & info [] ~docv:"RECOVERY_KEY" ~doc)

let enable_cmd =
  let term =
    Cmdliner.Term.(
      const enable_run $ Cli.verbosity_term $ Cli.homeserver_term
      $ Cli.username_term $ Cli.password_term $ Cli.profile_term)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "enable"
       ~doc:"Create a key backup and upload this device's room keys")
    term

let restore_cmd =
  let term =
    Cmdliner.Term.(
      const restore_run $ Cli.verbosity_term $ Cli.homeserver_term
      $ Cli.username_term $ Cli.password_term $ Cli.profile_term
      $ recovery_key_term)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "restore"
       ~doc:"Restore room keys from an existing backup")
    term

let () =
  exit
    (Cmdliner.Cmd.eval
       (Cmdliner.Cmd.group
          (Cmdliner.Cmd.info "backup")
          [ enable_cmd; restore_cmd ]))
