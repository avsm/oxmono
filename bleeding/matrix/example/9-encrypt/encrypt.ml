module M = Matrix_eio
module E = Matrix_proto.Event
module S = Matrix_proto.Sync
module Cli = Matrix_cli

(* A safety net for an unattended run: give up after this many sync responses
   if nobody ever replies, rather than hang forever. Ctrl-C stops it sooner. *)
let max_rounds = 10

let print_incoming enc room_id (event : E.Raw_event.t) =
  match M.Encryption.decrypt_room_event enc room_id event with
  | Error err ->
      Logs.app (fun m ->
          m "! [%s] %s: undecryptable (%a)"
            (Matrix_proto.Id.Room_id.to_string room_id)
            (Matrix_proto.Id.User_id.to_string event.sender)
            M.Encryption.pp_decrypt_error err)
  | Ok d -> (
      match
        Jsont.Json.decode E.Text_message_content.jsont d.decrypted_content
      with
      | Ok content ->
          Logs.app (fun m ->
              m "* [%s] %s: %s"
                (Matrix_proto.Id.Room_id.to_string room_id)
                (Matrix_proto.Id.User_id.to_string event.sender)
                (E.Text_message_content.body content))
      | Error _ -> ())

let run () homeserver username password profile recipient message =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    M.login_password ~sw ~env ~homeserver ~user:username ~password ()
  in
  let user_id = M.Client.user_id client
  and device_id = M.Client.device_id client in
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Matrix_client.Crypto_store.create ~xdg ~profile in
  let enc = M.Encryption.of_env env ~user_id ~device_id ~store () in
  (* A fresh machine has nothing but its own device and one-time keys to
     publish; do that before anyone can claim a session with this device. *)
  M.Encryption.execute_requests enc client (M.Encryption.outgoing_requests enc);
  let room_id =
    M.Rooms.create client ~invite:[ recipient ] ~is_direct:true ~encrypted:true
      ()
  in
  (* The room's own [m.room.encryption] state is what the machine must agree
     with; a sync would learn it from the server, but nothing has synced yet,
     so record the settings this room was just created with directly. *)
  let settings = M.Encryption.enable_room_encryption () in
  M.Encryption.set_room_encryption_settings enc room_id
    (M.Encryption.room_encryption_content settings);
  let members = List.map fst (M.Rooms.get_joined_members client ~room_id) in
  let event_id =
    M.Encryption.send_encrypted_text enc client room_id ~body:message ~members
  in
  M.Encryption.save enc;
  Logs.app (fun m ->
      m "Room:    %s" (Matrix_proto.Id.Room_id.to_string room_id));
  Logs.app (fun m ->
      m "Invited: %s" (Matrix_proto.Id.User_id.to_string recipient));
  Logs.app (fun m ->
      m "Sent:    %s" (Matrix_proto.Id.Event_id.to_string event_id));
  let rounds = ref 0 in
  let reply_seen = ref false in
  let on_sync (response : S.Response.t) =
    (match response.rooms with
    | None -> ()
    | Some rooms ->
        List.iter
          (fun (rid, (room : S.Joined_room.t)) ->
            let room_id = Matrix_proto.Id.Room_id.of_string_exn rid in
            match room.timeline with
            | None -> ()
            | Some timeline ->
                List.iter
                  (fun (e : E.Raw_event.t) ->
                    if
                      E.Event_type.equal e.type_
                        E.Event_type.Room_message_encrypted
                    then begin
                      print_incoming enc room_id e;
                      if not (Matrix_proto.Id.User_id.equal e.sender user_id)
                      then reply_seen := true
                    end)
                  timeline.events)
          rooms.join);
    incr rounds;
    if !reply_seen || !rounds >= max_rounds then M.Sync.Stop
    else M.Sync.Continue
  in
  M.run_sync ~sw ~env client ~on_sync ~encryption:enc ()

let term =
  Cmdliner.Term.(
    const run $ Cli.verbosity_term $ Cli.homeserver_term $ Cli.username_term
    $ Cli.password_term $ Cli.profile_term $ Cli.recipient_term
    $ Cli.message_term)

let cmd =
  let doc = "Create an end-to-end encrypted room and send one message" in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "encrypt" ~doc) term

let () = exit (Cmdliner.Cmd.eval cmd)
