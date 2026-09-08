module M = Matrix_eio
module Vs = M.Verification_service
module Flow = Matrix_client.Verification.Flow
module Cli = Matrix_cli

(* Anything but an explicit yes is a no: the cost of a wrong yes is a device
   that reads everything from now on. *)
let ask question =
  Logs.app (fun m -> m "%s" question);
  match
    String.lowercase_ascii
      (String.trim (try read_line () with End_of_file -> ""))
  with
  | "y" | "yes" -> true
  | _ -> false

let confirm (p : Vs.prompt) =
  Logs.app (fun m ->
      m "Verifying %s" (Matrix_proto.Id.User_id.to_string p.their_user_id));
  List.iter
    (fun (e : Vs.emoji) ->
      Logs.app (fun m -> m "  %s  %s" e.symbol e.description))
    p.emoji;
  ask "Do the emoji match on both screens? [y/N] "

(* The SAS key agreement needs the peer's identity key, which only a
   [/keys/query] can supply, so both a request and an accept fetch it first. *)
let fetch_device enc client user_id =
  M.Encryption.track_users enc [ user_id ];
  M.Encryption.execute_requests enc client (M.Encryption.outgoing_requests enc)

let run () homeserver username password profile listen target =
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
  M.Encryption.execute_requests enc client (M.Encryption.outgoing_requests enc);
  let result = ref None in
  let ver =
    Vs.create ~client ~encryption:enc ~confirm
      ~on_result:(fun r -> result := Some r)
      ()
  in
  (match target with
  | Some target ->
      fetch_device enc client target;
      ignore (Vs.request ver client target);
      Logs.app (fun m ->
          m "Verification request sent to %s; waiting for it to accept..."
            (Matrix_proto.Id.User_id.to_string target))
  | None -> Logs.app (fun m -> m "Waiting for a verification request..."));
  let accepted = ref false in
  (* [on_sync] runs on the fiber [run_sync] forks; the epilogue below has to
     live inside it, because code placed after [run_sync] in this function
     would run as soon as the fiber is started, not once it finishes. *)
  let on_sync _ =
    if listen && not !accepted then
      List.iter
        (fun s ->
          if Flow.session_stage s = Flow.Requested then begin
            accepted := true;
            let their_user_id = Flow.session_their_user_id s in
            Logs.app (fun m ->
                m "%s wants to verify with you."
                  (Matrix_proto.Id.User_id.to_string their_user_id));
            if ask "Accept? [y/N] " then begin
              fetch_device enc client their_user_id;
              Vs.accept ver client s
            end
            else
              Vs.cancel ver client s Matrix_client.Verification.Cancel_code.User
          end)
        (Vs.sessions ver);
    match !result with
    | None -> M.Sync.Continue
    | Some r ->
        M.Encryption.save enc;
        (match r with
        | Vs.Verified { user_id; device_id } ->
            Logs.app (fun m ->
                m "Verified %s%s"
                  (Matrix_proto.Id.User_id.to_string user_id)
                  (match device_id with
                  | Some d -> " (" ^ Matrix_proto.Id.Device_id.to_string d ^ ")"
                  | None -> ""))
        | Vs.Publication_failed { user_id; reason; _ } ->
            Logs.err (fun m ->
                m
                  "Verification with %s succeeded, but publishing its \
                   signature failed: %s"
                  (Matrix_proto.Id.User_id.to_string user_id)
                  reason)
        | Vs.Cancelled { user_id; code } ->
            Logs.app (fun m ->
                m "Verification with %s was cancelled: %s"
                  (Matrix_proto.Id.User_id.to_string user_id)
                  (Matrix_client.Verification.Cancel_code.reason code)));
        M.Sync.Stop
  in
  M.run_sync ~sw ~env client ~on_sync ~encryption:enc ~verification:ver ()

let listen_term =
  let doc =
    "Wait for another device to ask to verify with this one, instead of \
     asking. Cannot be combined with $(i,USER_ID)."
  in
  Cmdliner.Arg.(value & flag & info [ "listen" ] ~doc)

let target_term =
  let doc = "The user to verify with, as $(b,@user:server)." in
  Cmdliner.Arg.(
    value & pos 0 (some Cli.user_id_conv) None & info [] ~docv:"USER_ID" ~doc)

let term =
  let run () homeserver username password profile listen target =
    match (listen, target) with
    | true, Some _ | false, None ->
        prerr_endline "give a user id, or --listen, but not both";
        exit Cli.exit_usage
    | _ -> run () homeserver username password profile listen target
  in
  Cmdliner.Term.(
    const run $ Cli.verbosity_term $ Cli.homeserver_term $ Cli.username_term
    $ Cli.password_term $ Cli.profile_term $ listen_term $ target_term)

let cmd =
  let doc = "Verify another of your devices by comparing emoji (SAS)" in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "verify" ~doc) term

let () = exit (Cmdliner.Cmd.eval cmd)
