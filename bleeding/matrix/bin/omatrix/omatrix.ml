(** omatrix - Command-line Matrix client.

    A CLI tool for interacting with Matrix homeservers, supporting session
    persistence and common operations like login, messaging, and room
    management.

    {b Quick Start}

    {[
      # Login and store session
      omatrix login -s https://matrix.org -u @you:matrix.org

      # Or log in with OAuth 2.0 in a browser (Matrix 1.15)
      omatrix login --oauth -s https://matrix.org

      # Send a direct message (uses stored session)
      omatrix msg -t @them:matrix.org "Hello!"

      # Send to a room
      omatrix msg -r '!roomid:matrix.org' "Hello room!"

      # Check current session
      omatrix whoami

      # Publish this device's encryption keys
      omatrix keys init

      # Verify another device by comparing emoji
      omatrix verify @them:matrix.org

      # Back the room keys up, so a new device can read old messages
      omatrix backup enable

      # Follow the timeline, decrypting encrypted rooms
      omatrix sync

      # Logout and clear session
      omatrix logout
    ]}

    Messages sent with [omatrix msg] are encrypted automatically whenever the
    room's state says [m.room.encryption], and [omatrix sync] decrypts what it
    can. Both need [omatrix keys init] to have run once for the profile.

    Environment variables:
    - [MATRIX_HOMESERVER]: Default homeserver URL
    - [MATRIX_USERNAME]: Default username
    - [MATRIX_PASSWORD]: Password (or [--password-file])
    - [MATRIX_SSSS_CREDENTIAL]: SSSS passphrase or recovery key for [qr grant]
      (or [--credential-file]) *)

open Cmdliner
module Cmd = Matrix_cli
module Session = Matrix_client.Session
module Profile_store = Matrix_client.Profile_store
module Base_client = Matrix_client.Base_client
module Store = Matrix_client.Store

let app_name = "omatrix"

let with_xdg ~env f =
  let fs = Eio.Stdenv.fs env in
  let xdg = Xdge.create fs "matrix" in
  f xdg

let load_session ~env ~profile =
  with_xdg ~env @@ fun xdg ->
  let store = Profile_store.create ~xdg ~profile in
  match Profile_store.load_session store with
  | Ok session -> Option.map (fun session -> (store, session)) session
  | Error e ->
      Logs.err (fun m ->
          m "The session of profile '%s' is unreadable: %a" profile
            Matrix_client.Error.pp e);
      Logs.err (fun m ->
          m "Refusing to overwrite it; move it aside to start over");
      exit Cmd.exit_internal

let log_no_session ~profile =
  Logs.err (fun m -> m "No session found for profile '%s'" profile);
  Logs.err (fun m -> m "Use 'omatrix login' to authenticate first")

let http_client ~env ~homeserver (policy : Cmd.http_policy) =
  let o = Cmd.http_options ~homeserver policy in
  Fetch_httpz.std ?retry:o.retry ?min_interval:o.min_interval
    ?max_concurrent:o.max_concurrent ?connect_timeout:o.connect_timeout
    ?idle_timeout:o.idle_timeout env

let client_from_session ~sw ~env ~policy ~store
    (session : Session.Session_file.t) =
  let homeserver = session.server.homeserver in
  let client0 =
    Matrix_eio.Client.create ~sw ~env ~homeserver
      ~fetch:(http_client ~env ~homeserver policy)
      ()
  in
  let matrix_session : Matrix_client.Client.session =
    {
      user_id = session.server.user_id;
      device_id = session.auth.device_id;
      access_token = session.auth.access_token;
      refresh_token = session.auth.refresh_token;
    }
  in
  let current = ref session in
  (* The refresh coordinator persists before notifying. This callback must not
     overwrite a newer rotation made by another process. *)
  let on_session_update (refreshed : Matrix_client.Client.session) expires_at =
    let old = !current in
    current :=
      {
        old with
        auth =
          {
            old.auth with
            access_token = refreshed.access_token;
            refresh_token = refreshed.refresh_token;
            access_token_expires_at = expires_at;
          };
      };
    Ok ()
  in
  let client = Matrix_eio.Client.with_session client0 matrix_session in
  let client =
    match (session.auth.refresh_token, session.auth.method_) with
    | None, _ -> client
    | Some _, Session.Auth.OAuth { client_id } ->
        Matrix_eio.Oauth.with_auto_refresh_expiry ~store ~on_session_update
          ?expires_at:session.auth.access_token_expires_at
          ~on_session_invalid:(fun Matrix_eio.Oauth.Invalid_grant ->
            Logs.err (fun m ->
                m "OAuth session is no longer valid; run 'omatrix login' again"))
          client ~client_id
    | Some _, Session.Auth.Matrix ->
        let unauthenticated =
          Matrix_client.Client.without_session (Matrix_eio.Client.base client)
        in
        let refresh (old : Matrix_client.Client.session) =
          match old.refresh_token with
          | None ->
              Error
                (Matrix_client.Error.Json_error
                   "session refresh requested without a refresh token")
          | Some refresh_token ->
              Result.map
                (fun (tokens : Matrix_client.Auth.refreshed_with_expiry) ->
                  Matrix_client.Client.
                    {
                      refreshed_tokens =
                        {
                          access_token = tokens.refreshed.access_token;
                          refresh_token = tokens.refreshed.refresh_token;
                        };
                      expires_at = tokens.expires_at;
                    })
                (Matrix_client.Auth.refresh_token_with_expiry unauthenticated
                   ~refresh_token)
        in
        Matrix_eio.Client.with_auto_refresh_expiry ~store ~refresh
          ~on_session_update ?expires_at:session.auth.access_token_expires_at
          client
  in
  (client, current)

(** Run [f] with a client built from the stored session, or exit. *)
let with_client ~profile ~policy f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match load_session ~env ~profile with
  | None ->
      log_no_session ~profile;
      exit Cmd.exit_auth
  | Some (store, session) ->
      let client, _current =
        client_from_session ~sw ~env ~policy ~store session
      in
      f ~env ~sw ~store ~session ~client

(** Turn an [Eio.Io] from the SDK into a diagnostic and an exit code, so a
    command body can be written as if nothing failed. *)
let guard what f =
  try f ()
  with Eio.Io (Matrix_eio.Error.E err, _) ->
    Logs.err (fun m -> m "%s: %a" what Matrix_eio.Error.pp_err err);
    exit Cmd.exit_network

(** The crypto store for a profile. It shares
    [$XDG_DATA_HOME/matrix/profiles/NAME/] with the session, so the device keys
    sit beside the access token that identifies the device. *)
let crypto_store ~env ~profile =
  with_xdg ~env @@ fun xdg -> Matrix_client.Crypto_store.create ~xdg ~profile

let base_store profile_store =
  Store.on_disk ~dir:(Profile_store.dir profile_store)

let load_base_state profile_store ~user_id =
  let store = base_store profile_store in
  (store, Base_client.of_store store ~user_id ())

let joined_members_from_state state room_id =
  Base_client.state_events state room_id
  |> List.filter_map (fun (event : Store.state_event) ->
      if
        Matrix_proto.Event.Event_type.equal event.event_type
          Matrix_proto.Event.Event_type.Room_member
        && Matrix_proto.Json.find_string "membership" event.content
           = Some "join"
      then Result.to_option (Matrix_proto.Id.User_id.of_string event.state_key)
      else None)

(** Restore this device's encryption machine from the profile, or generate a
    fresh Olm account if it has none. *)
let encryption ~env ~profile (session : Session.Session_file.t) =
  let store = crypto_store ~env ~profile in
  try
    Matrix_eio.Encryption.of_env env ~user_id:session.server.user_id
      ~device_id:session.auth.device_id ~store ()
  with Eio.Io (Matrix_eio.Error.E err, _) ->
    Logs.err (fun m ->
        m "Cannot read the stored crypto state: %a" Matrix_eio.Error.pp_err err);
    Logs.err (fun m ->
        m
          "Refusing to overwrite it — that would lose the ability to read this \
           device's history.");
    exit Cmd.exit_internal

(** Publish whatever the machine wants published — device keys on first run,
    one-time keys whenever the pool is low — and write it back to disk. *)
let publish_keys enc client =
  guard "Publishing keys" (fun () ->
      Matrix_eio.Encryption.execute_requests enc client
        (Matrix_eio.Encryption.outgoing_requests enc);
      Matrix_eio.Encryption.save enc)

(** An Ed25519 key in the groups of four a person reads out loud. *)
let fingerprint key =
  let b = Buffer.create (String.length key + 16) in
  String.iteri
    (fun i c ->
      if i > 0 && i mod 4 = 0 then Buffer.add_char b ' ';
      Buffer.add_char b c)
    key;
  Buffer.contents b

let save_login_or_exit ~env store file =
  match Profile_store.save_login store ~clock:env#clock file with
  | Ok () -> ()
  | Error e ->
      Logs.err (fun m ->
          m "Cannot write the session: %a" Matrix_client.Error.pp e);
      exit Cmd.exit_internal

let update_or_exit store update =
  match Profile_store.update_session store update with
  | Ok () -> ()
  | Error e ->
      Logs.err (fun m ->
          m "Cannot update the session: %a" Matrix_client.Error.pp e);
      exit Cmd.exit_internal

(** Persist a freshly obtained session under [profile].

    Both login flows end here, so an OAuth session is stored exactly like a
    password one — including its [refresh_token], which OAuth always issues and
    which the legacy flow issues only when asked. *)
let save_login ~env ~profile ~homeserver ?(method_ = Session.Auth.Matrix)
    ?access_token_expires_at (session : Matrix_client.Client.session) =
  with_xdg ~env @@ fun xdg ->
  let store = Profile_store.create ~xdg ~profile in
  let now = Ptime_clock.now () in
  let file : Session.Session_file.t =
    {
      server = { homeserver; user_id = session.user_id };
      auth =
        {
          access_token = session.access_token;
          device_id = session.device_id;
          refresh_token = session.refresh_token;
          access_token_expires_at;
          method_;
        };
      sync = { next_batch = None; filter_id = None };
      metadata =
        { created_at = now; last_used_at = now; client_name = app_name };
    }
  in
  save_login_or_exit ~env store file;
  Logs.app (fun m -> m "Session saved to profile '%s'" profile);
  Logs.app (fun m ->
      m "User ID: %s" (Matrix_proto.Id.User_id.to_string session.user_id));
  Logs.app (fun m ->
      m "Device ID: %s" (Matrix_proto.Id.Device_id.to_string session.device_id))

(** The legacy [m.login.password] flow. *)
let password_login ~sw ~env ~homeserver ~policy ~username ~password () =
  let require name = function
    | Some v -> v
    | None ->
        Logs.err (fun m -> m "Password login needs %s" name);
        Logs.err (fun m -> m "Pass it, or use --oauth for browser login");
        exit Cmd.exit_usage
  in
  let user = require "--username" username in
  let password = require "--password-file (or MATRIX_PASSWORD)" password in
  let client, access_token_expires_at =
    try
      Matrix_eio.login_password_with_expiry ~sw ~env ~homeserver ~user ~password
        ~fetch:(http_client ~env ~homeserver policy)
        ~request_refresh_token:true ()
    with Eio.Io (Matrix_eio.Error.E err, _) ->
      Logs.err (fun m -> m "Login failed: %a" Matrix_eio.Error.pp_err err);
      exit Cmd.exit_auth
  in
  match Matrix_eio.Client.session client with
  | Some session -> (session, access_token_expires_at)
  | None ->
      Logs.err (fun m -> m "Login returned no session");
      exit Cmd.exit_auth

(** The OAuth 2.0 authorisation code flow (Matrix 1.15).

    The homeserver's authorisation server is opened in a browser; omatrix
    listens on a loopback port for the redirect, exchanges the code and stores
    the resulting tokens. *)
let oauth_login ~sw ~env ~homeserver ~policy ~client_id () =
  (* Share this one policy-configured transport with the client-server calls
     and the off-origin OAuth issuer/registration requests. *)
  let http = http_client ~env ~homeserver policy in
  let client = Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch:http () in
  let open_url url =
    Logs.app (fun m -> m "Open this URL in a browser to authorise omatrix:");
    Logs.app (fun m -> m "");
    Logs.app (fun m -> m "  %s" url);
    Logs.app (fun m -> m "");
    Logs.app (fun m -> m "Waiting for the browser to come back...");
    Matrix_eio.Oauth.browser_opener ~sw ~env url
  in
  try
    Matrix_eio.Oauth.login_with_browser_full_expiry ~env ~http ?client_id client
      ~open_url ()
  with
  | Eio.Io (Matrix_eio.Oauth.E err, _) ->
      Logs.err (fun m -> m "OAuth login failed: %a" Matrix_eio.Oauth.pp_err err);
      exit Cmd.exit_auth
  | Eio.Io (Matrix_eio.Error.E err, _) ->
      Logs.err (fun m -> m "OAuth login failed: %a" Matrix_eio.Error.pp_err err);
      exit Cmd.exit_auth

let oauth_device_login ~sw ~env ~homeserver ~policy ~client_id () =
  (* OAuth discovery, dynamic registration and device polling may use an
     issuer origin different from the homeserver, so reuse the same backend. *)
  let http = http_client ~env ~homeserver policy in
  let client = Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch:http () in
  let show authorization =
    Logs.app (fun m -> m "Complete OAuth device authorisation:");
    Option.iter
      (fun uri -> Logs.app (fun m -> m "  Open: %s" (Uriz.to_string uri)))
      authorization
        .Matrix_eio.Oauth.Device_authorization.verification_uri_complete;
    Logs.app (fun m ->
        m "  User code: %s"
          authorization.Matrix_eio.Oauth.Device_authorization.user_code);
    Logs.app (fun m ->
        m "  Verification URL: %s"
          (Uriz.to_string
             authorization
               .Matrix_eio.Oauth.Device_authorization.verification_uri));
    Logs.app (fun m -> m "Waiting for authorisation...")
  in
  try
    Matrix_eio.Oauth.login_with_device_expiry ~env ~http ?client_id client ~show
      ()
  with
  | Eio.Io (Matrix_eio.Oauth.E err, _) ->
      Logs.err (fun m ->
          m "OAuth device login failed: %a" Matrix_eio.Oauth.pp_err err);
      exit Cmd.exit_auth
  | Eio.Io (Matrix_eio.Error.E err, _) ->
      Logs.err (fun m ->
          m "OAuth device login failed: %a" Matrix_eio.Error.pp_err err);
      exit Cmd.exit_auth

type login_mode = Legacy | OAuth_browser | OAuth_device

let select_login_mode ~oauth ~device_code =
  match (oauth, device_code) with
  | true, true -> Error "--oauth and --device-code cannot be used together"
  | true, false -> Ok OAuth_browser
  | false, true -> Ok OAuth_device
  | false, false -> Ok Legacy

let login_run ~homeserver ~username ~password ~oauth ~device_code ~client_id
    ~policy ~profile () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Logs.info (fun m -> m "Connecting to %s" (Uriz.to_string homeserver));

  let mode =
    match select_login_mode ~oauth ~device_code with
    | Ok mode -> mode
    | Error message ->
        Logs.err (fun m -> m "%s" message);
        exit Cmd.exit_usage
  in
  let session =
    match mode with
    | OAuth_browser ->
        let login = oauth_login ~sw ~env ~homeserver ~policy ~client_id () in
        ( login.session,
          Session.Auth.OAuth { client_id = login.client_id },
          login.expires_at )
    | OAuth_device ->
        let login =
          oauth_device_login ~sw ~env ~homeserver ~policy ~client_id ()
        in
        ( login.session,
          Session.Auth.OAuth { client_id = login.client_id },
          login.expires_at )
    | Legacy ->
        if client_id <> None then
          Logs.warn (fun m ->
              m "--client-id is only used with --oauth or --device-code");
        let session, expires_at =
          password_login ~sw ~env ~homeserver ~policy ~username ~password ()
        in
        (session, Session.Auth.Matrix, expires_at)
  in
  let session, method_, access_token_expires_at = session in

  Logs.info (fun m ->
      m "Logged in as %s" (Matrix_proto.Id.User_id.to_string session.user_id));

  save_login ~env ~profile ~homeserver ~method_ ?access_token_expires_at session;
  `Ok ()

let oauth_term =
  let doc =
    "Authenticate with OAuth 2.0 in a web browser (Matrix 1.15) instead of \
     sending a password. omatrix opens the homeserver's authorisation page and \
     listens on a loopback port for the redirect."
  in
  Arg.(value & flag & info [ "oauth" ] ~doc)

let device_code_term =
  let doc =
    "Authenticate with OAuth 2.0 using a device code instead of a browser. \
     Prints a user code and verification URL, then polls until authorization \
     completes."
  in
  Arg.(value & flag & info [ "device-code" ] ~doc)

let client_id_term =
  let doc =
    "Use a pre-registered OAuth $(docv) instead of registering dynamically. \
     Applies to $(b,--oauth), $(b,--device-code), and QR login."
  in
  Arg.(value & opt (some string) None & info [ "client-id" ] ~docv:"ID" ~doc)

let login_term =
  let run () homeserver username password oauth device_code client_id policy
      profile =
    login_run ~homeserver ~username ~password ~oauth ~device_code ~client_id
      ~policy ~profile ()
  in
  Term.(
    ret
      (const run $ Cmd.verbosity_term $ Cmd.homeserver_term
     $ Cmd.username_opt_term $ Cmd.password_opt_term $ oauth_term
     $ device_code_term $ client_id_term $ Cmd.http_policy_term
     $ Cmd.profile_term))

let login_cmd =
  let doc = "Authenticate with a Matrix homeserver" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Logs in to a Matrix homeserver and stores the session credentials for \
         later use. The session is saved to the profile directory under \
         $(b,\\$XDG_DATA_HOME/matrix/profiles/).";
      `P
        "Three flows are available. By default omatrix uses the legacy \
         password flow and needs $(b,--username) and $(b,--password). With \
         $(b,--oauth) it uses the OAuth 2.0 authorisation code flow introduced \
         in Matrix 1.15: it prints an authorisation URL (and tries to open it \
         in your browser), waits for the redirect on a loopback port, and \
         never sees your password. With $(b,--device-code), it prints a user \
         code and verification URL for a headless device flow, then polls \
         until the user authorises it. The OAuth flows never see your \
         password.";
      `S Manpage.s_examples;
      `Pre "  omatrix login -s https://matrix.org -u @you:matrix.org";
      `P "Using environment variables:";
      `Pre "  export MATRIX_PASSWORD=secret";
      `Pre "  omatrix login -s https://matrix.org -u @you:matrix.org";
      `P "Browser-based OAuth 2.0 login:";
      `Pre "  omatrix login --oauth -s https://matrix.org";
      `P "Headless OAuth 2.0 device-code login:";
      `Pre "  omatrix login --device-code -s https://matrix.org";
      `P "OAuth 2.0 with a client id provisioned out of band:";
      `Pre
        "  omatrix login --oauth --client-id s6BhdRkqt3 -s https://matrix.org";
      `P "Using a named profile:";
      `Pre
        "  omatrix login --profile work -s https://work.matrix.org -u \
         @you:work.matrix.org";
    ]
  in
  let info = Cmdliner.Cmd.info "login" ~doc ~man in
  Cmdliner.Cmd.v info login_term

let logout_run ~profile ~keep_local ~policy () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match load_session ~env ~profile with
  | None ->
      Logs.warn (fun m -> m "No session found for profile '%s'" profile);
      `Ok ()
  | Some (store, session) ->
      if not keep_local then begin
        Logs.info (fun m -> m "Logging out from server...");
        let client, _current =
          client_from_session ~sw ~env ~policy ~store session
        in
        try
          Matrix_eio.Auth.logout_session client session.auth;
          Logs.info (fun m -> m "Server logout successful")
        with Eio.Io _ ->
          Logs.warn (fun m ->
              m "Server logout failed (session may still be active)")
      end;

      Profile_store.clear store;
      Logs.app (fun m -> m "Session cleared for profile '%s'" profile);
      `Ok ()

let keep_local_term =
  let doc = "Only clear local session without notifying the server." in
  Arg.(value & flag & info [ "local" ] ~doc)

let logout_term =
  let run () profile keep_local policy =
    logout_run ~profile ~keep_local ~policy ()
  in
  Term.(
    ret
      (const run $ Cmd.verbosity_term $ Cmd.profile_term $ keep_local_term
     $ Cmd.http_policy_term))

let logout_cmd =
  let doc = "Log out and clear stored session" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Logs out from the Matrix homeserver and clears the stored session. By \
         default, this invalidates the access token on the server.";
      `S Manpage.s_examples;
      `Pre "  omatrix logout";
      `P "Clear only the local session (keep server session active):";
      `Pre "  omatrix logout --local";
      `P "Logout from a specific profile:";
      `Pre "  omatrix logout --profile work";
    ]
  in
  let info = Cmdliner.Cmd.info "logout" ~doc ~man in
  Cmdliner.Cmd.v info logout_term

let whoami_run ~profile () =
  Eio_main.run @@ fun env ->
  match load_session ~env ~profile with
  | None ->
      log_no_session ~profile;
      `Error (false, "Not logged in")
  | Some (_store, session) ->
      let user_id = Matrix_proto.Id.User_id.to_string session.server.user_id in
      let device_id =
        Matrix_proto.Id.Device_id.to_string session.auth.device_id
      in
      let homeserver = Uriz.to_string session.server.homeserver in

      Format.printf "Profile: %s@." profile;
      Format.printf "User ID: %s@." user_id;
      Format.printf "Device ID: %s@." device_id;
      Format.printf "Homeserver: %s@." homeserver;
      Format.printf "Last used: %a@." (Ptime.pp_rfc3339 ())
        session.metadata.last_used_at;
      `Ok ()

let whoami_term =
  let run () profile = whoami_run ~profile () in
  Term.(ret (const run $ Cmd.verbosity_term $ Cmd.profile_term))

let whoami_cmd =
  let doc = "Show current session information" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Displays information about the currently stored session, including \
         the user ID, device ID, and homeserver.";
      `S Manpage.s_examples;
      `Pre "  omatrix whoami";
      `Pre "  omatrix whoami --profile work";
    ]
  in
  let info = Cmdliner.Cmd.info "whoami" ~doc ~man in
  Cmdliner.Cmd.v info whoami_term

let msg_run ~profile ~room ~recipient ~message ~encrypted ~policy () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let store, session =
    match load_session ~env ~profile with
    | None ->
        log_no_session ~profile;
        exit Cmd.exit_auth
    | Some s -> s
  in

  let client, current_session =
    client_from_session ~sw ~env ~policy ~store session
  in
  let user_id = Matrix_eio.Client.user_id client in
  let state_store, initial_base_state = load_base_state store ~user_id in
  let base_state = ref initial_base_state in
  Logs.info (fun m ->
      m "Using session for %s" (Matrix_proto.Id.User_id.to_string user_id));

  let room_id =
    match (room, recipient) with
    | Some room_id, None -> room_id
    | None, Some recipient_id -> (
        Logs.info (fun m ->
            m "Finding or creating DM room with %s"
              (Matrix_proto.Id.User_id.to_string recipient_id));
        let encrypted_opt = if encrypted then Some true else None in
        try
          Matrix_eio.Account_data.get_or_create_dm client ~user_id:recipient_id
            ?encrypted:encrypted_opt ()
        with Eio.Io (Matrix_eio.Error.E err, _) ->
          Logs.err (fun m ->
              m "Failed to get/create DM room: %a" Matrix_eio.Error.pp_err err);
          exit Cmd.exit_network)
    | Some _, Some _ ->
        Logs.err (fun m -> m "Cannot specify both --room and --to");
        exit Cmd.exit_usage
    | None, None ->
        Logs.err (fun m -> m "Must specify --room or --to");
        exit Cmd.exit_usage
  in

  Logs.info (fun m ->
      m "Sending to room %s" (Matrix_proto.Id.Room_id.to_string room_id));

  let cached_room () = Base_client.find_room !base_state room_id in
  let cached_members_complete () =
    match cached_room () with
    | Some info -> info.membership = Base_client.Joined && info.members_complete
    | None -> false
  in
  let fetched_members = ref None in
  let room_members () =
    match !fetched_members with
    | Some members -> members
    | None ->
        let members = Matrix_eio.Rooms.get_members client ~room_id () in
        fetched_members := Some members;
        let updated = Base_client.replace_members !base_state room_id members in
        if updated != !base_state then begin
          base_state := updated;
          Base_client.persist state_store updated;
          Matrix_eio.Error.unwrap (Store.flush state_store)
        end;
        members
  in
  let active_members () =
    if cached_members_complete () then Base_client.members !base_state room_id
    else
      room_members ()
      |> List.filter_map (fun (member : Matrix_eio.Rooms.member) ->
          match member.membership with
          | Matrix_proto.Event.Membership.Join | Invite -> Some member.user_id
          | Leave | Ban | Knock -> None)
  in
  let joined_members () =
    if cached_members_complete () then
      joined_members_from_state !base_state room_id
    else
      guard "Listing the room's members" (fun () ->
          room_members ()
          |> List.filter_map (fun (member : Matrix_eio.Rooms.member) ->
              match member.membership with
              | Matrix_proto.Event.Membership.Join -> Some member.user_id
              | Invite | Leave | Ban | Knock -> None))
  in

  (* [--to] only asks the homeserver to invite the recipient; Matrix does not
     require an invitee to exist, so a typo'd or unknown user id gives no
     error there, and a room found in [m.direct] may be one the recipient
     since left. Either way the message would go unread, so this is worth a
     warning even though it never blocks the send. *)
  (match recipient with
  | None -> ()
  | Some recipient_id -> (
      try
        let is_member =
          List.exists
            (Matrix_proto.Id.User_id.equal recipient_id)
            (active_members ())
        in
        if not is_member then
          Logs.warn (fun m ->
              m
                "%s is not a member of this room; the invite may have failed, \
                 or the user may not exist, and the message may never be read"
                (Matrix_proto.Id.User_id.to_string recipient_id))
      with Eio.Io _ -> ()));

  (* Is the room encrypted? The room's own state is the authority — not
     --encrypted, which only says how to create a new DM. A room that says
     m.room.encryption must never be written to in the clear. *)
  let room_encryption =
    match cached_room () with
    | Some { encryption = Some settings; _ } -> Some settings
    | Some { encryption = None; encryption_state_complete = true; _ } -> None
    | Some { encryption = None; encryption_state_complete = false; _ } | None
      -> (
        try
          Some
            (Matrix_eio.State.get_state_event client ~room_id
               ~event_type:Matrix_proto.Event.Event_type.Room_encryption
               ~state_key:"" ())
        with
        | Eio.Io
            ( Matrix_eio.Error.E
                ( Matrix_eio.Error.Matrix
                    { errcode = Matrix_client.Error.M_NOT_FOUND; _ }
                | Http { status = 404; _ } ),
              _ ) ->
            None
        | Eio.Io (Matrix_eio.Error.E err, _) ->
            Logs.err (fun m ->
                m "Failed to inspect room encryption: %a"
                  Matrix_eio.Error.pp_err err);
            exit Cmd.exit_network)
  in

  let event_id =
    match room_encryption with
    | None -> (
        try Matrix_eio.Messages.send_text client ~room_id ~body:message ()
        with Eio.Io (Matrix_eio.Error.E err, _) ->
          Logs.err (fun m ->
              m "Failed to send message: %a" Matrix_eio.Error.pp_err err);
          exit Cmd.exit_network)
    | Some settings ->
        Logs.info (fun m -> m "Room is encrypted; encrypting with Megolm");
        let enc = encryption ~env ~profile session in
        publish_keys enc client;
        guard "Reading the room's encryption settings" (fun () ->
            Matrix_eio.Encryption.set_room_encryption_settings enc room_id
              settings);
        (* A prior [omatrix sync] leaves a complete, durable member projection.
           Fall back to one authoritative member request for a new or partial
           room, and save that projection for the next command. *)
        let members = joined_members () in
        Matrix_eio.Encryption.track_users enc members;
        let id =
          guard "Failed to send message" (fun () ->
              Matrix_eio.Encryption.send_encrypted_text enc client room_id
                ~body:message ~members)
        in
        guard "Saving" (fun () -> Matrix_eio.Encryption.save enc);
        id
  in

  Logs.app (fun m ->
      m "Message sent (event ID: %s)"
        (Matrix_proto.Id.Event_id.to_string event_id));

  let last_used_at = Ptime_clock.now () in
  update_or_exit store (fun session ->
      let updated =
        { session with metadata = { session.metadata with last_used_at } }
      in
      current_session := updated;
      updated);

  `Ok ()

let msg_term =
  let run () profile room recipient message encrypted policy =
    msg_run ~profile ~room ~recipient ~message ~encrypted ~policy ()
  in
  Term.(
    ret
      (const run $ Cmd.verbosity_term $ Cmd.profile_term $ Cmd.room_opt_term
     $ Cmd.recipient_opt_term $ Cmd.message_term $ Cmd.encrypted_term
     $ Cmd.http_policy_term))

let msg_cmd =
  let doc = "Send a message to a room or user" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Sends a text message to a Matrix room or directly to another user. \
         Requires a stored session from $(b,omatrix login).";
      `P
        "For direct messages (using $(b,--to)), an existing DM room is reused \
         if one exists, otherwise a new room is created.";
      `S Manpage.s_examples;
      `P "Send a direct message:";
      `Pre "  omatrix msg -t @alice:matrix.org \"Hello Alice!\"";
      `P "Send to a room:";
      `Pre "  omatrix msg -r '!roomid:matrix.org' \"Hello room!\"";
      `P "Create encrypted DM (for new rooms):";
      `Pre "  omatrix msg -t @alice:matrix.org -e \"Secret message\"";
      `P "Using a different profile:";
      `Pre
        "  omatrix msg --profile work -t @colleague:work.org \"Meeting at 3pm\"";
    ]
  in
  let info = Cmdliner.Cmd.info "msg" ~doc ~man in
  Cmdliner.Cmd.v info msg_term

let keys_init_run ~profile ~policy () =
  with_client ~profile ~policy @@ fun ~env ~sw:_ ~store:_ ~session ~client ->
  let existed =
    Matrix_client.Crypto_store.exists (crypto_store ~env ~profile)
  in
  let enc = encryption ~env ~profile session in
  publish_keys enc client;
  let ed25519, curve25519 = Matrix_eio.Encryption.identity_keys enc in
  Format.printf "Profile:     %s@." profile;
  Format.printf "User ID:     %s@."
    (Matrix_proto.Id.User_id.to_string session.server.user_id);
  Format.printf "Device ID:   %s@."
    (Matrix_proto.Id.Device_id.to_string session.auth.device_id);
  Format.printf "Fingerprint: %s@."
    (fingerprint (Matrix_client.Crypto_key.Ed25519.Public.to_base64 ed25519));
  Format.printf "Identity:    %s@."
    (Matrix_client.Crypto_key.Curve25519.Public.to_base64 curve25519);
  Format.printf "@.%s@."
    (if existed then "Restored the existing device keys and republished them."
     else "Generated a new Olm account and published its device keys.");
  Format.printf
    "Read the fingerprint out to your other devices to verify this one, or run \
     'omatrix verify'.@.";
  `Ok ()

let keys_init_term =
  let run () profile policy = keys_init_run ~profile ~policy () in
  Term.(
    ret
      (const run $ Cmd.verbosity_term $ Cmd.profile_term $ Cmd.http_policy_term))

let keys_init_cmd =
  let doc = "Create or restore this device's encryption keys" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Generates an Olm account for the profile if it has none, publishes \
         its device keys and a pool of one-time keys to the homeserver, and \
         prints the device's Ed25519 fingerprint.";
      `P
        "The keys live in the profile directory beside the session, in \
         $(b,device.json), $(b,one_time_keys.json) and $(b,crypto_state.json). \
         Running this again on a profile that already has an account restores \
         it rather than replacing it: a new account would lose the ability to \
         read everything this device has already received.";
      `S Manpage.s_examples;
      `Pre "  omatrix keys init";
      `Pre "  omatrix keys init --profile work";
    ]
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "init" ~doc ~man) keys_init_term

let keys_cmd =
  let doc = "Manage end-to-end encryption keys" in
  let man =
    [
      `S Manpage.s_description;
      `P "Commands for this device's end-to-end encryption identity.";
    ]
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "keys" ~doc ~man)
    ~default [ keys_init_cmd ]

module Vs = Matrix_eio.Verification_service

(** Ask on the terminal. Anything but an explicit yes is a no, because the cost
    of a wrong yes is a device that reads everything from now on. *)
let ask question =
  Format.printf "%s" question;
  Format.print_flush ();
  let answer = try read_line () with End_of_file -> "" in
  match String.lowercase_ascii (String.trim answer) with
  | "y" | "yes" -> true
  | _ -> false

let show_prompt (p : Vs.prompt) =
  Format.printf "@.Verifying %s%s@.@."
    (Matrix_proto.Id.User_id.to_string p.their_user_id)
    (match p.their_device_id with
    | Some d -> " on device " ^ Matrix_proto.Id.Device_id.to_string d
    | None -> "");
  List.iter
    (fun (e : Vs.emoji) -> Format.printf "    %s  %s@." e.symbol e.description)
    p.emoji;
  let a, b, c = p.decimals in
  Format.printf "@.  or, in numbers: %d %d %d@.@." a b c

(* One long-poll sync per iteration, with the encryption machine and the
   verification driver both attached, so an incoming message is decrypted,
   routed to its flow and answered without anything further from here. *)
let verify_run ~profile ~listen ~target ~device ~timeout ~policy () =
  with_client ~profile ~policy @@ fun ~env ~sw:_ ~store ~session ~client ->
  let clock = Eio.Stdenv.clock env in
  let deadline = Eio.Time.now clock +. timeout in
  let enc = encryption ~env ~profile session in
  publish_keys enc client;
  let state_store = base_store store in
  let svc =
    Matrix_eio.Sync_service.of_store ~store:state_store
      ~user_id:session.server.user_id ()
  in
  let results = ref [] in
  let confirm p =
    show_prompt p;
    ask "Do they match? [y/N] "
  in
  let ver =
    Vs.create ~client ~encryption:enc ~confirm
      ~on_result:(fun r -> results := !results @ [ r ])
      ()
  in
  (* Bound both the server-side long poll and the whole request. The former
     avoids asking for a poll that is longer than the deadline, while the
     latter also covers connection and response processing time. *)
  let sync () =
    let remaining = deadline -. Eio.Time.now clock in
    if remaining <= 0. then false
    else
      try
        let () =
          guard "Sync failed" (fun () ->
              let timeout_ms =
                int_of_float (Float.min 30000. (remaining *. 1000.))
              in
              let params =
                { Matrix_client.Sync.default_params with timeout = timeout_ms }
              in
              Eio.Time.with_timeout_exn clock remaining (fun () ->
                  ignore
                    (Matrix_eio.Sync_service.sync_once client svc ~params
                       ~encryption:enc ~verification:ver ())))
        in
        true
      with Eio.Time.Timeout -> false
  in
  (* A first sync drains the to-device backlog and refreshes the device
     lists, so the request below goes to a device we actually know. *)
  if not (sync ()) then begin
    Logs.warn (fun m -> m "Gave up waiting for the other device");
    exit Cmd.exit_internal
  end;
  let accepted = Hashtbl.create 4 in
  let accept_incoming () =
    List.iter
      (fun s ->
        let id =
          Matrix_client.Verification.Transaction.id
            (Matrix_client.Verification.Flow.session_transaction s)
        in
        if
          (not (Hashtbl.mem accepted id))
          && Matrix_client.Verification.Flow.session_stage s
             = Matrix_client.Verification.Flow.Requested
        then begin
          Hashtbl.replace accepted id ();
          let who =
            Matrix_proto.Id.User_id.to_string
              (Matrix_client.Verification.Flow.session_their_user_id s)
          in
          Format.printf "@.%s wants to verify with you.@." who;
          if ask "Accept? [y/N] " then
            guard "Accepting" (fun () -> Vs.accept ver client s)
          else
            guard "Cancelling" (fun () ->
                Vs.cancel ver client s
                  Matrix_client.Verification.Cancel_code.User)
        end)
      (Vs.sessions ver)
  in
  if listen then Logs.app (fun m -> m "Waiting for a verification request...")
  else begin
    let their_user_id =
      match target with
      | Some u -> u
      | None ->
          Logs.err (fun m -> m "Give a user id to verify, or pass --listen");
          exit Cmd.exit_usage
    in
    (* Their device list has to be current before a request can name a
       device, and before the SAS can look up the key it will MAC. *)
    Matrix_eio.Encryption.track_users enc [ their_user_id ];
    guard "Fetching device keys" (fun () ->
        Matrix_eio.Encryption.execute_requests enc client
          (Matrix_eio.Encryption.outgoing_requests enc));
    (match Matrix_eio.Encryption.devices_of enc their_user_id with
    | [] ->
        Logs.err (fun m ->
            m "%s has no devices we can see"
              (Matrix_proto.Id.User_id.to_string their_user_id));
        exit Cmd.exit_usage
    | _ -> ());
    (match device with
    | None -> ()
    | Some d -> (
        match
          Matrix_eio.Encryption.find_device enc their_user_id ~device_id:d
        with
        | Some _ -> ()
        | None ->
            Logs.err (fun m ->
                m "%s has no device %s"
                  (Matrix_proto.Id.User_id.to_string their_user_id)
                  (Matrix_proto.Id.Device_id.to_string d));
            exit Cmd.exit_usage));
    guard "Sending the request" (fun () ->
        ignore (Vs.request ver client ?device_id:device their_user_id));
    Logs.app (fun m ->
        m "Verification request sent to %s; waiting for them to accept..."
          (Matrix_proto.Id.User_id.to_string their_user_id))
  end;
  let rec loop () =
    if !results <> [] then ()
    else if not (sync ()) then
      Logs.warn (fun m -> m "Gave up waiting for the other device")
    else begin
      if listen then accept_incoming ();
      loop ()
    end
  in
  loop ();
  guard "Saving" (fun () -> Matrix_eio.Encryption.save enc);
  let ok =
    List.exists
      (function
        | Vs.Verified _ -> true
        | Vs.Publication_failed _ | Vs.Cancelled _ -> false)
      !results
  in
  List.iter
    (function
      | Vs.Verified { user_id; device_id } ->
          Format.printf "@.Verified %s%s.@."
            (Matrix_proto.Id.User_id.to_string user_id)
            (match device_id with
            | Some d -> " (" ^ Matrix_proto.Id.Device_id.to_string d ^ ")"
            | None -> "")
      | Vs.Publication_failed { user_id; reason; _ } ->
          Format.printf
            "@.Verification with %s succeeded, but publishing its signature \
             failed: %s@."
            (Matrix_proto.Id.User_id.to_string user_id)
            reason
      | Vs.Cancelled { user_id; code } ->
          Format.printf "@.Verification with %s was cancelled: %s@."
            (Matrix_proto.Id.User_id.to_string user_id)
            (Matrix_client.Verification.Cancel_code.reason code))
    !results;
  if ok then `Ok () else exit Cmd.exit_internal

let listen_term =
  let doc =
    "Wait for another device to ask to verify with this one, instead of \
     asking. Cannot be combined with a user id."
  in
  Arg.(value & flag & info [ "listen" ] ~doc)

let verify_target_term =
  let doc = "The user to verify with, as $(b,@user:server)." in
  Arg.(
    value & pos 0 (some Cmd.user_id_conv) None & info [] ~docv:"USER_ID" ~doc)

let verify_device_term =
  let doc =
    "Verify only this device of theirs. Without it every device the user has \
     is asked, and the first to answer takes the flow."
  in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"DEVICE_ID" ~doc)

let positive_finite_float_conv =
  let parse value =
    match float_of_string_opt value with
    | Some value when Float.is_finite value && value > 0. -> Ok value
    | _ -> Error (`Msg "must be a finite number greater than zero")
  in
  Arg.conv (parse, Format.pp_print_float)

let verify_timeout_term =
  let doc =
    "Maximum time to wait for verification, in seconds. Must be finite and \
     greater than zero."
  in
  Arg.(
    value
    & opt positive_finite_float_conv 120.
    & info [ "timeout" ] ~docv:"SECONDS" ~doc)

let verify_term =
  let run () profile listen target device timeout policy =
    let device =
      match device with
      | None -> None
      | Some d -> (
          match Matrix_proto.Id.Device_id.of_string d with
          | Ok id -> Some id
          | Error _ ->
              Logs.err (fun m -> m "Invalid device id '%s'" d);
              exit Cmd.exit_usage)
    in
    if listen && target <> None then begin
      Logs.err (fun m -> m "Give a user id or --listen, not both");
      exit Cmd.exit_usage
    end;
    verify_run ~profile ~listen ~target ~device ~timeout ~policy ()
  in
  Term.(
    ret
      (const run $ Cmd.verbosity_term $ Cmd.profile_term $ listen_term
     $ verify_target_term $ verify_device_term $ verify_timeout_term
     $ Cmd.http_policy_term))

let verify_cmd =
  let doc = "Verify another device with emoji (SAS)" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Runs the short authentication string protocol against another device. \
         Both sides are shown seven emoji; if they are the same on both \
         screens there is nobody in the middle, and each side then marks the \
         other's device verified.";
      `P
        "omatrix sends an $(b,m.key.verification.request) to the named user's \
         devices and then syncs until the flow finishes, printing the emoji \
         and asking whether they match. With $(b,--listen) it does the \
         opposite: it waits for somebody else to ask. It gives up after 120 \
         seconds by default; $(b,--timeout) changes that deadline.";
      `P
        "A verified device is the only one this client will ever forward room \
         keys to, and is what makes a decrypted message show as coming from a \
         device you trust.";
      `S Manpage.s_examples;
      `P "Verify another of your own devices:";
      `Pre "  omatrix verify @you:matrix.org";
      `P "Verify one specific device:";
      `Pre "  omatrix verify @you:matrix.org ABCDEFGHIJ";
      `P "Accept a request from somebody else:";
      `Pre "  omatrix verify --listen";
      `P "Wait longer for a slow or offline device:";
      `Pre "  omatrix verify --timeout 300 @you:matrix.org";
    ]
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "verify" ~doc ~man) verify_term

module Backup = Matrix_client.Backup

let backup_version_missing = function
  | Matrix_client.Error.Http_error { status = 404; _ } -> true
  | Matrix_client.Error.Matrix_error
      { errcode = Matrix_client.Error.M_NOT_FOUND; _ } ->
      true
  | _ -> false

let backup_enable_run ~profile ~policy () =
  with_client ~profile ~policy @@ fun ~env ~sw:_ ~store:_ ~session ~client ->
  let server_info =
    match
      Matrix_client.Room_keys.get_current_version
        (Matrix_eio.Client.base client)
    with
    | Ok info -> Some info
    | Error error when backup_version_missing error -> None
    | Error error ->
        Logs.err (fun m ->
            m "Fetching the current backup version failed: %a"
              Matrix_client.Error.pp error);
        exit Cmd.exit_network
  in
  let enc = encryption ~env ~profile session in
  match
    Backup.version_state
      ~server:
        (Option.map
           (fun info -> info.Matrix_client.Room_keys.version)
           server_info)
      ~local:(Matrix_eio.Encryption.backup_version enc)
  with
  | Backup.Server_only server ->
      Logs.err (fun m ->
          m
            "The homeserver already has backup version %s, but this device has \
             no matching local backup key. Run 'omatrix backup status' or \
             'omatrix backup restore' first."
            server);
      exit Cmd.exit_internal
  | Backup.Diverged { server; local } ->
      Logs.err (fun m ->
          m
            "The homeserver has backup version %s, while this device points to \
             local version %s. Run 'omatrix backup status' or 'omatrix backup \
             restore' first."
            server local);
      exit Cmd.exit_internal
  | Backup.Local_only local ->
      Logs.err (fun m ->
          m
            "The homeserver has no backup, but this device still points to \
             local version %s. Inspect the profile before enabling a new \
             backup."
            local);
      exit Cmd.exit_internal
  | Backup.Current version -> (
      let info = Option.get server_info in
      let local_key =
        (Matrix_eio.Encryption.snapshot enc).state.backup.encryption_key
      in
      match
        Backup.current_version_state ~algorithm:info.algorithm
          ~auth_data:info.auth_data ~local_key
      with
      | Backup.Compatible ->
          let uploaded =
            guard "Uploading room keys" (fun () ->
                Matrix_eio.Encryption.backup_pending enc client)
          in
          guard "Saving" (fun () -> Matrix_eio.Encryption.save enc);
          Format.printf "Backup version %s is already enabled and current.@."
            version;
          Format.printf "%d room key%s uploaded.@." uploaded
            (if uploaded = 1 then "" else "s");
          `Ok ()
      | Backup.Missing_local_key ->
          Logs.err (fun m ->
              m
                "Backup version %s matches by identifier, but this device has \
                 no persisted public backup key. Refusing to upload."
                version);
          exit Cmd.exit_internal
      | Backup.Unsupported_algorithm algorithm ->
          Logs.err (fun m ->
              m
                "Backup version %s uses unsupported algorithm %s. Refusing to \
                 upload."
                version algorithm);
          exit Cmd.exit_internal
      | Backup.Malformed_auth_data reason ->
          Logs.err (fun m ->
              m
                "Backup version %s has invalid authentication data (%s). \
                 Refusing to upload."
                version reason);
          exit Cmd.exit_internal
      | Backup.Different_public_key ->
          Logs.err (fun m ->
              m
                "Backup version %s has a different public key from this \
                 device's persisted key. Refusing to upload."
                version);
          exit Cmd.exit_internal)
  | Backup.Absent ->
      publish_keys enc client;
      let random =
        Matrix_client.Client.random (Matrix_eio.Client.base client)
      in
      let key = Backup.Decryption_key.generate ~random in
      (* Sign the auth data with this device's Ed25519 key, so another device
         can tell which device made it before trusting it. *)
      let auth_data =
        Matrix_eio.Encryption.sign enc
          (Backup.auth_data_to_json
             { public_key = Backup.Decryption_key.public key; signatures = [] })
      in
      let version =
        guard "Creating the backup version" (fun () ->
            Matrix_eio.Room_keys.create_version client
              ~algorithm:Backup.backup_algorithm ~auth_data)
      in
      Matrix_eio.Encryption.enable_backup enc ~version ~decryption_key:key
        (Backup.Decryption_key.public key);
      let uploaded =
        guard "Uploading room keys" (fun () ->
            Matrix_eio.Encryption.backup_pending enc client)
      in
      guard "Saving" (fun () -> Matrix_eio.Encryption.save enc);
      let recovery = Backup.Recovery_key.encode key in
      Format.printf "Backup version %s created.@." version;
      Format.printf "%d room key%s uploaded.@." uploaded
        (if uploaded = 1 then "" else "s");
      Format.printf "@.Recovery key:@.@.    %s@.@." recovery;
      Format.printf
        "Write this down. It is the only thing that can read the backup, it is \
         not stored on the server, and this is the only time omatrix will show \
         it.@.";
      `Ok ()

let backup_status_run ~profile ~policy () =
  with_client ~profile ~policy @@ fun ~env ~sw:_ ~store:_ ~session ~client ->
  let enc = encryption ~env ~profile session in
  (match
     try Some (Matrix_eio.Room_keys.get_current_version client)
     with Eio.Io _ -> None
   with
  | None -> Format.printf "Server:  no backup@."
  | Some (info : Matrix_client.Room_keys.version_info) ->
      Format.printf "Server:  version %s, %d key%s@." info.version info.count
        (if info.count = 1 then "" else "s");
      Format.printf "         algorithm %s, etag %s@." info.algorithm info.etag);
  (match Matrix_eio.Encryption.backup_version enc with
  | None ->
      Format.printf "Local:   not backing up@.";
      Format.printf
        "@.Run 'omatrix backup enable' to start, or 'omatrix backup restore \
         --recovery-key-file FILE' to read an existing backup.@."
  | Some v ->
      let pending = Matrix_eio.Encryption.backup_pending_count enc in
      Format.printf "Local:   backing up to version %s@." v;
      Format.printf "Pending: %d session%s not yet uploaded@." pending
        (if pending = 1 then "" else "s"));
  `Ok ()

let read_backup_recovery_key path =
  let contents =
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))
    with Sys_error _ | End_of_file ->
      Logs.err (fun m -> m "Cannot read the recovery key file");
      exit Cmd.exit_usage
  in
  if String.trim contents = "" then begin
    Logs.err (fun m -> m "The recovery key file is empty");
    exit Cmd.exit_usage
  end;
  match Backup.Recovery_key.decode contents with
  | Ok key -> key
  | Error _ ->
      Logs.err (fun m -> m "The recovery key file does not contain a valid key");
      exit Cmd.exit_usage

let backup_restore_run ~profile ~recovery_key_file ~policy () =
  let key = read_backup_recovery_key recovery_key_file in
  with_client ~profile ~policy @@ fun ~env ~sw:_ ~store:_ ~session ~client ->
  let info =
    guard "Fetching the backup version" (fun () ->
        Matrix_eio.Room_keys.get_current_version client)
  in
  let public_key = Backup.Decryption_key.public key in
  let invalid =
    match
      Backup.current_version_state ~algorithm:info.algorithm
        ~auth_data:info.auth_data ~local_key:(Some public_key)
    with
    | Backup.Compatible -> None
    | Backup.Different_public_key ->
        Some "The recovery key does not match the server's current backup"
    | Backup.Unsupported_algorithm _ ->
        Some "The server's current backup uses an unsupported algorithm"
    | Backup.Malformed_auth_data _ ->
        Some "The server's current backup has invalid authentication data"
    | Backup.Missing_local_key -> Some "No recovery public key is available"
  in
  Option.iter
    (fun message ->
      Logs.err (fun m -> m "%s; refusing to restore" message);
      exit Cmd.exit_usage)
    invalid;
  let enc = encryption ~env ~profile session in
  Matrix_eio.Encryption.enable_backup enc ~version:info.version
    ~decryption_key:key public_key;
  let imported =
    guard "Restoring" (fun () ->
        Matrix_eio.Encryption.restore_from_backup enc client)
  in
  guard "Saving" (fun () -> Matrix_eio.Encryption.save enc);
  Format.printf "Imported %d room key%s from backup version %s.@." imported
    (if imported = 1 then "" else "s")
    info.version;
  `Ok ()

let backup_enable_cmd =
  let doc = "Create a key backup and upload this device's room keys" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Checks the homeserver before enabling a backup. With no existing \
         local or server backup, it generates a backup key, creates a new \
         version, uploads every room key this device holds, and prints the \
         recovery key. If the current server version matches the local one, it \
         only uploads pending keys and is safe to run again.";
      `P
        "The server stores only room keys encrypted to the backup's public \
         half. The recovery key is the private half; without it the backup is \
         unreadable, and omatrix prints it exactly once. If a server backup is \
         found without its matching local key, omatrix refuses to overwrite \
         it; use $(b,backup status) or $(b,backup restore) first.";
      `S Manpage.s_examples;
      `Pre "  omatrix backup enable";
    ]
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "enable" ~doc ~man)
    Term.(
      ret
        (const (fun () profile policy -> backup_enable_run ~profile ~policy ())
        $ Cmd.verbosity_term $ Cmd.profile_term $ Cmd.http_policy_term))

let backup_status_cmd =
  let doc = "Show the key backup's state" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Reports the backup version the homeserver holds and how many keys are \
         in it, and whether this device is uploading to it.";
      `S Manpage.s_examples;
      `Pre "  omatrix backup status";
    ]
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "status" ~doc ~man)
    Term.(
      ret
        (const (fun () profile policy -> backup_status_run ~profile ~policy ())
        $ Cmd.verbosity_term $ Cmd.profile_term $ Cmd.http_policy_term))

let recovery_key_file_term =
  let doc =
    "Read the recovery key printed by $(b,omatrix backup enable) from $(docv). \
     Whitespace is ignored. The key itself is never accepted as an argument."
  in
  Arg.(
    required
    & opt (some string) None
    & info [ "recovery-key-file" ] ~docv:"FILE" ~doc)

(* Collect rejected arguments so Cmdliner's generic parse error cannot echo a
   recovery key supplied using the former positional syntax. *)
let backup_restore_positional_term =
  Arg.(value & pos_all string [] & info [] ~docs:Manpage.s_none ~docv:"ARG")

let backup_restore_cmd =
  let doc = "Import room keys from the backup" in
  let man =
    [
      `S Manpage.s_synopsis;
      `P "$(mname) backup restore --recovery-key-file=FILE [OPTION]…";
      `S Manpage.s_description;
      `P
        "Reads the recovery key from $(b,--recovery-key-file), checks that it \
         matches the current backup's algorithm and public key, then downloads \
         the backup, decrypts it, and imports every session this device does \
         not already hold at an equal or lower message index. A session that \
         fails to decrypt is skipped, so one bad entry does not cost the rest.";
      `S Manpage.s_examples;
      `Pre "  omatrix backup restore --recovery-key-file recovery.key";
    ]
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "restore" ~doc ~man)
    Term.(
      ret
        (const (fun () profile recovery_key_file positional policy ->
             if positional <> [] then
               `Error
                 ( false,
                   "Positional arguments are not accepted; use \
                    --recovery-key-file FILE" )
             else backup_restore_run ~profile ~recovery_key_file ~policy ())
        $ Cmd.verbosity_term $ Cmd.profile_term $ recovery_key_file_term
        $ backup_restore_positional_term $ Cmd.http_policy_term))

let backup_cmd =
  let doc = "Manage the server-side room key backup" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "A key backup lets a new device read old encrypted messages. The \
         homeserver stores the room keys encrypted to a key it does not have, \
         so only somebody with the recovery key can read them.";
    ]
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "backup" ~doc ~man)
    ~default
    [ backup_enable_cmd; backup_status_cmd; backup_restore_cmd ]

let body_of content =
  match content with
  | Jsont.Object (o, _) -> (
      match Jsont.Json.find_mem "body" o with
      | Some (_, Jsont.String (s, _)) -> Some s
      | _ -> None)
  | _ -> None

let print_event ~room ~mark (e : Matrix_proto.Event.Raw_event.t) =
  let sender = Matrix_proto.Id.User_id.to_string e.sender in
  let ty = Matrix_proto.Event.Event_type.to_string e.type_ in
  match body_of e.content with
  | Some body -> Format.printf "%s [%s] %s: %s@." mark room sender body
  | None -> Format.printf "%s [%s] %s: <%s>@." mark room sender ty

let sync_count_action ~count responses =
  if count > 0 && responses >= count then Matrix_eio.Sync_service.Stop
  else Matrix_eio.Sync_service.Continue

exception Sync_signal

let with_sync_signals condition f =
  let handler =
    Sys.Signal_handle (fun _ -> Eio.Condition.broadcast condition)
  in
  let previous = ref [] in
  let install signal =
    let old = Sys.signal signal handler in
    previous := (signal, old) :: !previous
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (signal, old) -> Sys.set_signal signal old) !previous)
    (fun () ->
      install Sys.sigint;
      install Sys.sigterm;
      f ())

module Qr4108 = Matrix_eio.Qr_login.Msc4108
module QrSession = Qr4108.Application_eio.Session

let qr_credential_env = "MATRIX_SSSS_CREDENTIAL"

let qr_credential_file_term =
  let doc =
    Printf.sprintf
      "Read the SSSS passphrase or recovery key from $(docv). Without it the \
       credential is taken from $(b,%s). The value is never accepted as a \
       command-line argument."
      qr_credential_env
  in
  Arg.(
    value & opt (some file) None & info [ "credential-file" ] ~docv:"FILE" ~doc)

let read_qr_credential_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let contents = really_input_string ic (in_channel_length ic) in
      let n = String.length contents in
      if n > 0 && contents.[n - 1] = '\n' then String.sub contents 0 (n - 1)
      else contents)

let qr_credential ~file =
  match (file, Sys.getenv_opt qr_credential_env) with
  | Some path, _ -> read_qr_credential_file path
  | None, Some credential -> credential
  | None, None ->
      Logs.err (fun m ->
          m
            "QR grant needs an SSSS passphrase or recovery key; pass \
             --credential-file or set %s"
            qr_credential_env);
      exit Cmd.exit_usage

let print_qr_progress = function
  | QrSession.Establishing_channel ->
      Logs.app (fun m -> m "Establishing the QR secure channel...")
  | QrSession.Awaiting_check_code code ->
      Logs.app (fun m ->
          m "Check code: %02d (compare it with the other device)" code)
  | QrSession.Grant_oauth Qr4108.Application.Grant_starting ->
      Logs.app (fun m -> m "Starting device activation...")
  | QrSession.Grant_oauth
      (Qr4108.Application.Waiting_for_authorization { verification_uri }) ->
      Logs.app (fun m ->
          m "The new device needs authorisation at: %s"
            (Uriz.to_string verification_uri))
  | QrSession.Grant_oauth Qr4108.Application.Grant_syncing_secrets ->
      Logs.app (fun m -> m "Sending the device secrets...")
  | QrSession.Grant_oauth Qr4108.Application.Grant_done ->
      Logs.app (fun m -> m "Device activation completed.")
  | QrSession.OAuth _ -> Logs.app (fun m -> m "Running device login OAuth...")
  | QrSession.Secrets -> Logs.app (fun m -> m "Sending recovery secrets...")
  | QrSession.Trust_and_backup ->
      Logs.app (fun m -> m "Finishing device trust and backup setup...")
  | QrSession.Done -> Logs.app (fun m -> m "QR login completed.")

let qr_scan_base64 () =
  Logs.app (fun m ->
      m
        "Paste the MSC4108 QR payload as Base64, then press Enter (Ctrl-C to \
         cancel):");
  match read_line () with
  | exception End_of_file ->
      Logs.err (fun m -> m "No QR payload was provided");
      exit Cmd.exit_usage
  | line -> (
      match Qr4108.of_base64 (String.trim line) with
      | Ok code -> code
      | Error error ->
          Logs.err (fun m ->
              m "Invalid MSC4108 QR payload: %a" Qr4108.pp_codec_error error);
          exit Cmd.exit_usage)

let qr_timeout_term =
  let doc =
    "Maximum time to wait for the QR rendezvous and device activation, in \
     seconds. Must be finite and greater than zero."
  in
  Arg.(
    value
    & opt positive_finite_float_conv 300.
    & info [ "timeout" ] ~docv:"SECONDS" ~doc)

let qr_server_term =
  let uri_conv = Matrix_cli.uri_conv in
  let doc =
    "Matrix homeserver URL used by the new device and its MSC4108 rendezvous. \
     This option is required and is not read from the environment."
  in
  Arg.(
    required
    & opt (some uri_conv) None
    & info [ "server"; "s"; "homeserver" ] ~docv:"URL" ~doc)

let qr_recovery_passphrase_file_term =
  let doc =
    "Read the new device's SSSS passphrase from $(docv). The passphrase is \
     never accepted as a command-line argument. Without this option, a fresh \
     Base58 recovery key is printed once instead. Creating the store replaces \
     the account's default SSSS recovery key."
  in
  Arg.(
    value
    & opt (some file) None
    & info [ "recovery-passphrase-file" ] ~docv:"FILE" ~doc)

let read_qr_recovery_passphrase_file path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let contents = really_input_string ic (in_channel_length ic) in
        let length = String.length contents in
        if
          length >= 2
          && contents.[length - 2] = '\r'
          && contents.[length - 1] = '\n'
        then String.sub contents 0 (length - 2)
        else if length > 0 && contents.[length - 1] = '\n' then
          String.sub contents 0 (length - 1)
        else contents)
  with Sys_error error ->
    Logs.err (fun m -> m "Cannot read recovery passphrase file: %s" error);
    exit Cmd.exit_usage

let qr_profile_vacant ~env ~profile =
  match load_session ~env ~profile with
  | Some _ ->
      Logs.err (fun m ->
          m
            "Profile '%s' already has a session; choose a new profile for QR \
             login"
            profile);
      exit Cmd.exit_usage
  | None ->
      let store = crypto_store ~env ~profile in
      if Matrix_client.Crypto_store.exists store then begin
        Logs.err (fun m ->
            m
              "Profile '%s' already has device encryption state; choose a new \
               profile for QR login"
              profile);
        exit Cmd.exit_usage
      end

let qr_login_run ~homeserver ~profile ~client_id ~timeout
    ~recovery_passphrase_file ~policy () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  qr_profile_vacant ~env ~profile;
  let recovery_passphrase =
    Option.map read_qr_recovery_passphrase_file recovery_passphrase_file
  in
  Option.iter
    (fun passphrase ->
      if String.equal passphrase "" then begin
        Logs.err (fun m -> m "The recovery passphrase file is empty");
        exit Cmd.exit_usage
      end)
    recovery_passphrase;
  let client =
    Matrix_eio.Client.create ~sw ~env ~homeserver
      ~fetch:(http_client ~env ~homeserver policy)
      ()
  in
  let transport =
    Qr4108.Rendezvous.transport_of_client
      ~sleep:(fun seconds -> Eio.Time.sleep (Eio.Stdenv.clock env) seconds)
      (Matrix_eio.Client.base client)
  in
  let rendezvous_server =
    Uriz.with_path homeserver Matrix_eio.Qr_login.Msc4108.rendezvous_path
  in
  let display_qr code =
    match Qr4108.to_base64 code with
    | Ok payload ->
        Format.printf
          "@.MSC4108 QR payload (Base64; give this to the existing \
           device):@.%s@."
          payload
    | Error error ->
        raise
          (Matrix_eio.Error.err
             (Matrix_eio.Error.Json
                (Format.asprintf "cannot encode generated QR payload: %a"
                   Qr4108.pp_codec_error error)))
  in
  let run_nested () =
    Eio.Switch.run @@ fun qr_sw ->
    let condition = Eio.Condition.create () in
    let watcher_stop, stop_watcher = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
        match
          Eio.Fiber.first
            (fun () ->
              Eio.Condition.await_no_mutex condition;
              `Signal)
            (fun () ->
              Eio.Promise.await watcher_stop;
              `Done)
        with
        | `Signal -> Eio.Switch.fail qr_sw Sync_signal
        | `Done -> ());
    Fun.protect
      ~finally:(fun () -> Eio.Promise.resolve stop_watcher ())
      (fun () ->
        with_sync_signals condition (fun () ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) timeout (fun () ->
                QrSession.login ~env ~transport ~rendezvous_server
                  ~random:
                    (Matrix_client.Client.random
                       (Matrix_eio.Client.base client))
                  ~display_qr
                  ~confirm_check_code:(fun code ->
                    Format.printf "@.Check code: %02d@." code;
                    ask "Do the codes match? [y/N] ")
                  ?client_id
                  ~crypto_store:(crypto_store ~env ~profile)
                  ~on_authenticated:(fun session ~client_id ~expires_at ->
                    save_login ~env ~profile ~homeserver
                      ~method_:(Session.Auth.OAuth { client_id })
                      ?access_token_expires_at:expires_at session)
                  ~timeout ~on_progress:print_qr_progress client ())))
  in
  let result =
    try run_nested () with
    | Sync_signal ->
        Logs.warn (fun m -> m "QR login cancelled");
        exit Cmd.exit_ok
    | Eio.Time.Timeout ->
        Logs.err (fun m -> m "QR login timed out after %.1f seconds" timeout);
        exit Cmd.exit_network
    | Eio.Io (QrSession.Session_error error, _) ->
        Logs.err (fun m -> m "QR login failed: %a" QrSession.pp_error error);
        exit Cmd.exit_network
    | Eio.Io (Qr4108.Application_eio.Application_error error, _) ->
        Logs.err (fun m ->
            m "QR login application failed: %a"
              Qr4108.Application_eio.pp_application_error error);
        exit Cmd.exit_network
    | Eio.Io (Matrix_eio.Error.E error, _) ->
        Logs.err (fun m ->
            m "QR login failed: %a" Matrix_eio.Error.pp_err error);
        exit Cmd.exit_network
  in
  (* The authenticated session was saved by [on_authenticated], before the
     protocol imported secrets or created remote SSSS state. *)
  let backup_key =
    Option.map
      (fun (backup : Qr4108.Secrets.backup) -> backup.decryption_key)
      result.backup
  in
  let created =
    guard "Creating the recovery secret store" (fun () ->
        match recovery_passphrase with
        | None ->
            Matrix_eio.Secrets.create_recovery_store result.client
              ~random:
                (Matrix_client.Client.random
                   (Matrix_eio.Client.base result.client))
              ~private_identity:result.private_identity ?backup_key ()
        | Some passphrase ->
            Matrix_eio.Secrets.create_recovery_store result.client
              ~random:
                (Matrix_client.Client.random
                   (Matrix_eio.Client.base result.client))
              ~passphrase ~private_identity:result.private_identity ?backup_key
              ())
  in
  (match recovery_passphrase with
  | Some _ ->
      Logs.app (fun m ->
          m
            "SSSS recovery store created and protected by the supplied \
             passphrase; its recovery key is not printed.")
  | None ->
      Logs.app (fun m ->
          m
            "IMPORTANT: save this Base58 recovery key now. It is the only \
             recovery credential for the newly created default SSSS store:");
      Format.printf "%s@." created.recovery_key;
      Logs.app (fun m ->
          m
            "The new default SSSS store replaced any previous default; this \
             key will not be shown again."));
  `Ok ()

let qr_login_term =
  let run () homeserver profile client_id timeout recovery_passphrase_file
      policy =
    qr_login_run ~homeserver ~profile ~client_id ~timeout
      ~recovery_passphrase_file ~policy ()
  in
  Term.(
    ret
      (const run $ Cmd.verbosity_term $ qr_server_term $ Cmd.profile_term
     $ client_id_term $ qr_timeout_term $ qr_recovery_passphrase_file_term
     $ Cmd.http_policy_term))

let qr_login_cmd =
  let doc = "Log in a new device through experimental MSC4108 QR login" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Creates a textual Base64 MSC4108 QR payload for an existing device to \
         scan or paste into $(b,omatrix qr grant), confirms the secure-channel \
         check code, and runs the OAuth device login. The profile must be \
         unused; an existing session or device crypto state is never \
         overwritten.";
      `P
        "After login, the imported cross-signing and optional backup secrets \
         are written to a fresh encrypted SSSS store. This replaces the \
         account's default SSSS recovery key. Supply \
         $(b,--recovery-passphrase-file) to protect it with a passphrase; \
         otherwise the generated Base58 recovery key is printed once.";
      `P
        "QR raster rendering and image scanning remain caller-owned. This \
         command deliberately exchanges the QR payload as Base64 text.";
      `S Manpage.s_examples;
      `Pre "  omatrix qr login -s https://matrix.example.org";
    ]
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "login" ~doc ~man) qr_login_term

let qr_grant_run ~profile ~credential_file ~timeout ~policy () =
  with_client ~profile ~policy @@ fun ~env ~sw ~store:_ ~session ~client ->
  let enc = encryption ~env ~profile session in
  let credential = qr_credential ~file:credential_file in
  let secret_store =
    guard "Opening secret storage" (fun () ->
        Matrix_eio.Secrets.open_secret_store client ~credential)
  in
  let private_identity =
    guard "Importing cross-signing keys" (fun () ->
        Matrix_eio.Secrets.import_cross_signing secret_store ~encryption:enc)
  in
  (* A backup key may have been imported into the machine by SSSS.  Export it
     only when both halves are present; a device with write-only backup state
     must not claim to hand over a recovery key. *)
  guard "Importing the room-key backup secret" (fun () ->
      Matrix_eio.Secrets.import_backup secret_store ~encryption:enc);
  guard "Saving imported encryption state" (fun () ->
      Matrix_eio.Encryption.save enc);
  let backup =
    let state = (Matrix_eio.Encryption.snapshot enc).state.backup in
    match (state.version, state.decryption_key) with
    | Some backup_version, Some decryption_key ->
        Some Qr4108.Secrets.{ backup_version; decryption_key }
    | _ -> None
  in
  let transport =
    Qr4108.Rendezvous.transport_of_client
      ~sleep:(fun seconds -> Eio.Time.sleep (Eio.Stdenv.clock env) seconds)
      (Matrix_eio.Client.base client)
  in
  let authorize uri =
    Format.printf
      "@.Open this URL on the existing device to authorise it:@.%s@."
      (Uriz.to_string uri);
    if ask "Approve the device activation? [y/N] " then
      Qr4108.Application.Confirm
    else Qr4108.Application.Cancel
  in
  let run_nested () =
    Eio.Switch.run @@ fun qr_sw ->
    let condition = Eio.Condition.create () in
    let watcher_stop, stop_watcher = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
        match
          Eio.Fiber.first
            (fun () ->
              Eio.Condition.await_no_mutex condition;
              `Signal)
            (fun () ->
              Eio.Promise.await watcher_stop;
              `Done)
        with
        | `Signal -> Eio.Switch.fail qr_sw Sync_signal
        | `Done -> ());
    Fun.protect
      ~finally:(fun () -> Eio.Promise.resolve stop_watcher ())
      (fun () ->
        with_sync_signals condition (fun () ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) timeout (fun () ->
                QrSession.grant ~env ~transport
                  ~random:
                    (Matrix_client.Client.random
                       (Matrix_eio.Client.base client))
                  ~scan_qr:qr_scan_base64 ~expected_intent:Qr4108.Login
                  ~confirm_check_code:(fun code ->
                    Format.printf "@.Check code: %02d@." code;
                    ask "Do the codes match? [y/N] ")
                  ?backup ~authorize ~timeout ~on_progress:print_qr_progress
                  ~private_identity ~encryption:enc client ())))
  in
  (try run_nested () with
  | Sync_signal ->
      Logs.warn (fun m -> m "QR grant cancelled");
      exit Cmd.exit_ok
  | Eio.Time.Timeout ->
      Logs.err (fun m -> m "QR grant timed out after %.1f seconds" timeout);
      exit Cmd.exit_network
  | Eio.Io (QrSession.Session_error error, _) ->
      Logs.err (fun m -> m "QR grant failed: %a" QrSession.pp_error error);
      exit Cmd.exit_network
  | Eio.Io (Qr4108.Application_eio.Application_error error, _) ->
      Logs.err (fun m ->
          m "QR grant application failed: %a"
            Qr4108.Application_eio.pp_application_error error);
      exit Cmd.exit_network
  | Eio.Io (Matrix_eio.Error.E error, _) ->
      Logs.err (fun m -> m "QR grant failed: %a" Matrix_eio.Error.pp_err error);
      exit Cmd.exit_network);
  Logs.app (fun m -> m "The existing device granted access.");
  `Ok ()

let qr_grant_term =
  let run () profile credential_file timeout policy =
    qr_grant_run ~profile ~credential_file ~timeout ~policy ()
  in
  Term.(
    ret
      (const run $ Cmd.verbosity_term $ Cmd.profile_term
     $ qr_credential_file_term $ qr_timeout_term $ Cmd.http_policy_term))

let qr_grant_cmd =
  let doc = "Grant device access through experimental MSC4108 QR login" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Reads an MSC4108 QR payload as Base64 from standard input, confirms \
         the secure-channel check code, shows the OAuth approval URL and hands \
         the current profile's cross-signing and optional backup secrets to \
         the new device. The profile must have an SSSS passphrase or recovery \
         key available through --credential-file or MATRIX_SSSS_CREDENTIAL.";
      `P
        "This command intentionally accepts textual Base64 rather than \
         rendering or scanning images. Ctrl-C cancels the rendezvous and \
         attempts its cleanup.";
      `P
        "For safety, the rendezvous URL encoded in the payload must have the \
         same origin as the profile's homeserver. External rendezvous services \
         are rejected.";
      `S Manpage.s_examples;
      `Pre "  omatrix qr grant --credential-file ssss.key";
    ]
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "grant" ~doc ~man) qr_grant_term

let qr_cmd =
  let doc = "Use experimental MSC4108 QR login" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Experimental: full two-role OAuth/MAS validation is still pending. \
         The QR login command group provides `qr login` for a new device and \
         `qr grant` for the existing device. QR image rendering and scanning \
         remain outside these commands.";
    ]
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "qr" ~doc ~man)
    ~default
    [ qr_login_cmd; qr_grant_cmd ]

let sync_run ~profile ~count ~no_encryption ~policy () =
  with_client ~profile ~policy @@ fun ~env ~sw ~store ~session ~client ->
  let enc =
    if no_encryption then None else Some (encryption ~env ~profile session)
  in
  Option.iter (fun e -> publish_keys e client) enc;
  (* Keep the same durable room projection used by [omatrix msg].  In
     particular, a successful sync makes the joined-member snapshot available
     to a later one-shot send without another /members request. *)
  let state_store = base_store store in
  let svc =
    Matrix_eio.Sync_service.of_store ~store:state_store
      ~user_id:session.server.user_id ()
  in
  let show (changes : Matrix_client.Base_client.changes) =
    List.iter
      (fun (c : Matrix_client.Base_client.room_change) ->
        let room = Matrix_client.Base_client.display_name c.info in
        (* Substitute each plaintext for the ciphertext it came out of, so
           the timeline reads in order. Both lists key off the very event
           values in [timeline], so physical equality is the right test. *)
        let plain =
          List.map
            (fun (d : Matrix_client.Base_client.decrypted) ->
              (d.encrypted, d.plaintext))
            c.decrypted
        in
        List.iter
          (fun (e : Matrix_proto.Event.Raw_event.t) ->
            match List.assq_opt e plain with
            | Some p -> print_event ~room ~mark:"*" p
            | None -> (
                match List.assq_opt e c.undecrypted with
                | Some err ->
                    Format.printf "! [%s] %s: <undecryptable: %a>@." room
                      (Matrix_proto.Id.User_id.to_string e.sender)
                      Matrix_client.Encryption.pp_decrypt_error err
                | None -> print_event ~room ~mark:" " e))
          c.timeline)
      changes.room_changes
  in
  let outcome =
    Fun.protect
      ~finally:(fun () ->
        Option.iter
          (fun e ->
            Eio.Cancel.protect (fun () ->
                guard "Saving" (fun () -> Matrix_eio.Encryption.save e)))
          enc)
      (fun () ->
        let completion, resolve_completion = Eio.Promise.create () in
        let complete result =
          if not (Eio.Promise.is_resolved completion) then
            Eio.Promise.resolve resolve_completion result
        in
        let condition = Eio.Condition.create () in
        let responses = ref 0 in
        let on_error error =
          Logs.err (fun m -> m "Sync failed: %a" Matrix_eio.Error.pp_err error);
          complete `Error;
          Matrix_eio.Sync_service.Stop
        in
        let run_nested () =
          try
            Eio.Switch.run @@ fun sync_sw ->
            let watcher_stop, stop_watcher = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () ->
                match
                  Eio.Fiber.first
                    (fun () ->
                      Eio.Condition.await_no_mutex condition;
                      `Signal)
                    (fun () ->
                      Eio.Promise.await watcher_stop;
                      `Done)
                with
                | `Signal ->
                    complete `Signal;
                    Eio.Switch.fail sync_sw Sync_signal
                | `Done -> ());
            Fun.protect
              ~finally:(fun () -> Eio.Promise.resolve stop_watcher ())
              (fun () ->
                with_sync_signals condition (fun () ->
                    Matrix_eio.Sync_service.run ~sw:sync_sw
                      ~clock:(Eio.Stdenv.clock env) client svc ?encryption:enc
                      ~on_response:(fun _response ->
                        incr responses;
                        match sync_count_action ~count !responses with
                        | Matrix_eio.Sync_service.Stop ->
                            complete (`Count !responses);
                            Matrix_eio.Sync_service.Stop
                        | Matrix_eio.Sync_service.Continue ->
                            Matrix_eio.Sync_service.Continue
                        | Matrix_eio.Sync_service.Retry_after _ -> assert false)
                      ~on_error
                      ~on_change:(fun _state changes -> show changes)
                      ();
                    Eio.Promise.await completion))
          with
          | Sync_signal -> Eio.Promise.await completion
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | exn ->
              Logs.err (fun m ->
                  m "Sync stopped unexpectedly: %s" (Printexc.to_string exn));
              complete `Error;
              `Error
        in
        run_nested ())
  in
  match outcome with
  | `Count _ | `Signal -> `Ok ()
  | `Error -> exit Cmd.exit_network

let count_term =
  let doc =
    "Stop after $(docv) sync responses. The default, 0, is to keep syncing \
     until interrupted."
  in
  Arg.(value & opt int 0 & info [ "count"; "n" ] ~docv:"N" ~doc)

let no_encryption_term =
  let doc =
    "Do not load the encryption keys, and print encrypted events as ciphertext."
  in
  Arg.(value & flag & info [ "no-encryption" ] ~doc)

let sync_term =
  let run () profile count no_encryption policy =
    sync_run ~profile ~count ~no_encryption ~policy ()
  in
  Term.(
    ret
      (const run $ Cmd.verbosity_term $ Cmd.profile_term $ count_term
     $ no_encryption_term $ Cmd.http_policy_term))

let sync_cmd =
  let doc = "Follow the timeline, decrypting as it goes" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Runs the sync loop and prints each new timeline event as $(b,[room] \
         sender: body).";
      `P
        "With $(b,--count N), it stops after exactly N successful sync \
         responses. Without it, or with zero, it keeps running until you press \
         Ctrl-C; shutdown cancels the in-flight poll and saves the encryption \
         state.";
      `P
        "Encrypted rooms are decrypted with this device's keys, which is what \
         $(b,omatrix keys init) publishes; a decrypted event is marked with \
         $(b,*). An event whose Megolm session has not arrived is marked \
         $(b,!) — the key usually turns up in a later sync, or can be fetched \
         with $(b,omatrix backup restore).";
      `S Manpage.s_examples;
      `Pre "  omatrix sync";
      `P "One response and then stop:";
      `Pre "  omatrix sync --count 1";
    ]
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "sync" ~doc ~man) sync_term

let main_cmd =
  let doc = "Command-line Matrix client" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(b,omatrix) is a command-line client for the Matrix communication \
         protocol. It supports session persistence, allowing you to log in \
         once and perform subsequent operations without re-authenticating.";
      `S Manpage.s_commands;
      `P
        "$(b,login)   Authenticate with a homeserver and store session \
         (password, or $(b,--oauth) for the browser flow)";
      `P "$(b,logout)  Clear stored session and invalidate token";
      `P "$(b,whoami)  Show current session information";
      `P
        "$(b,msg)     Send a message to a room or user, encrypting when the \
         room is encrypted";
      `P "$(b,sync)    Follow the timeline, decrypting as it goes";
      `P "$(b,keys)    Create or restore this device's encryption keys";
      `P "$(b,verify)  Verify another device with emoji (SAS)";
      `P "$(b,qr)      Grant an existing device access through MSC4108";
      `P "$(b,backup)  Manage the server-side room key backup";
      `S "PROFILES";
      `P
        "$(b,omatrix) supports multiple profiles for managing different Matrix \
         accounts. Each profile stores its own session data independently.";
      `P
        "Session data is stored in $(b,\\$XDG_DATA_HOME/matrix/profiles/NAME/).";
      `P
        "The default profile is named 'default'. Use $(b,--profile NAME) to \
         use a different profile.";
      `S Manpage.s_environment;
      `I ("$(b,MATRIX_HOMESERVER)", "Default homeserver URL");
      `I ("$(b,MATRIX_USERNAME)", "Default username");
      `I ("$(b,MATRIX_PASSWORD)", "Password, or $(b,--password-file)");
      `I
        ( "$(b,MATRIX_SSSS_CREDENTIAL)",
          "SSSS passphrase or recovery key for $(b,qr grant), or use \
           $(b,--credential-file)" );
      `S Manpage.s_bugs;
      `P "Report bugs at <https://github.com/ocaml-matrix/ocaml-matrix/issues>.";
    ]
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  let info = Cmdliner.Cmd.info "omatrix" ~version:"0.1.0" ~doc ~man in
  Cmdliner.Cmd.group info ~default
    [
      login_cmd;
      logout_cmd;
      whoami_cmd;
      msg_cmd;
      sync_cmd;
      keys_cmd;
      verify_cmd;
      qr_cmd;
      backup_cmd;
    ]

let () = exit (Cmdliner.Cmd.eval main_cmd)
