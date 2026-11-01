module Id = Matrix_proto.Id
module Session = Matrix_client.Session
module Profile_store = Matrix_client.Profile_store
module Log = (val Logs.src_log Logging.src : Logs.LOG)

type clock = float Eio.Time.clock_ty Eio.Std.r

type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  clock : clock;
  client : Matrix_eio.Client.t;
  encryption : Matrix_eio.Encryption.t option;
  event_store : Matrix_ui.Event_store.t option;
  plugin_store : Plugin_store.t;
  profile_dir : Eio.Fs.dir_ty Eio.Path.t option;
}

type error =
  | Missing_credential of { profile : string; needs : string }
  | Session_unreadable of { profile : string; error : Matrix_client.Error.t }
  | Session_unwritable of Matrix_client.Error.t
  | Login_failed of string
  | Crypto_store of string
  | Key_upload of string
  | Event_store of { path : string; error : Matrix_ui.Event_store.Error.t }

let pp_error ppf = function
  | Missing_credential { profile; needs } ->
      Format.fprintf ppf
        "Profile '%s' has no stored session, so %s is needed to create one"
        profile needs
  | Session_unreadable { profile; error } ->
      Format.fprintf ppf
        "The session of profile '%s' is unreadable, and overwriting it would \
         lose this device's history: %a"
        profile Matrix_client.Error.pp error
  | Session_unwritable error ->
      Format.fprintf ppf "Cannot write the session: %a" Matrix_client.Error.pp
        error
  | Login_failed message -> Format.fprintf ppf "Login failed: %s" message
  | Crypto_store message ->
      Format.fprintf ppf
        "Cannot read the stored crypto state, and overwriting it would lose \
         this device's history: %s"
        message
  | Key_upload message ->
      Format.fprintf ppf "Publishing this device's keys: %s" message
  | Event_store { path; error } ->
      Format.fprintf ppf "Cannot open %s: %s" path
        (Matrix_ui.Event_store.Error.to_string error)

let error_to_string e = Format.asprintf "%a" pp_error e

let make ~env ~sw ~clock ~client ?encryption ?event_store ?plugin_store
    ?profile_dir () =
  {
    env;
    sw;
    clock;
    client;
    encryption;
    event_store;
    plugin_store =
      (match plugin_store with
      | Some store -> store
      | None -> Plugin_store.memory ());
    profile_dir;
  }

let v ~env ~sw ~client ?clock ?encryption ?event_store ?plugin_store () =
  let clock =
    match clock with Some clock -> clock | None -> Eio.Stdenv.clock env
  in
  make ~env ~sw ~clock ~client ?encryption ?event_store ?plugin_store ()

let profile_directory ~env ~profile =
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  Profile_store.dir (Profile_store.create ~xdg ~profile)

exception Refused of error

let refuse error = raise (Refused error)

let stored_session ~env ~profile =
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  match Profile_store.load_session (Profile_store.create ~xdg ~profile) with
  | Ok file -> file
  | Error error -> refuse (Session_unreadable { profile; error })

let save_session ~env ~profile ~homeserver
    (session : Matrix_client.Client.session) =
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
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
          access_token_expires_at = None;
          method_ = Matrix_client.Session.Auth.Matrix;
        };
      sync = { next_batch = None; filter_id = None };
      metadata =
        { created_at = now; last_used_at = now; client_name = "matrix.bot" };
    }
  in
  (match Profile_store.save_session store file with
  | Ok () -> ()
  | Error error -> refuse (Session_unwritable error));
  file

let log_in ~env ~sw ~profile ~homeserver ~username ~password =
  match stored_session ~env ~profile with
  | Some file ->
      Log.info (fun m ->
          m "Reusing the session of profile '%s' (%s, device %s)" profile
            (Id.User_id.to_string file.server.user_id)
            (Id.Device_id.to_string file.auth.device_id));
      file
  | None ->
      let require needs = function
        | Some value -> value
        | None -> refuse (Missing_credential { profile; needs })
      in
      let homeserver = require "--homeserver" homeserver in
      let user = require "--username" username in
      let password = require "--password-file (or MATRIX_PASSWORD)" password in
      let client =
        try Matrix_eio.login_password ~sw ~env ~homeserver ~user ~password ()
        with Eio.Io (Matrix_eio.Error.E _, _) as exn ->
          refuse (Login_failed (Format.asprintf "%a" Eio.Exn.pp exn))
      in
      let session =
        match Matrix_eio.Client.session client with
        | Some session -> session
        | None -> refuse (Login_failed "the homeserver returned no session")
      in
      Log.app (fun m ->
          m "Logged in as %s, device %s; the session is saved under '%s'"
            (Id.User_id.to_string session.user_id)
            (Id.Device_id.to_string session.device_id)
            profile);
      save_session ~env ~profile ~homeserver session

let client_of_session ~sw ~env (file : Session.Session_file.t) =
  let client =
    Matrix_eio.Client.create ~sw ~env ~homeserver:file.server.homeserver ()
  in
  Matrix_eio.Client.with_session client
    {
      user_id = file.server.user_id;
      device_id = file.auth.device_id;
      access_token = file.auth.access_token;
      refresh_token = file.auth.refresh_token;
    }

(* Device keys on the first run, one-time keys whenever the pool is low.
   Without this the bot is invisible to anyone trying to send it an
   encrypted message. *)
let open_encryption ~env ~profile (file : Session.Session_file.t) client =
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Matrix_client.Crypto_store.create ~xdg ~profile in
  let machine =
    try
      Matrix_eio.Encryption.of_env env ~user_id:file.server.user_id
        ~device_id:file.auth.device_id ~store ()
    with Eio.Io (Matrix_eio.Error.E _, _) as exn ->
      refuse (Crypto_store (Format.asprintf "%a" Eio.Exn.pp exn))
  in
  (try
     Matrix_eio.Encryption.execute_requests machine client
       (Matrix_eio.Encryption.outgoing_requests machine);
     Matrix_eio.Encryption.save machine
   with Eio.Io (Matrix_eio.Error.E _, _) as exn ->
     refuse (Key_upload (Format.asprintf "%a" Eio.Exn.pp exn)));
  machine

let connect ~env ~sw ~profile ?homeserver ?username ?password ?(encrypt = true)
    ?(persist_events = false) () =
  match
    let file = log_in ~env ~sw ~profile ~homeserver ~username ~password in
    let client = client_of_session ~sw ~env file in
    let encryption =
      if encrypt then Some (open_encryption ~env ~profile file client) else None
    in
    let dir = profile_directory ~env ~profile in
    let event_store =
      if not persist_events then None
      else
        let path = Eio.Path.native_exn Eio.Path.(dir / "events.sqlite3") in
        match
          Matrix_ui_sqlite.create ~plaintext_policy:Store_plaintext path
        with
        | Ok store -> Some store
        | Error error -> refuse (Event_store { path; error })
    in
    make ~env ~sw ~clock:(Eio.Stdenv.clock env) ~client ?encryption ?event_store
      ~plugin_store:(Plugin_store.open_file Eio.Path.(dir / "state.json"))
      ~profile_dir:dir ()
  with
  | context -> Ok context
  | exception Refused error -> Error error

let save t =
  Option.iter
    (fun machine ->
      try Matrix_eio.Encryption.save machine
      with Eio.Io _ as exn ->
        let exn =
          Eio.Exn.add_context exn "saving Matrix bot encryption state"
        in
        Log.warn (fun m -> m "Saving the crypto state: %a" Eio.Exn.pp exn))
    t.encryption

let env t = t.env
let switch t = t.sw
let clock t = t.clock
let client t = t.client
let user_id t = Matrix_eio.Client.user_id t.client
let encryption t = t.encryption
let event_store t = t.event_store
let plugin_store t = t.plugin_store
let profile_dir t = t.profile_dir
