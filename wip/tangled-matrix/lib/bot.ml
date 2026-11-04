(* SPDX-License-Identifier: ISC *)
module Me = Matrix_eio
module Mc = Matrix_client
module Id = Matrix_proto.Id
module Session = Mc.Session
module Cs = Mc.Cross_signing

type config = {
  user : string;
  password : string;
  homeserver : string option;
  recovery_key : string option;
  allow_unencrypted : bool;
}

type t = {
  client : Me.Client.t;
  user_id : Id.User_id.t;
  device_id : Id.Device_id.t;
  encryption : Me.Encryption.t;
  sync : Me.Sync_service.t;
  queue : Me.Send_queue.t;
  mutable synced : bool;
  allow_unencrypted : bool;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  waiting : (int, (string, string) result Eio.Promise.u) Hashtbl.t;
      (* Queue requests someone is waiting on, by queue-local ID. *)
}

let profile = "bot"
let user_id t = Id.User_id.to_string t.user_id
let device_id t = Id.Device_id.to_string t.device_id

let ok what = function
  | Ok x -> x
  | Error e -> Fmt.failwith "%s: %s" what (Mc.Error.to_string e)

let user_or_fail s =
  match Id.User_id.of_string s with
  | Ok u -> u
  | Error (`Msg m) -> Fmt.failwith "Not a Matrix user ID, %s: %s" s m

(* The client API of the bot's server, as Matrix clients find it. *)
let discover ~sw ~env config user_id =
  match config.homeserver with
  | Some h -> Uriz.of_string_exn h
  | None ->
      let server = Id.Server_name.to_string (Id.User_id.server_name user_id) in
      let c =
        Me.Client.create ~sw ~env
          ~homeserver:(Uriz.of_string_exn ("https://" ^ server))
          ()
      in
      (Me.Server.discover c).base_url

let session_file ~homeserver (s : Mc.Client.session) : Session.Session_file.t =
  let now = Ptime_clock.now () in
  {
    server = { homeserver; user_id = s.user_id };
    auth =
      {
        access_token = s.access_token;
        device_id = s.device_id;
        refresh_token = s.refresh_token;
        access_token_expires_at = None;
        method_ = Session.Auth.Matrix;
      };
    sync = { next_batch = None; filter_id = None };
    metadata =
      { created_at = now; last_used_at = now; client_name = "tangled-matrix" };
  }

let login ~sw ~env ~store config ~homeserver ?device () =
  let client = Me.Client.create ~sw ~env ~homeserver () in
  let params =
    {
      Me.Auth.device_id = Option.map Id.Device_id.to_string device;
      initial_device_display_name = Some "tangled-matrix";
    }
  in
  let client =
    Me.Auth.login_password client ~user:config.user ~password:config.password
      ~params ()
  in
  let session = Option.get (Mc.Client.session (Me.Client.base client)) in
  ok "Saving the Matrix session"
    (Mc.Profile_store.save_login store ~clock:env#clock
       (session_file ~homeserver session));
  client

(* A kept session is reused. One whose token the server has forgotten logs in
   again as the same device, which keeps its keys. *)
let session ~sw ~env ~store config =
  match
    ok "Reading the Matrix session" (Mc.Profile_store.load_session store)
  with
  | None ->
      let user_id = user_or_fail config.user in
      login ~sw ~env ~store config
        ~homeserver:(discover ~sw ~env config user_id)
        ()
  | Some (f : Session.Session_file.t) -> (
      let homeserver = f.server.homeserver in
      let client =
        Me.Client.with_session
          (Me.Client.create ~sw ~env ~homeserver ())
          {
            user_id = f.server.user_id;
            device_id = f.auth.device_id;
            access_token = f.auth.access_token;
            refresh_token = f.auth.refresh_token;
          }
      in
      match Me.Auth.whoami client with
      | _ -> client
      | exception
          Eio.Io (Me.Error.E (Matrix { errcode = M_UNKNOWN_TOKEN; _ }), _) ->
          Logs.warn (fun m ->
              m "The Matrix session expired, so logging in again");
          login ~sw ~env ~store config ~homeserver ~device:f.auth.device_id ())

let publish encryption client =
  Me.Encryption.execute_requests encryption client
    (Me.Encryption.outgoing_requests encryption);
  Me.Encryption.save encryption

let write_private path data =
  ok "Writing a private file" (Mc.Profile_store.atomic_write ~path ~data)

(* The device is verified by signing it with the account's self-signing key,
   which is recovered from secret storage, or created for an account that has
   no cross-signing keys yet. *)
let verify ~state ~config client encryption user_id =
  let keys = Me.Keys.query_keys client ~users:[ (user_id, []) ] () in
  let has_identity =
    List.exists (fun (u, _) -> Id.User_id.equal u user_id) keys.master_keys
  in
  if has_identity then (
    match config.recovery_key with
    | None ->
        Logs.warn (fun m ->
            m
              "The account has cross-signing keys, so its recovery key is \
               needed to verify this device. Until then, rooms show its \
               messages as from an unverified device.");
        false
    | Some credential ->
        let recovered = Me.Recovery.recover client ~encryption ~credential in
        Me.Secrets.sign_own_device client ~encryption recovered.private_identity;
        true)
  else
    let identity = Cs.create_private_identity ~user_id in
    Cs.generate_private_keys
      ~random:(Mc.Client.random (Me.Client.base client))
      identity;
    let upload =
      match Cs.build_upload identity with
      | Some u -> u
      | None -> failwith "The new cross-signing identity is incomplete"
    in
    let user = Id.User_id.to_string user_id in
    (match
       Me.Keys.upload_signing_keys_uiaa client ~master_key:upload.master_key
         ~self_signing_key:upload.self_signing_key
         ~user_signing_key:upload.user_signing_key
         ~auth_callback:(fun (r : Mc.Uiaa.uiaa_response) ->
           Some
             (Mc.Uiaa.password_auth ~user ~password:config.password
                ?session:r.session ()))
         ()
     with
    | None -> ()
    | Some _ -> failwith "The homeserver did not accept the cross-signing keys");
    Me.Encryption.track_users encryption [ user_id ];
    publish encryption client;
    Me.Secrets.sign_own_device client ~encryption identity;
    let enabled =
      Me.Recovery.enable client ~encryption ~private_identity:identity ()
    in
    write_private Eio.Path.(state / "recovery-key") (enabled.recovery_key ^ "\n");
    Logs.app (fun m ->
        m
          "Created cross-signing keys for %s. Its recovery key is in %a. Keep \
           it somewhere safe: verifying another device of the account needs \
           it."
          user Eio.Path.pp
          Eio.Path.(state / "recovery-key"));
    true

let connect ~sw ~env ~state config =
  let state = (state :> Eio.Fs.dir_ty Eio.Path.t) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 state;
  let root = Eio.Path.(state / "matrix") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let store = Mc.Profile_store.create_at ~root ~profile in
  let client = session ~sw ~env ~store config in
  let session = Option.get (Mc.Client.session (Me.Client.base client)) in
  let user_id = session.user_id and device_id = session.device_id in
  if Id.User_id.to_string user_id <> config.user then
    Fmt.failwith "The stored session is for %s, not %s"
      (Id.User_id.to_string user_id)
      config.user;
  let encryption =
    Me.Encryption.of_env env ~user_id ~device_id
      ~store:(Mc.Crypto_store.create_at ~root ~profile)
      ()
  in
  publish encryption client;
  let marker =
    Eio.Path.(state / ("verified-" ^ Id.Device_id.to_string device_id))
  in
  if not (Eio.Path.is_file marker) then
    if verify ~state ~config client encryption user_id then (
      write_private marker "";
      Logs.app (fun m ->
          m "Verified device %s of %s"
            (Id.Device_id.to_string device_id)
            config.user));
  let sync =
    Me.Sync_service.of_store
      ~store:(Mc.Store.on_disk ~dir:(Mc.Profile_store.dir store))
      ~user_id ()
  in
  let clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  let queue =
    Me.Send_queue.create
      ~random:(Mc.Client.random (Me.Client.base client))
      ~user_id ()
  in
  let t =
    {
      client;
      user_id;
      device_id;
      encryption;
      sync;
      queue;
      synced = false;
      allow_unencrypted = config.allow_unencrypted;
      clock;
      waiting = Hashtbl.create 8;
    }
  in
  Me.Send_queue.on_change queue (fun request ->
      let resolve r =
        match Hashtbl.find_opt t.waiting (Me.Send_queue.id request) with
        | Some u ->
            Hashtbl.remove t.waiting (Me.Send_queue.id request);
            Eio.Promise.resolve u r
        | None -> ()
      in
      match Me.Send_queue.status request with
      | Sent id -> resolve (Ok (Id.Event_id.to_string id))
      | Wedged ->
          resolve
            (Error
               (Option.fold ~none:"the homeserver refused it"
                  ~some:Mc.Error.to_string
                  (Me.Send_queue.last_error request)))
      | Cancelled -> resolve (Error "it was cancelled")
      | Pending | Sending | Uploaded _ -> ());
  (* Rooms the bot is invited to are joined. Which of them it posts in is up to
     the bindings. *)
  let join room =
    Eio.Fiber.fork ~sw (fun () ->
        try
          ignore (Me.Rooms.join client ~room_id_or_alias:(`Room_id room) ());
          Logs.app (fun m -> m "Joined %s" (Id.Room_id.to_string room))
        with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | e ->
            Logs.warn (fun m ->
                m "Could not join %s: %s"
                  (Id.Room_id.to_string room)
                  (Printexc.to_string e)))
  in
  Me.Sync_service.run ~sw ~clock client sync ~encryption
    ~on_change:(fun _ (changes : Me.Sync_service.changes) ->
      t.synced <- true;
      List.iter
        (fun (c : Me.Sync_service.room_change) ->
          let was =
            Option.map
              (fun (i : Mc.Base_client.room_info) -> i.membership)
              c.previous
          in
          if c.info.membership = Invited && was <> Some Mc.Base_client.Invited
          then join c.changed_room_id)
        changes.room_changes)
    ();
  Me.Send_queue.start ~sw ~clock ~encryption
    ~members:(Me.Sync_service.members sync)
    client queue;
  t

let room_id room =
  match Id.Room_id.of_string room with
  | Ok r -> r
  | Error (`Msg m) -> Fmt.failwith "Not a room ID, %s: %s" room m

let ready t ~room =
  t.synced
  &&
  let room = room_id room in
  match Mc.Base_client.find_room (Me.Sync_service.state t.sync) room with
  | Some info ->
      info.membership = Joined
      && (t.allow_unencrypted
         || Me.Encryption.is_room_encrypted t.encryption room)
  | None -> false

let await t request =
  let p, u = Eio.Promise.create () in
  Hashtbl.replace t.waiting (Me.Send_queue.id request) u;
  (* The request may already have been sent. *)
  (match Me.Send_queue.status request with
  | Sent id ->
      Hashtbl.remove t.waiting (Me.Send_queue.id request);
      Eio.Promise.resolve u (Ok (Id.Event_id.to_string id))
  | _ -> ());
  match
    Eio.Time.with_timeout t.clock 120. (fun () -> Ok (Eio.Promise.await p))
  with
  | Ok (Ok id) -> id
  | Ok (Error why) -> failwith ("The message was not sent: " ^ why)
  | Error `Timeout ->
      Hashtbl.remove t.waiting (Me.Send_queue.id request);
      ignore (Me.Send_queue.cancel t.queue request);
      failwith "The message was not sent within two minutes"

let send t ~room (m : Render.t) =
  let content =
    Matrix_proto.Event.Text_message_content.make ~body:m.plain ~msgtype:Notice
      ~formatted_body:m.html ()
    |> Jsont.Json.encode Matrix_proto.Event.Text_message_content.jsont
    |> Result.get_ok
  in
  await t
    (Me.Send_queue.send_message t.queue ~room_id:(room_id room)
       ~event_type:"m.room.message" ~content)

let edit t ~room ~event (m : Render.t) =
  let event_id =
    match Id.Event_id.of_string event with
    | Ok e -> e
    | Error (`Msg m) -> Fmt.failwith "Not an event ID, %s: %s" event m
  in
  ignore
    (await t
       (Me.Send_queue.send_edit t.queue ~room_id:(room_id room) ~event_id
          ~new_body:m.plain ~formatted_body:m.html
          ~format:"org.matrix.custom.html" ()))
