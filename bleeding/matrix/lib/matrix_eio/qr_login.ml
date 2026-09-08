type intent = Matrix_client.Qr_login.intent = Login | Reciprocate

let unwrap context fn =
  Error.with_context context (fun () -> Error.unwrap (fn ()))

type t = Matrix_client.Qr_login.t = {
  intent : intent;
  public_key : Matrix_client.Crypto_key.Curve25519.Public.t;
  rendezvous_id : string;
  base_url : Uriz.t;
}

type codec_error = Matrix_client.Qr_login.codec_error =
  | Not_enough_data
  | Invalid_prefix
  | Invalid_type of int
  | Invalid_intent of int
  | Invalid_utf8 of string
  | Invalid_base_url of string
  | Field_too_long of string
  | Invalid_base64 of string

type secure_channel_error = Matrix_client.Qr_login.secure_channel_error =
  | Invalid_channel_intent
  | Unsupported_qr_code_type

let pp_codec_error = Matrix_client.Qr_login.pp_codec_error
let make = Matrix_client.Qr_login.make
let of_bytes = Matrix_client.Qr_login.of_bytes
let to_bytes = Matrix_client.Qr_login.to_bytes
let of_base64 = Matrix_client.Qr_login.of_base64
let to_base64 = Matrix_client.Qr_login.to_base64
let rendezvous_path = Matrix_client.Qr_login.rendezvous_path

let rendezvous_server_supported client =
  unwrap "probing Matrix QR rendezvous support" (fun () ->
      Matrix_client.Qr_login.rendezvous_server_supported (Client.base client))

let establish_secure_channel = Matrix_client.Qr_login.establish_secure_channel

module Msc4108 = struct
  include Matrix_client.Qr_login.Msc4108

  (** The Eio adapter deliberately starts at an already established channel.
      Rendering a QR code and the rendezvous handshake have a separate lifetime,
      and callers can therefore choose their UI and cancellation policy before
      entering this module. *)
  module Application_eio = struct
    module Pure = Matrix_client.Qr_login.Msc4108.Application
    module Messages = Matrix_client.Qr_login.Msc4108.Messages

    type login_progress = Pure.login_progress

    type login_start = Pure.login_start =
      | Await_protocols
      | Homeserver_known of Uriz.t

    type grant_progress = Pure.grant_progress
    type grant_decision = Pure.grant_decision = Confirm | Cancel

    type login = {
      client : Client.t;
      session : Matrix_client.Client.session;
      client_id : string;
      expires_at : Ptime.t option;
      encryption : Encryption.t;
      private_identity : Matrix_client.Cross_signing.private_identity;
      backup : Matrix_client.Qr_login.Msc4108.Secrets.backup option;
    }

    type Eio.Exn.err +=
      | Application_error of
          Matrix_client.Qr_login.Msc4108.Secure_channel.error Pure.error

    let pp_application_error ppf error =
      Pure.pp_error Matrix_client.Qr_login.Msc4108.Secure_channel.pp_error ppf
        error

    let () =
      Eio.Exn.register_pp (fun ppf -> function
        | Application_error error ->
            pp_application_error ppf error;
            true
        | _ -> false)

    let fail_application error =
      raise (Eio.Exn.create (Application_error error))

    let protect_channel ~context channel f =
      Fun.protect
        (fun () -> Error.with_context context f)
        ~finally:(fun () -> ignore (Secure_channel.close channel))

    let client_id_and_metadata ?http ?client_id ?client_metadata ?allow_insecure
        client =
      let base = Client.base client in
      let http = Option.value http ~default:(Client.http client) in
      let metadata =
        unwrap "fetching OAuth metadata for QR login" (fun () ->
            Matrix_client.Oauth.Metadata.fetch ~http base)
      in
      unwrap "validating OAuth metadata for QR login" (fun () ->
          Matrix_client.Oauth.Metadata.validate_device ?allow_insecure metadata);
      let client_id =
        match client_id with
        | Some id -> id
        | None ->
            if metadata.registration_endpoint = None then
              raise
                (Error.err
                   (Error.Json
                      "OAuth authorisation server has no registration endpoint"));
            let metadata' =
              Option.value client_metadata
                ~default:Oauth.default_device_client_metadata
            in
            (unwrap "registering an OAuth client for QR login" (fun () ->
                 Matrix_client.Oauth.Registration.register ~http base metadata
                   metadata'))
              .client_id
      in
      if String.trim client_id = "" then
        raise (Error.err (Error.Json "OAuth client_id is empty"));
      (http, metadata, client_id)

    let add_seconds at seconds =
      match Mtime.Span.of_float_ns (seconds *. 1e9) with
      | None -> at
      | Some span -> Option.value (Mtime.add_span at span) ~default:at

    let await_token ~env ~http ~base ~metadata ~client_id ~started
        ~(authorization : Matrix_client.Oauth.Device_authorization.t) ?timeout
        () =
      let ( let* ) = Result.bind in
      let timeout =
        match timeout with
        | None -> None
        | Some value when Float.is_finite value && value > 0. -> Some value
        | Some _ ->
            raise
              (Error.err
                 (Error.Json "OAuth timeout must be finite and positive"))
      in
      let expiry = add_seconds started (float authorization.expires_in) in
      let deadline =
        match timeout with
        | None -> expiry
        | Some value ->
            let caller = add_seconds started value in
            if Mtime.compare caller expiry < 0 then caller else expiry
      in
      let remaining () =
        let now = Eio.Time.Mono.now env#mono_clock in
        if Mtime.compare now deadline >= 0 then None
        else Some (Mtime.Span.to_float_ns (Mtime.span now deadline) /. 1e9)
      in
      let wait seconds =
        match remaining () with
        | None -> Error Pure.Expired
        | Some left ->
            Eio.Time.Mono.sleep_until env#mono_clock
              (add_seconds
                 (Eio.Time.Mono.now env#mono_clock)
                 (Float.min (float seconds) left));
            if remaining () = None then Error Pure.Expired else Ok ()
      in
      let rec poll interval =
        let* () = wait interval in
        let left = Option.value (remaining ()) ~default:0. in
        if left <= 0. then Error Pure.Expired
        else
          let result =
            try
              Some
                (Eio.Time.Timeout.run_exn
                   (Eio.Time.Timeout.seconds env#mono_clock left) (fun () ->
                     Error.with_context
                       "polling OAuth device authorization for QR login"
                       (fun () ->
                         Matrix_client.Oauth.Device_authorization.poll ~http
                           base metadata ~client_id
                           ~device_code:authorization.device_code ())))
            with Eio.Time.Timeout -> None
          in
          match result with
          | None -> Error Pure.Expired
          | Some (Ok token) -> Ok token
          | Some
              (Error
                 Matrix_client.Oauth.Device_authorization.Authorization_pending)
            ->
              poll interval
          | Some (Error Matrix_client.Oauth.Device_authorization.Slow_down) ->
              poll (interval + 5)
          | Some (Error Matrix_client.Oauth.Device_authorization.Access_denied)
            ->
              Error Pure.Access_denied
          | Some (Error Matrix_client.Oauth.Device_authorization.Expired_token)
            ->
              Error Pure.Expired
          | Some
              (Error
                 (Matrix_client.Oauth.Device_authorization.OAuth_error error))
            ->
              Error
                (Pure.Token_error
                   (Format.asprintf "%a" Oauth.pp_oauth_error error))
          | Some
              (Error
                 (Matrix_client.Oauth.Device_authorization.Transport_error error))
            ->
              Error.raise_client_error
                ~context:"polling OAuth device authorization for QR login" error
      in
      poll authorization.interval

    let login ~env ?http ?client_id ?client_metadata ?scope ?allow_insecure
        ?timeout ?crypto_store ?(start = Await_protocols)
        ?(on_progress = fun _ -> ()) ?on_authenticated channel client () =
      protect_channel ~context:"performing Matrix QR login" channel (fun () ->
          let base = Client.base client in
          (match Client.session client with
          | None -> ()
          | Some _ ->
              raise
                (Error.err
                   (Error.Json "QR login requires an unauthenticated client")));
          let start =
            match start with
            | Await_protocols -> Await_protocols
            | Homeserver_known homeserver ->
                if
                  not
                    (Matrix_client.Client.same_origin (Client.base client)
                       homeserver)
                then
                  raise
                    (Error.err
                       (Error.Json
                          "known homeserver differs from the client origin"));
                Homeserver_known homeserver
          in
          let account =
            Matrix_client.Olm.Account.create
              ~random:(Matrix_client.Client.random base)
              ()
          in
          let curve = Matrix_client.Olm.Account.curve25519_key account in
          let device_id =
            match
              Matrix_proto.Id.Device_id.of_string
                (Matrix_client.Crypto_key.Curve25519.Public.to_base64 curve)
            with
            | Ok id -> id
            | Error (`Msg error) ->
                raise
                  (Error.err
                     (Error.Json ("invalid Curve25519 device ID: " ^ error)))
          in
          let http, metadata, client_id =
            client_id_and_metadata ?http ?client_id ?client_metadata
              ?allow_insecure client
          in
          let authorization =
            unwrap "requesting OAuth device authorization for QR login"
              (fun () ->
                Matrix_client.Oauth.Device_authorization.request ?scope ~http
                  base metadata ~client_id ~device_id ())
          in
          (* [expires_in] starts when the authorization response arrives, not
             after the peer has approved the protocol. *)
          let authorization_started = Eio.Time.Mono.now env#mono_clock in
          let session = ref None in
          let authenticated = ref None in
          let encryption = ref None in
          let private_identity = ref None in
          let backup = ref None in
          let expires_at = ref None in
          let initial_keys = ref None in
          let hooks : _ Pure.login_hooks =
            {
              prepare =
                (fun ~homeserver ->
                  match homeserver with
                  | Some advertised
                    when not
                           (Matrix_client.Client.same_origin
                              (Client.base client) advertised) ->
                      Error "channel advertised a different homeserver"
                  | _ ->
                      Ok
                        {
                          Pure.grant =
                            {
                              verification_uri = authorization.verification_uri;
                              verification_uri_complete =
                                authorization.verification_uri_complete;
                            };
                          device_id =
                            Matrix_client.Crypto_key.Curve25519.Public.to_base64
                              curve;
                          user_code = authorization.user_code;
                          await_token =
                            (fun () ->
                              match
                                await_token ~env ~http ~base ~metadata
                                  ~client_id ~started:authorization_started
                                  ~authorization ?timeout ()
                              with
                              | Ok value -> Ok value
                              | Error Pure.Access_denied ->
                                  Error Pure.Access_denied
                              | Error Pure.Expired -> Error Pure.Expired
                              | Error (Pure.Token_error value) ->
                                  Error (Pure.Token_error value));
                        });
              activate =
                (fun ~device_id:wire_device_id token ->
                  let expected_device_id =
                    Matrix_client.Crypto_key.Curve25519.Public.to_base64 curve
                  in
                  if not (String.equal wire_device_id expected_device_id) then
                    Error "peer changed the OAuth device ID"
                  else
                    let requested =
                      match
                        Matrix_proto.Id.Device_id.of_string expected_device_id
                      with
                      | Ok id -> id
                      | Error (`Msg error) ->
                          Error.raise_client_error
                            ~context:"validating the QR login device ID"
                            (Matrix_client.Error.Json_error
                               ("invalid Curve25519 device ID: " ^ error))
                    in
                    match
                      Option.bind token.scope
                        Matrix_client.Oauth.device_id_of_scope
                    with
                    | Some token_device
                      when not
                             (Matrix_proto.Id.Device_id.equal token_device
                                requested) ->
                        Error "OAuth token was issued for another device"
                    | _ ->
                        let session' =
                          unwrap "finishing the OAuth QR login" (fun () ->
                              Matrix_client.Oauth.Token.finish_login base
                                ~device_id:requested token)
                        in
                        (* The OAuth session is valid at this point.  Give the
                           caller a chance to persist it before any encryption
                           state is created or any further protocol work. *)
                        Option.iter
                          (fun callback ->
                            callback session' ~client_id
                              ~expires_at:token.expires_at)
                          on_authenticated;
                        let client' = Client.with_session client session' in
                        let encryption' =
                          Encryption.create_with_account
                            ~random:(Matrix_client.Client.random base)
                            ~user_id:session'.user_id ~device_id:requested
                            ~account ?store:crypto_store ()
                        in
                        let keys =
                          Encryption.device_keys_for_upload encryption'
                        in
                        Encryption.save encryption';
                        session := Some session';
                        authenticated := Some client';
                        encryption := Some encryption';
                        initial_keys := Some keys;
                        expires_at := token.expires_at;
                        Ok ());
              import_secrets =
                (fun bundle ->
                  match
                    (!session, !authenticated, !encryption, !initial_keys)
                  with
                  | Some session', Some client', Some encryption', Some initial
                    -> (
                      match
                        Matrix_client.Qr_login.Msc4108.Secrets.import
                          ~user_id:session'.user_id bundle
                      with
                      | Error error ->
                          Error
                            (Format.asprintf "%a"
                               Matrix_client.Qr_login.Msc4108.Secrets.pp_error
                               error)
                      | Ok imported -> (
                          if
                            not
                              (Matrix_proto.Id.User_id.equal
                                 (Matrix_client.Cross_signing.identity_user_id
                                    imported.private_identity)
                                 session'.user_id)
                          then Error "imported identity belongs to another user"
                          else
                            let signed =
                              match
                                Matrix_client.Cross_signing.self_signing_secret
                                  imported.private_identity
                              with
                              | Some secret -> secret
                              | None ->
                                  (* [Secrets.import] accepts only complete bundles,
                               but retain an explicit protocol error if that
                               invariant changes. *)
                                  raise
                                    (Error.err
                                       (Error.Json
                                          "imported bundle has no self-signing \
                                           secret"))
                            in
                            let signed_keys =
                              Matrix_client.Cross_signing.sign_device_keys
                                ~signer:signed ~signer_user_id:session'.user_id
                                initial
                            in
                            let pending =
                              Encryption.outgoing_requests encryption'
                            in
                            let pending =
                              List.map
                                (function
                                  | Matrix_client.Encryption.Keys_upload
                                      {
                                        device_keys = _;
                                        one_time_keys;
                                        fallback_keys;
                                      } ->
                                      Matrix_client.Encryption.Keys_upload
                                        {
                                          device_keys = Some signed_keys;
                                          one_time_keys;
                                          fallback_keys;
                                        }
                                  | request -> request)
                                pending
                            in
                            let pending =
                              if
                                List.exists
                                  (function
                                    | Matrix_client.Encryption.Keys_upload _ ->
                                        true
                                    | _ -> false)
                                  pending
                              then pending
                              else
                                Matrix_client.Encryption.Keys_upload
                                  {
                                    device_keys = Some signed_keys;
                                    one_time_keys = [];
                                    fallback_keys = [];
                                  }
                                :: pending
                            in
                            let failed = ref None in
                            Encryption.execute_requests
                              ~on_error:(fun error -> failed := Some error)
                              encryption' client' pending;
                            match !failed with
                            | Some error ->
                                Error (Format.asprintf "%a" Error.pp_err error)
                            | None ->
                                Option.iter
                                  (fun (backup' :
                                         Matrix_client.Qr_login.Msc4108.Secrets
                                         .backup) ->
                                    Encryption.enable_backup encryption'
                                      ~version:backup'.backup_version
                                      ~decryption_key:backup'.decryption_key
                                      (Matrix_client.Backup.Decryption_key
                                       .public backup'.decryption_key))
                                  imported.backup;
                                let queried =
                                  unwrap "querying keys after QR login"
                                    (fun () ->
                                      Matrix_client.Keys.query_keys
                                        (Client.base client')
                                        ~users:[ (session'.user_id, []) ]
                                        ())
                                in
                                Encryption.receive_keys_query encryption'
                                  queried;
                                let self_signing_key =
                                  let public =
                                    Matrix_client.Crypto_key.Ed25519.Private
                                    .public signed
                                  in
                                  Matrix_client.Cross_signing.key
                                    ~role:
                                      Matrix_client.Cross_signing.Self_signing
                                    {
                                      user_id = session'.user_id;
                                      usage =
                                        [ Matrix_client.Keys.Self_signing ];
                                      keys =
                                        [
                                          ( Matrix_client.Crypto_key.Key_id.v
                                              ~algorithm:"ed25519"
                                              ~id:
                                                (Matrix_client.Crypto_key
                                                 .Ed25519
                                                 .Public
                                                 .to_base64 public),
                                            Matrix_client.Crypto_key.Ed25519
                                            .Public
                                            .to_base64 public );
                                        ];
                                      signatures = [];
                                    }
                                in
                                let own_device =
                                  List.find_map
                                    (fun (user_id, devices) ->
                                      if
                                        not
                                          (Matrix_proto.Id.User_id.equal user_id
                                             session'.user_id)
                                      then None
                                      else
                                        List.find_map
                                          (fun (device_id, device) ->
                                            if
                                              Matrix_proto.Id.Device_id.equal
                                                device_id session'.device_id
                                            then
                                              Some
                                                (Matrix_client.Cross_signing
                                                 .create_device device)
                                            else None)
                                          devices)
                                    queried.device_keys
                                in
                                let verified =
                                  match own_device with
                                  | Some device
                                    when Matrix_client.Cross_signing
                                         .verify_device_signature
                                           ~self_signing_key ~device ->
                                      Encryption.set_device_trust encryption'
                                        session'.user_id
                                        ~device_id:session'.device_id
                                        Encryption.Verified;
                                      Encryption.save encryption';
                                      private_identity :=
                                        Some imported.private_identity;
                                      backup := imported.backup;
                                      Ok ()
                                  | _ ->
                                      Error
                                        "server did not return a valid \
                                         self-signature"
                                in
                                verified))
                  | _ -> Error "OAuth activation did not complete");
              on_progress;
            }
          in
          match
            Pure.run_login ~start ~channel:(Pure.secure_channel channel) ~hooks
          with
          | Error error -> fail_application error
          | Ok () -> (
              match
                (!authenticated, !session, !encryption, !private_identity)
              with
              | ( Some client,
                  Some session,
                  Some encryption,
                  Some private_identity ) ->
                  {
                    client;
                    session;
                    client_id;
                    expires_at = !expires_at;
                    encryption;
                    private_identity;
                    backup = !backup;
                  }
              | _ ->
                  raise
                    (Error.err
                       (Error.Json
                          "QR login completed without an authenticated result"))
              ))

    let device_exists client device_id =
      match Matrix_proto.Id.Device_id.of_string device_id with
      | Error (`Msg error) -> Error error
      | Ok device_id -> (
          match
            Matrix_client.Devices.get_device (Client.base client) ~device_id
          with
          | Ok _ -> Ok true
          | Error (Matrix_client.Error.Http_error { status = 404; _ })
          | Error
              (Matrix_client.Error.Matrix_error
                 { errcode = Matrix_client.Error.M_NOT_FOUND; _ }) ->
              Ok false
          | Error error -> Error (Matrix_client.Error.to_string error))

    let await_device ~env ?(timeout = Oauth.default_timeout) client device_id =
      let started = Eio.Time.Mono.now env#mono_clock in
      let deadline = add_seconds started timeout in
      let rec loop () =
        if Mtime.compare (Eio.Time.Mono.now env#mono_clock) deadline >= 0 then
          false
        else
          let found =
            match Matrix_client.Devices.get_devices (Client.base client) with
            | Ok devices ->
                List.exists
                  (fun (device : Matrix_client.Devices.device) ->
                    String.equal
                      (Matrix_proto.Id.Device_id.to_string device.device_id)
                      device_id)
                  devices
            | Error _ -> false
          in
          if found then true
          else begin
            Eio.Time.Mono.sleep_until env#mono_clock
              (add_seconds (Eio.Time.Mono.now env#mono_clock) 1.);
            loop ()
          end
      in
      loop ()

    let grant ~env ?(advertise_protocols = true) ?timeout ?backup ~authorize
        ?(on_progress = fun _ -> ()) ~private_identity ~encryption channel
        client () =
      protect_channel ~context:"granting a Matrix QR login" channel (fun () ->
          let timeout = Option.value timeout ~default:Oauth.default_timeout in
          if not (Float.is_finite timeout && timeout > 0.) then
            raise
              (Error.err
                 (Error.Json "grant timeout must be finite and positive"));
          let session =
            match Client.session client with
            | Some session -> session
            | None -> raise (Error.err Error.Not_logged_in)
          in
          if
            not
              (Matrix_proto.Id.User_id.equal
                 (Matrix_client.Cross_signing.identity_user_id private_identity)
                 session.user_id)
          then
            raise
              (Error.err (Error.Json "private identity belongs to another user"));
          if
            (not
               (Matrix_proto.Id.User_id.equal
                  (Encryption.user_id encryption)
                  session.user_id))
            || not
                 (Matrix_proto.Id.Device_id.equal
                    (Encryption.device_id encryption)
                    session.device_id)
          then
            raise
              (Error.err
                 (Error.Json "encryption state belongs to another device"));
          let hooks : Pure.grant_hooks =
            {
              export_secrets =
                (fun () ->
                  match
                    Matrix_client.Qr_login.Msc4108.Secrets.export
                      ~private_identity ?backup ()
                  with
                  | Ok value -> Ok value
                  | Error error ->
                      Error
                        (Format.asprintf "%a"
                           Matrix_client.Qr_login.Msc4108.Secrets.pp_error error));
              device_exists = (fun id -> device_exists client id);
              authorize = (fun uri -> Ok (authorize uri));
              await_device =
                (fun id -> Ok (await_device ~env ~timeout client id));
              on_progress;
            }
          in
          let start =
            if advertise_protocols then
              Pure.Advertise_protocols (Client.homeserver client)
            else Pure.Protocols_already_known
          in
          match
            Pure.run_grant ~start ~channel:(Pure.secure_channel channel) ~hooks
          with
          | Ok () -> ()
          | Error error -> fail_application error)

    let application_login = login
    let application_grant = grant

    module Session = struct
      type progress =
        | Establishing_channel
        | Awaiting_check_code of int
        | OAuth of login_progress
        | Grant_oauth of grant_progress
        | Secrets
        | Trust_and_backup
        | Done

      type persistence =
        Matrix_client.Cross_signing.private_identity ->
        Matrix_client.Qr_login.Msc4108.Secrets.backup option ->
        unit

      type error =
        | Check_code_rejected
        | Secure_channel_error of Secure_channel.error

      type Eio.Exn.err += Session_error of error

      let pp_error ppf = function
        | Check_code_rejected ->
            Format.pp_print_string ppf "secure-channel check code rejected"
        | Secure_channel_error error ->
            Format.fprintf ppf "secure channel: %a" Secure_channel.pp_error
              error

      let () =
        Eio.Exn.register_pp (fun ppf -> function
          | Session_error error ->
              pp_error ppf error;
              true
          | _ -> false)

      let fail error = raise (Eio.Exn.create (Session_error error))

      let secure = function
        | Ok value -> value
        | Error error -> fail (Secure_channel_error error)

      let report callback progress = callback progress
      let cleanup f = Eio.Cancel.protect (fun () -> ignore (f ()))

      let establish_login ~transport ~rendezvous_server ~random ~display_qr
          ~confirm_check_code ~on_progress () =
        report on_progress Establishing_channel;
        let displayed =
          secure (Secure_channel.login ~transport ~rendezvous_server ~random ())
        in
        let displayed_live = ref true in
        let almost =
          Fun.protect
            ~finally:(fun () ->
              if !displayed_live then
                cleanup (fun () -> Secure_channel.cancel_displayed displayed))
            (fun () ->
              display_qr (Secure_channel.qr_code displayed);
              let almost = secure (Secure_channel.connect displayed) in
              displayed_live := false;
              almost)
        in
        let almost_live = ref true in
        Fun.protect
          ~finally:(fun () ->
            if !almost_live then
              cleanup (fun () -> Secure_channel.cancel_almost almost))
          (fun () ->
            let code = Secure_channel.check_code almost in
            report on_progress (Awaiting_check_code code);
            if not (confirm_check_code code) then begin
              ignore (Secure_channel.cancel_almost almost);
              fail Check_code_rejected
            end;
            let channel =
              secure (Secure_channel.confirm almost ~check_code:code)
            in
            almost_live := false;
            channel)

      let establish_grant ~transport ~random ~scan_qr ~expected_intent
          ~confirm_check_code ~on_progress () =
        report on_progress Establishing_channel;
        let channel =
          secure
            (Secure_channel.from_qr_code ~transport ~random ~expected_intent
               (scan_qr ()))
        in
        let live = ref true in
        Fun.protect
          ~finally:(fun () ->
            if !live then cleanup (fun () -> Secure_channel.close channel))
          (fun () ->
            let code = Secure_channel.check_code_established channel in
            report on_progress (Awaiting_check_code code);
            if not (confirm_check_code code) then begin
              ignore (Secure_channel.close channel);
              fail Check_code_rejected
            end;
            live := false;
            channel)

      let login ~env ~transport ~rendezvous_server ~random ~display_qr
          ~confirm_check_code ?persist ?http ?client_id ?client_metadata ?scope
          ?allow_insecure ?timeout ?crypto_store ?start
          ?(on_progress = fun _ -> ()) ?on_authenticated client () =
        let channel =
          establish_login ~transport ~rendezvous_server ~random ~display_qr
            ~confirm_check_code ~on_progress ()
        in
        let application_progress progress =
          match progress with
          | Pure.Starting | Pure.Waiting_for_token _ ->
              report on_progress (OAuth progress)
          | Pure.Syncing_secrets -> report on_progress Secrets
          | Pure.Done -> report on_progress Trust_and_backup
        in
        let result =
          application_login ~env ?http ?client_id ?client_metadata ?scope
            ?allow_insecure ?timeout ?crypto_store ?start ?on_authenticated
            ~on_progress:application_progress channel client ()
        in
        Option.iter
          (fun persist -> persist result.private_identity result.backup)
          persist;
        report on_progress Done;
        result

      let grant ~env ~transport ~random ~scan_qr ~expected_intent
          ~confirm_check_code ?advertise_protocols ?timeout ?backup ~authorize
          ?(on_progress = fun _ -> ()) ~private_identity ~encryption client () =
        let channel =
          establish_grant ~transport ~random ~scan_qr ~expected_intent
            ~confirm_check_code ~on_progress ()
        in
        let application_progress progress =
          match progress with
          | Pure.Grant_starting | Pure.Waiting_for_authorization _ ->
              report on_progress (Grant_oauth progress)
          | Pure.Grant_syncing_secrets -> report on_progress Secrets
          | Pure.Grant_done -> report on_progress Trust_and_backup
        in
        application_grant ~env ?advertise_protocols ?timeout ?backup ~authorize
          ~on_progress:application_progress ~private_identity ~encryption
          channel client ();
        report on_progress Done
    end
  end
end
