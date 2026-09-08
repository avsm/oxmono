let default_key_event_type = "m.secret_storage.default_key"
let key_event_type ~key_id = "m.secret_storage.key." ^ key_id
let ( let* ) = Result.bind
let json_error msg = Error (Error.Json_error msg)

(* Account data that has never been set answers 404 / M_NOT_FOUND. *)
let is_not_found = function
  | Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ } -> true
  | Error.Http_error { status = 404; _ } -> true
  | _ -> false

let account_data_type name = Matrix_proto.Event.Event_type.of_string name

let get_account_data_opt client ~event_type =
  match Account_data.get client ~event_type:(account_data_type event_type) with
  | Ok json -> Ok (Some json)
  | Error e when is_not_found e -> Ok None
  | Error e -> Error e

type default_key = { key : string }

let default_key_jsont =
  Jsont.Object.(
    map (fun key -> { key })
    |> mem "key" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.key)
    |> finish)

let decode_json codec json =
  Jsont.Json.decode codec json |> Result.map_error (fun e -> Error.Json_error e)

let encode_json codec v =
  Jsont.Json.encode codec v |> Result.map_error (fun e -> Error.Json_error e)

let get_default_key_id client =
  let* json = get_account_data_opt client ~event_type:default_key_event_type in
  match json with
  | None -> Ok None
  | Some json ->
      let* d = decode_json default_key_jsont json in
      Ok (Some d.key)

let set_default_key_id client ~key_id =
  let* content = encode_json default_key_jsont { key = key_id } in
  Account_data.set client
    ~event_type:(account_data_type default_key_event_type)
    ~content

let get_key_description client ~key_id =
  let* json =
    Account_data.get client
      ~event_type:(account_data_type (key_event_type ~key_id))
  in
  decode_json Secret_storage.Key_description.jsont json

let put_key_description client ~key_id description =
  let* content = encode_json Secret_storage.Key_description.jsont description in
  Account_data.set client
    ~event_type:(account_data_type (key_event_type ~key_id))
    ~content

(* The inner values stay as JSON: a copy encrypted under another key may use
   an algorithm this client does not know, and must survive a round trip
   through [store_secret] untouched. *)
type secret_content = { encrypted : (string * Jsont.json) list }

let secret_content_jsont =
  let encrypted_jsont = Json_codec.string_map Matrix_proto.Json.Codec.json in
  Jsont.Object.(
    map (fun encrypted -> { encrypted })
    |> mem "encrypted" encrypted_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.encrypted)
    |> finish)

let decrypt_secret key ~name data =
  match Secret_storage.decrypt key ~name data with
  | Ok secret when String.is_valid_utf_8 secret -> Ok secret
  | Ok _ -> json_error (Printf.sprintf "secret %S is not valid UTF-8" name)
  | Error (`Msg msg) ->
      json_error (Printf.sprintf "could not decrypt secret %S: %s" name msg)

let get_secret_opt client ~key_id ~key ~name =
  let* json = get_account_data_opt client ~event_type:name in
  match json with
  | None -> Ok None
  | Some json -> (
      let* content = decode_json secret_content_jsont json in
      match List.assoc_opt key_id content.encrypted with
      | None -> Ok None
      | Some entry ->
          let* data = decode_json Secret_storage.Encrypted.jsont entry in
          let* secret = decrypt_secret key ~name data in
          Ok (Some secret))

let get_secret client ~key_id ~key ~name =
  (* Keep the original API's distinction between an absent account-data event
     (the homeserver's M_NOT_FOUND/HTTP 404) and an event with no copy for this
     key.  The optional API below intentionally collapses the former to None. *)
  let* json = Account_data.get client ~event_type:(account_data_type name) in
  let* content = decode_json secret_content_jsont json in
  match List.assoc_opt key_id content.encrypted with
  | None ->
      json_error
        (Printf.sprintf "secret %S has no copy encrypted under key %S" name
           key_id)
  | Some entry ->
      let* data = decode_json Secret_storage.Encrypted.jsont entry in
      decrypt_secret key ~name data

type store = {
  store_client : Client.t;
  store_user_id : Matrix_proto.Id.User_id.t;
  store_homeserver : Uriz.t;
  store_key_id : string;
  store_key : Secret_storage.key;
}

type created_store = { store : store; key_id : string; recovery_key : string }

(* Rust uses a fresh 32-character alphanumeric identifier for every newly
   created secret-storage key.  The bytes are deliberately kept separate from
   the key material: this identifier is public account-data metadata. *)
let fresh_key_id ~random =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  in
  let bytes = Random.generate random 32 in
  String.init 32 (fun i ->
      let byte = Char.code (String.get bytes i) in
      String.get alphabet (byte mod String.length alphabet))

let open_secret_store client ~credential =
  let* session =
    match Client.session client with
    | Some session -> Ok session
    | None -> Error Error.No_session
  in
  let* key_id = get_default_key_id client in
  let key_id =
    match key_id with
    | Some key_id -> Ok key_id
    | None -> json_error "secret storage has no default key"
  in
  let* key_id = key_id in
  let* description = get_key_description client ~key_id in
  if description.algorithm <> Secret_storage.algorithm then
    json_error
      (Printf.sprintf "unsupported secret storage algorithm %S"
         description.algorithm)
  else
    let check key =
      match Secret_storage.check_key key description with
      | Secret_storage.Correct | Secret_storage.Unchecked -> Ok key
      | Secret_storage.Incorrect ->
          json_error "secret storage credential does not match the key"
    in
    (* Rust's [from_account_data] tries the passphrase interpretation first
       whenever passphrase metadata is present, and only then falls back to
       the cryptographic Base58 representation. *)
    let decode_recovery () =
      Secret_storage.Recovery_key.decode credential
      |> Result.map_error (fun (`Msg msg) -> Error.Json_error msg)
    in
    let key =
      match description.passphrase with
      | Some passphrase -> (
          let passphrase_key =
            match
              Secret_storage.key_of_passphrase ~passphrase:credential passphrase
            with
            | Ok key -> check key
            | Error (`Msg msg) -> Error (Error.Json_error msg)
          in
          match passphrase_key with
          | Ok _ as key -> key
          | Error passphrase_error -> (
              (* Rust accepts a recovery key even when passphrase metadata is
                 present, but if both interpretations fail it preserves the
                 more useful passphrase error. *)
              match Result.bind (decode_recovery ()) check with
              | Ok _ as key -> key
              | Error _ -> Error passphrase_error))
      | None -> Result.bind (decode_recovery ()) check
    in
    let* key = key in
    Ok
      {
        store_client = client;
        store_user_id = session.user_id;
        store_homeserver = Client.homeserver client;
        store_key_id = key_id;
        store_key = key;
      }

let store_key_id t = t.store_key_id
let store_user_id t = t.store_user_id
let store_homeserver t = t.store_homeserver

let get_store_secret t ~name =
  get_secret_opt t.store_client ~key_id:t.store_key_id ~key:t.store_key ~name

type backup_import_result = Not_configured | Imported

type backup_import_error =
  | Inconsistent_backup_key
  | Missing_or_invalid_backup_secret
  | Other of Error.t

let import_backup_typed t ~(encryption : Encryption_driver.t) =
  let machine = Encryption_driver.machine encryption in
  let user_id = Encryption.user_id machine in
  let import_secret () =
    match get_store_secret t ~name:Secret_storage.secret_megolm_backup_v1 with
    | Error (Error.Json_error _) -> Error Missing_or_invalid_backup_secret
    | Error error -> Error (Other error)
    | Ok None -> Ok Not_configured
    | Ok (Some encoded_key) -> (
        match Backup.Decryption_key.of_base64 encoded_key with
        | Error (`Msg msg) ->
            Error
              (Other
                 (Error.Json_error
                    (Printf.sprintf "invalid room-key backup decryption key: %s"
                       msg)))
        | Ok key -> (
            match Room_keys.get_current_version t.store_client with
            | Error error when is_not_found error -> Ok Not_configured
            | Error error -> Error (Other error)
            | Ok version -> (
                match
                  Backup.current_version_state ~algorithm:version.algorithm
                    ~auth_data:version.auth_data
                    ~local_key:(Some (Backup.Decryption_key.public key))
                with
                | Backup.Compatible -> (
                    Encryption.enable_backup machine ~version:version.version
                      ~decryption_key:key
                      (Backup.Decryption_key.public key);
                    match Encryption_driver.save encryption with
                    | Ok () -> Ok Imported
                    | Error error -> Error (Other error))
                | Backup.Different_public_key -> Error Inconsistent_backup_key
                | Backup.Missing_local_key ->
                    Error
                      (Other
                         (Error.Json_error
                            "room-key backup has no local decryption key"))
                | Backup.Unsupported_algorithm algorithm ->
                    Error
                      (Other
                         (Error.Json_error
                            (Printf.sprintf
                               "unsupported room-key backup algorithm %S"
                               algorithm)))
                | Backup.Malformed_auth_data message ->
                    Error
                      (Other
                         (Error.Json_error
                            (Printf.sprintf
                               "malformed room-key backup auth data: %s" message)))
                )))
  in
  match Client.session t.store_client with
  | Some session when Matrix_proto.Id.User_id.equal session.user_id user_id ->
      import_secret ()
  | Some _ ->
      Error
        (Other
           (Error.Json_error
              "secret storage and encryption machine belong to different users"))
  | None -> Error (Other Error.No_session)

let import_backup t ~encryption =
  match import_backup_typed t ~encryption with
  | Ok Not_configured | Ok Imported -> Ok ()
  | Error Inconsistent_backup_key ->
      json_error "room-key backup public key does not match secret storage"
  | Error Missing_or_invalid_backup_secret ->
      json_error "room-key backup secret is missing or invalid"
  | Error (Other error) -> Error error

let import_error e =
  Error
    (Error.Json_error
       (Format.asprintf "%a" Cross_signing.pp_private_identity_import_error e))

let identity_public encryption ~user_id ~role get_key =
  Option.bind (get_key encryption user_id) (fun published ->
      Cross_signing.key_ed25519 (Cross_signing.key ~role published))

let queried_device_keys (device : Encryption.device) : Keys.device_keys =
  {
    user_id = device.user_id;
    device_id = device.device_id;
    algorithms = device.algorithms;
    keys = device.keys;
    signatures = device.signatures;
    dehydrated = device.dehydrated;
    unsigned = None;
  }

let verify_own_device t ~encryption identity =
  match Cross_signing.self_signing_secret identity with
  | None -> Ok ()
  | Some signer -> (
      let user_id = Encryption.user_id encryption in
      let device_id = Encryption.device_id encryption in
      match Encryption.find_device encryption user_id ~device_id with
      | None ->
          (* Rust treats a missing own device as non-fatal: the private-key
             import is still useful, even though there is nothing to sign. *)
          Ok ()
      | Some device -> (
          let signed =
            Cross_signing.sign_device_keys ~signer ~signer_user_id:user_id
              (queried_device_keys device)
          in
          let* json = encode_json Keys.device_keys_jsont signed in
          let target = Matrix_proto.Id.Device_id.to_string device_id in
          let* response =
            Keys.upload_signatures t.store_client
              [ (user_id, [ (target, json) ]) ]
          in
          if response.failures <> [] then
            json_error
              "the homeserver rejected the recovered self-signing key's \
               own-device signature"
          else
            let* query =
              Keys.query_keys t.store_client ~users:[ (user_id, []) ] ()
            in
            Encryption.receive_keys_query encryption query;
            match
              ( Encryption.find_device encryption user_id ~device_id,
                Encryption.identity_self_signing_key encryption user_id )
            with
            | Some device, Some self_signing
              when Cross_signing.verify_device_signature
                     ~self_signing_key:
                       (Cross_signing.key ~role:Cross_signing.Self_signing
                          self_signing)
                     ~device:
                       (Cross_signing.create_device
                          (queried_device_keys device)) ->
                Encryption.set_device_trust encryption user_id ~device_id
                  Encryption.Verified;
                Ok ()
            | _ ->
                json_error
                  "the recovered own-device signature was absent from the \
                   refreshed key query"))

let import_cross_signing t ~encryption =
  let user_id = Encryption.user_id encryption in
  match Client.session t.store_client with
  | Some session when Matrix_proto.Id.User_id.equal session.user_id user_id -> (
      (* Read the three optional secrets before the key query, as the Rust SDK's
         SecretStore does.  No private value is retained in an error message. *)
      let* master_secret =
        get_store_secret t ~name:Secret_storage.secret_cross_signing_master
      in
      let* self_signing_secret =
        get_store_secret t
          ~name:Secret_storage.secret_cross_signing_self_signing
      in
      let* user_signing_secret =
        get_store_secret t
          ~name:Secret_storage.secret_cross_signing_user_signing
      in
      let* query = Keys.query_keys t.store_client ~users:[ (user_id, []) ] () in
      Encryption.receive_keys_query encryption query;
      let get_master e u = Encryption.identity_master_key e u in
      let get_self e u = Encryption.identity_self_signing_key e u in
      let get_user e u = Encryption.identity_user_signing_key e u in
      let master =
        identity_public encryption ~user_id ~role:Cross_signing.Master
          get_master
      in
      let self_signing =
        identity_public encryption ~user_id ~role:Cross_signing.Self_signing
          get_self
      in
      let user_signing =
        identity_public encryption ~user_id ~role:Cross_signing.User_signing
          get_user
      in
      match (master, self_signing, user_signing) with
      | ( Some expected_master,
          Some expected_self_signing,
          Some expected_user_signing ) -> (
          match
            Cross_signing.private_identity_of_secrets ~user_id ~expected_master
              ~expected_self_signing ~expected_user_signing
              ~master:master_secret ~self_signing:self_signing_secret
              ~user_signing:user_signing_secret
          with
          | Ok identity ->
              let* () = verify_own_device t ~encryption identity in
              Ok identity
          | Error e -> import_error e)
      | _ ->
          json_error
            "secret storage import requires all three validated cross-signing \
             public keys")
  | Some _ ->
      json_error
        "secret storage and encryption machine belong to different users"
  | None -> Error Error.No_session

let store_secret client ~random ~key_id ~key ~name secret =
  let* () =
    if String.is_valid_utf_8 secret then Ok ()
    else json_error (Printf.sprintf "secret %S is not valid UTF-8" name)
  in
  let* existing = get_account_data_opt client ~event_type:name in
  let* content =
    match existing with
    | None -> Ok { encrypted = [] }
    | Some json -> decode_json secret_content_jsont json
  in
  let* entry =
    encode_json Secret_storage.Encrypted.jsont
      (Secret_storage.encrypt ~random key ~name secret)
  in
  let encrypted =
    (key_id, entry) :: List.filter (fun (k, _) -> k <> key_id) content.encrypted
  in
  let* body = encode_json secret_content_jsont { encrypted } in
  Account_data.set client ~event_type:(account_data_type name) ~content:body

let put_store_secret t ~random ~name secret =
  store_secret t.store_client ~random ~key_id:t.store_key_id ~key:t.store_key
    ~name secret

let create_secret_store client ~random ?passphrase ?(secrets = []) () =
  let* session =
    match Client.session client with
    | Some session -> Ok session
    | None -> Error Error.No_session
  in
  let* key, passphrase_info =
    match passphrase with
    | None -> Ok (Secret_storage.generate_key ~random, None)
    | Some passphrase ->
        let info = Secret_storage.Passphrase_info.v ~random () in
        Secret_storage.key_of_passphrase ~passphrase info
        |> Result.map (fun key -> (key, Some info))
        |> Result.map_error (fun (`Msg msg) -> Error.Json_error msg)
  in
  let key_id = fresh_key_id ~random in
  let description =
    Secret_storage.Key_description.v ~random ?passphrase:passphrase_info key
  in
  (* Keep the Rust order: publish the key description, export any supplied
     secrets, and only then make this key the default.  There is no rollback
     endpoint for account data, so a later failure can leave an orphaned key
     description but never points the default at an incomplete key. *)
  let* () = put_key_description client ~key_id description in
  let rec put_secrets = function
    | [] -> Ok ()
    | (name, value) :: rest ->
        let* () = store_secret client ~random ~key_id ~key ~name value in
        put_secrets rest
  in
  let* () = put_secrets secrets in
  let* () = set_default_key_id client ~key_id in
  let store =
    {
      store_client = client;
      store_user_id = session.user_id;
      store_homeserver = Client.homeserver client;
      store_key_id = key_id;
      store_key = key;
    }
  in
  Ok { store; key_id; recovery_key = Secret_storage.Recovery_key.encode key }

let recovery_secret_values private_identity backup_key =
  let seed = function
    | None -> None
    | Some secret ->
        Some
          (Matrix_proto.Base64.encode
             (Crypto_key.Ed25519.Private.to_bytes secret))
  in
  List.filter_map
    (fun (name, value) -> Option.map (fun value -> (name, value)) value)
    [
      ( Secret_storage.secret_cross_signing_master,
        seed (Cross_signing.master_secret private_identity) );
      ( Secret_storage.secret_cross_signing_user_signing,
        seed (Cross_signing.user_signing_secret private_identity) );
      ( Secret_storage.secret_cross_signing_self_signing,
        seed (Cross_signing.self_signing_secret private_identity) );
      ( Secret_storage.secret_megolm_backup_v1,
        Option.map Backup.Decryption_key.to_base64 backup_key );
    ]

let export_recovery_secrets t ~random ~private_identity ?backup_key () =
  let rec put = function
    | [] -> Ok ()
    | (name, value) :: rest ->
        let* () = put_store_secret t ~random ~name value in
        put rest
  in
  put (recovery_secret_values private_identity backup_key)

let create_recovery_store client ~random ?passphrase ~private_identity
    ?backup_key () =
  let* session =
    match Client.session client with
    | Some session -> Ok session
    | None -> Error Error.No_session
  in
  if
    not
      (Matrix_proto.Id.User_id.equal session.user_id
         (Cross_signing.identity_user_id private_identity))
  then
    json_error
      "cross-signing identity and authenticated session belong to different \
       users"
  else
    (* Keep the Rust export order.  Missing keys are intentionally omitted:
       recovery can be partial when a device has not obtained every seed. *)
    let secrets = recovery_secret_values private_identity backup_key in
    create_secret_store client ~random ?passphrase ~secrets ()
