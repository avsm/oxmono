open Result.Syntax

let src = Logs.Src.create "matrix.recovery" ~doc:"Matrix recovery lifecycle"

module Log = (val Logs.src_log src : Logs.LOG)

(* Callback and best-effort refresh failures are diagnostic only.  Do not
   render an Eio context here: lower layers may have attached a complete HTTP
   URL, including query credentials.  Cancellation remains control flow and
   is handled explicitly at each boundary below. *)
let safe_exception_string = function
  | Eio.Io _ -> "I/O operation failed"
  | exn -> Printexc.to_string exn

let key_backup_event_type = "m.key_backup"
let backup_disabled_event_type = "m.org.matrix.custom.backup_disabled"

let key_backup_jsont =
  Jsont.Object.(map Fun.id |> mem "enabled" Jsont.bool |> finish)

let backup_disabled_jsont =
  Jsont.Object.(map Fun.id |> mem "disabled" Jsont.bool |> finish)

type state = Unknown | Enabled | Disabled | Incomplete
type marker = Absent | Valid of bool | Malformed of string
type markers = { stable : marker; unstable : marker }

let marker codec = function
  | None -> Absent
  | Some json -> (
      match Jsont.Json.decode codec json with
      | Ok value -> Valid value
      | Error message -> Malformed message)

let key_backup_marker = marker key_backup_jsont
let backup_disabled_marker = marker backup_disabled_jsont

type inputs = {
  secret_storage_enabled : bool option;
  cross_signing_complete : bool option;
  backup_enabled : bool option;
  stable_marker : marker;
  unstable_marker : marker;
}

let marked_disabled ~stable_marker ~unstable_marker =
  match stable_marker with
  | Valid enabled -> Some (not enabled)
  | Absent -> (
      match unstable_marker with Valid disabled -> Some disabled | _ -> None)
  | Malformed _ -> None

let state inputs =
  match (inputs.secret_storage_enabled, inputs.cross_signing_complete) with
  | None, _ | _, None -> Unknown
  | Some false, _ -> Disabled
  | Some true, Some false -> Incomplete
  | Some true, Some true -> (
      match inputs.backup_enabled with
      | Some true -> Enabled
      | Some false -> (
          match
            marked_disabled ~stable_marker:inputs.stable_marker
              ~unstable_marker:inputs.unstable_marker
          with
          | Some true -> Enabled
          | Some false | None -> Incomplete)
      | None -> Unknown)

type account_data_write = { event_type : string; content : Jsont.json }

let object' members = Jsont.Json.object' members

let bool_member name value =
  object' [ Jsont.Json.mem (Jsont.Json.name name) (Jsont.Json.bool value) ]

let empty_content = object' []

let empty_secret_content =
  object'
    [ Jsont.Json.mem (Jsont.Json.name "encrypted") (Jsont.Json.object' []) ]

let known_secret_event_types =
  [
    Secret_storage.secret_cross_signing_master;
    Secret_storage.secret_cross_signing_user_signing;
    Secret_storage.secret_cross_signing_self_signing;
    Secret_storage.secret_megolm_backup_v1;
  ]

let mark_enabled_writes () =
  [
    { event_type = key_backup_event_type; content = bool_member "enabled" true };
    {
      event_type = backup_disabled_event_type;
      content = bool_member "disabled" false;
    };
  ]

let disable_writes ?default_key_id () =
  Option.fold ~none:[]
    ~some:(fun key_id ->
      [
        { event_type = Secrets.key_event_type ~key_id; content = empty_content };
      ])
    default_key_id
  @ [
      { event_type = Secrets.default_key_event_type; content = empty_content };
      {
        event_type = key_backup_event_type;
        content = bool_member "enabled" false;
      };
      {
        event_type = backup_disabled_event_type;
        content = bool_member "disabled" true;
      };
    ]
  @ List.map
      (fun event_type -> { event_type; content = empty_secret_content })
      known_secret_event_types

let is_not_found = function
  | Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ } -> true
  | Error.Http_error { status = 404; _ } -> true
  | _ -> false

let fetch_marker client ~event_type ~decode =
  match
    Account_data.get client
      ~event_type:(Matrix_proto.Event.Event_type.of_string event_type)
  with
  | Ok json -> Ok (decode (Some json))
  | Error error when is_not_found error -> Ok Absent
  | Error (Error.Json_error message) -> Ok (Malformed message)
  | Error error -> Error error

let fetch_markers client =
  match
    fetch_marker client ~event_type:key_backup_event_type
      ~decode:key_backup_marker
  with
  | Error error -> Error error
  | Ok stable -> (
      match
        fetch_marker client ~event_type:backup_disabled_event_type
          ~decode:backup_disabled_marker
      with
      | Error error -> Error error
      | Ok unstable -> Ok { stable; unstable })

let apply_writes client writes =
  let rec loop = function
    | [] -> Ok ()
    | { event_type; content } :: rest -> (
        match
          Account_data.set client
            ~event_type:(Matrix_proto.Event.Event_type.of_string event_type)
            ~content
        with
        | Ok () -> loop rest
        | Error error -> Error error)
  in
  loop writes

let mark_backup_enabled client = apply_writes client (mark_enabled_writes ())

let disable_account_data ?default_key_id client =
  apply_writes client (disable_writes ?default_key_id ())

let disable client ~encryption =
  let machine = Encryption_driver.machine encryption in
  match Encryption.backup_version machine with
  | None -> Error (Error.Policy_denied "recovery backup is not enabled")
  | Some version ->
      let* () =
        match Room_keys.delete_version client ~version with
        | Ok () -> Ok ()
        | Error error when is_not_found error -> Ok ()
        | Error error -> Error error
      in
      Encryption.disable_backup machine;
      let* () = Encryption_driver.save encryption in
      (* Rust deliberately treats a missing, malformed, or temporarily
         unreadable default-key event as absence while disabling. The backup
         has already gone at this point, so account-data cleanup must still be
         attempted. *)
      let default_key_id =
        match Secrets.get_default_key_id client with
        | Ok key_id -> key_id
        | Error _ -> None
      in
      disable_account_data ?default_key_id client

let disable_and_delete_backups client ~encryption =
  let* session =
    match Client.session client with
    | Some session -> Ok session
    | None -> Error Error.No_session
  in
  let machine = Encryption_driver.machine encryption in
  if
    not
      (Matrix_proto.Id.User_id.equal session.user_id
         (Encryption.user_id machine))
  then
    Error
      (Error.Json_error
         "authenticated session and encryption machine belong to different \
          users")
  else
    (* Do not change local state until every remote deletion has succeeded.
       Rust treats a missing DELETE target as already gone, but only a missing
       GET ends this loop: another GET verifies that no server backup remains. *)
    let rec delete_all () =
      match Room_keys.get_current_version client with
      | Ok version ->
          if String.equal version.version "" then
            Error (Error.Json_error "current backup version has no version")
          else
            let* () =
              match
                Room_keys.delete_version client ~version:version.version
              with
              | Ok () -> Ok ()
              | Error error when is_not_found error -> Ok ()
              | Error error -> Error error
            in
            delete_all ()
      | Error error when is_not_found error -> Ok ()
      | Error error -> Error error
    in
    let* () = delete_all () in
    Encryption.disable_backup machine;
    Encryption_driver.save encryption

type recovered = {
  store : Secrets.store;
  private_identity : Cross_signing.private_identity;
}

let recover client ~encryption ~credential =
  (* Keep these steps separate deliberately.  In particular, the driver is
     persisted after cross-signing import and before backup validation, so a
     bad backup secret cannot lose the recovered identity/device state. *)
  let* store = Secrets.open_secret_store client ~credential in
  let* private_identity =
    Secrets.import_cross_signing store
      ~encryption:(Encryption_driver.machine encryption)
  in
  let* () = Encryption_driver.save encryption in
  let* () = Secrets.import_backup store ~encryption in
  Ok { store; private_identity }

(* Create or resume the room-key backup without creating a new SSSS store.
   Keeping this operation separate is important for [recover_and_fix_backup],
   whose repair must export into the store it just opened. *)
let enable_backup_for_recovery client ~encryption ~private_identity =
  let machine = Encryption_driver.machine encryption in
  let backup_version, backup_key, create_backup =
    match (Encryption.snapshot machine).state.backup with
    | { version = Some version; decryption_key = Some key; _ } ->
        (version, Some key, false)
    | { version = None; decryption_key = None; _ } -> ("", None, true)
    | _ -> ("", None, false)
  in
  let* backup_version, backup_key =
    if not create_backup then
      if String.equal backup_version "" then
        Error
          (Error.Policy_denied
             "local backup state has no persisted version and decryption key")
      else Ok (backup_version, backup_key)
    else
      let* () =
        match Room_keys.get_current_version client with
        | Ok version ->
            Error
              (Error.Policy_denied
                 (Printf.sprintf
                    "a server-side room-key backup already exists (version %s)"
                    version.version))
        | Error error when is_not_found error -> Ok ()
        | Error error -> Error error
      in
      let* () = mark_backup_enabled client in
      let key = Backup.Decryption_key.generate ~random:(Client.random client) in
      let auth_data =
        {
          Backup.public_key = Backup.Decryption_key.public key;
          signatures = [];
        }
      in
      let auth_data =
        match Cross_signing.master_secret private_identity with
        | None -> auth_data
        | Some signing_key ->
            let key_id =
              Crypto_key.Key_id.v ~algorithm:"ed25519"
                ~id:
                  (Crypto_key.Ed25519.Public.to_base64
                     (Crypto_key.Ed25519.Private.public signing_key))
            in
            Backup.sign_auth_data ~signing_key
              ~user_id:(Cross_signing.identity_user_id private_identity)
              ~key_id auth_data
      in
      let auth_data =
        Backup.auth_data_to_json auth_data |> Encryption.sign machine
      in
      let* version =
        Room_keys.create_version client ~algorithm:Backup.backup_algorithm
          ~auth_data
      in
      Encryption.enable_backup machine ~version ~decryption_key:key
        (Backup.Decryption_key.public key);
      let* () = Encryption_driver.save encryption in
      Ok (version, Some key)
  in
  Ok (backup_version, backup_key)

let recover_and_fix_backup client ~encryption ~credential =
  (* Session and machine ownership are local checks and must precede opening
     the remote store. The recovered identity is necessarily checked by the
     cross-signing import after that store has been opened. *)
  let* session =
    match Client.session client with
    | Some session -> Ok session
    | None -> Error Error.No_session
  in
  let machine = Encryption_driver.machine encryption in
  if
    not
      (Matrix_proto.Id.User_id.equal session.user_id
         (Encryption.user_id machine))
  then
    Error
      (Error.Json_error
         "authenticated session and encryption machine belong to different \
          users")
  else
    let* store = Secrets.open_secret_store client ~credential in
    let* private_identity =
      Secrets.import_cross_signing store ~encryption:machine
    in
    let* () = Encryption_driver.save encryption in
    match Secrets.import_backup_typed store ~encryption with
    | Ok _ -> Ok { store; private_identity }
    | Error (Secrets.Other error) -> Error error
    | Error
        ( Secrets.Inconsistent_backup_key
        | Secrets.Missing_or_invalid_backup_secret ) ->
        let* () = disable_and_delete_backups client ~encryption in
        let* backup_version, backup_key =
          enable_backup_for_recovery client ~encryption ~private_identity
        in
        let* () =
          Secrets.export_recovery_secrets store ~random:(Client.random client)
            ~private_identity ?backup_key ()
        in
        ignore backup_version;
        Ok { store; private_identity }

type backup_upload = Not_waited | Uploaded of int | Upload_failed of Error.t

type enabled = {
  store : Secrets.store;
  key_id : string;
  recovery_key : string;
  backup_version : string;
  backup_upload : backup_upload;
}

let same_user session machine identity =
  Matrix_proto.Id.User_id.equal session.Client.user_id
    (Encryption.user_id machine)
  && Matrix_proto.Id.User_id.equal session.Client.user_id
       (Cross_signing.identity_user_id identity)

let check_state client ~encryption ~private_identity =
  let* session =
    match Client.session client with
    | Some session -> Ok session
    | None -> Error Error.No_session
  in
  let machine = Encryption_driver.machine encryption in
  if not (same_user session machine private_identity) then
    Error
      (Error.Json_error
         "authenticated session, encryption machine, and cross-signing \
          identity belong to different users")
  else
    match Secrets.get_default_key_id client with
    | Error (Error.Json_error _) -> Ok Disabled
    | Error error -> Error error
    | Ok None -> Ok Disabled
    | Ok (Some _) -> (
        let cross_signing_complete =
          Option.is_some (Cross_signing.master_secret private_identity)
          && Option.is_some (Cross_signing.self_signing_secret private_identity)
          && Option.is_some (Cross_signing.user_signing_secret private_identity)
        in
        if not cross_signing_complete then Ok Incomplete
        else
          let backup = (Encryption.snapshot machine).state.backup in
          match (backup.version, backup.encryption_key) with
          | Some _, Some _ -> Ok Enabled
          | _ ->
              let* markers = fetch_markers client in
              Ok
                (state
                   {
                     secret_storage_enabled = Some true;
                     cross_signing_complete = Some true;
                     backup_enabled = Some false;
                     stable_marker = markers.stable;
                     unstable_marker = markers.unstable;
                   }))

let enable client ~encryption ~private_identity ?passphrase
    ?(wait_for_backups_to_upload = false) () =
  let* session =
    match Client.session client with
    | Some session -> Ok session
    | None -> Error Error.No_session
  in
  let machine = Encryption_driver.machine encryption in
  if not (same_user session machine private_identity) then
    Error
      (Error.Json_error
         "authenticated session, encryption machine, and cross-signing \
          identity belong to different users")
  else
    let* backup_version, backup_key =
      enable_backup_for_recovery client ~encryption ~private_identity
    in
    let* created =
      Secrets.create_recovery_store client ~random:(Client.random client)
        ?passphrase ~private_identity ?backup_key ()
    in
    let backup_upload =
      if wait_for_backups_to_upload then
        match Encryption_driver.backup_pending encryption client with
        | Ok count -> Uploaded count
        | Error error -> Upload_failed error
      else Not_waited
    in
    Ok
      {
        store = created.store;
        key_id = created.key_id;
        recovery_key = created.recovery_key;
        backup_version;
        backup_upload;
      }

let reset_key client ~encryption ~private_identity ?passphrase () =
  let* session =
    match Client.session client with
    | Some session -> Ok session
    | None -> Error Error.No_session
  in
  let machine = Encryption_driver.machine encryption in
  if not (same_user session machine private_identity) then
    Error
      (Error.Json_error
         "authenticated session, encryption machine, and cross-signing \
          identity belong to different users")
  else
    let backup_key =
      (Encryption.snapshot machine).state.backup.decryption_key
    in
    Secrets.create_recovery_store client ~random:(Client.random client)
      ?passphrase ~private_identity ?backup_key ()

type recovered_and_reset = {
  store : Secrets.store;
  key_id : string;
  recovery_key : string;
  private_identity : Cross_signing.private_identity;
}

let recover_and_reset client ~encryption ~credential ?passphrase () =
  let* recovered = recover client ~encryption ~credential in
  let* created =
    reset_key client ~encryption ~private_identity:recovered.private_identity
      ?passphrase ()
  in
  Ok
    {
      store = created.store;
      key_id = created.key_id;
      recovery_key = created.recovery_key;
      private_identity = recovered.private_identity;
    }

let derive_state = state
let disable_and_delete_backups_client = disable_and_delete_backups

module Manager = struct
  type subscription = int

  type remote_observation = {
    default_key : [ `Absent | `Malformed of string | `Present ];
    markers : markers option;
  }

  let default_key_jsont =
    Jsont.Object.(
      map Fun.id |> mem "key" Matrix_proto.Json.Codec.string |> finish)

  type t = {
    client : Client.t;
    encryption : Encryption_driver.t;
    mutable base : Base_client.state;
    mutable private_identity : Cross_signing.private_identity option;
    mutable current : state;
    mutable remote_observation : remote_observation option;
    (* Preserve one generated upload across a returned UIAA challenge. *)
    mutable pending_identity : Cross_signing.private_identity option;
    (* Whether the reset began with an active local backup. This survives a
       UIAA continuation and is the only safe signal for Rust-style backup
       re-enablement: an explicit disabled marker must not be overridden. *)
    mutable pending_reenable_backup : bool;
    mutable next_subscription : int;
    subscribers : (subscription, state -> unit) Hashtbl.t;
  }

  (* The default-key event is deliberately decoded here rather than fetched:
     the manager's ordinary projection is a view over committed base state.
     A malformed default key is the same disabled observation as in
     [check_state], while malformed marker data remains visible to [state]. *)
  let default_key_id = function
    | None -> `Absent
    | Some json -> (
        match Jsont.Json.decode default_key_jsont json with
        | Ok _ -> `Present
        | Error message -> `Malformed message)

  let complete_identity t =
    match t.private_identity with
    | None -> false
    | Some identity ->
        Option.is_some (Cross_signing.master_secret identity)
        && Option.is_some (Cross_signing.self_signing_secret identity)
        && Option.is_some (Cross_signing.user_signing_secret identity)

  let backup_enabled t =
    let backup =
      (Encryption.snapshot (Encryption_driver.machine t.encryption)).state
        .backup
    in
    Option.is_some backup.version && Option.is_some backup.encryption_key

  let observed_inputs t ~default_key ~stable_marker ~unstable_marker =
    match default_key with
    | `Absent | `Malformed _ ->
        {
          secret_storage_enabled = Some false;
          cross_signing_complete = Some (complete_identity t);
          backup_enabled = Some (backup_enabled t);
          stable_marker = Absent;
          unstable_marker = Absent;
        }
    | `Present ->
        {
          secret_storage_enabled = Some true;
          cross_signing_complete = Some (complete_identity t);
          backup_enabled = Some (backup_enabled t);
          stable_marker;
          unstable_marker;
        }

  let local_inputs t base =
    observed_inputs t
      ~default_key:
        (default_key_id
           (Base_client.find_account_data base Secrets.default_key_event_type))
      ~stable_marker:
        (key_backup_marker
           (Base_client.find_account_data base key_backup_event_type))
      ~unstable_marker:
        (backup_disabled_marker
           (Base_client.find_account_data base backup_disabled_event_type))

  let publish t next =
    if next <> t.current then begin
      t.current <- next;
      (* Snapshot first: a callback can unsubscribe itself or another
         callback, and this manager intentionally has no hidden scheduler. *)
      Hashtbl.to_seq_values t.subscribers
      |> List.of_seq
      |> List.iter (fun callback ->
          try callback next with
          | Eio.Cancel.Cancelled _ as exn ->
              let bt = Printexc.get_raw_backtrace () in
              Printexc.raise_with_backtrace exn bt
          | exn ->
              Log.err (fun m ->
                  m "recovery state subscriber failed: %s"
                    (safe_exception_string exn)))
    end

  let derive_remote t observation =
    match observation.markers with
    | Some markers ->
        derive_state
          (observed_inputs t ~default_key:observation.default_key
             ~stable_marker:markers.stable ~unstable_marker:markers.unstable)
    | None -> (
        match observation.default_key with
        | `Absent | `Malformed _ -> Disabled
        | `Present ->
            if not (complete_identity t) then Incomplete
            else if backup_enabled t then Enabled
            else Incomplete)

  let derive_local t base =
    match
      ( default_key_id
          (Base_client.find_account_data base Secrets.default_key_event_type),
        Base_client.next_batch base )
    with
    | `Absent, None -> Unknown
    | _ -> derive_state (local_inputs t base)

  let derive t base =
    match t.remote_observation with
    | Some observation -> derive_remote t observation
    | None -> derive_local t base

  let client t = t.client
  let encryption t = t.encryption
  let base_state t = t.base
  let private_identity t = t.private_identity
  let state t = t.current

  let manager_user t =
    Encryption.user_id (Encryption_driver.machine t.encryption)

  let check_base_user t base =
    if
      not
        (Matrix_proto.Id.User_id.equal (manager_user t)
           (Base_client.user_id base))
    then
      invalid_arg
        "Matrix_client.Recovery.Manager.refresh_from_base: base belongs to a \
         different user"

  let check_identity_user t = function
    | None -> ()
    | Some identity ->
        if
          not
            (Matrix_proto.Id.User_id.equal (manager_user t)
               (Cross_signing.identity_user_id identity))
        then
          invalid_arg
            "Matrix_client.Recovery.Manager.set_private_identity: identity \
             belongs to a different user"

  let create ?private_identity ?base client ~encryption =
    let base =
      match base with
      | Some base -> base
      | None -> (
          match Client.session client with
          | Some session -> Base_client.create ~user_id:session.user_id ()
          | None ->
              invalid_arg
                "Matrix_client.Recovery.Manager.create: base is required \
                 without a session")
    in
    let base_user = Base_client.user_id base in
    let machine_user =
      Encryption.user_id (Encryption_driver.machine encryption)
    in
    let session_matches =
      Matrix_proto.Id.User_id.equal base_user machine_user
      &&
      match Client.session client with
      | None -> true
      | Some session -> Matrix_proto.Id.User_id.equal session.user_id base_user
    in
    let identity_matches =
      match private_identity with
      | None -> true
      | Some identity ->
          Matrix_proto.Id.User_id.equal base_user
            (Cross_signing.identity_user_id identity)
    in
    if (not session_matches) || not identity_matches then
      invalid_arg
        "Matrix_client.Recovery.Manager.create: client, base, encryption, and \
         identity belong to different users";
    let result =
      {
        client;
        encryption;
        base;
        private_identity;
        current = Unknown;
        remote_observation = None;
        pending_identity = None;
        pending_reenable_backup = false;
        next_subscription = 0;
        subscribers = Hashtbl.create 4;
      }
    in
    result.current <- derive result base;
    result

  let refresh_from_base t base =
    check_base_user t base;
    t.base <- base;
    (* A committed sync snapshot is authoritative and already retains global
       account data omitted from an incremental response. *)
    t.remote_observation <- None;
    let next = derive t base in
    publish t next;
    next

  let set_private_identity t private_identity =
    check_identity_user t private_identity;
    t.private_identity <- private_identity;
    let next = derive t t.base in
    publish t next;
    next

  let remote_default t =
    match
      Account_data.get t.client
        ~event_type:
          (Matrix_proto.Event.Event_type.of_string
             Secrets.default_key_event_type)
    with
    | Ok json -> (
        match Jsont.Json.decode default_key_jsont json with
        | Ok _ -> Ok `Present
        | Error message -> Ok (`Malformed message))
    | Error error when is_not_found error -> Ok `Absent
    | Error (Error.Json_error message) -> Ok (`Malformed message)
    | Error error -> Error error

  let refresh t =
    let result =
      match remote_default t with
      | Error error -> Error error
      | Ok ((`Absent | `Malformed _) as default_key) ->
          t.remote_observation <- Some { default_key; markers = None };
          let next = Disabled in
          publish t next;
          Ok next
      | Ok `Present ->
          let complete = complete_identity t in
          if not complete then begin
            t.remote_observation <-
              Some { default_key = `Present; markers = None };
            let next = Incomplete in
            publish t next;
            Ok next
          end
          else if backup_enabled t then begin
            t.remote_observation <-
              Some { default_key = `Present; markers = None };
            let next = Enabled in
            publish t next;
            Ok next
          end
          else
            let* markers = fetch_markers t.client in
            t.remote_observation <-
              Some { default_key = `Present; markers = Some markers };
            let next =
              derive_state
                {
                  secret_storage_enabled = Some true;
                  cross_signing_complete = Some true;
                  backup_enabled = Some false;
                  stable_marker = markers.stable;
                  unstable_marker = markers.unstable;
                }
            in
            publish t next;
            Ok next
    in
    (match result with
    | Ok _ -> ()
    | Error _ ->
        (* Keep the last confirmed remote value, but never strand a state
           change that was already committed to the local base snapshot. *)
        publish t (derive t t.base));
    result

  let subscribe t callback =
    let subscription = t.next_subscription in
    t.next_subscription <- subscription + 1;
    Hashtbl.replace t.subscribers subscription callback;
    match callback t.current with
    | () -> subscription
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Hashtbl.remove t.subscribers subscription;
        Printexc.raise_with_backtrace exn bt

  let unsubscribe t subscription = Hashtbl.remove t.subscribers subscription

  let watch t callback =
    let subscription = subscribe t callback in
    fun () -> unsubscribe t subscription

  let best_effort_refresh t =
    try ignore (refresh t) with
    | Eio.Cancel.Cancelled _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        publish t (derive t t.base);
        Printexc.raise_with_backtrace exn bt
    | exn ->
        publish t (derive t t.base);
        Log.debug (fun m ->
            m "recovery best-effort refresh failed: %s"
              (safe_exception_string exn))

  let run t operation =
    match operation () with
    | result ->
        best_effort_refresh t;
        result
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        (* An operation may have changed and persisted only part of the local
           encryption state before I/O cancellation or another exception. Do
           not hide cancellation, but do publish that local boundary first. *)
        publish t (derive t t.base);
        Printexc.raise_with_backtrace exn bt

  let disable t = run t (fun () -> disable t.client ~encryption:t.encryption)

  let disable_and_delete_backups t =
    run t (fun () ->
        disable_and_delete_backups t.client ~encryption:t.encryption)

  let recover t ~credential =
    run t (fun () ->
        let result = recover t.client ~encryption:t.encryption ~credential in
        (match result with
        | Ok recovered -> t.private_identity <- Some recovered.private_identity
        | Error _ -> ());
        result)

  let recover_and_fix_backup t ~credential =
    run t (fun () ->
        let result =
          recover_and_fix_backup t.client ~encryption:t.encryption ~credential
        in
        (match result with
        | Ok recovered -> t.private_identity <- Some recovered.private_identity
        | Error _ -> ());
        result)

  let enable t ?private_identity ?passphrase ?wait_for_backups_to_upload () =
    let private_identity =
      match private_identity with
      | Some identity -> Some identity
      | None -> t.private_identity
    in
    run t (fun () ->
        let result =
          match private_identity with
          | None ->
              Error (Error.Policy_denied "no private cross-signing identity")
          | Some private_identity ->
              enable t.client ~encryption:t.encryption ~private_identity
                ?passphrase ?wait_for_backups_to_upload ()
        in
        (match (private_identity, result) with
        | Some identity, Ok _ -> t.private_identity <- Some identity
        | _ -> ());
        result)

  let reset_key t ?private_identity ?passphrase () =
    run t (fun () ->
        match
          match private_identity with
          | Some identity -> Some identity
          | None -> t.private_identity
        with
        | None ->
            Error (Error.Policy_denied "no private cross-signing identity")
        | Some private_identity ->
            reset_key t.client ~encryption:t.encryption ~private_identity
              ?passphrase ())

  let recover_and_reset t ~credential ?passphrase () =
    run t (fun () ->
        let result =
          recover_and_reset t.client ~encryption:t.encryption ~credential
            ?passphrase ()
        in
        (match result with
        | Ok recovered -> t.private_identity <- Some recovered.private_identity
        | Error _ -> ());
        result)

  let reset_identity t ~auth_callback () =
    run t (fun () ->
        let upload_device_keys_if_needed () =
          let machine = Encryption_driver.machine t.encryption in
          match
            List.find_map
              (function
                | Encryption.Keys_upload
                    {
                      device_keys = Some device_keys;
                      one_time_keys;
                      fallback_keys;
                    } as request ->
                    Some (request, device_keys, one_time_keys, fallback_keys)
                | _ -> None)
              (Encryption.outgoing_requests machine)
          with
          | None -> Ok ()
          | Some (request, device_keys, one_time_keys, fallback_keys) ->
              let one_time_keys =
                match one_time_keys with [] -> None | keys -> Some keys
              in
              let fallback_keys =
                match fallback_keys with [] -> None | keys -> Some keys
              in
              let* response =
                Keys.upload_keys t.client ~device_keys ?one_time_keys
                  ?fallback_keys ()
              in
              (* Keep the request pending until the homeserver has accepted
                 the complete payload. This is deliberately the same
                 mark/receive/save boundary as [Encryption_driver], since the
                 replacement signing-key signature may refer to these device
                 keys immediately afterwards. *)
              Encryption.mark_sent machine request;
              Encryption.receive_keys_upload machine response;
              Encryption_driver.save t.encryption
        in
        let upload_own_device_signature identity =
          let machine = Encryption_driver.machine t.encryption in
          match Cross_signing.self_signing_secret identity with
          | None ->
              Error
                (Error.Json_error
                   "generated cross-signing identity has no self-signing key")
          | Some signer ->
              let user_id = Encryption.user_id machine in
              let device_id = Encryption.device_id machine in
              let signed =
                Encryption.device_keys_for_upload machine
                |> Cross_signing.sign_device_keys ~signer
                     ~signer_user_id:user_id
              in
              let* json =
                Jsont.Json.encode Keys.device_keys_jsont signed
                |> Result.map_error (fun message -> Error.Json_error message)
              in
              let* response =
                Keys.upload_signatures t.client
                  [
                    ( user_id,
                      [ (Matrix_proto.Id.Device_id.to_string device_id, json) ]
                    );
                  ]
              in
              if response.failures = [] then Ok ()
              else
                Error
                  (Error.Json_error
                     "the homeserver rejected the replacement identity's \
                      own-device signature")
        in
        let install identity =
          (* The remote identity is now authoritative. Only this success
             boundary replaces the caller's old capability. *)
          Encryption.reset_cross_signing
            (Encryption_driver.machine t.encryption);
          t.private_identity <- Some identity;
          match Encryption_driver.save t.encryption with
          | Ok () ->
              if
                (* Rust re-enables only a backup that was active before reset;
                 an explicit disabled marker remains authoritative. This does
                 not recreate SSSS, whose recovery key cannot be returned by
                 this API, so the post-reset state is still truthfully
                 Disabled until the caller runs [enable]. *)
                t.pending_reenable_backup
              then
                match
                  enable_backup_for_recovery t.client ~encryption:t.encryption
                    ~private_identity:identity
                with
                | Ok _ ->
                    t.pending_identity <- None;
                    t.pending_reenable_backup <- false;
                    Uiaa.Uiaa_success identity
                | Error error -> Uiaa.Uiaa_error error
              else begin
                t.pending_identity <- None;
                Uiaa.Uiaa_success identity
              end
          | Error error -> Uiaa.Uiaa_error error
        in
        let upload identity =
          (* Install the continuation before entering transport: cancellation
             or an ambiguous exception must not cause a fresh identity/body on
             the next call. *)
          t.pending_identity <- Some identity;
          match Cross_signing.build_upload identity with
          | None ->
              (* [generate_private_keys] above makes this unreachable. Keep
                 it as a typed failure rather than claiming a reset happened
                 if the representation grows another required key. *)
              Uiaa.Uiaa_error
                (Error.Json_error
                   "generated cross-signing identity is incomplete")
          | Some upload -> (
              match upload_device_keys_if_needed () with
              | Error error -> Uiaa.Uiaa_error error
              | Ok () -> (
                  match
                    Keys.upload_signing_keys_uiaa t.client
                      ~master_key:upload.master_key
                      ~self_signing_key:upload.self_signing_key
                      ~user_signing_key:upload.user_signing_key ~auth_callback
                      ()
                  with
                  | Uiaa.Uiaa_success () -> (
                      match upload_own_device_signature identity with
                      | Ok () -> install identity
                      | Error error -> Uiaa.Uiaa_error error)
                  | Uiaa.Uiaa_auth_required challenge ->
                      Uiaa.Uiaa_auth_required challenge
                  | Uiaa.Uiaa_error error -> Uiaa.Uiaa_error error))
        in
        match t.pending_identity with
        | Some identity -> upload identity
        | None -> (
            (* This is intentionally the first operation which can mutate the
               machine. In particular, never discard the old identity while a
               server-side backup is still present. *)
            let should_reenable =
              t.pending_reenable_backup || backup_enabled t
            in
            match
              disable_and_delete_backups_client t.client
                ~encryption:t.encryption
            with
            | Error error -> Uiaa.Uiaa_error error
            | Ok () -> (
                t.pending_reenable_backup <- should_reenable;
                (* Rust marks secret storage disabled before rotating the
                   cross-signing identity. This is a deliberate partial
                   boundary: a later upload failure leaves the old identity
                   local, but not the old SSSS marker or backup. *)
                match
                  Account_data.set t.client
                    ~event_type:
                      (Matrix_proto.Event.Event_type.of_string
                         Secrets.default_key_event_type)
                    ~content:empty_content
                with
                | Error error -> Uiaa.Uiaa_error error
                | Ok () ->
                    let user_id =
                      Encryption.user_id
                        (Encryption_driver.machine t.encryption)
                    in
                    let identity =
                      Cross_signing.create_private_identity ~user_id
                    in
                    Cross_signing.generate_private_keys
                      ~random:(Client.random t.client) identity;
                    upload identity)))

  let cancel_pending_identity_reset t =
    t.pending_identity <- None;
    t.pending_reenable_backup <- false
end
