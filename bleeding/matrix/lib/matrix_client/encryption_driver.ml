module E = Encryption

let src =
  Logs.Src.create "matrix.encryption_driver" ~doc:"E2EE request execution"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind

type t = {
  machine : E.t;
  store : Crypto_store.t option;
  backup_upload_mutex : Eio.Mutex.t;
  shared_history_mutex : Eio.Mutex.t;
}

type room_key_bundle_outcome =
  | Bundle_not_applicable
  | Bundle_imported of int
  | Bundle_rejected_sender
  | Bundle_discarded_not_found
  | Bundle_discarded_malformed of string
  | Bundle_retry_key_query of Error.t
  | Bundle_retry_download of Media.encrypted_error

type share_room_history_outcome =
  | History_not_shared_visibility
  | History_not_shared_identity
  | History_no_keys
  | History_shared of int

type share_room_history_error =
  | Share_encryption_error of Error.t
  | Share_media_error of Media.encrypted_error

type invite_outcome = Invite_sent of share_room_history_outcome

type invite_error =
  | Invite_share_error of share_room_history_error
  | Invite_request_error of Error.t

let machine t = t.machine
let store t = t.store

let v ?store machine =
  {
    machine;
    store;
    backup_upload_mutex = Eio.Mutex.create ();
    shared_history_mutex = Eio.Mutex.create ();
  }

let create ~random ~user_id ~device_id ?store () =
  match store with
  | None -> Ok (v (E.create ~random ~user_id ~device_id ()))
  | Some st -> (
      let* snapshot = Crypto_store.load st in
      match snapshot with
      | None -> Ok (v ~store:st (E.create ~random ~user_id ~device_id ()))
      | Some snap ->
          let machine = E.of_snapshot ~random ~user_id ~device_id snap in
          let removed = E.clear_expired_pending_key_bundles machine in
          let* () =
            match removed with
            | [] -> Ok ()
            | _ -> Crypto_store.save st (E.snapshot machine)
          in
          Ok (v ~store:st machine))

let create_with_account ~random ~user_id ~device_id ~account ?store () =
  match store with
  | None ->
      Ok (v (E.create_with_account ~random ~user_id ~device_id ~account ()))
  | Some st -> (
      let* snapshot = Crypto_store.load st in
      match snapshot with
      | None ->
          Ok
            (v ~store:st
               (E.create_with_account ~random ~user_id ~device_id ~account ()))
      | Some _ ->
          Error
            (Error.Json_error
               "create_with_account requires an empty crypto store"))

let save t =
  match t.store with
  | None -> Ok ()
  | Some st -> Crypto_store.save st (E.snapshot t.machine)

let execute_request t client (r : E.request) =
  match r with
  | E.Keys_upload { device_keys; one_time_keys; fallback_keys } ->
      let one_time_keys = match one_time_keys with [] -> None | l -> Some l in
      let fallback_keys = match fallback_keys with [] -> None | l -> Some l in
      let* resp =
        Keys.upload_keys client ?device_keys ?one_time_keys ?fallback_keys ()
      in
      E.mark_sent t.machine r;
      E.receive_keys_upload t.machine resp;
      Ok ()
  | E.Keys_query users ->
      let* resp =
        Keys.query_keys client ~users:(List.map (fun u -> (u, [])) users) ()
      in
      E.receive_keys_query t.machine resp;
      Ok ()
  | E.Keys_claim keys ->
      let* resp = Keys.claim_keys client ~keys () in
      ignore (E.receive_keys_claim t.machine resp);
      Ok ()
  | E.To_device { event_type; txn_id; messages } ->
      let* () = To_device.send client ~event_type ~txn_id messages in
      E.mark_sent t.machine r;
      Ok ()
  | E.Room_key_share { txn_id; messages; _ } ->
      let* () =
        To_device.send client ~event_type:"m.room.encrypted" ~txn_id messages
      in
      E.mark_sent t.machine r;
      Ok ()
  | E.Room_key_bundle_share { txn_id; messages; _ } ->
      let* () =
        To_device.send client ~event_type:"m.room.encrypted" ~txn_id messages
      in
      E.mark_sent t.machine r;
      Ok ()
  | E.Room_keys_upload { version; rooms } ->
      let* _ = Room_keys.put_keys client ~version rooms in
      E.mark_sent t.machine r;
      Ok ()

(* One request failing must not stop the rest of the batch, and must not
   cost the caller the outcome [sync_hook] built around it: a rejected
   [Keys_upload] is retried on the next sync (nothing here marks it sent),
   and durable [m.secret.*] lifecycle requests remain in [outgoing_requests].
   The [m.key.verification.*] events in that same outcome are one-shot and
   cannot be recovered once dropped. *)
let execute_requests ?on_error t client requests =
  List.iter
    (fun r ->
      match execute_request t client r with
      | Ok () -> ()
      | Error e ->
          Log.warn (fun m ->
              m "%a failed: %s" E.pp_request r (Error.to_string e));
          Option.iter
            (fun callback ->
              try callback e with
              | Eio.Cancel.Cancelled _ as exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  Printexc.raise_with_backtrace exn bt
              | exn ->
                  Log.warn (fun m ->
                      m "encryption request error callback failed: %s"
                        (Printexc.to_string exn)))
            on_error)
    requests

let clear_bundle_lifecycle t (bundle : E.received_key_bundle) =
  ignore
    (E.clear_received_key_bundle t.machine ~room_id:bundle.room_id
       ~sender:bundle.sender);
  ignore (E.clear_pending_key_bundle t.machine ~room_id:bundle.room_id)

let media_not_found = function
  | Media.Media_error
      ( Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ }
      | Error.Http_error { status = 404; _ } ) ->
      true
  | _ -> false

let accept_received_room_key_bundle ?now t client ~joined
    (bundle : E.received_key_bundle) =
  if
    not
      (E.should_accept_room_key_bundle ?now t.machine ~room_id:bundle.room_id
         ~joined ~sender:bundle.sender ())
  then Bundle_not_applicable
  else
    match Keys.query_keys client ~users:[ (bundle.sender, []) ] () with
    | Error error -> Bundle_retry_key_query error
    | Ok response
      when not
             (List.exists
                (fun (user_id, _) ->
                  Matrix_proto.Id.User_id.equal user_id bundle.sender)
                response.Keys.device_keys) ->
        if response.failures <> [] then
          Bundle_retry_key_query
            (Error.Network_error
               "the room-key bundle sender's key query was incomplete")
        else begin
          (* A successful authoritative answer which omits the sender must not
             fall back to device keys retained from an older query. *)
          E.receive_keys_query t.machine
            {
              response with
              device_keys = (bundle.sender, []) :: response.device_keys;
            };
          clear_bundle_lifecycle t bundle;
          Bundle_rejected_sender
        end
    | Ok response -> (
        E.receive_keys_query t.machine response;
        if not (E.room_key_bundle_sender_is_trusted t.machine bundle) then begin
          clear_bundle_lifecycle t bundle;
          Bundle_rejected_sender
        end
        else
          match Media.download_encrypted client bundle.file with
          | Error error when media_not_found error ->
              clear_bundle_lifecycle t bundle;
              Bundle_discarded_not_found
          | Error error -> Bundle_retry_download error
          | Ok plaintext -> (
              match
                Jsont_bytesrw.decode_string
                  Room_key_export.room_key_bundle_jsont plaintext
              with
              | Error message ->
                  clear_bundle_lifecycle t bundle;
                  Bundle_discarded_malformed message
              | Ok decoded -> (
                  match
                    E.accept_room_key_bundle ?now t.machine
                      ~room_id:bundle.room_id ~joined ~sender:bundle.sender
                      decoded
                  with
                  | None -> Bundle_not_applicable
                  | Some imported ->
                      ignore
                        (E.clear_received_key_bundle t.machine
                           ~room_id:bundle.room_id ~sender:bundle.sender);
                      Bundle_imported imported)))

let sync_hook ?on_error t client response =
  let outcome = E.process_sync t.machine response in
  execute_requests ?on_error t client outcome.E.requests;
  outcome

let sync_hook_sliding ?on_error t client response =
  let outcome = E.process_sliding_sync t.machine response in
  execute_requests ?on_error t client outcome.E.requests;
  outcome

(* One pass discovers the members whose device lists are unknown, the next
   claims a one-time key for the devices that pass revealed. A device with no
   keys left to claim never becomes reachable, so the walk is bounded rather
   than run to a fixed point. *)
let rec ensure_sessions t client ~members fuel =
  if fuel <= 0 then Ok ()
  else
    match E.ensure_sessions t.machine ~members with
    | [] -> Ok ()
    | requests ->
        execute_requests t client requests;
        ensure_sessions t client ~members (fuel - 1)

let encrypt_room_event t client room ~event_type ~content ~members =
  let* () = ensure_sessions t client ~members 2 in
  let* encrypted, requests =
    E.encrypt_room_event t.machine room ~event_type ~content ~members
  in
  execute_requests t client requests;
  Ok encrypted

let send_encrypted t client room ~event_type ~content ~members =
  let* content =
    encrypt_room_event t client room ~event_type ~content ~members
  in
  Messages.send_event client ~room_id:room
    ~event_type:Matrix_proto.Event.Event_type.Room_message_encrypted ~content

(* A backup PUT is version-addressed. These responses mean that the version
   selected by the local recovery key is no longer the server's active backup,
   so retrying the same batch forever is both useless and liable to hide a
   remote rotation. This follows matrix-rust-sdk's backup uploader: disable the
   local uploader durably, return the original request error, and leave all
   sessions eligible if a caller later enables the replacement version. *)
let backup_version_is_gone = function
  | Error.Matrix_error
      { errcode = Error.M_NOT_FOUND | Error.M_WRONG_ROOM_KEYS_VERSION; _ }
  | Error.Http_error { status = 404; _ } ->
      true
  | _ -> false

let backup_pending t client =
  let request_count = function
    | E.Room_keys_upload { rooms; _ } ->
        List.fold_left (fun n (_, sessions) -> n + List.length sessions) 0 rooms
    | _ -> 0
  in
  Eio.Mutex.use_rw ~protect:true t.backup_upload_mutex (fun () ->
      let rec loop uploaded =
        let* pending = E.pending_backup t.machine in
        match pending with
        | None -> Ok uploaded
        | Some request -> (
            let count = request_count request in
            match execute_request t client request with
            | Ok () ->
                let* () = save t in
                loop (uploaded + count)
            | Error error when backup_version_is_gone error -> (
                Log.warn (fun m ->
                    m
                      "The active key-backup version disappeared or changed; \
                       disabling uploads");
                E.disable_backup t.machine;
                match save t with
                | Ok () -> Error error
                | Error save_error -> Error save_error)
            | Error error -> Error error)
      in
      loop 0)

let restore_from_backup t client =
  match E.backup_version t.machine with
  | None -> Error (Error.Json_error "no key backup is enabled")
  | Some version ->
      let* rooms = Room_keys.get_keys client ~version in
      E.import_backup t.machine rooms

let restore_room_from_backup_unlocked t client room_id =
  if not (E.backup_decryption_enabled t.machine) then Ok 0
  else if E.room_key_backup_is_fully_downloaded t.machine room_id then Ok 0
  else
    match E.backup_version t.machine with
    | None -> Ok 0
    | Some version -> (
        let* sessions = Room_keys.get_room_keys client ~version ~room_id in
        let* imported =
          E.import_backup t.machine
            [ (Matrix_proto.Id.Room_id.to_string room_id, sessions) ]
        in
        (* Do not make the marker visible until the snapshot containing it has
           been durably written. If persistence fails, clear the in-memory
           marker so a later attempt retries the download. *)
        E.mark_room_key_backup_fully_downloaded t.machine room_id;
        let persisted =
          try save t
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            E.clear_room_key_backup_fully_downloaded t.machine room_id;
            Printexc.raise_with_backtrace exn bt
        in
        match persisted with
        | Ok () -> Ok imported
        | Error error ->
            E.clear_room_key_backup_fully_downloaded t.machine room_id;
            Error error)

let restore_room_from_backup t client room_id =
  Eio.Mutex.use_rw ~protect:true t.shared_history_mutex (fun () ->
      restore_room_from_backup_unlocked t client room_id)

let restore_session_from_backup t client ~room_id ~session_id =
  Eio.Mutex.use_rw ~protect:true t.shared_history_mutex (fun () ->
      if not (E.backup_decryption_enabled t.machine) then Ok 0
      else
        match E.backup_version t.machine with
        | None -> Ok 0
        | Some version ->
            let* session =
              Room_keys.get_session_key client ~version ~room_id ~session_id
            in
            let* imported =
              E.import_backup t.machine
                [
                  ( Matrix_proto.Id.Room_id.to_string room_id,
                    [
                      (Matrix_proto.Id.Session_id.to_string session_id, session);
                    ] );
                ]
            in
            let* () = save t in
            Ok imported)

let share_room_history t client ~room_id ~recipient ~history_visibility =
  Eio.Mutex.use_rw ~protect:true t.shared_history_mutex (fun () ->
      let own_user = E.user_id t.machine in
      match E.identity_master_key t.machine own_user with
      | None -> Ok History_not_shared_identity
      | Some _ -> (
          match history_visibility with
          | Matrix_proto.Event.History_visibility.Invited
          | Matrix_proto.Event.History_visibility.Joined ->
              Ok History_not_shared_visibility
          | Matrix_proto.Event.History_visibility.Shared
          | Matrix_proto.Event.History_visibility.World_readable -> (
              let* _restored =
                match restore_room_from_backup_unlocked t client room_id with
                | Ok count -> Ok count
                | Error error -> Error (Share_encryption_error error)
              in
              let bundle = E.build_room_key_bundle t.machine ~room_id in
              if bundle.room_keys = [] && bundle.withheld = [] then
                Ok History_no_keys
              else
                let json =
                  match
                    Jsont_bytesrw.encode_string
                      Room_key_export.room_key_bundle_jsont bundle
                  with
                  | Ok json -> Ok json
                  | Error message ->
                      Error (Share_encryption_error (Error.Json_error message))
                in
                let* json = json in
                let* _uri, file =
                  match Media.upload_encrypted client ~data:json () with
                  | Ok uploaded -> Ok uploaded
                  | Error error -> Error (Share_media_error error)
                in
                let* queried =
                  match
                    Keys.query_keys client ~users:[ (recipient, []) ] ()
                  with
                  | Ok response -> Ok response
                  | Error error -> Error (Share_encryption_error error)
                in
                if queried.failures <> [] then
                  Error
                    (Share_encryption_error
                       (Error.Network_error
                          "recipient device-key query was incomplete"))
                else
                  (* A successful empty answer is authoritative. Injecting the
                     recipient tuple makes [receive_keys_query] discard devices
                     retained from an older query rather than using stale keys.
                     Do not fold a failed answer into the machine: its device
                     list and trust state are not authoritative. *)
                  let queried =
                    if
                      List.exists
                        (fun (user, _) ->
                          Matrix_proto.Id.User_id.equal user recipient)
                        queried.device_keys
                    then queried
                    else
                      {
                        queried with
                        device_keys = (recipient, []) :: queried.device_keys;
                      }
                  in
                  E.receive_keys_query t.machine queried;
                  let rec execute_required = function
                    | [] -> Ok ()
                    | request :: rest ->
                        let* () = execute_request t client request in
                        execute_required rest
                  in
                  let* () =
                    match
                      execute_required
                        (E.ensure_sessions t.machine ~members:[ recipient ])
                    with
                    | Ok () -> Ok ()
                    | Error error -> Error (Share_encryption_error error)
                  in
                  let* request =
                    match
                      E.share_room_key_bundle t.machine ~room_id ~recipient
                        ~file
                    with
                    | Ok request -> Ok request
                    | Error error -> Error (Share_encryption_error error)
                  in
                  (* This checkpoint covers both the fresh authoritative
                     device list and any claimed/session/Olm ratchet state.
                     It must happen even when there is no usable recipient
                     device and hence no request to send. *)
                  let* () =
                    match save t with
                    | Ok () -> Ok ()
                    | Error error -> Error (Share_encryption_error error)
                  in
                  match request with
                  | None -> Ok History_no_keys
                  | Some (E.Room_key_bundle_share { messages; _ } as request) ->
                      (* Olm encryption advanced the sender session ratchet;
                         the checkpoint above persists it before the network
                         send. [mark_sent] is a no-op for this request, so no
                         redundant post-send snapshot is required. *)
                      let* () =
                        match execute_request t client request with
                        | Ok () -> Ok ()
                        | Error error -> Error (Share_encryption_error error)
                      in
                      let count =
                        List.fold_left
                          (fun n (_, targets) -> n + List.length targets)
                          0 messages
                      in
                      Ok (History_shared count)
                  | Some _ -> Ok History_no_keys)))

let invite_user_by_id t client ~room_id ~user_id ?reason ~history_visibility ()
    =
  match
    share_room_history t client ~room_id ~recipient:user_id ~history_visibility
  with
  | Error error -> Error (Invite_share_error error)
  | Ok share_outcome -> (
      match Rooms.invite client ~room_id ~user_id ?reason () with
      | Ok () -> Ok (Invite_sent share_outcome)
      | Error error -> Error (Invite_request_error error))
