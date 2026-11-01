let path suffix =
  "/_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device" ^ suffix

let events_route = Route.v (path "/{device_id}/events")

let log_src =
  Logs.Src.create "matrix.dehydrated_device" ~doc:"MSC3814 dehydrated devices"

module Log = (val Logs.src_log log_src : Logs.LOG)

type t = { device_id : Matrix_proto.Id.Device_id.t; device_data : Jsont.json }

let pickle_key_secret_name = "org.matrix.msc3814"

module Pickle_key = struct
  type t = string
  type error = [ `Msg of string ]

  let generate ~random = Random.generate random 32
  let to_base64 t = Matrix_proto.Base64.encode t

  let of_base64 value =
    let invalid () =
      Error (`Msg "pickle key is not a valid 32-byte Base64 value")
    in
    match Matrix_proto.Base64.decode value with
    | Error _ -> invalid ()
    | Ok bytes when String.length bytes <> 32 -> invalid ()
    | Ok bytes -> Ok bytes
end

let t_jsont =
  Jsont.Object.(
    map (fun device_id device_data -> { device_id; device_data })
    |> mem "device_id" Matrix_proto.Id.Device_id.jsont ~enc:(fun (t : t) ->
        t.device_id)
    |> mem "device_data" Matrix_proto.Json.Codec.json ~enc:(fun (t : t) ->
        t.device_data)
    |> finish)

type device_id_response = { device_id : Matrix_proto.Id.Device_id.t }
[@@warning "-69"]

let device_id_response_jsont =
  Jsont.Object.(
    map (fun device_id -> { device_id })
    |> mem "device_id" Matrix_proto.Id.Device_id.jsont
         ~enc:(fun (t : device_id_response) -> t.device_id)
    |> finish)

let json_map_jsont = Json_codec.string_map Matrix_proto.Json.Codec.json

type put_request = {
  put_device_id : Matrix_proto.Id.Device_id.t;
  initial_device_display_name : string option;
  put_device_data : Jsont.json;
  device_keys : Jsont.json option;
  one_time_keys : (string * Jsont.json) list;
  fallback_keys : (string * Jsont.json) list;
}
[@@warning "-69"]

let put_request_jsont =
  Jsont.Object.(
    map
      (fun
        put_device_id
        initial_device_display_name
        put_device_data
        device_keys
        one_time_keys
        fallback_keys
      ->
        {
          put_device_id;
          initial_device_display_name;
          put_device_data;
          device_keys;
          one_time_keys;
          fallback_keys;
        })
    |> mem "device_id" Matrix_proto.Id.Device_id.jsont ~enc:(fun t ->
        t.put_device_id)
    |> opt_mem "initial_device_display_name" Matrix_proto.Json.Codec.string
         ~enc:(fun t -> t.initial_device_display_name)
    |> mem "device_data" Matrix_proto.Json.Codec.json ~enc:(fun t ->
        t.put_device_data)
    |> opt_mem "device_keys" Matrix_proto.Json.Codec.json ~enc:(fun t ->
        t.device_keys)
    |> mem "one_time_keys" json_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.one_time_keys)
         ~enc_omit:(fun value -> (( = ) []) value)
    |> mem "fallback_keys" json_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.fallback_keys)
         ~enc_omit:(fun value -> (( = ) []) value)
    |> finish)

type events = { next_batch : string option; events : Jsont.json list }

let events_jsont =
  Jsont.Object.(
    map (fun next_batch events -> { next_batch; events })
    |> opt_mem "next_batch" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : events) -> t.next_batch)
    |> mem "events"
         (Jsont.list Matrix_proto.Json.Codec.json)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : events) -> t.events)
    |> finish)

type events_request = { from : string option } [@@warning "-69"]

let events_request_jsont =
  Jsont.Object.(
    map (fun from -> { from })
    |> opt_mem "next_batch" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.from)
    |> finish)

let ( let* ) = Result.bind

type legacy_device_data = { algorithm : string; device_pickle : string }

let legacy_device_data_jsont =
  Jsont.Object.(
    map (fun algorithm device_pickle -> { algorithm; device_pickle })
    |> mem "algorithm" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.algorithm)
    |> mem "device_pickle" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.device_pickle)
    |> error_unknown |> finish)

let is_supported client =
  match Client.Http.get_absolute client ~path:(path "") () with
  | Ok _ -> Ok true
  | Error error -> (
      match Error.errcode error with
      | Some Error.M_NOT_FOUND -> Ok true
      | Some Error.M_UNRECOGNIZED -> Ok false
      | _ -> Error error)

let get client =
  let* reply = Client.Http.get_absolute client ~path:(path "") () in
  Client.Http.decode_response t_jsont reply

let is_absent_error error =
  match Error.errcode error with
  | Some Error.M_NOT_FOUND | Some Error.M_UNRECOGNIZED -> true
  | _ -> false

let get_if_present client =
  match get client with
  | Ok device -> Ok (Some device)
  | Error error when is_absent_error error -> Ok None
  | Error error -> Error error

let is_key_stored (store : Secrets.store) =
  Result.map Option.is_some
    (Secrets.get_store_secret store ~name:pickle_key_secret_name)

let load_key (store : Secrets.store) =
  let* value = Secrets.get_store_secret store ~name:pickle_key_secret_name in
  match value with
  | None -> Ok None
  | Some value ->
      Pickle_key.of_base64 value |> Result.map Option.some
      |> Result.map_error (fun (`Msg message) -> Error.Json_error message)

let cached_key (driver : Encryption_driver.t) =
  let machine = Encryption_driver.machine driver in
  match Encryption.dehydrated_pickle_key machine with
  | None -> Ok None
  | Some value ->
      Pickle_key.of_base64 value |> Result.map Option.some
      |> Result.map_error (fun (`Msg message) -> Error.Json_error message)

let cache_key driver key =
  let machine = Encryption_driver.machine driver in
  Encryption.set_dehydrated_pickle_key machine (Pickle_key.to_base64 key);
  Encryption_driver.save driver

let reset_key_with_driver (driver : Encryption_driver.t) (store : Secrets.store)
    ~random =
  let key = Pickle_key.generate ~random in
  let* () =
    Secrets.put_store_secret store ~random ~name:pickle_key_secret_name
      (Pickle_key.to_base64 key)
  in
  let* () = cache_key driver key in
  Ok key

let load_key_with_driver ?(create_if_missing = false) ?random
    (driver : Encryption_driver.t) (store : Secrets.store) =
  let* cached = cached_key driver in
  match cached with
  | Some key -> Ok (Some key)
  | None -> (
      let* fetched = load_key store in
      match fetched with
      | None when not create_if_missing -> Ok None
      | None -> (
          match random with
          | None ->
              Error
                (Error.Json_error
                   "creating a missing pickle key requires random entropy")
          | Some random ->
              let* key = reset_key_with_driver driver store ~random in
              Ok (Some key))
      | Some key ->
          let* () = cache_key driver key in
          Ok (Some key))

let reset_key (store : Secrets.store) ~random =
  let key = Pickle_key.generate ~random in
  let* () =
    Secrets.put_store_secret store ~random ~name:pickle_key_secret_name
      (Pickle_key.to_base64 key)
  in
  Ok key

let put client ~device_id ?initial_device_display_name ~device_data ?device_keys
    ?(one_time_keys = []) ?(fallback_keys = []) () =
  let request =
    {
      put_device_id = device_id;
      initial_device_display_name;
      put_device_data = device_data;
      device_keys;
      one_time_keys;
      fallback_keys;
    }
  in
  let* body = Client.Http.encode_body put_request_jsont request in
  let* reply = Client.Http.put_absolute client ~path:(path "") ~body () in
  let* r = Client.Http.decode_response device_id_response_jsont reply in
  Ok r.device_id

let put_and_remember (driver : Encryption_driver.t) client ~device_id
    ?initial_device_display_name ~device_data ?device_keys ?one_time_keys
    ?fallback_keys () =
  let* uploaded =
    put client ~device_id ?initial_device_display_name ~device_data ?device_keys
      ?one_time_keys ?fallback_keys ()
  in
  Encryption.set_last_uploaded_device_id
    (Encryption_driver.machine driver)
    uploaded;
  let* () = Encryption_driver.save driver in
  Ok uploaded

type create_event =
  | Created of Matrix_proto.Id.Device_id.t
  | Uploaded of Matrix_proto.Id.Device_id.t

let create_and_upload_with_callbacks ?(on_event = fun _ -> ())
    (driver : Encryption_driver.t) client ~private_identity ~pickle_key
    ?(initial_device_display_name = "Dehydrated device") ~random () =
  let machine = Encryption_driver.machine driver in
  let user_id = Encryption.user_id machine in
  if
    not
      (Matrix_proto.Id.User_id.equal user_id
         (Cross_signing.identity_user_id private_identity))
  then
    Error
      (Error.Json_error
         "dehydrated device identity does not belong to the encryption machine")
  else
    match Cross_signing.self_signing_secret private_identity with
    | None ->
        Error (Error.Json_error "dehydrated device self-signing key is missing")
    | Some self_signing ->
        let account = Olm.Account.create ~random () in
        let device_id =
          Matrix_proto.Id.Device_id.of_string
            (Crypto_key.Curve25519.Public.to_base64
               (Olm.Account.curve25519_key account))
        in
        let* device_id =
          Result.map_error
            (fun (`Msg message) -> Error.Json_error message)
            device_id
        in
        Olm.Account.generate_one_time_keys ~random account
          (Olm.Account.max_one_time_keys account);
        Olm.Account.generate_fallback_key ~random account;
        (* [Pickle_key.t] is already the validated 32-byte key. Avoid an
           encode/decode round trip with an otherwise partial failure path. *)
        let raw_pickle_key = pickle_key in
        let* device_pickle =
          Olm_dehydrated_pickle.pickle ~device_id ~pickle_key:raw_pickle_key
            account
          |> Result.map_error (fun (`Msg message) -> Error.Json_error message)
        in
        let encryption =
          Encryption.create_with_account ~random ~user_id ~device_id ~account ()
        in
        let device_keys =
          Encryption.device_keys_for_upload ~dehydrated:true encryption
          |> Cross_signing.sign_device_keys ~signer:self_signing
               ~signer_user_id:user_id
        in
        let device_key_id =
          Crypto_key.Key_id.of_device ~algorithm:"ed25519" device_id
        in
        let signed_key key signature =
          Keys.
            {
              key = Crypto_key.Curve25519.Public.to_base64 key;
              fallback = None;
              signatures = Some [ (user_id, [ (device_key_id, signature) ]) ];
            }
        in
        let one_time_keys =
          List.map
            (fun (key_id, key, signature) -> (key_id, signed_key key signature))
            (Olm.Account.signed_one_time_keys account)
        in
        let fallback_keys =
          match Olm.Account.fallback_key account with
          | None -> []
          | Some (key_id, key) ->
              let signature =
                Olm.Account.sign account
                  (Keys.one_time_key_signing_json ~fallback:true
                     (Crypto_key.Curve25519.Public.to_base64 key))
              in
              [
                ( key_id,
                  let value = signed_key key signature in
                  { value with Keys.fallback = Some true } );
              ]
        in
        let wire_keys keys =
          List.fold_left
            (fun encoded (key_id, key) ->
              let* encoded = encoded in
              let* json =
                Jsont.Json.encode Keys.one_time_key_jsont key
                |> Result.map_error (fun message -> Error.Json_error message)
              in
              Ok ((Crypto_key.Key_id.to_string key_id, json) :: encoded))
            (Ok []) keys
          |> Result.map List.rev
        in
        let device_data =
          Jsont.Json.object'
            [
              Jsont.Json.mem
                (Jsont.Json.name "algorithm")
                (Jsont.Json.string "org.matrix.msc3814.v1.olm");
              Jsont.Json.mem
                (Jsont.Json.name "device_pickle")
                (Jsont.Json.string device_pickle);
            ]
        in
        (* The local account and its wire representation are complete now, but
           the server has not accepted them yet. This is the same boundary as
           the Rust manager's [Created] notification. *)
        on_event (Created device_id);
        let* device_keys =
          Jsont.Json.encode Keys.device_keys_jsont device_keys
          |> Result.map_error (fun message -> Error.Json_error message)
        in
        let* one_time_keys = wire_keys one_time_keys in
        let* fallback_keys = wire_keys fallback_keys in
        let* uploaded =
          put_and_remember driver client ~device_id ~initial_device_display_name
            ~device_data ~device_keys ~one_time_keys ~fallback_keys ()
        in
        on_event (Uploaded uploaded);
        Ok uploaded

let create_and_upload (driver : Encryption_driver.t) client ~private_identity
    ~pickle_key ?initial_device_display_name ~random () =
  create_and_upload_with_callbacks driver client ~private_identity ~pickle_key
    ?initial_device_display_name ~random ()

let delete client =
  let* reply = Client.Http.delete_absolute client ~path:(path "") () in
  let* _ = Client.Http.decode_response device_id_response_jsont reply in
  Ok ()

let delete_if_present ?(on_deleted = fun () -> ()) client =
  match delete client with
  | Ok () ->
      on_deleted ();
      Ok ()
  | Error error when is_absent_error error -> Ok ()
  | Error error -> Error error

let get_events client ~device_id ?from () =
  let* body = Client.Http.encode_body events_request_jsont { from } in
  let path =
    Route.expand_exn events_route
      [ ("device_id", Matrix_proto.Id.Device_id.to_string device_id) ]
  in
  let* reply = Client.Http.post_absolute client ~path ~body () in
  Client.Http.decode_response events_jsont reply

type rehydrate_outcome = {
  device_id : Matrix_proto.Id.Device_id.t;
  room_keys_imported : int;
  to_device_events : int;
  delete_error : Error.t option;
}

type rehydrate_event =
  | Rehydration_started of Matrix_proto.Id.Device_id.t
  | Rehydration_progress of { room_keys_imported : int; to_device_events : int }
  | Rehydration_completed of {
      device_id : Matrix_proto.Id.Device_id.t;
      room_keys_imported : int;
      to_device_events : int;
    }

let max_rehydrate_events = 100_000

let raw_pickle_key key =
  (* Pickle_key.t is deliberately abstract to callers, but is represented by
     the 32 raw bytes returned by [of_base64]. Re-encoding and decoding keeps
     this conversion in one place and avoids exposing the representation. *)
  match Matrix_proto.Base64.decode (Pickle_key.to_base64 key) with
  | Ok value -> Ok value
  | Error _ -> Error (Error.Json_error "invalid dehydrated-device pickle key")

let saturating_add a b = if b > max_int - a then max_int else a + b

let rehydrate_with_max_events ?(on_event = fun _ -> ())
    ?(on_deleted = fun () -> ()) ~max_events (driver : Encryption_driver.t)
    client ~pickle_key ~random () =
  let primary = Encryption_driver.machine driver in
  let* device = get_if_present client in
  match device with
  | None -> Ok None
  | Some device -> (
      let downloaded_id = device.device_id in
      (match Encryption.last_uploaded_device_id primary with
      | Some uploaded_id
        when not (Matrix_proto.Id.Device_id.equal uploaded_id downloaded_id) ->
          Log.warn (fun m ->
              m
                "Downloaded dehydrated device id %a differs from last uploaded \
                 id %a; continuing"
                Matrix_proto.Id.Device_id.pp downloaded_id
                Matrix_proto.Id.Device_id.pp uploaded_id)
      | _ -> ());
      on_event (Rehydration_started downloaded_id);
      let* data =
        match Jsont.Json.decode legacy_device_data_jsont device.device_data with
        | Error message -> Error (Error.Json_error message)
        | Ok data
          when not (String.equal data.algorithm "org.matrix.msc3814.v1.olm") ->
            Error
              (Error.Json_error
                 (Printf.sprintf "unsupported dehydrated-device algorithm %S"
                    data.algorithm))
        | Ok data -> Ok data
      in
      let* raw_key = raw_pickle_key pickle_key in
      let* decoded =
        Olm_dehydrated_pickle.unpickle ~device_id:downloaded_id
          ~pickle_key:raw_key data.device_pickle
        |> Result.map_error (fun (`Msg message) -> Error.Json_error message)
      in
      let* temporary =
        Encryption_driver.create_with_account ~random
          ~user_id:(Encryption.user_id primary)
          ~device_id:downloaded_id ~account:decoded.account ()
      in
      let temporary_machine = Encryption_driver.machine temporary in
      let synthetic_response events : Matrix_proto.Sync.Response.t =
        {
          next_batch = "dehydrated";
          rooms = None;
          presence = None;
          account_data = None;
          to_device = Some { Matrix_proto.Sync.Raw_events.events };
          device_lists = None;
          device_one_time_keys_count = [];
          device_unused_fallback_key_types = None;
        }
      in
      let rec drain cursor imported_count event_count =
        let* page =
          get_events client ~device_id:downloaded_id ?from:cursor ()
        in
        let page_count = List.length page.events in
        if page_count = 0 then
          Ok
            {
              device_id = downloaded_id;
              room_keys_imported = imported_count;
              to_device_events = event_count;
              delete_error = None;
            }
        else
          let response = synthetic_response page.events in
          ignore (Encryption.process_sync temporary_machine response);
          let imported =
            Encryption.import_room_keys primary
              (Encryption.export_room_keys temporary_machine)
          in
          let imported_count =
            saturating_add imported_count imported.imported_count
          in
          let event_count = saturating_add event_count page_count in
          let* () = Encryption_driver.save driver in
          on_event
            (Rehydration_progress
               {
                 room_keys_imported = imported_count;
                 to_device_events = event_count;
               });
          if event_count >= max_events then
            Error
              (Error.Json_error
                 "dehydrated-device event limit exceeded while rehydrating")
          else
            match page.next_batch with
            | None ->
                Ok
                  {
                    device_id = downloaded_id;
                    room_keys_imported = imported_count;
                    to_device_events = event_count;
                    delete_error = None;
                  }
            | Some next when Option.exists (String.equal next) cursor ->
                Error
                  (Error.Json_error
                     "dehydrated-device events pagination repeated its cursor")
            | Some next -> drain (Some next) imported_count event_count
      in
      let* result = drain None 0 0 in
      on_event
        (Rehydration_completed
           {
             device_id = result.device_id;
             room_keys_imported = result.room_keys_imported;
             to_device_events = result.to_device_events;
           });
      match delete_if_present ~on_deleted client with
      | Ok () -> Ok (Some result)
      | Error error ->
          Log.warn (fun m ->
              m "Failed to delete rehydrated dehydrated device %a: %a"
                Matrix_proto.Id.Device_id.pp downloaded_id Error.pp error);
          Ok (Some { result with delete_error = Some error }))

let rehydrate driver client ~pickle_key ~random () =
  rehydrate_with_max_events ~max_events:max_rehydrate_events driver client
    ~pickle_key ~random ()

let rehydrate_with_callbacks ?on_deleted ~on_event driver client ~pickle_key
    ~random () =
  rehydrate_with_max_events ~on_event ?on_deleted
    ~max_events:max_rehydrate_events driver client ~pickle_key ~random ()
