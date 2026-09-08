module D = Matrix_client.Dehydrated_device

let unwrap context fn =
  Error.with_context context (fun () -> Error.unwrap (fn ()))

let pickle_key_secret_name = D.pickle_key_secret_name

module Pickle_key = D.Pickle_key

type t = D.t = {
  device_id : Matrix_proto.Id.Device_id.t;
  device_data : Jsont.json;
}

type events = D.events = {
  next_batch : string option;
  events : Jsont.json list;
}

type rehydrate_outcome = D.rehydrate_outcome = {
  device_id : Matrix_proto.Id.Device_id.t;
  room_keys_imported : int;
  to_device_events : int;
  delete_error : Matrix_client.Error.t option;
}

let get client =
  unwrap "fetching the dehydrated Matrix device" (fun () ->
      D.get (Client.base client))

let get_if_present client =
  unwrap "optionally fetching the dehydrated Matrix device" (fun () ->
      D.get_if_present (Client.base client))

let is_supported client =
  unwrap "probing dehydrated-device support" (fun () ->
      D.is_supported (Client.base client))

let put client ~device_id ?initial_device_display_name ~device_data ?device_keys
    ?one_time_keys ?fallback_keys () =
  unwrap "uploading a dehydrated Matrix device" (fun () ->
      D.put (Client.base client) ~device_id ?initial_device_display_name
        ~device_data ?device_keys ?one_time_keys ?fallback_keys ())

let put_and_remember driver client ~device_id ?initial_device_display_name
    ~device_data ?device_keys ?one_time_keys ?fallback_keys () =
  unwrap "uploading and recording a dehydrated Matrix device" (fun () ->
      D.put_and_remember driver (Client.base client) ~device_id
        ?initial_device_display_name ~device_data ?device_keys ?one_time_keys
        ?fallback_keys ())

let create_and_upload driver client ~private_identity ~pickle_key
    ?initial_device_display_name ~random () =
  unwrap "creating and uploading a dehydrated Matrix device" (fun () ->
      D.create_and_upload driver (Client.base client) ~private_identity
        ~pickle_key ?initial_device_display_name ~random ())

let rehydrate driver client ~pickle_key ~random () =
  unwrap "rehydrating a Matrix device" (fun () ->
      D.rehydrate driver (Client.base client) ~pickle_key ~random ())

let delete client =
  unwrap "deleting the dehydrated Matrix device" (fun () ->
      D.delete (Client.base client))

let delete_if_present client =
  unwrap "deleting the dehydrated Matrix device if present" (fun () ->
      D.delete_if_present (Client.base client))

let is_key_stored store =
  unwrap "checking the stored dehydrated-device key" (fun () ->
      D.is_key_stored store)

let load_key store =
  unwrap "loading the dehydrated-device key" (fun () -> D.load_key store)

let cached_key driver =
  unwrap "reading the cached dehydrated-device key" (fun () ->
      D.cached_key driver)

let load_key_with_driver ?create_if_missing ?random driver store =
  unwrap "loading the dehydrated-device key into encryption state" (fun () ->
      D.load_key_with_driver ?create_if_missing ?random driver store)

let reset_key store ~random =
  unwrap "resetting the dehydrated-device key" (fun () ->
      D.reset_key store ~random)

let reset_key_with_driver driver store ~random =
  unwrap "resetting the cached dehydrated-device key" (fun () ->
      D.reset_key_with_driver driver store ~random)

let get_events client ~device_id ?from () =
  unwrap "fetching dehydrated-device events" (fun () ->
      D.get_events (Client.base client) ~device_id ?from ())

module Manager = struct
  (** The Rust SDK calls this state stream's variants lifecycle events. Keep the
      payload small and stable here: applications generally only need the device
      id and cumulative rehydration counters. *)
  type event =
    | Created of Matrix_proto.Id.Device_id.t
    | Uploaded of Matrix_proto.Id.Device_id.t
    | Deleted
    | Key_cached
    | Rehydration_started of Matrix_proto.Id.Device_id.t
    | Rehydration_progress of {
        room_keys_imported : int;
        to_device_events : int;
      }
    | Rehydration_completed of {
        device_id : Matrix_proto.Id.Device_id.t;
        room_keys_imported : int;
        to_device_events : int;
      }
    | Rehydration_error of string
    | Rotation_error of string

  type callback = event -> unit
  type subscription = int

  type t = {
    driver : Encryption.t;
    client : Client.t;
    private_identity : Matrix_client.Cross_signing.private_identity;
    random : Matrix_client.Random.t;
    mutable next_subscription : int;
    mutable subscribers : (subscription * callback) list;
    mutable running : bool;
    mutable generation : int;
    mutable cancellation : Eio.Cancel.t option;
  }

  let create ?random ~encryption ~client ~private_identity () =
    {
      driver = encryption;
      client;
      private_identity;
      random =
        Option.value random
          ~default:(Matrix_client.Client.random (Client.base client));
      next_subscription = 0;
      subscribers = [];
      running = false;
      generation = 0;
      cancellation = None;
    }

  (* All manager operations, including [stop], are same-domain operations.
     This is deliberate: Eio cancellation contexts are domain-local and the
     manager does not add a cross-domain lock around the client's crypto
     machine. *)
  let subscribe t callback =
    let id = t.next_subscription in
    t.next_subscription <- id + 1;
    t.subscribers <- t.subscribers @ [ (id, callback) ];
    id

  let unsubscribe t subscription =
    t.subscribers <-
      List.filter (fun (id, _) -> id <> subscription) t.subscribers

  let emit t event =
    (* Snapshotting makes self-unsubscription and removal of another callback
       during notification deterministic. Exceptions from ordinary callbacks
       are isolated, while cancellation remains a control-flow signal. *)
    let callbacks = t.subscribers in
    List.iter
      (fun (id, callback) ->
        if List.exists (fun (current, _) -> current = id) t.subscribers then
          try callback event with
          | Eio.Cancel.Cancelled _ as exn ->
              let bt = Printexc.get_raw_backtrace () in
              Printexc.raise_with_backtrace exn bt
          | exn ->
              Logs.warn (fun m ->
                  m "dehydrated-device callback failed: %s"
                    (Printexc.to_string exn)))
      callbacks

  let stop t =
    t.running <- false;
    t.generation <- t.generation + 1;
    match t.cancellation with
    | None -> ()
    | Some cancellation ->
        t.cancellation <- None;
        Eio.Cancel.cancel cancellation Exit

  let event_of_create t = function
    | D.Created id -> emit t (Created id)
    | D.Uploaded id -> emit t (Uploaded id)

  let event_of_rehydrate t = function
    | D.Rehydration_started id -> emit t (Rehydration_started id)
    | D.Rehydration_progress { room_keys_imported; to_device_events } ->
        emit t (Rehydration_progress { room_keys_imported; to_device_events })
    | D.Rehydration_completed
        { device_id; room_keys_imported; to_device_events } ->
        emit t
          (Rehydration_completed
             { device_id; room_keys_imported; to_device_events })

  let create_now t key =
    unwrap "rotating the dehydrated Matrix device" (fun () ->
        D.create_and_upload_with_callbacks ~on_event:(event_of_create t)
          t.driver (Client.base t.client) ~private_identity:t.private_identity
          ~pickle_key:key ~random:t.random ())

  let rehydrate_now t key =
    unwrap "rehydrating a managed Matrix device" (fun () ->
        D.rehydrate_with_callbacks ~on_event:(event_of_rehydrate t)
          ~on_deleted:(fun () -> emit t Deleted)
          t.driver (Client.base t.client) ~pickle_key:key ~random:t.random ())

  let rotation_error t exn bt =
    match exn with
    | Eio.Cancel.Cancelled _ -> Printexc.raise_with_backtrace exn bt
    | exn -> emit t (Rotation_error (Printexc.to_string exn))

  let rotate_tick t =
    try
      match cached_key t.driver with
      | None -> emit t (Rotation_error "no cached pickle key for rotation")
      | Some key -> ignore (create_now t key)
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      rotation_error t exn bt

  let schedule t ~clock ~interval generation =
    Eio.Fiber.fork ~sw:(Client.switch t.client) (fun () ->
        Eio.Cancel.sub (fun cancellation ->
            if (not t.running) || t.generation <> generation then
              Eio.Cancel.cancel cancellation Exit
            else t.cancellation <- Some cancellation;
            Fun.protect
              ~finally:(fun () ->
                if t.generation = generation then t.cancellation <- None)
              (fun () ->
                try
                  while t.running && t.generation = generation do
                    Eio.Time.sleep clock interval;
                    if t.running && t.generation = generation then rotate_tick t
                  done
                with Eio.Cancel.Cancelled _ -> ())))

  let start ?(only_if_key_cached = false) ?(create_new_key = false)
      ?(skip_rehydration = false) ?(interval = 7. *. 24. *. 60. *. 60.) ~clock t
      ~store () =
    if interval <= 0. then
      invalid_arg "dehydrated-device interval must be positive";
    if only_if_key_cached && Option.is_none (cached_key t.driver) then ()
    else begin
      stop t;
      let had_cached_key = Option.is_some (cached_key t.driver) in
      let initial_key =
        if skip_rehydration then None
        else
          load_key_with_driver ~create_if_missing:false ~random:t.random
            t.driver store
      in
      (* [load_key_with_driver] persists an SSSS value into the local cache on
         this branch. Publish that boundary before any rehydration callbacks,
         matching the Rust manager's state-stream ordering. *)
      if (not had_cached_key) && Option.is_some initial_key then
        emit t Key_cached;
      let rehydrate_failed =
        if skip_rehydration then false
        else
          match initial_key with
          | None -> false
          | Some key -> (
              try
                ignore (rehydrate_now t key);
                false
              with
              | Eio.Cancel.Cancelled _ as exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  Printexc.raise_with_backtrace exn bt
              | exn ->
                  emit t (Rehydration_error (Printexc.to_string exn));
                  true)
      in
      let key =
        if create_new_key && not rehydrate_failed then (
          let key = reset_key_with_driver t.driver store ~random:t.random in
          emit t Key_cached;
          key)
        else
          match initial_key with
          | Some key -> key
          | None ->
              let key =
                load_key_with_driver ~create_if_missing:true ~random:t.random
                  t.driver store
                |> Option.get
              in
              emit t Key_cached;
              key
      in
      ignore (create_now t key);
      t.running <- true;
      t.generation <- t.generation + 1;
      schedule t ~clock ~interval t.generation
    end

  let delete t =
    stop t;
    unwrap "deleting the managed dehydrated Matrix device" (fun () ->
        D.delete_if_present
          ~on_deleted:(fun () -> emit t Deleted)
          (Client.base t.client))
end
