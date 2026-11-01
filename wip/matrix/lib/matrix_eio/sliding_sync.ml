module S = Matrix_client.Sliding_sync
module Request = Matrix_proto.Sliding_sync.Request
module Response = Matrix_proto.Sliding_sync.Response

exception Request_changed

module Controller = struct
  type active_request = { cancel : Eio.Cancel.t; mutable invalidated : bool }
  type active_wait = { cancel : Eio.Cancel.t }

  type t = {
    mutex : Eio.Mutex.t;
    mutable request : Request.t;
    mutable active_request : active_request option;
    mutable claimed : bool;
    mutable next_presence_wakeup : int;
    mutable presence_wakeup : int option;
    (* This epoch covers all effective local changes which are allowed to
       interrupt a loop.  It is separate from the request itself: callers
       may deliberately defer cancellation, in which case the request can
       change while a retry sleep remains undisturbed. *)
    mutable wakeup_generation : int;
    mutable active_wait : active_wait option;
  }

  let compare_room_id left right =
    String.compare
      (Matrix_proto.Id.Room_id.to_string left)
      (Matrix_proto.Id.Room_id.to_string right)

  let normalise_subscriptions subscriptions =
    let subscriptions =
      List.fold_left
        (fun subscriptions (room_id, settings) ->
          let without_room =
            List.filter
              (fun (candidate, _) ->
                not (Matrix_proto.Id.Room_id.equal candidate room_id))
              subscriptions
          in
          (room_id, settings) :: without_room)
        [] subscriptions
    in
    List.sort
      (fun (left, _) (right, _) -> compare_room_id left right)
      subscriptions

  let equal_settings (left : Request.room_subscription)
      (right : Request.room_subscription) =
    left.timeline_limit = right.timeline_limit
    && List.equal Matrix_proto.Sliding_sync.Required_state.equal
         left.required_state right.required_state

  let equal_subscription (left_id, left_settings) (right_id, right_settings) =
    Matrix_proto.Id.Room_id.equal left_id right_id
    && equal_settings left_settings right_settings

  let equal_subscriptions = List.equal equal_subscription

  let create request =
    let room_subscriptions =
      normalise_subscriptions request.Request.room_subscriptions
    in
    {
      mutex = Eio.Mutex.create ();
      request = { request with Request.room_subscriptions };
      active_request = None;
      claimed = false;
      next_presence_wakeup = 0;
      presence_wakeup = None;
      wakeup_generation = 0;
      active_wait = None;
    }

  let request t = Eio.Mutex.use_ro t.mutex (fun () -> t.request)

  let mutate ?(cancel_in_flight_request = true)
      ?(force = fun _current _updated -> false) t update =
    let cancel =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          let current = t.request.Request.room_subscriptions in
          let room_subscriptions = normalise_subscriptions (update current) in
          if
            (not (force current room_subscriptions))
            && equal_subscriptions current room_subscriptions
          then None
          else begin
            t.request <- { t.request with Request.room_subscriptions };
            if cancel_in_flight_request then begin
              t.wakeup_generation <- t.wakeup_generation + 1;
              match t.active_request with
              | Some active ->
                  active.invalidated <- true;
                  Some active.cancel
              | None -> Option.map (fun wait -> wait.cancel) t.active_wait
            end
            else None
          end)
    in
    Option.iter (fun cancel -> Eio.Cancel.cancel cancel Request_changed) cancel

  (* Presence callbacks are registered on the client, whose unregister
     operation permits a callback already copied by a concurrent setter to run
     afterwards.  Keep a generation in the controller so such a stale callback
     cannot cancel a later loop when this controller is reused. *)
  let begin_presence_wakeup t =
    let token =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          let token = t.next_presence_wakeup in
          t.next_presence_wakeup <- token + 1;
          t.presence_wakeup <- Some token;
          token)
    in
    let wake () =
      let cancel =
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
            match t.presence_wakeup with
            | Some current when current = token -> (
                t.wakeup_generation <- t.wakeup_generation + 1;
                match t.active_request with
                | Some active ->
                    active.invalidated <- true;
                    Some active.cancel
                | None -> Option.map (fun wait -> wait.cancel) t.active_wait)
            | Some _ | None -> None)
      in
      Option.iter
        (fun cancel -> Eio.Cancel.cancel cancel Request_changed)
        cancel
    in
    let finish () =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          match t.presence_wakeup with
          | Some current when current = token -> t.presence_wakeup <- None
          | Some _ | None -> ())
    in
    (wake, finish)

  let settings ?(required_state = []) ?(timeline_limit = 10) () :
      Request.room_subscription =
    { required_state; timeline_limit }

  let replace room_id settings subscriptions =
    if
      List.exists
        (fun (candidate, _) -> Matrix_proto.Id.Room_id.equal candidate room_id)
        subscriptions
    then
      List.map
        (fun ((candidate, _) as subscription) ->
          if Matrix_proto.Id.Room_id.equal candidate room_id then
            (candidate, settings)
          else subscription)
        subscriptions
    else (room_id, settings) :: subscriptions

  let add_room_subscriptions ?cancel_in_flight_request ?required_state
      ?timeline_limit ~room_ids t =
    let settings = settings ?required_state ?timeline_limit () in
    mutate ?cancel_in_flight_request t (fun subscriptions ->
        List.fold_left
          (fun subscriptions room_id -> replace room_id settings subscriptions)
          subscriptions room_ids)

  let remove_room_subscriptions ?cancel_in_flight_request ~room_ids t =
    mutate ?cancel_in_flight_request t (fun subscriptions ->
        List.filter
          (fun (room_id, _) ->
            not (List.exists (Matrix_proto.Id.Room_id.equal room_id) room_ids))
          subscriptions)

  let subscriptions ?required_state ?timeline_limit room_ids =
    let settings = settings ?required_state ?timeline_limit () in
    List.map (fun room_id -> (room_id, settings)) room_ids

  let set_room_subscriptions ?cancel_in_flight_request ?required_state
      ?timeline_limit ~room_ids t =
    mutate ?cancel_in_flight_request t (fun _ ->
        subscriptions ?required_state ?timeline_limit room_ids)

  let reset_and_add_room_subscriptions ?cancel_in_flight_request ?required_state
      ?timeline_limit ~room_ids t =
    mutate ?cancel_in_flight_request
      ~force:(fun current _updated -> current <> [] || room_ids <> [])
      t
      (fun _ -> subscriptions ?required_state ?timeline_limit room_ids)

  type 'a poll_result = Restart | Result of 'a
  type wait_result = Restart_wait | Elapsed

  let wakeup_generation t =
    Eio.Mutex.use_ro t.mutex (fun () -> t.wakeup_generation)

  (* Wait for an [on_error] retry without losing a local change which lands
     between the failed poll and installing the timer.  The expected epoch is
     captured before the poll; installing the wait and checking the epoch are
     one mutex-protected operation.  Thus a change either cancels this wait or
     is observed before the sleep starts. *)
  let retry_after t ~expected_generation ~clock delay =
    try
      Eio.Cancel.sub (fun cancel ->
          Eio.Cancel.check cancel;
          let already_woken =
            Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                if t.wakeup_generation <> expected_generation then true
                else begin
                  t.active_wait <- Some { cancel };
                  false
                end)
          in
          if already_woken then Restart_wait
          else begin
            Fun.protect
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                        match t.active_wait with
                        | Some active when active.cancel == cancel ->
                            t.active_wait <- None
                        | Some _ | None -> ())))
              (fun () ->
                Eio.Time.sleep clock delay;
                let stale =
                  Eio.Mutex.use_ro t.mutex (fun () ->
                      t.wakeup_generation <> expected_generation)
                in
                if stale then Restart_wait else Elapsed)
          end)
    with Eio.Cancel.Cancelled Request_changed ->
      Eio.Fiber.check ();
      Restart_wait

  let poll t f =
    let active, result =
      try
        Eio.Cancel.sub (fun cancel ->
            let request, active =
              Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                  let active = { cancel; invalidated = false } in
                  t.active_request <- Some active;
                  (t.request, active))
            in
            Fun.protect
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                        match t.active_request with
                        | Some current when current == active ->
                            t.active_request <- None
                        | Some _ | None -> ())))
              (fun () ->
                Eio.Cancel.check cancel;
                let result = f request in
                Eio.Cancel.check cancel;
                (active, result)))
      with Eio.Cancel.Cancelled Request_changed ->
        Eio.Fiber.check ();
        raise Request_changed
    in
    let invalidated = Eio.Mutex.use_ro t.mutex (fun () -> active.invalidated) in
    if invalidated then Restart else Result result

  let poll t f = try poll t f with Request_changed -> Restart

  let update_to_device_since t since =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if Request.to_device_enabled t.request then
          t.request <- Request.with_to_device_since ~since t.request)

  let clear_room_subscriptions t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.request.Request.room_subscriptions <> [] then
          t.request <- Request.clear_room_subscriptions t.request)

  let claim t =
    let already_claimed =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          if t.claimed then true
          else begin
            t.claimed <- true;
            false
          end)
    in
    if already_claimed then
      invalid_arg "Matrix_eio.Sliding_sync.Controller is already driving a loop"

  let release t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.claimed <- false)
end

module Own_profile = struct
  type t = {
    user_id : Matrix_proto.Id.User_id.t;
    mutable current : Matrix_client.Profile.profile option;
    mutable next_subscription : int;
    subscribers : (int, Matrix_client.Profile.profile option -> unit) Hashtbl.t;
  }

  type subscription = int

  let profile_src =
    Logs.Src.create "matrix.eio.sliding_sync.profile"
      ~doc:"Sliding-sync own profile projection"

  module Profile_log = (val Logs.src_log profile_src : Logs.LOG)

  let create ~user_id =
    {
      user_id;
      current = None;
      next_subscription = 0;
      subscribers = Hashtbl.create 4;
    }

  let current t = t.current
  let user_id t = t.user_id

  let equal_profile left right =
    match (left, right) with
    | None, None -> true
    | ( Some (left : Matrix_client.Profile.profile),
        Some (right : Matrix_client.Profile.profile) ) ->
        left.displayname = right.displayname
        && Option.equal Matrix_client.Media.Mxc.equal left.avatar_url
             right.avatar_url
        && left.fields = right.fields
    | _ -> false

  let notify t value =
    Hashtbl.to_seq_values t.subscribers
    |> List.of_seq
    |> List.iter (fun callback ->
        try callback value with
        | Eio.Cancel.Cancelled _ as exn ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace exn bt
        | exn ->
            Profile_log.err (fun m ->
                m "sliding sync own-profile subscriber failed: %s"
                  (Printexc.to_string exn)))

  let set t value =
    if not (equal_profile t.current value) then begin
      t.current <- value;
      notify t value
    end

  let profile_of_fields fields =
    let string_field name =
      Option.bind (List.assoc_opt name fields) Matrix_proto.Json.as_string
    in
    let avatar_url =
      Option.bind (string_field "avatar_url") (fun value ->
          match Matrix_client.Media.Mxc.of_string value with
          | Ok value -> Some value
          | Error _ -> None)
    in
    let fields =
      List.filter
        (fun (name, _) -> name <> "displayname" && name <> "avatar_url")
        fields
    in
    {
      Matrix_client.Profile.displayname = string_field "displayname";
      avatar_url;
      fields;
    }

  let refresh t state =
    let value =
      match Matrix_client.Base_client.find_profile state t.user_id with
      | None -> None
      | Some fields -> Some (profile_of_fields fields)
    in
    set t value

  let refresh_base t state =
    let value =
      match Matrix_client.Base_client.find_profile state t.user_id with
      | None -> None
      | Some fields -> Some (profile_of_fields fields)
    in
    set t value

  let subscribe t callback =
    let subscription = t.next_subscription in
    t.next_subscription <- subscription + 1;
    Hashtbl.replace t.subscribers subscription callback;
    (try callback t.current with
    | Eio.Cancel.Cancelled _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Hashtbl.remove t.subscribers subscription;
        Printexc.raise_with_backtrace exn bt
    | exn ->
        Profile_log.err (fun m ->
            m "sliding sync own-profile subscriber failed: %s"
              (Printexc.to_string exn)));
    subscription

  let unsubscribe t subscription = Hashtbl.remove t.subscribers subscription

  let watch t callback =
    let subscription = subscribe t callback in
    fun () -> unsubscribe t subscription
end

let src = Logs.Src.create "matrix.eio.sliding_sync" ~doc:"Sliding sync loop"

module Log = (val Logs.src_log src : Logs.LOG)

let path = S.path
let default_timeout_ms = S.default_timeout_ms
let native_feature = S.native_feature
let is_available_in = S.is_available_in

let is_available client =
  Error.unwrap ~context:"probing Matrix sliding-sync support"
    (S.is_available (Client.base client))

let sync_once client ?pos ?timeout_ms ?set_presence request =
  Error.unwrap ~context:"performing a Matrix sliding-sync request"
    (S.sync_once (Client.base client) ?pos ?timeout_ms ?set_presence request)

let is_unsupported = function
  | Error.Matrix { errcode = Matrix_client.Error.M_UNRECOGNIZED; _ } -> true
  | Error.Http { status = 404; _ } -> true
  | _ -> false

let is_expired_pos = function
  | Error.Matrix
      { errcode = Matrix_client.Error.M_UNKNOWN_CODE "M_UNKNOWN_POS"; _ } ->
      true
  | _ -> false

let refresh_own_profile_base own_profile state =
  Option.iter
    (fun observer -> Own_profile.refresh_base observer state)
    own_profile

(* Store operations normally report [Error.t], but a disk backend may raise an
   Eio exception while flushing. Keep such failures on the loop's retry path
   too, rather than letting them escape the fiber. *)
let protect_store f =
  try f ()
  with Eio.Io _ as exn ->
    let exn = Eio.Exn.add_context exn "updating sliding-sync state storage" in
    Error (Matrix_client.Error.Network_error (Fmt.str "%a" Eio.Exn.pp exn))

(* A failed service-store commit leaves both the published state and its
   cursor unchanged, so it is safe to pass that failure through the loop's
   retry policy. Exceptions raised after [on_committed] starts are callback or
   hook failures and retain the usual behaviour of failing the owning switch. *)
let protect_service_before_commit committed f =
  try Ok (f ())
  with Eio.Io (Error.E error, _) when not !committed -> Error error

let with_presence_wakeup client controller set_presence f =
  let unregister_presence_wakeup =
    match set_presence with
    | Some _ -> fun () -> ()
    | None -> (
        let wake, finish = Controller.begin_presence_wakeup controller in
        try
          let unregister = Client.register_presence_wakeup client wake in
          fun () ->
            Eio.Cancel.protect (fun () ->
                (* Invalidate the controller generation before removing the
                    client listener. The client deliberately allows a callback
                    already copied by a concurrent setter to run. *)
                finish ();
                unregister ())
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          finish ();
          Printexc.raise_with_backtrace exn bt)
  in
  Fun.protect ~finally:unregister_presence_wakeup f

let loop_service ~clock client ~service ?initial_pos
    ?(timeout_ms = S.default_timeout_ms) ?(txn_id = false) ?set_presence
    ?thread_subscription_store ?own_profile ?encryption ?verification
    ?on_encryption_error ?(on_change = fun _ _ -> ()) ~callbacks controller =
  let random = Matrix_client.Client.random (Client.base client) in
  let rec go pos =
    Eio.Fiber.check ();
    let expected_generation = Controller.wakeup_generation controller in
    let again pos = function
      | Sync.Continue -> go pos
      | Sync.Stop -> ()
      | Sync.Retry_after delay -> (
          match
            Controller.retry_after controller ~expected_generation ~clock delay
          with
          | Controller.Restart_wait -> go pos
          | Controller.Elapsed -> go pos)
    in
    let generation = Sync_service.generation service in
    let response =
      Controller.poll controller (fun request ->
          let request =
            if txn_id then
              Request.with_txn_id
                ~txn_id:(Some (Matrix_client.Random.txn_id random))
                request
            else request
          in
          ( request,
            Error.with_context "polling Matrix sliding sync" (fun () ->
                S.sync_once (Client.base client) ?pos ~timeout_ms ?set_presence
                  request) ))
    in
    match response with
    | Controller.Restart -> go pos
    | Controller.Result (request, Error e) ->
        if is_expired_pos (Error.of_client_error e) then begin
          Controller.clear_room_subscriptions controller;
          let committed = ref false in
          match
            protect_service_before_commit committed (fun () ->
                Sync_service.reset_sliding_session service;
                committed := true)
          with
          | Error error -> again pos (callbacks.Sync.on_error error)
          | Ok () ->
              Controller.update_to_device_since controller None;
              refresh_own_profile_base own_profile (Sync_service.state service);
              again None (callbacks.Sync.on_error (Error.of_client_error e))
        end
        else again pos (callbacks.Sync.on_error (Error.of_client_error e))
    | Controller.Result (request, Ok response) -> (
        let committed = ref false in
        let before_commit =
          match thread_subscription_store with
          | None -> None
          | Some store ->
              Some
                (fun () ->
                  match
                    protect_store (fun () ->
                        Matrix_client.Thread_subscriptions
                        .apply_sliding_extension store ~previous_pos:pos
                          response.extensions.thread_subscriptions)
                  with
                  | Ok () -> ()
                  | Error error ->
                      Error.raise_client_error
                        ~context:
                          "applying thread subscriptions from sliding sync"
                        error)
        in
        match
          protect_service_before_commit committed (fun () ->
              Sync_service.apply_sliding_if_current generation
                ~to_device_enabled:(Request.to_device_enabled request)
                ?before_commit ?encryption ?verification ?on_encryption_error
                ~on_committed:(fun state changes ->
                  committed := true;
                  on_change state changes)
                client service response)
        with
        | Error error -> again pos (callbacks.Sync.on_error error)
        | Ok None -> go pos
        | Ok (Some _) ->
            refresh_own_profile_base own_profile (Sync_service.state service);
            (match
               ( Request.to_device_enabled request,
                 Response.to_device_next_batch response )
             with
            | true, (Some _ as since) ->
                Controller.update_to_device_since controller since
            | _ -> ());
            again (Some response.pos) (callbacks.Sync.on_response response))
  in
  let rec prepare () =
    let committed = ref false in
    match
      protect_service_before_commit committed (fun () ->
          let migrated = Sync_service.migrate_legacy_sliding_state service in
          committed := true;
          (migrated, Sync_service.state service))
    with
    | Error error -> (
        match callbacks.Sync.on_error error with
        | Sync.Continue -> prepare ()
        | Sync.Stop -> None
        | Sync.Retry_after delay ->
            Eio.Time.sleep clock delay;
            prepare ())
    | Ok (migrated, state) -> Some (migrated, state)
  in
  with_presence_wakeup client controller set_presence (fun () ->
      match prepare () with
      | None -> ()
      | Some (migrated, initial_state) ->
          let initial_pos =
            match Matrix_client.Base_client.sliding_pos initial_state with
            | Some pos -> Some pos
            | None -> if migrated then None else initial_pos
          in
          let request = Controller.request controller in
          if Request.to_device_enabled request then
            Controller.update_to_device_since controller
              (Matrix_client.Base_client.sliding_to_device_since initial_state);
          refresh_own_profile_base own_profile initial_state;
          go initial_pos)

let loop ~clock client ?initial_pos ?timeout_ms ?txn_id ?set_presence
    ?thread_subscription_store ?state_store ?own_profile ?profile_service
    ?service ?encryption ?verification ?on_encryption_error ?on_change
    ~callbacks controller =
  match service with
  | Some service ->
      (match (state_store, profile_service) with
      | None, None -> ()
      | _ ->
          invalid_arg
            "Matrix_eio.Sliding_sync: service is incompatible with state_store \
             and profile_service");
      (match (own_profile, Client.session client) with
      | Some observer, Some session
        when not
               (Matrix_proto.Id.User_id.equal
                  (Own_profile.user_id observer)
                  session.user_id) ->
          invalid_arg
            "Matrix_eio.Sliding_sync: own_profile belongs to another user"
      | _ -> ());
      (match Client.session client with
      | Some session
        when not
               (Matrix_proto.Id.User_id.equal
                  (Matrix_client.Base_client.user_id
                     (Sync_service.state service))
                  session.user_id) ->
          invalid_arg "Matrix_eio.Sliding_sync: service belongs to another user"
      | Some _ | None -> ());
      loop_service ~clock client ~service ?initial_pos ?timeout_ms ?txn_id
        ?set_presence ?thread_subscription_store ?own_profile ?encryption
        ?verification ?on_encryption_error ?on_change ~callbacks controller
  | None ->
      (* Keep the old optional arguments as a source-compatible entry point,
         but never run the old [Sliding_sync_state] fold.  A supplied
         [profile_service] is itself the canonical service (the old name was
         unfortunate); otherwise make a private service over [state_store].
         This means a legacy snapshot is migrated by [loop_service] and then
         only the Base_client projection is folded and persisted. *)
      let service =
        match profile_service with
        | Some service -> (
            match state_store with
            | Some store
              when match Sync_service.store service with
                   | Some service_store -> service_store != store
                   | None -> true ->
                invalid_arg
                  "Matrix_eio.Sliding_sync: profile_service and state_store \
                   must refer to the same Store.t when used as the \
                   compatibility service"
            | _ -> service)
        | None -> (
            let user_id = Client.user_id client in
            match state_store with
            | Some store -> Sync_service.of_store ~store ~user_id ()
            | None -> Sync_service.of_user ~user_id ())
      in
      (match (own_profile, Client.session client) with
      | Some observer, Some session
        when not
               (Matrix_proto.Id.User_id.equal
                  (Own_profile.user_id observer)
                  session.user_id) ->
          invalid_arg
            "Matrix_eio.Sliding_sync: own_profile belongs to another user"
      | _ -> ());
      (match Client.session client with
      | Some session
        when not
               (Matrix_proto.Id.User_id.equal
                  (Matrix_client.Base_client.user_id
                     (Sync_service.state service))
                  session.user_id) ->
          invalid_arg
            "Matrix_eio.Sliding_sync: compatibility service belongs to another \
             user"
      | Some _ | None -> ());
      loop_service ~clock client ~service ?initial_pos ?timeout_ms ?txn_id
        ?set_presence ?thread_subscription_store ?own_profile ?encryption
        ?verification ?on_encryption_error ?on_change ~callbacks controller

let sync_forever_controlled ~sw ~clock client ?initial_pos ?timeout_ms ?txn_id
    ?set_presence ?thread_subscription_store ?state_store ?own_profile
    ?profile_service ?service ?encryption ?verification ?on_encryption_error
    ?on_change ~callbacks controller =
  Controller.claim controller;
  try
    Eio.Fiber.fork ~sw (fun () ->
        Fun.protect
          ~finally:(fun () ->
            Eio.Cancel.protect (fun () -> Controller.release controller))
          (fun () ->
            loop ~clock client ?initial_pos ?timeout_ms ?txn_id ?set_presence
              ?thread_subscription_store ?state_store ?own_profile
              ?profile_service ?service ?encryption ?verification
              ?on_encryption_error ?on_change ~callbacks controller))
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Controller.release controller;
    Printexc.raise_with_backtrace exn bt

let sync_forever ~sw ~clock client ?initial_pos ?timeout_ms ?txn_id
    ?set_presence ?thread_subscription_store ?state_store ?own_profile
    ?profile_service ?service ?encryption ?verification ?on_encryption_error
    ?on_change ~callbacks request =
  sync_forever_controlled ~sw ~clock client ?initial_pos ?timeout_ms ?txn_id
    ?set_presence ?thread_subscription_store ?state_store ?own_profile
    ?profile_service ?service ?encryption ?verification ?on_encryption_error
    ?on_change ~callbacks
    (Controller.create request)

let sync_to_stream ~sw ~clock client ~stream ?initial_pos ?timeout_ms ?txn_id
    ?set_presence ?thread_subscription_store ?state_store ?own_profile
    ?profile_service ?service ?encryption ?verification ?on_encryption_error
    ?on_change ?on_error request =
  sync_forever ~sw ~clock client ?initial_pos ?timeout_ms ?txn_id ?set_presence
    ?thread_subscription_store ?state_store ?own_profile ?profile_service
    ?service ?encryption ?verification ?on_encryption_error ?on_change
    ~callbacks:
      (Sync.callbacks ?on_error
         ~on_response:(fun r ->
           Eio.Stream.add stream r;
           Sync.Continue)
         ())
    request

let create_sync_stream ~sw ~clock client ?(capacity = 10) ?initial_pos
    ?timeout_ms ?txn_id ?set_presence ?thread_subscription_store ?state_store
    ?own_profile ?profile_service ?service ?encryption ?verification
    ?on_encryption_error ?on_change ?on_error request =
  let stream = Eio.Stream.create capacity in
  sync_to_stream ~sw ~clock client ~stream ?initial_pos ?timeout_ms ?txn_id
    ?set_presence ?thread_subscription_store ?state_store ?own_profile ?on_error
    ?profile_service ?service ?encryption ?verification ?on_encryption_error
    ?on_change request;
  stream
