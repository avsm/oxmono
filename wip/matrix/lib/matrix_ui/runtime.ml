module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Crypto_key = Matrix_client.Crypto_key
module String_set = Set.Make (String)

type sync_state =
  | Not_started
  | Syncing
  | Live of { batch : string }
  | Failed of string
  | Offline
  | Stopped

type backfill_job = {
  room_id : Id.Room_id.t;
  generation : int;
  targets : (string, unit) Hashtbl.t;
  mutable handle : Back_pagination.handle option;
}

type t = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Std.r;
  client : Matrix_eio.Client.t;
  sync : Matrix_eio.Sync_service.t;
  encryption : Matrix_eio.Encryption.t option;
  verification : Matrix_eio.Verification_service.t option;
  event_cache : Event_cache.t;
  thread_info : Thread_info.t;
  thread_cache : Thread_cache.t;
  room_list : Room_list.t;
  room_identity : Room_identity.t;
  typing_users : (string, Id.User_id.t list Observable.Value.t) Hashtbl.t;
  typing_mutex : Eio.Mutex.t;
  send_queue : Matrix_client.Send_queue.t;
  sync_state : sync_state Observable.Value.t;
  recovery_manager : Matrix_client.Recovery.Manager.t option;
  recovery_state : Matrix_client.Recovery.state Observable.Value.t option;
  mutable recovery_subscription :
    Matrix_client.Recovery.Manager.subscription option;
  mutable decryption_subscription : (unit -> unit) option;
  mutable decryption_schedule : Id.Room_id.t -> unit;
  (* Serializes projection of a committed sync response with local room
     cleanup. It is deliberately separate from Sync_service's state lock: no
     user callback runs while either lock is held. *)
  projection_mutex : Eio.Mutex.t;
  (* The state whose room changes have reached the event-cache projection. *)
  mutable projected_state : Matrix_client.Base_client.state option;
  pinned_events : (string, Pinned_events.t) Hashtbl.t;
  thread_lists : (string, Thread_list.t) Hashtbl.t;
  timelines : (string, Room_timeline.t) Hashtbl.t;
  timeline_mutex : Eio.Mutex.t;
  mutable started : bool;
  mutable stop_requested : bool;
  mutable request_stop : (unit -> unit) option;
  room_key_request : Matrix_eio.Encryption.request -> unit;
  utd_hook : Utd_hook.t option;
  device_created_at : Ptime.t option;
  requested_room_keys : (string, unit) Hashtbl.t;
  mutable request_order : string Queue.t;
  back_pagination : Back_pagination.t;
  backfill_mutex : Eio.Mutex.t;
  backfills : (string, backfill_job) Hashtbl.t;
  mutable backfill_generation : int;
}

(* Raised inside the switch [start] owns to unwind it; never escapes. *)
exception Runtime_stopped

let event_cache t = t.event_cache
let thread_info t = t.thread_info
let thread_cache t = t.thread_cache

let pinned_events t room_id =
  let view, initial_refresh =
    Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
        let key = Id.Room_id.to_string room_id in
        match Hashtbl.find_opt t.pinned_events key with
        | Some view -> (view, false)
        | None ->
            let view =
              Pinned_events.create
                ~client:(Matrix_eio.Client.base t.client)
                ~event_cache:t.event_cache ~room_id ()
            in
            Hashtbl.add t.pinned_events key view;
            (view, true))
  in
  (if initial_refresh then
     match
       Pinned_events.refresh view ~state:(Matrix_eio.Sync_service.state t.sync)
     with
     | Ok () -> ()
     | Error error ->
         Logs.warn (fun m ->
             m "ui: initial pinned-event refresh for %s failed: %s"
               (Id.Room_id.to_string room_id)
               (Matrix_client.Error.to_string error)));
  view

let event_focused t room_id event_id ?limit ?thread_mode () =
  Event_focused.create
    ~client:(Matrix_eio.Client.base t.client)
    ~event_cache:t.event_cache ~thread_cache:t.thread_cache ~room_id ~event_id
    ?limit ?thread_mode ()

let thread_list t room_id =
  Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
      let key = Id.Room_id.to_string room_id in
      match Hashtbl.find_opt t.thread_lists key with
      | Some thread_list -> thread_list
      | None ->
          let thread_list =
            Thread_list.create
              ~client:(Matrix_eio.Client.base t.client)
              ~thread_info:t.thread_info ~event_cache:t.event_cache
              ~thread_cache:t.thread_cache ~room_id ()
          in
          Hashtbl.add t.thread_lists key thread_list;
          thread_list)

let room_list t = t.room_list
let send_queue t = t.send_queue
let sync_service t = t.sync
let sync_state t = t.sync_state
let recovery_state t = t.recovery_state
let room_identity t = t.room_identity

let typing_users t room_id =
  Eio.Mutex.use_rw ~protect:true t.typing_mutex (fun () ->
      let key = Id.Room_id.to_string room_id in
      match Hashtbl.find_opt t.typing_users key with
      | Some users -> users
      | None ->
          let users = Observable.Value.create [] in
          Hashtbl.add t.typing_users key users;
          users)

let forget_typing_users t room_id =
  Eio.Mutex.use_rw ~protect:true t.typing_mutex (fun () ->
      let key = Id.Room_id.to_string room_id in
      Option.iter
        (fun users -> Observable.Value.set users [])
        (Hashtbl.find_opt t.typing_users key);
      Hashtbl.remove t.typing_users key)

(* The room's [m.fully_read] marker, falling back to the latest of the own
   user's read receipts where the account has never set one. *)
let read_marker t room_id () =
  let receipts =
    Matrix_client.Base_client.receipts
      (Matrix_eio.Sync_service.state t.sync)
      room_id
  in
  match Matrix_client.Read_state.fully_read receipts with
  | Some event_id -> Some event_id
  | None -> Matrix_client.Read_state.latest_read receipts

let refresh_thread_cache ~state ~thread_info ~thread_cache room_id =
  let infos = Thread_info.snapshot thread_info room_id in
  Array.iter
    (fun (info : Thread_info.info) ->
      match info.root.event_id with
      | None -> ()
      | Some root_id ->
          Thread_cache.ingest_thread thread_cache ~room_id ~root_id
            ~events:(info.root :: Option.to_list info.latest_reply))
    infos;
  Thread_cache.set_room_receipts thread_cache ~room_id
    (Matrix_client.Base_client.receipts state room_id);
  Array.iter
    (fun (info : Thread_info.info) ->
      Option.iter
        (fun root_id ->
          Thread_cache.set_unread thread_cache ~room_id ~root_id info.unread)
        info.root.event_id)
    infos

let create ~sw ~clock ~client ~sync ?encryption ?recovery_manager ?verification
    ?event_store ?send_queue ?send_queue_store ?send_queue_media_store
    ?send_queue_media_owner ?on_room_key_request ?utd_hook ?device_created_at ()
    =
  let state = Matrix_eio.Sync_service.state sync in
  let send_queue =
    match send_queue with
    | Some queue ->
        if
          Option.is_some send_queue_store
          || Option.is_some send_queue_media_store
          || Option.is_some send_queue_media_owner
        then
          invalid_arg
            ("Matrix_ui.Runtime.create: send_queue construction arguments "
           ^ "cannot be combined with an explicit send_queue")
        else queue
    | None ->
        Matrix_client.Send_queue.create
          ~random:(Matrix_client.Client.random (Matrix_eio.Client.base client))
          ~user_id:(Matrix_eio.Client.user_id client)
          ?store:send_queue_store ?media_store:send_queue_media_store
          ?media_owner:send_queue_media_owner ()
  in
  let event_cache = Event_cache.create ?store:event_store () in
  let back_pagination =
    Back_pagination.create ~sw
      ~client:(Matrix_eio.Client.base client)
      ~event_cache ~max_concurrent:3 ()
  in
  let thread_info =
    Thread_info.create
      ?store:(Matrix_eio.Sync_service.store sync)
      ~user_id:(Matrix_client.Base_client.user_id state)
      ()
  in
  let thread_cache =
    Thread_cache.create ~event_cache
      ?store:(Matrix_eio.Sync_service.store sync)
      ()
  in
  Event_cache.track_send_queue event_cache send_queue;
  let room_list =
    Room_list.create event_cache (Matrix_eio.Sync_service.state sync)
  in
  let room_identity =
    Room_identity.create
      ~own_user:(Matrix_client.Base_client.user_id state)
      ?encryption ()
  in
  (* A runtime restored from a store must expose existing identity warnings
     before the first network response, just as the room list does. *)
  Room_identity.refresh room_identity state;
  Option.iter
    (fun manager ->
      ignore (Matrix_client.Recovery.Manager.refresh_from_base manager state))
    recovery_manager;
  let recovery_state =
    Option.map
      (fun manager ->
        Observable.Value.create (Matrix_client.Recovery.Manager.state manager))
      recovery_manager
  in
  let t =
    {
      sw;
      clock;
      client;
      sync;
      encryption;
      verification;
      event_cache;
      thread_info;
      thread_cache;
      room_list;
      room_identity;
      typing_users = Hashtbl.create 16;
      typing_mutex = Eio.Mutex.create ();
      send_queue;
      sync_state = Observable.Value.create Not_started;
      recovery_manager;
      recovery_state;
      recovery_subscription = None;
      decryption_subscription = None;
      decryption_schedule = (fun _ -> ());
      projection_mutex = Eio.Mutex.create ();
      projected_state = Some state;
      pinned_events = Hashtbl.create 16;
      thread_lists = Hashtbl.create 16;
      timelines = Hashtbl.create 16;
      timeline_mutex = Eio.Mutex.create ();
      started = false;
      stop_requested = false;
      request_stop = None;
      room_key_request =
        (match on_room_key_request with
        | Some f -> f
        | None -> (
            fun request ->
              match encryption with
              | None -> ()
              | Some encryption ->
                  Matrix_eio.Encryption.execute_requests encryption client
                    [ request ]));
      utd_hook;
      device_created_at;
      requested_room_keys = Hashtbl.create 128;
      request_order = Queue.create ();
      back_pagination;
      backfill_mutex = Eio.Mutex.create ();
      backfills = Hashtbl.create 16;
      backfill_generation = 0;
    }
  in
  (* Subscribe at construction so every physical decryption path using this
     shared cache is covered.  Scheduling is enabled by [start]; before then
     the restored-room pass provides the initial reconciliation. *)
  t.decryption_subscription <-
    Some
      (Event_cache.subscribe_physical_decryption event_cache (fun room_id ->
           t.decryption_schedule room_id));
  (* Restore receipt, unread and subscription fields before exposing the
     runtime. The normalized roots/counts came from the base-store slot, while
     event identity still comes from the shared event cache. *)
  List.iter
    (fun (room : Matrix_client.Base_client.room_info) ->
      Thread_info.refresh_room thread_info ~state ~room_id:room.room_id
        ~events:
          (Event_cache.snapshot event_cache room.room_id
          |> Array.to_list
          |> List.map Event_cache.effective);
      refresh_thread_cache ~state ~thread_info ~thread_cache room.room_id)
    (Matrix_client.Base_client.rooms state);
  let recovery_subscription =
    Option.bind recovery_manager (fun manager ->
        Option.map
          (fun recovery_state ->
            Matrix_client.Recovery.Manager.subscribe manager (fun value ->
                Observable.Value.set recovery_state value))
          recovery_state)
  in
  t.recovery_subscription <- recovery_subscription;
  Eio.Switch.on_release sw (fun () ->
      Option.iter (fun unsubscribe -> unsubscribe ()) t.decryption_subscription;
      t.decryption_subscription <- None;
      match (t.recovery_manager, t.recovery_subscription) with
      | Some manager, Some subscription ->
          Matrix_client.Recovery.Manager.unsubscribe manager subscription
      | _ -> ());
  Matrix_client.Send_queue.on_change send_queue (fun _ ->
      Room_list.refresh room_list (Matrix_eio.Sync_service.state sync));
  t

let plaintext_of (event : Event.Raw_event.t)
    (decrypted : Matrix_client.Encryption.decrypted_event) =
  {
    event with
    Event.Raw_event.type_ = Event.Event_type.of_string decrypted.decrypted_type;
    content = decrypted.decrypted_content;
  }

let request_key room_id session_id sender_key =
  let room_id = Id.Room_id.to_string room_id in
  let session_id = Id.Session_id.to_string session_id in
  let sender_key = Option.value sender_key ~default:"" in
  Printf.sprintf "%d:%s%d:%s%d:%s" (String.length room_id) room_id
    (String.length session_id) session_id (String.length sender_key) sender_key

let forget_room_key_requests t room_id session_id =
  let room_id = Id.Room_id.to_string room_id in
  let session_id = Id.Session_id.to_string session_id in
  let prefix =
    Printf.sprintf "%d:%s%d:%s" (String.length room_id) room_id
      (String.length session_id) session_id
  in
  Hashtbl.filter_map_inplace
    (fun key () -> if String.starts_with ~prefix key then None else Some ())
    t.requested_room_keys;
  (* Successful requests are normally followed by a fresh request only for a
     different session. Remove stale queue entries now as well, so repeated
     success cycles do not leave tombstones behind or evict a newly re-added
     identity by accident. *)
  let compact = Queue.create () in
  Queue.iter
    (fun key ->
      if Hashtbl.mem t.requested_room_keys key then Queue.push key compact)
    t.request_order;
  t.request_order <- compact

let forget_room_key_request t key =
  Hashtbl.remove t.requested_room_keys key;
  let compact = Queue.create () in
  Queue.iter
    (fun queued ->
      if Hashtbl.mem t.requested_room_keys queued then Queue.push queued compact)
    t.request_order;
  t.request_order <- compact

let receipt_targets state room_id =
  let receipts = Matrix_client.Base_client.receipts state room_id in
  let targets = ref String_set.empty in
  let add event_id =
    targets := String_set.add (Id.Event_id.to_string event_id) !targets
  in
  Option.iter
    (fun receipt -> add receipt.Matrix_client.Read_state.event_id)
    (Matrix_client.Read_state.public_read receipts);
  Option.iter
    (fun receipt -> add receipt.Matrix_client.Read_state.event_id)
    (Matrix_client.Read_state.private_read receipts);
  List.iter
    (fun thread_id ->
      Option.iter
        (fun receipt -> add receipt.Matrix_client.Read_state.event_id)
        (Matrix_client.Read_state.thread_public_read receipts ~thread_id);
      Option.iter
        (fun receipt -> add receipt.Matrix_client.Read_state.event_id)
        (Matrix_client.Read_state.thread_private_read receipts ~thread_id))
    (Matrix_client.Read_state.thread_ids receipts);
  String_set.elements !targets

let cache_has_target cache room_id target =
  Array.exists
    (fun (cached : Event_cache.event) ->
      match (Event_cache.effective cached).Event.Raw_event.event_id with
      | Some event_id -> String.equal target (Id.Event_id.to_string event_id)
      | None -> false)
    (Event_cache.snapshot cache room_id)

let inserted_has_target targets inserted =
  List.exists
    (fun (cached : Event_cache.event) ->
      match (Event_cache.effective cached).Event.Raw_event.event_id with
      | Some event_id -> Hashtbl.mem targets (Id.Event_id.to_string event_id)
      | None -> false)
    inserted

(* Recompute the room-wide count only from a physically complete suffix. A
   receipt target in the external [/event] registry is deliberately useless
   here: it has no timeline position and cannot establish which messages came
   after it. *)
let rec reconcile_unread_room t room_id =
  Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
      if t.stop_requested || Event_cache.is_forgotten t.event_cache room_id then
        ()
      else
        let state = Matrix_eio.Sync_service.state t.sync in
        (* Do not count a cache which has not been projected from this exact
         base state. A sync may have committed while this fiber was waiting. *)
        match t.projected_state with
        | None -> ()
        | Some projected when projected != state -> ()
        | Some _projected -> (
            match Matrix_client.Base_client.find_room state room_id with
            | None -> ()
            | Some _room ->
                let receipts =
                  Matrix_client.Base_client.receipts state room_id
                in
                let target_ids =
                  List.filter_map
                    (fun receipt ->
                      Option.map
                        (fun receipt ->
                          Id.Event_id.to_string
                            receipt.Matrix_client.Read_state.event_id)
                        receipt)
                    [
                      Matrix_client.Read_state.public_read receipts;
                      Matrix_client.Read_state.private_read receipts;
                    ]
                in
                let physical_events, physical_gaps =
                  Event_cache.snapshot_with_gaps t.event_cache room_id
                in
                let positions =
                  List.filter_map
                    (fun target ->
                      Array.find_index
                        (fun (cached : Event_cache.event) ->
                          cached.delivery = Event_cache.Synced
                          && Matrix_client.Read_state.is_main_timeline_event
                               (Event_cache.effective cached)
                          &&
                          match
                            (Event_cache.effective cached)
                              .Event.Raw_event.event_id
                          with
                          | Some event_id ->
                              String.equal target
                                (Id.Event_id.to_string event_id)
                          | None -> false)
                        physical_events)
                    target_ids
                in
                let complete =
                  if
                    target_ids <> []
                    && List.length positions < List.length target_ids
                    && Event_cache.has_unloaded_history t.event_cache room_id
                  then
                    (* A missing receipt target may still be in the lazy
                       persisted prefix.  Recounting only the resident suffix
                       would replace the server's unread counts using the
                       wrong horizon.  Hydration publishes another cache
                       change, which schedules a later reconciliation. *)
                    false
                  else
                    match positions with
                    | [] -> true
                    | _ ->
                        let last_target = List.fold_left max 0 positions in
                        (* A hole after the read horizon makes the suffix
                           incomplete. With no resolved receipt there is no
                           horizon, so Rust's known-window recount applies. *)
                        not
                          (List.exists
                             (fun gap -> gap.Event_cache.index > last_target)
                             physical_gaps)
                in
                if complete then begin
                  let events =
                    Array.to_list physical_events
                    |> List.filter (fun (cached : Event_cache.event) ->
                        cached.delivery = Event_cache.Synced)
                    |> List.map Event_cache.effective
                    |> List.filter
                         Matrix_client.Read_state.is_main_timeline_event
                  in
                  let notification event =
                    Matrix_client.Push_evaluator.notification_for_event
                      (Matrix_client.Base_client.ruleset state)
                      (Matrix_client.Base_client.push_context state room_id)
                      event
                  in
                  let counts =
                    Matrix_client.Read_state.count_unread
                      ~user_id:(Matrix_client.Base_client.user_id state)
                      ~notification receipts events
                  in
                  (* Validation and the conditional transition are one atomic
                     cache-then-service operation. Room-list observers run only
                     after the cache lock has been released. *)
                  try
                    match
                      Event_cache.with_snapshot_if_current t.event_cache room_id
                        physical_events physical_gaps (fun () ->
                          Matrix_eio.Sync_service
                          .set_local_unread_counts_if_current t.sync ~room_id
                            ~expected:state counts)
                    with
                    | None -> schedule_unread_reconciliation t room_id
                    | Some None -> schedule_unread_reconciliation t room_id
                    | Some (Some state') ->
                        t.projected_state <- Some state';
                        if state' != state then
                          Room_list.refresh t.room_list state'
                  with
                  | Eio.Cancel.Cancelled _ as exn ->
                      let bt = Printexc.get_raw_backtrace () in
                      Printexc.raise_with_backtrace exn bt
                  | Eio.Io _ as exn ->
                      let contextual =
                        Eio.Exn.add_context exn
                          "persisting unread-count reconciliation"
                      in
                      Logs.warn (fun m ->
                          m "ui: unread reconciliation for %s failed: %a"
                            (Id.Room_id.to_string room_id)
                            Eio.Exn.pp contextual)
                end))

and schedule_unread_reconciliation t room_id =
  if not t.stop_requested then
    Eio.Fiber.fork ~sw:t.sw (fun () ->
        Eio.Fiber.yield ();
        reconcile_unread_room t room_id)

let backfill_eligible t state room_id =
  if t.stop_requested || Event_cache.is_forgotten t.event_cache room_id then
    None
  else
    match Matrix_client.Base_client.find_room state room_id with
    | Some room ->
        if
          Option.is_some
            (Observable.Value.get
               (Event_cache.prev_batch t.event_cache room_id))
        then Some room
        else None
    | None -> None

let prune_backfill_targets t job =
  Hashtbl.filter_map_inplace
    (fun target () ->
      if cache_has_target t.event_cache job.room_id target then None
      else Some ())
    job.targets

let rec finish_backfill t job result =
  let retry =
    Eio.Mutex.use_rw ~protect:true t.backfill_mutex (fun () ->
        let key = Id.Room_id.to_string job.room_id in
        match Hashtbl.find_opt t.backfills key with
        | Some current ->
            if current != job then false
            else begin
              current.handle <- None;
              prune_backfill_targets t current;
              let state = Matrix_eio.Sync_service.state t.sync in
              let valid =
                current.generation = t.backfill_generation
                && (not t.stop_requested)
                && Option.is_some
                     (Matrix_client.Base_client.find_room state job.room_id)
                && not (Event_cache.is_forgotten t.event_cache job.room_id)
              in
              if not valid then (
                Hashtbl.remove t.backfills key;
                false)
              else if Hashtbl.length current.targets = 0 then false
              else if
                result.Back_pagination.reason <> Back_pagination.Stop_condition
              then false
              else
                match
                  Observable.Value.get
                    (Event_cache.prev_batch t.event_cache job.room_id)
                with
                | Some _ -> true
                | None ->
                    Hashtbl.remove t.backfills key;
                    false
            end
        | None -> false)
  in
  if not t.stop_requested then begin
    Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
        let state = Matrix_eio.Sync_service.state t.sync in
        if
          job.generation = t.backfill_generation
          && (not (Event_cache.is_forgotten t.event_cache job.room_id))
          && Option.is_some
               (Matrix_client.Base_client.find_room state job.room_id)
          && Eio.Mutex.use_ro t.backfill_mutex (fun () ->
              match
                Hashtbl.find_opt t.backfills (Id.Room_id.to_string job.room_id)
              with
              | Some current -> current == job
              | None -> false)
        then begin
          List.iter
            (fun (cached : Event_cache.event) ->
              match
                Thread_info.summary_of_root (Event_cache.effective cached)
              with
              | Thread_info.Known _, _ ->
                  Thread_info.ingest_root t.thread_info ~room_id:job.room_id
                    (Event_cache.effective cached)
              | (Thread_info.Unknown | Thread_info.Known_none), _ -> ())
            result.Back_pagination.events;
          Thread_info.refresh_room t.thread_info ~state ~room_id:job.room_id
            ~events:
              (Event_cache.snapshot t.event_cache job.room_id
              |> Array.to_list
              |> List.map Event_cache.effective);
          let open_timeline =
            Eio.Mutex.use_ro t.timeline_mutex (fun () ->
                Hashtbl.find_opt t.timelines (Id.Room_id.to_string job.room_id))
          in
          Option.iter Room_timeline.refresh open_timeline;
          Room_list.refresh t.room_list state;
          schedule_unread_reconciliation t job.room_id
        end)
  end;
  Eio.Mutex.use_rw ~protect:true t.backfill_mutex (fun () ->
      let key = Id.Room_id.to_string job.room_id in
      match Hashtbl.find_opt t.backfills key with
      | Some current when current == job ->
          let token =
            Observable.Value.get
              (Event_cache.prev_batch t.event_cache job.room_id)
          in
          if Hashtbl.length current.targets = 0 || Option.is_none token then
            Hashtbl.remove t.backfills key
      | Some _ | None -> ());
  if retry then begin
    let state = Matrix_eio.Sync_service.state t.sync in
    (* [schedule_backfill] coalesces this follow-up with a receipt which may
       have arrived while the page was in flight. *)
    schedule_backfill t state job.room_id
  end

and schedule_backfill t state room_id =
  match backfill_eligible t state room_id with
  | None -> ()
  | Some _ -> (
      let targets = receipt_targets state room_id in
      if targets <> [] then
        let key = Id.Room_id.to_string room_id in
        let launch =
          Eio.Mutex.use_rw ~protect:true t.backfill_mutex (fun () ->
              if t.stop_requested then `Skip
              else
                let job =
                  match Hashtbl.find_opt t.backfills key with
                  | Some job -> job
                  | None ->
                      let job =
                        {
                          room_id;
                          generation = t.backfill_generation;
                          targets = Hashtbl.create 8;
                          handle = None;
                        }
                      in
                      Hashtbl.add t.backfills key job;
                      job
                in
                List.iter
                  (fun target -> Hashtbl.replace job.targets target ())
                  targets;
                prune_backfill_targets t job;
                if Hashtbl.length job.targets = 0 then begin
                  let handle = job.handle in
                  Hashtbl.remove t.backfills key;
                  `Cancel handle
                end
                else if job.handle <> None then `None
                else
                  let request =
                    {
                      Back_pagination.room_id;
                      priority = Back_pagination.Normal;
                      batch_size = 30;
                      max_batches = Some 20;
                      stop =
                        (fun inserted ~reached_start:_ ->
                          Eio.Mutex.use_ro t.backfill_mutex (fun () ->
                              inserted_has_target job.targets inserted));
                    }
                  in
                  let handle =
                    Back_pagination.enqueue t.back_pagination request
                  in
                  job.handle <- Some handle;
                  `Launch (job, handle))
        in
        match launch with
        | `Skip | `None -> ()
        | `Cancel handle -> Option.iter Back_pagination.cancel handle
        | `Launch (job, handle) ->
            Eio.Fiber.fork ~sw:t.sw (fun () ->
                let result =
                  try Back_pagination.await handle
                  with Eio.Cancel.Cancelled _ ->
                    Back_pagination.cancel handle;
                    {
                      Back_pagination.reason = Back_pagination.Cancelled;
                      events = [];
                      batches = 0;
                    }
                in
                finish_backfill t job result))

let cancel_backfill t room_id =
  let handle =
    Eio.Mutex.use_rw ~protect:true t.backfill_mutex (fun () ->
        let key = Id.Room_id.to_string room_id in
        let handle =
          Option.bind (Hashtbl.find_opt t.backfills key) (fun job -> job.handle)
        in
        Hashtbl.remove t.backfills key;
        handle)
  in
  Option.iter Back_pagination.cancel handle

let remember_room_key_request t key =
  if Hashtbl.mem t.requested_room_keys key then false
  else (
    Hashtbl.add t.requested_room_keys key ();
    Queue.push key t.request_order;
    (* A server or a lost key can keep an event pending indefinitely. Keep
       this state bounded while retaining enough history to collapse ordinary
       duplicate observations. *)
    while Queue.length t.request_order > 1024 do
      let oldest = Queue.pop t.request_order in
      if Hashtbl.mem t.requested_room_keys oldest then
        Hashtbl.remove t.requested_room_keys oldest
    done;
    true)

let retry_room t room_id =
  match t.encryption with
  | None -> ()
  | Some encryption ->
      let requests = ref [] in
      let observe_unknown ~room_id ~session_id ~sender_key =
        (* A sender key is advisory on the wire, but when present it must
           be a real Curve25519 key before it can be used in a request. *)
        let parsed_sender_key =
          Option.bind sender_key (fun encoded ->
              Result.to_option (Crypto_key.Curve25519.Public.of_base64 encoded))
        in
        let sender_key_is_valid =
          Option.is_none sender_key || Option.is_some parsed_sender_key
        in
        if sender_key_is_valid then
          let key = request_key room_id session_id sender_key in
          if remember_room_key_request t key then
            let request =
              match parsed_sender_key with
              | None ->
                  Matrix_eio.Encryption.request_room_key encryption ~room_id
                    ~session_id ()
              | Some sender_key ->
                  Matrix_eio.Encryption.request_room_key encryption ~room_id
                    ~session_id ~sender_key ()
            in
            requests := (key, request) :: !requests
      in
      let observe encrypted =
        match
          Matrix_eio.Encryption.decrypt_room_event encryption room_id encrypted
        with
        | Ok decrypted ->
            forget_room_key_requests t room_id decrypted.decrypted_session_id;
            Option.iter
              (fun hook ->
                Option.iter (Utd_hook.on_late_decrypt hook) encrypted.event_id)
              t.utd_hook;
            ignore
              (Event_cache.set_decrypted t.event_cache room_id ~encrypted
                 ~plaintext:(plaintext_of encrypted decrypted))
        | Error (Matrix_client.Encryption.Unknown_session error) ->
            observe_unknown ~room_id:error.room_id ~session_id:error.session_id
              ~sender_key:error.sender_key
        | Error (Matrix_client.Encryption.Unknown_message_index error) ->
            observe_unknown ~room_id:error.room_id ~session_id:error.session_id
              ~sender_key:error.sender_key
        | Error _ -> ()
      in
      List.iter observe (Event_cache.undecrypted t.event_cache room_id);
      List.iter
        (fun (key, request) ->
          try t.room_key_request request
          with Eio.Io (Matrix_eio.Error.E _, _) as exn ->
            (* A failed transport must not poison the identity forever. *)
            forget_room_key_request t key;
            Logs.warn (fun m ->
                m "ui: room key request failed: %a" Eio.Exn.pp exn))
        (List.rev !requests)

(* A [Ciphertext_only] store — the default — persists the wire event and
   nothing else, so a reloaded room is a list of [m.room.encrypted] events
   with no plaintext beside them. Megolm keys outlive the process, so each
   successful sync retries those events, and opening a timeline retries its
   room immediately as well. *)
let recover_plaintext t room_id = retry_room t room_id

let timeline ?event_filter ?resolve_mxc t room_id =
  Eio.Mutex.use_rw ~protect:true t.timeline_mutex (fun () ->
      let key = Id.Room_id.to_string room_id in
      match Hashtbl.find_opt t.timelines key with
      | Some timeline -> timeline
      | None ->
          recover_plaintext t room_id;
          let timeline =
            Room_timeline.create ~sw:t.sw
              ~client:(Matrix_eio.Client.base t.client)
              ~send_queue:t.send_queue
              ?encryption:
                (Option.map Matrix_eio.Encryption.machine t.encryption)
              ~own_user:(Matrix_eio.Client.user_id t.client)
              ~read_state:(fun () ->
                Matrix_client.Base_client.receipts
                  (Matrix_eio.Sync_service.state t.sync)
                  room_id)
              ~read_marker:(read_marker t room_id) ?event_filter t.event_cache
              ?resolve_mxc room_id
          in
          Hashtbl.add t.timelines key timeline;
          timeline)

let remove_timeline ~discard t room_id =
  Eio.Mutex.use_rw ~protect:true t.timeline_mutex (fun () ->
      let key = Id.Room_id.to_string room_id in
      match Hashtbl.find_opt t.timelines key with
      | None -> ()
      | Some timeline ->
          Hashtbl.remove t.timelines key;
          if discard then Room_timeline.discard timeline
          else Room_timeline.close timeline)

let close_timeline t room_id = remove_timeline ~discard:false t room_id

let forget t room_id =
  match
    Matrix_client.Rooms.forget (Matrix_eio.Client.base t.client) ~room_id
  with
  | Error error -> Error error
  | Ok () ->
      cancel_backfill t room_id;
      (* Invalidate a response fetched before the server forget before the
         account-data GET/PUT below can yield. [forget_room] invalidates once
         more after all local state has been removed. *)
      Matrix_eio.Sync_service.begin_forget t.sync;
      (* Rust removes a forgotten room from the caller's [m.direct] account
         data only when its cached room was a direct message. This is
         best-effort: the room is still forgotten locally if account-data
         cleanup fails. *)
      let was_direct =
        Option.fold ~none:false
          ~some:(fun (info : Matrix_client.Base_client.room_info) -> info.is_dm)
          (Matrix_client.Base_client.find_room
             (Matrix_eio.Sync_service.state t.sync)
             room_id)
      in
      (if was_direct then
         match
           Matrix_client.Account_data.unmark_room_as_dm
             (Matrix_eio.Client.base t.client)
             ~room_id
         with
         | Ok () -> (
             try Matrix_eio.Sync_service.remove_direct_room t.sync room_id with
             | Eio.Cancel.Cancelled _ as exn ->
                 let bt = Printexc.get_raw_backtrace () in
                 Printexc.raise_with_backtrace exn bt
             | Eio.Io _ as exn ->
                 let contextual =
                   Eio.Exn.add_context exn "persisting forgotten-room removal"
                 in
                 Logs.warn (fun m ->
                     m
                       "ui: could not persist removal of forgotten room %s \
                        from m.direct: %a"
                       (Id.Room_id.to_string room_id)
                       Eio.Exn.pp contextual))
         | Error error ->
             Logs.warn (fun m ->
                 m "ui: could not remove forgotten room %s from m.direct: %s"
                   (Id.Room_id.to_string room_id)
                   (Matrix_client.Error.to_string error)));
      (* The server is authoritative: no local state is touched until its
         forget request succeeds. Remove store-backed state before projecting
         the deletion so a sync callback which was already committed either
         publishes first and is then cleared, or observes the absent room and
         skips it. *)
      (match Matrix_eio.Sync_service.store t.sync with
      | None -> ()
      | Some store -> (
          match
            Matrix_client.Thread_subscriptions.remove_room store ~room_id
          with
          | Ok () -> ()
          | Error error ->
              Logs.warn (fun m ->
                  m "ui: could not remove thread subscriptions for %s: %s"
                    (Id.Room_id.to_string room_id)
                    (Matrix_client.Error.to_string error))));
      Matrix_eio.Sync_service.forget_room t.sync room_id;
      Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
          Event_cache.forget_room t.event_cache room_id;
          Thread_cache.forget_room t.thread_cache room_id;
          Thread_info.remove_room t.thread_info room_id;
          let key = Id.Room_id.to_string room_id in
          Option.iter Pinned_events.close (Hashtbl.find_opt t.pinned_events key);
          Hashtbl.remove t.pinned_events key;
          Option.iter Thread_list.close (Hashtbl.find_opt t.thread_lists key);
          Hashtbl.remove t.thread_lists key;
          (* [Room_timeline.close] deliberately preserves its last snapshot.
             A forget is stronger than an ordinary close, so discard the open
             handle after clearing its cache and leave no readable room data
             behind. *)
          remove_timeline ~discard:true t room_id;
          forget_typing_users t room_id;
          let state = Matrix_eio.Sync_service.state t.sync in
          Room_list.refresh t.room_list state;
          Room_identity.refresh t.room_identity state;
          t.projected_state <- Some state);
      (* Tombstone the cache before notifying queue observers. In-flight
         callbacks and cancellation notifications then cannot recreate an
         echo; a callback which deliberately enqueues a new request may reopen
         the room, as documented by Event_cache. *)
      Matrix_client.Send_queue.forget_room t.send_queue room_id;
      (* [Send_queue.save] only updates its store in memory.  Flush here so a
         separately configured queue store cannot resurrect forgotten sends on
         restart.  This is deliberately one best-effort flush: when the queue
         shares the sync store, the same call also covers the sync mutation. *)
      (match Matrix_client.Send_queue.store t.send_queue with
      | None -> ()
      | Some store -> (
          match Matrix_client.Store.flush store with
          | Ok () -> ()
          | Error error ->
              Logs.warn (fun m ->
                  m "ui: could not flush forgotten send queue: %s"
                    (Matrix_client.Error.to_string error))));
      Ok ()

let close_timelines t =
  Eio.Mutex.use_rw ~protect:true t.timeline_mutex (fun () ->
      Hashtbl.iter (fun _ timeline -> Room_timeline.close timeline) t.timelines;
      Hashtbl.clear t.timelines)

let close_pinned_events t =
  Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
      Hashtbl.iter (fun _ view -> Pinned_events.close view) t.pinned_events;
      Hashtbl.clear t.pinned_events)

let close_thread_lists t =
  Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
      Hashtbl.iter
        (fun _ thread_list -> Thread_list.close thread_list)
        t.thread_lists;
      Hashtbl.clear t.thread_lists)

(* Straight to [Matrix_client] rather than to [Matrix_eio.Rooms], whose
   wrappers raise: a runtime is what a bot drives, and a bot answering an
   invite wants the [M_FORBIDDEN] as a value. The membership is not written
   into the sync state here, because the state belongs to [Sync_service] and
   a guessed one that a failed or racing sync then contradicted would be
   worse than a room list that is one response behind. *)
let pending_invite_acceptance t room_id =
  match t.encryption with
  | None -> None
  | Some encryption -> (
      match
        Matrix_client.Base_client.find_room
          (Matrix_eio.Sync_service.state t.sync)
          room_id
      with
      | Some { membership = Matrix_client.Base_client.Invited; _ } ->
          Option.map
            (fun inviter -> (encryption, inviter))
            (Matrix_client.Base_client.inviter
               (Matrix_eio.Sync_service.state t.sync)
               room_id)
      | Some _ | None -> None)

let join_room t ~room_id_or_alias ?(via = []) () =
  let resolved_room_id =
    match room_id_or_alias with
    | `Room_id room_id -> Ok room_id
    | `Room_alias alias ->
        Result.map
          (fun (info : Matrix_client.Directory.alias_info) -> info.room_id)
          (Matrix_client.Directory.resolve_alias
             (Matrix_eio.Client.base t.client)
             ~alias)
  in
  match resolved_room_id with
  | Error _ as error -> error
  | Ok resolved_room_id -> (
      let acceptance = pending_invite_acceptance t resolved_room_id in
      match
        Matrix_client.Rooms.join
          (Matrix_eio.Client.base t.client)
          ~room_id_or_alias ~via ()
      with
      | Error _ as error -> error
      | Ok joined_room_id -> (
          match acceptance with
          | None -> Ok ()
          | Some (encryption, inviter) ->
              Matrix_client.Encryption.record_invite_acceptance
                (Matrix_eio.Encryption.machine encryption)
                ~room_id:joined_room_id ~inviter;
              Matrix_client.Encryption_driver.save encryption))

let join t room_id = join_room t ~room_id_or_alias:(`Room_id room_id) ()

let leave t room_id =
  Matrix_client.Rooms.leave (Matrix_eio.Client.base t.client) ~room_id ()

let typing_update events =
  List.fold_left
    (fun latest event ->
      match Matrix_proto.Json.find_string "type" event with
      | Some "m.typing" -> (
          match Matrix_proto.Json.find_mem "content" event with
          | None -> latest
          | Some content -> (
              match Matrix_client.Typing.users_of_content content with
              | Ok users -> Some users
              | Error _ -> latest))
      | Some _ | None -> latest)
    None events

let apply_typing t (change : Matrix_client.Base_client.room_change) =
  match typing_update change.ephemeral with
  | None -> ()
  | Some users ->
      let own_user = Matrix_eio.Client.user_id t.client in
      let users =
        List.filter (fun user -> not (Id.User_id.equal own_user user)) users
      in
      Observable.Value.set (typing_users t change.changed_room_id) users

let refresh_pinned_events t state (changes : Matrix_client.Base_client.changes)
    =
  let room_ids =
    List.fold_left
      (fun ids (change : Matrix_client.Base_client.room_change) ->
        if List.exists (Id.Room_id.equal change.changed_room_id) ids then ids
        else change.changed_room_id :: ids)
      [] changes.room_changes
  in
  let views =
    Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
        List.filter_map
          (fun room_id ->
            Option.map
              (fun view -> (room_id, view))
              (Hashtbl.find_opt t.pinned_events (Id.Room_id.to_string room_id)))
          room_ids)
  in
  List.iter
    (fun (room_id, view) ->
      match Pinned_events.refresh view ~state with
      | Ok () -> ()
      | Error error ->
          Logs.warn (fun m ->
              m "ui: pinned-event refresh for %s failed: %s"
                (Id.Room_id.to_string room_id)
                (Matrix_client.Error.to_string error)))
    views

let apply_changes t _committed_state
    (changes : Matrix_client.Base_client.changes) =
  let state, changes =
    Eio.Mutex.use_rw ~protect:true t.projection_mutex (fun () ->
        (* The state may have been locally changed after Sync_service committed but
     before this callback acquired the projection lock. Never let such an old
     room delta reopen a room which has already been forgotten; unrelated
     changes from the same committed response remain publishable. *)
        let state = Matrix_eio.Sync_service.state t.sync in
        let changes =
          {
            changes with
            room_changes =
              List.filter
                (fun (change : Matrix_client.Base_client.room_change) ->
                  Option.is_some
                    (Matrix_client.Base_client.find_room state
                       change.changed_room_id))
                changes.room_changes;
          }
        in
        List.iter (apply_typing t) changes.room_changes;
        (* A bundled summary is meaningful only on a thread root.  Roots
           discovered by the thread-list service are inserted directly into
           [Thread_info]; sync contributes summaries and replies here. *)
        List.iter
          (fun (change : Matrix_client.Base_client.room_change) ->
            List.iter
              (fun event ->
                match Thread_info.summary_of_root event with
                | Thread_info.Known _, _ ->
                    Thread_info.ingest_root t.thread_info
                      ~room_id:change.changed_room_id event
                | (Thread_info.Unknown | Thread_info.Known_none), _ -> ())
              change.timeline)
          changes.room_changes;
        List.iter
          (Event_cache.apply_room_change t.event_cache)
          changes.room_changes;
        List.iter
          (fun (change : Matrix_client.Base_client.room_change) ->
            Thread_cache.ingest t.thread_cache ~room_id:change.changed_room_id
              ~events:change.timeline)
          changes.room_changes;
        List.iter
          (fun (change : Matrix_client.Base_client.room_change) ->
            Thread_info.refresh_room t.thread_info ~state
              ~room_id:change.changed_room_id
              ~events:
                (Event_cache.snapshot t.event_cache change.changed_room_id
                |> Array.to_list
                |> List.map Event_cache.effective))
          changes.room_changes;
        List.iter
          (fun (change : Matrix_client.Base_client.room_change) ->
            refresh_thread_cache ~state ~thread_info:t.thread_info
              ~thread_cache:t.thread_cache change.changed_room_id)
          changes.room_changes;
        Option.iter
          (fun hook ->
            List.iter
              (fun (change : Matrix_client.Base_client.room_change) ->
                List.iter
                  (fun ((event : Event.Raw_event.t), error) ->
                    match event.event_id with
                    | None -> ()
                    | Some _ ->
                        let context =
                          match t.encryption with
                          | None -> Matrix_client.Encryption.default_utd_context
                          | Some encryption ->
                              Matrix_client.Encryption.utd_context
                                (Matrix_eio.Encryption.machine encryption)
                                error
                        in
                        let cause =
                          Matrix_client.Encryption.classify_utd event error
                            context
                        in
                        Utd_hook.on_utd hook ~event ~cause
                          ?event_local_age:
                            (Option.map
                               (fun created ->
                                 Int64.sub
                                   (Event.Timestamp.to_ms event.origin_server_ts)
                                   (Event.Timestamp.to_ms
                                      (Event.Timestamp.of_ptime created)))
                               t.device_created_at)
                          ~user_trusts_own_identity:
                            context.local_device_verified ())
                  change.undecrypted)
              changes.room_changes)
          t.utd_hook;
        (* The cache is merged before retrying: this covers both events from this
     response and undecrypted history loaded from disk. *)
        let rooms =
          List.fold_left
            (fun rooms room_id ->
              if List.exists (Id.Room_id.equal room_id) rooms then rooms
              else room_id :: rooms)
            []
            (List.map
               (fun (c : Matrix_client.Base_client.room_change) ->
                 c.changed_room_id)
               changes.room_changes
            @ List.map
                (fun (room : Matrix_client.Store.room_info) -> room.room_id)
                (Matrix_client.Base_client.rooms state))
        in
        List.iter (retry_room t) rooms;
        (* The read marker lives in the receipts, not in the event cache, so a
     response that moved one has to ask the room's timeline to re-project. *)
        List.iter
          (fun (change : Matrix_client.Base_client.room_change) ->
            if change.ephemeral <> [] || change.room_account_data <> [] then
              match
                Hashtbl.find_opt t.timelines
                  (Id.Room_id.to_string change.changed_room_id)
              with
              | Some timeline -> Room_timeline.refresh timeline
              | None -> ())
          changes.room_changes;
        Room_list.refresh t.room_list state;
        Room_identity.refresh t.room_identity state;
        if not t.stop_requested then
          Observable.Value.set t.sync_state (Live { batch = changes.batch });
        t.projected_state <- Some state;
        (state, changes))
  in
  (* Recovery subscribers are caller code and must not run while the runtime's
     projection lock is held. *)
  refresh_pinned_events t state changes;
  Option.iter
    (fun manager ->
      ignore (Matrix_client.Recovery.Manager.refresh_from_base manager state))
    t.recovery_manager;
  List.iter
    (fun (change : Matrix_client.Base_client.room_change) ->
      schedule_backfill t state change.changed_room_id)
    changes.room_changes;
  List.iter
    (fun (change : Matrix_client.Base_client.room_change) ->
      schedule_unread_reconciliation t change.changed_room_id)
    changes.room_changes;
  (state, changes)

(* The same backoff [Sync_service.run] uses by default: 500 ms doubling to
   a 60 s ceiling, reset on the first success. *)
let min_backoff = 0.5
let max_backoff = 60.0

let start ?params ?(on_change = fun _ _ -> ()) ?on_error ?on_encryption_error t
    =
  if t.started then invalid_arg "Matrix_ui.Runtime.start: already started";
  t.started <- true;
  t.stop_requested <- false;
  Observable.Value.set t.sync_state Syncing;
  let stop_promise, stop_resolver = Eio.Promise.create () in
  t.request_stop <-
    Some
      (fun () ->
        t.stop_requested <- true;
        if not (Eio.Promise.is_resolved stop_promise) then
          Eio.Promise.resolve stop_resolver ());
  t.decryption_schedule <-
    (fun room_id -> schedule_unread_reconciliation t room_id);
  List.iter
    (fun (room : Matrix_client.Base_client.room_info) ->
      schedule_backfill t (Matrix_eio.Sync_service.state t.sync) room.room_id)
    (Matrix_client.Base_client.rooms (Matrix_eio.Sync_service.state t.sync));
  List.iter
    (fun (room : Matrix_client.Base_client.room_info) ->
      schedule_unread_reconciliation t room.room_id)
    (Matrix_client.Base_client.rooms (Matrix_eio.Sync_service.state t.sync));
  let backoff = ref min_backoff in
  (* Every failed request is published as [Failed], whatever is decided
     about retrying, so that a wedged token or an unreachable server is
     visible rather than an indefinite [Syncing]. A retry then publishes
     [Offline] until it succeeds and puts the state back to [Live] through
     [apply_changes]; a decision to stop is [Stopped], because the loop will
     not come back by itself. *)
  let handle_error error =
    if t.stop_requested then Matrix_eio.Sync_service.Stop
    else (
      Observable.Value.set t.sync_state
        (Failed (Format.asprintf "%a" Matrix_eio.Error.pp_err error));
      let control =
        match on_error with
        | Some on_error -> on_error error
        | None ->
            if Matrix_eio.Error.is_retryable error then (
              let delay = !backoff in
              backoff := Float.min max_backoff (delay *. 2.);
              Matrix_eio.Sync_service.Retry_after delay)
            else Matrix_eio.Sync_service.Stop
      in
      (match control with
      | Matrix_eio.Sync_service.Stop ->
          if not t.stop_requested then Observable.Value.set t.sync_state Stopped;
          if not (Eio.Promise.is_resolved stop_promise) then
            Eio.Promise.resolve stop_resolver ()
      | Matrix_eio.Sync_service.Continue | Matrix_eio.Sync_service.Retry_after _
        ->
          if not t.stop_requested then Observable.Value.set t.sync_state Offline);
      if t.stop_requested then Matrix_eio.Sync_service.Stop else control)
  in
  (* The services run under a switch of the runtime's own, so that
     {!stop} can cancel exactly the fibers it forked and leave the switch
     given to {!create} — timelines, and whatever the caller put on it —
     alone. *)
  Eio.Fiber.fork ~sw:t.sw (fun () ->
      (try
         Eio.Switch.run (fun services ->
             Matrix_eio.Send_queue.start ~sw:services ~clock:t.clock
               ?encryption:t.encryption
               ~members:(Matrix_eio.Sync_service.members t.sync)
               t.client t.send_queue;
             Matrix_eio.Sync_service.run ~sw:services ~clock:t.clock t.client
               t.sync ?params ?encryption:t.encryption
               ?verification:t.verification ~on_error:handle_error
               ?on_encryption_error
               ~on_change:(fun state changes ->
                 backoff := min_backoff;
                 let state, changes = apply_changes t state changes in
                 on_change state changes)
               ();
             Eio.Promise.await stop_promise;
             Eio.Switch.fail services Runtime_stopped)
       with Runtime_stopped -> ());
      Observable.Value.set t.sync_state Stopped)

let stop t =
  let was_started = t.started in
  (match t.request_stop with Some request -> request () | None -> ());
  if was_started then begin
    t.decryption_schedule <- (fun _ -> ());
    t.backfill_generation <- t.backfill_generation + 1;
    let handles =
      Eio.Mutex.use_rw ~protect:true t.backfill_mutex (fun () ->
          let handles =
            Hashtbl.to_seq_values t.backfills
            |> Seq.filter_map (fun job -> job.handle)
            |> List.of_seq
          in
          Hashtbl.clear t.backfills;
          handles)
    in
    List.iter Back_pagination.cancel handles;
    Back_pagination.close t.back_pagination
  end;
  close_timelines t;
  close_pinned_events t;
  close_thread_lists t;
  Thread_cache.close t.thread_cache;
  Observable.Value.set t.sync_state Stopped
