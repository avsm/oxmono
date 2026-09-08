module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Read_state = Matrix_client.Read_state

type pagination_token = Matrix_client.Paginator.pagination_token =
  | Not_started
  | Has_more of string
  | Hit_end

type pagination = { backward : pagination_token; forward : pagination_token }

type snapshot = {
  room_id : Id.Room_id.t;
  root_id : Id.Event_id.t;
  root : Event.Raw_event.t option;
  replies : Event.Raw_event.t list;
  events : Event.Raw_event.t list;
  pagination : pagination;
  receipts : Read_state.t;
  unread : Read_state.counts;
}

type persisted = {
  room_id : Id.Room_id.t;
  root_id : Id.Event_id.t;
  event_ids : Id.Event_id.t list option;
  backward : pagination_token;
  forward : pagination_token;
  receipts : Read_state.t option;
  unread : Read_state.counts option;
}

type persisted_token = { token_kind : string; token : string option }

let persisted_token_jsont : persisted_token Jsont.t =
  Jsont.Object.(
    map (fun token_kind token -> { token_kind; token })
    |> mem "kind" Matrix_proto.Json.Codec.string ~enc:(fun x -> x.token_kind)
    |> opt_mem "token" Matrix_proto.Json.Codec.string ~enc:(fun x -> x.token)
    |> finish)

let token_of_wire token =
  match (token.token_kind, token.token) with
  | "not_started", None -> Not_started
  | "hit_end", None -> Hit_end
  | "has_more", Some token -> Has_more token
  | _ -> Jsont.Error.msg Jsont.Meta.none "invalid thread pagination token"

let wire_of_token = function
  | Not_started -> { token_kind = "not_started"; token = None }
  | Hit_end -> { token_kind = "hit_end"; token = None }
  | Has_more token -> { token_kind = "has_more"; token = Some token }

let token_jsont : pagination_token Jsont.t =
  let object_jsont =
    Jsont.map ~dec:token_of_wire ~enc:wire_of_token persisted_token_jsont
  in
  let string_jsont =
    Jsont.map
      ~dec:(fun token -> Has_more token)
      ~enc:(function
        | Has_more token -> token
        | _ -> Jsont.Error.msg Jsont.Meta.none "token is not Has_more")
      Matrix_proto.Json.Codec.string
  in
  Jsont.any ~dec_string:string_jsont ~dec_object:object_jsont
    ~enc:(function Has_more _ -> string_jsont | _ -> object_jsont)
    ()

let counts_jsont : Read_state.counts Jsont.t =
  Jsont.Object.(
    map (fun unread notifications highlights ->
        ({ unread; notifications; highlights } : Read_state.counts))
    |> mem "unread" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun x ->
        x.Read_state.unread)
    |> mem "notifications" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun x ->
        x.Read_state.notifications)
    |> mem "highlights" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun x ->
        x.Read_state.highlights)
    |> finish)

let persisted_jsont : persisted Jsont.t =
  Jsont.Object.(
    map (fun room_id root_id event_ids backward forward receipts unread ->
        { room_id; root_id; event_ids; backward; forward; receipts; unread })
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun x -> x.room_id)
    |> mem "root_id" Id.Event_id.jsont ~enc:(fun x -> x.root_id)
    |> opt_mem "event_ids" (Jsont.list Id.Event_id.jsont) ~enc:(fun x ->
        x.event_ids)
    |> mem "backward" token_jsont
         ~dec_absent:(fun () -> Not_started)
         ~enc:(fun x -> x.backward)
    |> mem "forward" token_jsont
         ~dec_absent:(fun () -> Not_started)
         ~enc:(fun x -> x.forward)
    |> opt_mem "receipts" Read_state.jsont ~enc:(fun x -> x.receipts)
    |> opt_mem "unread" counts_jsont ~enc:(fun x -> x.unread)
    |> finish)

type persisted_state = { format_version : int; entries : persisted list }

let persisted_state_jsont : persisted_state Jsont.t =
  Jsont.Object.(
    map (fun format_version entries -> { format_version; entries })
    |> mem "format_version" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun x ->
        x.format_version)
    |> mem "entries" (Jsont.list persisted_jsont) ~enc:(fun x -> x.entries)
    |> finish)

let slot = Matrix_client.Store.Slot.v ~name:"thread_cache" persisted_state_jsont

type entry = {
  room_id : Id.Room_id.t;
  root_id : Id.Event_id.t;
  mutable pagination : pagination;
  mutable event_ids : Id.Event_id.t list;
  mutable receipts : Read_state.t;
  mutable unread : Read_state.counts;
}

type subscription_kind = Events | Receipts | Unread

type subscription = {
  active : bool Atomic.t;
  kind : subscription_kind;
  callback : snapshot -> unit;
  room_id : Id.Room_id.t;
  root_id : Id.Event_id.t;
}

type t = {
  event_cache : Event_cache.t;
  store : Matrix_client.Store.t option;
  mutex : Eio.Mutex.t;
  entries : (string, entry) Hashtbl.t;
  subscriptions : (int, subscription) Hashtbl.t;
  mutable next_subscription : int;
  mutable closed : bool;
  forget_subscriptions : (string, unit -> unit) Hashtbl.t;
}

let key room_id root_id =
  Id.Room_id.to_string room_id ^ "\000" ^ Id.Event_id.to_string root_id

let root_id_of_event event = event.Event.Raw_event.event_id
let first_some left right = match left with Some _ -> left | None -> right

let thread_relation event =
  match
    Matrix_proto.Json.find_mem "m.relates_to" event.Event.Raw_event.content
  with
  | None -> None
  | Some json -> (
      match Jsont.Json.decode Event.Relates_to.jsont json with
      | Ok relation
        when Event.Rel_type.equal relation.rel_type Event.Rel_type.Thread ->
          Some relation.event_id
      | Ok _ | Error _ -> None)

let has_thread_summary event =
  match event.Event.Raw_event.unsigned with
  | None -> false
  | Some unsigned -> (
      match Event.Unsigned.relations unsigned with
      | None -> false
      | Some relations ->
          Option.is_some (Matrix_proto.Json.find_mem "m.thread" relations))

let callback_snapshot t kind (entry : entry) =
  Hashtbl.to_seq_values t.subscriptions
  |> Seq.filter_map (fun subscription ->
      if
        subscription.kind = kind
        && Id.Room_id.equal entry.room_id subscription.room_id
        && Id.Event_id.equal entry.root_id subscription.root_id
      then Some subscription
      else None)
  |> List.of_seq

let publish subscriptions snapshot =
  List.iter
    (fun subscription ->
      if Atomic.get subscription.active then
        try subscription.callback snapshot with
        | Eio.Cancel.Cancelled _ as exn ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace exn bt
        | exn ->
            Logs.warn (fun m ->
                m "ui: thread-cache listener failed: %s"
                  (Printexc.to_string exn)))
    subscriptions

let persist t =
  match t.store with
  | None -> ()
  | Some store -> (
      let savepoint = Matrix_client.Store.snapshot store in
      let entries =
        Hashtbl.to_seq_values t.entries
        |> Seq.map (fun (entry : entry) ->
            {
              room_id = entry.room_id;
              root_id = entry.root_id;
              event_ids = Some entry.event_ids;
              backward = entry.pagination.backward;
              forward = entry.pagination.forward;
              receipts = Some entry.receipts;
              unread = Some entry.unread;
            })
        |> List.of_seq
        |> List.sort (fun (left : persisted) right ->
            let by_room = Id.Room_id.compare left.room_id right.room_id in
            if by_room <> 0 then by_room
            else Id.Event_id.compare left.root_id right.root_id)
      in
      let rollback () = Matrix_client.Store.restore store savepoint in
      match
        Matrix_client.Store.Slot.set store slot { format_version = 1; entries }
      with
      | Error error ->
          rollback ();
          Logs.warn (fun m ->
              m "ui: thread-cache metadata save failed: %s"
                (Matrix_client.Error.to_string error))
      | Ok () -> (
          try
            match Matrix_client.Store.flush store with
            | Ok () -> ()
            | Error error ->
                rollback ();
                Logs.warn (fun m ->
                    m "ui: thread-cache metadata flush failed: %s"
                      (Matrix_client.Error.to_string error))
          with
          | Eio.Cancel.Cancelled _ as exn ->
              let bt = Printexc.get_raw_backtrace () in
              rollback ();
              Printexc.raise_with_backtrace exn bt
          | Eio.Io _ as exn ->
              rollback ();
              let contextual =
                Eio.Exn.add_context exn "flushing thread-cache metadata"
              in
              Logs.warn (fun m ->
                  m "ui: thread-cache metadata flush raised: %a" Eio.Exn.pp
                    contextual)))

let make_snapshot t (entry : entry) =
  let fallback_replies () =
    Event_cache.related_events t.event_cache entry.room_id ~target:entry.root_id
      ~rel_type:Event.Rel_type.Thread ()
  in
  let ordered =
    if entry.event_ids = [] then
      Option.to_list
        (Event_cache.find_event t.event_cache entry.room_id entry.root_id)
      @ fallback_replies ()
    else
      List.filter_map
        (Event_cache.find_event t.event_cache entry.room_id)
        entry.event_ids
  in
  let root =
    List.find_opt
      (fun (event : Event.Raw_event.t) ->
        Option.equal Id.Event_id.equal event.event_id (Some entry.root_id))
      ordered
  in
  let replies =
    List.filter
      (fun (event : Event.Raw_event.t) ->
        not (Option.equal Id.Event_id.equal event.event_id (Some entry.root_id)))
      ordered
  in
  {
    room_id = entry.room_id;
    root_id = entry.root_id;
    root;
    replies;
    events = Option.fold ~none:replies ~some:(fun root -> root :: replies) root;
    pagination = entry.pagination;
    receipts = entry.receipts;
    unread = entry.unread;
  }

let notification t (entry : entry) kind =
  if
    (not t.closed) && not (Event_cache.is_forgotten t.event_cache entry.room_id)
  then Some (callback_snapshot t kind entry, make_snapshot t entry)
  else None

let emit = function
  | Some (callbacks, snapshot) -> publish callbacks snapshot
  | None -> ()

let emit_pair (callbacks, snapshot) = publish callbacks snapshot

let find_entry t room_id root_id =
  Hashtbl.find_opt t.entries (key room_id root_id)

let forget_room t room_id =
  let unsubscribe =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let room_key = Id.Room_id.to_string room_id in
        Hashtbl.filter_map_inplace
          (fun _ (entry : entry) ->
            if Id.Room_id.equal entry.room_id room_id then None else Some entry)
          t.entries;
        let unsubscribe = Hashtbl.find_opt t.forget_subscriptions room_key in
        Hashtbl.remove t.forget_subscriptions room_key;
        persist t;
        unsubscribe)
  in
  Option.iter (fun unsubscribe -> Eio.Cancel.protect unsubscribe) unsubscribe

let install_room_subscription t room_id =
  let room_key = Id.Room_id.to_string room_id in
  if Hashtbl.mem t.forget_subscriptions room_key then true
  else begin
    let active = ref false in
    let forgotten_during_registration = ref false in
    let unsubscribe =
      Event_cache.subscribe_forget_room t.event_cache room_id (fun () ->
          if !active then forget_room t room_id
          else forgotten_during_registration := true)
    in
    active := true;
    if !forgotten_during_registration then (
      Eio.Cancel.protect unsubscribe;
      false)
    else (
      Hashtbl.replace t.forget_subscriptions room_key unsubscribe;
      true)
  end

let ensure_entry t room_id root_id =
  if not (install_room_subscription t room_id) then None
  else
    let key = key room_id root_id in
    match Hashtbl.find_opt t.entries key with
    | Some entry -> Some (entry, false)
    | None ->
        let entry =
          {
            room_id;
            root_id;
            event_ids = [];
            pagination = { backward = Not_started; forward = Not_started };
            receipts = Read_state.empty;
            unread = Read_state.zero_counts;
          }
        in
        Hashtbl.add t.entries key entry;
        Some (entry, true)

let register t room_id event =
  Event_cache.register_external_event t.event_cache room_id ~event

let remember_event (entry : entry) event_id =
  if List.exists (Id.Event_id.equal event_id) entry.event_ids then false
  else begin
    entry.event_ids <- entry.event_ids @ [ event_id ];
    true
  end

let max_events_per_thread = Event_store.Internal.max_external_events

let bound_replies replies =
  let excess = List.length replies - (max_events_per_thread - 1) in
  if excess > 0 then List.drop excess replies else replies

let remember_events t (entry : entry) events =
  let previous = entry.event_ids in
  List.iter
    (fun event ->
      Option.iter
        (fun event_id -> ignore (remember_event entry event_id))
        event.Event.Raw_event.event_id)
    events;
  let replies =
    List.filter
      (fun id -> not (Id.Event_id.equal id entry.root_id))
      entry.event_ids
  in
  let compare left right =
    match
      ( Event_cache.find_event t.event_cache entry.room_id left,
        Event_cache.find_event t.event_cache entry.room_id right )
    with
    | Some left_event, Some right_event ->
        let by_time =
          Event.Timestamp.compare left_event.origin_server_ts
            right_event.origin_server_ts
        in
        if by_time <> 0 then by_time else Id.Event_id.compare left right
    | _ -> Id.Event_id.compare left right
  in
  entry.event_ids <- entry.root_id :: bound_replies (List.sort compare replies);
  entry.event_ids <> previous

let ingest t ~room_id ~events =
  let notifications =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed || Event_cache.is_forgotten t.event_cache room_id then []
        else begin
          let changed = ref false in
          let touched = Hashtbl.create (List.length events) in
          List.iter
            (fun event ->
              if
                Option.for_all (Id.Room_id.equal room_id)
                  event.Event.Raw_event.room_id
              then
                match root_id_of_event event with
                | None -> ()
                | Some event_id ->
                    let relation_root = thread_relation event in
                    let bundled_root =
                      if
                        Option.is_none relation_root && has_thread_summary event
                      then Some event_id
                      else None
                    in
                    let known_root =
                      if
                        Option.is_none relation_root
                        && Option.is_none bundled_root
                      then
                        Option.map
                          (fun _ -> event_id)
                          (find_entry t room_id event_id)
                      else None
                    in
                    let root_id =
                      first_some relation_root
                        (first_some bundled_root known_root)
                    in
                    Option.iter
                      (fun root ->
                        match ensure_entry t room_id root with
                        | None -> ()
                        | Some (entry, created) ->
                            register t room_id event;
                            let remembered =
                              remember_events t entry [ event ]
                            in
                            if created || remembered then changed := true;
                            Hashtbl.replace touched (key room_id root) entry)
                      root_id)
            events;
          let notifications =
            Hashtbl.to_seq_values touched
            |> Seq.filter_map (fun entry -> notification t entry Events)
            |> List.of_seq
          in
          if !changed then persist t;
          notifications
        end)
  in
  List.iter emit_pair notifications

let ingest_thread t ~room_id ~root_id ~events =
  let notifications =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed || Event_cache.is_forgotten t.event_cache room_id then []
        else if
          List.exists
            (fun event ->
              (not
                 (Option.for_all (Id.Room_id.equal room_id)
                    event.Event.Raw_event.room_id))
              || Option.exists
                   (fun target -> not (Id.Event_id.equal target root_id))
                   (thread_relation event))
            events
        then []
        else
          match ensure_entry t room_id root_id with
          | None -> []
          | Some (entry, created) ->
              List.iter (register t room_id) events;
              let remembered = remember_events t entry events in
              if created || remembered then persist t;
              Option.to_list (notification t entry Events))
  in
  List.iter emit_pair notifications

let snapshot t ~room_id ~root_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
      if t.closed || Event_cache.is_forgotten t.event_cache room_id then None
      else Option.map (make_snapshot t) (find_entry t room_id root_id))

let set_pagination t ~room_id ~root_id ~backward ~forward =
  let notification =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if
          (not t.closed) && not (Event_cache.is_forgotten t.event_cache room_id)
        then (
          match ensure_entry t room_id root_id with
          | None -> None
          | Some (entry, false) when entry.pagination = { backward; forward } ->
              None
          | Some (entry, _) ->
              entry.pagination <- { backward; forward };
              persist t;
              notification t entry Events)
        else None)
  in
  emit notification

let set_receipts t ~room_id ~root_id receipts =
  let notification =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if
          (not t.closed) && not (Event_cache.is_forgotten t.event_cache room_id)
        then (
          match ensure_entry t room_id root_id with
          | None -> None
          | Some (entry, false) when entry.receipts = receipts -> None
          | Some (entry, _) ->
              entry.receipts <- receipts;
              persist t;
              notification t entry Receipts)
        else None)
  in
  emit notification

let set_room_receipts t ~room_id receipts =
  let notifications =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if
          (not t.closed) && not (Event_cache.is_forgotten t.event_cache room_id)
        then (
          let changed = ref false in
          let notifications =
            Hashtbl.fold
              (fun _ (entry : entry) acc ->
                if
                  Id.Room_id.equal entry.room_id room_id
                  && entry.receipts <> receipts
                then begin
                  changed := true;
                  entry.receipts <- receipts;
                  match notification t entry Receipts with
                  | None -> acc
                  | Some notification -> notification :: acc
                end
                else acc)
              t.entries []
          in
          if !changed then persist t;
          notifications)
        else [])
  in
  List.iter emit_pair notifications

let set_unread t ~room_id ~root_id unread =
  let notification =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if
          (not t.closed) && not (Event_cache.is_forgotten t.event_cache room_id)
        then (
          match ensure_entry t room_id root_id with
          | None -> None
          | Some (entry, false) when entry.unread = unread -> None
          | Some (entry, _) ->
              entry.unread <- unread;
              persist t;
              notification t entry Unread)
        else None)
  in
  emit notification

let subscribe t ~room_id ~root_id kind callback =
  let registered =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed then None
        else
          let id = t.next_subscription in
          t.next_subscription <- id + 1;
          let subscription =
            { active = Atomic.make true; kind; callback; room_id; root_id }
          in
          Hashtbl.add t.subscriptions id subscription;
          let initial =
            Option.map (make_snapshot t) (find_entry t room_id root_id)
          in
          Some (id, initial))
  in
  match registered with
  | None -> fun () -> ()
  | Some (id, initial) ->
      let active = Atomic.make true in
      let remove () =
        if Atomic.compare_and_set active true false then
          Eio.Cancel.protect (fun () ->
              Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                  Option.iter
                    (fun subscription -> Atomic.set subscription.active false)
                    (Hashtbl.find_opt t.subscriptions id);
                  Hashtbl.remove t.subscriptions id))
      in
      (match initial with
      | None -> ()
      | Some initial -> (
          try callback initial
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            remove ();
            Printexc.raise_with_backtrace exn bt));
      remove

let subscribe_events ~room_id ~root_id t callback =
  subscribe ~room_id ~root_id t Events callback

let subscribe_receipts ~room_id ~root_id t callback =
  subscribe ~room_id ~root_id t Receipts callback

let subscribe_unread ~room_id ~root_id t callback =
  subscribe ~room_id ~root_id t Unread callback

let close t =
  let unsubscribes =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed then []
        else begin
          t.closed <- true;
          Hashtbl.clear t.entries;
          Hashtbl.iter
            (fun _ subscription -> Atomic.set subscription.active false)
            t.subscriptions;
          Hashtbl.clear t.subscriptions;
          let unsubscribe =
            Hashtbl.to_seq_values t.forget_subscriptions |> List.of_seq
          in
          Hashtbl.clear t.forget_subscriptions;
          unsubscribe
        end)
  in
  List.iter (fun unsubscribe -> Eio.Cancel.protect unsubscribe) unsubscribes

let create ~event_cache ?store () =
  let t =
    {
      event_cache;
      store;
      mutex = Eio.Mutex.create ();
      entries = Hashtbl.create 32;
      subscriptions = Hashtbl.create 8;
      next_subscription = 0;
      closed = false;
      forget_subscriptions = Hashtbl.create 8;
    }
  in
  (match store with
  | None -> ()
  | Some store -> (
      match Matrix_client.Store.Slot.find store slot with
      | Ok (Some state) when state.format_version = 1 ->
          List.iter
            (fun (persisted : persisted) ->
              let entry =
                {
                  room_id = persisted.room_id;
                  root_id = persisted.root_id;
                  event_ids =
                    (match Option.value persisted.event_ids ~default:[] with
                    | [] -> []
                    | _root :: replies ->
                        persisted.root_id :: bound_replies replies);
                  pagination =
                    {
                      backward = persisted.backward;
                      forward = persisted.forward;
                    };
                  receipts =
                    Option.value persisted.receipts ~default:Read_state.empty;
                  unread =
                    Option.value persisted.unread
                      ~default:Read_state.zero_counts;
                }
              in
              Hashtbl.replace t.entries (key entry.room_id entry.root_id) entry;
              ignore (install_room_subscription t entry.room_id))
            state.entries
      | Ok (Some state) ->
          Logs.warn (fun m ->
              m "ui: unsupported thread-cache format version %d"
                state.format_version)
      | Ok None -> ()
      | Error error ->
          Logs.warn (fun m ->
              m "ui: thread-cache restore failed: %s"
                (Matrix_client.Error.to_string error))));
  t
