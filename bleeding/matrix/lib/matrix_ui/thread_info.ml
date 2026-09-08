module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Json = Matrix_proto.Json
module Read_state = Matrix_client.Read_state

type summary = { reply_count : int; latest_reply_id : Id.Event_id.t option }
type summary_status = Unknown | Known_none | Known of summary

type info = {
  room_id : Id.Room_id.t;
  root : Event.Raw_event.t;
  latest_reply : Event.Raw_event.t option;
  reply_count : int;
  summary_status : summary_status;
  subscription : Matrix_client.Thread_subscriptions.stored_subscription option;
  public_read : Read_state.receipt option;
  private_read : Read_state.receipt option;
  latest_read : Id.Event_id.t option;
  unread : Read_state.counts;
}

type persisted = {
  p_room_id : Id.Room_id.t;
  p_root : Event.Raw_event.t;
  p_latest_reply : Event.Raw_event.t option;
  p_reply_count : int;
  p_status : string;
  p_latest_reply_id : Id.Event_id.t option;
}

let persisted_jsont : persisted Jsont.t =
  Jsont.Object.(
    map
      (fun
        p_room_id
        p_root
        p_latest_reply
        p_reply_count
        p_status
        p_latest_reply_id
      ->
        {
          p_room_id;
          p_root;
          p_latest_reply;
          p_reply_count;
          p_status;
          p_latest_reply_id;
        })
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun t -> t.p_room_id)
    |> mem "root" Event.Raw_event.persisted_jsont ~enc:(fun t -> t.p_root)
    |> opt_mem "latest_reply" Event.Raw_event.persisted_jsont ~enc:(fun t ->
        t.p_latest_reply)
    |> mem "reply_count" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun t ->
        t.p_reply_count)
    |> mem "summary_status" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.p_status)
    |> opt_mem "latest_reply_id" Id.Event_id.jsont ~enc:(fun t ->
        t.p_latest_reply_id)
    |> finish)

let persisted_list_jsont = Jsont.list persisted_jsont

type persisted_state = { format_version : int; entries : persisted list }

let persisted_state_jsont : persisted_state Jsont.t =
  Jsont.Object.(
    map (fun format_version entries -> { format_version; entries })
    |> mem "format_version" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun t ->
        t.format_version)
    |> mem "entries" persisted_list_jsont ~enc:(fun t -> t.entries)
    |> finish)

let slot = Matrix_client.Store.Slot.v ~name:"thread_info" persisted_state_jsont

type t = {
  store : Matrix_client.Store.t option;
  user_id : Id.User_id.t;
  values : (string, info list) Hashtbl.t;
  observables : (string, info Observable.List.t) Hashtbl.t;
  listeners : (string, (int * (info array -> unit)) list) Hashtbl.t;
  mutable next_listener : int;
}

let room_key room_id = Id.Room_id.to_string room_id

let root_key root =
  Option.map Id.Event_id.to_string root.Event.Raw_event.event_id

let summary_status_to_string = function
  | Unknown -> "unknown"
  | Known_none -> "none"
  | Known _ -> "some"

let summary_status_of_persisted status count latest_reply_id =
  match status with
  | "unknown" -> Unknown
  | "none" -> Known_none
  | "some" -> Known { reply_count = count; latest_reply_id }
  | _ -> Unknown

let without_bundled_relations (event : Event.Raw_event.t) =
  {
    event with
    unsigned = Option.map Event.Unsigned.without_relations event.unsigned;
  }

let persisted_of_info (value : info) =
  {
    p_room_id = value.room_id;
    p_root = without_bundled_relations value.root;
    p_latest_reply = Option.map without_bundled_relations value.latest_reply;
    p_reply_count = value.reply_count;
    p_status = summary_status_to_string value.summary_status;
    p_latest_reply_id =
      (match value.summary_status with
      | Known summary -> summary.latest_reply_id
      | Unknown | Known_none -> None);
  }

let info_of_persisted (value : persisted) =
  let reply_count = max 0 value.p_reply_count in
  {
    room_id = value.p_room_id;
    root = value.p_root;
    latest_reply = value.p_latest_reply;
    reply_count;
    summary_status =
      summary_status_of_persisted value.p_status reply_count
        value.p_latest_reply_id;
    subscription = None;
    public_read = None;
    private_read = None;
    latest_read = None;
    unread = Read_state.zero_counts;
  }

let persist t =
  match t.store with
  | None -> ()
  | Some store -> (
      let values =
        Hashtbl.fold
          (fun _ values acc -> List.rev_append values acc)
          t.values []
        |> List.sort (fun (a : info) b ->
            let room =
              String.compare (room_key a.room_id) (room_key b.room_id)
            in
            if room <> 0 then room
            else
              String.compare
                (Option.value (root_key a.root) ~default:"")
                (Option.value (root_key b.root) ~default:""))
        |> List.map persisted_of_info
      in
      let state = { format_version = 1; entries = values } in
      let store_state = Matrix_client.Store.Slot.find store slot in
      match store_state with
      | Ok (Some old) when old = state -> ()
      | Ok _ | Error _ -> (
          match Matrix_client.Store.Slot.set store slot state with
          | Error error ->
              Logs.warn (fun m ->
                  m "ui: could not persist thread summaries: %s"
                    (Matrix_client.Error.to_string error))
          | Ok () -> (
              try
                match Matrix_client.Store.flush store with
                | Ok () -> ()
                | Error error ->
                    Logs.warn (fun m ->
                        m "ui: could not flush thread summaries: %s"
                          (Matrix_client.Error.to_string error))
              with
              | Eio.Cancel.Cancelled _ as exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  Printexc.raise_with_backtrace exn bt
              | Eio.Io _ as exn ->
                  let contextual =
                    Eio.Exn.add_context exn "flushing thread summaries"
                  in
                  Logs.warn (fun m ->
                      m "ui: could not flush thread summaries: %a" Eio.Exn.pp
                        contextual))))

let create ?store ~user_id () =
  let t =
    {
      store;
      user_id;
      values = Hashtbl.create 8;
      observables = Hashtbl.create 8;
      listeners = Hashtbl.create 8;
      next_listener = 0;
    }
  in
  Option.iter
    (fun store ->
      match Matrix_client.Store.Slot.find store slot with
      | Error error ->
          Logs.warn (fun m ->
              m "ui: could not restore thread summaries: %s"
                (Matrix_client.Error.to_string error))
      | Ok None -> ()
      | Ok (Some { format_version = 1; entries = values }) ->
          List.iter
            (fun value ->
              match root_key value.p_root with
              | None -> ()
              | Some _ ->
                  let item = info_of_persisted value in
                  let key = room_key item.room_id in
                  let old =
                    Option.value (Hashtbl.find_opt t.values key) ~default:[]
                  in
                  Hashtbl.replace t.values key (item :: old))
            values
      | Ok (Some { format_version; _ }) ->
          Logs.warn (fun m ->
              m "ui: ignoring unsupported thread-summary format %d"
                format_version))
    store;
  t

let infos t room_id =
  let key = room_key room_id in
  match Hashtbl.find_opt t.observables key with
  | Some value -> value
  | None ->
      let value =
        Observable.List.create
          (List.sort
             (fun (a : info) b ->
               String.compare
                 (Option.value (root_key a.root) ~default:"")
                 (Option.value (root_key b.root) ~default:""))
             (Option.value (Hashtbl.find_opt t.values key) ~default:[]))
      in
      Hashtbl.add t.observables key value;
      value

let snapshot t room_id = Observable.List.snapshot (infos t room_id)

let notify t room_id =
  let key = room_key room_id in
  let values = snapshot t room_id in
  let listeners = Option.value (Hashtbl.find_opt t.listeners key) ~default:[] in
  List.iter
    (fun (_, callback) ->
      try callback values with
      | Eio.Cancel.Cancelled _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
      | exn ->
          Logs.warn (fun m ->
              m "ui: thread summary listener failed: %s"
                (Printexc.to_string exn)))
    listeners

let subscribe t room_id callback =
  let key = room_key room_id in
  let id = t.next_listener in
  t.next_listener <- id + 1;
  let listeners = Option.value (Hashtbl.find_opt t.listeners key) ~default:[] in
  Hashtbl.replace t.listeners key ((id, callback) :: listeners);
  (try callback (snapshot t room_id)
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     Hashtbl.replace t.listeners key listeners;
     Printexc.raise_with_backtrace exn bt);
  let active = ref true in
  fun () ->
    if !active then (
      active := false;
      match Hashtbl.find_opt t.listeners key with
      | None -> ()
      | Some listeners ->
          let listeners =
            List.filter (fun (candidate, _) -> candidate <> id) listeners
          in
          if listeners = [] then Hashtbl.remove t.listeners key
          else Hashtbl.replace t.listeners key listeners)

let publish t room_id values =
  let key = room_key room_id in
  let values =
    List.sort
      (fun (a : info) b ->
        String.compare
          (Option.value (root_key a.root) ~default:"")
          (Option.value (root_key b.root) ~default:""))
      values
  in
  Hashtbl.replace t.values key values;
  Observable.List.reconcile_by
    ~key:(fun (value : info) -> Option.value (root_key value.root) ~default:"")
    ~equal:( = ) (infos t room_id) values;
  notify t room_id

type member = Absent | Present of Jsont.json | Malformed

let member name json =
  match Json.as_object json with
  | None -> Malformed
  | Some object' -> (
      match Jsont.Json.find_mem name object' with
      | None -> Absent
      | Some (_, value) -> Present value)

let summary_of_root (root : Event.Raw_event.t) =
  let unknown = (Unknown, None) in
  match root.unsigned with
  | None -> (Known_none, None)
  | Some unsigned -> (
      match Event.Unsigned.relations unsigned with
      | None -> (Known_none, None)
      | Some relations -> (
          match member "m.thread" relations with
          | Absent -> (Known_none, None)
          | Malformed -> unknown
          | Present thread -> (
              match member "count" thread with
              | Present count -> (
                  match Json.as_int count with
                  | Some count when count >= 0 -> (
                      match member "latest_event" thread with
                      | Absent | Malformed -> unknown
                      | Present value -> (
                          match
                            Jsont.Json.decode Event.Raw_event.jsont value
                          with
                          | Ok event ->
                              let latest_reply_id = event.event_id in
                              ( Known { reply_count = count; latest_reply_id },
                                Some event )
                          | Error _ -> unknown))
                  | _ -> unknown)
              | Absent | Malformed -> unknown)))

let event_id event = event.Event.Raw_event.event_id

let thread_relation_root event =
  match Json.find_mem "m.relates_to" event.Event.Raw_event.content with
  | None -> None
  | Some relates_to -> (
      match
        ( Json.find_string "rel_type" relates_to,
          Json.find_string "event_id" relates_to )
      with
      | Some "m.thread", Some root ->
          Id.Event_id.of_string root |> Result.to_option
      | _ -> None)

let newer_event a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some a, Some b -> (
      let timestamp =
        Int64.compare
          (Event.Timestamp.to_ms a.Event.Raw_event.origin_server_ts)
          (Event.Timestamp.to_ms b.Event.Raw_event.origin_server_ts)
      in
      if timestamp > 0 then Some a
      else if timestamp < 0 then Some b
      else
        match (event_id a, event_id b) with
        | Some aid, Some bid
          when String.compare
                 (Id.Event_id.to_string aid)
                 (Id.Event_id.to_string bid)
               >= 0 ->
            Some a
        | _ -> Some b)

let merge_status old incoming =
  match (old, incoming) with
  | _, Unknown -> old
  | Unknown, status -> status
  | Known_none, Known summary -> Known summary
  | Known old, Known incoming ->
      Known
        {
          reply_count = max old.reply_count incoming.reply_count;
          latest_reply_id =
            (match incoming.latest_reply_id with
            | Some _ -> incoming.latest_reply_id
            | None -> old.latest_reply_id);
        }
  | status, Known_none -> status

let status_count = function
  | Known summary -> summary.reply_count
  | Unknown | Known_none -> 0

let ingest_root t ~room_id (root : Event.Raw_event.t) =
  match event_id root with
  | None -> ()
  | Some root_id ->
      let key = room_key room_id in
      let old_values =
        Option.value (Hashtbl.find_opt t.values key) ~default:[]
      in
      let incoming_status, incoming_latest = summary_of_root root in
      let values =
        match
          List.find_opt
            (fun (value : info) ->
              Option.equal Id.Event_id.equal (event_id value.root)
                (Some root_id))
            old_values
        with
        | None ->
            {
              room_id;
              root;
              latest_reply = incoming_latest;
              reply_count = status_count incoming_status;
              summary_status = incoming_status;
              subscription = None;
              public_read = None;
              private_read = None;
              latest_read = None;
              unread = Read_state.zero_counts;
            }
            :: old_values
        | Some old ->
            let status = merge_status old.summary_status incoming_status in
            let latest_reply = newer_event old.latest_reply incoming_latest in
            let status =
              match status with
              | Known summary ->
                  Known
                    {
                      summary with
                      latest_reply_id = Option.bind latest_reply event_id;
                    }
              | Unknown | Known_none -> status
            in
            let value =
              {
                old with
                root;
                latest_reply;
                reply_count = max old.reply_count (status_count status);
                summary_status = status;
              }
            in
            value
            :: List.filter
                 (fun (candidate : info) -> candidate != old)
                 old_values
      in
      publish t room_id values;
      persist t

let deduplicate_events events =
  let seen = Hashtbl.create (List.length events) in
  List.filter
    (fun event ->
      match event_id event with
      | None -> false
      | Some event_id ->
          let key = Id.Event_id.to_string event_id in
          if Hashtbl.mem seen key then false
          else begin
            Hashtbl.add seen key ();
            true
          end)
    events

let refresh_room t ~state ~room_id ~events =
  let key = room_key room_id in
  let current = Option.value (Hashtbl.find_opt t.values key) ~default:[] in
  let receipts = Matrix_client.Base_client.receipts state room_id in
  let notification event =
    Matrix_client.Push_evaluator.notification_for_event
      (Matrix_client.Base_client.ruleset state)
      (Matrix_client.Base_client.push_context state room_id)
      event
  in
  let values =
    List.map
      (fun (old : info) ->
        match event_id old.root with
        | None -> old
        | Some root_id ->
            let local_replies =
              List.filter
                (fun event ->
                  Option.exists
                    (fun id -> not (Id.Event_id.equal id root_id))
                    (event_id event)
                  && Option.is_some (thread_relation_root event))
                events
            in
            let local_replies =
              List.filter
                (fun event ->
                  Option.equal Id.Event_id.equal
                    (thread_relation_root event)
                    (Some root_id))
                local_replies
              |> deduplicate_events
            in
            let root =
              match
                List.find_opt
                  (fun event ->
                    Option.equal Id.Event_id.equal (event_id event)
                      (Some root_id))
                  events
              with
              | Some event -> event
              | None -> old.root
            in
            let incoming_status, incoming_latest = summary_of_root root in
            let latest_reply =
              List.fold_left
                (fun latest event -> newer_event latest (Some event))
                (newer_event old.latest_reply incoming_latest)
                local_replies
            in
            let merged_status =
              merge_status old.summary_status incoming_status
            in
            let reply_count =
              max old.reply_count
                (max (status_count merged_status) (List.length local_replies))
            in
            let status =
              let summary =
                {
                  reply_count;
                  latest_reply_id = Option.bind latest_reply event_id;
                }
              in
              match merged_status with
              | Known _ -> Known summary
              | (Unknown | Known_none) when local_replies <> [] -> Known summary
              | Unknown -> Unknown
              | Known_none -> Known_none
            in
            let subscription =
              match t.store with
              | None -> old.subscription
              | Some store -> (
                  match
                    Matrix_client.Thread_subscriptions.find_stored store
                      ~room_id ~thread_root:root_id
                  with
                  | Ok value -> value
                  | Error _ -> old.subscription)
            in
            {
              old with
              root;
              latest_reply;
              reply_count;
              summary_status = status;
              subscription;
              public_read =
                Read_state.thread_public_read receipts ~thread_id:root_id;
              private_read =
                Read_state.thread_private_read receipts ~thread_id:root_id;
              latest_read =
                Read_state.thread_latest_read receipts ~thread_id:root_id;
              unread =
                Read_state.count_unread_in_thread ~user_id:t.user_id
                  ~thread_id:root_id ~notification receipts events;
            })
      current
  in
  publish t room_id values;
  persist t

let remove_room t room_id =
  let key = room_key room_id in
  Hashtbl.remove t.values key;
  (match Hashtbl.find_opt t.observables key with
  | None -> ()
  | Some observable ->
      Observable.List.reconcile_by ~key:(fun _ -> ()) ~equal:( = ) observable []);
  persist t;
  notify t room_id;
  Hashtbl.remove t.observables key;
  Hashtbl.remove t.listeners key
