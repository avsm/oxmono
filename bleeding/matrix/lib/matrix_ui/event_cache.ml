module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Send_queue = Matrix_client.Send_queue
module Model = Event_store.Internal

type delivery = Model.delivery = Synced | Sending | Queued | Failed of string

type event = Model.event = {
  stable_id : string;
  event : Event.Raw_event.t;
  clear_event : Event.Raw_event.t option;
  delivery : delivery;
}

type prepend_applied = { inserted : event list; reached_start : bool }
type prepend_result = Applied of prepend_applied | Stale | Forgotten

type hydration =
  | Hydrated of event list
  | No_persisted_history
  | Hydration_failed of Event_store.Error.t

module Gap_id = struct
  type t = int

  let equal = Int.equal
  let to_string = string_of_int
end

type gap = { id : Gap_id.t; token : string; index : int }

let raw_is_redacted (event : Event.Raw_event.t) =
  Option.exists
    (fun unsigned -> Option.is_some (Event.Unsigned.redacted_because unsigned))
    event.unsigned

(* [unsigned] and [room_id] legitimately differ between the same event fetched
   from /sync, /messages and /event.  The signed/event content does not. *)
let same_wire_event (left : Event.Raw_event.t) (right : Event.Raw_event.t) =
  Option.equal Id.Event_id.equal left.event_id right.event_id
  && Id.User_id.equal left.sender right.sender
  && Event.Timestamp.compare left.origin_server_ts right.origin_server_ts = 0
  && Event.Event_type.equal left.type_ right.type_
  && Option.equal String.equal left.state_key right.state_key
  && Option.equal Id.Event_id.equal left.redacts right.redacts
  && Jsont.Json.equal left.content right.content

let effective (event : event) =
  if raw_is_redacted event.event then event.event
  else Option.value event.clear_event ~default:event.event

(* The mutable twin of [Event_store.Internal.chunk]: the same shape, with the
   fields a merge updates in place so that appending an event does not
   rebuild the room. Invariant: no two gaps are adjacent, no events chunk is empty, and
   every chunk's token — a gap's [token], an events chunk's [prev_token] —
   back-paginates to the position immediately before what the chunk that
   {e follows} the token's owner holds. *)

type events_chunk = {
  ec_id : int;
  mutable prev_token : string option;
  mutable next_token : string option;
  mutable ec_events : event array;
}

type gap_chunk = { g_id : int; mutable g_token : string }
type chunk = Ev of events_chunk | Gp of gap_chunk

(* The store's layout is deliberately kept separately from [chunks].  An
   [Unloaded_events] entry is metadata only: unlike [Gp], it is not a server
   history hole and its token must never be handed to /messages.  The list is
   oldest first and contains the persisted prefix immediately before the
   resident topology. *)
type persisted_chunk =
  | Unloaded_events of {
      uc_id : int;
      uc_prev_token : string option;
      uc_next_token : string option;
      uc_stable_ids : string list;
    }
  | Unloaded_gap of { ug_id : int; ug_token : string }

type room = {
  room_id : Id.Room_id.t;
  events : event Observable.List.t;
  prev_batch : string option Observable.Value.t;
  has_gap : bool Observable.Value.t;
  gaps : gap list Observable.Value.t;
  mutable chunks : chunk list;
  mutable persisted_prefix : persisted_chunk list;
  mutable next_chunk_id : int;
  mutable pending : Model.change list; (* Newest first. *)
  (* Events fetched by an event-focused view, such as pinned events, that do
     not yet have a place in the room timeline.  These are deliberately not
     chunks: they have no pagination position or token. *)
  external_events : (string, event) Hashtbl.t;
  mutable external_event_order : string list; (* Newest first. *)
}

type forget_subscription = { subscription_id : int; room_key : string }

type decryption_subscription = {
  decryption_subscription_id : int;
  decryption_callback : Id.Room_id.t -> unit;
}

type t = {
  store : Event_store.t option;
  max_events_per_room : int;
  chunk_capacity : int;
  rooms : (string, room) Hashtbl.t;
  (* A room removed by [forget_room] stays tombstoned until a real sync or a
     newly enqueued pending send brings it back. Queue callbacks for requests
     which were cancelled, or which were already in flight, must not recreate
     the cache behind the caller's back. *)
  forgotten_rooms : (string, unit) Hashtbl.t;
  mutable forgotten_requests : Send_queue.request list;
  mutable tracked_queue : Send_queue.t option;
  mutex : Eio.Mutex.t;
  last_error : Event_store.Error.t option Observable.Value.t;
  mutable local_sequence : int;
  (* The queue mutates a request in place when an in-flight send is replaced
     by its compensating redaction. Keep the previous transaction id long
     enough to move its local echo to the new identity. *)
  mutable send_transactions : (Send_queue.request * string) list;
  mutable next_forget_subscription : int;
  mutable forget_subscriptions : (forget_subscription * (unit -> unit)) list;
  mutable next_decryption_subscription : int;
  mutable decryption_subscriptions : decryption_subscription list;
}

let create ?store ?(max_events_per_room = 10_000) ?(chunk_capacity = 128) () =
  if max_events_per_room < 1 then invalid_arg "Event_cache.create";
  if chunk_capacity < 1 then invalid_arg "Event_cache.create";
  {
    store;
    max_events_per_room;
    chunk_capacity;
    rooms = Hashtbl.create 32;
    forgotten_rooms = Hashtbl.create 32;
    forgotten_requests = [];
    tracked_queue = None;
    mutex = Eio.Mutex.create ();
    last_error = Observable.Value.create None;
    local_sequence = 0;
    send_transactions = [];
    next_forget_subscription = 0;
    forget_subscriptions = [];
    next_decryption_subscription = 0;
    decryption_subscriptions = [];
  }

let last_error t = t.last_error

let fresh_id room =
  let id = room.next_chunk_id in
  room.next_chunk_id <- id + 1;
  id

let flatten chunks =
  List.concat_map
    (function Ev chunk -> Array.to_list chunk.ec_events | Gp _ -> [])
    chunks

let count chunks =
  List.fold_left
    (fun total -> function
      | Ev chunk -> total + Array.length chunk.ec_events | Gp _ -> total)
    0 chunks

let gaps_of chunks =
  let rec walk index acc = function
    | [] -> List.rev acc
    | Ev chunk :: rest -> walk (index + Array.length chunk.ec_events) acc rest
    | Gp gap :: rest ->
        walk index ({ id = gap.g_id; token = gap.g_token; index } :: acc) rest
  in
  walk 0 [] chunks

(* A gap describes the hole before what follows it, so a trailing one
   describes nothing; an events chunk emptied by a cancelled echo describes
   nothing either, and two gaps that end up side by side describe one hole,
   which the rightmost token — the one closest to the events that follow —
   fills. *)
let normalise_chunks chunks =
  List.filter
    (function Ev chunk -> Array.length chunk.ec_events > 0 | Gp _ -> true)
    chunks
  |> fun chunks ->
  List.fold_right
    (fun chunk acc ->
      match (chunk, acc) with
      | Gp _, [] -> []
      | Gp _, Gp _ :: _ -> acc
      | _ -> chunk :: acc)
    chunks []

let start_token = function
  | Gp gap :: _ -> Some gap.g_token
  | Ev chunk :: _ -> chunk.prev_token
  | [] -> None

let model_chunk_of_persisted = function
  | Unloaded_gap gap -> Model.Gap { gap_id = gap.ug_id; token = gap.ug_token }
  | Unloaded_events chunk ->
      Model.Events
        {
          chunk_id = chunk.uc_id;
          prev_token = chunk.uc_prev_token;
          next_token = chunk.uc_next_token;
          events = [];
        }

let model_chunk_of_resident = function
  | Gp gap -> Model.Gap { gap_id = gap.g_id; token = gap.g_token }
  | Ev chunk ->
      Model.Events
        {
          chunk_id = chunk.ec_id;
          prev_token = chunk.prev_token;
          next_token = chunk.next_token;
          events = Array.to_list chunk.ec_events;
        }

let all_model_chunks room =
  List.map model_chunk_of_persisted room.persisted_prefix
  @ List.map model_chunk_of_resident room.chunks

(* The layout alone: [Event_store.Internal.Layout] reads the shape and leaves the
   rows where they are, so the events are not worth encoding here. *)
let model_layout room =
  {
    Model.next_chunk_id = room.next_chunk_id;
    chunks =
      List.map
        (function
          | Model.Gap gap -> Model.Gap gap
          | Model.Events chunk -> Model.Events { chunk with events = [] })
        (all_model_chunks room);
    external_events =
      List.rev room.external_event_order
      |> List.filter_map (fun key -> Hashtbl.find_opt room.external_events key);
  }

let layout_change room = Model.Layout (model_layout room)

let persisted_of_metadata = function
  | Model.Gap_metadata gap ->
      Unloaded_gap { ug_id = gap.gap_id; ug_token = gap.token }
  | Model.Events_metadata chunk ->
      Unloaded_events
        {
          uc_id = chunk.chunk_id;
          uc_prev_token = chunk.prev_token;
          uc_next_token = chunk.next_token;
          uc_stable_ids = chunk.stable_ids;
        }

let of_full (stored : Model.room) =
  let resident_models = stored.chunks in
  let resident =
    List.filter_map
      (function
        | Model.Gap gap -> Some (Gp { g_id = gap.gap_id; g_token = gap.token })
        | Model.Events chunk ->
            if chunk.events = [] then None
            else
              Some
                (Ev
                   {
                     ec_id = chunk.chunk_id;
                     prev_token = chunk.prev_token;
                     next_token = chunk.next_token;
                     ec_events = Array.of_list chunk.events;
                   }))
      resident_models
  in
  (resident, [], stored.next_chunk_id, stored.external_events)

let of_initial = function
  | Model.Full stored -> of_full stored
  | Model.Tail { metadata; newest } ->
      let start =
        let rec find index newest = function
          | [] -> Option.value newest ~default:0
          | Model.Events_metadata _ :: rest ->
              find (index + 1) (Some index) rest
          | Model.Gap_metadata _ :: rest -> find (index + 1) newest rest
        in
        let newest = find 0 None metadata.chunks in
        match newest with
        | 0 -> 0
        | index -> (
            match List.nth_opt metadata.chunks (index - 1) with
            | Some (Model.Gap_metadata _) -> index - 1
            | _ -> index)
      in
      let prefix_models = List.take start metadata.chunks in
      let resident_models = List.drop start metadata.chunks in
      let prefix = List.map persisted_of_metadata prefix_models in
      let resident =
        List.filter_map
          (function
            | Model.Gap_metadata gap ->
                Some (Gp { g_id = gap.gap_id; g_token = gap.token })
            | Model.Events_metadata chunk ->
                Option.bind newest (fun loaded ->
                    if loaded.chunk_id = chunk.chunk_id then
                      Some
                        (Ev
                           {
                             ec_id = chunk.chunk_id;
                             prev_token = chunk.prev_token;
                             next_token = chunk.next_token;
                             ec_events = Array.of_list loaded.events;
                           })
                    else None))
          resident_models
      in
      (resident, prefix, metadata.next_chunk_id, metadata.external_events)

let record room change = room.pending <- change :: room.pending

(* A change that only appends, replaces or removes one event updates the flat
   observable in place, so that a subscriber sees a granular diff; anything
   that moves chunks around reconciles the whole list by [stable_id]. *)

let publish_structure room =
  Observable.Value.set room.prev_batch (start_token room.chunks);
  Observable.Value.set room.has_gap
    (List.exists (function Gp _ -> true | Ev _ -> false) room.chunks);
  Observable.Value.set room.gaps (gaps_of room.chunks)

let resync room =
  Observable.List.reconcile_by
    ~key:(fun (event : event) -> event.stable_id)
    ~equal:( == ) room.events (flatten room.chunks);
  publish_structure room

let report t = function
  | Ok value ->
      Observable.Value.set t.last_error None;
      value
  | Error error ->
      Observable.Value.set t.last_error (Some error);
      None

let load_initial t room_id =
  match t.store with
  | None -> None
  | Some store -> (
      match Event_store.load_room_initial store room_id with
      | Ok (Some initial) ->
          Observable.Value.set t.last_error None;
          Some initial
      | Ok None ->
          Observable.Value.set t.last_error None;
          None
      | Error error -> report t (Error error))

let room_locked t room_id =
  let key = Id.Room_id.to_string room_id in
  match Hashtbl.find_opt t.rooms key with
  | Some room -> room
  | None ->
      let chunks, persisted_prefix, next_chunk_id, external_events =
        match load_initial t room_id with
        | None -> ([], [], 0, [])
        | Some initial -> of_initial initial
      in
      let external_events =
        List.filter_map
          (fun (event : Model.event) ->
            Option.map
              (fun key -> (key, event))
              (Option.map Id.Event_id.to_string event.event.event_id))
          external_events
      in
      let external_events =
        let excess =
          List.length external_events
          - min t.max_events_per_room Model.max_external_events
        in
        if excess > 0 then List.drop excess external_events else external_events
      in
      let room =
        {
          room_id;
          events = Observable.List.create (flatten chunks);
          prev_batch = Observable.Value.create (start_token chunks);
          has_gap =
            Observable.Value.create
              (List.exists (function Gp _ -> true | Ev _ -> false) chunks);
          gaps = Observable.Value.create (gaps_of chunks);
          chunks;
          persisted_prefix;
          next_chunk_id;
          pending = [];
          external_events = Hashtbl.create 16;
          external_event_order = List.rev (List.map fst external_events);
        }
      in
      List.iter
        (fun (key, event) -> Hashtbl.replace room.external_events key event)
        external_events;
      Hashtbl.add t.rooms key room;
      room

let room_is_forgotten t room_id =
  Hashtbl.mem t.forgotten_rooms (Id.Room_id.to_string room_id)

let is_forgotten t room_id =
  Eio.Mutex.use_ro t.mutex (fun () -> room_is_forgotten t room_id)

let clear_room room =
  room.chunks <- [];
  room.persisted_prefix <- [];
  room.pending <- [];
  Hashtbl.clear room.external_events;
  room.external_event_order <- [];
  Observable.List.reconcile_by
    ~key:(fun (event : event) -> event.stable_id)
    ~equal:( == ) room.events [];
  publish_structure room

let notify_forget callback =
  try callback () with
  | Eio.Cancel.Cancelled _ as exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt
  | exn ->
      Logs.warn (fun m ->
          m "ui: room-forget listener failed: %s" (Printexc.to_string exn))

let forget_room t room_id =
  let callbacks =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let key = Id.Room_id.to_string room_id in
        Hashtbl.replace t.forgotten_rooms key ();
        let queued =
          match t.tracked_queue with
          | None -> []
          | Some queue -> Send_queue.room_requests queue room_id
        in
        let tracked =
          List.filter_map
            (fun (request, _) ->
              if Send_queue.room_id request = room_id then Some request
              else None)
            t.send_transactions
        in
        let add_unique request requests =
          if List.exists (fun known -> known == request) requests then requests
          else request :: requests
        in
        t.forgotten_requests <-
          List.fold_left
            (fun requests request -> add_unique request requests)
            t.forgotten_requests (queued @ tracked);
        (* Keep the room object in the table after clearing it. Existing handles
           then remain the handles a later sync repopulates, and a failed store
           deletion cannot make the next read reload stale events from disk. *)
        clear_room (room_locked t room_id);
        (match t.store with
        | None -> Observable.Value.set t.last_error None
        | Some store -> (
            match Event_store.remove_room store room_id with
            | Ok () -> Observable.Value.set t.last_error None
            | Error error -> Observable.Value.set t.last_error (Some error)));
        (* Snapshot callbacks while the cache lock is held, but never run
           caller code under that lock.  A callback may safely unsubscribe. *)
        List.filter_map
          (fun ({ room_key; _ }, callback) ->
            if String.equal room_key key then Some callback else None)
          t.forget_subscriptions)
  in
  List.iter notify_forget callbacks

let subscribe_forget_room t room_id callback =
  let room_key = Id.Room_id.to_string room_id in
  let subscription, forgotten =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if room_is_forgotten t room_id then (None, true)
        else begin
          let subscription_id = t.next_forget_subscription in
          t.next_forget_subscription <- subscription_id + 1;
          let subscription = { subscription_id; room_key } in
          t.forget_subscriptions <-
            (subscription, callback) :: t.forget_subscriptions;
          (Some subscription, false)
        end)
  in
  if forgotten then notify_forget callback;
  match subscription with
  | None -> fun () -> ()
  | Some subscription ->
      let active = ref true in
      fun () ->
        if !active then begin
          active := false;
          Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
              t.forget_subscriptions <-
                List.filter
                  (fun ({ subscription_id; _ }, _) ->
                    subscription_id <> subscription.subscription_id)
                  t.forget_subscriptions)
        end

let notify_decryption room_id callback =
  try callback room_id with
  | Eio.Cancel.Cancelled _ as exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt
  | exn ->
      Logs.warn (fun m ->
          m "ui: physical-decryption listener failed: %s"
            (Printexc.to_string exn))

let subscribe_physical_decryption t callback =
  let subscription_id =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let subscription_id = t.next_decryption_subscription in
        t.next_decryption_subscription <- subscription_id + 1;
        t.decryption_subscriptions <-
          {
            decryption_subscription_id = subscription_id;
            decryption_callback = callback;
          }
          :: t.decryption_subscriptions;
        subscription_id)
  in
  let active = ref true in
  fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if !active then begin
          active := false;
          t.decryption_subscriptions <-
            List.filter
              (fun subscription ->
                subscription.decryption_subscription_id <> subscription_id)
              t.decryption_subscriptions
        end)

let with_room t room_id f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> f (room_locked t room_id))

let events t room_id = with_room t room_id (fun room -> room.events)
let snapshot t room_id = Observable.List.snapshot (events t room_id)

let snapshot_with_gaps t room_id =
  with_room t room_id (fun room ->
      (Observable.List.snapshot room.events, gaps_of room.chunks))

let with_snapshot_if_current t room_id expected_events expected_gaps callback =
  (* The callback runs with the cache mutex held.  It must not call back into
     [Event_cache], and must not invoke user callbacks; it may acquire the
     sync-service lock.  Keeping this lock order (cache, then service) makes
     validation and the conditional state transition one atomic operation. *)
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let room = room_locked t room_id in
      let events = Observable.List.snapshot room.events in
      let gaps = gaps_of room.chunks in
      let same_events =
        Array.length events = Array.length expected_events
        && Array.for_all2 ( = ) events expected_events
      in
      let same_gaps =
        List.equal
          (fun left right ->
            Gap_id.equal left.id right.id
            && String.equal left.token right.token
            && left.index = right.index)
          gaps expected_gaps
      in
      if same_events && same_gaps then Some (callback ()) else None)

let prev_batch t room_id = with_room t room_id (fun room -> room.prev_batch)
let has_gap t room_id = with_room t room_id (fun room -> room.has_gap)
let gaps t room_id = with_room t room_id (fun room -> room.gaps)

let has_unloaded_history t room_id =
  with_room t room_id (fun room -> room.persisted_prefix <> [])

let persist_result t room =
  let changes = List.rev room.pending in
  match t.store with
  | None ->
      room.pending <- [];
      Ok ()
  | Some store -> (
      match Event_store.apply store room.room_id changes with
      | Ok () ->
          room.pending <- [];
          Observable.Value.set t.last_error None;
          Ok ()
      | Error error ->
          Observable.Value.set t.last_error (Some error);
          Error error)

let persist t room = ignore (persist_result t room)

let hydration_error t error =
  Observable.Value.set t.last_error (Some error);
  Hydration_failed error

let hydrate_previous t room_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let room = room_locked t room_id in
      (* A failed incremental write leaves the cache deliberately ahead of its
         store.  Retry that exact atomic delta before consulting lazy metadata:
         otherwise a stable event moved out of an unloaded chunk would still
         be present in the old on-disk row and look like corrupt metadata. *)
      match if room.pending = [] then Ok () else persist_result t room with
      | Error error -> Hydration_failed error
      | Ok () -> (
          match room.chunks with
          | Gp _ :: _ -> No_persisted_history
          | _ -> (
              match List.rev room.persisted_prefix with
              | [] -> No_persisted_history
              | Unloaded_gap _ :: _ -> No_persisted_history
              | Unloaded_events metadata :: reversed_rest -> (
                  let loaded =
                    match t.store with
                    | None -> Ok None
                    | Some store ->
                        Event_store.load_events_chunk store room_id
                          metadata.uc_id
                  in
                  match loaded with
                  | Error error -> hydration_error t error
                  | Ok None ->
                      hydration_error t
                        (Event_store.Error.Codec
                           (Printf.sprintf "missing persisted events chunk %d"
                              metadata.uc_id))
                  | Ok (Some loaded) -> (
                      let events = Array.of_list loaded.events in
                      let stable_ids =
                        Array.to_list events
                        |> List.map (fun (event : event) -> event.stable_id)
                      in
                      let expected = metadata.uc_stable_ids in
                      let duplicate ids =
                        let seen = Hashtbl.create (List.length ids) in
                        List.exists
                          (fun id ->
                            if Hashtbl.mem seen id then true
                            else (
                              Hashtbl.replace seen id ();
                              false))
                          ids
                      in
                      let known = Hashtbl.create 64 in
                      List.iter
                        (fun (event : event) ->
                          Hashtbl.replace known event.stable_id ())
                        (flatten room.chunks);
                      if
                        loaded.chunk_id <> metadata.uc_id
                        || loaded.prev_token <> metadata.uc_prev_token
                        || loaded.next_token <> metadata.uc_next_token
                        || Array.length events = 0
                        || (not (List.equal String.equal stable_ids expected))
                        || duplicate stable_ids
                        || Array.exists
                             (fun (event : event) ->
                               Hashtbl.mem known event.stable_id)
                             events
                      then
                        hydration_error t
                          (Event_store.Error.Codec
                             (Printf.sprintf "invalid persisted events chunk %d"
                                metadata.uc_id))
                      else
                        (* A focused view may have fetched an event while its
                       physical timeline chunk was still unloaded.  Promote
                       any matching detached plaintext as the chunk becomes
                       resident, and remove the now-redundant detached row. *)
                        let removed_external = Hashtbl.create 4 in
                        let changed_events = ref [] in
                        let events =
                          Array.mapi
                            (fun position (event : event) ->
                              match event.event.event_id with
                              | None -> event
                              | Some event_id -> (
                                  let key = Id.Event_id.to_string event_id in
                                  match
                                    Hashtbl.find_opt room.external_events key
                                  with
                                  | None -> event
                                  | Some detached ->
                                      Hashtbl.replace removed_external key ();
                                      if
                                        Option.is_none event.clear_event
                                        && (not (raw_is_redacted event.event))
                                        && (not
                                              (raw_is_redacted detached.event))
                                        && same_wire_event event.event
                                             detached.event
                                        && Option.is_some detached.clear_event
                                      then (
                                        let event =
                                          {
                                            event with
                                            clear_event = detached.clear_event;
                                          }
                                        in
                                        changed_events :=
                                          (position, event) :: !changed_events;
                                        event)
                                      else event))
                            events
                        in
                        let promotion_changes =
                          if Hashtbl.length removed_external = 0 then []
                          else
                            let layout =
                              let layout = model_layout room in
                              Model.Layout
                                {
                                  layout with
                                  external_events =
                                    List.filter
                                      (fun (event : event) ->
                                        match event.event.event_id with
                                        | None -> true
                                        | Some event_id ->
                                            not
                                              (Hashtbl.mem removed_external
                                                 (Id.Event_id.to_string event_id)))
                                      layout.external_events;
                                }
                            in
                            layout
                            :: List.map
                                 (fun (position, event) ->
                                   Model.Put_event
                                     {
                                       chunk_id = loaded.chunk_id;
                                       position;
                                       event;
                                     })
                                 (List.rev !changed_events)
                        in
                        let stored =
                          match (promotion_changes, t.store) with
                          | [], _ | _, None -> Ok ()
                          | changes, Some store ->
                              Event_store.apply store room.room_id
                                (List.rev room.pending @ changes)
                        in
                        match stored with
                        | Error error -> hydration_error t error
                        | Ok () ->
                            if promotion_changes <> [] then room.pending <- [];
                            Hashtbl.iter
                              (fun key () ->
                                Hashtbl.remove room.external_events key)
                              removed_external;
                            if Hashtbl.length removed_external > 0 then
                              room.external_event_order <-
                                List.filter
                                  (fun key ->
                                    not (Hashtbl.mem removed_external key))
                                  room.external_event_order;
                            let prefix_without_events =
                              List.rev reversed_rest
                            in
                            let preceding_gap, prefix_without_gap =
                              match List.rev prefix_without_events with
                              | Unloaded_gap gap :: rest ->
                                  ( Some
                                      (Gp
                                         {
                                           g_id = gap.ug_id;
                                           g_token = gap.ug_token;
                                         }),
                                    List.rev rest )
                              | _ -> (None, prefix_without_events)
                            in
                            let chunk =
                              Ev
                                {
                                  ec_id = loaded.chunk_id;
                                  prev_token = loaded.prev_token;
                                  next_token = loaded.next_token;
                                  ec_events = events;
                                }
                            in
                            room.persisted_prefix <- prefix_without_gap;
                            room.chunks <-
                              List.filter_map Fun.id
                                [ preceding_gap; Some chunk ]
                              @ room.chunks;
                            resync room;
                            Observable.Value.set t.last_error None;
                            Hydrated (Array.to_list events))))))

let flush_room t room_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let room = room_locked t room_id in
      match t.store with
      | None -> room.pending <- []
      | Some store -> (
          let resident_changes =
            List.filter_map
              (function
                | Gp _ -> None
                | Ev chunk ->
                    Some
                      (Model.Replace_events_chunk
                         {
                           chunk_id = chunk.ec_id;
                           events = Array.to_list chunk.ec_events;
                         }))
              room.chunks
          in
          match
            Event_store.apply store room_id
              (List.rev room.pending @ (layout_change room :: resident_changes))
          with
          | Ok () ->
              room.pending <- [];
              Observable.Value.set t.last_error None
          | Error error -> Observable.Value.set t.last_error (Some error)))

let transaction_id (event : Event.Raw_event.t) =
  Option.bind event.unsigned Event.Unsigned.transaction_id
  |> Option.map Id.Transaction_id.to_string

let remote_key (event : Event.Raw_event.t) =
  Option.map Id.Event_id.to_string event.event_id

let stable_id_of_transaction transaction = "txn:" ^ transaction

let stable_id t event =
  match remote_key event with
  | Some id -> "event:" ^ id
  | None -> (
      match transaction_id event with
      | Some id -> stable_id_of_transaction id
      | None ->
          t.local_sequence <- t.local_sequence + 1;
          Printf.sprintf "local:%d" t.local_sequence)

(* Over the published list rather than the chunks, so that the index is an
   index into [snapshot]. *)
let position t room_id event_id =
  let wanted = Id.Event_id.to_string event_id in
  let events = snapshot t room_id in
  let rec scan index =
    if index >= Array.length events then None
    else if
      Option.equal String.equal (remote_key events.(index).event) (Some wanted)
    then Some index
    else scan (index + 1)
  in
  scan 0

let find_chunk_event room wanted =
  let rec scan = function
    | [] -> None
    | Gp _ :: rest -> scan rest
    | Ev chunk :: rest -> (
        match
          Array.find_opt
            (fun (event : event) ->
              Option.equal String.equal (remote_key event.event) (Some wanted))
            chunk.ec_events
        with
        | Some event -> Some event
        | None -> scan rest)
  in
  scan room.chunks

let remove_external_event room key =
  Hashtbl.remove room.external_events key;
  room.external_event_order <-
    List.filter
      (fun known -> not (String.equal known key))
      room.external_event_order

let find_event t room_id event_id =
  let wanted = Id.Event_id.to_string event_id in
  with_room t room_id (fun room ->
      match find_chunk_event room wanted with
      | Some event -> Some (effective event)
      | None ->
          Option.map effective (Hashtbl.find_opt room.external_events wanted))

(* Decode only the relation envelope here.  Event content is intentionally
   left open-ended elsewhere in the cache, and a malformed relation must not
   make a cache lookup fail. *)
let relation_of_event (event : Event.Raw_event.t) =
  match Matrix_proto.Json.find_mem "m.relates_to" event.content with
  | None -> None
  | Some json ->
      Result.to_option (Jsont.Json.decode Event.Relates_to.jsont json)

let related_events t room_id ~target ?rel_type () =
  let target = Id.Event_id.to_string target in
  with_room t room_id (fun room ->
      let seen = Hashtbl.create 32 in
      let candidates = ref [] in
      let add (cached : event) =
        let raw = effective cached in
        match relation_of_event raw with
        | Some relation
          when Id.Event_id.to_string relation.event_id = target
               && Option.fold ~none:true
                    ~some:(fun wanted ->
                      Event.Rel_type.equal wanted relation.rel_type)
                    rel_type ->
            let key =
              match raw.event_id with
              | Some event_id -> "event:" ^ Id.Event_id.to_string event_id
              | None -> cached.stable_id
            in
            if not (Hashtbl.mem seen key) then begin
              Hashtbl.add seen key ();
              candidates := raw :: !candidates
            end
        | _ -> ()
      in
      (* Physical records are visited first: a timeline copy is authoritative
         when a relation was previously fetched into the detached registry. *)
      List.iter add (flatten room.chunks);
      List.iter
        (fun key -> Option.iter add (Hashtbl.find_opt room.external_events key))
        (List.rev room.external_event_order);
      List.sort
        (fun (left : Event.Raw_event.t) (right : Event.Raw_event.t) ->
          let by_ts =
            Event.Timestamp.compare left.origin_server_ts right.origin_server_ts
          in
          if by_ts <> 0 then by_ts
          else
            let event_id = function
              | None -> ""
              | Some id -> Id.Event_id.to_string id
            in
            String.compare (event_id left.event_id) (event_id right.event_id))
        !candidates)

let register_external_event t room_id ~(event : Event.Raw_event.t) =
  if
    Option.exists
      (fun event_room -> not (Id.Room_id.equal event_room room_id))
      event.room_id
  then ()
  else
    match remote_key event with
    | None -> ()
    | Some key ->
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
            (* An event fetched before [forget_room] must not re-create the
             forgotten room.  A real sync is the only path that removes this
             tombstone. *)
            if not (room_is_forgotten t room_id) then
              let room = room_locked t room_id in
              (* A timeline event is authoritative.  It is still useful to
                 remove an old detached copy if one somehow survived a race. *)
              if Option.is_some (find_chunk_event room key) then (
                if Hashtbl.mem room.external_events key then begin
                  remove_external_event room key;
                  record room (layout_change room);
                  persist t room
                end)
              else begin
                let cached_clear =
                  Option.bind (Hashtbl.find_opt room.external_events key)
                    (fun cached ->
                      if
                        (not (raw_is_redacted event))
                        && (not (raw_is_redacted cached.event))
                        && same_wire_event cached.event event
                      then cached.clear_event
                      else None)
                in
                Hashtbl.replace room.external_events key
                  {
                    stable_id = "event:" ^ key;
                    event;
                    clear_event = cached_clear;
                    delivery = Synced;
                  };
                room.external_event_order <-
                  key
                  :: List.filter
                       (fun known -> not (String.equal known key))
                       room.external_event_order;
                while
                  List.length room.external_event_order
                  > min t.max_events_per_room Model.max_external_events
                do
                  match List.rev room.external_event_order with
                  | [] -> ()
                  | oldest :: _ -> remove_external_event room oldest
                done;
                record room (layout_change room);
                persist t room
              end)

type locator = { l_chunk : events_chunk; l_pos : int; l_flat : int }

let locate room predicate =
  let rec walk flat = function
    | [] -> None
    | Gp _ :: rest -> walk flat rest
    | Ev chunk :: rest -> (
        let length = Array.length chunk.ec_events in
        let rec scan index =
          if index >= length then None
          else if predicate chunk.ec_events.(index) then
            Some { l_chunk = chunk; l_pos = index; l_flat = flat + index }
          else scan (index + 1)
        in
        match scan 0 with
        | Some found -> Some found
        | None -> walk (flat + length) rest)
  in
  walk 0 room.chunks

let same_event incoming cached =
  match transaction_id incoming with
  | Some transaction ->
      Option.equal String.equal (transaction_id cached.event) (Some transaction)
  | None -> (
      match remote_key incoming with
      | None -> false
      | Some id -> Option.equal String.equal (remote_key cached.event) (Some id)
      )

let find_existing room incoming = locate room (same_event incoming)

let by_transaction transaction cached =
  Option.equal String.equal (transaction_id cached.event) (Some transaction)

let set_event room locator event =
  locator.l_chunk.ec_events.(locator.l_pos) <- event;
  Observable.List.set room.events ~index:locator.l_flat event;
  record room
    (Model.Put_event
       { chunk_id = locator.l_chunk.ec_id; position = locator.l_pos; event })

let remove_event room locator =
  let chunk = locator.l_chunk in
  let stable_id = chunk.ec_events.(locator.l_pos).stable_id in
  chunk.ec_events <-
    Array.append
      (Array.sub chunk.ec_events 0 locator.l_pos)
      (Array.sub chunk.ec_events (locator.l_pos + 1)
         (Array.length chunk.ec_events - locator.l_pos - 1));
  Observable.List.remove room.events ~index:locator.l_flat;
  record room (Model.Delete_event { stable_id });
  if Array.length chunk.ec_events = 0 then (
    room.chunks <-
      List.filter
        (function Ev candidate -> candidate != chunk | Gp _ -> true)
        room.chunks;
    publish_structure room;
    record room (layout_change room))

(* Appending is only ever done at the very end of the room, so the flat
   index is the list's current length. *)
let append_event room chunk event =
  let position = Array.length chunk.ec_events in
  chunk.ec_events <- Array.append chunk.ec_events [| event |];
  Observable.List.append room.events event;
  record room (Model.Put_event { chunk_id = chunk.ec_id; position; event })

let last_events_chunk room =
  match List.rev room.chunks with Ev chunk :: _ -> Some chunk | _ -> None

(* The chunk a sync window's new events go into: the newest one while it has
   room, otherwise a fresh chunk whose [prev_token] is the response's
   [prev_batch] — which is exactly the back-pagination token of the window's
   first event, and so of the new chunk's left edge. *)
let target_chunk t room ~prev_token =
  match last_events_chunk room with
  | Some chunk
    when Array.length chunk.ec_events < t.chunk_capacity
         || Option.is_none prev_token ->
      chunk
  | Some _ | None ->
      let chunk =
        {
          ec_id = fresh_id room;
          prev_token;
          next_token = None;
          ec_events = [||];
        }
      in
      room.chunks <- room.chunks @ [ Ev chunk ];
      publish_structure room;
      record room (layout_change room);
      chunk

let push_gap room token =
  (match List.rev room.chunks with
  | Ev chunk :: _ -> chunk.next_token <- Some token
  | _ -> ());
  let gap = { g_id = fresh_id room; g_token = token } in
  room.chunks <- room.chunks @ [ Gp gap ];
  publish_structure room;
  record room (layout_change room)

(* Over budget, the oldest events chunk goes and a gap takes its place,
   carrying the token that describes the position it was cut at — the next
   chunk's own left-edge token, or the cut chunk's right-edge one. Filling
   that gap re-fetches exactly what was dropped and nothing that is still
   held. Where neither token is known the cut cannot be described, and the
   room is left over budget rather than made to lie about its history. *)
let trim t room =
  let rec drop chunks total =
    if total <= t.max_events_per_room then chunks
    else
      (* A gap in front of the chunk being cut described the hole before it;
         the gap that replaces the chunk subsumes it. *)
      let rec skip = function Gp _ :: rest -> skip rest | chunks -> chunks in
      match skip chunks with
      | Ev chunk :: rest -> (
          let length = Array.length chunk.ec_events in
          let replacement =
            match rest with
            | Gp _ :: _ -> Some rest
            | Ev next :: _ as rest -> (
                let token =
                  match next.prev_token with
                  | Some _ as token -> token
                  | None -> chunk.next_token
                in
                match token with
                | Some token ->
                    Some (Gp { g_id = fresh_id room; g_token = token } :: rest)
                | None -> None)
            | [] -> None
          in
          match replacement with
          | Some chunks -> drop chunks (total - length)
          | None -> chunks)
      | Gp _ :: _ | [] -> chunks
  in
  let chunks = drop room.chunks (count room.chunks) in
  if chunks != room.chunks then (
    room.chunks <- chunks;
    resync room;
    record room (layout_change room))

let clear_for decrypted event =
  match remote_key event with
  | None -> None
  | Some id -> Hashtbl.find_opt decrypted id

let stable_keys_of_raw (event : Event.Raw_event.t) =
  List.filter_map Fun.id
    [
      Option.map (fun id -> "event:" ^ id) (remote_key event);
      Option.map (fun id -> "txn:" ^ id) (transaction_id event);
    ]

(* The short prefixes are private lookup namespaces.  The stable prefixes are
   persisted identities.  Keep both: a remote echo may have gained an event ID
   while still carrying the transaction identity of its local echo. *)
let keys_of (event : Event.Raw_event.t) =
  List.filter_map Fun.id
    [
      Option.map (fun id -> "e:" ^ id) (remote_key event);
      Option.map (fun id -> "t:" ^ id) (transaction_id event);
    ]
  @ stable_keys_of_raw event

let hidden_stable_ids room =
  let ids = Hashtbl.create 64 in
  List.iter
    (function
      | Unloaded_gap _ -> ()
      | Unloaded_events chunk ->
          List.iter (fun id -> Hashtbl.replace ids id ()) chunk.uc_stable_ids)
    room.persisted_prefix;
  ids

(* A restored send queue may still own a local echo which has aged into an
   unloaded chunk.  Tracking the queue must expose that echo at the live tail,
   but hydrating every older chunk to find it would defeat lazy loading.

   The stable id is the event row's store key, so a following [Put_event]
   moves it to the resident target chunk in both persistent backends.  Adjust
   the metadata first; if removing the row empties its old chunk, also rewrite
   the layout.  Any gaps made adjacent by that removal collapse to the
   rightmost token, matching [normalise_chunks]. *)
let promote_hidden_stable_id room stable_id =
  let found = ref false in
  let shape_changed = ref false in
  let prefix =
    List.filter_map
      (function
        | Unloaded_gap _ as gap -> Some gap
        | Unloaded_events chunk as events ->
            if not (List.exists (String.equal stable_id) chunk.uc_stable_ids)
            then Some events
            else begin
              found := true;
              let stable_ids =
                List.filter
                  (fun known -> not (String.equal known stable_id))
                  chunk.uc_stable_ids
              in
              match stable_ids with
              | [] ->
                  shape_changed := true;
                  None
              | _ ->
                  Some
                    (Unloaded_events
                       {
                         uc_id = chunk.uc_id;
                         uc_prev_token = chunk.uc_prev_token;
                         uc_next_token = chunk.uc_next_token;
                         uc_stable_ids = stable_ids;
                       })
            end)
      room.persisted_prefix
  in
  if not !found then false
  else begin
    let prefix =
      List.fold_right
        (fun chunk acc ->
          match (chunk, acc) with
          | Unloaded_gap _, Unloaded_gap _ :: _ -> acc
          | _ -> chunk :: acc)
        prefix []
    in
    let prefix, trailing_gap =
      match List.rev prefix with
      | Unloaded_gap gap :: rest ->
          (List.rev rest, Some (gap.ug_id, gap.ug_token))
      | _ -> (prefix, None)
    in
    room.persisted_prefix <- prefix;
    Option.iter
      (fun (gap_id, token) ->
        match room.chunks with
        | Gp _ :: _ -> ()
        | Ev _ :: _ ->
            room.chunks <- Gp { g_id = gap_id; g_token = token } :: room.chunks;
            publish_structure room
        | [] ->
            (* A valid lazy initial load always keeps its newest events chunk
               resident, so this cannot be the sole remaining gap. *)
            ())
      trailing_gap;
    if !shape_changed then record room (layout_change room);
    true
  end

let hidden_matches hidden event =
  List.exists (Hashtbl.mem hidden) (stable_keys_of_raw event)

let merge_event t room hidden decrypted target incoming =
  if hidden_matches hidden incoming then ()
  else begin
    (* A fetched event has no timeline position, but a later sync may bring the
     same event into the ordered history. Promote it, carrying any plaintext
     that arrived while it was detached. *)
    let detached =
      Option.bind (remote_key incoming) (fun key ->
          let found = Hashtbl.find_opt room.external_events key in
          Option.iter
            (fun _ ->
              remove_external_event room key;
              (* [Put_event] only updates a physical row.  Persist the detached
               registry removal separately so a restart cannot resurrect the
               promoted copy. *)
              record room (layout_change room))
            found;
          found)
    in
    let clear_event =
      if raw_is_redacted incoming then None
      else
        match clear_for decrypted incoming with
        | Some _ as clear -> clear
        | None ->
            Option.bind detached (fun event ->
                if
                  same_wire_event event.event incoming
                  && not (raw_is_redacted event.event)
                then event.clear_event
                else None)
    in
    match find_existing room incoming with
    | Some locator ->
        let previous = locator.l_chunk.ec_events.(locator.l_pos) in
        set_event room locator
          {
            stable_id = previous.stable_id;
            event = incoming;
            clear_event =
              (match clear_event with
              | Some _ -> clear_event
              | None
                when same_wire_event previous.event incoming
                     && not (raw_is_redacted incoming) ->
                  previous.clear_event
              | None -> None);
            delivery = Synced;
          }
    | None ->
        append_event room target
          {
            stable_id = stable_id t incoming;
            event = incoming;
            clear_event;
            delivery = Synced;
          }
  end

(* Does a sync window overlap the synced history already held?

   A [limited] window is the tail of the room, so an event in it that is
   already held proves the two are contiguous and there is no hole between
   them. Only synced events count: a local echo inside the window says
   nothing about the history before it, because the echo is newer than
   everything the cache holds. *)
let overlaps_held room hidden timeline =
  let ids = Hashtbl.copy hidden in
  List.iter
    (function
      | Gp _ -> ()
      | Ev chunk ->
          Array.iter
            (fun event ->
              if event.delivery = Synced then begin
                Hashtbl.replace ids event.stable_id ();
                List.iter
                  (fun key -> Hashtbl.replace ids key ())
                  (keys_of event.event)
              end)
            chunk.ec_events)
    room.chunks;
  Hashtbl.length ids > 0
  && List.exists
       (fun event -> List.exists (Hashtbl.mem ids) (keys_of event))
       timeline

(* Pull the unsent echoes out of the chunks so that a gap and the window
   that follows it go behind them; they are appended again afterwards. *)
let extract_pending room =
  let pending = ref [] in
  let chunks =
    List.filter_map
      (function
        | Gp gap -> Some (Gp gap)
        | Ev chunk ->
            let kept, unsent =
              Array.to_list chunk.ec_events
              |> List.partition (fun event -> event.delivery = Synced)
            in
            pending := !pending @ unsent;
            if kept = [] then None
            else (
              chunk.ec_events <- Array.of_list kept;
              Some (Ev chunk)))
      room.chunks
  in
  if !pending <> [] then (
    room.chunks <- chunks;
    resync room;
    record room (layout_change room));
  !pending

let normalise room =
  let chunks = normalise_chunks room.chunks in
  if List.length chunks <> List.length room.chunks then (
    room.chunks <- chunks;
    publish_structure room;
    record room (layout_change room))

let apply_room_change t (change : Matrix_client.Base_client.room_change) =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Hashtbl.remove t.forgotten_rooms
        (Id.Room_id.to_string change.changed_room_id);
      let room = room_locked t change.changed_room_id in
      let hidden = hidden_stable_ids room in
      let decrypted = Hashtbl.create (List.length change.decrypted) in
      List.iter
        (fun (event : Matrix_client.Base_client.decrypted) ->
          Option.iter
            (fun id ->
              Hashtbl.replace decrypted (Id.Event_id.to_string id)
                event.plaintext)
            event.encrypted.event_id)
        change.decrypted;
      (* A limited response means the server may have skipped events between
         what we hold and what it sent, unless the window overlaps that
         history — the case a process which reloaded its cache from disk
         hits on every start, because its first [/sync] has no [since]. The
         hole becomes a gap chunk carrying the response's [prev_batch], and
         the older history stays where it is. A window with nothing new in
         it says nothing about a hole either. *)
      let carries_new =
        List.exists
          (fun event ->
            Option.is_none (find_existing room event)
            && not (hidden_matches hidden event))
          change.timeline
      in
      let echoes =
        match change.info.prev_batch with
        | Some token
          when change.limited && carries_new
               && not (overlaps_held room hidden change.timeline) ->
            let echoes = extract_pending room in
            push_gap room token;
            echoes
        | _ -> []
      in
      (* [prev_batch] back-paginates to just before the window's first
         event, so it is the left-edge token of any chunk the window
         opens. *)
      let target = target_chunk t room ~prev_token:change.info.prev_batch in
      List.iter (merge_event t room hidden decrypted target) change.timeline;
      (* Echoes go back after the fresh window, unless the window already
         carries the sent event under the same transaction id. *)
      List.iter
        (fun echo ->
          if Option.is_none (find_existing room echo.event) then
            let target = target_chunk t room ~prev_token:None in
            append_event room target echo)
        echoes;
      normalise room;
      trim t room;
      publish_structure room;
      persist t room)

(* Everything the room holds, by both identities: an event we sent is held
   under its transaction id while it is still a local echo, and a page that
   brings it back with an event id must not add it twice. *)
let held_keys room =
  let ids = Hashtbl.create 64 in
  List.iter
    (function
      | Gp _ -> ()
      | Ev chunk ->
          Array.iter
            (fun event ->
              Hashtbl.replace ids event.stable_id ();
              List.iter
                (fun key -> Hashtbl.replace ids key ())
                (keys_of event.event))
            chunk.ec_events)
    room.chunks;
  Hashtbl.iter
    (fun stable_id () -> Hashtbl.replace ids stable_id ())
    (hidden_stable_ids room);
  ids

let ids_before room index =
  let ids = Hashtbl.create 64 in
  Hashtbl.iter
    (fun stable_id () -> Hashtbl.replace ids stable_id ())
    (hidden_stable_ids room);
  List.iteri
    (fun position -> function
      | Gp _ -> ()
      | Ev chunk ->
          if position < index then
            Array.iter
              (fun event ->
                Hashtbl.replace ids event.stable_id ();
                List.iter
                  (fun key -> Hashtbl.replace ids key ())
                  (keys_of event.event))
              chunk.ec_events)
    room.chunks;
  ids

let take index list = List.filteri (fun position _ -> position < index) list
let drop index list = List.filteri (fun position _ -> position >= index) list

(* [index] is the position in the chunk list the page was taken at.
   [from_gap] says which kind of token it was: the gap chunk sitting there,
   which the page replaces, or the [prev_token] of the events chunk sitting
   there, in front of which the page goes. Only a gap can be closed — the
   oldest edge of a contiguous room is described by a token, not by a hole,
   so paginating it never invents one. *)
let splice t room ~index ~from_gap ~events ~end_token =
  let held = held_keys room in
  let older = ids_before room index in
  let fresh =
    List.filter
      (fun (event : Event.Raw_event.t) ->
        match keys_of event with
        | [] -> true
        | keys ->
            if List.exists (Hashtbl.mem held) keys then false
            else (
              List.iter (fun key -> Hashtbl.replace held key ()) keys;
              true))
      events
  in
  (* A page can contain an event previously fetched out-of-band.  It is not
     considered held for deduplication: the page must still insert it into the
     timeline.  Remove the detached copy after remembering its plaintext. *)
  let external_clear event =
    Option.bind (remote_key event) (fun key ->
        Option.bind (Hashtbl.find_opt room.external_events key) (fun cached ->
            if
              (not (raw_is_redacted event))
              && (not (raw_is_redacted cached.event))
              && same_wire_event cached.event event
            then cached.clear_event
            else None))
  in
  (* Reaching an event held on the older side of the gap means the hole is
     filled, and so does a page that was entirely duplicates. A response with
     no [end] reached the room's beginning. *)
  let met_older =
    List.exists
      (fun event -> List.exists (Hashtbl.mem older) (keys_of event))
      events
  in
  let all_duplicates = events <> [] && fresh = [] in
  (* Match the Rust event cache: a non-empty page made entirely of duplicates
     closes the gap. Continuing from the server-provided token cannot reveal
     anything useful and can otherwise leave pagination spinning on overlap. *)
  let end_token = if all_duplicates then None else end_token in
  let entries =
    List.map
      (fun event ->
        {
          stable_id = stable_id t event;
          event;
          clear_event = external_clear event;
          delivery = Synced;
        })
      fresh
  in
  List.iter
    (fun (event : Event.Raw_event.t) ->
      Option.iter (remove_external_event room) (remote_key event))
    fresh;
  let before = take index room.chunks in
  let after = drop (if from_gap then index + 1 else index) room.chunks in
  let inserted =
    if entries = [] then []
    else
      [
        Ev
          {
            ec_id = fresh_id room;
            prev_token = end_token;
            next_token = None;
            ec_events = Array.of_list entries;
          };
      ]
  in
  let retained =
    if not from_gap then []
    else if
      Option.is_none end_token || met_older || (index > 0 && all_duplicates)
    then []
    else
      match List.nth_opt room.chunks index with
      | Some (Gp gap) ->
          gap.g_token <- Option.value end_token ~default:gap.g_token;
          [ Gp gap ]
      | _ -> []
  in
  (* With nothing inserted and no gap left, the token belongs to whatever
     now stands at the position: [None] there is the room's beginning, which
     is what makes the timeline show its start marker. *)
  (if inserted = [] && retained = [] then
     match after with Ev chunk :: _ -> chunk.prev_token <- end_token | _ -> ());
  room.chunks <- normalise_chunks (before @ retained @ inserted @ after);
  resync room;
  record room (layout_change room);
  List.iter
    (function
      | Gp _ -> ()
      | Ev chunk ->
          Array.iteri
            (fun position event ->
              record room
                (Model.Put_event { chunk_id = chunk.ec_id; position; event }))
            chunk.ec_events)
    inserted;
  entries

let prepend t room_id ~events ~prev_batch =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let room = room_locked t room_id in
      ignore
        (match room.chunks with
        | Gp _ :: _ ->
            splice t room ~index:0 ~from_gap:true ~events ~end_token:prev_batch
        | Ev _ :: _ ->
            splice t room ~index:0 ~from_gap:false ~events ~end_token:prev_batch
        | [] ->
            splice t room ~index:0 ~from_gap:false ~events ~end_token:prev_batch);
      publish_structure room;
      persist t room)

let prepend_if_token t room_id ~expected_prev_batch ~events ~prev_batch =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if room_is_forgotten t room_id then Forgotten
      else
        let room = room_locked t room_id in
        match start_token room.chunks with
        | Some token when String.equal token expected_prev_batch ->
            let inserted =
              match room.chunks with
              | Gp _ :: _ ->
                  splice t room ~index:0 ~from_gap:true ~events
                    ~end_token:prev_batch
              | Ev _ :: _ | [] ->
                  splice t room ~index:0 ~from_gap:false ~events
                    ~end_token:prev_batch
            in
            publish_structure room;
            persist t room;
            Applied
              {
                inserted;
                reached_start =
                  Option.is_none (start_token room.chunks)
                  && room.persisted_prefix = [];
              }
        | Some _ | None -> Stale)

let index_of_gap room gap_id =
  let rec walk index = function
    | [] -> None
    | Gp gap :: _ when Gap_id.equal gap.g_id gap_id -> Some index
    | _ :: rest -> walk (index + 1) rest
  in
  walk 0 room.chunks

let resolve_gap t room_id ~gap ~events ~prev_batch =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let room = room_locked t room_id in
      match index_of_gap room gap with
      | None -> ()
      | Some index ->
          ignore
            (splice t room ~index ~from_gap:true ~events ~end_token:prev_batch);
          publish_structure room;
          persist t room)

let set_decrypted t room_id ~encrypted ~plaintext =
  let changed, callbacks =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let room = room_locked t room_id in
        if raw_is_redacted encrypted then (false, [])
        else
          match find_existing room encrypted with
          | None -> (
              match remote_key encrypted with
              | None -> (false, [])
              | Some key -> (
                  match Hashtbl.find_opt room.external_events key with
                  | None -> (false, [])
                  | Some event ->
                      if raw_is_redacted event.event then (false, [])
                      else (
                        (* Detached [/event] results are deliberately not
                           physical timeline records and must not wake the
                           room-wide unread reconciler. *)
                        Hashtbl.replace room.external_events key
                          { event with clear_event = Some plaintext };
                        record room (layout_change room);
                        persist t room;
                        (true, []))))
          | Some locator ->
              let event = locator.l_chunk.ec_events.(locator.l_pos) in
              if raw_is_redacted event.event then (false, [])
              else if Option.equal ( = ) event.clear_event (Some plaintext) then
                (false, [])
              else begin
                set_event room locator
                  { event with clear_event = Some plaintext };
                persist t room;
                ( true,
                  List.map
                    (fun subscription -> subscription.decryption_callback)
                    t.decryption_subscriptions )
              end)
  in
  if changed then List.iter (notify_decryption room_id) callbacks;
  changed

let undecrypted t room_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let room = room_locked t room_id in
      let is_undecrypted (event : event) =
        Option.is_none event.clear_event
        && (not (raw_is_redacted event.event))
        && event.event.Event.Raw_event.type_
           = Event.Event_type.Room_message_encrypted
      in
      let timeline =
        flatten room.chunks
        |> List.filter_map (fun event ->
            if is_undecrypted event then Some event.event else None)
      in
      let detached =
        List.rev room.external_event_order
        |> List.filter_map (fun key ->
            Option.bind (Hashtbl.find_opt room.external_events key)
              (fun event ->
                if is_undecrypted event then Some event.event else None))
      in
      timeline @ detached)

let delivery_of_status ~error = function
  | Send_queue.Pending -> Queued
  | Send_queue.Sending -> Sending
  | Send_queue.Sent _ -> Synced
  | Send_queue.Uploaded _ -> Synced
  | Send_queue.Wedged ->
      Failed
        (match error with
        | Some e -> Matrix_client.Error.to_string e
        | None -> "the send was rejected")
  | Send_queue.Cancelled -> Failed "cancelled"

let remember_send_transaction t request =
  let transaction = Send_queue.txn_id request in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match
        List.find_opt (fun (known, _) -> known == request) t.send_transactions
      with
      | None ->
          t.send_transactions <- (request, transaction) :: t.send_transactions
      | Some (_, previous) when String.equal previous transaction -> ()
      | Some (_, previous) ->
          (* [send_queue] deliberately reuses the request record when an
             in-flight send becomes a compensating redaction. Remove only a
             still-local old echo: a synced event with the old transaction
             must remain visible until the redaction itself arrives. *)
          let room = room_locked t (Send_queue.room_id request) in
          (match locate room (by_transaction previous) with
          | Some locator
            when Option.is_none
                   locator.l_chunk.ec_events.(locator.l_pos).event.event_id ->
              remove_event room locator;
              persist t room
          | Some _ | None -> ());
          t.send_transactions <-
            (request, transaction)
            :: List.filter
                 (fun (known, _) -> known != request)
                 t.send_transactions)

let apply_request t queue request =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let room_id = Send_queue.room_id request in
      let transaction = Send_queue.txn_id request in
      let forget () =
        t.send_transactions <-
          List.filter (fun (known, _) -> known != request) t.send_transactions;
        t.forgotten_requests <-
          List.filter (fun known -> known != request) t.forgotten_requests
      in
      let request_was_forgotten =
        List.exists (fun known -> known == request) t.forgotten_requests
      in
      if request_was_forgotten || room_is_forgotten t room_id then
        match Send_queue.status request with
        | Send_queue.Cancelled | Send_queue.Sent _ -> forget ()
        | _ -> ()
      else
        let room = room_locked t room_id in
        match
          (locate room (by_transaction transaction), Send_queue.status request)
        with
        | Some locator, Send_queue.Cancelled ->
            if
              Option.is_none
                locator.l_chunk.ec_events.(locator.l_pos).event.event_id
            then remove_event room locator;
            persist t room;
            forget ()
        | Some locator, status -> (
            let cached = locator.l_chunk.ec_events.(locator.l_pos) in
            let local = Option.is_none cached.event.event_id in
            let event =
              match status with
              | Send_queue.Sent event_id when local ->
                  let event = Send_queue.local_echo queue request in
                  { event with Event.Raw_event.event_id = Some event_id }
              | Send_queue.Sent _ -> cached.event
              | _ when local -> Send_queue.local_echo queue request
              | _ -> cached.event
            in
            set_event room locator
              {
                cached with
                event;
                delivery =
                  (if Option.is_some cached.event.event_id then Synced
                   else
                     delivery_of_status
                       ~error:(Send_queue.last_error request)
                       status);
              };
            persist t room;
            match status with Send_queue.Sent _ -> forget () | _ -> ())
        | None, (Send_queue.Cancelled | Send_queue.Sent _) -> forget ()
        | None, _ -> ())

let track_send_queue t queue =
  t.tracked_queue <- Some queue;
  let ensure request =
    match Send_queue.kind request with
    | Send_queue.Upload_request _ -> ()
    | _ ->
        let admitted =
          Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
              let room_id = Send_queue.room_id request in
              if
                List.exists (fun known -> known == request) t.forgotten_requests
              then false
              else if room_is_forgotten t room_id then
                match Send_queue.status request with
                | Send_queue.Pending ->
                    (* A pending callback for an identity that was not present
                       when the room was forgotten is the queue's synchronous
                       notification for a newly enqueued request. *)
                    Hashtbl.remove t.forgotten_rooms
                      (Id.Room_id.to_string room_id);
                    true
                | _ -> false
              else true)
        in
        if admitted then begin
          remember_send_transaction t request;
          Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
              let room = room_locked t (Send_queue.room_id request) in
              let transaction = Send_queue.txn_id request in
              match locate room (by_transaction transaction) with
              | Some _ -> ()
              | None ->
                  let stable_id = stable_id_of_transaction transaction in
                  ignore (promote_hidden_stable_id room stable_id);
                  let target = target_chunk t room ~prev_token:None in
                  append_event room target
                    {
                      stable_id;
                      event = Send_queue.local_echo queue request;
                      clear_event = None;
                      delivery =
                        delivery_of_status
                          ~error:(Send_queue.last_error request)
                          (Send_queue.status request);
                    };
                  persist t room)
        end
  in
  List.iter ensure (Send_queue.requests queue);
  Send_queue.on_change queue (fun request ->
      ensure request;
      apply_request t queue request)

let find_echo t request =
  with_room t (Send_queue.room_id request) (fun room ->
      Option.map
        (fun locator -> locator.l_chunk.ec_events.(locator.l_pos))
        (locate room (by_transaction (Send_queue.txn_id request))))
