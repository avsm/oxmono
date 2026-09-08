module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Read_state = Matrix_client.Read_state

type reaction = {
  key : string;
  count : int;
  senders : Id.User_id.t list;
  own : bool;
}

type event_item = {
  id : string;
  event : Presentation.t;
  delivery : Event_cache.delivery;
  reactions : reaction list;
  edited : bool;
  redacted : bool;
  reply_to : Id.Event_id.t option;
}

type gap_id = Event_cache.Gap_id.t
type date = { year : int; month : int; day : int }

type virtual_item =
  | Gap of { id : gap_id }
  | Timeline_start
  | Read_marker
  | Date_divider of date

type item =
  | Event of event_item
  | Virtual of { id : string; content : virtual_item }

type event_filter = Presentation.t -> bool

(* One cached event's projection, kept between refreshes so that
   [Presentation.of_event] — which parses and sanitises HTML through
   [markup] — runs once per event rather than once per change. The entry is
   valid while the cache holds the very same record; any edit, decryption
   or delivery change replaces that record and so invalidates it. *)
type memo = {
  cached : Event_cache.event;
  presented : Presentation.t;
  mutable edited : (Jsont.json * Presentation.t) option;
}

type relation_kind =
  | Redaction_relation of string
  | Reaction_relation of {
      target : string;
      key : string;
      sender : Id.User_id.t;
    }
  | Edit_relation of string

type relation_entry = {
  stable_id : string;
  kind : relation_kind;
  mutable memo : memo;
  mutable position : int;
}

type relation_index = {
  by_stable : (string, relation_entry) Hashtbl.t;
  by_target : (string, relation_entry list) Hashtbl.t;
  redactions : (string, int) Hashtbl.t;
}

type t = {
  room_id : Id.Room_id.t;
  cache : Event_cache.t;
  client : Matrix_client.Client.t;
  queue : Matrix_client.Send_queue.t;
  resolve_mxc : (string -> string option) option;
  encryption : Matrix_client.Encryption.t option;
  own_user : Id.User_id.t option;
  read_state : (unit -> Matrix_client.Read_state.t) option;
  read_marker : (unit -> Id.Event_id.t option) option;
  event_filter : event_filter;
  items : item Observable.List.t;
  loading : bool Observable.Value.t;
  pagination_error : Matrix_client.Error.t option Observable.Value.t;
  pagination_mutex : Eio.Mutex.t;
  mutable memo : (string, memo) Hashtbl.t;
  relations : relation_index;
  (* Requests do not synchronously update the sync service's receipt state.
     Keep successful local sends here so two UI actions in one sync interval
     cannot send the same (or an older) receipt twice. *)
  local_receipts : (string, Id.Event_id.t) Hashtbl.t;
  mutable close : unit -> unit;
}

let room_id t = t.room_id
let items t = t.items
let snapshot t = Observable.List.snapshot t.items
let loading t = t.loading
let pagination_error t = t.pagination_error

let item_id = function
  | Event event -> event.id
  | Virtual virtual_ -> virtual_.id

let equal_reaction left right =
  String.equal left.key right.key
  && Int.equal left.count right.count
  && Bool.equal left.own right.own
  && List.equal Id.User_id.equal left.senders right.senders

let equal_item left right =
  match (left, right) with
  | Event left, Event right ->
      String.equal left.id right.id
      && Presentation.equal left.event right.event
      && left.delivery = right.delivery
      && List.equal equal_reaction left.reactions right.reactions
      && Bool.equal left.edited right.edited
      && Bool.equal left.redacted right.redacted
      && Option.equal Id.Event_id.equal left.reply_to right.reply_to
  | Virtual left, Virtual right ->
      String.equal left.id right.id && left.content = right.content
  | Event _, Virtual _ | Virtual _, Event _ -> false

let event_id event =
  Option.map Id.Event_id.to_string event.Event.Raw_event.event_id

let present_memos t cached =
  let live = Hashtbl.create (Array.length cached) in
  let presented =
    Array.map
      (fun (event : Event_cache.event) ->
        let memo =
          match Hashtbl.find_opt t.memo event.stable_id with
          | Some memo when memo.cached == event -> memo
          | _ ->
              {
                cached = event;
                presented =
                  Presentation.of_event ?resolve_mxc:t.resolve_mxc
                    (Event_cache.effective event);
                edited = None;
              }
        in
        Hashtbl.replace live event.stable_id memo;
        memo)
      cached
  in
  t.memo <- live;
  presented

let relation_target = function
  | Redaction_relation target -> target
  | Reaction_relation { target; _ } -> target
  | Edit_relation target -> target

let equal_relation_kind left right =
  match (left, right) with
  | Redaction_relation left, Redaction_relation right -> String.equal left right
  | Edit_relation left, Edit_relation right -> String.equal left right
  | ( Reaction_relation
        { target = left_target; key = left_key; sender = left_sender },
      Reaction_relation
        { target = right_target; key = right_key; sender = right_sender } ) ->
      String.equal left_target right_target
      && String.equal left_key right_key
      && Id.User_id.equal left_sender right_sender
  | Redaction_relation _, _ | Reaction_relation _, _ | Edit_relation _, _ ->
      false

let relation_kind memo =
  match memo.presented.Presentation.content with
  | Presentation.Redaction { target = Some target; _ } ->
      Some (Redaction_relation (Id.Event_id.to_string target))
  | Presentation.Reaction { key; target } ->
      Some
        (Reaction_relation
           {
             target = Id.Event_id.to_string target;
             key;
             sender = memo.presented.Presentation.sender;
           })
  | _ -> (
      match memo.presented.Presentation.relation with
      | Some { target; kind = Presentation.Replacement } ->
          Some (Edit_relation (Id.Event_id.to_string target))
      | _ -> None)

let remove_relation_entry index entry =
  Hashtbl.remove index.by_stable entry.stable_id;
  let target = relation_target entry.kind in
  (match Hashtbl.find_opt index.by_target target with
  | None -> ()
  | Some entries ->
      let entries =
        List.filter
          (fun candidate -> candidate.stable_id <> entry.stable_id)
          entries
      in
      if entries = [] then Hashtbl.remove index.by_target target
      else Hashtbl.replace index.by_target target entries);
  match entry.kind with
  | Redaction_relation target -> (
      match Hashtbl.find_opt index.redactions target with
      | Some count when count > 1 ->
          Hashtbl.replace index.redactions target (count - 1)
      | Some _ | None -> Hashtbl.remove index.redactions target)
  | Reaction_relation _ | Edit_relation _ -> ()

let add_relation_entry index entry =
  Hashtbl.replace index.by_stable entry.stable_id entry;
  let target = relation_target entry.kind in
  Hashtbl.replace index.by_target target
    (entry :: Option.value (Hashtbl.find_opt index.by_target target) ~default:[]);
  match entry.kind with
  | Redaction_relation target ->
      Hashtbl.replace index.redactions target
        (1 + Option.value (Hashtbl.find_opt index.redactions target) ~default:0)
  | Reaction_relation _ | Edit_relation _ -> ()

let relation_entry_is_redacted index entry =
  Option.fold ~none:false
    ~some:(Hashtbl.mem index.redactions)
    (event_id (entry : relation_entry).memo.presented.Presentation.raw)

let sync_relation_index index presented =
  let live = Hashtbl.create (Array.length presented) in
  Array.iteri
    (fun position memo ->
      Hashtbl.replace live memo.cached.stable_id ();
      match relation_kind memo with
      | None -> (
          match Hashtbl.find_opt index.by_stable memo.cached.stable_id with
          | Some entry -> remove_relation_entry index entry
          | None -> ())
      | Some kind -> (
          match Hashtbl.find_opt index.by_stable memo.cached.stable_id with
          | Some entry when equal_relation_kind entry.kind kind ->
              entry.memo <- memo;
              entry.position <- position
          | Some entry ->
              remove_relation_entry index entry;
              add_relation_entry index
                { stable_id = memo.cached.stable_id; kind; memo; position }
          | None ->
              add_relation_entry index
                { stable_id = memo.cached.stable_id; kind; memo; position }))
    presented;
  let stale =
    Hashtbl.fold
      (fun stable_id entry stale ->
        if Hashtbl.mem live stable_id then stale else entry :: stale)
      index.by_stable []
  in
  List.iter (fun entry -> remove_relation_entry index entry) stale

let clear_relation_index index =
  Hashtbl.clear index.by_stable;
  Hashtbl.clear index.by_target;
  Hashtbl.clear index.redactions

let present t cached =
  let presented = present_memos t cached in
  sync_relation_index t.relations presented;
  presented

let reactions_for own_user index target =
  let by_key = Hashtbl.create 8 in
  Option.value (Hashtbl.find_opt index.by_target target) ~default:[]
  |> List.filter (fun entry -> not (relation_entry_is_redacted index entry))
  |> List.filter_map (fun entry ->
      match entry.kind with
      | Reaction_relation { key; sender; _ } ->
          Some (entry.position, key, sender)
      | Redaction_relation _ | Edit_relation _ -> None)
  |> List.sort (fun (left, _, _) (right, _, _) -> Int.compare right left)
  |> List.iter (fun (_, key, sender) ->
      let senders = Option.value (Hashtbl.find_opt by_key key) ~default:[] in
      if not (List.exists (Id.User_id.equal sender) senders) then
        Hashtbl.replace by_key key (sender :: senders));
  Hashtbl.to_seq by_key
  |> Seq.map (fun (key, senders) ->
      let senders = List.rev senders in
      {
        key;
        count = List.length senders;
        senders;
        own =
          Option.fold ~none:false
            ~some:(fun own -> List.exists (Id.User_id.equal own) senders)
            own_user;
      })
  |> List.of_seq
  |> List.sort (fun left right -> String.compare left.key right.key)

let apply_edit ?resolve_mxc index target memo =
  let original = memo.presented in
  let candidates =
    Option.value (Hashtbl.find_opt index.by_target target) ~default:[]
    |> List.filter (fun entry -> not (relation_entry_is_redacted index entry))
    |> List.filter (fun entry ->
        match entry.kind with
        | Edit_relation _ -> true
        | Redaction_relation _ | Reaction_relation _ -> false)
  in
  let candidate =
    candidates
    |> List.filter (fun edit ->
        Id.User_id.equal
          (edit : relation_entry).memo.presented.Presentation.sender
          original.Presentation.sender)
    |> List.sort (fun left right ->
        let timestamp =
          Event.Timestamp.compare
            (right : relation_entry).memo.presented.Presentation.timestamp
            left.memo.presented.Presentation.timestamp
        in
        if timestamp <> 0 then timestamp
        else Int.compare right.position left.position)
    |> List.find_map (fun edit ->
        Presentation.new_content
          (Event_cache.effective (edit : relation_entry).memo.cached))
  in
  match candidate with
  | None -> (original, false)
  | Some content -> (
      match memo.edited with
      | Some (cached, presented) when cached == content -> (presented, true)
      | _ ->
          let presented =
            Presentation.of_event ?resolve_mxc
              { original.Presentation.raw with Event.Raw_event.content }
          in
          memo.edited <- Some (content, presented);
          (presented, true))

let edit_revisions t ~event_id =
  let cached = Event_cache.snapshot t.cache t.room_id |> Array.to_list in
  let event_id_of_cached (event : Event_cache.event) =
    Option.map Id.Event_id.to_string event.event.Event.Raw_event.event_id
  in
  let original =
    List.find_opt
      (fun event ->
        Option.equal String.equal (event_id_of_cached event)
          (Some (Id.Event_id.to_string event_id)))
      cached
  in
  match original with
  | None -> []
  | Some original ->
      let original_presented =
        Presentation.of_event ?resolve_mxc:t.resolve_mxc
          (Event_cache.effective original)
      in
      let encrypted (event : Event_cache.event) =
        String.equal
          (Event.Event_type.to_string event.event.Event.Raw_event.type_)
          "m.room.encrypted"
      in
      let redacted = Hashtbl.create 16 in
      List.iter
        (fun event ->
          match
            (Presentation.of_event ?resolve_mxc:t.resolve_mxc
               (Event_cache.effective event))
              .content
          with
          | Presentation.Redaction { target = Some target; _ } ->
              Hashtbl.replace redacted (Id.Event_id.to_string target) ()
          | _ -> ())
        cached;
      let candidates =
        cached
        |> List.filter_map (fun event ->
            match event_id_of_cached event with
            | None -> None
            | Some id when Hashtbl.mem redacted id -> None
            | Some id ->
                let presented =
                  Presentation.of_event ?resolve_mxc:t.resolve_mxc
                    (Event_cache.effective event)
                in
                if
                  Presentation.is_valid_replacement_with_encryption
                    ~original:original_presented
                    ~original_encrypted:(encrypted original)
                    ~replacement:presented
                    ~replacement_encrypted:(encrypted event)
                  &&
                  match presented.relation with
                  | Some { kind = Replacement; target } ->
                      Id.Event_id.equal target event_id
                  | _ -> false
                then Some (id, presented, encrypted event)
                else None)
        |> List.sort (fun (left_id, left, _) (right_id, right, _) ->
            let timestamp =
              Event.Timestamp.compare left.Presentation.timestamp
                right.Presentation.timestamp
            in
            if timestamp <> 0 then timestamp
            else String.compare left_id right_id)
      in
      let seen = Hashtbl.create (List.length candidates) in
      let revisions =
        List.filter_map
          (fun (id, presented, _) ->
            if Hashtbl.mem seen id then None
            else (
              Hashtbl.replace seen id ();
              Presentation.replacement ?resolve_mxc:t.resolve_mxc presented))
          candidates
      in
      original_presented :: revisions

let date_of_event event =
  Event.Timestamp.to_ptime_opt event.Event.Raw_event.origin_server_ts
  |> Option.map Ptime.to_date

(* The rule is to keep the state events a member is told about and drop the
   configuration a room is built out of. [Event_cache.snapshot] drops
   nothing, for a caller who wants the rest. *)
let should_hide event =
  match event.Presentation.content with
  | Presentation.Reaction _ | Presentation.Redaction _ -> true
  | Presentation.Membership { change = Presentation.No_change; _ } -> true
  | Presentation.State { state; _ } -> (
      match state with
      | Presentation.Room_name _ | Presentation.Room_topic _
      | Presentation.Room_avatar _ | Presentation.Room_canonical_alias _
      | Presentation.Room_encryption | Presentation.Room_pinned_events
      | Presentation.Room_tombstone _ ->
          false
      | Presentation.Room_create | Presentation.Room_power_levels
      | Presentation.Room_join_rules | Presentation.Room_history_visibility
      | Presentation.Room_guest_access | Presentation.Room_server_acl
      | Presentation.Room_third_party_invite _ | Presentation.Policy_rule _
      | Presentation.Space_child | Presentation.Space_parent
      | Presentation.Beacon_info _ | Presentation.Other_state_type _ ->
          true)
  | _ -> (
      match event.relation with
      | Some { kind = Replacement; _ } -> true
      | _ -> false)

(* Relation events rejected here are still inspected by the relation index, so
   callers can compose this predicate with their own visibility choices without
   losing edit, reaction or redaction aggregation. *)
let default_event_filter event = not (should_hide event)

(* An undecryptable ciphertext remains an item even when a caller's filter
   rejects everything else. Keeping it visible preserves the retry/decryption
   signal; once plaintext replaces it, the caller's predicate applies. *)
let passes_event_filter t event =
  match event.Presentation.content with
  | Presentation.Unable_to_decrypt -> true
  | _ -> t.event_filter event

(* The marker goes immediately after the [m.fully_read] event, skipping
   forward over the run of the own user's events that follows it, and is not
   shown at all when nothing is marked read, when the marked event is not
   among the items, or when the position it lands on is the last one: a
   marker at the end tells a reader nothing. *)
let insert_read_marker own_user fully_read items =
  match fully_read with
  | None -> items
  | Some fully_read -> (
      let items = Array.of_list items in
      let length = Array.length items in
      let target = Id.Event_id.to_string fully_read in
      let found = ref None in
      Array.iteri
        (fun index -> function
          | Event event
            when Option.equal String.equal
                   (event_id event.event.Presentation.raw)
                   (Some target) ->
              found := Some index
          | _ -> ())
        items;
      match !found with
      | None -> Array.to_list items
      | Some index ->
          let is_own item =
            match (item, own_user) with
            | Event event, Some own ->
                Id.User_id.equal event.event.Presentation.sender own
            | Event _, None -> false
            | Virtual _, _ -> true
          in
          let rec skip position =
            if position >= length then length - 1
            else if is_own items.(position) then skip (position + 1)
            else position - 1
          in
          let anchor = skip (index + 1) in
          if anchor + 1 >= length then Array.to_list items
          else
            Array.to_list (Array.sub items 0 (anchor + 1))
            @ Virtual { id = "virtual:read-marker"; content = Read_marker }
              :: Array.to_list
                   (Array.sub items (anchor + 1) (length - anchor - 1)))

let project t cached =
  let presented = present t cached in
  let gaps = Observable.Value.get (Event_cache.gaps t.cache t.room_id) in
  let result = ref [] in
  let last_date = ref None in
  let emit_gaps position =
    List.iter
      (fun (gap : Event_cache.gap) ->
        if gap.index = position then
          result :=
            Virtual
              {
                id = "virtual:gap:" ^ Event_cache.Gap_id.to_string gap.id;
                content = Gap { id = gap.id };
              }
            :: !result)
      gaps
  in
  emit_gaps 0;
  if
    !result = []
    && Option.is_none
         (Observable.Value.get (Event_cache.prev_batch t.cache t.room_id))
  then
    result :=
      [ Virtual { id = "virtual:timeline-start"; content = Timeline_start } ];
  Array.iteri
    (fun position memo ->
      if position > 0 then emit_gaps position;
      let cached = memo.cached in
      let original = memo.presented in
      let raw = original.Presentation.raw in
      if passes_event_filter t original then (
        let date = date_of_event raw in
        if date <> !last_date then (
          Option.iter
            (fun (year, month, day) ->
              result :=
                Virtual
                  {
                    id =
                      Printf.sprintf "virtual:date:%04d-%02d-%02d" year month
                        day;
                    content = Date_divider { year; month; day };
                  }
                :: !result)
            date;
          last_date := date);
        let target = event_id raw in
        let event, edited =
          match target with
          | None -> (original, false)
          | Some id -> apply_edit ?resolve_mxc:t.resolve_mxc t.relations id memo
        in
        let reaction_list =
          match target with
          | None -> []
          | Some id -> reactions_for t.own_user t.relations id
        in
        let redacted =
          Option.fold ~none:false
            ~some:(Hashtbl.mem t.relations.redactions)
            target
          || Option.fold ~none:false
               ~some:(fun unsigned ->
                 Option.is_some (Event.Unsigned.redacted_because unsigned))
               raw.unsigned
        in
        let reply_to =
          match original.Presentation.relation with
          | Some { target; kind = Reply } -> Some target
          | _ -> None
        in
        result :=
          Event
            {
              id = cached.stable_id;
              event;
              delivery = cached.delivery;
              reactions = reaction_list;
              edited;
              redacted;
              reply_to;
            }
          :: !result))
    presented;
  emit_gaps (Array.length presented);
  let items = List.rev !result in
  insert_read_marker t.own_user
    (match t.read_marker with None -> None | Some marker -> marker ())
    items

let refresh t =
  let projected = project t (Event_cache.snapshot t.cache t.room_id) in
  Observable.List.reconcile_by ~key:item_id ~equal:equal_item t.items projected

let create ~sw ~client ~send_queue ?encryption ?own_user ?read_state
    ?read_marker ?resolve_mxc ?(event_filter = default_event_filter) cache
    room_id =
  let t =
    {
      room_id;
      cache;
      client;
      queue = send_queue;
      resolve_mxc;
      encryption;
      own_user;
      read_state;
      read_marker;
      event_filter;
      items = Observable.List.create [];
      loading = Observable.Value.create false;
      pagination_error = Observable.Value.create None;
      pagination_mutex = Eio.Mutex.create ();
      memo = Hashtbl.create 64;
      relations =
        {
          by_stable = Hashtbl.create 64;
          by_target = Hashtbl.create 32;
          redactions = Hashtbl.create 16;
        };
      local_receipts = Hashtbl.create 8;
      close = (fun () -> ());
    }
  in
  (* Subscribe before the first projection so that no change to the cache
     can slip between the snapshot and the subscription. *)
  let _, event_subscription =
    Observable.List.subscribe ~sw (Event_cache.events cache room_id)
  in
  let _, gap_subscription =
    Observable.Value.subscribe ~sw (Event_cache.gaps cache room_id)
  in
  let _, token_subscription =
    Observable.Value.subscribe ~sw (Event_cache.prev_batch cache room_id)
  in
  t.close <-
    (fun () ->
      Observable.List.unsubscribe event_subscription;
      Observable.Value.unsubscribe gap_subscription;
      Observable.Value.unsubscribe token_subscription);
  refresh t;
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        match Observable.List.next event_subscription with
        | None -> ()
        | Some _ ->
            refresh t;
            loop ()
      in
      loop ();
      `Stop_daemon);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        match Observable.Value.next gap_subscription with
        | None -> ()
        | Some _ ->
            refresh t;
            loop ()
      in
      loop ();
      `Stop_daemon);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        match Observable.Value.next token_subscription with
        | None -> ()
        | Some _ ->
            refresh t;
            loop ()
      in
      loop ();
      `Stop_daemon);
  t

(* Unsubscribing is what the daemon fibers block on: [next] then returns
   [None] and each of them returns. *)
let close t = t.close ()

let discard t =
  close t;
  Hashtbl.clear t.memo;
  clear_relation_index t.relations;
  Observable.List.reconcile_by ~key:item_id ~equal:equal_item t.items [];
  Observable.Value.set t.loading false;
  Observable.Value.set t.pagination_error None

let plaintext_of (event : Event.Raw_event.t)
    (decrypted : Matrix_client.Encryption.decrypted_event) =
  {
    event with
    Event.Raw_event.type_ = Event.Event_type.of_string decrypted.decrypted_type;
    content = decrypted.decrypted_content;
  }

let decrypt_page t events =
  Option.iter
    (fun encryption ->
      events
      |> List.filter (fun (event : Event.Raw_event.t) ->
          event.type_ = Event.Event_type.Room_message_encrypted)
      |> List.iter (fun encrypted ->
          match
            Matrix_client.Encryption.decrypt_room_event encryption t.room_id
              encrypted
          with
          | Ok decrypted ->
              ignore
                (Event_cache.set_decrypted t.cache t.room_id ~encrypted
                   ~plaintext:(plaintext_of encrypted decrypted))
          | Error _ -> ()))
    t.encryption

let paginate t ~limit ~from ~splice =
  Observable.Value.set t.loading true;
  Fun.protect
    ~finally:(fun () -> Observable.Value.set t.loading false)
    (fun () ->
      match
        Matrix_client.Messages.get_messages t.client ~room_id:t.room_id ~from
          ~dir:Matrix_proto.Common.Direction.Backward ~limit ()
      with
      | Error error ->
          Observable.Value.set t.pagination_error (Some error);
          Error error
      | Ok response ->
          let page = response.page in
          let oldest_first = List.rev page.Matrix_proto.Common.Page.chunk in
          splice ~events:oldest_first
            ~prev_batch:page.Matrix_proto.Common.Page.next_batch;
          decrypt_page t oldest_first;
          Observable.Value.set t.pagination_error None;
          Ok ())

type pagination = [ `Reached_start | `More | `Nothing_to_do ]

(* A pagination that cannot take the lock answers [`Nothing_to_do] instead
   of waiting for the one in flight: a caller that loops on the result would
   otherwise queue behind it and then fetch a page from a token the first
   call has already spent. *)
let exclusively t f =
  if Eio.Mutex.try_lock t.pagination_mutex then
    Fun.protect
      ~finally:(fun () -> Eio.Mutex.unlock t.pagination_mutex)
      (fun () -> (f () :> (pagination, Matrix_client.Error.t) result))
  else Ok `Nothing_to_do

let gap_named t id =
  List.find_opt
    (fun (gap : Event_cache.gap) -> Event_cache.Gap_id.equal gap.id id)
    (Observable.Value.get (Event_cache.gaps t.cache t.room_id))

let token t = Observable.Value.get (Event_cache.prev_batch t.cache t.room_id)

let paginate_back t ?(limit = 30) () =
  if limit < 1 then invalid_arg "Matrix_ui.Room_timeline.paginate_back";
  exclusively t (fun () ->
      let local_result () =
        Observable.Value.set t.loading true;
        Fun.protect
          ~finally:(fun () -> Observable.Value.set t.loading false)
          (fun () ->
            match Event_cache.hydrate_previous t.cache t.room_id with
            | Event_cache.Hydrated events ->
                (* Hydrated records have already crossed the persistence
                   boundary; decrypt them through the same path as a remote
                   page before publishing the projection. *)
                decrypt_page t
                  (List.map
                     (fun (event : Event_cache.event) -> event.event)
                     events);
                refresh t;
                Observable.Value.set t.pagination_error None;
                `Result
                  (Ok
                     (match token t with
                     | None
                       when not
                              (Event_cache.has_unloaded_history t.cache
                                 t.room_id) ->
                         `Reached_start
                     | None -> `More
                     | Some _ -> `More))
            | Event_cache.Hydration_failed error ->
                let error =
                  Matrix_client.Error.Json_error
                    (Event_store.Error.to_string error)
                in
                Observable.Value.set t.pagination_error (Some error);
                `Result (Error error)
            | Event_cache.No_persisted_history -> `Network)
      in
      match local_result () with
      | `Result result -> result
      | `Network -> (
          match token t with
          | None when Event_cache.has_unloaded_history t.cache t.room_id ->
              let error =
                Matrix_client.Error.Json_error
                  "persisted Matrix history has no reachable predecessor"
              in
              Observable.Value.set t.pagination_error (Some error);
              Error error
          | None -> Ok `Nothing_to_do
          | Some from -> (
              match
                paginate t ~limit ~from ~splice:(fun ~events ~prev_batch ->
                    Event_cache.prepend t.cache t.room_id ~events ~prev_batch)
              with
              | Error error -> Error error
              | Ok () ->
                  (* The daemon fiber would re-project too, a scheduler turn
                     later; doing it here is what lets a caller read [items]
                     on the line after the call and see the page. *)
                  refresh t;
                  Ok
                    (match token t with
                    | None
                      when not
                             (Event_cache.has_unloaded_history t.cache t.room_id)
                      ->
                        `Reached_start
                    | None -> `More
                    | Some _ -> `More))))

let paginate_gap t ?(limit = 30) ~gap () =
  if limit < 1 then invalid_arg "Matrix_ui.Room_timeline.paginate_gap";
  exclusively t (fun () ->
      match gap_named t gap with
      | None -> Ok `Nothing_to_do
      | Some found -> (
          match
            paginate t ~limit ~from:found.token
              ~splice:(fun ~events ~prev_batch ->
                Event_cache.resolve_gap t.cache t.room_id ~gap ~events
                  ~prev_batch)
          with
          | Error error -> Error error
          | Ok () ->
              refresh t;
              Ok
                (if
                   Option.is_some (gap_named t gap)
                   || Event_cache.has_unloaded_history t.cache t.room_id
                 then `More
                 else `Reached_start)))

let jstring = Jsont.Json.string

let jobject members =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       members)

let msgtype_of = function
  | `Text -> "m.text"
  | `Notice -> "m.notice"
  | `Emote -> "m.emote"

let reply_relation event_id =
  ( "m.relates_to",
    jobject
      [
        ( "m.in_reply_to",
          jobject [ ("event_id", jstring (Id.Event_id.to_string event_id)) ] );
      ] )

let send_message t ?(msgtype = `Text) ?formatted ?reply_to ~body () =
  let formatted =
    match formatted with
    | None -> []
    | Some html ->
        [
          ("format", jstring "org.matrix.custom.html");
          ("formatted_body", jstring (Presentation.Html.sanitize html));
        ]
  in
  let relation =
    match reply_to with
    | None -> []
    | Some event_id -> [ reply_relation event_id ]
  in
  let content =
    jobject
      (("msgtype", jstring (msgtype_of msgtype))
      :: ("body", jstring body)
      :: (formatted @ relation))
  in
  Matrix_client.Send_queue.send_message t.queue ~room_id:t.room_id
    ~event_type:"m.room.message" ~content

let send_text t ~body = send_message t ~body ()

let send_reply t ?formatted ~event_id ~body () =
  send_message t ?formatted ~reply_to:event_id ~body ()

type location_asset = Self | Pin | Custom of string

let location_asset_type = function
  | Self -> "m.self"
  | Pin -> "m.pin"
  | Custom event_type -> event_type

let send_location t ?description ?zoom_level ?asset ?reply_to ~geo_uri ~body ()
    =
  Option.iter
    (fun zoom ->
      if zoom < 0 || zoom > 20 then
        invalid_arg "Matrix_ui.Room_timeline.send_location: zoom_level")
    zoom_level;
  let location =
    ("uri", jstring geo_uri)
    :: Option.to_list
         (Option.map (fun value -> ("description", jstring value)) description)
    @ Option.to_list
        (Option.map
           (fun value -> ("zoom_level", Jsont.Json.int value))
           zoom_level)
  in
  let asset =
    Option.to_list
      (Option.map
         (fun value ->
           ( "org.matrix.msc3488.asset",
             jobject [ ("type", jstring (location_asset_type value)) ] ))
         asset)
  in
  let relation = Option.to_list (Option.map reply_relation reply_to) in
  let content =
    jobject
      ([
         ("msgtype", jstring "m.location");
         ("body", jstring body);
         ("geo_uri", jstring geo_uri);
         ("org.matrix.msc3488.location", jobject location);
       ]
      @ asset @ relation)
  in
  Matrix_client.Send_queue.send_message t.queue ~room_id:t.room_id
    ~event_type:"m.room.message" ~content

let send_edit t ~event_id ?formatted ~body () =
  let formatted_body = Option.map Presentation.Html.sanitize formatted in
  Matrix_client.Send_queue.send_edit t.queue ~room_id:t.room_id ~event_id
    ~new_body:body ?formatted_body ()

let send_reaction t ~relates_to ~key =
  Matrix_client.Send_queue.send_reaction t.queue ~room_id:t.room_id ~relates_to
    ~key

let redact t ~event_id ?reason () =
  Matrix_client.Send_queue.send_redaction t.queue ~room_id:t.room_id ~event_id
    ?reason ()

let item_of_request t request =
  match Event_cache.find_echo t.cache request with
  | None -> None
  | Some echo ->
      Array.find_map
        (function
          | Event item when String.equal item.id echo.stable_id -> Some item
          | Event _ | Virtual _ -> None)
        (Observable.List.snapshot t.items)

let delivery t request =
  Option.map (fun item -> item.delivery) (item_of_request t request)

type receipt_type = Matrix_client.Receipts.receipt_type =
  | Read
  | Read_private
  | Fully_read

type receipt = { receipt_type : receipt_type; event_id : Id.Event_id.t }

let receipt_key receipt_type thread_id =
  Matrix_client.Receipts.receipt_type_to_string receipt_type
  ^ ":"
  ^ Option.fold ~none:"" ~some:Id.Event_id.to_string thread_id

let receipt_target_of_state state receipt_type thread_id =
  match (receipt_type, thread_id) with
  | Fully_read, None -> Read_state.fully_read state
  | Fully_read, Some _ -> None
  | Read, None ->
      Option.map (fun r -> r.Read_state.event_id) (Read_state.public_read state)
  | Read_private, None ->
      Option.map
        (fun r -> r.Read_state.event_id)
        (Read_state.private_read state)
  | Read, Some thread_id ->
      Option.map
        (fun r -> r.Read_state.event_id)
        (Read_state.thread_public_read state ~thread_id)
  | Read_private, Some thread_id ->
      Option.map
        (fun r -> r.Read_state.event_id)
        (Read_state.thread_private_read state ~thread_id)

let event_in_thread ~thread_id (event : Presentation.t) =
  match event.event_id with
  | Some id when Id.Event_id.equal id thread_id -> true
  | _ -> (
      match event.relation with
      | Some { target; kind = Presentation.Thread } ->
          Id.Event_id.equal target thread_id
      | _ -> false)

let cached_presentations t =
  Event_cache.snapshot t.cache t.room_id
  |> Array.map (fun event ->
      Presentation.of_event ?resolve_mxc:t.resolve_mxc
        (Event_cache.effective event))

let cached_position t ?thread_id event_id =
  let presentations = cached_presentations t in
  let found = ref None in
  Array.iteri
    (fun index (event : Presentation.t) ->
      if
        Option.equal Id.Event_id.equal event.event_id (Some event_id)
        &&
        match thread_id with
        | None -> true
        | Some thread_id -> event_in_thread ~thread_id event
      then found := Some index)
    presentations;
  !found

let newer_cached_event t ?thread_id left right =
  match
    (cached_position t ?thread_id left, cached_position t ?thread_id right)
  with
  | Some left_position, Some right_position ->
      if left_position >= right_position then left else right
  | Some _, None -> left
  | None, Some _ -> right
  | None, None -> right

let local_receipt_target t receipt_type thread_id =
  Hashtbl.find_opt t.local_receipts (receipt_key receipt_type thread_id)

let read_state_target t receipt_type thread_id =
  Option.bind t.read_state (fun read_state ->
      receipt_target_of_state (read_state ()) receipt_type thread_id)

let latest_target t receipt_type thread_id =
  match
    ( local_receipt_target t receipt_type thread_id,
      read_state_target t receipt_type thread_id )
  with
  | None, None -> None
  | Some target, None | None, Some target -> Some target
  | Some local, Some state -> (
      match
        (cached_position t ?thread_id local, cached_position t ?thread_id state)
      with
      | Some local_position, Some state_position ->
          Some (if local_position >= state_position then local else state)
      (* A successful local send is the only ordering information available
         until the next sync when neither target is cached. *)
      | Some _, None | None, None -> Some local
      | None, Some _ -> Some state)

let implicit_latest_target t ?thread_id () =
  match t.own_user with
  | None -> None
  | Some own_user ->
      cached_presentations t |> Array.to_list
      |> List.filter_map (fun (event : Presentation.t) ->
          match event.event_id with
          | None -> None
          | Some event_id
            when Id.User_id.equal event.sender own_user
                 &&
                 match thread_id with
                 | Some thread_id -> event_in_thread ~thread_id event
                 | None -> Read_state.is_main_timeline_event event.raw ->
              Some event_id
          | Some _ -> None)
      |> List.fold_left
           (fun latest candidate ->
             match latest with
             | None -> Some candidate
             | Some latest ->
                 Some (newer_cached_event t ?thread_id latest candidate))
           None

let latest_user_read_receipt t ?receipt_type ?thread_id () =
  let kinds =
    match receipt_type with
    | Some receipt_type -> [ receipt_type ]
    | None -> [ Read; Read_private ]
  in
  let explicit =
    List.fold_left
      (fun latest receipt_type ->
        match (latest, latest_target t receipt_type thread_id) with
        | None, target -> target
        | target, None -> target
        | Some latest, Some candidate ->
            Some (newer_cached_event t ?thread_id latest candidate))
      None kinds
  in
  match receipt_type with
  | Some _ -> explicit
  | None -> (
      match (explicit, implicit_latest_target t ?thread_id ()) with
      | None, target -> target
      | target, None -> target
      | Some explicit, Some implicit ->
          Some (newer_cached_event t ?thread_id explicit implicit))

let receipt_is_newer t ?thread_id ~current ~candidate () =
  match Id.Event_id.equal current candidate with
  | true -> false
  | false -> (
      match
        ( cached_position t ?thread_id current,
          cached_position t ?thread_id candidate )
      with
      | Some current, Some candidate -> candidate > current
      (* An event outside this timeline cannot establish a regression. Let the
         server decide, as the low-level endpoint is monotonic too. *)
      | _ -> true)

let clear_marked_unread t =
  Matrix_client.Account_data.set_marked_unread t.client ~room_id:t.room_id
    ~unread:false

let receipt_target_away_from_own_event t ?thread_id ~event_id ~marking () =
  match t.own_user with
  | None -> Some event_id
  | Some own_user -> (
      let events = cached_presentations t in
      let target_position =
        Array.find_index
          (fun (event : Presentation.t) ->
            Option.equal Id.Event_id.equal event.event_id (Some event_id))
          events
      in
      let is_candidate (event : Presentation.t) =
        (not (Id.User_id.equal event.sender own_user))
        &&
        match thread_id with
        | None -> true
        | Some thread_id -> event_in_thread ~thread_id event
      in
      let rec previous position =
        if position < 0 then None
        else
          match events.(position).event_id with
          | Some candidate when is_candidate events.(position) -> Some candidate
          | _ -> previous (position - 1)
      in
      match target_position with
      | Some position when Id.User_id.equal own_user events.(position).sender
        -> (
          match previous (position - 1) with
          | Some _ as target -> target
          | None -> if marking then Some event_id else None)
      | _ -> Some event_id)

type prepared_receipt = {
  receipt_type : receipt_type;
  event_id : Id.Event_id.t;
  thread_id : Id.Event_id.t option;
}

let receipt_thread_id ?thread_id receipt_type =
  match receipt_type with
  | Fully_read -> None
  | Read | Read_private -> thread_id

let current_target_for_send t receipt_type thread_id =
  match receipt_type with
  | Read | Fully_read -> latest_target t receipt_type thread_id
  | Read_private -> (
      match
        (latest_target t Read_private thread_id, latest_target t Read thread_id)
      with
      | None, target | target, None -> target
      | Some private_read, Some public_read ->
          Some (newer_cached_event t ?thread_id private_read public_read))

let prepare_receipt t ?thread_id ~marking receipt_type ~event_id =
  let thread_id = receipt_thread_id ?thread_id receipt_type in
  match receipt_type with
  | Fully_read ->
      let current = current_target_for_send t receipt_type thread_id in
      if
        Option.fold ~none:true
          ~some:(fun current ->
            receipt_is_newer t ?thread_id ~current ~candidate:event_id ())
          current
      then Some { receipt_type; event_id; thread_id }
      else None
  | Read | Read_private -> (
      match
        receipt_target_away_from_own_event t ?thread_id ~event_id ~marking ()
      with
      | None -> None
      | Some target_event_id ->
          let current = current_target_for_send t receipt_type thread_id in
          if
            Option.fold ~none:true
              ~some:(fun current ->
                receipt_is_newer t ?thread_id ~current
                  ~candidate:target_event_id ())
              current
          then Some { receipt_type; event_id = target_event_id; thread_id }
          else None)

let remember_receipt t receipt =
  Hashtbl.replace t.local_receipts
    (receipt_key receipt.receipt_type receipt.thread_id)
    receipt.event_id

let send_prepared_receipt t receipt =
  match receipt.receipt_type with
  | Fully_read ->
      let result =
        Matrix_client.Receipts.set_read_marker t.client ~room_id:t.room_id
          ~fully_read:receipt.event_id ()
      in
      (match result with Ok () -> remember_receipt t receipt | Error _ -> ());
      result
  | Read | Read_private ->
      let result =
        Matrix_client.Receipts.send_receipt t.client ~room_id:t.room_id
          ~event_id:receipt.event_id ~receipt_type:receipt.receipt_type
          ?thread_id:receipt.thread_id ()
      in
      (match result with Ok () -> remember_receipt t receipt | Error _ -> ());
      result

let send_single_receipt_internal t ?thread_id ~marking receipt_type ~event_id =
  let thread_id = receipt_thread_id ?thread_id receipt_type in
  match prepare_receipt t ?thread_id receipt_type ~marking ~event_id with
  | Some receipt -> (
      match send_prepared_receipt t receipt with
      | Ok () -> Ok true
      | Error error -> Error error)
  | None -> (
      match receipt_type with
      | (Read | Read_private) when Option.is_some thread_id -> Ok false
      | Read | Read_private | Fully_read -> (
          match clear_marked_unread t with
          | Ok () -> Ok false
          | Error error -> Error error))

let send_single_receipt t ?thread_id receipt_type ~event_id =
  send_single_receipt_internal t ?thread_id ~marking:false receipt_type
    ~event_id

let send_multiple_receipts t receipts =
  match receipts with
  | [] -> clear_marked_unread t
  | receipts -> (
      let prepared =
        List.filter_map
          (fun ({ receipt_type; event_id } : receipt) ->
            prepare_receipt t ~marking:false receipt_type ~event_id)
          receipts
      in
      let fully_read =
        List.find_opt
          (fun receipt -> receipt.receipt_type = Fully_read)
          prepared
      in
      let public_read =
        List.find_opt (fun receipt -> receipt.receipt_type = Read) prepared
      in
      let private_read =
        List.find_opt
          (fun receipt -> receipt.receipt_type = Read_private)
          prepared
      in
      let batched =
        List.filter_map Fun.id [ fully_read; public_read; private_read ]
      in
      let send_one receipt =
        match send_prepared_receipt t receipt with
        | Ok () -> Ok ()
        | Error error -> Error error
      in
      let send_rest result receipt =
        match result with
        | Error _ as error -> error
        | Ok () ->
            let skip receipt =
              List.exists (fun batched -> receipt == batched) batched
            in
            if skip receipt then Ok () else send_one receipt
      in
      let result =
        match batched with
        | [] -> Ok ()
        | _ -> (
            let result =
              Matrix_client.Receipts.set_read_marker t.client ~room_id:t.room_id
                ?fully_read:(Option.map (fun r -> r.event_id) fully_read)
                ?read:(Option.map (fun r -> r.event_id) public_read)
                ?read_private:(Option.map (fun r -> r.event_id) private_read)
                ()
            in
            match result with
            | Ok () ->
                List.iter (remember_receipt t) batched;
                List.fold_left send_rest (Ok ()) prepared
            | Error _ as error -> error)
      in
      match (prepared, result) with
      | [], Ok () -> clear_marked_unread t
      | _, result -> result)

let latest_receipt_event t ?thread_id () =
  cached_presentations t |> Array.to_list |> List.rev
  |> List.find_map (fun (event : Presentation.t) ->
      Option.bind event.event_id (fun event_id ->
          match thread_id with
          | None -> Some event_id
          | Some thread_id ->
              if event_in_thread ~thread_id event then Some event_id else None))

let mark_as_read t ?thread_id receipt_type =
  match latest_receipt_event t ?thread_id () with
  | None -> (
      match clear_marked_unread t with
      | Ok () -> Ok false
      | Error error -> Error error)
  | Some event_id ->
      send_single_receipt_internal t ?thread_id ~marking:true receipt_type
        ~event_id
