module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

type t = {
  client : Matrix_client.Client.t;
  event_cache : Event_cache.t;
  room_id : Id.Room_id.t;
  max_events_to_load : int;
  events : Event.Raw_event.t Observable.List.t;
  mutex : Eio.Mutex.t;
  mutable loaded : (string * Event.Raw_event.t) list;
  mutable closed : bool;
  mutable generation : int;
  mutable forget_subscription : (unit -> unit) option;
}

let close t =
  let unsubscribe =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed then None
        else (
          t.closed <- true;
          t.generation <- t.generation + 1;
          t.loaded <- [];
          Observable.List.reconcile_by
            ~key:(fun _ -> ())
            ~equal:( = ) t.events [];
          let unsubscribe = t.forget_subscription in
          t.forget_subscription <- None;
          unsubscribe))
  in
  Option.iter (fun unsubscribe -> unsubscribe ()) unsubscribe

let create ?(max_events_to_load = 128) ~client ~event_cache ~room_id () =
  if max_events_to_load < 0 then
    invalid_arg "Matrix_ui.Pinned_events.create: max_events_to_load";
  let t =
    {
      client;
      event_cache;
      room_id;
      events = Observable.List.create [];
      mutex = Eio.Mutex.create ();
      loaded = [];
      closed = false;
      generation = 0;
      max_events_to_load;
      forget_subscription = None;
    }
  in
  let unsubscribe =
    Event_cache.subscribe_forget_room event_cache room_id (fun () -> close t)
  in
  let retain_subscription =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed then false
        else (
          t.forget_subscription <- Some unsubscribe;
          true))
  in
  (* [forget_room] may have run after registering the callback but before the
     returned unsubscribe function could be installed in [t]. *)
  if not retain_subscription then unsubscribe ();
  t

let events t = t.events
let snapshot t = Observable.List.snapshot t.events

let pinned_ids ~max_events_to_load state room_id =
  match
    Matrix_client.Base_client.find_state_event state room_id
      ~event_type:Event.Event_type.Room_pinned_events ()
  with
  | None -> []
  | Some state_event -> (
      match
        Jsont.Json.decode Matrix_proto.Event.Room_pinned_events_content.jsont
          state_event.content
      with
      | Error _ -> []
      | Ok content ->
          let seen = Hashtbl.create 16 in
          let ids =
            List.filter_map
              (fun encoded ->
                match Id.Event_id.of_string encoded with
                | Error _ -> None
                | Ok id ->
                    let key = Id.Event_id.to_string id in
                    if Hashtbl.mem seen key then None
                    else (
                      Hashtbl.add seen key ();
                      Some (key, id)))
              (Matrix_proto.Event.Room_pinned_events_content.pinned content)
          in
          if List.length ids <= max_events_to_load then ids
          else
            ids |> List.rev
            |> List.filteri (fun index _ -> index < max_events_to_load)
            |> List.rev)

let lookup_loaded loaded key =
  Option.map snd
    (List.find_opt (fun (known, _) -> String.equal known key) loaded)

let generation_is_current t generation =
  Eio.Mutex.use_ro t.mutex (fun () ->
      (not t.closed) && t.generation = generation)

let fetch t ~generation key event_id loaded =
  match Event_cache.find_event t.event_cache t.room_id event_id with
  | Some event -> Ok (event, loaded)
  | None -> (
      match lookup_loaded loaded key with
      | Some event -> Ok (event, loaded)
      | None -> (
          match
            Matrix_client.Messages.get_event t.client ~room_id:t.room_id
              ~event_id
          with
          | Error _ as error -> error
          | Ok event -> (
              match event.event_id with
              | Some actual when String.equal (Id.Event_id.to_string actual) key
                -> (
                  match event.room_id with
                  | Some actual_room
                    when not (Id.Room_id.equal actual_room t.room_id) ->
                      Error
                        (Matrix_client.Error.Json_error
                           "GET /event returned an event for a different room")
                  | _ ->
                      if generation_is_current t generation then
                        Event_cache.register_external_event t.event_cache
                          t.room_id ~event;
                      Ok (event, (key, event) :: loaded))
              | Some _ | None ->
                  Error
                    (Matrix_client.Error.Json_error
                       "GET /event returned a different or missing event id"))))

(* Relation enrichment is deliberately bounded per pinned root. The endpoint
   is optional on older homeservers, so an unavailable enrichment does not
   make the target itself unusable. Tokens are remembered to avoid a broken
   server looping forever. *)
let is_pinned_relation event_id (event : Event.Raw_event.t) =
  match Matrix_proto.Json.find_mem "m.relates_to" event.content with
  | None -> false
  | Some json -> (
      match Jsont.Json.decode Event.Relates_to.jsont json with
      | Error _ -> false
      | Ok relation -> (
          Id.Event_id.equal relation.event_id event_id
          &&
          match relation.rel_type with
          | Event.Rel_type.Annotation | Event.Rel_type.Replace -> true
          | Event.Rel_type.Custom _ | Event.Rel_type.Reference
          | Event.Rel_type.Thread ->
              false))

let fetch_relation_pages t ~generation event_id ~limit =
  let rec page seen token remaining attempts =
    if
      remaining <= 0 || attempts >= 64
      || not (generation_is_current t generation)
    then Ok ()
    else if Option.exists (fun token -> Hashtbl.mem seen token) token then Ok ()
    else begin
      Option.iter (fun token -> Hashtbl.replace seen token ()) token;
      match
        Matrix_client.Relations.get_raw_relations t.client ~room_id:t.room_id
          ~event_id ~limit:(min 256 remaining) ?from:token
          ~dir:Matrix_proto.Common.Direction.Backward ~recurse:true ()
      with
      | Error _ -> Ok ()
      | Ok result -> (
          let fetched =
            if List.length result.chunk > remaining then
              List.take remaining result.chunk
            else result.chunk
          in
          if generation_is_current t generation then
            List.iter
              (fun event ->
                if is_pinned_relation event_id event then
                  Event_cache.register_external_event t.event_cache t.room_id
                    ~event)
              fetched;
          match (generation_is_current t generation, result.next_batch) with
          | false, _ | true, None -> Ok ()
          | true, Some _ ->
              page seen result.next_batch
                (remaining - List.length fetched)
                (attempts + 1))
    end
  in
  page (Hashtbl.create 8) None limit 0

let enrich_relations t ~generation event_id =
  (* There is no persisted relation-completeness marker in the current store
     model. Refresh conservatively; this may repeat GETs, but cannot silently
     freeze a partial relation set after a cold reload. As in matrix-rust-sdk,
     request recursive unfiltered pages and retain only annotations and
     replacements locally. *)
  fetch_relation_pages t ~generation event_id ~limit:256

let refresh t ~state =
  (* Do not hold the view lock while making network requests. [generation]
     makes close, reorder, and a newer refresh invalidate late completions. *)
  let generation, ids, loaded =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if t.closed then (None, [], [])
        else begin
          t.generation <- t.generation + 1;
          let generation = t.generation in
          let ids =
            pinned_ids ~max_events_to_load:t.max_events_to_load state t.room_id
          in
          let loaded =
            List.filter
              (fun (key, _) ->
                List.exists (fun (wanted, _) -> String.equal key wanted) ids)
              t.loaded
          in
          (Some generation, ids, loaded)
        end)
  in
  match generation with
  | None -> Ok ()
  | Some generation -> (
      let rec resolve acc loaded = function
        | [] -> Ok (List.rev acc, loaded)
        | (key, event_id) :: rest -> (
            match fetch t ~generation key event_id loaded with
            | Error _ as error ->
                Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                    if (not t.closed) && t.generation = generation then
                      t.loaded <- loaded);
                error
            | Ok (event, loaded) -> (
                match enrich_relations t ~generation event_id with
                | Error _ as error -> error
                | Ok () -> resolve (event :: acc) loaded rest))
      in
      match resolve [] loaded ids with
      | Error _ as error -> error
      | Ok (resolved, _loaded) ->
          Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
              if t.closed || t.generation <> generation then Ok ()
              else begin
                let loaded =
                  List.filter_map
                    (fun event ->
                      Option.map
                        (fun id -> (Id.Event_id.to_string id, event))
                        event.Event.Raw_event.event_id)
                    resolved
                in
                t.loaded <- loaded;
                Observable.List.reconcile_by
                  ~key:(fun event ->
                    Id.Event_id.to_string
                      (Option.get event.Event.Raw_event.event_id))
                  ~equal:( = ) t.events resolved;
                Ok ()
              end))
