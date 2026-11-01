module Store = Matrix_ui.Event_store
module Model = Store.Internal
module Cache = Matrix_ui.Event_cache
module Raw = Matrix_proto.Event.Raw_event
module Id = Matrix_proto.Id
module Back = Matrix_ui.Back_pagination
module Timeline = Matrix_ui.Room_timeline
module Client = Matrix_client.Client
module Send_queue = Matrix_client.Send_queue
module Observable = Matrix_ui.Observable

let room_id = Id.Room_id.of_string_exn "!lazy:example.org"

let raw id body =
  match
    Jsont_bytesrw.decode_string Raw.jsont
      (Printf.sprintf
         {|{"event_id":%S,"sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":%S}}|}
         id body)
  with
  | Ok event -> event
  | Error error -> Alcotest.fail error

let stored id body =
  let event_id = "$" ^ id ^ ":example.org" in
  {
    Model.stable_id = "event:" ^ event_id;
    event = raw event_id body;
    clear_event = None;
    delivery = Model.Synced;
  }

let events_chunk chunk_id ?prev_token ?next_token events =
  Model.Events { chunk_id; prev_token; next_token; events }

let gap gap_id token = Model.Gap { gap_id; token }
let room chunks = { Model.chunks; next_chunk_id = 100; external_events = [] }

module Backend = struct
  type t = {
    rooms : (string, Model.room) Hashtbl.t;
    initial_calls : int ref;
    full_calls : int ref;
    chunk_calls : int list ref;
    apply_calls : Model.change list list ref;
    remove_calls : int ref;
    fail_chunk : int option ref;
    fail_apply : bool ref;
    chunk_override :
      (Model.events_chunk option, Store.Error.t) result option ref;
    initial_override : (Model.initial option, Store.Error.t) result option ref;
  }

  let create room =
    let rooms = Hashtbl.create 4 in
    Hashtbl.replace rooms (Id.Room_id.to_string room_id) room;
    {
      rooms;
      initial_calls = ref 0;
      full_calls = ref 0;
      chunk_calls = ref [];
      apply_calls = ref [];
      remove_calls = ref 0;
      fail_chunk = ref None;
      fail_apply = ref false;
      chunk_override = ref None;
      initial_override = ref None;
    }

  let key room_id = Id.Room_id.to_string room_id
  let find t room_id = Hashtbl.find_opt t.rooms (key room_id)

  let load_room t room_id =
    incr t.full_calls;
    Ok (find t room_id)

  let metadata chunks =
    List.map
      (function
        | Model.Gap gap -> Model.Gap_metadata gap
        | Model.Events chunk ->
            Model.Events_metadata
              {
                chunk_id = chunk.chunk_id;
                prev_token = chunk.prev_token;
                next_token = chunk.next_token;
                stable_ids =
                  List.map (fun event -> event.Model.stable_id) chunk.events;
              })
      chunks

  let newest_events chunks =
    List.rev chunks
    |> List.find_map (function
      | Model.Events chunk -> Some chunk
      | Model.Gap _ -> None)

  let load_room_initial t room_id =
    incr t.initial_calls;
    match !(t.initial_override) with
    | Some result -> result
    | None -> (
        match find t room_id with
        | None -> Ok None
        | Some room ->
            Ok
              (Some
                 (Model.Tail
                    {
                      metadata =
                        {
                          Model.chunks = metadata room.Model.chunks;
                          next_chunk_id = room.next_chunk_id;
                          external_events = room.external_events;
                        };
                      newest = newest_events room.Model.chunks;
                    })))

  let load_events_chunk t room_id chunk_id =
    t.chunk_calls := chunk_id :: !(t.chunk_calls);
    match !(t.chunk_override) with
    | Some result -> result
    | None -> (
        match !(t.fail_chunk) with
        | Some failed when failed = chunk_id ->
            Error
              (Store.Error.Codec
                 (Printf.sprintf "chunk %d is malformed" chunk_id))
        | _ -> (
            match find t room_id with
            | None -> Ok None
            | Some room ->
                List.find_map
                  (function
                    | Model.Events chunk when chunk.chunk_id = chunk_id ->
                        Some (Ok (Some chunk))
                    | Model.Gap _ | Model.Events _ -> None)
                  room.Model.chunks
                |> Option.value ~default:(Ok None)))

  let replace_events room chunk_id events =
    {
      room with
      Model.chunks =
        List.map
          (function
            | Model.Gap gap -> Model.Gap gap
            | Model.Events chunk when chunk.chunk_id = chunk_id ->
                Model.Events { chunk with events }
            | Model.Events chunk -> Model.Events chunk)
          room.Model.chunks;
    }

  let apply_change room = function
    | Model.Replace_events_chunk { chunk_id; events } ->
        replace_events room chunk_id events
    | Model.Layout layout ->
        let old_events =
          List.filter_map
            (function
              | Model.Gap _ -> None
              | Model.Events chunk -> Some (chunk.chunk_id, chunk.events))
            room.chunks
        in
        {
          layout with
          Model.chunks =
            List.map
              (function
                | Model.Gap gap -> Model.Gap gap
                | Model.Events chunk when chunk.events = [] ->
                    Model.Events
                      {
                        chunk with
                        events =
                          Option.value
                            (List.assoc_opt chunk.chunk_id old_events)
                            ~default:[];
                      }
                | Model.Events chunk -> Model.Events chunk)
              layout.Model.chunks;
        }
    | Model.Put_event { chunk_id; position; event } ->
        let room =
          {
            room with
            Model.chunks =
              List.map
                (function
                  | Model.Gap gap -> Model.Gap gap
                  | Model.Events chunk ->
                      Model.Events
                        {
                          chunk with
                          events =
                            List.filter
                              (fun (known : Model.event) ->
                                not
                                  (String.equal known.stable_id event.stable_id))
                              chunk.events;
                        })
                room.Model.chunks;
          }
        in
        replace_events room chunk_id
          (match
             List.find_map
               (function
                 | Model.Events chunk when chunk.chunk_id = chunk_id ->
                     Some chunk.events
                 | Model.Gap _ | Model.Events _ -> None)
               room.Model.chunks
           with
          | None -> [ event ]
          | Some events ->
              let position = Int.min position (List.length events) in
              let before = List.take position events in
              let after = List.drop position events in
              before @ [ event ] @ after)
    | Model.Delete_event { stable_id } ->
        {
          room with
          Model.chunks =
            List.map
              (function
                | Model.Gap gap -> Model.Gap gap
                | Model.Events chunk ->
                    Model.Events
                      {
                        chunk with
                        events =
                          List.filter
                            (fun (event : Model.event) ->
                              not (String.equal event.stable_id stable_id))
                            chunk.events;
                      })
              room.Model.chunks;
        }

  let save_room t room_id room =
    Hashtbl.replace t.rooms (key room_id) room;
    Ok ()

  let apply t room_id changes =
    t.apply_calls := changes :: !(t.apply_calls);
    if !(t.fail_apply) then Error (Store.Error.Backend "injected apply failure")
    else begin
      let current = Option.value (find t room_id) ~default:(room []) in
      Hashtbl.replace t.rooms (key room_id)
        (List.fold_left apply_change current changes);
      Ok ()
    end

  let remove_room t room_id =
    incr t.remove_calls;
    Hashtbl.remove t.rooms (key room_id);
    Ok ()

  let close _ = ()
end

module Lazy_backend = struct
  include Backend

  let load_room_initial = Backend.load_room_initial
  let load_events_chunk = Backend.load_events_chunk
end

let store backend = Store.v_lazy (module Lazy_backend) backend

let client fetch =
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 1024 'x'))
  in
  Client.create
    ~config:
      (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
    ~fetch ~random

let ids snapshot =
  Array.to_list snapshot
  |> List.filter_map (fun (event : Cache.event) ->
      Option.map Id.Event_id.to_string event.event.event_id)

let gap_ids cache =
  Observable.Value.get (Cache.gaps cache room_id)
  |> List.map (fun (gap : Cache.gap) -> (gap.index, gap.token))

let tail_room () =
  let old = stored "old" "old" in
  let tail = stored "tail" "tail" in
  ( room
      [
        events_chunk 1 ~prev_token:"start" ~next_token:"gap-token" [ old ];
        gap 2 "gap-token";
        events_chunk 3 ~prev_token:"tail-prev" [ tail ];
      ],
    old,
    tail )

let contiguous_room () =
  let old = stored "old" "old" in
  let tail = stored "tail" "tail" in
  ( room
      [
        events_chunk 1 ~prev_token:"start" [ old ];
        events_chunk 3 ~prev_token:"old-start" [ tail ];
      ],
    old,
    tail )

let test_cold_load_is_tail_only () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = tail_room () in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  Alcotest.(check (list string))
    "tail only" [ "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  Alcotest.(check int) "one metadata load" 1 !(backend.initial_calls);
  Alcotest.(check int) "no eager full load" 0 !(backend.full_calls);
  Alcotest.(check (list (pair int string)))
    "gap identity and index"
    [ (0, "gap-token") ]
    (gap_ids cache);
  Alcotest.(check bool)
    "older chunk remains unloaded" true
    (Cache.has_unloaded_history cache room_id)

let test_single_chunk_hydration_and_growth () =
  Eio_main.run @@ fun _ ->
  let persisted, old, _ = contiguous_room () in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  match Cache.hydrate_previous cache room_id with
  | Cache.Hydrated hydrated ->
      Alcotest.(check (list string))
        "hydrated identity" [ "$old:example.org" ]
        (List.filter_map
           (fun (event : Cache.event) ->
             Option.map Id.Event_id.to_string event.event.event_id)
           hydrated);
      Alcotest.(check (list int)) "one read" [ 1 ] !(backend.chunk_calls);
      Alcotest.(check (list string))
        "ordered resident history"
        [ "$old:example.org"; "$tail:example.org" ]
        (ids (Cache.snapshot cache room_id));
      Alcotest.(check bool)
        "no unloaded history" false
        (Cache.has_unloaded_history cache room_id)
  | Cache.No_persisted_history -> Alcotest.fail "expected one persisted chunk"
  | Cache.Hydration_failed error ->
      Alcotest.failf "hydration failed: %a" Store.Error.pp error

let test_resident_snapshot_grows_one_chunk_at_a_time () =
  Eio_main.run @@ fun _ ->
  let chunks =
    [
      events_chunk 1 [ stored "old-1" "old-1" ];
      events_chunk 2 [ stored "old-2" "old-2" ];
      events_chunk 4 [ stored "tail" "tail" ];
    ]
  in
  let backend = Backend.create (room chunks) in
  let cache = Cache.create ~store:(store backend) () in
  Alcotest.(check (list string))
    "initial resident tail" [ "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydrated _ -> ()
  | Cache.No_persisted_history -> Alcotest.fail "missing first old chunk"
  | Cache.Hydration_failed error ->
      Alcotest.failf "first hydration: %a" Store.Error.pp error);
  Alcotest.(check (list string))
    "first hydration adds one chunk"
    [ "$old-2:example.org"; "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydrated _ -> ()
  | Cache.No_persisted_history -> Alcotest.fail "missing second old chunk"
  | Cache.Hydration_failed error ->
      Alcotest.failf "second hydration: %a" Store.Error.pp error);
  Alcotest.(check (list string))
    "second hydration adds one chunk"
    [ "$old-1:example.org"; "$old-2:example.org"; "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  Alcotest.(check (list int))
    "one backend read per hydration" [ 1; 2 ] !(backend.chunk_calls)

let test_incremental_mutation_keeps_unloaded_chunks () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = tail_room () in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  ignore (Cache.snapshot cache room_id);
  Cache.prepend cache room_id
    ~events:[ raw "$new:example.org" "new" ]
    ~prev_batch:None;
  Cache.flush_room cache room_id;
  Alcotest.(check bool)
    "mutation uses an incremental apply" true
    (!(backend.apply_calls) <> []);
  let saved = Hashtbl.find backend.rooms (Id.Room_id.to_string room_id) in
  Alcotest.(check bool)
    "unloaded chunk remains persisted" true
    (List.exists
       (function
         | Model.Events chunk -> chunk.chunk_id = 1 | Model.Gap _ -> false)
       saved.Model.chunks);
  Alcotest.(check (list string))
    "unloaded rows remain persisted"
    [ "$old:example.org"; "$new:example.org"; "$tail:example.org" ]
    (Model.room_events saved
    |> List.filter_map (fun (event : Model.event) ->
        Option.map Id.Event_id.to_string event.event.event_id));
  Alcotest.(check bool)
    "old rows are not eagerly decoded" true
    (!(backend.chunk_calls) = [])

let test_back_pagination_hydrates_before_network () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let persisted, old, _ = contiguous_room () in
  let backend = Backend.create persisted in
  let requests = ref 0 in
  let fetch =
    Fetch_mock.client (fun _request ->
        incr requests;
        Alcotest.fail "network pagination happened before local hydration")
  in
  let cache = Cache.create ~store:(store backend) () in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ~max_concurrent:1
      ()
  in
  let handle =
    Back.enqueue scheduler
      {
        Back.room_id;
        priority = Back.Normal;
        batch_size = 2;
        max_batches = Some 1;
        stop = (fun _ ~reached_start:_ -> true);
      }
  in
  let result = Back.await handle in
  Back.cancel handle;
  Alcotest.(check int) "no network request" 0 !requests;
  Alcotest.(check (list string))
    "local page returned" [ "$old:example.org" ]
    (List.filter_map
       (fun (event : Cache.event) ->
         Option.map Id.Event_id.to_string event.event.event_id)
       result.events);
  Alcotest.(check (list int))
    "one local chunk read" [ 1 ] !(backend.chunk_calls);
  ignore old

let test_room_timeline_resolves_gap_then_hydrates () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let persisted, old, _ = tail_room () in
  let backend = Backend.create persisted in
  let requests = ref 0 in
  let encoded =
    Jsont_bytesrw.encode_string Raw.jsont old.event |> Result.get_ok
  in
  let fetch =
    Fetch_mock.client (fun request ->
        incr requests;
        if !requests > 1 then
          Alcotest.fail "timeline requested the network before local history";
        Fetch_mock.respond
          (Printf.sprintf {|{"start":"gap-token","chunk":[%s]}|} encoded)
          request)
  in
  let cache = Cache.create ~store:(store backend) () in
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 256 'q'))
  in
  let send_queue =
    Send_queue.create ~random
      ~user_id:(Id.User_id.of_string_exn "@alice:example.org")
      ()
  in
  let timeline =
    Timeline.create ~sw ~client:(client fetch) ~send_queue cache room_id
  in
  Alcotest.(check bool)
    "closing the visible gap is not the persisted start" true
    (Timeline.paginate_back timeline ~limit:2 () = Ok `More);
  Alcotest.(check int) "one gap request" 1 !requests;
  Alcotest.(check bool)
    "the next page hydrates locally" true
    (Timeline.paginate_back timeline ~limit:2 () = Ok `More);
  Alcotest.(check int) "still one network request" 1 !requests;
  Alcotest.(check (list string))
    "timeline hydration restores the hidden event"
    [ "$old:example.org"; "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  Timeline.close timeline

let test_hidden_duplicate_closes_gap_without_duplicate () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = tail_room () in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  let duplicate = raw "$old:example.org" "arrived from server" in
  match
    Cache.prepend_if_token cache room_id ~expected_prev_batch:"gap-token"
      ~events:[ duplicate ] ~prev_batch:None
  with
  | Cache.Applied applied -> (
      Alcotest.(check int)
        "hidden duplicate was not inserted" 0
        (List.length applied.inserted);
      Alcotest.(check bool)
        "gap closed but hidden history remains" false applied.reached_start;
      match Cache.hydrate_previous cache room_id with
      | Cache.Hydrated _ ->
          Alcotest.(check (list string))
            "hydration preserves order and identity"
            [ "$old:example.org"; "$tail:example.org" ]
            (ids (Cache.snapshot cache room_id));
          Alcotest.(check (list string))
            "no duplicate after overlap"
            [ "$old:example.org"; "$tail:example.org" ]
            (List.sort_uniq String.compare (ids (Cache.snapshot cache room_id)))
      | Cache.No_persisted_history -> Alcotest.fail "hidden chunk was lost"
      | Cache.Hydration_failed error ->
          Alcotest.failf "hydration failed: %a" Store.Error.pp error)
  | Cache.Stale | Cache.Forgotten -> Alcotest.fail "unexpected stale overlap"

let test_mixed_hidden_overlap_closes_gap () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = tail_room () in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  let duplicate = raw "$old:example.org" "arrived from server" in
  let fresh = raw "$middle:example.org" "middle" in
  match
    Cache.prepend_if_token cache room_id ~expected_prev_batch:"gap-token"
      ~events:[ duplicate; fresh ] ~prev_batch:(Some "continue")
  with
  | Cache.Applied applied -> (
      Alcotest.(check (list string))
        "only the fresh event is inserted" [ "$middle:example.org" ]
        (List.filter_map
           (fun (event : Cache.event) ->
             Option.map Id.Event_id.to_string event.event.event_id)
           applied.inserted);
      Alcotest.(check bool)
        "the hidden overlap closes the server gap" false
        (Observable.Value.get (Cache.has_gap cache room_id));
      Alcotest.(check bool)
        "hidden history is not the room start" false applied.reached_start;
      match Cache.hydrate_previous cache room_id with
      | Cache.Hydrated _ ->
          Alcotest.(check (list string))
            "hydrated order crosses the closed gap"
            [ "$old:example.org"; "$middle:example.org"; "$tail:example.org" ]
            (ids (Cache.snapshot cache room_id))
      | Cache.No_persisted_history -> Alcotest.fail "hidden chunk was lost"
      | Cache.Hydration_failed error ->
          Alcotest.failf "hydration failed: %a" Store.Error.pp error)
  | Cache.Stale | Cache.Forgotten -> Alcotest.fail "unexpected stale overlap"

let test_hidden_local_echo_matches_remote_transaction () =
  Eio_main.run @@ fun _ ->
  let transaction = Id.Transaction_id.v "lazy-transaction" in
  let local_wire =
    {
      (raw "$temporary:example.org" "local") with
      event_id = None;
      unsigned =
        Some (Matrix_proto.Event.Unsigned.make ~transaction_id:transaction ());
    }
  in
  let local =
    {
      Model.stable_id = "txn:lazy-transaction";
      event = local_wire;
      clear_event = None;
      delivery = Model.Sending;
    }
  in
  let remote =
    {
      (raw "$remote:example.org" "local") with
      unsigned =
        Some (Matrix_proto.Event.Unsigned.make ~transaction_id:transaction ());
    }
  in
  let tail = stored "tail" "tail" in
  let persisted =
    room
      [ events_chunk 1 [ local ]; gap 2 "gap-token"; events_chunk 3 [ tail ] ]
  in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  match
    Cache.prepend_if_token cache room_id ~expected_prev_batch:"gap-token"
      ~events:[ remote ] ~prev_batch:None
  with
  | Cache.Applied applied -> (
      Alcotest.(check int)
        "remote echo is deduplicated against hidden transaction" 0
        (List.length applied.inserted);
      Alcotest.(check bool)
        "remote echo closes the visible gap" false
        (Observable.Value.get (Cache.has_gap cache room_id));
      match Cache.hydrate_previous cache room_id with
      | Cache.Hydrated _ ->
          Alcotest.(check int)
            "one stable echo remains" 2
            (Array.length (Cache.snapshot cache room_id))
      | Cache.No_persisted_history -> Alcotest.fail "local echo chunk was lost"
      | Cache.Hydration_failed error ->
          Alcotest.failf "local echo hydration failed: %a" Store.Error.pp error)
  | Cache.Stale | Cache.Forgotten -> Alcotest.fail "unexpected stale echo"

let queued_echo () =
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 256 'z'))
  in
  let queue =
    Send_queue.create ~random
      ~user_id:(Id.User_id.of_string_exn "@alice:example.org")
      ()
  in
  let request = Send_queue.send_text queue ~room_id ~body:"still pending" in
  let transaction = Send_queue.txn_id request in
  let echo =
    {
      Model.stable_id = "txn:" ^ transaction;
      event = Send_queue.local_echo queue request;
      clear_event = None;
      delivery = Model.Queued;
    }
  in
  (queue, request, echo)

let test_track_queue_promotes_hidden_echo () =
  Eio_main.run @@ fun _ ->
  let queue, request, echo = queued_echo () in
  let old = stored "old" "old" in
  let tail = stored "tail" "tail" in
  let backend =
    Backend.create
      (room [ events_chunk 1 [ old; echo ]; events_chunk 3 [ tail ] ])
  in
  backend.fail_apply := true;
  let cache = Cache.create ~store:(store backend) () in
  Cache.track_send_queue cache queue;
  let transaction = Send_queue.txn_id request in
  Alcotest.(check (list string))
    "restored echo is moved to the resident tail"
    [ "event:$tail:example.org"; "txn:" ^ transaction ]
    (Cache.snapshot cache room_id
    |> Array.to_list
    |> List.map (fun (event : Cache.event) -> event.stable_id));
  (match Observable.Value.get (Cache.last_error cache) with
  | Some (Store.Error.Backend _) -> ()
  | Some error -> Alcotest.failf "wrong apply error: %a" Store.Error.pp error
  | None -> Alcotest.fail "injected queue-promotion failure was not reported");
  backend.fail_apply := false;
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydrated [ event ] ->
      Alcotest.(check string)
        "only the older event hydrates" "event:$old:example.org" event.stable_id
  | Cache.Hydrated _ -> Alcotest.fail "the hidden echo was hydrated twice"
  | Cache.No_persisted_history -> Alcotest.fail "older event chunk was lost"
  | Cache.Hydration_failed error ->
      Alcotest.failf "queue-promotion retry failed: %a" Store.Error.pp error);
  Alcotest.(check int)
    "the stable echo exists once after retry" 1
    (Cache.snapshot cache room_id
    |> Array.to_list
    |> List.filter (fun (event : Cache.event) ->
        String.equal event.stable_id ("txn:" ^ transaction))
    |> List.length);
  Alcotest.(check bool)
    "retry clears the store error" true
    (Option.is_none (Observable.Value.get (Cache.last_error cache)))

let test_hidden_echo_chunk_compacts_layout () =
  Eio_main.run @@ fun _ ->
  let queue, request, echo = queued_echo () in
  let old = stored "old" "old" in
  let tail = stored "tail" "tail" in
  let backend =
    Backend.create
      (room
         [
           events_chunk 1 [ old ];
           gap 2 "hidden-gap";
           events_chunk 3 [ echo ];
           events_chunk 4 [ tail ];
         ])
  in
  backend.fail_apply := true;
  let store = store backend in
  let cache = Cache.create ~store () in
  Cache.track_send_queue cache queue;
  Alcotest.(check (list (pair int string)))
    "the exposed gap keeps its identity"
    [ (0, "hidden-gap") ]
    (gap_ids cache);
  backend.fail_apply := false;
  Cache.flush_room cache room_id;
  let transaction = Send_queue.txn_id request in
  let saved = Hashtbl.find backend.rooms (Id.Room_id.to_string room_id) in
  Alcotest.(check bool)
    "the empty hidden echo chunk is removed" false
    (List.exists
       (function
         | Model.Events chunk -> chunk.chunk_id = 3 | Model.Gap _ -> false)
       saved.Model.chunks);
  Alcotest.(check int)
    "flush replays the failed stable-id move" 1
    (Model.room_events saved
    |> List.filter (fun (event : Model.event) ->
        String.equal event.stable_id ("txn:" ^ transaction))
    |> List.length);
  let cold = Cache.create ~store () in
  Alcotest.(check (list string))
    "the compacted layout survives restart"
    [ "event:$tail:example.org"; "txn:" ^ transaction ]
    (Cache.snapshot cold room_id
    |> Array.to_list
    |> List.map (fun (event : Cache.event) -> event.stable_id));
  Alcotest.(check (list (pair int string)))
    "the gap survives restart"
    [ (0, "hidden-gap") ]
    (gap_ids cold)

let test_invalid_initial_metadata_is_rejected () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = contiguous_room () in
  let backend = Backend.create persisted in
  let metadata : Model.room_metadata =
    {
      Model.chunks = Backend.metadata persisted.Model.chunks;
      next_chunk_id = persisted.next_chunk_id;
      external_events = [];
    }
  in
  backend.initial_override :=
    Some (Ok (Some (Model.Tail { metadata; newest = None })));
  let cache = Cache.create ~store:(store backend) () in
  Alcotest.(check (list string))
    "invalid tail is not partially exposed" []
    (ids (Cache.snapshot cache room_id));
  match Observable.Value.get (Cache.last_error cache) with
  | Some (Store.Error.Codec _) -> ()
  | Some error -> Alcotest.failf "wrong initial error: %a" Store.Error.pp error
  | None -> Alcotest.fail "invalid initial metadata was silently accepted"

let test_hydration_promotes_detached_plaintext () =
  Eio_main.run @@ fun _ ->
  let persisted, old, _ = contiguous_room () in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  let fetched =
    {
      old.event with
      unsigned = Some (Matrix_proto.Event.Unsigned.make ~age:5L ());
      room_id = Some room_id;
    }
  in
  Cache.register_external_event cache room_id ~event:fetched;
  let plaintext = raw "$old:example.org" "decrypted old" in
  Alcotest.(check bool)
    "detached decryption recorded" true
    (Cache.set_decrypted cache room_id ~encrypted:fetched ~plaintext);
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydrated (event :: _) ->
      Alcotest.(check bool)
        "detached plaintext follows the physical event" true
        (Option.equal ( = ) event.clear_event (Some plaintext))
  | Cache.Hydrated [] -> Alcotest.fail "hydrated chunk was empty"
  | Cache.No_persisted_history -> Alcotest.fail "hidden chunk was lost"
  | Cache.Hydration_failed error ->
      Alcotest.failf "hydration failed: %a" Store.Error.pp error);
  let saved = Hashtbl.find backend.rooms (Id.Room_id.to_string room_id) in
  Alcotest.(check int)
    "promoted detached row is removed" 0
    (List.length saved.external_events)

let test_failed_promotion_is_retryable () =
  Eio_main.run @@ fun _ ->
  let persisted, old, tail = contiguous_room () in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  Cache.register_external_event cache room_id ~event:old.event;
  backend.fail_apply := true;
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydration_failed (Store.Error.Backend _) -> ()
  | Cache.Hydration_failed error ->
      Alcotest.failf "wrong promotion failure: %a" Store.Error.pp error
  | Cache.Hydrated _ -> Alcotest.fail "failed promotion mutated the cache"
  | Cache.No_persisted_history -> Alcotest.fail "hidden chunk was lost");
  Alcotest.(check (list string))
    "failed promotion leaves the resident topology untouched"
    [ "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  Alcotest.(check bool)
    "failed promotion remains retryable" true
    (Cache.has_unloaded_history cache room_id);
  backend.fail_apply := false;
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydrated _ -> ()
  | Cache.Hydration_failed error ->
      Alcotest.failf "promotion retry failed: %a" Store.Error.pp error
  | Cache.No_persisted_history -> Alcotest.fail "promotion was not retried");
  Alcotest.(check (list string))
    "retry hydrates exactly once"
    [ "$old:example.org"; "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  Alcotest.(check bool)
    "retry clears the store error" true
    (Option.is_none (Observable.Value.get (Cache.last_error cache)))

let test_malformed_chunk_preserves_topology () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = contiguous_room () in
  let backend = Backend.create persisted in
  backend.fail_chunk := Some 1;
  let cache = Cache.create ~store:(store backend) () in
  let before = ids (Cache.snapshot cache room_id) in
  let gaps_before = gap_ids cache in
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydration_failed (Store.Error.Codec _) -> ()
  | Cache.Hydration_failed error ->
      Alcotest.failf "wrong failure: %a" Store.Error.pp error
  | Cache.Hydrated _ -> Alcotest.fail "malformed chunk hydrated"
  | Cache.No_persisted_history -> Alcotest.fail "missing malformed chunk");
  Alcotest.(check (list string))
    "resident topology retained" before
    (ids (Cache.snapshot cache room_id));
  Alcotest.(check (list (pair int string)))
    "gap topology retained" gaps_before (gap_ids cache)

let test_missing_and_mismatched_chunks_are_retryable () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = contiguous_room () in
  let backend = Backend.create persisted in
  let cache = Cache.create ~store:(store backend) () in
  backend.chunk_override := Some (Ok None);
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydration_failed (Store.Error.Codec _) -> ()
  | Cache.Hydration_failed error ->
      Alcotest.failf "wrong missing-chunk failure: %a" Store.Error.pp error
  | Cache.Hydrated _ | Cache.No_persisted_history ->
      Alcotest.fail "missing chunk was accepted");
  let old_chunk =
    List.find_map
      (function
        | Model.Events chunk when chunk.chunk_id = 1 -> Some chunk
        | Model.Events _ | Model.Gap _ -> None)
      persisted.Model.chunks
    |> Option.get
  in
  backend.chunk_override :=
    Some (Ok (Some { old_chunk with prev_token = Some "wrong-token" }));
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydration_failed (Store.Error.Codec _) -> ()
  | Cache.Hydration_failed error ->
      Alcotest.failf "wrong mismatch failure: %a" Store.Error.pp error
  | Cache.Hydrated _ | Cache.No_persisted_history ->
      Alcotest.fail "mismatched chunk was accepted");
  Alcotest.(check (list string))
    "failed reads do not change the tail" [ "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  backend.chunk_override := None;
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydrated _ -> ()
  | Cache.Hydration_failed error ->
      Alcotest.failf "valid retry failed: %a" Store.Error.pp error
  | Cache.No_persisted_history -> Alcotest.fail "valid retry was lost");
  Alcotest.(check (list string))
    "valid retry hydrates once"
    [ "$old:example.org"; "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id))

let test_forget_removes_lazy_layout () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = tail_room () in
  let backend = Backend.create persisted in
  let store = store backend in
  let cache = Cache.create ~store () in
  ignore (Cache.snapshot cache room_id);
  Cache.forget_room cache room_id;
  Alcotest.(check int) "backend removal" 1 !(backend.remove_calls);
  let cold = Cache.create ~store () in
  Alcotest.(check (list string))
    "forgotten layout gone" []
    (ids (Cache.snapshot cold room_id));
  Alcotest.(check bool)
    "no full load after forget" true
    (!(backend.full_calls) = 0)

let test_memory_lazy_plaintext_policy () =
  Eio_main.run @@ fun _ ->
  let old =
    {
      (stored "old" "encrypted old") with
      clear_event = Some (raw "$old:example.org" "clear old");
    }
  in
  let tail =
    {
      (stored "tail" "encrypted tail") with
      clear_event = Some (raw "$tail:example.org" "clear tail");
    }
  in
  let persisted = room [ events_chunk 1 [ old ]; events_chunk 2 [ tail ] ] in
  let check_policy name store expected =
    Alcotest.(check unit)
      (name ^ " seed") ()
      (Result.get_ok (Store.save_room store room_id persisted));
    let cache = Cache.create ~store () in
    let tail = (Cache.snapshot cache room_id).(0) in
    Alcotest.(check bool)
      (name ^ " applies to the cold tail")
      expected
      (Option.is_some tail.clear_event);
    match Cache.hydrate_previous cache room_id with
    | Cache.Hydrated [ old ] ->
        Alcotest.(check bool)
          (name ^ " applies to an older lazy chunk")
          expected
          (Option.is_some old.clear_event)
    | Cache.Hydrated _ -> Alcotest.fail "unexpected hydrated memory chunk"
    | Cache.No_persisted_history -> Alcotest.fail "memory chunk was not lazy"
    | Cache.Hydration_failed error ->
        Alcotest.failf "memory hydration failed: %a" Store.Error.pp error
  in
  check_policy "ciphertext-only" (Store.memory ()) false;
  check_policy "plaintext"
    (Store.memory ~plaintext_policy:Store_plaintext ())
    true

let with_temp_store f =
  let path = Filename.temp_file "matrix-ui-lazy-" ".sqlite3" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let file = path ^ suffix in
          if Sys.file_exists file then Sys.remove file)
        [ ""; "-wal"; "-shm" ])
    (fun () -> f path)

let test_backends_reject_invalid_lazy_writes () =
  Eio_main.run @@ fun _ ->
  let persisted, _, _ = contiguous_room () in
  let check name store =
    Alcotest.(check unit)
      (name ^ " seed") ()
      (Result.get_ok (Store.save_room store room_id persisted));
    let invalid_layout =
      Model.Layout
        {
          persisted with
          chunks =
            persisted.chunks @ [ Model.Gap { gap_id = 90; token = "bad" } ];
        }
    in
    (match Store.apply store room_id [ invalid_layout ] with
    | Error (Store.Error.Backend _) -> ()
    | Error error ->
        Alcotest.failf "%s malformed layout: %a" name Store.Error.pp error
    | Ok () -> Alcotest.fail (name ^ " accepted a trailing gap"));
    (match
       Store.apply store room_id
         [ Model.Replace_events_chunk { chunk_id = 1; events = [] } ]
     with
    | Error (Store.Error.Backend _) -> ()
    | Error error ->
        Alcotest.failf "%s empty replacement: %a" name Store.Error.pp error
    | Ok () -> Alcotest.fail (name ^ " accepted an empty events chunk"));
    let loaded = Store.load_room store room_id |> Result.get_ok |> Option.get in
    Alcotest.(check int)
      (name ^ " failures are atomic")
      2
      (List.length (Model.room_events loaded));
    Store.close store
  in
  check "memory" (Store.memory ());
  with_temp_store @@ fun path ->
  let sqlite = Matrix_ui_sqlite.create path |> Result.get_ok in
  check "sqlite" sqlite

let test_sqlite_hydration_restart_keeps_topology () =
  Eio_main.run @@ fun _ ->
  with_temp_store @@ fun path ->
  let old = stored "old" "old" in
  let near = stored "near" "near" in
  let tail = stored "tail" "tail" in
  let persisted =
    room
      [
        events_chunk 1 [ old ];
        gap 2 "middle-gap";
        events_chunk 3 [ near ];
        events_chunk 4 [ tail ];
      ]
  in
  let open_store () = Matrix_ui_sqlite.create path |> Result.get_ok in
  let first_store = open_store () in
  Alcotest.(check unit)
    "seed SQLite topology" ()
    (Result.get_ok (Store.save_room first_store room_id persisted));
  let first_cache = Cache.create ~store:first_store () in
  (match Cache.hydrate_previous first_cache room_id with
  | Cache.Hydrated [ event ] ->
      Alcotest.(check string)
        "nearest predecessor hydrates" "event:$near:example.org" event.stable_id
  | Cache.Hydrated _ -> Alcotest.fail "unexpected first SQLite hydration"
  | Cache.No_persisted_history -> Alcotest.fail "SQLite predecessor was lost"
  | Cache.Hydration_failed error ->
      Alcotest.failf "first SQLite hydration failed: %a" Store.Error.pp error);
  Alcotest.(check (list (pair int string)))
    "hydration exposes the preceding gap"
    [ (0, "middle-gap") ]
    (gap_ids first_cache);
  Cache.flush_room first_cache room_id;
  Store.close first_store;
  let reopened_store = open_store () in
  let reopened = Cache.create ~store:reopened_store () in
  Alcotest.(check (list string))
    "restart is lazy again" [ "$tail:example.org" ]
    (ids (Cache.snapshot reopened room_id));
  (match Cache.hydrate_previous reopened room_id with
  | Cache.Hydrated [ event ] ->
      Alcotest.(check string)
        "restart hydrates the same predecessor" "event:$near:example.org"
        event.stable_id
  | Cache.Hydrated _ -> Alcotest.fail "unexpected restarted SQLite hydration"
  | Cache.No_persisted_history ->
      Alcotest.fail "restarted SQLite predecessor was lost"
  | Cache.Hydration_failed error ->
      Alcotest.failf "restarted SQLite hydration failed: %a" Store.Error.pp
        error);
  Alcotest.(check (list string))
    "restart retains event order"
    [ "$near:example.org"; "$tail:example.org" ]
    (ids (Cache.snapshot reopened room_id));
  Alcotest.(check (list (pair int string)))
    "restart retains gap identity"
    [ (0, "middle-gap") ]
    (gap_ids reopened);
  Store.close reopened_store

let test_sqlite_old_malformed_row_is_deferred () =
  Eio_main.run @@ fun _ ->
  with_temp_store @@ fun path ->
  let persisted, _, _ = contiguous_room () in
  let store =
    match Matrix_ui_sqlite.create path with
    | Ok store -> store
    | Error error ->
        Alcotest.failf "create SQLite store: %a" Store.Error.pp error
  in
  Alcotest.(check unit)
    "seed SQLite" ()
    (Result.get_ok (Store.save_room store room_id persisted));
  Store.close store;
  let db = Sqlite3.db_open path in
  Sqlite3.Rc.check
    (Sqlite3.exec db
       "UPDATE ui_events SET event_json='{malformed}' WHERE \
        room_id='!lazy:example.org' AND chunk_id=1");
  ignore (Sqlite3.db_close db);
  let store =
    match Matrix_ui_sqlite.create path with
    | Ok store -> store
    | Error error ->
        Alcotest.failf "reopen SQLite store: %a" Store.Error.pp error
  in
  let cache = Cache.create ~store () in
  Alcotest.(check (list string))
    "cold tail survives malformed old row" [ "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  (match Cache.hydrate_previous cache room_id with
  | Cache.Hydration_failed (Store.Error.Codec _) -> ()
  | Cache.Hydration_failed error ->
      Alcotest.failf "wrong SQLite error: %a" Store.Error.pp error
  | Cache.Hydrated _ ->
      Alcotest.fail "malformed old row decoded during cold load"
  | Cache.No_persisted_history -> Alcotest.fail "old row was lost from layout");
  Alcotest.(check (list string))
    "tail remains after deferred failure" [ "$tail:example.org" ]
    (ids (Cache.snapshot cache room_id));
  Store.close store

let () =
  Alcotest.run "lazy event store"
    [
      ( "lazy storage",
        [
          Alcotest.test_case "cold load is tail only" `Quick
            test_cold_load_is_tail_only;
          Alcotest.test_case "single chunk hydration and growth" `Quick
            test_single_chunk_hydration_and_growth;
          Alcotest.test_case "resident snapshot grows one chunk at a time"
            `Quick test_resident_snapshot_grows_one_chunk_at_a_time;
          Alcotest.test_case "incremental mutation keeps unloaded chunks" `Quick
            test_incremental_mutation_keeps_unloaded_chunks;
          Alcotest.test_case "back pagination hydrates before network" `Quick
            test_back_pagination_hydrates_before_network;
          Alcotest.test_case "timeline resolves gap then hydrates" `Quick
            test_room_timeline_resolves_gap_then_hydrates;
          Alcotest.test_case "hidden duplicate closes gap" `Quick
            test_hidden_duplicate_closes_gap_without_duplicate;
          Alcotest.test_case "mixed hidden overlap closes gap" `Quick
            test_mixed_hidden_overlap_closes_gap;
          Alcotest.test_case "hidden local echo matches remote transaction"
            `Quick test_hidden_local_echo_matches_remote_transaction;
          Alcotest.test_case "queue tracking promotes a hidden echo" `Quick
            test_track_queue_promotes_hidden_echo;
          Alcotest.test_case "hidden echo chunk compacts its layout" `Quick
            test_hidden_echo_chunk_compacts_layout;
          Alcotest.test_case "invalid initial metadata is rejected" `Quick
            test_invalid_initial_metadata_is_rejected;
          Alcotest.test_case "hydration promotes detached plaintext" `Quick
            test_hydration_promotes_detached_plaintext;
          Alcotest.test_case "failed promotion is retryable" `Quick
            test_failed_promotion_is_retryable;
          Alcotest.test_case "malformed chunk preserves topology" `Quick
            test_malformed_chunk_preserves_topology;
          Alcotest.test_case "missing and mismatched chunks retry" `Quick
            test_missing_and_mismatched_chunks_are_retryable;
          Alcotest.test_case "forget removes lazy layout" `Quick
            test_forget_removes_lazy_layout;
          Alcotest.test_case "memory lazy plaintext policy" `Quick
            test_memory_lazy_plaintext_policy;
          Alcotest.test_case "backends reject invalid lazy writes" `Quick
            test_backends_reject_invalid_lazy_writes;
          Alcotest.test_case "SQLite hydration survives restart" `Quick
            test_sqlite_hydration_restart_keeps_topology;
          Alcotest.test_case "SQLite defers malformed old row" `Quick
            test_sqlite_old_malformed_row_is_deferred;
        ] );
    ]
