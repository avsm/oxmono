module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Cache = Matrix_ui.Event_cache
module Back = Matrix_ui.Back_pagination
module Observable = Matrix_ui.Observable
module Client = Matrix_client.Client

let room_id = Id.Room_id.of_string_exn "!room:example.org"
let other_room_id = Id.Room_id.of_string_exn "!other:example.org"
let third_room_id = Id.Room_id.of_string_exn "!third:example.org"
let high_room_id = Id.Room_id.of_string_exn "!high:example.org"

let raw json =
  match Jsont_bytesrw.decode_string Event.Raw_event.jsont json with
  | Ok event -> event
  | Error error -> Alcotest.fail error

let event ?txn id body =
  let unsigned =
    match txn with
    | None -> ""
    | Some txn -> Printf.sprintf {|,"unsigned":{"transaction_id":%S}|} txn
  in
  raw
    (Printf.sprintf
       {|{"event_id":%S,"sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":%S}%s}|}
       id body unsigned)

let client fetch =
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 1024 'x'))
  in
  Client.create
    ~config:
      (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
    ~fetch ~random

let seed cache event =
  Cache.prepend cache room_id ~events:[ event ] ~prev_batch:(Some "p0")

let page ?end_ events =
  Printf.sprintf {|{"chunk":[%s]%s,"start":"p0"}|} (String.concat "," events)
    (match end_ with
    | None -> ""
    | Some token -> Printf.sprintf {|,"end":%S|} token)

let with_scheduler ?(max_concurrent = 1) handler f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let fetch = Fetch_mock.client handler in
  let cache = Cache.create () in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ~max_concurrent ()
  in
  f cache scheduler

let request ?(priority = Back.Normal) ?max_batches
    ?(stop = fun _ ~reached_start:_ -> false) ?(batch_size = 2) () =
  { Back.room_id; priority; batch_size; max_batches; stop }

let request_for room_id ?(priority = Back.Normal) ?max_batches
    ?(stop = fun _ ~reached_start:_ -> false) ?(batch_size = 2) () =
  { Back.room_id; priority; batch_size; max_batches; stop }

let test_exact_insertions_and_url () =
  let first = event ~txn:"txn-1" "$first:example.org" "first" in
  let older = event "$older:example.org" "older" in
  let calls = ref [] in
  with_scheduler
    (fun request ->
      calls := Fetch.Middleware.Url.to_string request.url :: !calls;
      Fetch_mock.respond
        (page
           [
             {|{"event_id":"$older:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":"older"}}|};
           ])
        request)
    (fun cache scheduler ->
      seed cache first;
      let handle =
        Back.enqueue scheduler
          (request ~stop:(fun _ ~reached_start:_ -> true) ())
      in
      let result = Back.await handle in
      Back.cancel handle;
      Back.cancel handle;
      Alcotest.(check bool)
        "stop predicate precedes terminal reason" true
        (match result.reason with Back.Stop_condition -> true | _ -> false);
      Alcotest.(check int) "one inserted event" 1 (List.length result.events);
      Alcotest.(check int) "one batch" 1 result.batches;
      Alcotest.(check (list string))
        "inserted identity" [ "$older:example.org" ]
        (List.filter_map
           (fun (event : Cache.event) ->
             Option.map Id.Event_id.to_string event.event.event_id)
           result.events);
      Alcotest.(check (list string))
        "messages URL"
        [
          "https://hs.example/_matrix/client/v3/rooms/!room:example.org/messages?dir=b&from=p0&limit=2";
        ]
        (List.rev !calls);
      ignore older)

let test_transaction_overlap_is_exact () =
  Eio_mock.Backend.run @@ fun () ->
  let cache = Cache.create () in
  let first = event ~txn:"txn-1" "$first:example.org" "first" in
  let duplicate = event ~txn:"txn-1" "$server:example.org" "same" in
  seed cache first;
  match
    Cache.prepend_if_token cache room_id ~expected_prev_batch:"p0"
      ~events:[ duplicate; event "$older:example.org" "older" ]
      ~prev_batch:(Some "p1")
  with
  | Cache.Applied applied -> (
      Alcotest.(check int)
        "transaction duplicate omitted" 1
        (List.length applied.inserted);
      Alcotest.(check bool)
        "partial overlap retains the next token" false applied.reached_start;
      match
        Cache.prepend_if_token cache room_id ~expected_prev_batch:"p1"
          ~events:[ duplicate ] ~prev_batch:(Some "p2")
      with
      | Cache.Applied duplicate_page ->
          Alcotest.(check int)
            "all-duplicate page inserts nothing" 0
            (List.length duplicate_page.inserted);
          Alcotest.(check bool)
            "all-duplicate page closes the obsolete gap" true
            duplicate_page.reached_start;
          Alcotest.(check (option string))
            "all-duplicate page clears the token" None
            (Observable.Value.get (Cache.prev_batch cache room_id))
      | Cache.Stale | Cache.Forgotten ->
          Alcotest.fail "unexpected all-duplicate cache outcome")
  | Cache.Stale | Cache.Forgotten -> Alcotest.fail "unexpected cache outcome"

let test_priority_fifo_and_coalescing () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cache = Cache.create () in
  seed cache (event "$a-seed:example.org" "a");
  Cache.prepend cache other_room_id
    ~events:[ event "$b-seed:example.org" "b" ]
    ~prev_batch:(Some "p0");
  Cache.prepend cache third_room_id
    ~events:[ event "$c-seed:example.org" "c" ]
    ~prev_batch:(Some "p0");
  Cache.prepend cache high_room_id
    ~events:[ event "$d-seed:example.org" "d" ]
    ~prev_batch:(Some "p0");
  let started, signal_started = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let calls = ref [] in
  let blocked = ref false in
  let fetch =
    Fetch_mock.client (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        let room =
          if
            String.ends_with
              ~suffix:"!other:example.org/messages?dir=b&from=p0&limit=2" url
          then other_room_id
          else if
            String.ends_with
              ~suffix:"!third:example.org/messages?dir=b&from=p0&limit=2" url
          then third_room_id
          else if
            String.ends_with
              ~suffix:"!high:example.org/messages?dir=b&from=p0&limit=2" url
          then high_room_id
          else room_id
        in
        calls := Id.Room_id.to_string room :: !calls;
        if room = room_id && not !blocked then begin
          blocked := true;
          Eio.Promise.resolve signal_started ();
          Eio.Promise.await release
        end;
        Fetch_mock.respond (page []) request)
  in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ~max_concurrent:1
      ()
  in
  let first = Back.enqueue scheduler (request_for room_id ()) in
  Eio.Promise.await started;
  let low =
    Back.enqueue scheduler (request_for other_room_id ~priority:Back.Low ())
  in
  let low_fifo =
    Back.enqueue scheduler (request_for third_room_id ~priority:Back.Low ())
  in
  let high =
    Back.enqueue scheduler (request_for high_room_id ~priority:Back.High ())
  in
  (* A second normal request for the occupied room shares the first run. *)
  let shared = Back.enqueue scheduler (request_for room_id ()) in
  Eio.Promise.resolve signal_release ();
  let first_result = Back.await first in
  let shared_result = Back.await shared in
  let high_result = Back.await high in
  let low_result = Back.await low in
  let low_fifo_result = Back.await low_fifo in
  Alcotest.(check int)
    "coalesced room requests use one page" 1 first_result.batches;
  Alcotest.(check int)
    "coalesced result is shared" first_result.batches shared_result.batches;
  Alcotest.(check (list string))
    "high priority queued room runs first"
    [
      Id.Room_id.to_string room_id;
      Id.Room_id.to_string high_room_id;
      Id.Room_id.to_string other_room_id;
      Id.Room_id.to_string third_room_id;
    ]
    (List.rev !calls);
  Alcotest.(check int) "high run fetched" 1 high_result.batches;
  Alcotest.(check int) "first same-priority run fetched" 1 low_result.batches;
  Alcotest.(check int)
    "same-priority FIFO run fetched" 1 low_fifo_result.batches

let test_max_concurrent_and_same_room_exclusion () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cache = Cache.create () in
  seed cache (event "$a-seed:example.org" "a");
  Cache.prepend cache other_room_id
    ~events:[ event "$b-seed:example.org" "b" ]
    ~prev_batch:(Some "p0");
  let started, signal_started = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let starts = ref 0 in
  let fetch =
    Fetch_mock.client (fun request ->
        incr starts;
        if !starts = 2 then Eio.Promise.resolve signal_started ();
        Eio.Promise.await release;
        Fetch_mock.respond (page []) request)
  in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ~max_concurrent:2
      ()
  in
  let a = Back.enqueue scheduler (request_for room_id ()) in
  let b = Back.enqueue scheduler (request_for other_room_id ()) in
  Eio.Promise.await started;
  Alcotest.(check int) "different rooms overlap" 2 !starts;
  Eio.Promise.resolve signal_release ();
  ignore (Back.await a);
  ignore (Back.await b);
  let same_started, signal_same_started = Eio.Promise.create () in
  let same_release, signal_same_release = Eio.Promise.create () in
  let same_calls = ref 0 in
  let same_fetch =
    Fetch_mock.client (fun request ->
        incr same_calls;
        if !same_calls = 1 then Eio.Promise.resolve signal_same_started ();
        Eio.Promise.await same_release;
        if !same_calls = 1 then Fetch_mock.respond (page ~end_:"p1" []) request
        else Fetch_mock.respond (page []) request)
  in
  let same_scheduler =
    Back.create ~sw ~client:(client same_fetch) ~event_cache:cache
      ~max_concurrent:2 ()
  in
  (* The previous terminal pages made both rooms start-reached; seed a fresh
     room so this phase has a token to fetch. *)
  let third = Id.Room_id.of_string_exn "!same:example.org" in
  Cache.prepend cache third
    ~events:[ event "$same-seed:example.org" "same" ]
    ~prev_batch:(Some "p0");
  let normal = Back.enqueue same_scheduler (request_for third ()) in
  Eio.Promise.await same_started;
  let high =
    Back.enqueue same_scheduler (request_for third ~priority:Back.High ())
  in
  Eio.Fiber.yield ();
  Alcotest.(check int) "same room priorities do not overlap" 1 !same_calls;
  Eio.Promise.resolve signal_same_release ();
  ignore (Back.await normal);
  ignore (Back.await high);
  Alcotest.(check int) "same room second priority runs afterward" 2 !same_calls

let test_stop_and_batch_limit () =
  let calls = ref 0 in
  with_scheduler
    (fun request ->
      incr calls;
      let token = Printf.sprintf "p%d" !calls in
      Fetch_mock.respond
        (page ~end_:token
           [
             Printf.sprintf
               {|{"event_id":"$page%d:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":"page"}}|}
               !calls;
           ])
        request)
    (fun cache scheduler ->
      seed cache (event "$seed:example.org" "seed");
      let stop _ ~reached_start:_ = false in
      let result =
        Back.await
          (Back.enqueue scheduler
             (request ~max_batches:1 ~stop ~batch_size:1 ()))
      in
      Alcotest.(check bool)
        "batch limit" true
        (match result.reason with Back.Batch_limit -> true | _ -> false);
      Alcotest.(check int) "one request" 1 !calls)

let test_cancel_one_coalesced_then_forget () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cache = Cache.create () in
  seed cache (event "$seed:example.org" "seed");
  let started, signal_started = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let fetch =
    Fetch_mock.client (fun request ->
        Eio.Promise.resolve signal_started ();
        Eio.Promise.await release;
        Fetch_mock.respond (page []) request)
  in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ()
  in
  let active = Back.enqueue scheduler (request ()) in
  Eio.Promise.await started;
  let cancelled = Back.enqueue scheduler (request ()) in
  Back.cancel cancelled;
  Cache.forget_room cache room_id;
  Alcotest.(check bool)
    "cancelled coalesced waiter" true
    (match (Back.await cancelled).reason with
    | Back.Cancelled -> true
    | _ -> false);
  Eio.Promise.resolve signal_release ();
  Alcotest.(check bool)
    "active coalesced waiter is forgotten" true
    (match (Back.await active).reason with
    | Back.Forgotten -> true
    | _ -> false)

let test_accumulated_events_are_oldest_first () =
  let calls = ref 0 in
  with_scheduler
    (fun request ->
      incr calls;
      if !calls = 1 then
        Fetch_mock.respond
          (page ~end_:"p1"
             [
               {|{"event_id":"$middle:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":"middle"}}|};
             ])
          request
      else
        Fetch_mock.respond
          (page
             [
               {|{"event_id":"$oldest:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":"oldest"}}|};
             ])
          request)
    (fun cache scheduler ->
      seed cache (event "$seed:example.org" "seed");
      let result = Back.await (Back.enqueue scheduler (request ())) in
      let ids =
        List.filter_map
          (fun (event : Cache.event) ->
            Option.map Id.Event_id.to_string event.event.event_id)
          result.events
      in
      Alcotest.(check (list string))
        "oldest first across pages"
        [ "$oldest:example.org"; "$middle:example.org" ]
        ids;
      Alcotest.(check int) "two pages" 2 result.batches)

let test_reached_start_and_no_data () =
  let calls = ref 0 in
  with_scheduler
    (fun request ->
      incr calls;
      if !calls = 1 then Fetch_mock.respond (page ~end_:"p1" []) request
      else Fetch_mock.respond (page []) request)
    (fun cache scheduler ->
      seed cache (event "$seed:example.org" "seed");
      let result = Back.await (Back.enqueue scheduler (request ())) in
      Alcotest.(check bool)
        "no data" true
        (match result.reason with Back.No_data -> true | _ -> false);
      let second = Back.await (Back.enqueue scheduler (request ())) in
      Alcotest.(check bool)
        "reached start after empty terminal page" true
        (match second.reason with Back.Reached_start -> true | _ -> false);
      Alcotest.(check int) "terminal second request" 2 !calls)

let test_stale_and_forgotten () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cache = Cache.create () in
  let first = event "$first:example.org" "first" in
  seed cache first;
  let stale =
    Cache.prepend_if_token cache room_id ~expected_prev_batch:"wrong"
      ~events:[ event "$late:example.org" "late" ]
      ~prev_batch:(Some "p1")
  in
  Alcotest.(check bool)
    "stale page discarded" true
    (match stale with Cache.Stale -> true | _ -> false);
  Cache.forget_room cache room_id;
  Alcotest.(check bool)
    "tombstone visible" true
    (Cache.is_forgotten cache room_id);
  let fetch =
    Fetch_mock.client (fun request ->
        Fetch_mock.respond (page ~end_:"p1" []) request)
  in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ()
  in
  let result = Back.await (Back.enqueue scheduler (request ())) in
  Alcotest.(check bool)
    "forgotten result" true
    (match result.reason with Back.Forgotten -> true | _ -> false)

let test_close_completes_queued_work () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cache = Cache.create () in
  seed cache (event "$seed:example.org" "seed");
  Cache.prepend cache other_room_id
    ~events:[ event "$other-seed:example.org" "seed" ]
    ~prev_batch:(Some "p0");
  let started, signal_started = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let first = ref true in
  let fetch =
    Fetch_mock.client (fun request ->
        if !first then begin
          first := false;
          Eio.Promise.resolve signal_started ()
        end;
        Eio.Promise.await release;
        Fetch_mock.respond (page []) request)
  in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ~max_concurrent:1
      ()
  in
  let running = Back.enqueue scheduler (request ()) in
  Eio.Promise.await started;
  let queued =
    Back.enqueue scheduler (request_for other_room_id ~priority:Back.Low ())
  in
  Back.close scheduler;
  let queued_result = Back.await queued in
  Alcotest.(check bool)
    "queued work is closed" true
    (match queued_result.reason with Back.Closed -> true | _ -> false);
  Eio.Promise.resolve signal_release ();
  let running_result = Back.await running in
  Alcotest.(check bool)
    "running work is closed" true
    (match running_result.reason with Back.Closed -> true | _ -> false);
  Alcotest.(check int)
    "late close response did not insert" 1
    (Array.length (Cache.snapshot cache room_id))

let test_cancel_releases_occupancy () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cache = Cache.create () in
  seed cache (event "$seed:example.org" "seed");
  let started, signal_started = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let calls = ref 0 in
  let fetch =
    Fetch_mock.client (fun request ->
        incr calls;
        if !calls = 1 then begin
          Eio.Promise.resolve signal_started ();
          Eio.Promise.await release
        end;
        Fetch_mock.respond (page []) request)
  in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ()
  in
  let cancelled = Back.enqueue scheduler (request ()) in
  Eio.Promise.await started;
  Back.cancel cancelled;
  let cancelled_result = Back.await cancelled in
  Alcotest.(check bool)
    "cancelled waiter" true
    (match cancelled_result.reason with Back.Cancelled -> true | _ -> false);
  Eio.Promise.resolve signal_release ();
  Eio.Fiber.yield ();
  let retry = Back.await (Back.enqueue scheduler (request ())) in
  Alcotest.(check bool)
    "occupancy released for retry" true
    (match retry.reason with Back.Reached_start -> true | _ -> false);
  Alcotest.(check int) "retry fetched" 2 !calls

let test_failure_retains_token_for_retry () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cache = Cache.create () in
  seed cache (event "$seed:example.org" "seed");
  let calls = ref 0 in
  let fetch =
    Fetch_mock.client (fun request ->
        incr calls;
        if !calls = 1 then Fetch_mock.respond ~status:503 "{}" request
        else Fetch_mock.respond (page []) request)
  in
  let scheduler =
    Back.create ~sw ~client:(client fetch) ~event_cache:cache ()
  in
  let failed = Back.await (Back.enqueue scheduler (request ())) in
  Alcotest.(check bool)
    "network failure is reported" true
    (match failed.reason with Back.Failed _ -> true | _ -> false);
  Alcotest.(check (option string))
    "failed page retains cache token" (Some "p0")
    (Observable.Value.get (Cache.prev_batch cache room_id));
  let retried = Back.await (Back.enqueue scheduler (request ())) in
  Alcotest.(check bool)
    "retry reaches empty start" true
    (match retried.reason with Back.Reached_start -> true | _ -> false);
  Alcotest.(check int) "retry made a second request" 2 !calls

let test_validation () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let scheduler =
    Back.create ~sw
      ~client:
        (client
           (Fetch_mock.client (fun request ->
                Fetch_mock.respond (page []) request)))
      ~event_cache:(Cache.create ()) ()
  in
  let raises f =
    try
      ignore (f ());
      false
    with Invalid_argument _ -> true
  in
  Alcotest.(check bool)
    "zero batch rejected" true
    (raises (fun () -> Back.enqueue scheduler (request ~batch_size:0 ())));
  Alcotest.(check bool)
    "zero max rejected" true
    (raises (fun () -> Back.enqueue scheduler (request ~max_batches:0 ())));
  Alcotest.(check bool)
    "zero workers rejected" true
    (try
       ignore
         (Back.create ~sw
            ~client:
              (client
                 (Fetch_mock.client (fun request ->
                      Fetch_mock.respond (page []) request)))
            ~event_cache:(Cache.create ()) ~max_concurrent:0 ());
       false
     with Invalid_argument _ -> true)

let () =
  Alcotest.run "back-pagination"
    [
      ( "scheduler",
        [
          Alcotest.test_case "exact insertion and URL" `Quick
            test_exact_insertions_and_url;
          Alcotest.test_case "transaction overlap" `Quick
            test_transaction_overlap_is_exact;
          Alcotest.test_case "priority and coalescing" `Quick
            test_priority_fifo_and_coalescing;
          Alcotest.test_case "concurrency and room exclusion" `Quick
            test_max_concurrent_and_same_room_exclusion;
          Alcotest.test_case "stop and batch limit" `Quick
            test_stop_and_batch_limit;
          Alcotest.test_case "cancel coalesced then forget" `Quick
            test_cancel_one_coalesced_then_forget;
          Alcotest.test_case "oldest-first accumulation" `Quick
            test_accumulated_events_are_oldest_first;
          Alcotest.test_case "reached start and no data" `Quick
            test_reached_start_and_no_data;
          Alcotest.test_case "stale and forgotten" `Quick
            test_stale_and_forgotten;
          Alcotest.test_case "close queued work" `Quick
            test_close_completes_queued_work;
          Alcotest.test_case "cancel releases occupancy" `Quick
            test_cancel_releases_occupancy;
          Alcotest.test_case "failure retains token" `Quick
            test_failure_retains_token_for_retry;
          Alcotest.test_case "validation" `Quick test_validation;
        ] );
    ]
