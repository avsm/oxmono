module Client = Matrix_client.Client
module Paginator = Matrix_client.Paginator
module Id = Matrix_proto.Id

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 '\x42')
  end

let uid value = Result.get_ok (Id.User_id.of_string value)
let did value = Result.get_ok (Id.Device_id.of_string value)
let rid value = Result.get_ok (Id.Room_id.of_string value)
let eid value = Result.get_ok (Id.Event_id.of_string value)

let session : Client.session =
  {
    user_id = uid "@alice:example.org";
    device_id = did "DEVICE";
    access_token = "token";
    refresh_token = None;
  }

let client fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)
  |> fun client -> Client.with_session client session

let raw id timestamp =
  Printf.sprintf
    {|{"event_id":"%s","sender":"@bob:example.org","origin_server_ts":%d,"type":"m.room.message","content":{"msgtype":"m.text","body":"%s"}}|}
    id timestamp id

let ids events =
  List.map
    (fun (event : Matrix_proto.Event.Raw_event.t) ->
      Id.Event_id.to_string (Option.get event.event_id))
    events

let check_ids label expected events =
  Alcotest.(check (list string)) label expected (ids events)

let url request = Fetch.Middleware.Url.to_string request.Fetch.Middleware.url
let respond body request = Fetch_mock.respond body request

let test_start_orders_context_and_tracks_edges () =
  Eio_mock.Backend.run @@ fun () ->
  let requests = ref [] in
  let fetch =
    Fetch_mock.client (fun request ->
        requests := url request :: !requests;
        respond
          (Printf.sprintf
             {|{"event":%s,"events_before":[%s,%s],"events_after":[%s,%s],"start":"older","end":null,"state":[]}|}
             (raw "$target" 3) (raw "$before2" 2) (raw "$before1" 1)
             (raw "$after1" 4) (raw "$after2" 5))
          request)
  in
  let paginator =
    Paginator.create ~client:(client fetch) ~room_id:(rid "!room:example.org")
      ()
  in
  let observed = ref [] in
  let unsubscribe =
    Paginator.subscribe paginator (fun state -> observed := state :: !observed)
  in
  let result =
    Result.get_ok
      (Paginator.start_from paginator ~event_id:(eid "$target") ~limit:4 ())
  in
  check_ids "chronological context"
    [ "$before1"; "$before2"; "$target"; "$after1"; "$after2" ]
    result.events;
  Alcotest.(check bool) "has previous" true result.has_previous;
  Alcotest.(check bool) "no next" false result.has_next;
  Alcotest.(check bool)
    "tokens" true
    (Paginator.tokens paginator
    = {
        Paginator.previous = Paginator.Has_more "older";
        next = Paginator.Hit_end;
      });
  Alcotest.(check bool)
    "state transitions" true
    (List.rev !observed
    = [ Paginator.Initial; Paginator.Fetching_target; Paginator.Idle ]);
  unsubscribe ();
  Result.get_ok (Paginator.reset paginator);
  Alcotest.(check int) "unsubscribed" 3 (List.length !observed);
  Alcotest.(check string)
    "context URL"
    "https://hs.example/_matrix/client/v3/rooms/!room:example.org/context/$target?limit=4"
    (List.hd (List.rev !requests))

let test_directional_pages_exhaust_without_another_request () =
  Eio_mock.Backend.run @@ fun () ->
  let count = ref 0 in
  let urls = ref [] in
  let fetch =
    Fetch_mock.client (fun request ->
        incr count;
        urls := !urls @ [ url request ];
        let body =
          match !count with
          | 1 ->
              Printf.sprintf
                {|{"event":%s,"events_before":[],"events_after":[],"start":"p1","end":"n1","state":[]}|}
                (raw "$target" 3)
          | 2 ->
              Printf.sprintf {|{"chunk":[%s,%s],"start":"p1","state":[]}|}
                (raw "$target" 3) (raw "$old1" 1)
          | 3 ->
              Printf.sprintf
                {|{"chunk":[%s],"start":"n1","end":"n2","state":[]}|}
                (raw "$new1" 4)
          | 4 ->
              Printf.sprintf {|{"chunk":[%s],"start":"n2","state":[]}|}
                (raw "$new2" 5)
          | _ -> Alcotest.fail "paginator requested past an exhausted edge"
        in
        respond body request)
  in
  let paginator =
    Paginator.create ~client:(client fetch) ~room_id:(rid "!room:example.org")
      ()
  in
  ignore
    (Result.get_ok
       (Paginator.start_from paginator ~event_id:(eid "$target") ()));
  let backward =
    Result.get_ok (Paginator.paginate_backward paginator ~limit:2 ())
  in
  check_ids "backward overlap is deduplicated" [ "$old1" ] backward.events;
  Alcotest.(check bool) "backward hit start" true backward.hit_end;
  let again = Result.get_ok (Paginator.paginate_backward paginator ()) in
  Alcotest.(check (list string)) "no repeated page" [] (ids again.events);
  Alcotest.(check bool) "known start" true again.hit_end;
  let forward =
    Result.get_ok (Paginator.paginate_forward paginator ~limit:1 ())
  in
  check_ids "forward page" [ "$new1" ] forward.events;
  Alcotest.(check bool) "more forward" false forward.hit_end;
  let final =
    Result.get_ok (Paginator.paginate_forward paginator ~limit:1 ())
  in
  check_ids "last forward page" [ "$new2" ] final.events;
  Alcotest.(check bool) "forward hit end" true final.hit_end;
  ignore (Result.get_ok (Paginator.paginate_forward paginator ()));
  Alcotest.(check int) "exhausted edges make no requests" 4 !count;
  Alcotest.(check (list string))
    "direction and tokens"
    [
      "https://hs.example/_matrix/client/v3/rooms/!room:example.org/context/$target?limit=10";
      "https://hs.example/_matrix/client/v3/rooms/!room:example.org/messages?dir=b&from=p1&limit=2";
      "https://hs.example/_matrix/client/v3/rooms/!room:example.org/messages?dir=f&from=n1&limit=1";
      "https://hs.example/_matrix/client/v3/rooms/!room:example.org/messages?dir=f&from=n2&limit=1";
    ]
    !urls

let test_state_reentrancy_and_failure_rollback () =
  Eio_mock.Backend.run @@ fun () ->
  let paginator_ref = ref None in
  let reentrant_rejected = ref false in
  let attempts = ref 0 in
  let fetch =
    Fetch_mock.client (fun request ->
        incr attempts;
        (match !paginator_ref with
        | Some paginator when !attempts = 1 -> (
            match
              Paginator.start_from paginator ~event_id:(eid "$other") ()
            with
            | Error
                (Paginator.Invalid_state
                   { expected = Paginator.Initial; actual = Fetching_target })
              ->
                reentrant_rejected := true
            | _ -> Alcotest.fail "reentrant start was not rejected")
        | _ -> ());
        if !attempts = 1 then
          respond
            (Printf.sprintf
               {|{"event":%s,"events_before":[],"events_after":[],"start":"p","end":"n","state":[]}|}
               (raw "$target" 1))
            request
        else Fetch_mock.respond ~status:502 "not json" request)
  in
  let paginator =
    Paginator.create ~client:(client fetch) ~room_id:(rid "!room:example.org")
      ()
  in
  paginator_ref := Some paginator;
  ignore
    (Result.get_ok
       (Paginator.start_from paginator ~event_id:(eid "$target") ()));
  Alcotest.(check bool) "reentrant call rejected" true !reentrant_rejected;
  (match Paginator.paginate_backward paginator () with
  | Error (Paginator.Client_error _) -> ()
  | _ -> Alcotest.fail "expected the HTTP failure");
  Alcotest.(check bool)
    "failure restores idle" true
    (Paginator.state paginator = Paginator.Idle);
  Alcotest.(check bool)
    "failed request retains token" true
    ((Paginator.tokens paginator).previous = Paginator.Has_more "p")

let test_not_found_restores_initial () =
  Eio_mock.Backend.run @@ fun () ->
  let fetch =
    Fetch_mock.client (fun request ->
        Fetch_mock.respond ~status:404
          {|{"errcode":"M_NOT_FOUND","error":"missing"}|} request)
  in
  let paginator =
    Paginator.create ~client:(client fetch) ~room_id:(rid "!room:example.org")
      ()
  in
  (match Paginator.start_from paginator ~event_id:(eid "$missing") () with
  | Error (Paginator.Event_not_found event_id) ->
      Alcotest.(check string)
        "event id" "$missing"
        (Id.Event_id.to_string event_id)
  | _ -> Alcotest.fail "expected Event_not_found");
  Alcotest.(check bool)
    "restored initial" true
    (Paginator.state paginator = Paginator.Initial)

let () =
  Alcotest.run "matrix.client paginator"
    [
      ( "room",
        [
          Alcotest.test_case "context ordering and nullable edges" `Quick
            test_start_orders_context_and_tracks_edges;
          Alcotest.test_case "directional token exhaustion" `Quick
            test_directional_pages_exhaust_without_another_request;
          Alcotest.test_case "reentrancy and failure rollback" `Quick
            test_state_reentrancy_and_failure_rollback;
          Alcotest.test_case "not found restores initial" `Quick
            test_not_found_restores_initial;
        ] );
    ]
