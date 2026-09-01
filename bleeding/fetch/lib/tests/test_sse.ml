open Fetch

let checks = ref 0

let check name condition =
  incr checks;
  if not condition then failwith ("test_sse: " ^ name)

module Chunks = struct
  type t = {
    mutable chunks : string list;
    reads : int ref;
  }

  let read_methods = []

  let single_read t dst =
    match t.chunks with
    | [] -> raise End_of_file
    | chunk :: rest ->
        incr t.reads;
        t.chunks <- rest;
        Cstruct.blit_from_string chunk 0 dst 0 (String.length chunk);
        String.length chunk
end

let chunk_source reads chunks =
  Eio.Resource.T
    (Chunks.{ chunks; reads }, Eio.Flow.Pi.source (module Chunks))

let stream_response reads chunks (req : Middleware.request) =
  let headers = Http.Header.of_list [ "Content-Type", Sse.media_type ] in
  Middleware.Pi.response ~close:(fun () -> ()) ~status:200 ~headers ~version:`HTTP_1_1
    ~body:(chunk_source reads chunks) ~url:req.url ()

let next = function
  | Seq.Cons (event, rest) -> event, rest
  | Seq.Nil -> failwith "test_sse: expected event"

let event ?(name = "message") ?id ?retry data =
  Sse.{ name; data; id; retry }

let decode_string ~sw ?max_event body =
  let reads = ref 0 in
  let client = Fetch_mock.client (stream_response reads [ body ]) in
  Fetch.get ~sw client "https://events.example/vector"
  |> Sse.decode ?max_event |> List.of_seq

let test_framing () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let reads = ref 0 in
  let chunks =
    [ "data: one\r\r";
      "event: state\ndata: two\ndata: more\nid: 7\nretry: 25\n\n";
      "data: dropped" ]
  in
  let client = Fetch_mock.client (stream_response reads chunks) in
  let response = Fetch.get ~sw client "https://events.example/stream" in
  let events = Sse.decode response in
  let first, events = next (events ()) in
  check "lone CR dispatches without another read" (!reads = 1);
  check "default event name" (first.name = "message");
  check "first data" (first.data = "one");
  let second, events = next (events ()) in
  check "named event" (second.name = "state");
  check "joined data" (second.data = "two\nmore");
  check "event id" (second.id = Some "7");
  check "retry" (second.retry = Some 25);
  check "partial final block is dropped"
    (match events () with Seq.Nil -> true | Seq.Cons _ -> false)

let response body content_type req =
  Fetch_mock.respond
    ~headers:(Http.Header.of_list [ "Content-Type", content_type ])
    body req

let test_fields_and_limits () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let body =
    ": comment\r\nevent:\rdata:\nid: bad\000id\nretry: +1\n\n"
  in
  let client = Fetch_mock.client (response body Sse.media_type) in
  let event, rest =
    Fetch.get ~sw client "https://events.example/fields"
    |> Sse.decode |> fun events -> next (events ())
  in
  check "empty event name defaults" (event.name = "message");
  check "empty data dispatches" (event.data = "");
  check "NUL id ignored" (event.id = None);
  check "non-decimal retry ignored" (event.retry = None);
  check "single event"
    (match rest () with Seq.Nil -> true | Seq.Cons _ -> false);
  let oversized =
    Fetch_mock.client (response "data: forever\ndata: still" Sse.media_type)
  in
  let events =
    Fetch.get ~sw oversized "https://events.example/large"
    |> Sse.decode ~max_event:12
  in
  check "whole block is bounded"
    (match events () with
    | exception
        Eio.Io
          (E (Decode_failure { error = Media.Too_large 12; _ }), _) ->
        true
    | _ -> false);
  let wrong = Fetch_mock.client (response "data: x\n\n" "text/plain") in
  let wrong = Fetch.get ~sw wrong "https://events.example/wrong" in
  check "content type checked eagerly"
    (match (Sse.decode wrong) () with
    | exception
        Eio.Io
          (E
             (Decode_failure
                { error = Media.Unsupported (Some "text/plain"); _ }),
           _) ->
        true
    | _ -> false)

let test_framing_vectors () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  check "multiline data"
    (decode_string ~sw
       "event: state\ndata: {\ndata:   \"a\": 1\ndata: }\n\n"
     = [ event ~name:"state" "{\n  \"a\": 1\n}" ]);
  check "empty data field keeps newline"
    (decode_string ~sw "data: a\ndata\ndata: b\n\n"
     = [ event "a\n\nb" ]);
  check "id-only block updates following event"
    (decode_string ~sw
       "event: discarded\nid: 7\n\nevent: ping\ndata: 1\n\n"
     = [ event ~name:"ping" ~id:"7" "1" ]);
  check "field delimiter removes only one space"
    (decode_string ~sw "data:x\n\ndata:  x\n\n"
     = [ event "x"; event " x" ]);
  check "comments and unknown fields are ignored"
    (decode_string ~sw
       ": keep-alive\nevent: ping\n: another\nid: 9\nnosuch: 1\ndata: {}\n\n"
     = [ event ~name:"ping" ~id:"9" "{}" ]);
  check "CRLF framing"
    (decode_string ~sw "id: 1\r\nevent: state\r\ndata: {}\r\n\r\n"
     = [ event ~name:"state" ~id:"1" "{}" ]);
  check "lone CR framing"
    (decode_string ~sw "data: a\rdata: b\r\r" = [ event "a\nb" ]);
  check "mixed framing"
    (decode_string ~sw "data: a\r\n\ndata: b\n\r\n"
     = [ event "a"; event "b" ]);
  check "overflowing retry is ignored"
    (decode_string ~sw
       "retry: 999999999999999999999999999999999999\ndata: x\n\n"
     = [ event "x" ]);
  check "partial final event is dropped"
    (decode_string ~sw "data: a\n\ndata: b\n" = [ event "a" ]);
  (* WHATWG "parsing an event stream": one leading U+FEFF is removed and
     everything after it is data, a second BOM included. *)
  let bom = "\xef\xbb\xbf" in
  check "leading BOM leaves the first event intact"
    (decode_string ~sw (bom ^ "data: a\n\ndata: b\n\n")
     = [ event "a"; event "b" ]);
  check "BOM before a field name is removed, not the field"
    (decode_string ~sw (bom ^ "event: ping\ndata: a\n\n")
     = [ event ~name:"ping" "a" ]);
  check "only the first BOM is removed, so the second hides the field"
    (decode_string ~sw (bom ^ bom ^ "data: a\n\n") = []);
  check "a BOM inside a value is data"
    (decode_string ~sw (bom ^ "data: " ^ bom ^ "a\n\n")
     = [ event (bom ^ "a") ]);
  check "a BOM on a later line is data"
    (decode_string ~sw ("data: a\n\n" ^ bom ^ "data: b\n\n")
     = [ event "a" ]);
  check "a BOM alone dispatches nothing"
    (decode_string ~sw (bom ^ "\n\n") = []);
  (* A BOM holds neither CR nor LF, so it cannot straddle a line however
     the transport splits it. *)
  let decode_chunks chunks =
    let reads = ref 0 in
    let client = Fetch_mock.client (stream_response reads chunks) in
    Fetch.get ~sw client "https://events.example/split"
    |> Sse.decode |> List.of_seq
  in
  check "a BOM split across reads is still removed"
    (decode_chunks [ "\xef"; "\xbb"; "\xbf"; "data: a\n\n" ]
     = [ event "a" ]);
  let many_lines =
    String.concat "" (List.init 20 (fun _ -> "unknown: x\n")) ^ "\n"
  in
  check "many short fields share one bound"
    (match decode_string ~sw ~max_event:32 many_lines with
    | exception
        Eio.Io
          (E (Decode_failure { error = Media.Too_large 32; _ }), _) ->
        true
    | _ -> false)

let test_connect () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let server (req : Middleware.request) =
    check "connect sends Accept"
      (Http.Header.get req.headers "accept" = Some Sse.media_type);
    check "connect sends last id"
      (Http.Header.get req.headers "last-event-id" = Some "41");
    Fetch_mock.Sse.respond (fun sink -> Fetch_mock.Sse.send sink "ok") req
  in
  match
    Sse.connect ~sw ~last_event_id:"41" (Fetch_mock.client server)
      "https://events.example/connect"
  with
  | Error _ -> failwith "test_sse: connect rejected 200"
  | Ok events ->
      let event, _ = next (events ()) in
      check "connect decodes" (event.data = "ok")

let test_control_id_reconnect () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let reconnect_id = ref (Some "not-called") in
  let server (req : Middleware.request) =
    incr calls;
    match !calls with
    | 1 -> response "id: bad\001id\ndata: one\n\n" Sse.media_type req
    | _ ->
        reconnect_id := Http.Header.get req.headers "last-event-id";
        Fetch_mock.respond ~status:401 "stop" req
  in
  let subscription =
    Sse.subscribe ~sw ~clock:env#mono_clock ~backoff_initial:0.1
      ~backoff_max:0.1 ~capacity:2 (Fetch_mock.client server)
      "https://events.example/control-id"
  in
  check "a control-bearing event id is retained"
    (match Eio.Stream.take (Sse.events subscription) with
    | `Event event -> event.data = "one" && event.id = Some "bad\001id"
    | `End -> false);
  check "a control-bearing id does not stop reconnection"
    (Eio.Stream.take (Sse.events subscription) = `End && !calls = 2);
  check "a control-bearing id is omitted from Last-Event-ID"
    (!reconnect_id = None);
  check "the retained id stays observable"
    (Sse.last_event_id subscription = Some "bad\001id")

let test_subscription () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let ids = ref [] in
  let server (req : Middleware.request) =
    incr calls;
    ids := Http.Header.get req.headers "last-event-id" :: !ids;
    match !calls with
    | 1 ->
        response "id: silent\nretry: 0\n\ndata: one\n\n"
          Sse.media_type req
    | 2 ->
        Fetch_mock.Sse.respond
          (fun sink -> Fetch_mock.Sse.send sink ~id:"2" "two") req
    | _ -> Fetch_mock.respond ~status:401 "denied" req
  in
  let subscription =
    Sse.subscribe ~sw ~clock:env#mono_clock
      ~last_event_id:"seed" ~backoff_initial:1. ~backoff_max:1.
      ~capacity:4 (Fetch_mock.client server)
      "https://events.example/subscription"
  in
  let one = Eio.Stream.take (Sse.events subscription) in
  let two = Eio.Stream.take (Sse.events subscription) in
  let ending = Eio.Stream.take (Sse.events subscription) in
  check "first subscription event"
    (match one with
    | `Event event -> event.data = "one" && event.id = Some "silent"
    | `End -> false);
  check "second subscription event"
    (match two with `Event event -> event.data = "two" | `End -> false);
  check "subscription ends" (ending = `End);
  check "last id retained" (Sse.last_event_id subscription = Some "2");
  check "ids quoted on reconnect"
    (List.rev !ids = [ Some "seed"; Some "silent"; Some "2" ]);
  check "fatal status reported"
    (match Eio.Promise.await (Sse.result subscription) with
    | Error (Rejected response) -> status response = 401
    | _ -> false)

let test_connection_retry () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let server req =
    incr calls;
    match !calls with
    | 1 -> raise (Fetch.err (Connection_failure Eio.Net.Timeout))
    | 2 ->
        Fetch_mock.Sse.respond
          (fun sink -> Fetch_mock.Sse.send sink "reconnected") req
    | _ -> Fetch_mock.respond ~status:401 "stop" req
  in
  let subscription =
    Sse.subscribe ~sw ~clock:env#mono_clock ~backoff_initial:1.
      ~backoff_max:1. ~capacity:2 (Fetch_mock.client server)
      "https://events.example/retry"
  in
  check "connection failure retried"
    (match Eio.Stream.take (Sse.events subscription) with
    | `Event event -> event.data = "reconnected"
    | `End -> false);
  check "retry ends after fatal response"
    (Eio.Stream.take (Sse.events subscription) = `End);
  check "connection was retried" (!calls = 3)

let test_close () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server req = Fetch_mock.respond ~status:503 "retry" req in
  let subscription =
    Sse.subscribe ~sw ~clock:env#mono_clock
      ~backoff_initial:100. ~backoff_max:100. ~capacity:1
      (Fetch_mock.client server) "https://events.example/close"
  in
  Sse.close subscription;
  check "close ends stream" (Eio.Stream.take (Sse.events subscription) = `End);
  check "close resolves successfully"
    (Eio.Promise.await (Sse.result subscription) = Ok ())

let event_response actions (req : Middleware.request) =
  let flow = Eio_mock.Flow.make "event-stream" in
  Eio_mock.Flow.on_read flow actions;
  Middleware.Pi.response ~close:(fun () -> ()) ~status:200
    ~headers:(Http.Header.of_list [ "Content-Type", Sse.media_type ])
    ~version:`HTTP_1_1
    ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t) ~url:req.url ()

let held = `Run (fun () -> Eio.Fiber.await_cancel ())

let test_backpressure () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server =
    event_response
      [ `Return "id: 1\ndata: one\n\n";
        `Return "id: 2\ndata: two\n\n";
        `Return "id: 3\ndata: three\n\n";
        held ]
  in
  let subscription =
    Sse.subscribe ~sw ~clock:env#mono_clock ~capacity:1
      (Fetch_mock.client server) "https://events.example/backpressure"
  in
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done;
  check "bounded stream fills one slot"
    (Eio.Stream.length (Sse.events subscription) = 1);
  check "producer blocks after parsing the next event"
    (Sse.last_event_id subscription = Some "2");
  let take_data () =
    match Eio.Stream.take (Sse.events subscription) with
    | `Event event -> event.data
    | `End -> "End"
  in
  check "backpressured events stay ordered"
    (List.init 3 (fun _ -> take_data ()) = [ "one"; "two"; "three" ]);
  Sse.close subscription;
  check "backpressured subscription closes"
    (Eio.Stream.take (Sse.events subscription) = `End)

let test_switch_cancellation () =
  Eio_mock.Backend.run_full @@ fun env ->
  let subscription =
    Eio.Switch.run @@ fun sw ->
    let server =
      event_response
        [ `Return "data: one\n\n"; `Return "data: two\n\n"; held ]
    in
    let subscription =
      Sse.subscribe ~sw ~clock:env#mono_clock ~capacity:1
        (Fetch_mock.client server) "https://events.example/cancel"
    in
    for _ = 1 to 10 do
      Eio.Fiber.yield ()
    done;
    check "daemon is parked on a full stream"
      (Eio.Stream.length (Sse.events subscription) = 1);
    subscription
  in
  check "switch cancellation resolves result"
    (Eio.Promise.peek (Sse.result subscription) = Some (Ok ()))

(* S5: a consumer that stops draining a full stream must not be able to
   pin the daemon, and so the switch that owns it, forever. Under the mock
   backend a regression shows up as a detected deadlock rather than a
   hang. *)
let test_close_on_a_full_stream () =
  Eio_mock.Backend.run_full @@ fun env ->
  let subscription = ref None in
  (Eio.Switch.run @@ fun sw ->
   let server =
     event_response
       [ `Return "data: one\n\n"; `Return "data: two\n\n"; held ]
   in
   let sub =
     Sse.subscribe ~sw ~clock:env#mono_clock ~capacity:1
       (Fetch_mock.client server) "https://events.example/wedge"
   in
   subscription := Some sub;
   for _ = 1 to 10 do
     Eio.Fiber.yield ()
   done;
   check "daemon is parked on a full stream"
     (Eio.Stream.length (Sse.events sub) = 1);
   Sse.close sub;
   subscription := Some sub);
  let sub = Option.get !subscription in
  check "the owning switch finishes anyway"
    (Eio.Promise.peek (Sse.result sub) = Some (Ok ()));
  check "the undelivered event is still there"
    (match Eio.Stream.take (Sse.events sub) with
    | `Event event -> event.data = "one"
    | `End -> false)

(* A [retry] field is the server's, so it is clamped into
   [0.1 .. backoff_max] rather than obeyed. *)
let reconnect_after ~clock ~budget arrived =
  Eio.Fiber.first
    (fun () -> Eio.Time.Mono.sleep clock budget; `Slept)
    (fun () -> Eio.Promise.await arrived; `Reconnected)

let test_retry_clamp () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#mono_clock in
  let subscribe ~backoff_max body =
    let arrived, resolve = Eio.Promise.create () in
    let calls = ref 0 in
    let server req =
      incr calls;
      match !calls with
      | 1 -> response body Sse.media_type req
      | 2 ->
          Eio.Promise.resolve resolve ();
          Fetch_mock.respond ~status:401 "stop" req
      | _ -> Fetch_mock.respond ~status:401 "stop" req
    in
    let sub ~sw =
      Sse.subscribe ~sw ~clock ~backoff_initial:backoff_max ~backoff_max
        ~capacity:4 (Fetch_mock.client server) "https://events.example/retry"
    in
    (sub, arrived)
  in
  (Eio.Switch.run @@ fun sw ->
   (* Backoff alone would wait 30 s; [retry: 0] must not make it immediate. *)
   let sub, arrived = subscribe ~backoff_max:30. "retry: 0\ndata: x\n\n" in
   let s = sub ~sw in
   check "zero retry is floored at 100 ms"
     (reconnect_after ~clock ~budget:0.099 arrived = `Slept);
   check "but the retry field is still honoured"
     (reconnect_after ~clock ~budget:1. arrived = `Reconnected);
   Sse.close s);
  Eio.Switch.run @@ fun sw ->
  (* A retry of 999999999 ms is over eleven days; the cap wins. *)
  let sub, arrived =
    subscribe ~backoff_max:2. "retry: 999999999\ndata: x\n\n"
  in
  let s = sub ~sw in
  check "an absurd retry is capped at backoff_max"
    (reconnect_after ~clock ~budget:5. arrived = `Reconnected);
  Sse.close s

let invalid name f =
  check name
    (match f () with
    | exception Invalid_argument _ -> true
    | _ -> false)

let test_arguments () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = Fetch_mock.client (response "" Sse.media_type) in
  invalid "zero event bound" (fun () ->
    let response = Fetch.get ~sw client "https://events.example/args" in
    ignore ((Sse.decode ~max_event:0 response) ()));
  let subscribe ?backoff_initial ?backoff_max ?capacity () =
    Sse.subscribe ~sw ~clock:env#mono_clock ?backoff_initial ?backoff_max
      ?capacity client "https://events.example/args"
    |> ignore
  in
  invalid "zero initial backoff" (fun () ->
    subscribe ~backoff_initial:0. ());
  invalid "NaN backoff" (fun () ->
    subscribe ~backoff_initial:Float.nan ());
  invalid "infinite backoff cap" (fun () ->
    subscribe ~backoff_max:Float.infinity ());
  invalid "initial backoff exceeds cap" (fun () ->
    subscribe ~backoff_initial:2. ~backoff_max:1. ());
  invalid "zero capacity" (fun () -> subscribe ~capacity:0 ())

let () =
  test_framing ();
  test_fields_and_limits ();
  test_framing_vectors ();
  test_connect ();
  test_control_id_reconnect ();
  test_subscription ();
  test_connection_retry ();
  test_close ();
  test_backpressure ();
  test_switch_cancellation ();
  test_close_on_a_full_stream ();
  test_retry_clamp ();
  test_arguments ();
  Printf.printf "test_sse: %d checks ok\n" !checks
