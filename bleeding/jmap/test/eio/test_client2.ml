(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for the three parts of {!Jmap_eio.Client} that are about how bytes and
    sessions move rather than about what they say: the streaming blob endpoints
    of RFC 8620 §6.1 and §6.2, the [sessionState] lifecycle of RFC 8620 §2 and
    §3.4, and the concurrency limits the core capability of RFC 8620 §2 states.

    Everything runs on a mock Fetch backend under [Eio_mock.Backend.run], so a
    "concurrent" test is deterministic: the mock handler yields, which puts
    every fiber that is allowed to be in flight into the handler before any of
    them leaves it. *)

open Jmap_eio

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)

(* {1 A mock server} *)

let media_headers media = Http.Header.of_list [ ("content-type", media) ]
let json_headers = media_headers "application/json"

let respond_json ?status body req =
  Fetch_mock.respond ?status ~headers:json_headers body req

let not_found req = Fetch_mock.respond ~status:404 "not found" req

let path (req : Fetch.Middleware.request) =
  Fetch.Middleware.Url.path_and_query req.url

let token = "TOKEN"
let auth = Auth.bearer token
let well_known = "https://api.example.com/.well-known/jmap"
let account_id = Jmap.Proto.Id.of_string_exn "acc1"
let blob_id = Jmap.Proto.Id.of_string_exn "B1"

(* RFC 8620 §2: every member of the core capability object is mandatory, so a
   session that states one limit states them all. *)
let core_capability ~max_concurrent_requests ~max_concurrent_upload =
  Printf.sprintf
    {|{
      "maxSizeUpload": 50000000,
      "maxConcurrentUpload": %d,
      "maxSizeRequest": 10000000,
      "maxConcurrentRequests": %d,
      "maxCallsInRequest": 16,
      "maxObjectsInGet": 500,
      "maxObjectsInSet": 500,
      "collationAlgorithms": []
    }|}
    max_concurrent_upload max_concurrent_requests

let session_json
    ?(core =
      core_capability ~max_concurrent_requests:4 ~max_concurrent_upload:4)
    ?(state = "state-1") () =
  let core =
    if String.equal core "" then ""
    else Printf.sprintf "\"urn:ietf:params:jmap:core\": %s" core
  in
  Printf.sprintf
    {|{
  "capabilities": { %s },
  "accounts": {
    "acc1": {
      "name": "Test Account",
      "isPersonal": true,
      "isReadOnly": false,
      "accountCapabilities": {}
    }
  },
  "primaryAccounts": { "urn:ietf:params:jmap:core": "acc1" },
  "username": "test@example.com",
  "apiUrl": "https://api.example.com/jmap/api/",
  "downloadUrl": "https://api.example.com/jmap/download/{accountId}/{blobId}/{name}?type={type}",
  "uploadUrl": "https://api.example.com/jmap/upload/{accountId}/",
  "eventSourceUrl": "https://api.example.com/jmap/eventsource/?types={types}&closeafter={closeafter}&ping={ping}",
  "state": "%s"
}|}
    core state

let upload_response ~size =
  Printf.sprintf
    {|{ "accountId": "acc1", "blobId": "B1", "type": "text/plain", "size": %d }|}
    size

let echo_response ~session_state =
  Printf.sprintf
    {|{ "methodResponses": [ [ "Core/echo", {}, "c1" ] ], "sessionState": "%s" }|}
    session_state

let echo_request () =
  Jmap.Proto.Request.create
    ~using:[ Jmap.Proto.Capability.core ]
    ~method_calls:
      [
        Jmap.Proto.Invocation.create ~name:"Core/echo"
          ~arguments:(Jsont.Object ([], Jsont.Meta.none))
          ~method_call_id:"c1";
      ]
    ()

let client_exn ~sw ?max_body server =
  match
    Client.connect ~sw ~auth ?max_body
      (Transport.of_fetch (Fetch_mock.client server))
      well_known
  with
  | Ok client -> client
  | Error e -> Alcotest.failf "connect failed: %s" (Client.error_to_string e)

let client_timed_exn ~sw ~clock ~timeout server =
  match
    Client.connect ~sw ~auth ~timeout
      (Transport.of_fetch ~clock (Fetch_mock.client server))
      well_known
  with
  | Ok client -> client
  | Error e -> Alcotest.failf "connect failed: %s" (Client.error_to_string e)

let ok = function
  | Ok v -> v
  | Error e ->
      Alcotest.failf "expected success, got %s" (Client.error_to_string e)

(* {1 Streaming upload (RFC 8620 §6.1)} *)

(* What a backend sees of a streamed body: the length the client declared -
   which is what decides between [Content-Length] and chunked framing - and
   the bytes the flow yields. *)
type sent = { length : int64 option; data : string }

let read_body (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> { length = Some 0L; data = "" }
  | Fetch.String s ->
      { length = Some (Int64.of_int (String.length s)); data = s }
  | Fetch.Stream { length; flow } ->
      {
        length;
        data = Eio.Buf_read.(take_all (of_flow ~max_size:max_int flow));
      }

let upload_server sent req =
  match path req with
  | "/.well-known/jmap" -> respond_json (session_json ()) req
  | "/jmap/upload/acc1/" ->
      let body = read_body req in
      sent := Some body;
      respond_json (upload_response ~size:(String.length body.data)) req
  | _ -> not_found req

(* A declared length is passed to the transport, which sends it as
   [Content-Length]; without one the body is chunked, because its size is
   not known until the flow ends. Either way the bytes are the flow's. *)
let test_upload_flow_length () =
  let sent = ref None in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (upload_server sent) in
  let data = "streamed blob" in
  let resp =
    ok
      (Client.upload_flow client ~account_id ~content_type:"text/plain"
         ~length:(Int64.of_int (String.length data))
         (Eio.Flow.string_source data))
  in
  check_string "blob id" "B1"
    (Jmap.Proto.Id.to_string resp.Jmap.Proto.Blob.blob_id);
  match !sent with
  | None -> Alcotest.fail "no upload reached the server"
  | Some { length; data = got } ->
      Alcotest.(check (option int64)) "declared length" (Some 13L) length;
      check_string "streamed bytes" data got

let test_upload_flow_chunked () =
  let sent = ref None in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (upload_server sent) in
  let data = "no length known" in
  ignore
    (ok
       (Client.upload_flow client ~account_id ~content_type:"text/plain"
          (Eio.Flow.string_source data)));
  match !sent with
  | None -> Alcotest.fail "no upload reached the server"
  | Some { length; data = got } ->
      Alcotest.(check (option int64)) "no declared length" None length;
      check_string "streamed bytes" data got

(* The string form keeps a replayable body, which is what lets a retry or a
   redirect send it again. *)
let test_upload_string_is_replayable () =
  let sent = ref None in
  let server req =
    (match path req with
    | "/jmap/upload/acc1/" ->
        sent :=
          Some
            (match req.Fetch.Middleware.body with
            | Fetch.String _ -> "string"
            | Fetch.Stream _ -> "stream"
            | Fetch.Empty -> "empty")
    | _ -> ());
    upload_server (ref None) req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  ignore
    (ok
       (Client.upload client ~account_id ~content_type:"text/plain" ~data:"hi"));
  Alcotest.(check (option string)) "body kind" (Some "string") !sent

(* {1 Streaming download (RFC 8620 §6.2)} *)

let download_server ?(media = "text/plain") ~blob req =
  match path req with
  | "/.well-known/jmap" -> respond_json (session_json ()) req
  | _
    when String.length (path req) > 15
         && String.sub (path req) 0 15 = "/jmap/download/" ->
      Fetch_mock.respond ~headers:(media_headers media) blob req
  | _ -> not_found req

(* The bytes go to the sink and the media type comes back, which is the
   server's choice per RFC 8620 §6.2 and need not be the one asked for. *)
let test_download_to () =
  let blob =
    String.concat "" (List.init 500 (fun i -> Printf.sprintf "%03d\n" i))
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (download_server ~media:"image/png" ~blob) in
  let buf = Buffer.create 16 in
  let served =
    ok
      (Client.download_to client ~account_id ~blob_id ~accept:"text/plain"
         (Eio.Flow.buffer_sink buf))
  in
  check_string "served type" "image/png" served;
  check_int "bytes copied" (String.length blob) (Buffer.length buf);
  check_string "bytes" blob (Buffer.contents buf)

(* [download_with_type] is the same copy pointed at a bounded buffer, so a
   blob over the client's limit is a protocol error rather than a heap. *)
let test_download_with_type_bounded () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let max_body = 2048 in
  let client blob = client_exn ~sw ~max_body (download_server ~blob) in
  (* Just inside the limit still arrives whole. *)
  let small = String.make max_body 'x' in
  let media, data =
    ok (Client.download_with_type (client small) ~account_id ~blob_id ())
  in
  check_string "media" "text/plain" media;
  check_int "all of it" max_body (String.length data);
  (* One byte past it is a protocol error: the peer sent more than this
     client agreed to read. *)
  match
    Client.download_with_type
      (client (String.make (max_body + 1) 'x'))
      ~account_id ~blob_id ()
  with
  | Ok _ -> Alcotest.fail "expected the body limit to fire"
  | Error (Client.Transport (Fetch.Protocol_error _, _)) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)

(* {1 Session lifecycle (RFC 8620 §2, §3.4)} *)

(* Three fibers see the same new [sessionState] at once. The session resource
   is fetched once for the connection and once more between the three of
   them, and the observers run once. *)
let test_session_state_refetch () =
  let gets = ref 0 in
  let state = ref "state-1" in
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        incr gets;
        Eio.Fiber.yield ();
        respond_json (session_json ~state:!state ()) req
    | "/jmap/api/" ->
        (* Yield so that every fiber is inside the exchange before any of
           them looks at the session state. *)
        Eio.Fiber.yield ();
        respond_json (echo_response ~session_state:"state-2") req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  check_int "one fetch to connect" 1 !gets;
  let changes = ref [] in
  Client.on_session_change client (fun s ->
      changes := s.Jmap.Proto.Session.state :: !changes);
  (* From now on the session resource reports the state the API reports. *)
  state := "state-2";
  Eio.Fiber.all
    (List.init 3 (fun _ () ->
         ignore (ok (Client.request client (echo_request ())))));
  check_int "one refetch between three fibers" 2 !gets;
  check_int "one notification" 1 (List.length !changes);
  check_string "notified with the new session" "state-2" (List.hd !changes);
  check_string "session is fresh" "state-2"
    (Client.session client).Jmap.Proto.Session.state

(* A response that agrees with the session refetches nothing. *)
let test_session_state_stable () =
  let gets = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        incr gets;
        respond_json (session_json ()) req
    | "/jmap/api/" -> respond_json (echo_response ~session_state:"state-1") req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let changes = ref 0 in
  Client.on_session_change client (fun _ -> incr changes);
  ignore (ok (Client.request client (echo_request ())));
  ignore (ok (Client.request client (echo_request ())));
  check_int "no refetch" 1 !gets;
  check_int "no notification" 0 !changes

(* A server whose session resource lags its API - it keeps reporting the old
   state - is refetched once for that state and not once per request. *)
let test_session_state_lagging () =
  let gets = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        incr gets;
        respond_json (session_json ()) req
    | "/jmap/api/" -> respond_json (echo_response ~session_state:"state-9") req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  for _ = 1 to 4 do
    ignore (ok (Client.request client (echo_request ())))
  done;
  check_int "one refetch for one new state" 2 !gets

(* {!Client.session_changed} is broadcast by a refresh, so a fiber can wait
   for one rather than be called back. *)
let test_session_changed_condition () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let woken = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio.Condition.await_no_mutex (Client.session_changed client);
      woken := true)
    (fun () ->
      Eio.Fiber.yield ();
      ok (Client.refresh_session client));
  Alcotest.(check bool) "the waiter woke" true !woken

(* An observer that raises is not the refresh's failure, and does not stop
   the observers after it. *)
let test_observer_isolation () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let ran = ref 0 in
  Client.on_session_change client (fun _ ->
      incr ran;
      failwith "boom");
  Client.on_session_change client (fun _ -> incr ran);
  ok (Client.refresh_session client);
  check_int "both observers ran" 2 !ran

(* {1 Concurrency limits (RFC 8620 §2)} *)

(* A handler that counts how many fibers are inside it at once. The yields
   give every fiber that the semaphore lets through a chance to arrive
   before the first one leaves. *)
let counting ~in_flight ~peak k =
  incr in_flight;
  if !in_flight > !peak then peak := !in_flight;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  decr in_flight;
  k ()

let test_limits_from_session () =
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        respond_json
          (session_json
             ~core:
               (core_capability ~max_concurrent_requests:2
                  ~max_concurrent_upload:1)
             ())
          req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let requests, uploads = Client.concurrency_limits client in
  check_int "maxConcurrentRequests" 2 requests;
  check_int "maxConcurrentUpload" 1 uploads

(* Zero is not a usable concurrency limit, so retain the conservative default.
   Session decoding itself rejects an absent mandatory core capability. *)
let test_limits_default () =
  let server core req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ~core ()) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let zero =
    client_exn ~sw
      (server
         (core_capability ~max_concurrent_requests:0 ~max_concurrent_upload:0))
  in
  let requests, uploads = Client.concurrency_limits zero in
  check_int "zero is not a limit a client can obey" 4 requests;
  check_int "same for uploads" 4 uploads

(* Eight fibers, a server that states two: never more than two exchanges are
   in flight, and all eight complete. *)
let test_request_semaphore () =
  let in_flight = ref 0 and peak = ref 0 and posts = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        respond_json
          (session_json
             ~core:
               (core_capability ~max_concurrent_requests:2
                  ~max_concurrent_upload:4)
             ())
          req
    | "/jmap/api/" ->
        incr posts;
        counting ~in_flight ~peak (fun () ->
            respond_json (echo_response ~session_state:"state-1") req)
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  Eio.Fiber.all
    (List.init 8 (fun _ () ->
         ignore (ok (Client.request client (echo_request ())))));
  check_int "every request was made" 8 !posts;
  Alcotest.(check bool)
    (Printf.sprintf "at most 2 in flight, saw %d" !peak)
    true (!peak <= 2);
  check_int "the limit was actually reached" 2 !peak

(* The upload semaphore is a separate count, from "maxConcurrentUpload". *)
let test_upload_semaphore () =
  let in_flight = ref 0 and peak = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        respond_json
          (session_json
             ~core:
               (core_capability ~max_concurrent_requests:8
                  ~max_concurrent_upload:1)
             ())
          req
    | "/jmap/upload/acc1/" ->
        counting ~in_flight ~peak (fun () ->
            respond_json (upload_response ~size:2) req)
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  Eio.Fiber.all
    (List.init 4 (fun _ () ->
         ignore
           (ok
              (Client.upload_flow client ~account_id ~content_type:"text/plain"
                 ~length:2L
                 (Eio.Flow.string_source "hi")))));
  check_int "uploads are serialised at one" 1 !peak

(* {1 Caller-supplied flows (RFC 8620 §6.1, §6.2)} *)

(* An Eio failure that is not one {!Fetch} defines: what a sink on a full
   disk, or a source that cannot be read, raises. *)
type Eio.Exn.Backend.t += Disk_full

let flow_failure () =
  Eio.Exn.create (Eio.Fs.E (Eio.Fs.Permission_denied Disk_full))

let failing_sink =
  let module Sink = struct
    type t = unit

    let single_write () _ = raise (flow_failure ())
    let copy () ~src:_ = raise (flow_failure ())
  end in
  Eio.Resource.T ((), Eio.Flow.Pi.sink (module Sink))

(* A sink may raise anything at all, not only an [Eio.Io]. *)
let odd_sink =
  let module Sink = struct
    type t = unit

    let single_write () _ = raise Not_found
    let copy () ~src:_ = raise Not_found
  end in
  Eio.Resource.T ((), Eio.Flow.Pi.sink (module Sink))

let failing_source =
  let module Source = struct
    type t = unit

    let read_methods = []
    let single_read () _ = raise (flow_failure ())
  end in
  Eio.Resource.T ((), Eio.Flow.Pi.source (module Source))

(* The caller's own flow failing is still a failure of the download, and this
   module's operations answer with a value. Letting it out as an exception
   would make [(_, error) result] a lie. *)
let test_download_to_sink_failure () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (download_server ~blob:"BLOB") in
  (match Client.download_to client ~account_id ~blob_id failing_sink with
  | Ok _ -> Alcotest.fail "expected the sink failure to be reported"
  (* The sink is not the peer, so this is not a protocol failure. *)
  | Error (Client.Transport (Fetch.Invalid_request _, _)) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e));
  match Client.download_to client ~account_id ~blob_id odd_sink with
  | Ok _ -> Alcotest.fail "expected the sink failure to be reported"
  | Error (Client.Transport (Fetch.Invalid_request _, _)) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)

let test_upload_flow_source_failure () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (upload_server (ref None)) in
  match
    Client.upload_flow client ~account_id ~content_type:"text/plain"
      failing_source
  with
  | Ok _ -> Alcotest.fail "expected the source failure to be reported"
  | Error (Client.Transport (Fetch.Invalid_request _, _)) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)

let test_upload_flow_negative_length () =
  let sent = ref None in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (upload_server sent) in
  (match
     Client.upload_flow client ~account_id ~content_type:"text/plain"
       ~length:(-1L)
       (Eio.Flow.string_source "x")
   with
  | Ok _ -> Alcotest.fail "expected the negative length to be rejected"
  | Error (Client.Transport (Fetch.Invalid_request _, _)) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e));
  Alcotest.(check bool) "no upload was sent" true (Option.is_none !sent)

(* {1 Deadlines around a blob (RFC 8620 §6.2)} *)

(* A blob is however many bytes the server has, so [?timeout] bounds the
   response head and each idle wait, not the total transfer. A caller whose
   sink must be all-or-nothing can write a temporary file and rename it. *)
let slow_download_server ?(status = 200) ~clock ~chunks ~head_delay req =
  match path req with
  | "/.well-known/jmap" -> respond_json (session_json ()) req
  | p when String.starts_with ~prefix:"/jmap/download/" p ->
      if head_delay > 0. then Eio.Time.sleep clock head_delay;
      let flow = Eio_mock.Flow.make "blob" in
      Eio_mock.Flow.on_read flow
        (List.map
           (fun (delay, chunk) ->
             `Run
               (fun () ->
                 Eio.Time.sleep clock delay;
                 chunk))
           chunks
        @ [ `Raise End_of_file ]);
      Fetch.Middleware.Pi.response
        ~close:(fun () -> ())
        ~status
        ~headers:(media_headers "application/octet-stream")
        ~version:`HTTP_1_1
        ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t)
        ~url:req.Fetch.Middleware.url ()
  | _ -> not_found req

let test_download_timeout_bounds_head_and_idle () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  (* The total is twelve seconds, but every chunk arrives within five. *)
  let client =
    client_timed_exn ~sw ~clock ~timeout:5.
      (slow_download_server ~clock
         ~chunks:[ (4., "one"); (4., "two"); (4., "three") ]
         ~head_delay:0.)
  in
  let buf = Buffer.create 16 in
  let media =
    ok
      (Client.download_to client ~account_id ~blob_id (Eio.Flow.buffer_sink buf))
  in
  check_string "served type" "application/octet-stream" media;
  check_string "the whole blob arrived" "onetwothree" (Buffer.contents buf);
  (* Progress followed by an idle read is a timeout. The already-written
     prefix is explicit in the contract rather than an infinite wait. *)
  let buf = Buffer.create 16 in
  let client =
    client_timed_exn ~sw ~clock ~timeout:5.
      (slow_download_server ~clock
         ~chunks:[ (0., "one"); (10., "two") ]
         ~head_delay:0.)
  in
  (match
     Client.download_to client ~account_id ~blob_id (Eio.Flow.buffer_sink buf)
   with
  | Ok _ -> Alcotest.fail "expected the idle body read to time out"
  | Error (Client.Timeout seconds) ->
      Alcotest.(check (float 1e-9)) "idle deadline" 5. seconds
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e));
  check_string "the prefix reached the sink" "one" (Buffer.contents buf);
  (* The head itself is still bounded: a server that does not answer at all
     is a timeout, and nothing was written. *)
  let buf = Buffer.create 16 in
  let client =
    client_timed_exn ~sw ~clock ~timeout:5.
      (slow_download_server ~clock ~chunks:[ (0., "x") ] ~head_delay:3600.)
  in
  (match
     Client.download_to client ~account_id ~blob_id (Eio.Flow.buffer_sink buf)
   with
  | Ok _ -> Alcotest.fail "expected the response head to time out"
  | Error (Client.Timeout seconds) ->
      Alcotest.(check (float 1e-9)) "deadline" 5. seconds
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e));
  check_int "and nothing reached the sink" 0 (Buffer.length buf);
  (* A prompt error head must not disable the deadline while the client reads
     the response's diagnostic body. *)
  let client =
    client_timed_exn ~sw ~clock ~timeout:5.
      (slow_download_server ~status:500 ~clock
         ~chunks:[ (10., "eventual error") ]
         ~head_delay:0.)
  in
  match
    Client.download_to client ~account_id ~blob_id
      (Eio.Flow.buffer_sink (Buffer.create 16))
  with
  | Ok _ -> Alcotest.fail "expected the stalled error body to time out"
  | Error (Client.Timeout seconds) ->
      Alcotest.(check (float 1e-9)) "error body deadline" 5. seconds
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)

(* {1 The session refresh (RFC 8620 §2, §3.4)} *)

(* The refresh is a network exchange, so a caller must be able to give up on
   it: held under a mutex taken with [~protect:true] it would be beyond the
   reach of a deadline, of an [Eio.Cancel] scope and of Ctrl-C. The exchange
   goes on for the fibers that did not give up, and the lock is not left
   held. *)
let test_refresh_is_cancellable () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let release, release_u = Eio.Promise.create () in
  let hang = ref false in
  let gets = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        incr gets;
        if !hang then Eio.Promise.await release;
        respond_json (session_json ~state:"state-2" ()) req
    | _ -> not_found req
  in
  let client = client_exn ~sw server in
  hang := true;
  let outcome =
    Eio.Fiber.first
      (fun () ->
        ignore (Client.refresh_session client);
        `Refreshed)
      (fun () ->
        Eio.Fiber.yield ();
        `Gave_up)
  in
  Alcotest.(check bool) "the caller gave up on it" true (outcome = `Gave_up);
  check_int "and the exchange had started" 2 !gets;
  (* Let the shared exchange finish: a second caller joins the one in flight
     rather than starting another. *)
  Eio.Promise.resolve release_u ();
  hang := false;
  ok (Client.refresh_session client);
  check_int "the second caller joined it" 2 !gets;
  check_string "and the session was installed" "state-2"
    (Client.session client).Jmap.Proto.Session.state;
  (* Nothing is poisoned: a later refresh is a new exchange. *)
  ok (Client.refresh_session client);
  check_int "a later refresh is its own" 3 !gets

(* A session endpoint that is down must not turn every request into two round
   trips, and must not fail silently either: the attempt is recorded whether
   or not it worked, and the failure is kept. *)
let test_failed_refresh_is_recorded_once () =
  let gets = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        incr gets;
        if !gets = 1 then respond_json (session_json ()) req
        else Fetch_mock.respond ~status:503 "the session resource is down" req
    | "/jmap/api/" -> respond_json (echo_response ~session_state:"state-2") req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  for _ = 1 to 4 do
    ignore (ok (Client.request client (echo_request ())))
  done;
  check_int "one attempt for one new sessionState" 2 !gets;
  check_string "the session held is still the old one" "state-1"
    (Client.session client).Jmap.Proto.Session.state;
  match Client.last_refresh_error client with
  | Some (Client.Http_error (503, _)) -> ()
  | Some e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)
  | None -> Alcotest.fail "the failed refresh was swallowed"

(* A transport that raises something {!Fetch} does not define - the door
   {!Transport.of_fetch} opens - is reported as a session failure rather than
   escaping as an exception no signature mentions, and leaves the client
   usable. *)
let test_refresh_survives_a_stray_exception () =
  let boom = ref false in
  let server req =
    match path req with
    | "/.well-known/jmap" ->
        if !boom then failwith "the backend exploded";
        respond_json (session_json ()) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  boom := true;
  (match Client.refresh_session client with
  | Ok () -> Alcotest.fail "expected the refresh to fail"
  | Error (Client.Session_error _) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e));
  (match Client.last_refresh_error client with
  | Some (Client.Session_error _) -> ()
  | Some e ->
      Alcotest.failf "unexpected recorded error: %s" (Client.error_to_string e)
  | None -> Alcotest.fail "the unexpected exception was not recorded");
  boom := false;
  (* Not poisoned: the next refresh works. *)
  ok (Client.refresh_session client);
  Alcotest.(check bool)
    "a later success clears the recorded error" true
    (Option.is_none (Client.last_refresh_error client))

let () =
  Alcotest.run "jmap-eio-client"
    [
      ( "streaming blobs",
        [
          Alcotest.test_case "upload_flow declares a length" `Quick
            test_upload_flow_length;
          Alcotest.test_case "upload_flow without a length is chunked" `Quick
            test_upload_flow_chunked;
          Alcotest.test_case "upload keeps a replayable body" `Quick
            test_upload_string_is_replayable;
          Alcotest.test_case "download_to copies to a sink" `Quick
            test_download_to;
          Alcotest.test_case "download_with_type is bounded" `Quick
            test_download_with_type_bounded;
          Alcotest.test_case "a failing sink is an error" `Quick
            test_download_to_sink_failure;
          Alcotest.test_case "a failing source is an error" `Quick
            test_upload_flow_source_failure;
          Alcotest.test_case "a negative upload length is an error" `Quick
            test_upload_flow_negative_length;
          Alcotest.test_case "the deadline bounds the head and idle reads"
            `Quick test_download_timeout_bounds_head_and_idle;
        ] );
      ( "session state",
        [
          Alcotest.test_case "a mismatch refetches once" `Quick
            test_session_state_refetch;
          Alcotest.test_case "agreement refetches nothing" `Quick
            test_session_state_stable;
          Alcotest.test_case "a lagging session resource is not hammered" `Quick
            test_session_state_lagging;
          Alcotest.test_case "session_changed is broadcast" `Quick
            test_session_changed_condition;
          Alcotest.test_case "an observer that raises is isolated" `Quick
            test_observer_isolation;
          Alcotest.test_case "a refresh can be given up on" `Quick
            test_refresh_is_cancellable;
          Alcotest.test_case "a failed refresh is recorded, once" `Quick
            test_failed_refresh_is_recorded_once;
          Alcotest.test_case "a stray exception is not a poisoned lock" `Quick
            test_refresh_survives_a_stray_exception;
        ] );
      ( "concurrency limits",
        [
          Alcotest.test_case "limits come from the core capability" `Quick
            test_limits_from_session;
          Alcotest.test_case "zero limits default" `Quick test_limits_default;
          Alcotest.test_case "requests are bounded" `Quick
            test_request_semaphore;
          Alcotest.test_case "uploads are bounded separately" `Quick
            test_upload_semaphore;
        ] );
    ]
