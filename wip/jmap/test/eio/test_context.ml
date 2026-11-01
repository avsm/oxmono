open Jmap_eio

let origin = "https://context.example"
let session_url = origin ^ "/PRIVATE_PATH?token=QUERY_SECRET#FRAGMENT_SECRET"
let account_id = Jmap.Proto.Id.of_string_exn "acc"
let blob_id = Jmap.Proto.Id.of_string_exn "blob"

let session =
  {|{"capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":1000,"maxConcurrentUpload":1,"maxSizeRequest":10000,"maxConcurrentRequests":1,"maxCallsInRequest":16,"maxObjectsInGet":10,"maxObjectsInSet":10,"collationAlgorithms":[]}},"accounts":{},"primaryAccounts":{},"username":"user","apiUrl":"/api","downloadUrl":"/PRIVATE_PATH/{accountId}/{blobId}/{name}?type={type}&token=QUERY_SECRET","uploadUrl":"/upload/{accountId}","eventSourceUrl":"/events?types={types}&closeafter={closeafter}&ping={ping}","state":"s1"}|}

let request () =
  let open Jmap.Proto in
  Request.create ~using:[ Capability.core ]
    ~method_calls:
      [
        Invocation.create ~name:"Core/echo" ~method_call_id:"CALL_ID_SECRET"
          ~arguments:
            (Jsont.Object
               ( [
                   ( ("password", Jsont.Meta.none),
                     Jsont.String ("BODY_SECRET", Jsont.Meta.none) );
                 ],
                 Jsont.Meta.none ));
      ]
    ()

let failure =
  Eio.Exn.add_context
    (Fetch.err (Fetch.Protocol_error "injected read failure"))
    "reading lower-level flow"

let contains ~substring text =
  let n = String.length substring in
  let rec loop i =
    i + n <= String.length text
    && (String.sub text i n = substring || loop (i + 1))
  in
  loop 0

let has label substring text =
  Alcotest.(check bool) label true (contains ~substring text)

let check_context ~operation message =
  has "operation" operation message;
  has "endpoint origin" origin message;
  has "underlying failure" "injected read failure" message;
  has "underlying context" "reading lower-level flow" message

let transport_message = function
  | Error
      (Client.Transport (Fetch.Protocol_error "injected read failure", message))
    ->
      message
  | Error error ->
      Alcotest.failf "wrong error: %s" (Client.error_to_string error)
  | Ok _ -> Alcotest.fail "expected I/O failure"

let failing_response ?(media = "application/json") ?(status = 200) closed req =
  let flow = Eio_mock.Flow.make "broken response" in
  Eio_mock.Flow.on_read flow [ `Raise failure ];
  Fetch.Middleware.Pi.response ~status
    ~headers:(Http.Header.of_list [ ("content-type", media) ])
    ~version:`HTTP_1_1
    ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t)
    ~url:req.Fetch.Middleware.url
    ~close:(fun () -> closed := true)
    ()

let respond_session req =
  Fetch_mock.respond
    ~headers:(Http.Header.of_list [ ("content-type", "application/json") ])
    session req

let with_client handler f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let first = ref true in
  let server req =
    if !first then (
      first := false;
      respond_session req)
    else handler req
  in
  match
    Client.connect ~sw
      (Transport.of_fetch (Fetch_mock.client server))
      session_url
  with
  | Error error -> Alcotest.fail (Client.error_to_string error)
  | Ok client -> f client

let test_session_context () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let closed = ref false in
  let message =
    Client.connect ~sw
      (Transport.of_fetch (Fetch_mock.client (failing_response closed)))
      session_url
    |> transport_message
  in
  check_context ~operation:"fetching JMAP session" message;
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        "URL secrets omitted" false
        (contains ~substring:secret message))
    [ "PRIVATE_PATH"; "QUERY_SECRET"; "FRAGMENT_SECRET" ];
  Alcotest.(check bool) "response closed" true !closed

let test_refresh_context () =
  with_client (failing_response (ref false)) @@ fun client ->
  let message = Client.refresh_session client |> transport_message in
  check_context ~operation:"refreshing JMAP session" message;
  match Client.last_refresh_error client with
  | Some (Client.Transport (_, saved)) ->
      Alcotest.(check string) "stored context" message saved
  | _ -> Alcotest.fail "refresh diagnostic was not retained"

let test_request_context () =
  with_client (failing_response (ref false)) @@ fun client ->
  let message = Client.request client (request ()) |> transport_message in
  check_context ~operation:"calling JMAP methods Core/echo" message;
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        "request data omitted" false
        (contains ~substring:secret message))
    [ "BODY_SECRET"; "CALL_ID_SECRET" ]

let test_upload_context () =
  with_client (failing_response (ref false)) @@ fun client ->
  let message =
    Client.upload client ~account_id ~content_type:"text/plain"
      ~data:"UPLOAD_SECRET"
    |> transport_message
  in
  check_context ~operation:"uploading JMAP blob" message;
  Alcotest.(check bool)
    "upload data omitted" false
    (contains ~substring:"UPLOAD_SECRET" message)

let test_download_context () =
  with_client (failing_response ~media:"application/octet-stream" (ref false))
  @@ fun client ->
  let message =
    Client.download client ~account_id ~blob_id ~name:"FILENAME_SECRET" ()
    |> transport_message
  in
  check_context ~operation:"downloading JMAP blob" message;
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        "download URL data omitted" false
        (contains ~substring:secret message))
    [ "PRIVATE_PATH"; "QUERY_SECRET"; "FILENAME_SECRET" ]

let test_source_context () =
  let handler req =
    match req.Fetch.Middleware.body with
    | Fetch.Stream { flow; _ } ->
        Eio.Flow.copy flow (Eio.Flow.buffer_sink (Buffer.create 10));
        assert false
    | _ -> Alcotest.fail "expected streaming upload"
  in
  with_client handler @@ fun client ->
  let source = Eio_mock.Flow.make "broken upload source" in
  Eio_mock.Flow.on_read source [ `Raise failure ];
  match
    Client.upload_flow client ~account_id ~content_type:"text/plain" source
  with
  | Error (Client.Transport (Fetch.Invalid_request _, message)) ->
      check_context ~operation:"uploading JMAP blob" message;
      has "source operation" "the upload source failed" message
  | Error error -> Alcotest.fail (Client.error_to_string error)
  | Ok _ -> Alcotest.fail "expected upload source failure"

let test_sink_context () =
  with_client (Fetch_mock.respond "blob bytes") @@ fun client ->
  let sink = Eio_mock.Flow.make "broken download sink" in
  Eio_mock.Flow.on_copy_bytes sink [ `Raise failure ];
  match Client.download_to client ~account_id ~blob_id sink with
  | Error (Client.Transport (Fetch.Invalid_request _, message)) ->
      check_context ~operation:"downloading JMAP blob" message;
      has "sink operation" "the download sink failed" message
  | Error error -> Alcotest.fail (Client.error_to_string error)
  | Ok _ -> Alcotest.fail "expected download sink failure"

let test_raw_unix_context () =
  with_client (fun _ -> raise (Unix.Unix_error (Unix.ECONNRESET, "read", "")))
  @@ fun client ->
  match Client.request client (request ()) with
  | Error (Client.Transport (Fetch.Connection_failure _, message)) ->
      has "request operation" "calling JMAP methods Core/echo" message;
      has "origin" origin message;
      has "underlying Unix failure" "ECONNRESET" message
  | Error error -> Alcotest.fail (Client.error_to_string error)
  | Ok _ -> Alcotest.fail "expected Unix error"

let test_get_context () =
  with_client (fun _ -> raise failure) @@ fun client ->
  let message =
    Client.with_get client (origin ^ "/resource") (fun _ ->
        Alcotest.fail "callback called")
    |> transport_message
  in
  check_context ~operation:"fetching JMAP resource" message

let test_exception_printer () =
  with_client (failing_response (ref false)) @@ fun client ->
  match Client.request_exn client (request ()) with
  | exception (Client.Jmap_client_error _ as exn) ->
      let message = Printexc.to_string exn in
      check_context ~operation:"calling JMAP methods Core/echo" message;
      let unsafe =
        Client.Jmap_client_error
          (Client.Transport (Fetch.Protocol_error "test", "server\027[2J\ntext"))
      in
      let printed = Printexc.to_string unsafe in
      Alcotest.(check bool)
        "terminal controls escaped" false
        (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) printed)
  | _ -> Alcotest.fail "expected client exception"

let () =
  Alcotest.run "Client I/O context"
    [
      ( "context",
        [
          Alcotest.test_case "session and URL privacy" `Quick
            test_session_context;
          Alcotest.test_case "session refresh" `Quick test_refresh_context;
          Alcotest.test_case "method request" `Quick test_request_context;
          Alcotest.test_case "upload" `Quick test_upload_context;
          Alcotest.test_case "download" `Quick test_download_context;
          Alcotest.test_case "upload source" `Quick test_source_context;
          Alcotest.test_case "download sink" `Quick test_sink_context;
          Alcotest.test_case "raw Unix error" `Quick test_raw_unix_context;
          Alcotest.test_case "GET failure" `Quick test_get_context;
          Alcotest.test_case "registered exception printer" `Quick
            test_exception_printer;
        ] );
    ]
