(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Regression tests for sync response validation and parallel failures. *)

open Jmap_eio
module Sync = Jmap_eio.Sync

let well_known = "https://api.example.com/.well-known/jmap"
let account_id = Jmap.Proto.Id.of_string_exn "acc1"
let json_headers = Http.Header.of_list [ ("content-type", "application/json") ]

let session_json =
  {|{
  "capabilities": {
    "urn:ietf:params:jmap:core": {
      "maxSizeUpload": 50000000,
      "maxConcurrentUpload": 4,
      "maxSizeRequest": 10000000,
      "maxConcurrentRequests": 2,
      "maxCallsInRequest": 16,
      "maxObjectsInGet": 2,
      "maxObjectsInSet": 500,
      "collationAlgorithms": []
    },
    "urn:ietf:params:jmap:mail": {}
  },
  "accounts": {
    "acc1": {
      "name": "Test", "isPersonal": true, "isReadOnly": false,
      "accountCapabilities": {}
    }
  },
  "primaryAccounts": { "urn:ietf:params:jmap:core": "acc1" },
  "username": "test@example.com",
  "apiUrl": "https://api.example.com/jmap/api/",
  "downloadUrl": "https://api.example.com/download/{accountId}/{blobId}/{name}?type={type}",
  "uploadUrl": "https://api.example.com/upload/{accountId}/",
  "eventSourceUrl": "https://api.example.com/events?types={types}&closeafter={closeafter}&ping={ping}",
  "state": "state-1"
}|}

let path (req : Fetch.Middleware.request) =
  Fetch.Middleware.Url.path_and_query req.url

let body (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> ""
  | Fetch.String body -> body
  | Fetch.Stream _ -> Alcotest.fail "unexpected streaming request"

let respond json req = Fetch_mock.respond ~headers:json_headers json req

let client_exn ~sw reply =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond session_json req
    | "/jmap/api/" -> respond (reply (body req)) req
    | _ -> Fetch_mock.respond ~status:404 "not found" req
  in
  match
    Client.connect ~sw ~auth:(Auth.bearer "TOKEN")
      (Transport.of_fetch (Fetch_mock.client server))
      well_known
  with
  | Ok client -> client
  | Error error ->
      Alcotest.failf "connect failed: %s" (Client.error_to_string error)

let quoted values =
  String.concat "," (List.map (fun value -> "\"" ^ value ^ "\"") values)

let query_page ~position ids =
  Printf.sprintf
    {|{"methodResponses":[["Email/query",{"accountId":"acc1","queryState":"q1","canCalculateChanges":true,"position":%Ld,"ids":[%s],"limit":2},"c0"]],"sessionState":"state-1"}|}
    position (quoted ids)

let changes_round ~old_state ~new_state ~more =
  Printf.sprintf
    {|{"methodResponses":[["Email/changes",{"accountId":"acc1","oldState":"%s","newState":"%s","hasMoreChanges":%b,"created":[],"updated":[],"destroyed":[]},"c0"]],"sessionState":"state-1"}|}
    old_state new_state more

let query client =
  Sync.all_ids client ~page_size:2L (fun ~position ~limit ->
      Jmap.Chain.email_query ~account_id ~position ~limit ())

let test_forward_page_skip () =
  let round = ref 0 in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw (fun _ ->
        incr round;
        if !round = 1 then query_page ~position:0L [ "e1"; "e2" ]
        else query_page ~position:4L [ "e5"; "e6" ])
  in
  match query client with
  | Error (Sync.Nonadvancing_query { requested = 2L; returned = 4L }) -> ()
  | Error error ->
      Alcotest.failf "unexpected error: %s" (Sync.error_to_string error)
  | Ok _ -> Alcotest.fail "a forward page skip was accepted"

let changes client =
  Sync.changes client ~since:"s0" (fun ~since_state ~max_changes ->
      Jmap.Chain.email_changes ~account_id ~since_state ~max_changes ())

let expect_old_state_error ~requested ~returned result =
  match result with
  | Error (Sync.Mismatched_changes_state m)
    when m.requested = requested && m.returned = returned ->
      ()
  | Error error ->
      Alcotest.failf "unexpected error: %s" (Sync.error_to_string error)
  | Ok _ -> Alcotest.fail "a mismatched changes oldState was accepted"

let test_wrong_initial_old_state () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw (fun _ ->
        changes_round ~old_state:"wrong" ~new_state:"s1" ~more:false)
  in
  expect_old_state_error ~requested:"s0" ~returned:"wrong" (changes client)

let test_wrong_later_old_state () =
  let round = ref 0 in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw (fun _ ->
        incr round;
        if !round = 1 then
          changes_round ~old_state:"s0" ~new_state:"s1" ~more:true
        else changes_round ~old_state:"s0" ~new_state:"s2" ~more:false)
  in
  expect_old_state_error ~requested:"s1" ~returned:"s0" (changes client)

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec loop i =
    i + nl <= hl && (String.sub haystack i nl = needle || loop (i + 1))
  in
  loop 0

let test_parallel_first_failure () =
  Eio_mock.Backend.run @@ fun () ->
  let second_started, signal_second_started = Eio.Promise.create () in
  let first_answered, signal_first_answered = Eio.Promise.create () in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw (fun request ->
        if contains ~needle:"\"e1\"" request then begin
          Eio.Promise.await second_started;
          Eio.Promise.resolve signal_first_answered ();
          {|{"methodResponses":[["error",{"type":"serverFail","description":"FIRST"},"c0"]],"sessionState":"state-1"}|}
        end
        else begin
          Eio.Promise.resolve signal_second_started ();
          Eio.Promise.await first_answered;
          {|{"methodResponses":[["error",{"type":"serverFail","description":"SECOND"},"c0"]],"sessionState":"state-1"}|}
        end)
  in
  let ids = List.map Jmap.Proto.Id.of_string_exn [ "e1"; "e2"; "e3"; "e4" ] in
  match
    Sync.get_all client ~batch:2L ~max_concurrent:2 ids (fun ~ids ->
        Jmap.Chain.email_get ~account_id ~ids:(Jmap.Chain.ids ids) ())
  with
  | Error error ->
      let message = Sync.error_to_string error in
      Alcotest.(check bool)
        ("first error preserved: " ^ message)
        true
        (contains ~needle:"FIRST" message)
  | Ok _ -> Alcotest.fail "parallel failed batches succeeded"

let () =
  Alcotest.run "jmap-eio-sync-release"
    [
      ( "sync",
        [
          Alcotest.test_case "forward page skip" `Quick test_forward_page_skip;
          Alcotest.test_case "wrong initial changes oldState" `Quick
            test_wrong_initial_old_state;
          Alcotest.test_case "wrong later changes oldState" `Quick
            test_wrong_later_old_state;
          Alcotest.test_case "parallel first failure" `Quick
            test_parallel_first_failure;
        ] );
    ]
