(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Regression tests for Mosaic send outcomes and Identity selection. *)

module Io = Jmap_mosaic.Io
module Model = Jmap_mosaic.Model
module Proto = Jmap.Proto

let well_known = "https://api.example.com/.well-known/jmap"
let json_headers = Http.Header.of_list [ ("content-type", "application/json") ]

let session_json max_calls =
  Printf.sprintf
    {|{
  "capabilities": {
    "urn:ietf:params:jmap:core": {
      "maxSizeUpload": 50000000, "maxConcurrentUpload": 4,
      "maxSizeRequest": 10000000, "maxConcurrentRequests": 4,
      "maxCallsInRequest": %d, "maxObjectsInGet": 500,
      "maxObjectsInSet": 500, "collationAlgorithms": []
    },
    "urn:ietf:params:jmap:mail": {},
    "urn:ietf:params:jmap:submission": {}
  },
  "accounts": {
    "acc1": {"name":"Test","isPersonal":true,"isReadOnly":false,
      "accountCapabilities": {
        "urn:ietf:params:jmap:mail": {
          "maxMailboxesPerEmail": null, "maxMailboxDepth": null,
          "maxSizeMailboxName": 100, "maxSizeAttachmentsPerEmail": 0,
          "emailQuerySortOptions": [], "mayCreateTopLevelMailbox": true
        },
        "urn:ietf:params:jmap:submission": {"maxDelayedSend":0,"submissionExtensions":{}}
      }}
  },
  "primaryAccounts": {
    "urn:ietf:params:jmap:mail": "acc1",
    "urn:ietf:params:jmap:submission": "acc1"
  },
  "username": "local", "apiUrl": "https://api.example.com/jmap/api/",
  "downloadUrl": "https://api.example.com/download/{accountId}/{blobId}/{name}?type={type}",
  "uploadUrl": "https://api.example.com/upload/{accountId}/",
  "eventSourceUrl": "https://api.example.com/events?types={types}&closeafter={closeafter}&ping={ping}",
  "state": "state-1"
}|}
    max_calls

let path (req : Fetch.Middleware.request) =
  Fetch.Middleware.Url.path_and_query req.url

let request_body (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> ""
  | Fetch.String body -> body
  | Fetch.Stream _ -> Alcotest.fail "unexpected streaming request"

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec loop i =
    i + nl <= hl && (String.sub haystack i nl = needle || loop (i + 1))
  in
  loop 0

let with_io ?(max_calls = 16) reply f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server req =
    let response =
      match path req with
      | "/.well-known/jmap" -> session_json max_calls
      | "/jmap/api/" -> reply (request_body req)
      | _ -> Alcotest.fail "unexpected request path"
    in
    Fetch_mock.respond ~headers:json_headers response req
  in
  let clock = Eio.Stdenv.clock env in
  let transport =
    Jmap_eio.Transport.of_fetch ~clock (Fetch_mock.client server)
  in
  let io = Io.create ~sw ~transport env in
  let credentials =
    Model.{ url = well_known; scheme = Bearer; user = ""; secret = "TOKEN" }
  in
  Io.perform io (Model.Connect credentials) (function
    | Model.Connected _ -> ()
    | Model.Login_failed error -> Alcotest.failf "login failed: %s" error
    | _ -> Alcotest.fail "login produced the wrong response");
  f io

let id = Proto.Id.of_string_exn

let draft =
  Model.
    {
      answering = id "Moriginal";
      identity = id "Igood";
      from = "good@example.com";
      recipients = [ "reader@example.com" ];
      subject = "Hello";
      in_reply_to = [];
      references = [];
      text = "Body";
      drafts = id "Mdrafts";
      sent = id "Msent";
    }

let submission_response implicit =
  Printf.sprintf
    {|{"methodResponses":[
      ["Email/set",{"accountId":"acc1","newState":"s1","created":{"draft":{"id":"Mnew"}}},"c0"],
      ["EmailSubmission/set",{"accountId":"acc1","newState":"s2","created":{"send":{"id":"S1"}}},"c1"]%s
    ],"sessionState":"state-1"}|}
    implicit

let marked_answered =
  {|{"methodResponses":[["Email/set",{"accountId":"acc1","newState":"s3","updated":{"Moriginal":null}},"c0"]],"sessionState":"state-1"}|}

let sending ?max_calls reply =
  with_io ?max_calls reply @@ fun io ->
  let answer = ref None in
  Io.perform io (Model.Send draft) (fun message -> answer := Some message);
  Option.value !answer ~default:(Model.Send_failed "no response")

let send_result ?max_calls implicit =
  let round = ref 0 in
  let reply body =
    incr round;
    match max_calls with
    | Some 1 when !round = 1 ->
        {|{"methodResponses":[["Email/set",{"accountId":"acc1","newState":"s1","created":{"draft":{"id":"Mnew"}}},"c0"]],"sessionState":"state-1"}|}
    | Some 1 when !round = 2 ->
        {|{"methodResponses":[["EmailSubmission/set",{"accountId":"acc1","newState":"s2","created":{"send":{"id":"S1"}}},"c0"]],"sessionState":"state-1"}|}
    | _ when contains ~needle:"EmailSubmission/set" body ->
        submission_response implicit
    | _ -> marked_answered
  in
  sending ?max_calls reply

let warning = function
  | Model.Sent_with_warning warning -> warning
  | Model.Sent -> Alcotest.fail "expected a send warning"
  | Model.Send_failed failure ->
      Alcotest.failf "delivery reported failed: %s" failure
  | _ -> Alcotest.fail "send produced the wrong message"

let test_failed_filing_is_a_warning () =
  let result =
    send_result
      {|,
      ["Email/set",{"accountId":"acc1","newState":"s3","notUpdated":{"Mnew":{"type":"forbidden","description":"no Sent access"}}},"c1"]|}
  in
  Alcotest.(check bool)
    "filing failure retained" true
    (contains ~needle:"filing it failed" (warning result))

let test_missing_filing_is_a_warning () =
  Alcotest.(check bool)
    "missing filing retained" true
    (contains ~needle:"no filing outcome"
       (warning (send_result ~max_calls:1 "")))

let destroyed =
  {|{"methodResponses":[["Email/set",{"accountId":"acc1","newState":"s4","destroyed":["Mnew"]},"c0"]],"sessionState":"state-1"}|}

let test_refused_submission_destroys_the_draft () =
  let bodies = ref [] in
  let reply body =
    bodies := body :: !bodies;
    if contains ~needle:"EmailSubmission/set" body then
      {|{"methodResponses":[
      ["Email/set",{"accountId":"acc1","newState":"s1","created":{"draft":{"id":"Mnew"}}},"c0"],
      ["EmailSubmission/set",{"accountId":"acc1","newState":"s2","notCreated":{"send":{"type":"forbidden","description":"no submission"}}},"c1"]
    ],"sessionState":"state-1"}|}
    else destroyed
  in
  (match sending reply with
  | Model.Send_failed message ->
      Alcotest.(check bool)
        ("the refusal is reported: " ^ message)
        true
        (contains ~needle:"refused the change" message)
  | message -> Alcotest.failf "wrong message: %a" Model.pp_msg message);
  Alcotest.(check bool)
    "the draft is destroyed" true
    (List.exists (contains ~needle:{|"destroy":["Mnew"]|}) !bodies)

let test_submission_error_destroys_the_draft () =
  let bodies = ref [] in
  let reply body =
    bodies := body :: !bodies;
    if contains ~needle:"EmailSubmission/set" body then
      {|{"methodResponses":[
      ["Email/set",{"accountId":"acc1","newState":"s1","created":{"draft":{"id":"Mnew"}}},"c0"],
      ["error",{"type":"forbidden","description":"no submission"},"c1"]
    ],"sessionState":"state-1"}|}
    else destroyed
  in
  (match sending reply with
  | Model.Send_failed message ->
      Alcotest.(check bool)
        ("the method error is reported: " ^ message)
        true
        (contains ~needle:"forbidden" message)
  | message -> Alcotest.failf "wrong message: %a" Model.pp_msg message);
  Alcotest.(check bool)
    "the draft is destroyed" true
    (List.exists (contains ~needle:{|"destroy":["Mnew"]|}) !bodies)

let test_unreported_submission_does_not_invite_a_retry () =
  let bodies = ref [] in
  let reply body =
    bodies := body :: !bodies;
    {|{"methodResponses":[
      ["Email/set",{"accountId":"acc1","newState":"s1","created":{"draft":{"id":"Mnew"}}},"c0"],
      ["EmailSubmission/set",{"accountId":"acc1","newState":"s2"},"c1"]
    ],"sessionState":"state-1"}|}
  in
  match sending reply with
  | Model.Sent_with_warning warning ->
      Alcotest.(check bool)
        ("the outcome is unknown: " ^ warning)
        true
        (contains ~needle:"may have been submitted" warning);
      Alcotest.(check bool)
        "and the draft is left alone" false
        (List.exists (contains ~needle:{|"destroy"|}) !bodies)
  | message -> Alcotest.failf "wrong message: %a" Model.pp_msg message

let listing_response =
  {|{"methodResponses":[
      ["Email/query",{"accountId":"acc1","queryState":"q1","canCalculateChanges":false,"position":0,"ids":["E1"]},"c0"],
      ["Email/get",{"accountId":"acc1","state":"e1","list":[
        {"id":"E1","subject":"a\u001b[2Jb","preview":"p\tq",
         "from":[{"name":"Ada\nBad","email":"ada@example.com"}],
         "keywords":{},"hasAttachment":false}],"notFound":[]},"c1"]
    ],"sessionState":"state-1"}|}

let listed ?max_calls reply =
  with_io ?max_calls reply @@ fun io ->
  let answer = ref None in
  Io.perform io
    (Model.Load_messages (Model.Mailbox (id "Mbox")))
    (fun message -> answer := Some message);
  match !answer with
  | Some (Model.Messages (_, summaries)) -> summaries
  | Some message -> Alcotest.failf "wrong message: %a" Model.pp_msg message
  | None -> Alcotest.fail "the listing produced no response"

let test_listing_escapes_server_text () =
  let requests = ref 0 in
  let reply _ =
    incr requests;
    listing_response
  in
  match listed reply with
  | [ summary ] ->
      Alcotest.(check int) "one request" 1 !requests;
      Alcotest.(check string) "subject" "a\\x1B[2Jb" summary.subject;
      Alcotest.(check string) "preview" "p\\x09q" summary.preview;
      Alcotest.(check string) "sender" "Ada\\x0ABad" summary.sender
  | summaries -> Alcotest.failf "%d summaries" (List.length summaries)

let test_listing_falls_back_to_two_requests () =
  let query =
    {|{"methodResponses":[["Email/query",{"accountId":"acc1","queryState":"q1","canCalculateChanges":false,"position":0,"ids":["E1"]},"c0"]],"sessionState":"state-1"}|}
  in
  let got =
    {|{"methodResponses":[["Email/get",{"accountId":"acc1","state":"e1","list":[
        {"id":"E1","subject":"plain","keywords":{},"hasAttachment":false}],"notFound":[]},"c0"]],"sessionState":"state-1"}|}
  in
  let requests = ref 0 in
  let reply body =
    incr requests;
    if contains ~needle:"Email/query" body then query else got
  in
  match listed ~max_calls:1 reply with
  | [ summary ] ->
      Alcotest.(check int) "two requests" 2 !requests;
      Alcotest.(check string) "subject" "plain" summary.subject
  | summaries -> Alcotest.failf "%d summaries" (List.length summaries)

let test_selects_a_usable_identity () =
  let identity_response =
    {|{"methodResponses":[["Identity/get",{"accountId":"acc1","state":"i1","list":[
      {"id":"Ibad","email":""},{"id":"Igood","email":"good@example.com"}
    ],"notFound":[]},"c0"]],"sessionState":"state-1"}|}
  in
  with_io (fun _ -> identity_response) @@ fun io ->
  let answer = ref None in
  Io.perform io Model.Load_identity (fun message -> answer := Some message);
  match !answer with
  | Some (Model.Identity identity) ->
      Alcotest.(check string) "address" "good@example.com" identity.address;
      Alcotest.(check string)
        "identity id" "Igood"
        (Proto.Id.to_string identity.identity_id)
  | Some message -> Alcotest.failf "wrong response: %a" Model.pp_msg message
  | None -> Alcotest.fail "Identity/get produced no response"

let () =
  Alcotest.run "jmap-mosaic-io-release"
    [
      ( "io",
        [
          Alcotest.test_case "failed filing warns" `Quick
            test_failed_filing_is_a_warning;
          Alcotest.test_case "missing filing warns" `Quick
            test_missing_filing_is_a_warning;
          Alcotest.test_case "refused submission destroys the draft" `Quick
            test_refused_submission_destroys_the_draft;
          Alcotest.test_case "submission error destroys the draft" `Quick
            test_submission_error_destroys_the_draft;
          Alcotest.test_case "unreported submission warns" `Quick
            test_unreported_submission_does_not_invite_a_retry;
          Alcotest.test_case "listing escapes server text" `Quick
            test_listing_escapes_server_text;
          Alcotest.test_case "listing falls back to two requests" `Quick
            test_listing_falls_back_to_two_requests;
          Alcotest.test_case "select usable identity" `Quick
            test_selects_a_usable_identity;
        ] );
    ]
