(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Conformance tests for {!Jmap.Chain} and the Mailbox role vocabulary.

    Covers RFC 8620 Section 3.2 (several responses per method call id), Section
    3.6.2 (method errors), Section 3.7 (argument name collisions), Section 5.3
    (creation references) and Section 5.4 ([/copy] create maps), and RFC 8621
    Section 2.2 ([updatedProperties]), Section 2.3 ([sortAsTree]/[filterAsTree])
    and Section 4.5 ([collapseThreads]). *)

open Jmap.Proto

let encode jsont value = Jsont_bytesrw.encode_string' jsont value
let decode jsont s = Jsont_bytesrw.decode_string' jsont s
let json_string = Jsont.Json.string

let json_obj fields =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) fields)

let json_array = Jsont.Json.list
let json_null = Jsont.Json.null ()
let account = Id.of_string_exn "u33084183"

(* Encode the request built by [chain] and return it as a JSON string. *)
let request_json chain =
  let request =
    Jmap.Chain.build_request ~capabilities:[ "urn:ietf:params:jmap:mail" ] chain
  in
  match encode Request.jsont request with
  | Ok s -> s
  | Error e ->
      Alcotest.failf "request encode failed: %s" (Jsont.Error.to_string e)

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i =
    i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1))
  in
  go 0

let check_contains msg needle haystack =
  Alcotest.(check bool)
    (Printf.sprintf "%s: %s in %s" msg needle haystack)
    true
    (contains ~needle haystack)

(* {1 RFC 8620 Section 5.3 -- creation references} *)

let test_created_id_is_hash_prefixed () =
  let token = Id.creation "newbox" in
  Alcotest.(check string)
    "the map key carries no #" "newbox"
    (Id.to_string (Id.creation_id token));
  let cid = Id.creation_ref token in
  Alcotest.(check string) "#-prefixed" "#newbox" (Id.to_string cid);
  Alcotest.(check bool) "is a creation ref" true (Id.is_creation_ref cid);
  Alcotest.(check (option string))
    "creation id" (Some "newbox") (Id.to_creation_id cid);
  let plain = Id.of_string_exn "Mb1" in
  Alcotest.(check bool) "plain id is not a ref" false (Id.is_creation_ref plain);
  Alcotest.(check (option string))
    "plain id has no creation id" None (Id.to_creation_id plain)

let test_of_string_still_rejects_hash () =
  (* Section 1.2 alphabet is A-Za-z0-9_- ; '#' is not in it, and decoding a
     server response must keep rejecting it. *)
  (match Id.of_string "#newbox" with
  | Ok _ -> Alcotest.fail "Id.of_string accepted a creation reference"
  | Error _ -> ());
  (match decode Id.jsont {|"#newbox"|} with
  | Ok _ -> Alcotest.fail "Id.jsont decoded a creation reference"
  | Error _ -> ());
  (match Id.of_creation_id "not valid!" with
  | Ok _ -> Alcotest.fail "of_creation_id accepted an invalid creation id"
  | Error _ -> ());
  Alcotest.check_raises "of_creation_id_exn raises"
    (Invalid_argument
       "Invalid creation id: Invalid character ' ' in Id at position 3")
    (fun () -> ignore (Id.of_creation_id_exn "not valid!"))

let test_created_id_in_id_keyed_map () =
  (* mailboxIds is an Id[Boolean] map; a create may name a Mailbox created
     earlier in the same request. *)
  let mailbox_ids = [ (Id.creation_ref (Id.creation "newbox"), true) ] in
  match encode Json_map.id_to_bool mailbox_ids with
  | Error e -> Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e)
  | Ok s -> check_contains "mailboxIds key" {|"#newbox":true|} s

let test_created_id_in_destroy_and_update () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         email_set ~account_id:account
           ~update:
             [
               ( Id.creation_ref (Id.creation "draft1"),
                 Jmap.Proto.Patch.v [ Jmap.Proto.Email.Patch.set_keyword `Seen ]
               );
             ]
           ~destroy:(ids [ Id.creation_ref (Id.creation "x") ])
           ()
       in
       return ())
  in
  check_contains "destroy entry" {|["#x"]|} json;
  check_contains "update key" {|"#draft1"|} json

let mailbox_create_exn ~name =
  match Mailbox.create ~name () with
  | Ok m -> m
  | Error msg -> Alcotest.failf "Mailbox.create: %s" msg

let test_created_id_from_set_create () =
  (* The two-step create/reference pattern of Section 5.3: create a Mailbox,
     then create an Email whose mailboxIds names it by creation reference. *)
  let json =
    request_json
      (let open Jmap.Chain in
       let newbox = Id.creation "newbox" in
       let* _ =
         mailbox_set ~account_id:account
           ~create:[ (newbox, mailbox_create_exn ~name:"Todo") ]
           ()
       in
       let* _ =
         email_set ~account_id:account
           ~create:
             [
               ( Id.creation "draft1",
                 Email.v
                   ~mailbox_ids:(Email.of_mailboxes [ Id.creation_ref newbox ])
                   () );
             ]
           ()
       in
       return ())
  in
  (* RFC 8621 Section 2 types parentId and role T|null, so Mailbox.jsont
     always writes them. *)
  check_contains "the Mailbox create"
    {|"create":{"newbox":{"name":"Todo","parentId":null,"role":null}}|} json;
  check_contains "mailboxIds creation ref" {|"#newbox":true|} json

(* {1 Typed /set create objects} *)

(* The Email a create builder produces is encoded by [Email/set] with
   Email.jsont, so the bodyValues/textBody pair of RFC 8621 Section 4.6 is on
   the wire in the shape the record gives it. *)
let test_email_create_object () =
  let e =
    Email.create
      ~mailbox_ids:[ Id.of_string_exn "Mdrafts" ]
      ~keywords:[ `Draft ]
      ~from:[ Email_address.create ~name:"Me" "me@example.com" ]
      ~to_:[ Email_address.create "you@example.com" ]
      ~subject:"Hello" ~text_body:"Hello, world." ()
  in
  let json =
    match encode Email.jsont e with
    | Ok s -> s
    | Error err -> Alcotest.failf "encode: %s" (Jsont.Error.to_string err)
  in
  Alcotest.(check string)
    "create object"
    {|{"mailboxIds":{"Mdrafts":true},"keywords":{"$draft":true},"from":[{"name":"Me","email":"me@example.com"}],"to":[{"email":"you@example.com"}],"subject":"Hello","bodyValues":{"text":{"value":"Hello, world."}},"textBody":[{"partId":"text","type":"text/plain"}]}|}
    json

let test_email_create_alternative_bodies () =
  let e =
    Email.create
      ~mailbox_ids:[ Id.of_string_exn "Mdrafts" ]
      ~subject:"Both" ~text_body:"plain" ~html_body:"<p>rich</p>" ()
  in
  let json =
    match encode Email.jsont e with
    | Ok s -> s
    | Error err -> Alcotest.failf "encode: %s" (Jsont.Error.to_string err)
  in
  Alcotest.(check string)
    "both bodies"
    {|{"mailboxIds":{"Mdrafts":true},"subject":"Both","bodyValues":{"text":{"value":"plain"},"html":{"value":"<p>rich</p>"}},"textBody":[{"partId":"text","type":"text/plain"}],"htmlBody":[{"partId":"html","type":"text/html"}]}|}
    json

let test_email_create_rejects_bad_keyword () =
  (* RFC 8621 Section 4.1.1 keeps a space out of a keyword. *)
  Alcotest.(check bool)
    "a keyword with a space" true
    (try
       ignore
         (Email.create
            ~mailbox_ids:[ Id.of_string_exn "Mdrafts" ]
            ~keywords:[ `Custom "bad keyword" ]
            ());
       false
     with Invalid_argument _ -> true);
  Alcotest.(check bool)
    "an empty mailbox list" true
    (try
       ignore (Email.create ~mailbox_ids:[] ());
       false
     with Invalid_argument _ -> true)

let test_typed_set_creates () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         identity_set ~account_id:account
           ~create:
             [ (Id.creation "i1", Identity.v ~name:"Me" ~email:"me@x.tld" ()) ]
           ()
       in
       let* _ =
         email_submission_set ~account_id:account
           ~create:
             [
               ( Id.creation "s1",
                 Submission.create ~identity_id:(Id.of_string_exn "I1")
                   ~email_id:(Id.creation_ref (Id.creation "draft"))
                   () );
             ]
           ()
       in
       return ())
  in
  check_contains "the Identity create"
    {|"create":{"i1":{"name":"Me","email":"me@x.tld"}}|} json;
  check_contains "the EmailSubmission create"
    {|"create":{"s1":{"identityId":"I1","emailId":"#draft"}}|} json

(* {1 RFC 8621 Section 2 -- a Mailbox by role} *)

let test_mailbox_by_role () =
  let handle = ref None in
  let json =
    request_json
      (let open Jmap.Chain in
       let* h = mailbox_by_role ~account_id:account `Drafts in
       handle := Some h;
       return ())
  in
  check_contains "the role filter" {|"filter":{"role":"drafts"}|} json;
  check_contains "the get takes its ids by reference"
    {|"#ids":{"resultOf":"c0","name":"Mailbox/query","path":"/ids"}|} json;
  Alcotest.(check bool)
    "no properties argument asks for every property" false
    (contains ~needle:{|"properties"|} json);
  Alcotest.(check string)
    "the handle is the Mailbox/get" "Mailbox/get"
    (Jmap.Chain.method_name (Option.get !handle))

let test_mailbox_by_role_properties () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         mailbox_by_role ~account_id:account ~properties:[ `Id; `Total_emails ]
           `Inbox
       in
       return ())
  in
  check_contains "the given properties" {|"properties":["id","totalEmails"]|}
    json

(* {1 RFC 8620 Section 3.7 -- foo and #foo collisions} *)

let test_argument_collision () =
  let open Jmap.Chain in
  let clean =
    json_obj [ ("accountId", json_string "u1"); ("#ids", json_obj []) ]
  in
  Alcotest.(check bool)
    "no collision" true
    (Result.is_ok (check_arguments clean));
  let colliding = json_obj [ ("ids", json_array []); ("#ids", json_obj []) ] in
  Alcotest.(check bool)
    "collision detected" true
    (Result.is_error (check_arguments colliding));
  Alcotest.(check bool)
    "raw_invocation rejects it" true
    (try
       ignore
         (build_request ~capabilities:[]
            (raw_invocation ~name:"Foo/get" ~arguments:colliding));
       false
     with Invalid_argument _ -> true)

let test_mailbox_get_properties_collision () =
  let open Jmap.Chain in
  let ref_ =
    match
      Invocation.result_reference_of_strings ~result_of:"c0"
        ~name:"Mailbox/changes" ~path:"/updatedProperties"
    with
    | Ok r -> r
    | Error e -> Alcotest.failf "result reference: %s" (Jsont.Error.to_string e)
  in
  let rejects f =
    try
      ignore (build_request ~capabilities:[] (f ()));
      false
    with Invalid_argument _ -> true
  in
  Alcotest.(check bool)
    "properties + #properties rejected" true
    (rejects (fun () ->
         mailbox_get ~account_id:account ~properties:[ `Name ]
           ~properties_ref:ref_ ()));
  (* The raw escape hatch is the same argument, so it collides too. *)
  Alcotest.(check bool)
    "properties_raw + #properties rejected" true
    (rejects (fun () ->
         mailbox_get ~account_id:account ~properties_raw:[ "name" ]
           ~properties_ref:ref_ ()))

(* {1 RFC 8621 Section 2.3 -- sortAsTree / filterAsTree} *)

let test_mailbox_query_as_tree () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         mailbox_query ~account_id:account ~sort_as_tree:true
           ~filter_as_tree:false ()
       in
       return ())
  in
  check_contains "sortAsTree" {|"sortAsTree":true|} json;
  check_contains "filterAsTree" {|"filterAsTree":false|} json;
  (* Omitted by default: the server default is false, per Section 2.3. *)
  let bare =
    request_json
      (let open Jmap.Chain in
       let* _ = mailbox_query ~account_id:account () in
       return ())
  in
  Alcotest.(check bool)
    "absent when unset" false
    (contains ~needle:"sortAsTree" bare)

(* {1 RFC 8621 Section 4.5 -- Email/queryChanges collapseThreads} *)

let test_email_query_changes_collapse_threads () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         email_query_changes ~account_id:account ~since_query_state:"78540"
           ~collapse_threads:true ()
       in
       return ())
  in
  check_contains "collapseThreads" {|"collapseThreads":true|} json

(* {1 RFC 8621 Section 2.2 -- Mailbox/changes updatedProperties} *)

let mailbox_changes_response body =
  let chain_state = ref None in
  let handle =
    Jmap.Chain.build_request ~capabilities:[]
      (let open Jmap.Chain in
       let* h = mailbox_changes ~account_id:account ~since_state:"78540" () in
       chain_state := Some h;
       return h)
  in
  ignore handle;
  let h = Option.get !chain_state in
  match decode Response.jsont body with
  | Error e ->
      Alcotest.failf "response decode failed: %s" (Jsont.Error.to_string e)
  | Ok resp -> Jmap.Chain.parse h resp

let changes_body updated_properties =
  Printf.sprintf
    {|{"methodResponses":[["Mailbox/changes",{"accountId":"u33084183","oldState":"78541","newState":"78542","hasMoreChanges":false,%s"created":[],"updated":["MB23cfa8094c0f41e6"],"destroyed":[]},"c0"]],"sessionState":"75128aab4b1b"}|}
    updated_properties

let test_mailbox_changes_updated_properties () =
  let body =
    changes_body
      {|"updatedProperties":["totalEmails","unreadEmails","totalThreads","unreadThreads"],|}
  in
  match mailbox_changes_response body with
  | Error e ->
      Alcotest.failf "parse failed: %s" (Jmap.Chain.parse_error_to_string e)
  | Ok r ->
      Alcotest.(check (option (list string)))
        "updatedProperties"
        (Some [ "totalEmails"; "unreadEmails"; "totalThreads"; "unreadThreads" ])
        r.Jmap.Chain.updated_properties;
      Alcotest.(check string)
        "newState" "78542" r.Jmap.Chain.changes.Method.new_state

let test_mailbox_changes_updated_properties_null () =
  (* Section 2.2: "If the server is unable to tell if only counts have
     changed, it MUST just be null." An explicit null and an absent member
     must both decode. *)
  (match
     mailbox_changes_response (changes_body {|"updatedProperties":null,|})
   with
  | Error e ->
      Alcotest.failf "explicit null rejected: %s"
        (Jmap.Chain.parse_error_to_string e)
  | Ok r ->
      Alcotest.(check (option (list string)))
        "null" None r.Jmap.Chain.updated_properties);
  match mailbox_changes_response (changes_body "") with
  | Error e ->
      Alcotest.failf "absent member rejected: %s"
        (Jmap.Chain.parse_error_to_string e)
  | Ok r ->
      Alcotest.(check (option (list string)))
        "absent" None r.Jmap.Chain.updated_properties

let test_updated_properties_back_reference () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* ch = mailbox_changes ~account_id:account ~since_state:"78540" () in
       let* _ =
         mailbox_get ~account_id:account ~ids:(from_changes_updated ch)
           ~properties_ref:(from_changes_updated_properties ch)
           ()
       in
       return ())
  in
  check_contains "#properties back-reference" {|"#properties"|} json;
  check_contains "path" {|"/updatedProperties"|} json

(* {1 RFC 8621 Section 5.1 -- SearchSnippet/get} *)

let empty_condition : Email.Filter_condition.t =
  {
    in_mailbox = None;
    in_mailbox_other_than = None;
    before = None;
    after = None;
    min_size = None;
    max_size = None;
    all_in_thread_have_keyword = None;
    some_in_thread_have_keyword = None;
    none_in_thread_have_keyword = None;
    has_keyword = None;
    not_keyword = None;
    has_attachment = None;
    text = Some "foo";
    from = None;
    to_ = None;
    cc = None;
    bcc = None;
    subject = None;
    body = None;
    header = None;
  }

let test_search_snippet_get_response () =
  (* Section 5.1: the response has no "state" string and "notFound" is
     "Id[]|null", so the generic /get codec cannot decode it. *)
  let handle = ref None in
  ignore
    (Jmap.Chain.build_request ~capabilities:[]
       (let open Jmap.Chain in
        let* h =
          search_snippet_get ~account_id:account
            ~filter:(Filter.Condition empty_condition)
            ~email_ids:(ids [ Id.of_string_exn "M1"; Id.of_string_exn "M2" ])
            ()
        in
        handle := Some h;
        return ()));
  let h = Option.get !handle in
  let body =
    {|{"methodResponses":[["SearchSnippet/get",{"accountId":"u33084183","list":[{"emailId":"M1","subject":"The <mark>foo</mark> bar","preview":null}],"notFound":null},"c0"]],"sessionState":"75128aab4b1b"}|}
  in
  match decode Response.jsont body with
  | Error e ->
      Alcotest.failf "response decode failed: %s" (Jsont.Error.to_string e)
  | Ok resp -> (
      match Jmap.Chain.parse h resp with
      | Error e ->
          Alcotest.failf "snippet parse failed: %s"
            (Jmap.Chain.parse_error_to_string e)
      | Ok r ->
          Alcotest.(check string)
            "accountId" "u33084183"
            (Id.to_string r.Search_snippet.account_id);
          Alcotest.(check int)
            "one snippet" 1
            (List.length r.Search_snippet.list);
          Alcotest.(check bool)
            "notFound null" true
            (r.Search_snippet.not_found = None))

(* {1 RFC 8621 Section 2 -- Mailbox roles} *)

let test_role_to_string_total () =
  let all : Mailbox.role list =
    [
      `Inbox;
      `Sent;
      `Drafts;
      `Trash;
      `Junk;
      `Archive;
      `Flagged;
      `Important;
      `All;
      `Snoozed;
      `Scheduled;
      `Memos;
      `Other "receipts";
    ]
  in
  List.iter
    (fun role ->
      let s =
        try Mailbox.role_to_string role
        with e ->
          Alcotest.failf "role_to_string raised: %s" (Printexc.to_string e)
      in
      Alcotest.(check bool) "non-empty role name" true (String.length s > 0))
    all;
  (* Section 2: \Subscribed is a name attribute rather than a role. *)
  Alcotest.(check string)
    "subscribed" "subscribed"
    (Mailbox.role_to_string (`Other "subscribed"));
  Alcotest.(check bool)
    "subscribed is not a role" true
    (Mailbox.role_of_string "subscribed" = `Other "subscribed");
  (* Section 2: role values are IANA attribute names converted to lowercase. *)
  Alcotest.(check string)
    "inbox round trip" "inbox"
    (Mailbox.role_to_string (Mailbox.role_of_string "inbox"));
  Alcotest.(check string)
    "custom" "receipts"
    (Mailbox.role_to_string (Mailbox.role_of_string "receipts"))

(* {1 draft-ietf-mailmaint Section 4.2 -- mailbox attribute names} *)

let test_mailbox_attr_no_implied_backslash () =
  let module A = Mail_flag.Mailbox_attr in
  Alcotest.(check string) "Snoozed" "Snoozed" (A.to_string `Snoozed);
  Alcotest.(check string) "Scheduled" "Scheduled" (A.to_string `Scheduled);
  Alcotest.(check string) "Memos" "Memos" (A.to_string `Memos);
  (* RFC 6154 Section 2 attributes keep their backslash. *)
  Alcotest.(check string) "Drafts" "\\Drafts" (A.to_string `Drafts);
  Alcotest.(check bool)
    "Snoozed round trip" true
    (A.of_string (A.to_string `Snoozed) = `Snoozed)

(* {1 RFC 8620 Section 3.6.2 -- method errors} *)

(* A response that carries an "error" invocation for the call id of [h]. *)
let error_response ~type_ =
  Printf.sprintf
    {|{"methodResponses":[["error",{"type":"%s","description":"nope"},"c0"]],"sessionState":"s1"}|}
    type_

let query_handle () =
  let handle = ref None in
  ignore
    (Jmap.Chain.build_request ~capabilities:[]
       (let open Jmap.Chain in
        let* h = email_query ~account_id:account ~limit:1L () in
        handle := Some h;
        return ()));
  Option.get !handle

let response_of_string body =
  match decode Response.jsont body with
  | Ok resp -> resp
  | Error e ->
      Alcotest.failf "response decode failed: %s" (Jsont.Error.to_string e)

let test_method_error_is_found () =
  (* Section 3.6.2: "the response name is set to 'error'", sharing the method
     call id of the call that failed. *)
  let h = query_handle () in
  let resp = response_of_string (error_response ~type_:"unsupportedSort") in
  (match Jmap.Chain.method_error h resp with
  | None -> Alcotest.fail "method_error did not see the error response"
  | Some e ->
      Alcotest.(check string)
        "type" "unsupportedSort"
        (Error.Method_error.type_to_string e.Error.Method_error.type_);
      Alcotest.(check (option string))
        "description" (Some "nope") e.Error.Method_error.description);
  Alcotest.(check string)
    "printed with its description" "unsupportedSort (nope)"
    (match Jmap.Chain.method_error h resp with
    | Some e -> Error.Method_error.to_string e
    | None -> "")

let test_parse_reports_method_error () =
  (* An error is a well-formed answer, so parse must not report it as a
     decode failure. *)
  let h = query_handle () in
  let resp =
    response_of_string (error_response ~type_:"cannotCalculateChanges")
  in
  match Jmap.Chain.parse h resp with
  | Ok _ -> Alcotest.fail "parse decoded an error response as a query response"
  | Error (Jmap.Chain.Json_error e) ->
      Alcotest.failf "error response reported as JSON error: %s"
        (Jsont.Error.to_string e)
  | Error (Jmap.Chain.Method_error e) ->
      Alcotest.(check bool)
        "typed error" true
        (e.Error.Method_error.type_ = `Cannot_calculate_changes);
      Alcotest.(check string)
        "rendered" "cannotCalculateChanges (nope)"
        (Jmap.Chain.parse_error_to_string (Jmap.Chain.Method_error e))

let test_parse_reports_json_error () =
  let h = query_handle () in
  (* The right method name and call id, but the arguments of another method. *)
  let resp =
    response_of_string
      {|{"methodResponses":[["Email/query",{"accountId":"u33084183"},"c0"]],"sessionState":"s1"}|}
  in
  (match Jmap.Chain.parse h resp with
  | Ok _ -> Alcotest.fail "parse accepted a query response with no queryState"
  | Error (Jmap.Chain.Method_error _) ->
      Alcotest.fail "decode failure reported as a method error"
  | Error (Jmap.Chain.Json_error _) -> ());
  (* No response at all for the call id is a Json_error too. *)
  let empty =
    response_of_string {|{"methodResponses":[],"sessionState":"s1"}|}
  in
  Alcotest.(check bool)
    "no response is not a method error" true
    (Jmap.Chain.method_error h empty = None);
  match Jmap.Chain.parse h empty with
  | Error (Jmap.Chain.Json_error _) -> ()
  | _ -> Alcotest.fail "a missing response should be a Json_error"

let test_parse_exn_raises_parse_error () =
  let h = query_handle () in
  let resp = response_of_string (error_response ~type_:"unsupportedSort") in
  (match Jmap.Chain.parse_exn h resp with
  | _ -> Alcotest.fail "parse_exn decoded an error response"
  | exception Jmap.Chain.Parse_error (Jmap.Chain.Method_error e) ->
      Alcotest.(check string)
        "the error is carried" "unsupportedSort"
        (Error.Method_error.type_to_string e.Error.Method_error.type_);
      Alcotest.(check bool)
        "and printed by Printexc" true
        (contains ~needle:"unsupportedSort"
           (Printexc.to_string
              (Jmap.Chain.Parse_error (Jmap.Chain.Method_error e))))
  | exception e ->
      Alcotest.failf "unexpected exception: %s" (Printexc.to_string e));
  let empty =
    response_of_string {|{"methodResponses":[],"sessionState":"s1"}|}
  in
  match Jmap.Chain.parse_exn h empty with
  | _ -> Alcotest.fail "parse_exn decoded a missing response"
  | exception Jmap.Chain.Parse_error (Jmap.Chain.Json_error e) ->
      Alcotest.(check bool)
        "the method is named" true
        (contains ~needle:"Email/query" (Jsont.Error.to_string e))
  | exception e ->
      Alcotest.failf "unexpected exception: %s" (Printexc.to_string e)

(* {1 RFC 8620 Section 3.2 -- several responses per method call id} *)

let submission_set_handle () =
  let handle = ref None in
  ignore
    (Jmap.Chain.build_request ~capabilities:[]
       (let open Jmap.Chain in
        let k1 = Id.creation "k1" in
        let* h =
          email_submission_set ~account_id:account
            ~create:
              [
                ( k1,
                  Submission.create ~identity_id:(Id.of_string_exn "I1")
                    ~email_id:(Id.of_string_exn "M1") () );
              ]
            ~on_success_update_email:
              [
                ( Id.creation_ref k1,
                  Patch.v [ Email.Patch.remove_keyword `Draft ] );
              ]
            ()
        in
        handle := Some h;
        return ()));
  Option.get !handle

let test_several_responses_share_a_call_id () =
  (* Section 3.2: "A method may also return more than one response [...] all
     of the responses have the same method call id."  RFC 8621 Section 7.5
     makes onSuccessUpdateEmail send an implicit Email/set response, which
     here precedes the response of the call that was made. *)
  let h = submission_set_handle () in
  let body =
    {|{"methodResponses":[["Email/set",{"accountId":"u33084183","newState":"1"},"c0"],["EmailSubmission/set",{"accountId":"u33084183","newState":"2"},"c0"]],"sessionState":"s1"}|}
  in
  match Jmap.Chain.parse h (response_of_string body) with
  | Error e ->
      Alcotest.failf "parse failed: %s" (Jmap.Chain.parse_error_to_string e)
  | Ok r ->
      Alcotest.(check string)
        "the EmailSubmission/set response" "2" r.Method.new_state

let test_error_among_several_responses () =
  (* Section 3.6.2: the error shares the method call id, and may not come
     first. *)
  let h = submission_set_handle () in
  let body =
    {|{"methodResponses":[["Email/set",{"accountId":"u33084183","newState":"1"},"c0"],["error",{"type":"stateMismatch"},"c0"]],"sessionState":"s1"}|}
  in
  match Jmap.Chain.method_error h (response_of_string body) with
  | None -> Alcotest.fail "the error response was not found"
  | Some e ->
      Alcotest.(check string)
        "type" "stateMismatch"
        (Error.Method_error.type_to_string e.Error.Method_error.type_)

let test_unrelated_response_is_not_taken () =
  (* A response with the call id but another name is not the answer to this
     call, so there is no response to decode. *)
  let h = submission_set_handle () in
  let body =
    {|{"methodResponses":[["Email/set",{"accountId":"u33084183","newState":"1"},"c0"]],"sessionState":"s1"}|}
  in
  match Jmap.Chain.parse h (response_of_string body) with
  | Ok _ ->
      Alcotest.fail "an Email/set response answered an EmailSubmission/set"
  | Error (Jmap.Chain.Method_error _) ->
      Alcotest.fail "a missing response reported as a method error"
  | Error (Jmap.Chain.Json_error _) -> ()

(* {1 RFC 8620 Section 1.3 -- integers a client sends} *)

let raises_invalid_argument what f =
  Alcotest.(check bool)
    what true
    (try
       ignore (Jmap.Chain.build_request ~capabilities:[] (f ()));
       false
     with Invalid_argument _ -> true)

let test_out_of_range_integer_raises () =
  (* An "Int" a client builds outside the range Section 1.3 allows cannot be
     encoded, and a builder that cannot encode a value raises rather than
     sending a request that means something else. *)
  let open Jmap.Chain in
  raises_invalid_argument "negative limit" (fun () ->
      email_query ~account_id:account ~limit:(-1L) ());
  raises_invalid_argument "limit above 2^53-1" (fun () ->
      email_query ~account_id:account ~limit:9007199254740992L ());
  raises_invalid_argument "position below -2^53+1" (fun () ->
      email_query ~account_id:account ~position:(-9007199254740992L) ());
  raises_invalid_argument "negative maxChanges" (fun () ->
      email_changes ~account_id:account ~since_state:"1" ~max_changes:(-1L) ());
  raises_invalid_argument "zero maxChanges" (fun () ->
      email_changes ~account_id:account ~since_state:"1" ~max_changes:0L ());
  raises_invalid_argument "negative maxBodyValueBytes" (fun () ->
      email_get ~account_id:account ~max_body_value_bytes:(-1L) ());
  let json =
    request_json
      (let* _ = email_query ~account_id:account ~limit:50L ~position:10L () in
       return ())
  in
  check_contains "in-range limit" {|"limit":50|} json;
  check_contains "in-range position" {|"position":10|} json

(* {1 RFC 8620 Section 5.4 -- Email/copy create maps} *)

let test_email_copy_create () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         email_copy ~from_account_id:(Id.of_string_exn "u1") ~account_id:account
           ~create:
             [
               ( Id.creation "k1",
                 Id.of_string_exn "M1",
                 [ ("keywords", json_obj [ ("$seen", Jsont.Json.bool true) ]) ]
               );
             ]
           ~on_success_destroy_original:true ()
       in
       return ())
  in
  (* Section 5.4: "The id of the record to copy" is a property of the object,
     and the map is keyed by creation id. *)
  check_contains "creation id key and source id" {|"create":{"k1":{"id":"M1"|}
    json;
  check_contains "override" {|"keywords":{"$seen":true}|} json;
  check_contains "onSuccessDestroyOriginal" {|"onSuccessDestroyOriginal":true|}
    json

let test_email_copy_rejects_id_override () =
  let open Jmap.Chain in
  raises_invalid_argument "the source id cannot be overridden" (fun () ->
      email_copy ~from_account_id:(Id.of_string_exn "u1") ~account_id:account
        ~create:
          [
            ( Id.creation "k1",
              Id.of_string_exn "M1",
              [ ("id", json_string "M2") ] );
          ]
        ())

let test_email_copy_validates_arguments () =
  let open Jmap.Chain in
  raises_invalid_argument "equal accounts" (fun () ->
      email_copy ~from_account_id:account ~account_id:account
        ~create:[ (Id.creation "k1", Id.of_string_exn "M1", []) ]
        ());
  raises_invalid_argument "empty create" (fun () ->
      email_copy ~from_account_id:(Id.of_string_exn "u1") ~account_id:account
        ~create:[] ());
  raises_invalid_argument "forbidden override" (fun () ->
      email_copy ~from_account_id:(Id.of_string_exn "u1") ~account_id:account
        ~create:
          [
            ( Id.creation "k1",
              Id.of_string_exn "M1",
              [ ("subject", json_string "changed") ] );
          ]
        ())

let test_core_method_builders () =
  let subscription =
    Result.get_ok
      (Push.create_args ~device_client_id:"device"
         ~url:"https://example.com/push" ())
  in
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         blob_copy ~from_account_id:(Id.of_string_exn "u1") ~account_id:account
           ~blob_ids:(ids [ Id.of_string_exn "B1" ])
           ()
       in
       let* _ = push_subscription_get ~ids:(ids [ Id.of_string_exn "P1" ]) () in
       let* _ =
         push_subscription_set
           ~create:[ (Id.creation "push1", subscription) ]
           ()
       in
       return ())
  in
  check_contains "Blob/copy" {|["Blob/copy"|} json;
  check_contains "blob ids" {|"blobIds":["B1"]|} json;
  check_contains "PushSubscription/get" {|["PushSubscription/get"|} json;
  check_contains "PushSubscription/set" {|["PushSubscription/set"|} json;
  check_contains "push creation id" {|"create":{"push1":|} json

(* RFC 8620 Section 5.3: "the client refers to the new record using its
   creation id prefixed with a '#'", so the key of a create map is the bare
   creation id and only an argument carries the '#'. *)
let test_typed_creation_keys () =
  let json =
    request_json
      (let open Jmap.Chain in
       let newbox = Id.creation "newbox" in
       let* _ =
         mailbox_set ~account_id:account
           ~create:[ (newbox, mailbox_create_exn ~name:"Todo") ]
           ()
       in
       let* _ =
         email_import ~account_id:account
           ~emails:
             [
               ( Id.creation "imp1",
                 Email.Import.email ~blob_id:(Id.of_string_exn "B1")
                   ~mailbox_ids:[ Id.creation_ref newbox ]
                   () );
             ]
           ()
       in
       return ())
  in
  check_contains "the Mailbox/set key" {|"create":{"newbox":|} json;
  check_contains "the Email/import key" {|"emails":{"imp1":|} json;
  check_contains "the reference keeps its #" {|"#newbox":true|} json

(* {1 RFC 8620 Section 4 -- Core/echo} *)

let test_echo () =
  let args = json_obj [ ("hello", json_string "world") ] in
  let handle = ref None in
  let json =
    request_json
      (let open Jmap.Chain in
       let* h = echo args in
       handle := Some h;
       return ())
  in
  check_contains "invocation" {|["Core/echo",{"hello":"world"},"c0"]|} json;
  let body =
    {|{"methodResponses":[["Core/echo",{"hello":"world"},"c0"]],"sessionState":"s1"}|}
  in
  match Jmap.Chain.parse (Option.get !handle) (response_of_string body) with
  | Error e ->
      Alcotest.failf "echo parse failed: %s"
        (Jmap.Chain.parse_error_to_string e)
  | Ok (Jsont.Object (mems, _)) ->
      Alcotest.(check bool)
        "arguments returned unchanged" true
        (match Jsont.Json.find_mem "hello" mems with
        | Some (_, Jsont.String ("world", _)) -> true
        | _ -> false)
  | Ok _ -> Alcotest.fail "the Core/echo response is not an object"

let test_raw_arguments_are_objects_with_unique_members () =
  let open Jmap.Chain in
  raises_invalid_argument "non-object arguments" (fun () ->
      raw_invocation ~name:"Core/echo" ~arguments:(json_string "bad"));
  raises_invalid_argument "duplicate arguments" (fun () ->
      raw_invocation ~name:"Core/echo"
        ~arguments:
          (json_obj
             [ ("same", json_string "one"); ("same", json_string "two") ]))

(* {1 RFC 8621 Sections 4.1 and 2 -- typed property lists} *)

let test_typed_properties () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         email_get ~account_id:account
           ~ids:(ids [ Id.of_string_exn "M1" ])
           ~properties:
             [
               `Id;
               `Received_at;
               `Subject;
               `Header (Email_header.message_ids `In_reply_to);
               `Header (Email_header.raw ~all:true "Received");
             ]
           ~properties_raw:[ "x-vendor:thing" ]
           ~body_properties:
             [
               `Part_id;
               `Blob_id;
               `Part_headers;
               `Header (Email_header.raw "Content-Type");
             ]
           ()
       in
       return ())
  in
  (* Section 4.1.1 and 4.1.3 wire names, and Section 4.1.2 header forms. *)
  check_contains "properties"
    {|"properties":["id","receivedAt","subject","header:In-Reply-To:asMessageIds","header:Received:all","x-vendor:thing"]|}
    json;
  (* Section 4.1.4: [headers] is the wire name of the part header list. *)
  check_contains "bodyProperties"
    {|"bodyProperties":["partId","blobId","headers","header:Content-Type"]|}
    json

let test_typed_properties_other_types () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         mailbox_get ~account_id:account ~properties:[ `Id; `Total_emails ] ()
       in
       let* _ = thread_get ~account_id:account ~properties:[ `Email_ids ] () in
       let* _ =
         email_submission_get ~account_id:account
           ~properties:[ `Email_id; `Undo_status ]
           ()
       in
       return ())
  in
  check_contains "Mailbox" {|"properties":["id","totalEmails"]|} json;
  check_contains "Thread" {|"properties":["emailIds"]|} json;
  check_contains "EmailSubmission" {|"properties":["emailId","undoStatus"]|}
    json

let test_empty_filter_conditions () =
  (* RFC 8621 Sections 2.3 and 7.3: a condition with no properties set is
     true for every object, and must encode as an empty object. *)
  let encode_filter jsont c =
    match encode jsont (Filter.Condition c) with
    | Ok s -> s
    | Error e ->
        Alcotest.failf "filter encode failed: %s" (Jsont.Error.to_string e)
  in
  Alcotest.(check string)
    "Mailbox" "{}"
    (encode_filter Mailbox.filter_jsont Mailbox.Filter_condition.empty);
  Alcotest.(check string)
    "EmailSubmission" "{}"
    (encode_filter Submission.filter_jsont Submission.Filter_condition.empty);
  Alcotest.(check string)
    "Email" "{}"
    (encode_filter Email.filter_jsont Email.Filter_condition.empty);
  (* And record update syntax builds the real thing from it. *)
  let json =
    request_json
      (let open Jmap.Chain in
       let* _ =
         mailbox_query ~account_id:account
           ~filter:
             (Filter.Condition
                {
                  Mailbox.Filter_condition.empty with
                  role = Some (Some `Inbox);
                })
           ()
       in
       return ())
  in
  check_contains "role filter" {|"filter":{"role":"inbox"}|} json

(* {1 RFC 8620 Section 3.7 -- typed back-references} *)

let test_from_get_field_typed () =
  let json =
    request_json
      (let open Jmap.Chain in
       let* heads =
         email_get ~account_id:account
           ~ids:(ids [ Id.of_string_exn "M1" ])
           ~properties:[ `Id; `Thread_id ] ()
       in
       let* threads =
         thread_get ~account_id:account
           ~ids:(from_get_field heads Thread_id)
           ~properties:[ `Email_ids ] ()
       in
       let* _ =
         email_get ~account_id:account
           ~ids:(from_get_field threads Email_ids)
           ()
       in
       let* subs =
         email_submission_get ~account_id:account
           ~properties:[ `Id; `Email_id; `Identity_id; `Thread_id ]
           ()
       in
       let* _ =
         email_get ~account_id:account
           ~ids:(from_get_field subs Submission_email_id)
           ()
       in
       let* _ =
         identity_get ~account_id:account
           ~ids:(from_get_field subs Submission_identity_id)
           ()
       in
       let* _ =
         thread_get ~account_id:account
           ~ids:(from_get_field subs Submission_thread_id)
           ()
       in
       return ())
  in
  check_contains "threadId"
    {|"#ids":{"resultOf":"c0","name":"Email/get","path":"/list/*/threadId"}|}
    json;
  check_contains "emailIds"
    {|"#ids":{"resultOf":"c1","name":"Thread/get","path":"/list/*/emailIds"}|}
    json;
  check_contains "the EmailSubmission emailId"
    {|"resultOf":"c3","name":"EmailSubmission/get","path":"/list/*/emailId"|}
    json;
  check_contains "the EmailSubmission identityId"
    {|"resultOf":"c3","name":"EmailSubmission/get","path":"/list/*/identityId"|}
    json;
  check_contains "the EmailSubmission threadId"
    {|"resultOf":"c3","name":"EmailSubmission/get","path":"/list/*/threadId"|}
    json

let test_from_get_field_unrequested_property () =
  (* Section 3.7: a reference into a property the /get did not ask for
     resolves to nothing and is answered with invalidResultReference, so the
     builder refuses it. *)
  let open Jmap.Chain in
  raises_invalid_argument "a property the Email/get left out" (fun () ->
      let* heads =
        email_get ~account_id:account ~properties:[ `Id; `Subject ] ()
      in
      let* _ =
        thread_get ~account_id:account ~ids:(from_get_field heads Thread_id) ()
      in
      return ());
  (* No properties argument asks for all of them, so nothing is checked. *)
  let json =
    request_json
      (let* heads = email_get ~account_id:account () in
       let* _ =
         thread_get ~account_id:account ~ids:(from_get_field heads Thread_id) ()
       in
       return ())
  in
  check_contains "the reference is built" {|"path":"/list/*/threadId"|} json

let test_from_get_field_raw () =
  let open Jmap.Chain in
  let json =
    request_json
      (let* boxes =
         mailbox_get ~account_id:account ~properties:[ `Id ]
           ~properties_raw:[ "x-vendor/linked~Id" ] ()
       in
       let* _ =
         mailbox_get ~account_id:account
           ~ids:(from_get_field_raw boxes "x-vendor/linked~Id")
           ()
       in
       return ())
  in
  check_contains "the escaped raw wire name"
    {|"path":"/list/*/x-vendor~1linked~0Id"|} json;
  (* Section 5.1: "The id property of the object is always returned, even if
     not explicitly requested", so a reference to it needs no properties
     argument naming it. *)
  let json =
    request_json
      (let* boxes = mailbox_get ~account_id:account ~properties:[ `Name ] () in
       let* _ =
         mailbox_get ~account_id:account ~ids:(from_get_field_raw boxes "id") ()
       in
       return ())
  in
  check_contains "a reference to id" {|"path":"/list/*/id"|} json;
  raises_invalid_argument "a raw property the Mailbox/get left out" (fun () ->
      let* boxes = mailbox_get ~account_id:account ~properties:[ `Id ] () in
      let* _ =
        mailbox_get ~account_id:account
          ~ids:(from_get_field_raw boxes "x-vendor:linkedId")
          ()
      in
      return ())

(* {1 RFC 8620 Section 5.1 -- state only reads} *)

let state_reads_body =
  {|{"methodResponses":[["Email/get",{"accountId":"u33084183","state":"e1","list":[],"notFound":[]},"c0"],["Mailbox/get",{"accountId":"u33084183","state":"m1","list":[],"notFound":[]},"c1"],["Thread/get",{"accountId":"u33084183","state":"t1","list":[],"notFound":[]},"c2"]],"sessionState":"s1"}|}

let test_state_reads () =
  let handles = ref None in
  let json =
    request_json
      (let open Jmap.Chain in
       let* e = email_state ~account_id:account in
       let* m = mailbox_state ~account_id:account in
       let* t = thread_state ~account_id:account in
       handles := Some (e, m, t);
       return ())
  in
  check_contains "no ids are asked for" {|"ids":[]|} json;
  check_contains "the Email/get call" {|"Email/get"|} json;
  check_contains "the Mailbox/get call" {|"Mailbox/get"|} json;
  check_contains "the Thread/get call" {|"Thread/get"|} json;
  let e, m, t = Option.get !handles in
  let resp = response_of_string state_reads_body in
  Alcotest.(check string) "Email state" "e1" (Jmap.Chain.parse_exn e resp);
  Alcotest.(check string) "Mailbox state" "m1" (Jmap.Chain.parse_exn m resp);
  Alcotest.(check string) "Thread state" "t1" (Jmap.Chain.parse_exn t resp)

(* {1 RFC 8620 Section 3.6.2 -- a call that may answer an error} *)

let attempt_handle () =
  let handle = ref None in
  ignore
    (Jmap.Chain.build_request ~capabilities:[]
       (let open Jmap.Chain in
        let* h = email_query ~account_id:account ~limit:1L () in
        handle := Some (attempt h);
        return ()));
  Option.get !handle

let query_response_body =
  {|{"methodResponses":[["Email/query",{"accountId":"u33084183","queryState":"q1","canCalculateChanges":false,"position":0,"ids":["M1"]},"c0"]],"sessionState":"s1"}|}

let empty_response_body = {|{"methodResponses":[],"sessionState":"s1"}|}

let test_attempt_call () =
  let _, h =
    Jmap.Chain.build ~capabilities:[]
      (Jmap.Chain.attempt_call (Jmap.Chain.email_query ~account_id:account ()))
  in
  match
    Jmap.Chain.parse h
      (response_of_string (error_response ~type_:"cannotCalculateChanges"))
  with
  | Ok (Error _) -> ()
  | Ok (Ok _) -> Alcotest.fail "an error response decoded as a response"
  | Error e ->
      Alcotest.failf "attempt_call reported %s"
        (Jmap.Chain.parse_error_to_string e)

let test_attempt () =
  let h = attempt_handle () in
  (match
     Jmap.Chain.parse h
       (response_of_string (error_response ~type_:"cannotCalculateChanges"))
   with
  | Ok (Error e) ->
      Alcotest.(check string)
        "the method error is the value" "cannotCalculateChanges"
        (Error.Method_error.type_to_string e.Error.Method_error.type_)
  | Ok (Ok _) -> Alcotest.fail "an error response decoded as a response"
  | Error e ->
      Alcotest.failf "attempt reported %s" (Jmap.Chain.parse_error_to_string e));
  (match Jmap.Chain.parse h (response_of_string query_response_body) with
  | Ok (Ok r) -> Alcotest.(check int) "one id" 1 (List.length r.Method.ids)
  | Ok (Error _) -> Alcotest.fail "a response reported as a method error"
  | Error e ->
      Alcotest.failf "attempt reported %s" (Jmap.Chain.parse_error_to_string e));
  match Jmap.Chain.parse h (response_of_string empty_response_body) with
  | Error (Jmap.Chain.Json_error _) -> ()
  | _ -> Alcotest.fail "a missing response should still be a Json_error"

(* {1 Reading several handles at once} *)

let three_handles () =
  Jmap.Chain.build_handles ~capabilities:[]
    (let open Jmap.Chain in
     let* q = email_query ~account_id:account ~limit:1L () in
     let* g = email_get ~account_id:account ~ids:(from_query q) () in
     let+ s = email_state ~account_id:account in
     Handles.[ q; g; s ])

let three_responses ~middle =
  Printf.sprintf
    {|{"methodResponses":[["Email/query",{"accountId":"u33084183","queryState":"q1","canCalculateChanges":false,"position":0,"ids":["M1"]},"c0"],%s,["Email/get",{"accountId":"u33084183","state":"e1","list":[],"notFound":[]},"c2"]],"sessionState":"s1"}|}
    middle

let test_parse_all () =
  let handles = three_handles () in
  let body =
    three_responses
      ~middle:
        {|["Email/get",{"accountId":"u33084183","state":"e1","list":[{"id":"M1"}],"notFound":[]},"c1"]|}
  in
  match Jmap.Chain.parse_all handles (response_of_string body) with
  | Error e ->
      Alcotest.failf "parse_all failed: %s" (Jmap.Chain.parse_error_to_string e)
  | Ok Jmap.Chain.Results.[ q; g; s ] ->
      Alcotest.(check int) "one queried id" 1 (List.length q.Method.ids);
      Alcotest.(check int) "one Email" 1 (List.length g.Method.list);
      Alcotest.(check string) "the state read" "e1" s

let test_parse_all_reports_a_method_error () =
  let handles = three_handles () in
  let body = three_responses ~middle:{|["error",{"type":"serverFail"},"c1"]|} in
  match Jmap.Chain.parse_all handles (response_of_string body) with
  | Ok _ -> Alcotest.fail "parse_all decoded an error response"
  | Error (Jmap.Chain.Json_error e) ->
      Alcotest.failf "an error response reported as a JSON error: %s"
        (Jsont.Error.to_string e)
  | Error (Jmap.Chain.Method_error e) ->
      Alcotest.(check string)
        "the failing call's error" "serverFail"
        (Error.Method_error.type_to_string e.Error.Method_error.type_)

let test_parse_all_exn_raises () =
  let handles = three_handles () in
  match
    Jmap.Chain.parse_all_exn handles (response_of_string empty_response_body)
  with
  | _ -> Alcotest.fail "parse_all_exn decoded a missing response"
  | exception Jmap.Chain.Parse_error (Jmap.Chain.Json_error _) -> ()
  | exception e ->
      Alcotest.failf "unexpected exception: %s" (Printexc.to_string e)

(* {1 A method this library has no builder for} *)

let hello_jsont =
  Jsont.Object.map ~kind:"Hello" Fun.id
  |> Jsont.Object.mem "hello" Jsont.string ~enc:Fun.id
  |> Jsont.Object.finish

let test_invocation_with_a_codec () =
  let handle = ref None in
  let json =
    request_json
      (let open Jmap.Chain in
       let* h =
         invocation ~name:"Vendor/hello"
           ~arguments:(json_obj [ ("hello", json_string "world") ])
           hello_jsont
       in
       handle := Some h;
       return ())
  in
  check_contains "the invocation" {|["Vendor/hello",{"hello":"world"},"c0"]|}
    json;
  let body =
    {|{"methodResponses":[["Vendor/hello",{"hello":"there"},"c0"]],"sessionState":"s1"}|}
  in
  (match Jmap.Chain.parse (Option.get !handle) (response_of_string body) with
  | Ok v -> Alcotest.(check string) "decoded with the codec" "there" v
  | Error e ->
      Alcotest.failf "invocation parse failed: %s"
        (Jmap.Chain.parse_error_to_string e));
  Alcotest.(check bool)
    "colliding arguments are refused" true
    (try
       ignore
         (Jmap.Chain.build_request ~capabilities:[]
            (Jmap.Chain.invocation ~name:"Vendor/hello"
               ~arguments:
                 (json_obj [ ("ids", json_array []); ("#ids", json_obj []) ])
               hello_jsont));
       false
     with Invalid_argument _ -> true)

let () =
  Alcotest.run "Chain"
    [
      ( "creation references (RFC 8620 Section 5.3)",
        [
          Alcotest.test_case "created_id is #-prefixed" `Quick
            test_created_id_is_hash_prefixed;
          Alcotest.test_case "of_string stays strict" `Quick
            test_of_string_still_rejects_hash;
          Alcotest.test_case "id-keyed map" `Quick
            test_created_id_in_id_keyed_map;
          Alcotest.test_case "destroy and update" `Quick
            test_created_id_in_destroy_and_update;
          Alcotest.test_case "create then reference" `Quick
            test_created_id_from_set_create;
        ] );
      ( "typed /set creates (RFC 8621 Section 4.6)",
        [
          Alcotest.test_case "Email.create builds the Email" `Quick
            test_email_create_object;
          Alcotest.test_case "text and html bodies" `Quick
            test_email_create_alternative_bodies;
          Alcotest.test_case "Email.create validates keywords" `Quick
            test_email_create_rejects_bad_keyword;
          Alcotest.test_case "Identity and EmailSubmission" `Quick
            test_typed_set_creates;
        ] );
      ( "a Mailbox by role (RFC 8621 Section 2)",
        [
          Alcotest.test_case "query then get" `Quick test_mailbox_by_role;
          Alcotest.test_case "given properties" `Quick
            test_mailbox_by_role_properties;
        ] );
      ( "argument collisions (RFC 8620 Section 3.7)",
        [
          Alcotest.test_case "foo and #foo" `Quick test_argument_collision;
          Alcotest.test_case "Mailbox/get properties" `Quick
            test_mailbox_get_properties_collision;
        ] );
      ( "Mailbox/query (RFC 8621 Section 2.3)",
        [
          Alcotest.test_case "sortAsTree and filterAsTree" `Quick
            test_mailbox_query_as_tree;
        ] );
      ( "Mailbox/changes (RFC 8621 Section 2.2)",
        [
          Alcotest.test_case "updatedProperties" `Quick
            test_mailbox_changes_updated_properties;
          Alcotest.test_case "null updatedProperties" `Quick
            test_mailbox_changes_updated_properties_null;
          Alcotest.test_case "back-reference" `Quick
            test_updated_properties_back_reference;
        ] );
      ( "Email/queryChanges (RFC 8621 Section 4.5)",
        [
          Alcotest.test_case "collapseThreads" `Quick
            test_email_query_changes_collapse_threads;
        ] );
      ( "SearchSnippet/get (RFC 8621 Section 5.1)",
        [
          Alcotest.test_case "spec-shaped response" `Quick
            test_search_snippet_get_response;
        ] );
      ( "method errors (RFC 8620 Section 3.6.2)",
        [
          Alcotest.test_case "method_error finds it" `Quick
            test_method_error_is_found;
          Alcotest.test_case "parse reports Method_error" `Quick
            test_parse_reports_method_error;
          Alcotest.test_case "parse reports Json_error" `Quick
            test_parse_reports_json_error;
          Alcotest.test_case "parse_exn raises Parse_error" `Quick
            test_parse_exn_raises_parse_error;
        ] );
      ( "several responses per call id (RFC 8620 Section 3.2)",
        [
          Alcotest.test_case "the response of the call that was made" `Quick
            test_several_responses_share_a_call_id;
          Alcotest.test_case "an error that is not first" `Quick
            test_error_among_several_responses;
          Alcotest.test_case "another method's response is not taken" `Quick
            test_unrelated_response_is_not_taken;
        ] );
      ( "integers a client sends (RFC 8620 Section 1.3)",
        [
          Alcotest.test_case "out of range raises" `Quick
            test_out_of_range_integer_raises;
        ] );
      ( "Email/copy (RFC 8620 Section 5.4)",
        [
          Alcotest.test_case "create map shape" `Quick test_email_copy_create;
          Alcotest.test_case "the source id cannot be overridden" `Quick
            test_email_copy_rejects_id_override;
          Alcotest.test_case "arguments are valid" `Quick
            test_email_copy_validates_arguments;
        ] );
      ( "core methods (RFC 8620 Sections 6 and 7)",
        [
          Alcotest.test_case "Blob/copy and PushSubscription" `Quick
            test_core_method_builders;
        ] );
      ( "Core/echo (RFC 8620 Section 4)",
        [
          Alcotest.test_case "arguments come back" `Quick test_echo;
          Alcotest.test_case "a codec of the caller's" `Quick
            test_invocation_with_a_codec;
          Alcotest.test_case "arguments are objects with unique members" `Quick
            test_raw_arguments_are_objects_with_unique_members;
        ] );
      ( "typed creation ids (RFC 8620 Section 5.3)",
        [
          Alcotest.test_case "a create map key carries no #" `Quick
            test_typed_creation_keys;
        ] );
      ( "typed back-references (RFC 8620 Section 3.7)",
        [
          Alcotest.test_case "the id properties" `Quick
            test_from_get_field_typed;
          Alcotest.test_case "an unrequested property" `Quick
            test_from_get_field_unrequested_property;
          Alcotest.test_case "a raw property name" `Quick
            test_from_get_field_raw;
        ] );
      ( "state only reads (RFC 8620 Section 5.1)",
        [
          Alcotest.test_case "Email, Mailbox and Thread" `Quick test_state_reads;
        ] );
      ( "a call that may answer an error (RFC 8620 Section 3.6.2)",
        [
          Alcotest.test_case "attempt" `Quick test_attempt;
          Alcotest.test_case "attempt_call" `Quick test_attempt_call;
        ] );
      ( "reading several handles at once",
        [
          Alcotest.test_case "parse_all" `Quick test_parse_all;
          Alcotest.test_case "a method error among them" `Quick
            test_parse_all_reports_a_method_error;
          Alcotest.test_case "parse_all_exn raises" `Quick
            test_parse_all_exn_raises;
        ] );
      ( "typed property lists (RFC 8621 Sections 2, 3, 4.1 and 7)",
        [
          Alcotest.test_case "Email/get properties" `Quick test_typed_properties;
          Alcotest.test_case "Mailbox, Thread, EmailSubmission" `Quick
            test_typed_properties_other_types;
          Alcotest.test_case "empty filter conditions" `Quick
            test_empty_filter_conditions;
        ] );
      ( "roles (RFC 8621 Section 2)",
        [
          Alcotest.test_case "to_string is total" `Quick
            test_role_to_string_total;
        ] );
      ( "mailbox attributes (draft-ietf-mailmaint Section 4.2)",
        [
          Alcotest.test_case "no implied backslash" `Quick
            test_mailbox_attr_no_implied_backslash;
        ] );
    ]
