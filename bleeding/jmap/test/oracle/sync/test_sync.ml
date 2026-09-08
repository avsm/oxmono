(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Oracle tests for the synchronisation and search surface: /changes
   (RFC 8620 Section 5.2), /queryChanges (Section 5.6), Email/query sorting
   (RFC 8621 Section 4.4.2), SearchSnippet/get (RFC 8621 Section 5) and
   Thread/get (RFC 8621 Section 3). *)

open Jmap.Proto
module H = Oracle_harness
module Chain = Jmap.Chain
module Results = Jmap.Chain.Results
module Sync = Jmap_eio.Sync

let capabilities = H.capabilities
let ids_mem id l = List.exists (Id.equal id) l
let ids_str l = String.concat " " (List.map Id.to_string l)
let email_state t = H.call t (Chain.email_state ~account_id:t.H.account_id)

(* Follow hasMoreChanges to the end (RFC 8620 Section 5.2) and accumulate. *)
let rec drain_email_changes t ~since_state ~fuel (created, updated, destroyed) =
  let c =
    H.call t
      (Chain.email_changes ~account_id:t.H.account_id ~since_state
         ~max_changes:64L ())
  in
  let acc =
    (created @ c.created, updated @ c.updated, destroyed @ c.destroyed)
  in
  if c.has_more_changes && fuel > 0 then
    drain_email_changes t ~since_state:c.new_state ~fuel:(fuel - 1) acc
  else (c.new_state, acc)

(* Email/changes: a delivery shows up in "created". *)
let email_changes_created t =
  let before = email_state t in
  let id, _subject = H.deliver_and_wait t () in
  let _state, (created, _updated, destroyed) =
    drain_email_changes t ~since_state:before ~fuel:20 ([], [], [])
  in
  Alcotest.(check bool)
    (Printf.sprintf "created contains %s (got %s)" (Id.to_string id)
       (ids_str created))
    true (ids_mem id created);
  Alcotest.(check bool) "not destroyed" false (ids_mem id destroyed)

(* Email/changes from the newState of a drained run reports nothing more. *)
let email_changes_idempotent t =
  let before = email_state t in
  let id, _ = H.deliver_and_wait t () in
  let state, _ =
    drain_email_changes t ~since_state:before ~fuel:20 ([], [], [])
  in
  (* Other oracle suites may deliver into or destroy from the same account
     concurrently. Only the change already consumed must not be reported a
     second time, and RFC 8620 Section 5.2 lets the server answer
     cannotCalculateChanges once a state has aged out, which Cyrus does quickly
     under concurrent destroys: hence the {!Chain.attempt}. *)
  match
    H.call t
      Chain.(
        let+ h =
          email_changes ~account_id:t.H.account_id ~since_state:state
            ~max_changes:64L ()
        in
        attempt h)
  with
  | Error { Error.Method_error.type_ = `Cannot_calculate_changes; _ } -> ()
  | Error e ->
      Alcotest.failf "Email/changes: %s"
        (Error.Method_error.type_to_string e.Error.Method_error.type_)
  | Ok c ->
      Alcotest.(check bool)
        "delivery not reported twice" false (List.mem id c.created);
      Alcotest.(check string) "oldState echoed back" state c.old_state

(* Mailbox/changes: after a delivery the Inbox counts change, and RFC 8621
   Section 2.2's updatedProperties may name exactly which counts. The same
   request feeds updatedProperties back into Mailbox/get as "#properties". *)
let mailbox_changes_updated_properties t =
  let inbox = H.mailbox_with_role t `Inbox in
  let inbox_id = Option.get inbox.id in
  let before = H.call t (Chain.mailbox_state ~account_id:t.H.account_id) in
  let total_before = Option.value inbox.total_emails ~default:0L in
  let _id, _ = H.deliver_and_wait t () in
  let Results.[ ch; g ] =
    H.run_all t
      Chain.(
        let* ch =
          mailbox_changes ~account_id:t.H.account_id ~since_state:before
            ~max_changes:64L ()
        in
        let+ g =
          mailbox_get ~account_id:t.H.account_id ~ids:(from_changes_updated ch)
            ~properties_ref:(from_changes_updated_properties ch)
            ()
        in
        Handles.[ ch; g ])
  in
  Alcotest.(check bool)
    (Printf.sprintf "inbox %s in updated (got %s)" (Id.to_string inbox_id)
       (ids_str ch.changes.updated))
    true
    (ids_mem inbox_id ch.changes.updated);
  (* "String[]|null": either shape must decode. When it is an array it may only
     name the four count properties. *)
  (match ch.updated_properties with
  | None -> ()
  | Some props ->
      List.iter
        (fun p ->
          Alcotest.(check bool)
            (Printf.sprintf "updatedProperties member %S is a count property" p)
            true
            (List.mem p
               [
                 "totalEmails"; "unreadEmails"; "totalThreads"; "unreadThreads";
               ]))
        props);
  (* The Mailbox/get half of the same request must have resolved. *)
  match
    List.find_opt
      (fun (m : Mailbox.t) ->
        match m.id with Some i -> Id.equal i inbox_id | None -> false)
      g.list
  with
  | None -> Alcotest.failf "Mailbox/get did not return the inbox"
  | Some mb -> (
      match mb.total_emails with
      | None ->
          (* Only legal when updatedProperties restricted the property set. *)
          Alcotest.(check bool)
            "totalEmails omitted only if not requested" true
            (match ch.updated_properties with
            | Some props -> not (List.mem "totalEmails" props)
            | None -> false)
      | Some total ->
          (* Concurrent oracle suites destroy their own messages in the same
             inbox, so the count need not grow; the delivery just made keeps
             it at least one. *)
          Alcotest.(check bool)
            (Printf.sprintf "inbox totalEmails %Ld (was %Ld) is positive" total
               total_before)
            true (total >= 1L))

(* Email/queryChanges: a delivery is "added" at an index, and a destroy is
   "removed" (RFC 8620 Section 5.6). The filter picks out exactly the message
   this test delivers, so the deltas are unambiguous. *)
let email_query_changes_added_removed t =
  let subject, raw = H.message () in
  let filter = Email.filter ~subject () in
  let sort = [ Email.sort ~ascending:false `Received_at ] in
  let q0 =
    H.call t
      (Chain.email_query ~account_id:t.H.account_id ~filter ~sort
         ~calculate_total:true ~collapse_threads:false ())
  in
  Alcotest.(check int)
    "nothing matches the fresh subject yet" 0 (List.length q0.ids);
  if not q0.can_calculate_changes then Alcotest.skip ()
  else begin
    H.deliver t raw;
    let id = H.wait_for_email t ~subject () in
    let qc =
      H.call t
        (Chain.email_query_changes ~account_id:t.H.account_id
           ~since_query_state:q0.query_state ~filter ~sort ~max_changes:64L
           ~collapse_threads:false ())
    in
    Alcotest.(check string)
      "oldQueryState echoed" q0.query_state qc.old_query_state;
    Alcotest.(check bool)
      (Printf.sprintf "added contains %s" (Id.to_string id))
      true
      (List.exists (fun (a : Filter.added_item) -> Id.equal a.id id) qc.added);
    Alcotest.(check bool)
      "added at index 0" true
      (List.for_all (fun (a : Filter.added_item) -> a.index = 0L) qc.added);
    (* Now destroy it and ask again from the state we just reached. *)
    let s =
      H.call t
        (Chain.email_set ~account_id:t.H.account_id ~destroy:(Chain.ids [ id ])
           ())
    in
    Alcotest.(check bool)
      (Printf.sprintf "destroyed %s" (Id.to_string id))
      true
      (match s.destroyed with Some l -> ids_mem id l | None -> false);
    let qc2 =
      H.call t
        (Chain.email_query_changes ~account_id:t.H.account_id
           ~since_query_state:qc.new_query_state ~filter ~sort ~max_changes:64L
           ~collapse_threads:false ())
    in
    Alcotest.(check bool)
      (Printf.sprintf "removed contains %s (got %s)" (Id.to_string id)
         (ids_str qc2.removed))
      true (ids_mem id qc2.removed);
    Alcotest.(check int)
      "nothing added by the destroy" 0 (List.length qc2.added)
  end

(* SearchSnippet/get (RFC 8621 Section 5.1): no state string, and notFound is
   "Id[]|null" - Cyrus sends the null, which must decode as [None]. *)
let search_snippet_not_found_null t =
  let word = "quicksilver" in
  let subject, raw = H.message ~body:("A body mentioning " ^ word ^ ".\n") () in
  H.deliver t raw;
  let id = H.wait_for_email t ~subject () in
  let filter = Email.filter ~text:word () in
  let s =
    H.call t
      (Chain.search_snippet_get ~account_id:t.H.account_id ~filter
         ~email_ids:(Chain.ids [ id ]) ())
  in
  Alcotest.(check bool)
    "the requested id was found" true
    (match s.not_found with None -> true | Some l -> not (ids_mem id l));
  Alcotest.(check int) "one snippet" 1 (List.length s.list);
  match s.list with
  | [ sn ] ->
      Alcotest.(check string)
        "snippet is for the right email" (Id.to_string id)
        (Id.to_string sn.email_id);
      (* The server MAY be unable to produce snippets, in which case both are
         null; when it can, the match is wrapped in <mark> tags. *)
      let marked s =
        Option.fold ~none:false ~some:(fun v -> String.length v > 0) s
      in
      Alcotest.(check bool)
        "subject or preview present, or both null" true
        (marked sn.subject || marked sn.preview
        || (sn.subject = None && sn.preview = None))
  | _ -> ()

(* Email/query sorting on a keyword needs the Comparator's extra "keyword"
   property (RFC 8621 Section 4.4.2). Check both the wire round trip and that
   the server accepts it. *)
let keyword_comparator t =
  let c =
    Filter.comparator ~is_ascending:true ~keyword:(Keyword.to_string `Seen)
      "hasKeyword"
  in
  let json =
    match Jmap_eio.Codec.encode Filter.comparator_jsont c with
    | Ok s -> s
    | Error e ->
        Alcotest.failf "encoding the comparator: %s" (Jsont.Error.to_string e)
  in
  Alcotest.(check bool)
    (Printf.sprintf "keyword is on the wire: %s" json)
    true
    (String.length json > 0
    &&
    let needle = "\"keyword\":\"$seen\"" in
    let rec find i =
      i + String.length needle <= String.length json
      && (String.sub json i (String.length needle) = needle || find (i + 1))
    in
    find 0);
  (match Jmap_eio.Codec.decode Filter.comparator_jsont json with
  | Error e ->
      Alcotest.failf "decoding the comparator: %s" (Jsont.Error.to_string e)
  | Ok back ->
      Alcotest.(check (option string))
        "keyword survives the round trip"
        (Some (Keyword.to_string `Seen))
        back.keyword;
      Alcotest.(check string)
        "property survives the round trip" "hasKeyword" back.property);
  (* And the live query. A server may answer unsupportedSort; that is legal, so
     only a hard failure is a test failure. *)
  let _id, _ = H.deliver_and_wait t () in
  let inbox_id = Option.get (H.mailbox_with_role t `Inbox).id in
  let filter = Email.filter ~in_mailbox:inbox_id () in
  let sort = [ c; Email.sort ~ascending:false `Received_at ] in
  match
    H.call t
      Chain.(
        let+ h =
          email_query ~account_id:t.H.account_id ~filter ~sort ~limit:20L
            ~calculate_total:true ()
        in
        attempt h)
  with
  | Error e ->
      Alcotest.(check string)
        "only unsupportedSort is an acceptable refusal" "unsupportedSort"
        (Error.Method_error.type_to_string e.Error.Method_error.type_)
  | Ok q ->
      Alcotest.(check bool)
        "the keyword sort returned results" true (q.ids <> [])

(* Thread/get for a delivered message, via the RFC 8620 Section 3.7 chain:
   query -> get threadId -> Thread/get -> get every message in the thread. *)
let thread_of_delivered t =
  let email_id, subject = H.deliver_and_wait t () in
  let Results.[ threads; messages ] =
    H.run_all t
      Chain.(
        let* e1 =
          email_get ~account_id:t.H.account_id ~ids:(Chain.ids [ email_id ])
            ~properties:[ `Thread_id ] ()
        in
        let* th =
          thread_get ~account_id:t.H.account_id
            ~ids:(from_get_field e1 Thread_id)
            ~properties:[ `Id; `Email_ids ] ()
        in
        let+ e2 =
          email_get ~account_id:t.H.account_id
            ~ids:(from_get_field th Email_ids)
            ~properties:[ `Id; `Subject; `Thread_id ]
            ()
        in
        Handles.[ th; e2 ])
  in
  (match threads.list with
  | [ th ] ->
      Alcotest.(check bool) "thread has an id" true (th.id <> None);
      Alcotest.(check bool)
        (Printf.sprintf "thread emailIds contains %s" (Id.to_string email_id))
        true
        (match th.email_ids with Some l -> ids_mem email_id l | None -> false)
  | l -> Alcotest.failf "expected one thread, got %d" (List.length l));
  Alcotest.(check bool)
    "the delivered message is in the thread's messages" true
    (List.exists (fun (e : Email.t) -> e.subject = Some subject) messages.list)

(* {1 Jmap_eio.Sync against the oracle}

   Sync turns the three loops of RFC 8620 - paging a query (Section 5.5),
   draining /changes (Section 5.2) and batching a /get (Section 5.1) - into
   one call each. These check them against a live server, where the limits
   and state strings are the server's rather than a mock's. *)

let sync_fail e = Alcotest.failf "sync: %s" (Sync.error_to_string e)

(* RFC 8620 Section 5.5: paging with position/limit, following the limit the
   server reports rather than the one asked for. Three deliveries guarantee
   at least two pages at a page size of two. *)
let sync_pages_inbox t =
  let inbox_id = Option.get (H.mailbox_with_role t `Inbox).id in
  let delivered = List.init 3 (fun _ -> fst (H.deliver_and_wait t ())) in
  let filter = Email.filter ~in_mailbox:inbox_id () in
  let sort = [ Email.sort ~ascending:false `Received_at ] in
  let query ~position ~limit =
    Chain.email_query ~account_id:t.H.account_id ~filter ~sort ~position ~limit
      ~collapse_threads:false ()
  in
  (* Bounded: another suite's inbox may hold thousands of messages. *)
  let pages =
    Sync.pages t.H.client ~capabilities ~page_size:2L query
    |> Seq.take 30
    |> Seq.map (function Ok ids -> ids | Error e -> sync_fail e)
    |> List.of_seq
  in
  let ids = List.concat pages in
  Alcotest.(check bool)
    (Printf.sprintf "at least the three deliveries (got %d ids in %d pages)"
       (List.length ids) (List.length pages))
    true
    (List.length ids >= 3);
  (* Every page but the last is full, and no id is served twice: that is what
     paging by the server's limit buys. *)
  let unique = List.sort_uniq Id.compare ids in
  Alcotest.(check int)
    (Printf.sprintf "ids are unique across pages (%s)" (ids_str ids))
    (List.length ids) (List.length unique);
  List.iteri
    (fun i page ->
      if i < List.length pages - 1 then
        Alcotest.(check int)
          "a page before the last one is full" 2 (List.length page))
    pages;
  (* Newest first, so the three just delivered are near the front. *)
  List.iter
    (fun id ->
      Alcotest.(check bool)
        (Printf.sprintf "%s is in the walk" (Id.to_string id))
        true (ids_mem id ids))
    delivered

(* RFC 8620 Section 5.2: the drain from a recorded state reports the delivery
   made since, with every round folded together. *)
let sync_changes_since t =
  let before = email_state t in
  let id, _ = H.deliver_and_wait t () in
  match
    Sync.email_changes t.H.client ~capabilities ~account_id:t.H.account_id
      ~since:before ~max_changes:8L ()
  with
  | Error e -> sync_fail e
  | Ok `Cannot_calculate_changes ->
      (* Legal, and Cyrus does answer it once a state has aged out under the
         concurrent destroys of the other suites. *)
      Alcotest.skip ()
  | Ok (`Changes c) ->
      Alcotest.(check bool)
        (Printf.sprintf "created contains %s (got %s)" (Id.to_string id)
           (ids_str c.created))
        true (ids_mem id c.created);
      Alcotest.(check bool) "not also destroyed" false (ids_mem id c.destroyed);
      Alcotest.(check bool) "not also updated" false (ids_mem id c.updated);
      Alcotest.(check bool)
        "a new state to resume from" true
        (c.new_state <> "" && c.new_state <> before)

(* RFC 8620 Section 5.1: a /get is batched at maxObjectsInGet; ~batch:2 forces
   the split whatever the server allows, and every id must still come back. *)
let sync_get_all_batched t =
  let delivered = List.init 3 (fun _ -> fst (H.deliver_and_wait t ())) in
  match
    Sync.get_all t.H.client ~capabilities ~batch:2L delivered (fun ~ids ->
        Chain.email_get ~account_id:t.H.account_id ~ids:(Chain.ids ids)
          ~properties:[ `Id; `Subject ] ())
  with
  | Error e -> sync_fail e
  | Ok (emails, not_found) -> (
      Alcotest.(check (list string))
        "nothing missing" []
        (List.map Id.to_string not_found);
      Alcotest.(check int) "every id fetched" 3 (List.length emails);
      let got = List.filter_map Email.id emails in
      List.iter
        (fun id ->
          Alcotest.(check bool)
            (Printf.sprintf "%s came back" (Id.to_string id))
            true (ids_mem id got))
        delivered;
      (* The batches are concatenated in input order, so the id of the last
         batch is last. *)
      match List.rev got with
      | last :: _ ->
          Alcotest.(check string)
            "input order across batches"
            (Id.to_string (List.nth delivered 2))
            (Id.to_string last)
      | [] -> Alcotest.fail "no emails")

let () =
  H.run "oracle-sync"
    [
      ( "changes",
        [
          H.test_case "Email/changes reports the delivery" email_changes_created;
          H.test_case "Email/changes settles at newState"
            email_changes_idempotent;
          H.test_case "Mailbox/changes updatedProperties"
            mailbox_changes_updated_properties;
        ] );
      ( "queryChanges",
        [
          H.test_case "Email/queryChanges added then removed"
            email_query_changes_added_removed;
        ] );
      ( "search",
        [
          H.test_case "SearchSnippet/get notFound null"
            search_snippet_not_found_null;
          H.test_case "Email/query keyword comparator" keyword_comparator;
        ] );
      ( "threads",
        [ H.test_case "Thread/get for a delivered message" thread_of_delivered ]
      );
      ( "sync",
        [
          H.test_case "Sync.pages walks the inbox" sync_pages_inbox;
          H.test_case "Sync.email_changes since a state" sync_changes_since;
          H.test_case "Sync.get_all batches by two" sync_get_all_batched;
        ] );
    ]
