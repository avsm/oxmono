(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Live coverage for changes and query methods absent from other suites. *)

open Jmap.Proto
module H = Oracle_harness
module Chain = Jmap.Chain
module Client = Jmap_eio.Client

let call t chain = Client.call t.H.client ~capabilities:H.capabilities chain

let cannot_calculate = function
  | Error
      (Client.Method_error
         { Error.Method_error.type_ = `Cannot_calculate_changes; _ }) ->
      true
  | _ -> false

let fail error = Alcotest.failf "%s" (Client.error_to_string error)

let thread_changes t =
  let state = H.call t (Chain.thread_state ~account_id:t.H.account_id) in
  match
    call t
      (Chain.thread_changes ~account_id:t.H.account_id ~since_state:state ())
  with
  | Ok changes -> Alcotest.(check string) "oldState" state changes.old_state
  | result when cannot_calculate result -> ()
  | Error error -> fail error

let query_changes name query changes t =
  match call t query with
  | Error error -> fail error
  | Ok query_response ->
      if query_response.Method.can_calculate_changes then
        match call t (changes query_response.query_state) with
        | Ok response ->
            Alcotest.(check string)
              (name ^ " oldQueryState") query_response.query_state
              response.Method.old_query_state
        | result when cannot_calculate result -> ()
        | Error error -> fail error
      else
        Fmt.epr "%s: canCalculateChanges=false; incremental path skipped.@."
          name

let mailbox_query_changes t =
  query_changes "Mailbox/queryChanges"
    (Chain.mailbox_query ~account_id:t.H.account_id ())
    (fun since_query_state ->
      Chain.mailbox_query_changes ~account_id:t.H.account_id ~since_query_state
        ())
    t

let submission_query_and_changes t =
  query_changes "EmailSubmission/queryChanges"
    (Chain.email_submission_query ~account_id:t.H.account_id ())
    (fun since_query_state ->
      Chain.email_submission_query_changes ~account_id:t.H.account_id
        ~since_query_state ())
    t

let () =
  H.run "jmap oracle release coverage"
    [
      ( "methods",
        [
          H.test_case "Thread/changes" thread_changes;
          H.test_case "Mailbox/queryChanges" mailbox_query_changes;
          H.test_case "EmailSubmission/query and queryChanges"
            submission_query_and_changes;
        ] );
    ]
