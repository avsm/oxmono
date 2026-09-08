(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Identity (RFC 8621 Section 6) and EmailSubmission (Section 7) against the
   oracle: the whole send path, from the Identity the message is sent as, via
   the draft it is composed in, to the message the recipient receives. *)

open Jmap.Proto
module H = Oracle_harness
module Client = Jmap_eio.Client
module Chain = Jmap.Chain
module Results = Jmap.Chain.Results

(* {1 Building the records a /set creates}

   A [/set] "create" object is a partial object of the type being created,
   which the builders of {!Jmap.Proto} make and validate: every property is
   absent unless given, so a create names exactly what the client may set. *)

let address ?name email = { Email_address.name; email }

(* RFC 8621 Section 4.6: a draft is an Email created with the [$draft]
   keyword in the Drafts Mailbox. *)
let draft_email ~mailbox ~from ~to_ ~subject ~body =
  Email.v
    ~mailbox_ids:[ (mailbox, true) ]
    ~keywords:[ (`Draft, true) ]
    ~from:[ address from ]
    ~to_:[ address to_ ]
    ~subject
    ~body_structure:(Email_body.Part.v ~part_id:"b1" ~type_:"text/plain" ())
    ~body_values:[ ("b1", Email_body.Value.v body) ]
    ()

(* {1 Account helpers} *)

let mailbox_id t role =
  match (H.mailbox_with_role t role).id with
  | Some id -> id
  | None ->
      Alcotest.failf "mailbox with role %s has no id"
        (Mailbox.role_to_string role)

let primary_identity t =
  let r = H.call t (Chain.identity_get ~account_id:t.H.account_id ()) in
  match List.filter_map Identity.id r.list with
  | id :: _ -> id
  | [] -> Alcotest.fail "Identity/get returned no identity with an id"

let destroy_emails t ?client ~account_id ids =
  if ids <> [] then
    let r =
      H.call ?client t (Chain.email_set ~account_id ~destroy:(Chain.ids ids) ())
    in
    if r.not_destroyed <> None then
      Alcotest.failf "could not destroy %d test messages" (List.length ids)

(* A second login against the same server, to check what the recipient sees. *)
let recipient_client t =
  let user = if t.H.user = "user2" then "user3" else "user2" in
  let domain =
    match String.index_opt t.H.address '@' with
    | Some i ->
        String.sub t.H.address (i + 1) (String.length t.H.address - i - 1)
    | None -> "example.com"
  in
  let password =
    match Sys.getenv_opt "JMAP_ORACLE_PASSWORD" with
    | Some p when p <> "" -> p
    | _ -> "x"
  in
  let auth = Jmap_eio.Auth.basic ~user ~password in
  match H.connect_with ~sw:t.H.sw ~auth t.H.env with
  | Error e ->
      Alcotest.failf "cannot log in as %s: %s" user (Client.error_to_string e)
  | Ok client ->
      let account_id =
        match
          Session.primary_account_for Capability.mail (Client.session client)
        with
        | Some id -> id
        | None -> Alcotest.failf "%s has no primary mail account" user
      in
      (client, account_id, user ^ "@" ^ domain)

let wait_for t ?(timeout = 30.0) f =
  let clock = Eio.Stdenv.clock t.H.env in
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    match f () with
    | Some v -> Some v
    | None ->
        if Eio.Time.now clock > deadline then None
        else (
          Eio.Time.sleep clock 0.25;
          loop ())
  in
  loop ()

(* {1 Identity, RFC 8621 Section 6} *)

(* Identity/get is a standard /get: "The 'ids' argument may be null to fetch
   all at once" (Section 6.1). Every account can send, so the list is never
   empty, and an unknown id lands in notFound. *)
let identity_get t =
  let Results.[ all; unknown ] =
    H.run_all t
      Chain.(
        let* all = identity_get ~account_id:t.H.account_id () in
        let+ some =
          identity_get ~account_id:t.H.account_id
            ~ids:(Chain.ids [ Id.of_string_exn "no-such-identity" ])
            ~properties_raw:[ Identity.property_to_string `Email ]
            ()
        in
        Handles.[ all; some ])
  in
  Alcotest.(check bool)
    "the account has at least one identity" true (all.list <> []);
  List.iter
    (fun (i : Identity.t) ->
      Alcotest.(check bool) "every identity has an id" true (i.id <> None);
      (* RFC 8621 Section 6: "email" is immutable and always present, though
         the oracle sends it as the empty string. *)
      Alcotest.(check bool) "every identity has an email" true (i.email <> None))
    all.list;
  Alcotest.(check int)
    "unknown identity id is notFound" 1
    (List.length unknown.not_found);
  Alcotest.(check int) "and returns no object" 0 (List.length unknown.list)

(* Identity/changes from the current state reports nothing new
   (Section 6.2). *)
let identity_changes t =
  let r = H.call t (Chain.identity_get ~account_id:t.H.account_id ()) in
  let c =
    H.call t
      (Chain.identity_changes ~account_id:t.H.account_id ~since_state:r.state ())
  in
  Alcotest.(check string)
    "no changes since the state just read" r.state c.new_state;
  Alcotest.(check bool) "nothing created" true (c.created = []);
  Alcotest.(check bool) "nothing destroyed" true (c.destroyed = [])

(* Identity/set (Section 6.3) is a standard /set, but a server may not
   implement it at all: Cyrus answers "unknownMethod" for the whole method.
   The lifecycle runs create, update and destroy in one request and accepts
   either outcome, so the test states what the oracle actually supports
   rather than what it ought to. *)
let identity_lifecycle t =
  let name = H.unique "Oracle Identity" in
  let create =
    Identity.v ~name ~email:t.H.address ~text_signature:"-- \noracle" ()
  in
  let i1 = Id.creation "i1" in
  match
    H.call t
      Chain.(
        let+ h =
          identity_set ~account_id:t.H.account_id ~create:[ (i1, create) ] ()
        in
        attempt h)
  with
  | Error { Error.Method_error.type_ = `Unknown_method; _ } ->
      (* No Identity/set: the account's identities are fixed. *)
      Fmt.epr "Identity/set is unsupported by this oracle.@."
  | Error error ->
      Alcotest.failf "Identity/set failed: %a" Error.Method_error.pp error
  | Ok r -> (
      match Method.created r i1 with
      | Some created ->
          let iid =
            match created.id with
            | Some id -> id
            | None -> Alcotest.fail "Identity/set created object has no id"
          in
          (* Update the name, then destroy: RFC 8621 Section 6 makes "email"
             immutable but "name" freely settable. *)
          let patch =
            Patch.v [ Patch.set_field "name" (Jsont.Json.string "renamed") ]
          in
          let Results.[ up; down ] =
            H.run_all t
              Chain.(
                let* up =
                  identity_set ~account_id:t.H.account_id
                    ~update:[ (iid, patch) ]
                    ()
                in
                let+ down =
                  identity_set ~account_id:t.H.account_id
                    ~destroy:(Chain.id iid) ()
                in
                Handles.[ up; down ])
          in
          Alcotest.(check bool) "rename succeeded" true (up.not_updated = None);
          Alcotest.(check bool)
            "destroy succeeded" true
            (down.destroyed = Some [ iid ])
      | None ->
          (* A conformant server may still refuse the create, e.g. with
             "forbiddenFrom" (Section 6.3). *)
          Alcotest.(check bool)
            "create was refused with a SetError" true (r.not_created <> None))

(* {1 EmailSubmission, RFC 8621 Section 7} *)

(* The full send path of Section 7.5.1: save a draft, submit it, and let the
   implicit Email/set move the message from Drafts to Sent and drop the
   $draft keyword. Then check the recipient's mailbox. *)
let send_email t =
  let drafts = mailbox_id t `Drafts and sent = mailbox_id t `Sent in
  let identity = primary_identity t in
  let rcpt_client, rcpt_account, rcpt_address = recipient_client t in
  let subject = H.unique "oracle-send" in
  let draft =
    draft_email ~mailbox:drafts ~from:t.H.address ~to_:rcpt_address ~subject
      ~body:"Sent by the ocaml-jmap oracle suite.\n"
  in
  (* Section 7: with no envelope the server derives it from the header
     fields; passing one exercises the Envelope codec instead. *)
  let draft1 = Id.creation "draft1" and sub1 = Id.creation "sub1" in
  let submission =
    Submission.create ~identity_id:identity ~email_id:(Id.creation_ref draft1)
      ~envelope:
        (Submission.Envelope.v
           ~mail_from:(Submission.Address.v t.H.address)
           ~rcpt_to:[ Submission.Address.v rcpt_address ])
      ()
  in
  let on_success =
    Patch.v
      [
        Email.Patch.remove_from_mailbox drafts;
        Email.Patch.add_to_mailbox sent;
        Email.Patch.remove_keyword `Draft;
      ]
  in
  let Results.[ draft_r; sub_r ] =
    H.run_all t
      Chain.(
        let* draft_h =
          email_set ~account_id:t.H.account_id ~create:[ (draft1, draft) ] ()
        in
        let+ sub_h =
          email_submission_set ~account_id:t.H.account_id
            ~create:[ (sub1, submission) ]
            ~on_success_update_email:[ (Id.creation_ref sub1, on_success) ]
            ()
        in
        Handles.[ draft_h; sub_h ])
  in
  let email_id =
    match Method.created draft_r draft1 with
    | Some e -> (
        match e.id with
        | Some id -> id
        | None -> Alcotest.fail "Email/set created object has no id")
    | None -> Alcotest.failf "the draft was not created: %s" subject
  in
  let submission_id =
    match Method.created sub_r sub1 with
    | Some s ->
        (* Section 7: undoStatus, sendAt and threadId are all server-set. *)
        Alcotest.(check bool)
          "undoStatus is set on create" true (s.undo_status <> None);
        Alcotest.(check bool) "sendAt is set on create" true (s.send_at <> None);
        s.id
    | None ->
        Alcotest.failf "the submission was not created (notCreated: %b)"
          (sub_r.not_created <> None)
  in
  Alcotest.(check bool)
    "the created submission has an id" true (submission_id <> None);

  (* The implicit Email/set of Section 7.5 shares the call id of the
     EmailSubmission/set, so it is the second response with that id; its
     effect is visible in the Email itself. *)
  let e =
    H.email t email_id ~properties:[ `Mailbox_ids; `Keywords; `Subject ]
  in
  let mailboxes =
    List.filter_map
      (fun (id, v) -> if v then Some (Id.to_string id) else None)
      (Option.value ~default:[] e.mailbox_ids)
  in
  Alcotest.(check (list string))
    "onSuccessUpdateEmail moved the message to Sent"
    [ Id.to_string sent ]
    mailboxes;
  Alcotest.(check bool)
    "and removed the $draft keyword" false
    (Email.has_keyword `Draft e);

  (* The submission really was relayed: the recipient has the message. *)
  let received =
    wait_for t (fun () ->
        match
          H.query_by_subject ~client:rcpt_client ~account_id:rcpt_account t
            subject
        with
        | id :: _ -> Some id
        | [] -> None)
  in
  (match received with
  | Some _ -> ()
  | None -> Alcotest.failf "%s never received %S" rcpt_address subject);

  (* EmailSubmission/get and /query round trips (Sections 7.1 and 7.3). The
     oracle destroys submission objects as soon as the message is relayed,
     which Section 7 explicitly permits ("a server MAY destroy
     EmailSubmission objects at any time after the message is successfully
     sent"), so both may legitimately come back empty. *)
  let sid = Option.get submission_id in
  let Results.[ g; q; u; c ] =
    H.run_all t
      Chain.(
        let* get_h =
          email_submission_get ~account_id:t.H.account_id ~ids:(Chain.id sid) ()
        in
        let* query_h =
          email_submission_query ~account_id:t.H.account_id
            ~filter:(Submission.filter ~email_ids:[ email_id ] ())
            ~sort:[ Submission.sort ~ascending:false `Sent_at ]
            ~calculate_total:true ()
        in
        let* undo_h =
          email_submission_query ~account_id:t.H.account_id
            ~filter:(Submission.filter ~undo_status:`Final ())
            ()
        in
        let+ changes_h =
          email_submission_changes ~account_id:t.H.account_id ~since_state:"0"
            ()
        in
        Handles.[ get_h; query_h; undo_h; changes_h ])
  in
  (match g.list with
  | [ s ] ->
      Alcotest.(check (option string))
        "the fetched submission is for this email"
        (Some (Id.to_string email_id))
        (Option.map Id.to_string s.email_id);
      Alcotest.(check bool)
        "with a known undoStatus" true
        (match s.undo_status with
        | Some u -> Submission.undo_status_to_string u <> ""
        | None -> false)
  | [] ->
      Alcotest.(check (list string))
        "a submission the server dropped is reported as notFound"
        [ Id.to_string sid ]
        (List.map Id.to_string g.not_found)
  | l ->
      Alcotest.failf "expected at most one submission, got %d" (List.length l));
  Alcotest.(check bool)
    "the emailIds filter matches only what /get returned" true
    (List.length q.ids = List.length g.list);
  Alcotest.(check bool)
    "an undoStatus filter is accepted" true (u.position >= 0L);
  Alcotest.(check bool)
    "EmailSubmission/changes advances the state" true
    (c.new_state <> "" && not c.has_more_changes);

  (* Clean up both sides. *)
  destroy_emails t ~account_id:t.H.account_id [ email_id ];
  destroy_emails t ~client:rcpt_client ~account_id:rcpt_account
    (H.query_by_subject ~client:rcpt_client ~account_id:rcpt_account t subject)

(* Section 7.5: "If the Email or Identity id given cannot be found, the
   submission creation is rejected with a standard 'invalidProperties'
   SetError." Both halves are checked; a server that validates neither is
   reported rather than failed, since the two creates are independent. *)
let submission_errors t =
  let drafts = mailbox_id t `Drafts in
  let identity = primary_identity t in
  let subject = H.unique "oracle-badsend" in
  (* Addressed to ourselves, so that a server which accepts the bogus
     identity does not mail a third party. *)
  let draft =
    draft_email ~mailbox:drafts ~from:t.H.address ~to_:t.H.address ~subject
      ~body:"This submission is expected to be rejected.\n"
  in
  let draft2 = Id.creation "draft2"
  and bad1 = Id.creation "bad1"
  and bad2 = Id.creation "bad2" in
  let bad_identity =
    Submission.create
      ~identity_id:(Id.of_string_exn "no-such-identity")
      ~email_id:(Id.creation_ref draft2) ()
  in
  let bad_email =
    Submission.create ~identity_id:identity
      ~email_id:(Id.of_string_exn "Mno-such-email-at-all")
      ()
  in
  let Results.[ draft_r; ident_r; email_r ] =
    H.run_all t
      Chain.(
        let* draft_h =
          email_set ~account_id:t.H.account_id ~create:[ (draft2, draft) ] ()
        in
        let* ident_h =
          email_submission_set ~account_id:t.H.account_id
            ~create:[ (bad1, bad_identity) ]
            ()
        in
        let+ email_h =
          email_submission_set ~account_id:t.H.account_id
            ~create:[ (bad2, bad_email) ]
            ()
        in
        Handles.[ draft_h; ident_h; email_h ])
  in
  let email_id = Option.bind (Method.created draft_r draft2) Email.id in
  (* An unknown emailId must be a SetError; the codec must keep whatever
     type the server used, including one outside the registry. *)
  (match Method.set_failures email_r with
  | [ (cid, e) ] ->
      Alcotest.(check string)
        "the failing creation id" "bad2" (Id.to_string cid);
      Alcotest.(check bool)
        "a SetError type was decoded" true
        (Error.Set_error.type_to_string e.Error.Set_error.type_ <> "")
  | _ -> Alcotest.fail "a submission with an unknown emailId was not rejected");
  (* An unknown identityId ought to fail the same way. Cyrus accepts it, so
     only note the outcome and clean up whatever it produced. *)
  Alcotest.(check bool)
    "an unknown identityId is either rejected or silently accepted" true
    (ident_r.not_created <> None || ident_r.created <> None);
  (* If the server did accept the bogus identity it relayed the message to
     ourselves, so wait for the delivered copy before cleaning up: the draft
     and the copy share the subject. *)
  let want = if ident_r.created <> None then 2 else 1 in
  let ids =
    match
      wait_for t ~timeout:30.0 (fun () ->
          let ids = H.query_by_subject t subject in
          if List.length ids >= want then Some ids else None)
    with
    | Some ids -> ids
    | None ->
        Alcotest.failf "%d copies of %S never showed up for cleanup" want
          subject
  in
  let ids =
    List.sort_uniq Id.compare
      (ids @ match email_id with Some id -> [ id ] | None -> [])
  in
  destroy_emails t ~account_id:t.H.account_id ids

let () =
  H.run "oracle-submission"
    [
      ( "identity",
        [
          H.test_case "Identity/get" identity_get;
          H.test_case "Identity/changes" identity_changes;
          H.test_case "Identity/set lifecycle" identity_lifecycle;
        ] );
      ( "submission",
        [
          H.test_case "send with onSuccessUpdateEmail" send_email;
          H.test_case "EmailSubmission/set errors" submission_errors;
        ] );
    ]
