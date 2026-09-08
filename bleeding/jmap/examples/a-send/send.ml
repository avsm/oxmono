(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Cli = Jmap_eio.Cli
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results
module Sync = Jmap_eio.Sync

let capabilities =
  Proto.[ Capability.core; Capability.mail; Capability.submission ]

let undo = Option.fold ~none:"-" ~some:Proto.Submission.undo_status_to_string

let () =
  Cli.main "send" ~doc:"Save a draft and send it with an EmailSubmission"
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in
  let subject = Printf.sprintf "tutorial-send-%d" (Unix.getpid ()) in
  let identities =
    Client.call_exn client ~capabilities (Chain.identity_get ~account_id ())
  in
  let identity =
    match identities.list with
    | i :: _ -> i
    | [] -> Fmt.failwith "this account has no Identity to send from"
  in
  let identity_id = Option.get (Proto.Identity.id identity) in
  let username = (Client.session client).username in
  let address =
    Option.value
      (Proto.Identity.sending_address ~local_part:username identity)
      ~default:(username ^ "@example.com")
  in
  Fmt.pr "identity  %a sends as %s@." Proto.Id.pp identity_id address;
  let drafts = Sync.mailbox_id_exn client ~account_id `Drafts
  and sent = Sync.mailbox_id_exn client ~account_id `Sent
  and inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
  let draft = Proto.Email.creation "draft"
  and submit = Proto.Submission.creation "submit" in
  let message =
    Proto.Email.create ~mailbox_ids:[ drafts ] ~keywords:[ `Draft ]
      ~from:[ Proto.Email_address.create address ]
      ~to_:[ Proto.Email_address.create address ]
      ~subject ~text_body:"Sent by the a-send step of the tutorial.\n" ()
  in
  let send_it =
    Proto.Submission.create ~identity_id
      ~email_id:(Proto.Id.creation_ref draft)
      ()
  in
  let file_it =
    let open Proto.Email.Patch in
    Proto.Patch.v
      [ remove_from_mailbox drafts; add_to_mailbox sent; remove_keyword `Draft ]
  in
  let Results.[ saved; submitted ] =
    Client.run_exn client ~capabilities
      Chain.(
        let* eh = email_set ~account_id ~create:[ (draft, message) ] () in
        let+ sh =
          email_submission_set ~account_id
            ~create:[ (submit, send_it) ]
            ~on_success_update_email:[ (Proto.Id.creation_ref submit, file_it) ]
            ()
        in
        Handles.[ eh; sh ])
  in
  (match Proto.Method.(set_failures saved @ set_failures submitted) with
  | f :: _ -> Fmt.failwith "not created: %a" Proto.Method.pp_set_failure f
  | [] -> ());
  let draft_made = Option.get (Proto.Method.created saved draft)
  and submission = Option.get (Proto.Method.created submitted submit) in
  let email_id = Option.get (Proto.Email.id draft_made)
  and submission_id = Option.get (Proto.Submission.id submission) in
  Fmt.pr "submitted %a undoStatus=%s@." Proto.Id.pp submission_id
    (undo submission.undo_status);
  let read_back =
    Client.call_exn client ~capabilities
      (Chain.email_submission_get ~account_id ~ids:(Chain.id submission_id) ())
  in
  (match read_back.list with
  | [] -> Fmt.pr "read back the server has already discarded the submission@."
  | s :: _ -> Fmt.pr "read back undoStatus=%s@." (undo s.undo_status));
  let newest =
    Chain.(
      let* q =
        email_query ~account_id
          ~filter:(Proto.Email.filter ~in_mailbox:inbox ())
          ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
          ~limit:20L ()
      in
      email_get ~account_id ~ids:(from_query q) ~properties:[ `Id; `Subject ] ())
  in
  let mine (e : Proto.Email.t) =
    if Option.equal String.equal e.subject (Some subject) then e.id else None
  in
  let rec delivered attempts =
    let got = Client.call_exn client ~capabilities newest in
    match List.filter_map mine got.list with
    | [] when attempts > 0 ->
        Eio.Time.sleep (Eio.Stdenv.clock ctx.env) 1.;
        delivered (attempts - 1)
    | copies -> copies
  in
  let copies = delivered 20 in
  let destroyed =
    Client.call_exn client ~capabilities
      (Chain.email_set ~account_id ~destroy:(Chain.ids (email_id :: copies)) ())
  in
  (match Proto.Method.set_failures destroyed with
  | [] -> ()
  | f :: _ -> Fmt.failwith "%a" Proto.Method.pp_set_failure f);
  Fmt.pr "destroyed %a, and %d copy delivered to the Inbox@." Proto.Id.pp
    email_id (List.length copies)
