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
let at = Option.fold ~none:"-" ~some:Proto.Date.to_utc_string
let note = "part,quantity\nJM-8620,3\nJM-8621,1\n"

let attachment client ~account_id ~env attach =
  let upload path name =
    Eio.Path.with_open_in path @@ fun file ->
    let length = Optint.Int63.to_int64 (Eio.File.size file) in
    let r =
      Client.upload_flow_exn client ~account_id
        ~content_type:"application/octet-stream" ~length file
    in
    Fmt.pr "blob      %a %s (%Ld bytes)@." Proto.Id.pp r.blob_id name r.size;
    Proto.Email_body.Part.v ~blob_id:r.blob_id ~type_:"application/octet-stream"
      ~disposition:"attachment" ~name ()
  in
  match attach with
  | Some path ->
      upload Eio.Path.(Eio.Stdenv.fs env / path) (Filename.basename path)
  | None ->
      let name = "parts.csv" in
      let path =
        Eio.Path.(
          Eio.Stdenv.fs env
          / Filename.get_temp_dir_name ()
          / Printf.sprintf "compose-%d-%s" (Unix.getpid ()) name)
      in
      Eio.Path.save ~create:(`Or_truncate 0o600) path note;
      Fun.protect
        ~finally:(fun () -> Eio.Path.unlink path)
        (fun () -> upload path name)

let run (ctx : Cli.context) (to_opt, attach) =
  let client = ctx.client and account_id = ctx.account_id in
  let subject = Printf.sprintf "tutorial-compose-%d" (Unix.getpid ()) in
  let identities =
    Client.call_exn client ~capabilities (Chain.identity_get ~account_id ())
  in
  let identity =
    match identities.list with
    | i :: _ -> i
    | [] -> Fmt.failwith "this account has no Identity to send from"
  in
  let identity_id = Option.get identity.id in
  let username = (Client.session client).username in
  (* The oracle sends an Identity with an empty address. *)
  let from =
    Option.value
      (Proto.Identity.sending_address ~local_part:username identity)
      ~default:(username ^ "@example.com")
  in
  let to_ = Option.value to_opt ~default:from in
  Fmt.pr "identity  %a sends as %s@." Proto.Id.pp identity_id from;
  let drafts = Sync.mailbox_id_exn client ~account_id `Drafts
  and sent = Sync.mailbox_id_exn client ~account_id `Sent
  and inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
  let file = attachment client ~account_id ~env:ctx.env attach in
  let text = Proto.Email_body.Part.v ~part_id:"text" ~type_:"text/plain" () in
  let draft =
    Proto.Email.v
      ~mailbox_ids:[ (drafts, true) ]
      ~keywords:[ (`Draft, true) ]
      ~from:[ Proto.Email_address.create from ]
      ~to_:[ Proto.Email_address.create to_ ]
      ~subject
      ~body_structure:
        (Proto.Email_body.Part.v ~type_:"multipart/mixed"
           ~sub_parts:[ text; file ] ())
      ~body_values:
        [ ("text", Proto.Email_body.Value.v "Sent by the k-compose step.\n") ]
      ()
  in
  let draft_cid = Proto.Email.creation "draft" in
  let send_cid = Proto.Submission.creation "send" in
  let send_it =
    Proto.Submission.create ~identity_id
      ~email_id:(Proto.Id.creation_ref draft_cid)
      ~envelope:
        (Proto.Submission.Envelope.v
           ~mail_from:(Proto.Submission.Address.v from)
           ~rcpt_to:[ Proto.Submission.Address.v to_ ])
      ()
  in
  let file_it =
    let open Proto.Email.Patch in
    Proto.Patch.v
      [ remove_from_mailbox drafts; add_to_mailbox sent; remove_keyword `Draft ]
  in
  let Chain.Handles.[ _; subh ], Results.[ saved; submitted ], resp =
    Client.run_with_response_exn client ~capabilities
      Chain.(
        let* eh = email_set ~account_id ~create:[ (draft_cid, draft) ] () in
        let+ subh =
          email_submission_set ~account_id
            ~create:[ (send_cid, send_it) ]
            ~on_success_update_email:
              [ (Proto.Id.creation_ref send_cid, file_it) ]
            ()
        in
        Handles.[ eh; subh ])
  in
  (match Proto.Method.(set_failures saved @ set_failures submitted) with
  | f :: _ -> Fmt.failwith "not created: %a" Proto.Method.pp_set_failure f
  | [] -> ());
  let email_id =
    Option.get (Option.get (Proto.Method.created saved draft_cid)).id
  and submission = Option.get (Proto.Method.created submitted send_cid) in
  let submission_id = Option.get submission.id in
  Fmt.pr "submitted %a undoStatus=%s sendAt=%s@." Proto.Id.pp submission_id
    (undo submission.undo_status)
    (at submission.send_at);
  (match
     List.filter
       (fun (i : Proto.Invocation.t) -> String.equal i.name "Email/set")
       (Proto.Response.find_responses (Chain.call_id subh) resp)
   with
  | implicit :: _ -> Fmt.pr "filed     by the implicit %s@." implicit.name
  | [] -> Fmt.pr "filed     the server sent no implicit Email/set@.");
  let Results.[ read_back; message ] =
    Client.run_exn client ~capabilities
      Chain.(
        let* gh = email_submission_get ~account_id ~ids:(id submission_id) () in
        let+ mh =
          email_get ~account_id ~ids:(ids [ email_id ])
            ~properties:[ `Mailbox_ids; `Keywords; `Has_attachment ]
            ()
        in
        Handles.[ gh; mh ])
  in
  (match read_back.list with
  | [] -> Fmt.pr "read back the server has already discarded the submission@."
  | s :: _ -> Fmt.pr "read back undoStatus=%s@." (undo s.undo_status));
  (match message.list with
  | [ (e : Proto.Email.t) ] ->
      if not (Proto.Email.in_mailbox sent e) then
        Fmt.failwith "the message was not filed into Sent";
      if Proto.Email.has_keyword `Draft e then
        Fmt.failwith "the $draft keyword was not removed";
      Fmt.pr "message   is in Sent, hasAttachment=%b@."
        (Option.value e.has_attachment ~default:false)
  | _ -> Fmt.failwith "the message that was just sent is gone");
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
    match
      List.filter_map mine (Client.call_exn client ~capabilities newest).list
    with
    | [] when attempts > 0 ->
        Eio.Time.sleep (Eio.Stdenv.clock ctx.env) 1.;
        delivered (attempts - 1)
    | copies -> copies
  in
  let copies = delivered (if String.equal to_ from then 20 else 0) in
  let destroyed =
    Client.call_exn client ~capabilities
      (Chain.email_set ~account_id ~destroy:(Chain.ids (email_id :: copies)) ())
  in
  (match Proto.Method.set_failures destroyed with
  | [] -> ()
  | f :: _ -> Fmt.failwith "%a" Proto.Method.pp_set_failure f);
  Fmt.pr "destroyed %a, and %d copy delivered to the Inbox@." Proto.Id.pp
    email_id (List.length copies)

let to_term =
  let open Cmdliner in
  let doc = "Send to $(docv) rather than to the sending address itself." in
  Arg.(value & opt (some string) None & info [ "to" ] ~docv:"ADDRESS" ~doc)

let attach_term =
  let open Cmdliner in
  let doc = "Attach $(docv) rather than the small file the step writes." in
  Arg.(value & opt (some file) None & info [ "attach" ] ~docv:"FILE" ~doc)

let () =
  let args =
    Cmdliner.Term.(const (fun t a -> (t, a)) $ to_term $ attach_term)
  in
  Cli.main' "compose" ~doc:"Compose a message with an attachment and send it"
    ~args run
