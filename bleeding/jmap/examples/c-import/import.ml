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

let message subject =
  String.concat "\r\n"
    [
      "From: Alice <alice@example.org>";
      "To: You <you@example.com>";
      "Subject: " ^ subject;
      "Date: Tue, 02 Jan 2024 10:00:00 +0000";
      "Message-ID: <" ^ subject ^ "@example.org>";
      "Content-Type: text/plain; charset=utf-8";
      "";
      "Filed by the c-import step of the tutorial.";
      "";
    ]

let () =
  Cli.main "import"
    ~doc:"Import a message file, and parse a blob without storing it"
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in
  let inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
  let subject = Printf.sprintf "tutorial-import-%d" (Unix.getpid ()) in
  let blob =
    Client.upload_exn client ~account_id ~content_type:"message/rfc822"
      ~data:(message subject)
  in
  let blob_id = blob.blob_id in
  Fmt.pr "uploaded  %Ld octets as blob %a@." blob.size Proto.Id.pp blob_id;

  let one = Proto.Email.creation "one" in
  let Results.[ imported; parsed ] =
    Client.run_exn client
      Chain.(
        let* ih =
          email_import ~account_id
            ~emails:
              [
                ( one,
                  Proto.Email.Import.email ~blob_id ~mailbox_ids:[ inbox ]
                    ~keywords:[ `Seen ] () );
              ]
            ()
        in
        let+ ph =
          email_parse ~account_id ~blob_ids:(ids [ blob_id ])
            ~properties:[ `Id; `Mailbox_ids; `Subject; `From ]
            ()
        in
        Handles.[ ih; ph ])
  in
  (match Proto.Email.Import.not_created imported one with
  | Some e -> Fmt.failwith "Email/import: %a" Proto.Error.Set_error.pp e
  | None -> ());
  let made = Option.get (Proto.Email.Import.created imported one) in
  let email_id = Option.get (Proto.Email.id made) in
  Fmt.pr "imported  %a into the inbox, state now %s@." Proto.Id.pp email_id
    imported.new_state;

  let email = Option.get (Proto.Email.Parse.parsed parsed blob_id) in
  Fmt.pr "parsed    %S from %a, id=%a@."
    (Option.value email.subject ~default:"")
    Fmt.(option ~none:(any "nobody") (list ~sep:comma Proto.Email_address.pp))
    email.from
    Fmt.(option ~none:(any "null") Proto.Id.pp)
    email.id;

  let destroyed =
    Client.call_exn client
      (Chain.email_set ~account_id ~destroy:(Chain.id email_id) ())
  in
  (match Proto.Method.set_failures destroyed with
  | [] -> ()
  | f :: _ -> Fmt.failwith "left behind: %a" Proto.Method.pp_set_failure f);
  Fmt.pr "destroyed %a@." Proto.Id.pp email_id
