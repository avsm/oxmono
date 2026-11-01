(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results

let doc = "Import a message file, then parse the message forwarded inside it"

let forwarded_message label =
  let crlf = String.concat "\r\n" in
  let original =
    crlf
      [
        "From: Bob <bob@example.org>";
        "To: Alice <alice@example.org>";
        "Subject: The original question";
        "Date: Mon, 01 Jan 2024 09:00:00 +0000";
        "Message-ID: <original-" ^ label ^ "@example.org>";
        "Content-Type: text/plain; charset=utf-8";
        "";
        "Could you take a look at the report?";
        "";
      ]
  in
  crlf
    [
      "From: Alice <alice@example.org>";
      "To: You <you@example.com>";
      "Subject: Fwd: the original question";
      "Date: Tue, 02 Jan 2024 10:00:00 +0000";
      "Message-ID: <" ^ label ^ "@example.org>";
      "MIME-Version: 1.0";
      "Content-Type: multipart/mixed; boundary=\"" ^ label ^ "\"";
      "";
      "--" ^ label;
      "Content-Type: text/plain; charset=utf-8";
      "";
      "Forwarding this on, see attached.";
      "";
      "--" ^ label;
      "Content-Type: message/rfc822";
      "Content-Disposition: attachment; filename=\"original.eml\"";
      "";
      original;
      "--" ^ label ^ "--";
      "";
    ]

let rec flatten (p : Proto.Email_body.Part.t) =
  p :: List.concat_map flatten (Option.value p.sub_parts ~default:[])

let attached_message (e : Proto.Email.t) =
  Option.value e.attachments ~default:[]
  @ List.concat_map flatten (Option.to_list e.body_structure)
  |> List.find_opt (fun (p : Proto.Email_body.Part.t) ->
      p.type_ = Some "message/rfc822")

let file_term =
  let open Cmdliner in
  let doc =
    "Import the RFC 5322 message in $(docv) instead of the built-in forwarded \
     sample."
  in
  Arg.(value & opt (some file) None & info [ "f"; "file" ] ~docv:"EML" ~doc)

let () =
  Jmap_eio.Cli.main' "parse" ~doc ~args:file_term @@ fun ctx file ->
  let account_id = ctx.account_id and client = ctx.client in
  let label = Printf.sprintf "l-parse-%d" (Unix.getpid ()) in
  let blob =
    match file with
    | Some path ->
        Eio.Path.with_open_in Eio.Path.(Eio.Stdenv.fs ctx.env / path)
        @@ fun handle ->
        Client.upload_flow_exn client ~account_id ~content_type:"message/rfc822"
          ~length:(Optint.Int63.to_int64 (Eio.File.size handle))
          handle
    | None ->
        Client.upload_exn client ~account_id ~content_type:"message/rfc822"
          ~data:(forwarded_message label)
  in
  let blob_id = blob.blob_id in
  Fmt.pr "uploaded  %Ld octets as blob %a@." blob.size Proto.Id.pp blob_id;

  let name = "Imported " ^ label in
  let mailbox = Proto.Mailbox.create_exn ~name () in
  let mailbox_cid = Proto.Mailbox.creation "mailbox" in
  let email_cid = Proto.Email.creation "message" in
  let Results.[ created; imported ] =
    Client.run_exn client
      Chain.(
        let* mh =
          mailbox_set ~account_id ~create:[ (mailbox_cid, mailbox) ] ()
        in
        let+ ih =
          email_import ~account_id
            ~emails:
              [
                ( email_cid,
                  Proto.Email.Import.email ~blob_id
                    ~mailbox_ids:[ Proto.Id.creation_ref mailbox_cid ]
                    ~keywords:[ `Seen ] () );
              ]
            ()
        in
        Handles.[ mh; ih ])
  in
  (match Proto.Method.set_failures created with
  | (_, e) :: _ -> Fmt.failwith "Mailbox/set: %a" Proto.Error.Set_error.pp e
  | [] -> ());
  let mailbox_id =
    Option.get
      (Option.bind (Proto.Method.created created mailbox_cid) Proto.Mailbox.id)
  in
  let destroy () =
    match
      Proto.Method.set_failures
        (Client.call_exn client
           (Chain.mailbox_set ~account_id ~destroy:(Chain.id mailbox_id)
              ~on_destroy_remove_emails:true ()))
    with
    | [] -> Fmt.pr "destroyed the mailbox and the message it held@."
    | f :: _ -> Fmt.failwith "left behind: %a" Proto.Method.pp_set_failure f
  in
  Fun.protect ~finally:destroy @@ fun () ->
  (match Proto.Email.Import.not_created imported email_cid with
  | Some e -> Fmt.failwith "Email/import: %a" Proto.Error.Set_error.pp e
  | None -> ());
  let email_id =
    Option.get
      (Option.bind
         (Proto.Email.Import.created imported email_cid)
         Proto.Email.id)
  in
  Fmt.pr "imported  %a into %S, state now %s@." Proto.Id.pp email_id name
    imported.new_state;

  let got =
    Client.call_exn client
      (Chain.email_get ~account_id ~ids:(Chain.id email_id)
         ~properties:[ `Id; `Subject; `Keywords; `Attachments; `Body_structure ]
         ~body_properties:
           [ `Part_id; `Blob_id; `Type; `Name; `Size; `Sub_parts ]
         ())
  in
  let email =
    match got.list with
    | [ e ] -> e
    | _ -> Fmt.failwith "Email/get did not return the imported message"
  in
  Fmt.pr "message   %S keywords [%a]@."
    (Option.value email.subject ~default:"")
    Fmt.(list ~sep:sp string)
    (List.map Proto.Keyword.to_string (Proto.Email.keyword_list email));

  match attached_message email with
  | None ->
      Fmt.pr "no message/rfc822 part in this message, so nothing to parse@."
  | Some part ->
      let part_blob = Option.get part.blob_id in
      Fmt.pr "attached  %s, %a octets in blob %a@."
        (Option.value part.name ~default:"(unnamed)")
        Fmt.(option ~none:(any "?") int64)
        part.size Proto.Id.pp part_blob;
      let r =
        Client.call_exn client
          (Chain.email_parse ~account_id ~blob_ids:(Chain.id part_blob)
             ~properties:
               [
                 `Id;
                 `Mailbox_ids;
                 `Keywords;
                 `Received_at;
                 `Subject;
                 `From;
                 `Text_body;
                 `Body_values;
               ]
             ~fetch_text_body_values:true ~max_body_value_bytes:512L ())
      in
      let inner =
        match Proto.Email.Parse.parsed r part_blob with
        | Some e -> e
        | None when r.not_parsable <> None ->
            Fmt.failwith
              "Email/parse: the attachment is not an RFC 5322 message"
        | None ->
            Fmt.failwith "Email/parse: blob %a not found" Proto.Id.pp part_blob
      in
      let null b = if b then "null" else "set" in
      Fmt.pr "parsed    %S from %a@."
        (Option.value inner.subject ~default:"")
        Fmt.(
          option ~none:(any "nobody") (list ~sep:comma Proto.Email_address.pp))
        inner.from;
      Fmt.pr "          id %s, mailboxIds %s, keywords %s, receivedAt %s@."
        (null (inner.id = None))
        (null (inner.mailbox_ids = None))
        (null (inner.keywords = None))
        (null (inner.received_at = None));
      Option.iter
        (fun (v : Proto.Email_body.Value.t) ->
          Fmt.pr "          %s@." (String.trim v.value))
        (List.find_map
           (Proto.Email.body_value inner)
           (Option.value inner.text_body ~default:[]))
