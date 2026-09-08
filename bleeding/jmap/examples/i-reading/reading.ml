(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results

let doc = "Open the newest message the way a reading pane does"

let headers =
  Proto.Email_header.
    [
      message_ids `Message_id;
      date `Date;
      addresses `To;
      text (`Custom "List-Id");
      raw ~all:true "Received";
    ]

let properties : Proto.Email.property list =
  [
    `Thread_id;
    `Mailbox_ids;
    `Keywords;
    `Size;
    `Received_at;
    `Sent_at;
    `Subject;
    `From;
    `Has_attachment;
    `Body_structure;
    `Body_values;
    `Text_body;
    `Attachments;
  ]
  @ List.map (fun h -> `Header h) headers

let body_properties : Proto.Email.body_part_property list =
  [ `Part_id; `Blob_id; `Size; `Name; `Type; `Disposition; `Sub_parts ]

let opt pp = Fmt.(option ~none:(any "-") pp)
let bytes = Fmt.(option ~none:(any "?") int64)
let or_unknown = Option.value ~default:"?"
let date = Option.fold ~none:"-" ~some:Proto.Date.to_utc_string

let ids ppf = function
  | [] -> Format.pp_print_string ppf "-"
  | l -> Fmt.(list ~sep:sp string) ppf l

let rec print_part depth (p : Proto.Email_body.Part.t) =
  Fmt.pr "  %s%s%a, %a bytes%a%a@."
    (String.make (depth * 2) ' ')
    (or_unknown p.type_)
    Fmt.(option (any " part " ++ string))
    p.part_id bytes p.size
    Fmt.(option (any " " ++ string))
    p.disposition
    Fmt.(option (any " " ++ Dump.string))
    p.name;
  List.iter (print_part (depth + 1)) (Option.value p.sub_parts ~default:[])

let () =
  Jmap_eio.Cli.main "reading" ~doc @@ fun ctx ->
  let account_id = ctx.account_id in
  let Results.[ folders; got ] =
    Client.run_exn ctx.client
      Chain.(
        let* folders = mailbox_get ~account_id ~properties:[ `Id; `Name ] () in
        let* q =
          email_query ~account_id
            ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
            ~limit:1L ()
        in
        let+ email =
          email_get ~account_id ~ids:(from_query q) ~properties ~body_properties
            ~fetch_text_body_values:true ~max_body_value_bytes:4096L ()
        in
        Handles.[ folders; email ])
  in
  let folder id =
    List.find_map
      (fun (m : Proto.Mailbox.t) ->
        if Option.equal Proto.Id.equal (Some id) m.id then m.name else None)
      folders.list
    |> Option.value ~default:(Proto.Id.to_string id)
  in
  match got.list with
  | [] -> Fmt.failwith "the Inbox is empty; deliver a message first"
  | (email : Proto.Email.t) :: _ ->
      Fmt.pr "Thread    %a@." (opt Proto.Id.pp) email.thread_id;
      Fmt.pr "Mailboxes %a@." ids
        (List.map folder (Proto.Email.mailbox_list email));
      Fmt.pr "Keywords  %a@." ids
        (List.map Proto.Keyword.to_string (Proto.Email.keyword_list email));
      Fmt.pr "Size      %a bytes@." bytes email.size;
      Fmt.pr "Received  %s@." (date email.received_at);
      Fmt.pr "Sent      %s@." (date email.sent_at);
      Fmt.pr "From      %a@."
        (opt Fmt.(list ~sep:comma Proto.Email_address.pp))
        email.from;
      Fmt.pr "Subject   %s@." (Option.value email.subject ~default:"(none)");
      Fmt.pr "@.header fields@.";
      List.iter
        (fun h ->
          let property = Proto.Email_header.header_property_to_string h in
          Fmt.pr "  %-30s %s@." property
            (Option.value
               (Proto.Email.find_header_text email property)
               ~default:"(absent)"))
        headers;
      Fmt.pr "@.bodyStructure@.";
      Option.iter (print_part 0) email.body_structure;
      Fmt.pr "@.textBody@.";
      List.iter
        (fun (p : Proto.Email_body.Part.t) ->
          Proto.Email.body_value email p
          |> Option.iter (fun (v : Proto.Email_body.Value.t) ->
              Fmt.pr "  part %s (%s%s%s)@." (or_unknown p.part_id)
                (or_unknown p.type_)
                (if v.is_truncated then ", truncated" else "")
                (if v.is_encoding_problem then ", encoding problem" else "");
              String.split_on_char '\n' v.value |> List.iter (Fmt.pr "  | %s@.")))
        (Option.value email.text_body ~default:[]);
      let attachments = Option.value email.attachments ~default:[] in
      Fmt.pr "@.attachments %d (hasAttachment %a)@." (List.length attachments)
        (opt Fmt.bool) email.has_attachment;
      List.iter
        (fun (p : Proto.Email_body.Part.t) ->
          Fmt.pr "  %s (%s, %a bytes)@."
            (Option.value p.name ~default:"(unnamed)")
            (or_unknown p.type_) bytes p.size)
        attachments;
      List.find_map
        (fun (p : Proto.Email_body.Part.t) ->
          Option.map (fun blob_id -> (blob_id, p)) p.blob_id)
        attachments
      |> Option.iter (fun (blob_id, (p : Proto.Email_body.Part.t)) ->
          let name = Option.value p.name ~default:"attachment" in
          let accept =
            Option.value p.type_ ~default:"application/octet-stream"
          in
          let data =
            Client.download_exn ctx.client ~account_id ~blob_id ~name ~accept ()
          in
          Fmt.pr "  downloaded %d bytes of %s@." (String.length data) name)
