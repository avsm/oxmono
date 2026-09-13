(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Termanil_model
module P = Jmap.Proto

type t = {
  client : Jmap_eio.Client.t;
  service : string;
  account : string;
  account_id : P.Id.t;
}

let parse_id s =
  match P.Id.of_string_received s with
  | Ok x -> x
  | Error _ -> failwith "Invalid JMAP identifier"

let create ~client ~service ~account =
  { client; service; account; account_id = parse_id account }

let check_account t account =
  if not (P.Id.equal account t.account_id) then
    failwith "JMAP returned a different account"

let check_source t (source : email_ref) =
  if source.service <> t.service || source.account <> t.account then
    failwith "Message belongs to a different JMAP service or account"

let call t chain =
  match
    Jmap_eio.Client.call t.client
      ~capabilities:[ P.Capability.core; P.Capability.mail ]
      chain
  with
  | Ok r -> r
  | Error _ -> failwith "JMAP request failed; refresh before retrying"

let required_id = function
  | Some i -> P.Id.to_string i
  | None -> failwith "JMAP object has no id"

module Html_text = Html_text
module Search = Search

let addresses xs =
  Option.value xs ~default:[]
  |> List.map (fun (a : P.Email_address.t) ->
      match a.name with
      | Some n when n <> "" -> n ^ " <" ^ a.email ^ ">"
      | _ -> a.email)
  |> String.concat ", "

let metadata (m : P.Email.t) =
  let date =
    Option.fold ~none:"" ~some:(Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0)
  in
  [
    ("To", addresses m.to_);
    ("Cc", addresses m.cc);
    ("Bcc", addresses m.bcc);
    ("Reply-To", addresses m.reply_to);
    ("Date", date m.sent_at);
    ("Received", date m.received_at);
    ("Message-ID", String.concat ", " (Option.value m.message_id ~default:[]));
  ]
  |> List.filter (fun (_, value) -> value <> "")
  |> fun headers ->
  headers
  @ List.map
      (fun (p : P.Email_body.Part.t) ->
        ( "Attachment",
          Option.value p.name ~default:"(unnamed)"
          ^ " | "
          ^ Option.value p.type_ ~default:"application/octet-stream"
          ^ Option.fold ~none:""
              ~some:(fun n -> Printf.sprintf " | %Ld bytes" n)
              p.size ))
      (Option.value m.attachments ~default:[])

let message ~service ~account (m : P.Email.t) =
  let from = Option.value m.from ~default:[] in
  let sender =
    String.concat ", "
      (List.map
         (fun (a : P.Email_address.t) ->
           match a.name with
           | Some n when n <> "" -> n ^ " <" ^ a.email ^ ">"
           | _ -> a.email)
         from)
  in
  {
    source = { service; account; id = required_id m.id };
    thread_id =
      Option.fold ~none:(required_id m.id) ~some:P.Id.to_string m.thread_id;
    subject =
      (match m.subject with Some s when s <> "" -> s | _ -> "(no subject)");
    sender;
    addresses = List.map (fun (a : P.Email_address.t) -> a.email) from;
    received =
      Option.fold ~none:""
        ~some:(Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0)
        m.received_at;
    preview = Option.value m.preview ~default:"";
    seen = P.Email.has_keyword `Seen m;
    flagged = P.Email.has_keyword `Flagged m;
    metadata = metadata m;
  }

let body (m : P.Email.t) =
  let parts convert parts =
    Option.value parts ~default:[]
    |> List.filter_map (fun p ->
        Option.map
          (fun (v : P.Email_body.Value.t) ->
            convert v.value
            ^ (if v.is_encoding_problem then "\n[This part has encoding errors]"
               else "")
            ^ if v.is_truncated then "\n[Part truncated at 256 KiB]" else "")
          (P.Email.body_value m p))
    |> List.filter (fun s -> String.trim s <> "")
  in
  match parts Fun.id m.text_body with
  | [] -> (
      match parts Html_text.render m.html_body with
      | [] ->
          "[No readable body available]\n" ^ Option.value m.preview ~default:""
      | text -> "[HTML converted to text]\n" ^ String.concat "\n\n" text)
  | text -> String.concat "\n\n" text

let mailboxes t =
  let r =
    call t
      Jmap.Chain.(
        mailbox_get ~account_id:t.account_id
          ~properties:[ `Id; `Name; `Role; `Unread_emails ]
          ())
  in
  check_account t r.account_id;
  List.map
    (fun (m : P.Mailbox.t) ->
      {
        id = required_id m.id;
        name = Option.value m.name ~default:"(unnamed)";
        unread = Int64.to_int (Option.value m.unread_emails ~default:0L);
        inbox = m.role = Some `Inbox;
      })
    r.list
  |> List.sort (fun (a : mailbox) b ->
      compare (not a.inbox, a.name, a.id) (not b.inbox, b.name, b.id))

let properties =
  [ `Thread_id; `Id; `Subject; `From; `Received_at; `Preview; `Keywords ]

let detail_properties =
  [
    `Text_body;
    `Html_body;
    `Body_values;
    `Attachments;
    `Message_id;
    `References;
    `Reply_to;
    `To;
    `Cc;
    `Bcc;
    `Sent_at;
  ]

let messages t ~mailbox ~query ~position =
  if position < 0 then invalid_arg "negative message position";
  let limit =
    match P.Session.core_capability (Jmap_eio.Client.session t.client) with
    | None -> 50L
    | Some c -> Int64.min 50L c.max_objects_in_get
  in
  if limit < 1L then failwith "Server does not allow message reads";
  let filter = Search.filter ~mailbox:(Option.map parse_id mailbox) query in
  let q =
    call t
      Jmap.Chain.(
        email_query ~account_id:t.account_id ~filter
          ~position:(Int64.of_int position) ~limit ~calculate_total:true
          ~collapse_threads:true
          ~sort:[ P.Email.sort ~ascending:false `Received_at ]
          ())
  in
  check_account t q.account_id;
  if
    q.position <> Int64.of_int position
    || Int64.of_int (List.length q.ids) > limit
    || List.length (List.sort_uniq compare q.ids) <> List.length q.ids
  then failwith "JMAP returned an invalid message page";
  let rows =
    if q.ids = [] then []
    else
      let r =
        call t
          Jmap.Chain.(
            email_get ~account_id:t.account_id ~ids:(ids q.ids) ~properties ())
      in
      check_account t r.account_id;
      let rows =
        List.map
          (fun m ->
            let key = required_id m.P.Email.id in
            if not (List.exists (fun id -> P.Id.to_string id = key) q.ids) then
              failwith "JMAP returned an unrequested message";
            (key, message ~service:t.service ~account:t.account m))
          r.list
      in
      if
        List.length (List.sort_uniq String.compare (List.map fst rows))
        <> List.length rows
      then failwith "JMAP returned duplicate messages";
      let accounted = List.map fst rows @ List.map P.Id.to_string r.not_found in
      if
        List.sort String.compare accounted
        <> List.sort String.compare (List.map P.Id.to_string q.ids)
      then failwith "JMAP returned an incomplete message page";
      rows
  in
  let messages =
    List.filter_map (fun id -> List.assoc_opt (P.Id.to_string id) rows) q.ids
  in
  let consumed = List.length q.ids in
  let next = position + consumed in
  let has_more =
    consumed > 0
    &&
    match q.total with
    | Some total -> Int64.of_int next < total
    | None -> consumed >= Int64.to_int (Option.value q.limit ~default:limit)
  in
  {
    mailbox;
    query;
    position;
    next = (if has_more then Some next else None);
    messages;
  }

let get t source ~with_body =
  check_source t source;
  let r =
    call t
      Jmap.Chain.(
        email_get ~account_id:t.account_id
          ~ids:(ids [ parse_id source.id ])
          ~fetch_text_body_values:with_body ~fetch_html_body_values:with_body
          ~max_body_value_bytes:262144L
          ~properties:(properties @ if with_body then detail_properties else [])
          ())
  in
  check_account t r.account_id;
  match r.list with
  | [ m ] when required_id m.id = source.id -> (r.state, m)
  | _ -> failwith "Message vanished or JMAP returned another message"

let read t source =
  let _, m = get t source ~with_body:true in
  (message ~service:t.service ~account:t.account m, body m)

let set_keyword t source (keyword : [ `Seen | `Flagged ]) enabled =
  let keyword = (keyword :> P.Keyword.t) in
  let state, _ = get t source ~with_body:false in
  let entry =
    if enabled then P.Email.Patch.set_keyword keyword
    else P.Email.Patch.remove_keyword keyword
  in
  let r =
    call t
      Jmap.Chain.(
        email_set ~account_id:t.account_id ~if_in_state:state
          ~update:[ (parse_id source.id, P.Patch.v [ entry ]) ]
          ())
  in
  check_account t r.account_id;
  if
    List.mem_assoc (parse_id source.id) (Option.value r.not_updated ~default:[])
    || not
         (List.mem_assoc (parse_id source.id)
            (Option.value r.updated ~default:[]))
  then failwith "Server refused the message update; refresh before retrying"

let archive t source =
  let state, _ = get t source ~with_body:false in
  let boxes =
    call t
      Jmap.Chain.(
        mailbox_get ~account_id:t.account_id ~properties:[ `Id; `Role ] ())
  in
  check_account t boxes.account_id;
  let role wanted =
    match
      List.filter (fun (b : P.Mailbox.t) -> b.role = Some wanted) boxes.list
    with
    | [ b ] -> parse_id (required_id b.id)
    | _ -> failwith "Archiving requires unique Inbox and Archive mailboxes"
  in
  let inbox = role `Inbox and archive = role `Archive in
  let r =
    call t
      Jmap.Chain.(
        email_set ~account_id:t.account_id ~if_in_state:state
          ~update:
            [
              ( parse_id source.id,
                P.Patch.v
                  [
                    P.Email.Patch.remove_from_mailbox inbox;
                    P.Email.Patch.add_to_mailbox archive;
                  ] );
            ]
          ())
  in
  check_account t r.account_id;
  if
    (not
       (List.mem_assoc (parse_id source.id)
          (Option.value r.updated ~default:[])))
    || List.mem_assoc (parse_id source.id)
         (Option.value r.not_updated ~default:[])
  then failwith "Server refused archive. Refresh before retrying."

let conversation t source =
  let _, selected = get t source ~with_body:true in
  match selected.P.Email.thread_id with
  | None ->
      [
        (message ~service:t.service ~account:t.account selected, body selected);
      ]
  | Some thread ->
      let r =
        call t
          Jmap.Chain.(
            thread_get ~account_id:t.account_id ~ids:(ids [ thread ]) ())
      in
      check_account t r.account_id;
      let ids =
        match r.list with
        | [ th ] when th.P.Thread.id = Some thread ->
            Option.value th.email_ids ~default:[]
        | _ -> failwith "Conversation vanished or returned another thread"
      in
      if not (List.mem (parse_id source.id) ids) then
        failwith "Conversation does not contain this email";
      (* Bound body downloads while retaining the explicitly selected source. *)
      let limit =
        match P.Session.core_capability (Jmap_eio.Client.session t.client) with
        | None -> 50
        | Some c -> Int64.to_int (Int64.min 50L c.max_objects_in_get)
      in
      if limit < 1 then failwith "Server does not allow conversation reads";
      let recent =
        List.filter (fun id -> P.Id.to_string id <> source.id) ids
        |> List.rev |> List.to_seq
        |> Seq.take (limit - 1)
        |> List.of_seq |> List.rev
      in
      let wanted =
        List.filter
          (fun id -> P.Id.to_string id = source.id || List.mem id recent)
          ids
      in
      let r =
        call t
          Jmap.Chain.(
            email_get ~account_id:t.account_id ~ids:(ids wanted)
              ~properties:(properties @ detail_properties)
              ~fetch_text_body_values:true ~fetch_html_body_values:true
              ~max_body_value_bytes:262144L ())
      in
      check_account t r.account_id;
      let rows =
        List.map
          (fun (m : P.Email.t) ->
            if m.thread_id <> Some thread then
              failwith "JMAP returned an unrelated conversation email";
            ( required_id m.id,
              (message ~service:t.service ~account:t.account m, body m) ))
          r.list
      in
      if
        List.sort String.compare (List.map fst rows)
        <> List.sort String.compare (List.map P.Id.to_string wanted)
      then failwith "JMAP returned incomplete or duplicate conversation emails";
      List.map
        (fun id ->
          let m, body = List.assoc (P.Id.to_string id) rows in
          let body =
            if
              P.Id.to_string id = source.id
              && List.length ids > List.length wanted
            then
              Printf.sprintf
                "[Showing %d of %d conversation messages. Search can find \
                 older mail.]\n\n\
                 %s"
                (List.length wanted) (List.length ids) body
            else body
          in
          (m, body))
        wanted

let reply_target t source =
  let _, m = get t source ~with_body:true in
  let summary = message ~service:t.service ~account:t.account m in
  let to_ =
    match m.reply_to with
    | Some (_ :: _ as xs) -> xs
    | _ -> Option.value m.from ~default:[]
  in
  if to_ = [] then failwith "This message has no reply address";
  let subject =
    if String.starts_with ~prefix:"re:" (String.lowercase_ascii summary.subject)
    then summary.subject
    else "Re: " ^ summary.subject
  in
  ( summary.thread_id,
    subject,
    List.map (fun (a : P.Email_address.t) -> a.email) to_ )

let submission_call t chain =
  match
    Jmap_eio.Client.call t.client
      ~capabilities:
        [ P.Capability.core; P.Capability.mail; P.Capability.submission ]
      chain
  with
  | Ok r -> r
  | Error _ -> failwith "JMAP submission request failed"

let sending_identity t ~identity =
  let identities =
    submission_call t Jmap.Chain.(identity_get ~account_id:t.account_id ())
  in
  check_account t identities.account_id;
  let candidates =
    List.filter
      (fun (i : P.Identity.t) ->
        match identity with
        | None -> true
        | Some id -> Option.map P.Id.to_string i.id = Some id)
      identities.list
  in
  match candidates with
  | [ i ] -> i
  | _ -> failwith "Configure mail.identity to select one sending identity"

let identity_signature (identity : P.Identity.t) =
  match identity.text_signature with
  | Some s when String.trim s <> "" -> s
  | _ -> Option.fold ~none:"" ~some:Html_text.render identity.html_signature

let signature t ~identity ~override =
  match override with
  | Some text -> text
  | None ->
      let identity = sending_identity t ~identity in
      identity_signature identity

let prepare_reply t ~identity (d : draft) =
  check_source t d.source;
  let thread, subject, recipients = reply_target t d.source in
  if thread <> d.thread_id || subject <> d.subject || recipients <> d.recipients
  then
    failwith
      "Reply headers changed. Review the source message and draft metadata.";
  if String.trim d.body = "" then failwith "Reply is empty";
  let identity = sending_identity t ~identity in
  let from =
    match identity.P.Identity.email with
    | Some email when not (String.contains email '*') ->
        { P.Email_address.name = identity.name; email }
    | _ -> failwith "Sending identity needs an explicit address"
  in
  let identity_id = parse_id (required_id identity.id) in
  let boxes =
    call t
      Jmap.Chain.(
        mailbox_get ~account_id:t.account_id ~properties:[ `Id; `Role ] ())
  in
  check_account t boxes.account_id;
  let role wanted =
    match
      List.filter (fun (b : P.Mailbox.t) -> b.role = Some wanted) boxes.list
    with
    | [ b ] -> parse_id (required_id b.id)
    | _ -> failwith "Server needs unique Drafts and Sent mailboxes"
  in
  let drafts_box = role `Drafts and sent_box = role `Sent in
  let _, original = get t d.source ~with_body:true in
  let message_ids = Option.value original.message_id ~default:[] in
  let references = Option.value original.references ~default:[] @ message_ids in
  let email =
    P.Email.create ~mailbox_ids:[ drafts_box ] ~keywords:[ `Draft; `Seen ]
      ~from:[ from ]
      ~to_:
        (List.map
           (fun email -> { P.Email_address.name = None; email })
           recipients)
      ~subject ~in_reply_to:message_ids ~references ~text_body:d.body ()
  in
  fun ~before_submit ->
    let creation = P.Email.creation "reply" in
    let r =
      call t
        Jmap.Chain.(
          email_set ~account_id:t.account_id ~create:[ (creation, email) ] ())
    in
    check_account t r.account_id;
    let created =
      match P.Method.created r creation with
      | Some e -> e
      | None -> failwith "Server refused draft creation"
    in
    let email_id = parse_id (required_id created.id) in
    before_submit (P.Id.to_string email_id);
    let creation = P.Submission.creation "send" in
    let patch =
      P.Patch.v
        [
          P.Email.Patch.remove_keyword `Draft;
          P.Email.Patch.set_mailboxes [ sent_box ];
        ]
    in
    let r =
      submission_call t
        Jmap.Chain.(
          email_submission_set ~account_id:t.account_id
            ~create:
              [ (creation, P.Submission.create ~identity_id ~email_id ()) ]
            ~on_success_update_email:[ (P.Id.creation_ref creation, patch) ]
            ())
    in
    check_account t r.account_id;
    match P.Method.created r creation with
    | Some submission -> required_id submission.id
    | None -> failwith "Server refused reply submission"

let submission_for_email t source =
  check_source t source;
  let email_id = parse_id source.id in
  let q =
    submission_call t
      Jmap.Chain.(
        email_submission_query ~account_id:t.account_id
          ~filter:(P.Submission.filter ~email_ids:[ email_id ] ())
          ~limit:2L ())
  in
  check_account t q.account_id;
  match q.ids with
  | [] -> None
  | wanted ->
      let r =
        submission_call t
          Jmap.Chain.(
            email_submission_get ~account_id:t.account_id
              ~ids:(Jmap.Chain.ids wanted)
              ~properties:[ `Id; `Email_id; `Undo_status ]
              ())
      in
      check_account t r.account_id;
      List.iter
        (fun (s : P.Submission.t) ->
          if
            s.email_id <> Some email_id
            || not (List.mem (parse_id (required_id s.id)) wanted)
          then failwith "JMAP returned an unrelated submission")
        r.list;
      List.find_opt
        (fun (s : P.Submission.t) ->
          s.undo_status = Some `Final || s.undo_status = Some `Pending)
        r.list
      |> Option.map (fun s -> required_id s.P.Submission.id)
