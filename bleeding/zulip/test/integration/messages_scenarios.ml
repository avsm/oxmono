open Zulip_eio

let fail error = Alcotest.fail (Error.error_to_string error)
let ok = function Ok value -> value | Error error -> fail error

let check cond fmt =
  if not cond then Printf.ksprintf (fun message -> Alcotest.fail message) fmt
  else Printf.ksprintf ignore fmt

let find_scheduled id messages =
  List.find_opt
    (fun (message : Scheduled_messages.t) ->
      Scheduled_messages.Id.equal message.id id)
    messages

let find_draft id drafts =
  List.find_opt (fun (draft : Drafts.t) -> Drafts.Id.equal draft.id id) drafts

let find_attachment name attachments =
  List.find_opt
    (fun (attachment : Attachments.t) -> attachment.name = name)
    attachments

let upload_path attachment =
  match String.split_on_char '/' attachment.Attachments.path_id with
  | realm :: rest when rest <> [] -> (
      match int_of_string_opt realm with
      | Some realm_id -> (realm_id, String.concat "/" rest)
      | None -> Alcotest.failf "non-numeric realm in upload path %S" realm)
  | _ -> Alcotest.fail "upload path lacks realm and filename"

let tiny_png =
  "\137PNG\r\n\
   \026\n\
   \000\000\000\rIHDR\000\000\000\002\000\000\000\002\008\002\000\000\000\253\212\154s\000\000\000\018IDATx\156c\248\207\192\192\000\194\012\255\129\000\000\031\238\005\251\011\217h\139\000\000\000\000IEND\174B\096\130"

let run ~client ~channel ~recipient =
  let scheduled_id = ref None in
  let draft_id = ref None in
  let attachment_id = ref None in
  let message_id = ref None in
  let cleanup () =
    Option.iter
      (fun id ->
        ignore (Scheduled_messages.delete client ~scheduled_message_id:id))
      !scheduled_id;
    Option.iter (fun id -> ignore (Drafts.delete client ~draft_id:id)) !draft_id;
    Option.iter
      (fun id -> ignore (Attachments.delete client ~attachment_id:id))
      !attachment_id;
    Option.iter
      (fun id -> ignore (Messages.delete client ~message_id:id))
      !message_id
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  let now = int_of_float (Unix.gettimeofday ()) in

  let scheduled =
    Scheduled_messages.create_detailed client
      ~destination:(Direct [ recipient ]) ~content:"scheduled parity original"
      ~scheduled_delivery_timestamp:(now + 3600) ()
    |> ok
  in
  scheduled_id := Some scheduled.id;
  let listed = Scheduled_messages.list client |> ok in
  let original =
    match find_scheduled scheduled.id listed.scheduled_messages with
    | Some message -> message
    | None -> Alcotest.fail "created scheduled message was not listed"
  in
  Alcotest.(check string)
    "scheduled content round-trip" "scheduled parity original" original.content;
  Scheduled_messages.update client ~scheduled_message_id:scheduled.id
    {
      Scheduled_messages.destination = None;
      topic = None;
      content = Some "scheduled parity edited";
      scheduled_delivery_timestamp = Some (now + 7200);
    }
  |> ok;
  let updated =
    Scheduled_messages.list client |> ok |> fun page ->
    match find_scheduled scheduled.id page.scheduled_messages with
    | Some message -> message
    | None -> Alcotest.fail "updated scheduled message was not listed"
  in
  Alcotest.(check string)
    "scheduled edit round-trip" "scheduled parity edited" updated.content;
  Alcotest.(check int)
    "scheduled time round-trip" (now + 7200)
    updated.scheduled_delivery_timestamp;
  Scheduled_messages.delete client ~scheduled_message_id:scheduled.id |> ok;
  scheduled_id := None;
  check
    (Option.is_none
       (find_scheduled scheduled.id
          (Scheduled_messages.list client |> ok).scheduled_messages))
    "deleted scheduled message was still listed";

  let draft =
    {
      Drafts.destination = Direct [ recipient ];
      topic = "";
      content = "draft parity original";
      timestamp = None;
    }
  in
  let created_drafts = Drafts.create_detailed client [ draft ] |> ok in
  let created_draft_id =
    match created_drafts.ids with
    | [ id ] -> id
    | ids -> Alcotest.failf "create draft returned %d IDs" (List.length ids)
  in
  draft_id := Some created_draft_id;
  let listed_draft =
    Drafts.list client |> ok |> fun page ->
    Alcotest.(check int)
      "draft response count" (List.length page.drafts) page.count;
    match find_draft created_draft_id page.drafts with
    | Some draft -> draft
    | None -> Alcotest.fail "created draft was not listed"
  in
  Alcotest.(check string)
    "draft content round-trip" "draft parity original" listed_draft.content;
  Drafts.edit client ~draft_id:created_draft_id
    { draft with content = "draft parity edited" }
  |> ok;
  let updated_draft =
    Drafts.list client |> ok |> fun page ->
    match find_draft created_draft_id page.drafts with
    | Some draft -> draft
    | None -> Alcotest.fail "updated draft was not listed"
  in
  Alcotest.(check string)
    "draft edit round-trip" "draft parity edited" updated_draft.content;
  Drafts.delete client ~draft_id:created_draft_id |> ok;
  draft_id := None;
  check
    (Option.is_none
       (find_draft created_draft_id (Drafts.list client |> ok).drafts))
    "deleted draft was still listed";

  let sent_id =
    Messages.send_channel_id client ~channel_id:channel
      ~topic:"ocaml messages parity" ~content:"history original" ()
    |> ok
  in
  message_id := Some sent_id;
  let selected =
    Messages.get_messages client ~message_ids:[ sent_id ] () |> ok
  in
  check
    (List.exists
       (fun (message : Zulip.Message.t) ->
         Zulip.Id.Message.equal message.id sent_id)
       selected.messages)
    "selected-ID history omitted the requested message";
  let dated =
    Messages.get_messages client ~anchor:(Date "2000-01-01") ~num_before:0
      ~num_after:1
      ~narrow:[ Zulip.Narrow.id sent_id ]
      ()
    |> ok
  in
  check
    (List.exists
       (fun (message : Zulip.Message.t) ->
         Zulip.Id.Message.equal message.id sent_id)
       dated.messages)
    "date-anchored history omitted the requested message";
  Messages.edit client ~message_id:sent_id ~content:"history edited" () |> ok;
  let history = Messages.get_history client ~message_id:sent_id () |> ok in
  check (history.edits <> []) "edited message returned empty history";
  let oldest = List.hd history.edits in
  let latest = List.hd (List.rev history.edits) in
  Alcotest.(check (option string))
    "history original snapshot" (Some "history original") oldest.content;
  Alcotest.(check (option string))
    "history edited content" (Some "history edited") latest.content;
  Alcotest.(check (option string))
    "history previous content" (Some "history original") latest.previous_content;

  let flagged =
    Messages.update_flags_for_narrow client
      ~anchor:(Messages.Message_id sent_id) ~num_before:0 ~num_after:0
      ~narrow:[ Zulip.Narrow.id sent_id ]
      ~include_anchor:true ~op:Zulip.Message_flag.Add ~flag:`Starred ()
    |> ok
  in
  check (flagged.processed_count >= 1) "flag-by-narrow processed no messages";
  check (flagged.updated_count >= 1) "flag-by-narrow updated no messages";
  let fetched = Messages.get client ~message_id:sent_id |> ok in
  check
    (List.mem `Starred fetched.flags)
    "starred flag was absent after narrow update";
  ignore
    (Messages.update_flags_for_narrow client
       ~anchor:(Messages.Message_id sent_id) ~num_before:0 ~num_after:0
       ~narrow:[ Zulip.Narrow.id sent_id ]
       ~include_anchor:true ~op:Zulip.Message_flag.Remove ~flag:`Starred ()
    |> ok);

  let receipts = Messages.get_read_receipts client ~message_id:sent_id |> ok in
  check
    (not
       (List.exists
          (Zulip.Id.User.equal (Zulip.Message.sender_id fetched))
          receipts))
    "read receipts unexpectedly included the message sender";

  let filename = Printf.sprintf "ocaml-parity-%d.png" now in
  let upload_uri =
    Attachments.upload_file client ~filename ~content_type:"image/png" tiny_png
    |> ok
  in
  check (upload_uri <> "") "upload returned an empty URI";
  let attachments = Attachments.list client |> ok in
  let attachment =
    match find_attachment filename attachments.attachments with
    | Some attachment -> attachment
    | None -> Alcotest.fail "uploaded attachment was not listed"
  in
  attachment_id := Some attachment.id;
  Alcotest.(check int)
    "uploaded attachment size" (String.length tiny_png) attachment.size;
  Alcotest.(check string)
    "listed attachment path matches upload URI"
    ("/user_uploads/" ^ attachment.path_id)
    upload_uri;
  let realm_id, path_filename = upload_path attachment in
  let temporary =
    Attachments.temporary_url_detailed client ~realm_id ~filename:path_filename
    |> ok
  in
  check (temporary.url <> "") "temporary upload URL was empty";
  let rec wait_for_thumbnail attempts =
    let status =
      Attachments.thumbnail_status client ~realm_id ~filename:path_filename
      |> ok
    in
    if status.has_thumbnail then status
    else if attempts = 0 then
      Alcotest.fail "thumbnail generation did not complete"
    else (
      Unix.sleepf 0.25;
      wait_for_thumbnail (attempts - 1))
  in
  let thumbnail = wait_for_thumbnail 40 in
  Alcotest.(check bool) "image thumbnail is ready" true thumbnail.has_thumbnail;
  Attachments.delete client ~attachment_id:attachment.id |> ok;
  attachment_id := None;
  check
    (Option.is_none
       (find_attachment filename (Attachments.list client |> ok).attachments))
    "deleted attachment was still listed";
  match
    Channels.delete_topic client ~channel_id:channel
      ~topic:"ocaml messages parity"
    |> ok
  with
  | `Complete -> message_id := None
  | `Incomplete -> Alcotest.fail "single-message topic deletion was incomplete"
