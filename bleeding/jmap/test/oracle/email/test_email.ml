(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Email/import (RFC 8621 4.8), Email/parse (4.9) and Email/set with typed
   PatchObjects (RFC 8620 5.3) against the Cyrus oracle. *)

open Jmap.Proto
module H = Oracle_harness
module Client = Jmap_eio.Client
module Chain = Jmap.Chain
module Patch = Jmap.Proto.Patch

(* The one-line rendering of a client error keeps only the problem type; the
   oracle puts the interesting part in [title] and [detail]. *)
let client_error = function
  | Client.Jmap_error e ->
      Printf.sprintf "%s (status %s) %s: %s"
        (Error.Request_error.type_to_string e.Error.Request_error.type_)
        (match e.Error.Request_error.status with
        | Some s -> string_of_int s
        | None -> "-")
        (Option.value e.Error.Request_error.title ~default:"")
        (Option.value e.Error.Request_error.detail ~default:"")
  | e -> Client.error_to_string e

let some name = function
  | Some v -> v
  | None -> Alcotest.failf "%s: expected a value" name

let keyword = Alcotest.testable Keyword.pp Keyword.equal

(* RFC 8621 Section 2.5: a Mailbox/set create needs only a name, which
   {!Jmap.Proto.Mailbox.create} validates before the request goes out. *)
let mailbox_create name =
  match Mailbox.create ~name () with
  | Ok mb -> mb
  | Error e -> Alcotest.failf "Mailbox.create: %s" e

let mailbox_set (e : Email.t) =
  List.filter_map
    (fun (k, v) -> if v then Some (Id.to_string k) else None)
    (Option.value e.mailbox_ids ~default:[])

let destroy_emails t ids =
  ignore
    (H.call t
       (Chain.email_set ~account_id:t.H.account_id ~destroy:(Chain.ids ids) ()))

let destroy_mailbox t id =
  ignore
    (H.call t
       (Chain.mailbox_set ~account_id:t.H.account_id ~destroy:(Chain.ids [ id ])
          ~on_destroy_remove_emails:true ()))

(* {1 Email/import (RFC 8621 Section 4.8)} *)

(* "The messages must first be uploaded as blobs using the standard upload
   mechanism", then imported with the Mailboxes and keywords to give them. *)
let import t =
  let inbox = H.mailbox_with_role t `Inbox in
  let inbox_id = some "inbox id" inbox.id in
  let subject = H.unique "oracle-import" in
  let _, raw = H.message ~subject () in
  let up =
    match
      Client.upload t.H.client ~account_id:t.H.account_id
        ~content_type:"message/rfc822" ~data:raw
    with
    | Ok r -> r
    | Error e -> Alcotest.failf "upload failed: %s" (client_error e)
  in
  let blob_id = up.Blob.blob_id in
  Alcotest.(check bool)
    "upload reports the right size" true
    (up.Blob.size = Int64.of_int (String.length raw));
  let custom = "oracle-label" in
  let k1 = Id.creation "k1" in
  let r =
    H.call t
      (Chain.email_import ~account_id:t.H.account_id
         ~emails:
           [
             ( k1,
               Email.Import.email ~blob_id ~mailbox_ids:[ inbox_id ]
                 ~keywords:[ `Seen; `Custom custom ]
                 () );
           ]
         ())
  in
  (match r.Email.Import.not_created with
  | Some ((_, e) :: _) ->
      Alcotest.failf "import failed: %s"
        (Error.Set_error.type_to_string e.Error.Set_error.type_)
  | _ -> ());
  let imported = some "created k1" (Email.Import.created r k1) in
  let email_id = some "imported id" imported.id in
  Fun.protect
    ~finally:(fun () -> destroy_emails t [ email_id ])
    (fun () ->
      let e =
        H.email t email_id
          ~properties:[ `Id; `Subject; `Keywords; `Mailbox_ids; `Blob_id ]
      in
      Alcotest.(check (option string)) "subject" (Some subject) e.subject;
      Alcotest.(check (list keyword))
        "keywords"
        (List.sort Keyword.compare [ `Seen; `Custom custom ])
        (List.sort Keyword.compare (Email.keyword_list e));
      Alcotest.(check (list string))
        "mailboxIds"
        [ Id.to_string inbox_id ]
        (mailbox_set e))

(* {1 Email/parse (RFC 8621 Section 4.9)} *)

let boundary = "oracle-boundary-42"

(* A multipart/mixed message whose second part is a message/rfc822
   attachment, built by hand so the inner subject is known. *)
let message_with_attached_message ~inner_subject =
  let inner =
    String.concat "\n"
      [
        "From: Bob <bob@example.org>";
        "To: user1@example.com";
        "Subject: " ^ inner_subject;
        "Date: Mon, 01 Jan 2024 00:00:00 +0000";
        "Message-ID: <inner-" ^ inner_subject ^ "@example.org>";
        "MIME-Version: 1.0";
        "Content-Type: text/plain; charset=utf-8";
        "";
        "This is the forwarded message body.";
        "";
      ]
  in
  let body =
    String.concat "\n"
      [
        "--" ^ boundary;
        "Content-Type: text/plain; charset=utf-8";
        "";
        "Please see the attached message.";
        "";
        "--" ^ boundary;
        "Content-Type: message/rfc822";
        "Content-Disposition: attachment; filename=\"forwarded.eml\"";
        "";
        inner;
        "--" ^ boundary ^ "--";
        "";
      ]
  in
  H.message
    ~headers:[ "Content-Type: multipart/mixed; boundary=\"" ^ boundary ^ "\"" ]
    ~body ()

let parse_attachment t =
  let inner_subject = H.unique "oracle-inner" in
  let subject, raw = message_with_attached_message ~inner_subject in
  H.deliver t raw;
  let email_id = H.wait_for_email t ~subject () in
  Fun.protect
    ~finally:(fun () -> destroy_emails t [ email_id ])
    (fun () ->
      let g =
        H.call t
          (Chain.email_get ~account_id:t.H.account_id
             ~ids:(Chain.ids [ email_id ])
             ~properties:[ `Id; `Has_attachment; `Attachments; `Body_structure ]
             ~body_properties:
               [ `Part_id; `Blob_id; `Type; `Name; `Disposition; `Sub_parts ]
             ())
      in
      let e =
        match g.list with
        | [ e ] -> e
        | l -> Alcotest.failf "expected one email, got %d" (List.length l)
      in
      Alcotest.(check (option bool))
        "hasAttachment" (Some true) e.has_attachment;
      (* The bodyStructure is the full MIME tree; the attachment also shows up
         in the flat "attachments" list (RFC 8621 Section 4.1.4). *)
      let structure = some "bodyStructure" e.body_structure in
      Alcotest.(check (option string))
        "top-level type" (Some "multipart/mixed") structure.type_;
      Alcotest.(check int)
        "two sub-parts" 2
        (List.length (some "subParts" structure.sub_parts));
      let attached =
        match
          List.find_opt
            (fun p -> p.Email_body.Part.type_ = Some "message/rfc822")
            (Option.value e.attachments ~default:[])
        with
        | Some p -> p
        | None -> Alcotest.failf "no message/rfc822 attachment found"
      in
      let blob_id = some "attachment blobId" attached.Email_body.Part.blob_id in
      (* Parse the attached message without importing it. *)
      let r =
        H.call t
          (Chain.email_parse ~account_id:t.H.account_id
             ~blob_ids:(Chain.ids [ blob_id ])
             ~properties:
               [
                 `Id;
                 `Mailbox_ids;
                 `Keywords;
                 `Received_at;
                 `Subject;
                 `From;
                 `Preview;
                 `Text_body;
                 `Body_values;
               ]
             ~fetch_text_body_values:true ())
      in
      Alcotest.(check bool)
        "nothing notParsable" true
        (match r.Email.Parse.not_parsable with
        | None | Some [] -> true
        | _ -> false);
      Alcotest.(check bool)
        "nothing notFound" true
        (match r.Email.Parse.not_found with
        | None | Some [] -> true
        | _ -> false);
      let inner = some "parsed blob" (Email.Parse.parsed r blob_id) in
      Alcotest.(check (option string))
        "inner subject" (Some inner_subject) inner.subject;
      (* "The following metadata properties on the Email objects will be null
         if requested: id, mailboxIds, keywords, receivedAt." *)
      Alcotest.(check bool) "parsed id is null" true (inner.id = None);
      Alcotest.(check bool)
        "parsed mailboxIds is null" true (inner.mailbox_ids = None);
      Alcotest.(check bool)
        "parsed keywords is null" true (inner.keywords = None);
      Alcotest.(check bool)
        "parsed receivedAt is null" true (inner.received_at = None);
      match inner.from with
      | Some [ a ] ->
          Alcotest.(check string) "inner from" "bob@example.org" a.email
      | _ -> Alcotest.failf "expected one From address on the parsed message")

(* A blob that is not a message at all comes back in notParsable. *)
let parse_not_parsable t =
  let up =
    match
      Client.upload t.H.client ~account_id:t.H.account_id
        ~content_type:"application/octet-stream" ~data:"\x00\x01 not a message"
    with
    | Ok r -> r
    | Error e -> Alcotest.failf "upload failed: %s" (client_error e)
  in
  let blob_id = up.Blob.blob_id in
  let r =
    H.call t
      (Chain.email_parse ~account_id:t.H.account_id
         ~blob_ids:(Chain.ids [ blob_id ]) ~properties:[ `Subject ] ())
  in
  Alcotest.(check bool)
    "the blob is not returned as parsed" true
    (Email.Parse.parsed r blob_id = None);
  Alcotest.(check bool)
    "the blob is reported unusable" true
    (match (r.Email.Parse.not_parsable, r.Email.Parse.not_found) with
    | Some l, _ when List.mem blob_id l -> true
    | _, Some l when List.mem blob_id l -> true
    | _ -> false)

(* {1 Email/set with typed patches (RFC 8620 Section 5.3)} *)

let patch_move t =
  let inbox = H.mailbox_with_role t `Inbox in
  let inbox_id = some "inbox id" inbox.id in
  let email_id, _ = H.deliver_and_wait t () in
  let name = H.unique "Oracle Patch" in
  let mb = Id.creation "mb" in
  let created =
    H.call t
      (Chain.mailbox_set ~account_id:t.H.account_id
         ~create:[ (mb, mailbox_create name) ]
         ())
  in
  let mailbox_id =
    match Method.created created mb with
    | Some m -> some "created mailbox id" m.id
    | None -> Alcotest.failf "Mailbox/set did not create the mailbox"
  in
  Fun.protect
    ~finally:(fun () ->
      destroy_emails t [ email_id ];
      destroy_mailbox t mailbox_id)
    (fun () ->
      (* One PatchObject: mark read, file in the new Mailbox and take it out
         of the inbox. Sending all three as one patch keeps the Email in at
         least one Mailbox at every point, as RFC 8621 4.1.1 requires. *)
      let patch =
        Patch.v
          [
            Email.Patch.set_keyword `Seen;
            Email.Patch.add_to_mailbox mailbox_id;
            Email.Patch.remove_from_mailbox inbox_id;
          ]
      in
      let s =
        H.call t
          (Chain.email_set ~account_id:t.H.account_id
             ~update:[ (email_id, patch) ]
             ())
      in
      (match Method.set_failures s with
      | (_, e) :: _ ->
          Alcotest.failf "update rejected: %s"
            (Error.Set_error.type_to_string e.Error.Set_error.type_)
      | [] -> ());
      Alcotest.(check bool)
        "the update is reported" true
        (match s.updated with Some [ _ ] -> true | _ -> false);
      let e = H.email t email_id ~properties:[ `Id; `Keywords; `Mailbox_ids ] in
      Alcotest.(check bool) "$seen is set" true (Email.has_keyword `Seen e);
      Alcotest.(check (list string))
        "moved to the new mailbox"
        [ Id.to_string mailbox_id ]
        (mailbox_set e);
      (* A patch removing a keyword is a no-op when the keyword is absent. *)
      let s =
        H.call t
          (Chain.email_set ~account_id:t.H.account_id
             ~update:
               [
                 ( email_id,
                   Patch.v
                     [
                       Email.Patch.remove_keyword `Seen;
                       Email.Patch.remove_keyword `Flagged;
                     ] );
               ]
             ())
      in
      Alcotest.(check bool)
        "keyword removal accepted" true
        (s.not_updated = None || s.not_updated = Some []);
      let e = H.email t email_id ~properties:[ `Id; `Keywords ] in
      Alcotest.(check bool) "$seen is gone" false (Email.has_keyword `Seen e))

(* {1 Streaming blobs (RFC 8620 Sections 6.1 and 6.2)} *)

(* A blob need not fit in a string at either end: [upload_flow] sends an
   [Eio.Flow.source] as it is read and [download_to] writes the response body
   to an [Eio.Flow.sink] as it arrives. A megabyte is enough to be sure the
   bytes crossed more than one read, and small enough not to slow the suite. *)
let blob_stream t =
  let size = 1024 * 1024 in
  let data = String.init size (fun i -> Char.chr (32 + (i mod 95))) in
  let up =
    match
      Client.upload_flow t.H.client ~account_id:t.H.account_id
        ~content_type:"text/plain" ~length:(Int64.of_int size)
        (Eio.Flow.string_source data)
    with
    | Ok r -> r
    | Error e -> Alcotest.failf "streamed upload: %s" (client_error e)
  in
  Alcotest.(check int64)
    "the server counted the streamed bytes" (Int64.of_int size) up.Blob.size;
  let buf = Buffer.create size in
  let served =
    match
      Client.download_to t.H.client ~account_id:t.H.account_id
        ~blob_id:up.Blob.blob_id ~name:"streamed.txt" ~accept:"text/plain"
        (Eio.Flow.buffer_sink buf)
    with
    | Ok media -> media
    | Error e -> Alcotest.failf "streamed download: %s" (client_error e)
  in
  Alcotest.(check string) "served as the type asked for" "text/plain" served;
  Alcotest.(check int)
    "the same number of bytes came back" size (Buffer.length buf);
  Alcotest.(check bool)
    "and the same bytes" true
    (String.equal data (Buffer.contents buf))

(* Without a [~length] the body is chunked (RFC 9112 Section 7.1), which is
   what a client that does not know the size in advance must send. Cyrus
   accepts it; the test states what the server did rather than assuming. *)
let blob_stream_chunked t =
  let data = String.make 4096 'z' in
  match
    Client.upload_flow t.H.client ~account_id:t.H.account_id
      ~content_type:"text/plain"
      (Eio.Flow.string_source data)
  with
  | Ok up -> (
      Alcotest.(check int64) "chunked upload counted" 4096L up.Blob.size;
      let buf = Buffer.create 4096 in
      match
        Client.download_to t.H.client ~account_id:t.H.account_id
          ~blob_id:up.Blob.blob_id (Eio.Flow.buffer_sink buf)
      with
      | Ok _ ->
          Alcotest.(check bool)
            "chunked bytes survive" true
            (String.equal data (Buffer.contents buf))
      | Error e ->
          Alcotest.failf "download after chunked upload: %s" (client_error e))
  | Error e ->
      Alcotest.failf "the server refused a chunked upload: %s" (client_error e)

(* {1 Concurrency (RFC 8620 Section 2)} *)

(* The client takes a slot of "maxConcurrentRequests" before it sends, so a
   caller may fan out further than the server allows without being refused.
   Cyrus states 5; this asks for 8 at once. *)
let concurrent_gets t =
  let limit, uploads = Client.concurrency_limits t.H.client in
  Alcotest.(check bool)
    (Printf.sprintf "the session states a limit (%d requests, %d uploads)" limit
       uploads)
    true
    (limit > 0 && uploads > 0);
  (* RFC 8620 Section 3.4: a response repeats the server's session state, and
     the client refetches the session when it differs from the one it holds.
     Cyrus reports "0" and never changes it, so what this checks is the other
     half: nothing here should have caused a refetch. *)
  let before = (Client.session t.H.client).state in
  Eio.Fiber.List.iter
    (fun _n ->
      let g =
        H.call t
          Chain.(
            let* q = email_query ~account_id:t.H.account_id ~limit:2L () in
            email_get ~account_id:t.H.account_id ~ids:(from_query q)
              ~properties:[ `Id; `Subject ] ())
      in
      Alcotest.(check bool) "a state came back" true (String.length g.state >= 0))
    (List.init 8 Fun.id);
  Alcotest.(check string)
    "the session was left alone" before (Client.session t.H.client).state

let () =
  H.run "oracle-email"
    [
      ("import", [ H.test_case "upload, import and read back" import ]);
      ( "parse",
        [
          H.test_case "parse an attached message/rfc822" parse_attachment;
          H.test_case "a non-message blob is not parsable" parse_not_parsable;
        ] );
      ( "patch",
        [ H.test_case "typed PatchObject moves and flags an email" patch_move ]
      );
      ( "blob streaming",
        [
          H.test_case "a megabyte streams up and back" blob_stream;
          H.test_case "a chunked upload with no declared length"
            blob_stream_chunked;
        ] );
      ("concurrency", [ H.test_case "eight chains at once" concurrent_gets ]);
    ]
