# `k-compose`

<br>

The compose window: an address to send from, a file attached from the disk, a
message that leaves the account and is filed as sent.
[**`a-send`**](../a-send#readme) shows the core of it, an Email and an
EmailSubmission created in one request, and this step is the whole task built
around that pair. The frame is the one from
[**`1-session`**](../1-session#readme), with `--to ADDRESS` and `--attach FILE`
of its own; with neither, the step attaches a small file of its own and sends
the message to the account itself. Sending needs
`urn:ietf:params:jmap:submission` as well as core and mail, named in the
program's `capabilities` for the reason [**`a-send`**](../a-send#readme) gives.

<br>

An *Identity* is an address the user may send from, together with the defaults
to apply to a message sent from it
([RFC 8621 §6](https://www.rfc-editor.org/rfc/rfc8621#section-6)). Its `email`
may be the wildcard `*@domain`, which permits any local part at that domain, so
a client that wants a concrete From address expands it.
[`Proto.Identity.sending_address`](../../lib/mail/mail_identity.mli) is that
address, the `email` of the Identity with `*` replaced by the local part given.
It is `None` when the Identity has no address, which is outside the
specification and is what the Cyrus oracle sends, leaving the session username
as the only thing to go on.

```ocaml
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
```

[`Chain.identity_get`](../../lib/core/chain.mli) with no `ids` fetches every
Identity of the account, and the first is the one to send as unless the user
picks another. A `/get` returns the `id` of every record it answers with, so
`Option.get` on it cannot raise.

<br>

An attachment is a blob uploaded before the message is composed
([RFC 8620 §6.1](https://www.rfc-editor.org/rfc/rfc8620#section-6.1)). The
upload is a POST to the session's `uploadUrl`, outside the JMAP request
envelope altogether, and it answers with the `blobId` to refer to the bytes by.

```ocaml
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
```

[`Client.upload_flow_exn`](../../eio/client.mli) sends the bytes as they come
off the disk instead of holding the file in memory, with the size from
`Eio.File.size` as the `Content-Length` so the server need not buffer a chunked
body. The media type given at upload is the one the body part carries; a real
compose window guesses it from the name. Without `--attach` the step writes a
small file to a temporary path, uploads that and unlinks it.

RFC 8620 §6.1 lets a server delete a blob that nothing refers to, so the upload
and the `Email/set` that refers to it belong together; once a message carries
the blob it stays for as long as the message does.

<br>

```ocaml
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
```

[`Sync.mailbox_id_exn`](../../eio/sync.mli) looks up by role the three Mailboxes
the task needs, as in [**`a-send`**](../a-send#readme).

An Email with an attachment is composed from body parts rather than by
[`Proto.Email.create`](../../lib/mail/mail_email.mli), which offers a plain
text or HTML body and nothing more.
[RFC 8621 §4.6](https://www.rfc-editor.org/rfc/rfc8621#section-4.6) builds the
message from a `bodyStructure` of EmailBodyParts plus a `bodyValues` map keyed
by `partId`. The text part names a `partId` whose content is in `bodyValues`,
the attachment part names a `blobId` with `disposition` `attachment` and the
filename to present
([RFC 8621 §4.1.4](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.4)), and
the `multipart/mixed` part over the two of them is the message the server
builds. [`Proto.Email.v`](../../lib/mail/mail_email.mli) takes the properties
of such a create as they are.

<br>

```ocaml
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
```

An [EmailSubmission](../../lib/mail/mail_submission.mli) is the record of a
message being sent
([RFC 8621 §7](https://www.rfc-editor.org/rfc/rfc8621#section-7)), and creating
one is what sends the message. `envelope` overrides the SMTP `MAIL FROM` and
`RCPT TO` the server would otherwise derive from the header fields, which is how
a message reaches an address that appears in no header, such as a Bcc recipient.

`onSuccessUpdateEmail` is what turns a draft into a sent message
([RFC 8621 §7.5](https://www.rfc-editor.org/rfc/rfc8621#section-7.5)). It maps
the submission to a PatchObject the server applies with an implicit `Email/set`
once the submission succeeds, so the move out of Drafts, into Sent and away
from `$draft` is the server's own atomic act rather than a race the client runs
afterwards. Both calls travel in one request, and since the Email has no id
until the request runs, the submission and the patch name their records by
`Proto.Id.creation_ref` of the tokens the `create` maps are keyed by.

The response itself is wanted here as well as the two decoded replies, so the
request goes out with [`Client.run_with_response_exn`](../../eio/client.mli),
which reads the handles the chain ends in and hands the response back beside
them.

```ocaml
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
```

[`Proto.Method.created`](../../lib/proto/proto_method.mli) reads each new record
back under the creation id it was given, holding the properties the server set
on it. `undoStatus` is one of them: `pending` means an update to `canceled`
might still unsend the message and `final` means it is beyond recall, and
`sendAt` is when the message was handed to the SMTP server (RFC 8621 §7).

The implicit `Email/set` is visible in the response.
[RFC 8620 §3.2](https://www.rfc-editor.org/rfc/rfc8620#section-3.2) lets one
method call answer with more than one response, each carrying the call id of the
call that produced it rather than one of its own, so the two calls sent come
back as three entries in the response. `Client.run_with_response_exn` hands
back the handles beside the results and the response, and
[`Proto.Response.find_responses`](../../lib/proto/proto_response.mli) under
the call id of the submission's handle finds the `Email/set` the submission
made.

<br>

```ocaml
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
```

RFC 8621 §7 lets a server destroy an EmailSubmission "at any time after the
message is successfully sent", so an `EmailSubmission/get` finding nothing is
not a failure. The oracle discards it before the next request arrives.

The `Email/get` beside it is the check that the submission did what it promised,
reading the properties the implicit `Email/set` changed. The step fails unless
[`Proto.Email.in_mailbox`](../../lib/mail/mail_email.mli) finds Sent among the
mailboxes the message is in, `mailboxIds` mapping that id to `true`, and
[`Proto.Email.has_keyword`](../../lib/mail/mail_email.mli) no longer answers for
`$draft`. `hasAttachment` is set by the server from the body structure it
built.

<br>

```ocaml
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
```

With no `--to` the message goes to the account's own address, so a second copy
of it arrives in the Inbox by ordinary mail delivery, outside JMAP altogether.
The step waits for it with the polling query
[**`a-send`**](../a-send#readme) explains, then destroys the filed message and
every copy in one `Email/set`, leaving the account as it was found.

<pre><code><b>$ dune exec -- examples/k-compose/compose.exe --allow-insecure</b>
identity  user1 sends as user1@example.com
blob      Gad5f1b103deb1019681d230b0439e77294498c90 parts.csv (34 bytes)
submitted S112 undoStatus=final sendAt=2026-09-03T00:19:23Z
filed     by the implicit Email/set
read back the server has already discarded the submission
message   is in Sent, hasAttachment=true
destroyed Me59205ca2c095ee647747218, and 1 copy delivered to the Inbox
</code></pre>

The subject carries the process id, so runs never collide and the step may be
run as often as you like.

<br>

**Next steps:**

- The next example, [**`l-parse`**](../l-parse#readme), goes the other way and
  turns a whole RFC 5322 message into an Email.
- [**`p-stream`**](../p-stream#readme) returns to blobs and streams a large one
  in both directions.

<br>

[Up to the tutorial index](../#readme)
