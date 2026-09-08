# `a-send`

<br>

JMAP has no method that sends a message. A client saves an Email and then
creates an *EmailSubmission* for it, and the server does the sending and the
filing. This step composes a one line message to the account's own address,
submits it, reads the submission back, and destroys every copy it left behind.
The frame is the one from [**`1-session`**](../1-session#readme).

Every other step lets [`Client.call`](../../eio/client.mli) fill the `using`
array in from the session. This one names the three capabilities itself, so
that a server without `urn:ietf:params:jmap:submission` refuses the whole
request with `unknownCapability` rather than answering `unknownMethod` for the
one call that mattered.

```ocaml
let capabilities =
  Proto.[ Capability.core; Capability.mail; Capability.submission ]

let undo = Option.fold ~none:"-" ~some:Proto.Submission.undo_status_to_string
```

<br>

The address to send from is not the client's to choose. An *Identity* is one
address the user may send as, together with the defaults to apply to a message
sent from it
([RFC 8621 §6](https://www.rfc-editor.org/rfc/rfc8621#section-6)), and
[`Chain.identity_get`](../../lib/core/chain.mli) with no `ids` fetches them
all. A `/get` returns the `id` of every record it answers with, so `Option.get`
on the Identity's id cannot raise.

```ocaml
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
```

The Identity also carries the address itself, which
[`Proto.Identity.sending_address`](../../lib/mail/mail_identity.mli) resolves.
§6 lets that address be `*@domain`, standing for any local part at that domain,
and `sending_address` puts the local part it is given in place of the `*`. It is
`None` for an Identity with no address, which is what the oracle sends, so the
step falls back to the session username under the oracle's domain.

Three mailboxes are looked up by role. The draft has to be filed somewhere, the
submission has to move it somewhere else, and the copy that comes back by mail
has to be found afterwards.

```ocaml
  let drafts = Sync.mailbox_id_exn client ~account_id `Drafts
  and sent = Sync.mailbox_id_exn client ~account_id `Sent
  and inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
```

[`Sync.mailbox_id_exn`](../../eio/sync.mli) is a `Mailbox/query` filtered on the
role followed by a `Mailbox/get` of the ids it returns, in one request.
[RFC 8621 §2](https://www.rfc-editor.org/rfc/rfc8621#section-2) gives an account
at most one Mailbox per role, so an account missing one of these three is a
failure rather than an empty result, which is what the `_exn` form raises.

[`Proto.Email.create`](../../lib/mail/mail_email.mli) builds the record of an
`Email/set` `create`, a [`Proto.Email.t`](../../lib/mail/mail_email.mli) that
[`Chain.email_set`](../../lib/core/chain.mli) encodes. It exposes only the
properties a client may set
([RFC 8620 §5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3) requires
the rest to be omitted), and a `text_body` becomes both the `bodyValues` entry
and the `textBody` part of
[RFC 8621 §4.6](https://www.rfc-editor.org/rfc/rfc8621#section-4.6). The
`$draft` keyword marks a message as not yet sent
([RFC 8621 §4.1.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.1)).

```ocaml
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
```

Sending it is creating an
[EmailSubmission](../../lib/mail/mail_submission.mli) for it
([RFC 8621 §7](https://www.rfc-editor.org/rfc/rfc8621#section-7)).
`Proto.Submission.create` sets the three properties a client may set. The Email
it names is the draft made a moment earlier in the same request, so it is named
by `Proto.Id.creation_ref` of the same token, the creation reference of
[**`8-mailboxes-set`**](../8-mailboxes-set#readme), rather than by an id that
does not exist yet. With no `envelope` the server derives the SMTP `MAIL FROM`
and `RCPT TO` from the header fields.

`onSuccessUpdateEmail` is what turns a draft into a sent message
([RFC 8621 §7.5](https://www.rfc-editor.org/rfc/rfc8621#section-7.5)). It maps
the submission to a PatchObject the server applies with an implicit `Email/set`
once the submission succeeds, so the move out of Drafts, into Sent and away from
`$draft` is the server's single atomic act rather than a race the client runs
afterwards. The patch is built from
[`Proto.Email.Patch`](../../lib/mail/mail_email.mli) entries, whose paths are
the ones RFC 8620 §5.3 defines for a PatchObject. Its keys are EmailSubmission
ids, so the submission being created is named by its creation reference too.

Two responses are wanted, so the chain ends in `Handles.[ eh; sh ]` and
[`Client.run_exn`](../../eio/client.mli) answers with the matching
`Results.[ saved; submitted ]`, as [**`3-inbox`**](../3-inbox#readme)
describes.

```ocaml
  (match Proto.Method.(set_failures saved @ set_failures submitted) with
  | f :: _ -> Fmt.failwith "not created: %a" Proto.Method.pp_set_failure f
  | [] -> ());
  let draft_made = Option.get (Proto.Method.created saved draft)
  and submission = Option.get (Proto.Method.created submitted submit) in
  let email_id = Option.get (Proto.Email.id draft_made)
  and submission_id = Option.get (Proto.Submission.id submission) in
  Fmt.pr "submitted %a undoStatus=%s@." Proto.Id.pp submission_id
    (undo submission.undo_status);
```

[`Proto.Method.set_failures`](../../lib/proto/proto_method.mli) gathers the
per record failures of a `/set`, which [**`9-errors`**](../9-errors#readme) is
about. Nothing failed here, so both records exist and
[`Proto.Method.created`](../../lib/proto/proto_method.mli) reads each back under
the token it was created with, holding the properties the server set on it.

`undoStatus` is one of those properties. `pending` means an update to `canceled`
might still unsend the message and `final` means it is beyond recall (RFC 8621
§7). The same section lets a server destroy an EmailSubmission "at any time
after the message is successfully sent", and the oracle forgets it before the
next request arrives, so the `EmailSubmission/get` that follows finds nothing.

The message was addressed to the account itself, so a second copy of it arrives
in the Inbox by ordinary mail delivery, some time after the submission and
outside JMAP altogether. The step waits for it.

```ocaml
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
```

`newest` is the query and get of [**`3-inbox`**](../3-inbox#readme), filtered on
`inMailbox` and limited to the twenty most recent messages. The obvious filter
is the subject, which the step deliberately does not use. The Cyrus container
has no search index, so a `subject` or `text` filter there matches nothing at
all, whereas `inMailbox` is answered from the mailbox itself. The step fetches
those twenty subjects once a second until its own copy shows up.

One last `Email/set` then destroys the filed message and every copy together, by
concrete ids rather than by a result reference, which is what
[`Chain.ids`](../../lib/core/chain.mli) takes. The subject carries the process
id, so two runs never collide and the step may be run as often as you like.

<pre><code><b>$ dune exec -- examples/a-send/send.exe --allow-insecure</b>
identity  user1 sends as user1@example.com
submitted S102 undoStatus=final
read back the server has already discarded the submission
destroyed Medd1806e42968bca4a7fe07e, and 1 copy delivered to the Inbox
</code></pre>

<br>

**Next steps:**

- The next example, [**`b-blobs`**](../b-blobs#readme), puts bytes on the server
  and takes them back, which is how an attachment reaches a message.
- [**`c-import`**](../c-import#readme) goes the other way and turns a whole
  RFC 5322 message into an Email.

<br>

**See also:**

- [**`k-compose`**](../k-compose#readme) is the same task with an attachment,
  a chosen recipient and a full report of where the message ended up.

<br>

[Up to the tutorial index](../#readme)
