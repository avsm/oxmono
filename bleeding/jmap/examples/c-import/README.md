# `c-import`

<br>

A blob holding an RFC 5322 message can become mail in two ways. `Email/import`
files it into a mailbox and it is a message from then on, and `Email/parse`
reads it and hands back an Email object that is never stored. This step does
both to the same blob in one request, then destroys what it imported. The frame
is the one from [**`1-session`**](../1-session#readme).

```ocaml
  let inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
  let subject = Printf.sprintf "tutorial-import-%d" (Unix.getpid ()) in
  let blob =
    Client.upload_exn client ~account_id ~content_type:"message/rfc822"
      ~data:(message subject)
  in
  let blob_id = blob.blob_id in
  Fmt.pr "uploaded  %Ld octets as blob %a@." blob.size Proto.Id.pp blob_id;
```

<br>

Both methods start from a blob.
[RFC 8621 §4.8](https://www.rfc-editor.org/rfc/rfc8621#section-4.8) is explicit
that the messages "must first be uploaded as blobs using the standard upload
mechanism", which is the POST of
[RFC 8620 §6.1](https://www.rfc-editor.org/rfc/rfc8620#section-6.1) that
[**`b-blobs`**](../b-blobs#readme) covers. The step builds a short message as a
string with CRLF line endings, as RFC 5322 requires, and uploads it with
[`Client.upload_exn`](../../eio/client.mli) as `message/rfc822`.

An import must say where to file the message, so the Inbox is looked up first
with [`Sync.mailbox_id_exn`](../../eio/sync.mli), a `Mailbox/query` filtered on
the role followed by a `Mailbox/get` of the ids it returns, in one request.
[RFC 8621 §2](https://www.rfc-editor.org/rfc/rfc8621#section-2) gives an account
at most one Mailbox per role, so an account with no Inbox is a failure rather
than an empty result.

```ocaml
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
```

[`Chain.email_import`](../../lib/core/chain.mli) takes a map keyed by the
creation tokens of [**`8-mailboxes-set`**](../8-mailboxes-set#readme), here
[`Proto.Email.creation`](../../lib/mail/mail_email.mli) since the records it
names are Emails, whose values are [`Proto.Email.Import.email`](../../lib/mail/mail_email.mli) records
naming the blob and saying where to file the new message and with which
keywords. An import is a create like any other, so its results are split into
`created` and `notCreated`, read here with `Proto.Email.Import.created` and
`Proto.Email.Import.not_created` under the same token. Importing the same bytes
twice earns an `alreadyExists` SetError carrying the id of the message already
there, one of the SetErrors [**`9-errors`**](../9-errors#readme) is about.
Nothing is reported under `notCreated` here, so the `Option.get` on `created`
cannot raise.

```ocaml
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
```

[`Chain.email_parse`](../../lib/core/chain.mli) does not touch the mail store.
[RFC 8621 §4.9](https://www.rfc-editor.org/rfc/rfc8621#section-4.9) has it exist
so that a client "can parse and display attached messages without having to
import them as top-level Email objects in the mail store in their own right", so
the usual use is on the `blobId` of a `message/rfc822` part an `Email/get`
returned. It takes the same `properties` and body arguments as an `Email/get`,
and `Proto.Email.Parse.parsed` looks a result up by blob id, there being no
message id to look it up by.

Because nothing is stored, that section makes `id`, `mailboxIds`, `keywords` and
`receivedAt` null on a parsed Email. Cyrus does not do that here. The
`Email/import` earlier in this very request has already made the blob into a
stored message, and the server answers the parse with that message's own id, as
the output below shows. Ask it in a request of its own, or about a blob that was
never imported, and the fields come back null.

```ocaml
  let destroyed =
    Client.call_exn client
      (Chain.email_set ~account_id ~destroy:(Chain.id email_id) ())
  in
  (match Proto.Method.set_failures destroyed with
  | [] -> ()
  | f :: _ -> Fmt.failwith "left behind: %a" Proto.Method.pp_set_failure f);
  Fmt.pr "destroyed %a@." Proto.Id.pp email_id
```

Everything the step creates it destroys, and
[`Proto.Method.set_failures`](../../lib/proto/proto_method.mli) is the whole
question of whether the `Email/set` did what it was asked. The subject and the
`Message-ID` carry the process id, so no two runs upload the same bytes and the
import never meets its own leftovers.

<pre><code><b>$ dune exec -- examples/c-import/import.exe --allow-insecure</b>
uploaded  270 octets as blob G59504b9fccaaf50781806e078981650972c8ee75
imported  M59504b9fccaaf50781806e07 into the inbox, state now 1880
parsed    "tutorial-import-883890" from Alice <alice@example.org>, id=M59504b9fccaaf50781806e07
destroyed M59504b9fccaaf50781806e07
</code></pre>

The `state` an import reports is the account's Email state after the call, the
same string a `/get` returns and the one an
[**`e-changes`**](../e-changes#readme) call would start from.

<br>

**Next steps:**

- The next example, [**`d-paging`**](../d-paging#readme), stops asking for
  everything at once and starts respecting the limits the server publishes.
- [**`e-changes`**](../e-changes#readme) uses the state string this step printed
  to ask what has happened since.

<br>

**See also:**

- [**`l-parse`**](../l-parse#readme) imports a message file of your
  choosing and then parses the forwarded message inside it.

<br>

[Up to the tutorial index](../#readme)
