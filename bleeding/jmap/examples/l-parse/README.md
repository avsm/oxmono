# `l-parse`

<br>

A message file goes into the account as real mail, and the message forwarded
inside it is read back without ever being stored.
[**`c-import`**](../c-import#readme) introduces `Email/import` and `Email/parse`
one at a time; this step is the whole task, from a file on disk to the text of
the attached message. The frame is the one from
[**`1-session`**](../1-session#readme), with an option of its own.

```ocaml
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
```

An import starts from a blob, as
[RFC 8621 §4.8](https://www.rfc-editor.org/rfc/rfc8621#section-4.8) requires.
`--file EML` names an RFC 5322 message on disk, and the step uploads a built-in
forwarded message without it. A file is streamed with
[`Client.upload_flow_exn`](../../eio/client.mli), whose `length` is the
`Eio.File.size` of the open file, so an archive of any size is uploaded without
being held in memory. The built-in sample is already a string and goes through
`Client.upload_exn`. [`Cli.main'`](../../eio/cli.mli) is `Cli.main` for a step
that takes options of its own.

<br>

```ocaml
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
```

A migration tool files its messages somewhere of its own, so one request creates
the mailbox and imports into it.
[RFC 8620 §5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3) lets a later
call of the same request name a record an earlier one created by its creation id
prefixed with `#`, which is what
[`Proto.Id.creation_ref`](../../lib/proto/proto_id.mli) writes. The `mailboxIds`
of the [`Proto.Email.Import.email`](../../lib/mail/mail_email.mli) therefore
names a mailbox that does not exist when the request is sent, and RFC 8621 §4.8
requires at least one. Importing bytes the account already holds fails instead
with an `alreadyExists` SetError naming the message already there, read with
`Proto.Email.Import.not_created` as in [**`c-import`**](../c-import#readme).

```ocaml
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
```

Destroying the mailbox with `~on_destroy_remove_emails:true`, the extra argument
of [RFC 8621 §2.5](https://www.rfc-editor.org/rfc/rfc8621#section-2.5), takes
the imported message with it, so one call returns the account to the state it
was found in. `Fun.protect` runs it whether or not the rest of the step
succeeds, and
[`Proto.Method.set_failures`](../../lib/proto/proto_method.mli) is the whole
question of whether it did what it was asked.

<br>

```ocaml
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
```

The imported message is a message like any other, and this `/get` asks for both
views of its MIME tree that
[RFC 8621 §4.1.4](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.4) defines.
`bodyStructure` is "the full MIME structure of the message body, without
recursing into `message/rfc822` or `message/global` parts", and `attachments` is
the flat list of the parts a client offers as attachments. `bodyProperties`
chooses which properties of each part come back, and the one that matters here
is `blobId`, since the blob of a `message/rfc822` part is the attached message
in full.

```ocaml
let rec flatten (p : Proto.Email_body.Part.t) =
  p :: List.concat_map flatten (Option.value p.sub_parts ~default:[])

let attached_message (e : Proto.Email.t) =
  Option.value e.attachments ~default:[]
  @ List.concat_map flatten (Option.to_list e.body_structure)
  |> List.find_opt (fun (p : Proto.Email_body.Part.t) ->
      p.type_ = Some "message/rfc822")
```

The two views overlap, and either can be absent, so the parts are searched
together with `bodyStructure` flattened through its `subParts`. A message with
no `message/rfc822` part, which `--file` may well name, reports that there is
nothing to parse.

<br>

```ocaml
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
```

[`Chain.email_parse`](../../lib/core/chain.mli) takes the properties and body
arguments of an `Email/get`, and `Proto.Email.Parse.parsed` looks a result up by
blob id, there being no message id to look it up by. A blob that is not a
message comes back under `notParsable` and one the account does not hold under
`notFound`, so an absent entry in `parsed` is one of those two.

[RFC 8621 §4.9](https://www.rfc-editor.org/rfc/rfc8621#section-4.9) has the
server return null for `id`, `mailboxIds`, `keywords` and `receivedAt` on a
parsed Email, there being no such message in the mail store. `c-import` sees
them filled in because it parses a blob it imported in the same request; the
blob here is the attachment, which was never imported, and all four are null.
[`Proto.Email.body_value`](../../lib/mail/mail_email.mli) pairs a `textBody`
part with its entry in `bodyValues`, which `~fetch_text_body_values:true` asked
for, so the forwarded message is displayed without being stored.

<pre><code><b>$ dune exec -- examples/l-parse/parse.exe --allow-insecure</b>
uploaded  754 octets as blob Gbf7f2c155a40a2dd5ca32f369936289d242c278d
imported  Mbf7f2c155a40a2dd5ca32f36 into "Imported l-parse-3694328", state now 2145
message   "Fwd: the original question" keywords [$seen]
attached  original.eml, 264 octets in blob G1d23f6c1a7b357a0fc0092bcd6899d066b2b9f1c
parsed    "The original question" from Bob &lt;bob@example.org&gt;
          id null, mailboxIds null, keywords null, receivedAt null
          Could you take a look at the report?
destroyed the mailbox and the message it held
</code></pre>

Run it as `dune exec -- examples/l-parse/parse.exe --file message.eml` to import
a message of your own. The mailbox name and the `Message-ID` of the sample carry
the process id, so two runs never upload the same bytes and the import never
meets its own leftovers.

<br>

**Next steps:**

- The next example, [**`m-organise`**](../m-organise#readme), files and flags
  messages that are already in the account.
- [**`b-blobs`**](../b-blobs#readme) is the upload and download the blobs of this
  step come and go through.

<br>

[Up to the tutorial index](../#readme)
