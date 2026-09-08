# `i-reading`

<br>

The reading pane. A user clicks one message and the client must show its
headers, the folders it is filed in, the plain text of it, the MIME tree
underneath and the attachments it can fetch on demand, without pulling
megabytes of body down to do it.

The frame is the one from [**`1-session`**](../1-session#readme).

<br>

An `Email/get` returns exactly the properties it is asked for, so a reading
pane's request begins by naming them:

```ocaml
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
```

[RFC 8621 §4.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.1) puts an
Email's properties in three groups, and a pane needs all of them: `` `Thread_id ``,
`` `Mailbox_ids ``, `` `Keywords `` and `` `Size `` are metadata the mail store
holds about the message
([§4.1.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.1)); `` `Subject ``
and `` `From `` are parsed header fields with a name of their own
([§4.1.3](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.3)); the rest
describe the body
([§4.1.4](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.4)).

`headers` are the fields with no name of their own, asked for under the
`header:{name}:{form}` property names of §4.1.3.
[`Proto.Email_header`](../../lib/mail/mail_header.mli) builds one per form, and
each builder accepts only the fields
[§4.1.2](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.2) allows its form
for, so `header:From:asDate` cannot be built. `` `Custom `` names a field the
specification does not know, and `~all:true` adds the `:all` suffix, which
returns every instance of a field that may be repeated, such as the `Received`
trail.

`body_properties` restricts what comes back on each part of the MIME tree
([§4.2](https://www.rfc-editor.org/rfc/rfc8621#section-4.2)), which matters
because a part carries around a dozen properties and a large message has many
parts.

<br>

One request carries the whole pane:

```ocaml
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
```

`Mailbox/get` rides along because the message names the folders it is in by id
alone and a pane shows their names. It refers to nothing earlier in the request,
so it costs a call rather than a round trip, and `folder` turns an id into the
name the `Mailbox/get` gave it. The `ids` of the `Email/get` come from the
`Email/query` before it by result reference, as
[**`6-threads`**](../6-threads#readme) describes.

`fetch_text_body_values` fills `body_values` for the parts listed in
`text_body`, and `max_body_value_bytes` cuts each of those values off at 4096
octets. Without them the pane would receive the structure of the body and none
of its content; without the limit it would receive all of a message of any size.

<br>

Each `header:*` property lands in `dynamic_headers` under its full property
name, since the record has no field for it.
[`Proto.Email.find_header_text`](../../lib/mail/mail_email.mli) reads one back
in the rendered form, whatever the form asked for was, and its siblings
`find_header_addresses`, `find_header_date` and `find_header_all` give the
parsed value. A field the message does not carry comes back as `null` and reads
as `None`, which is why `List-Id` prints as absent below. A raw value keeps the
line folding it had in the message.

`body_structure` is the MIME tree of §4.1.4. A multipart part has `sub_parts`
and no `blob_id`; every other part has a `blob_id` and a `size`, and a part
whose content can be fetched as text also has a `part_id`:

```ocaml
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
```

`text_body` and `attachments` are the server's own flattening of that tree into
the parts to display and the parts to offer, so a client that only shows a
message never walks the tree at all. The text to display is the join of
`text_body` with `body_values` on `part_id`, which
[`Proto.Email.body_value`](../../lib/mail/mail_email.mli) performs, as in
[**`5-message`**](../5-message#readme). A value whose `is_truncated` is set stops
at `max_body_value_bytes`, and a pane offers to fetch the rest.

<br>

An attachment is never part of a method response. It is a blob, fetched from the
session's `downloadUrl` template
([RFC 8620 §6.2](https://www.rfc-editor.org/rfc/rfc8620#section-6.2)) by
[`Client.download_exn`](../../eio/client.mli), which expands the template and
returns the bytes. `name` is the filename the server puts in
`Content-Disposition` and `accept` is the media type it is asked to serve the
blob as:

```ocaml
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
```

An attachment can be larger than the process should hold in memory.
`Client.download_to` writes the bytes to an `Eio.Flow.sink` as they arrive
instead, and [**`p-stream`**](../p-stream#readme) uses it.

<br>

<pre><code><b>$ dune exec -- examples/i-reading/reading.exe --allow-insecure</b>
Thread    T5a6a64a60b5b74f7
Mailboxes Inbox
Keywords  -
Size      660 bytes
Received  2026-09-02T23:33:10Z
Sent      2026-09-02T23:33:10Z
From      Alice &lt;alice@example.org&gt;
Subject   run-examples oracle seed 2/3

header fields
  header:Message-ID:asMessageIds seed-1788391990-1@example.org
  header:Date:asDate             2026-09-02T23:33:10Z
  header:To:asAddresses          user1@example.com
  header:List-Id:asText          (absent)
  header:Received:all             from localhost ([172.17.0.1])
	 by cyrus-docker-test-server (Cyrus 3.13.6-135-g3ae789e39) with LMTPA;
	 Wed, 02 Sep 2026 23:33:10 +0000

bodyStructure
  text/plain part 1, 126 bytes

textBody
  part 1 (text/plain)
  | This is seed message 2 for the ocaml-jmap examples. It mentions the word oracle so the search example has something to find.
  | 

attachments 0 (hasAttachment false)
</code></pre>

The messages the Cyrus oracle is seeded with are single-part plain text, so
`bodyStructure` is one leaf and the download never runs. Against an account with
real mail the tree is several levels deep and the last line reports the bytes of
the first attachment.

<br>

**Next steps:**

- The next example, [**`j-conversation`**](../j-conversation#readme), follows
  this message out to the conversation it belongs to.
- [**`p-stream`**](../p-stream#readme) downloads a blob without holding it in
  memory.

<br>

[Up to the tutorial index](../#readme)
