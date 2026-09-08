# `5-message`

<br>

This step opens one message the way a reading pane does, asking for the parsed
header fields, the plain text body and the list of attachments in a single
`Email/get`.

The frame is the one from [**`1-session`**](../1-session#readme). The request
and the properties it names are what is new:

```ocaml
let to_header = Proto.Email_header.addresses `To
let to_property = Proto.Email_header.header_property_to_string to_header
```

```ocaml
  let got =
    Client.call_exn ctx.client
      Chain.(
        let* q =
          email_query ~account_id
            ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
            ~limit:1L ()
        in
        email_get ~account_id ~ids:(from_query q)
          ~properties:
            [
              `Subject;
              `From;
              `Received_at;
              `Text_body;
              `Body_values;
              `Attachments;
              `Header to_header;
            ]
          ~body_properties:[ `Part_id; `Type; `Size; `Name ]
          ~fetch_text_body_values:true ~max_body_value_bytes:2000L ())
  in
```

<br>

An Email is three views of the same message at once, and
[RFC 8621 §4.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.1) names them
separately. The metadata properties describe the message in the mail store, the
header convenience properties give it parsed header fields, and the body
properties give it a MIME structure. A client pays for the ones it names in
`properties` and no others.

[`Chain.email_get`](../../lib/core/chain.mli) takes them as typed variants, so
`` `Received_at `` cannot be misspelled the way the wire name `receivedAt` can
be. A property that was not asked for is `None` on
[`Proto.Email.t`](../../lib/mail/mail_email.mli), which is why almost every
field of the record is an option.

`` `Header to_header `` is the one property whose name is computed.
[RFC 8621 §4.1.3](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.3) lets a
client ask for any header field under a name of the form `header:{name}:{form}`,
and [§4.1.2](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.2) fixes the
seven forms a value may come back in.
[`Proto.Email_header.addresses`](../../lib/mail/mail_header.mli) builds the
`asAddresses` form and accepts only the fields the specification allows it for,
so `header:From:asDate` cannot be built at all. Such a value has no field of its
own on the record. It lands in `dynamic_headers` under the full property name,
and [`Proto.Email.find_header_addresses`](../../lib/mail/mail_email.mli) reads
it back out under exactly that name.

<br>

The three body arguments belong to `Email/get` alone
([RFC 8621 §4.2](https://www.rfc-editor.org/rfc/rfc8621#section-4.2)).
`body_properties` says which properties of each
[`Proto.Email_body.Part`](../../lib/mail/mail_body.mli) to return,
`fetch_text_body_values` fills `body_values` for the parts listed in
`text_body`, and `max_body_value_bytes` truncates each of those values at 2000
octets. A truncated value says so in `is_truncated`, so a reading pane can offer
to fetch the rest.

The parts and their contents arrive as two separate lists, joined by `part_id`
([RFC 8621 §4.1.4](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.4)), and a
part whose value was not fetched has no entry at all.
[`Proto.Email.body_value`](../../lib/mail/mail_email.mli) performs that join, so
walking the body is a walk over `text_body` alone:

```ocaml
      List.iter
        (fun (p : Proto.Email_body.Part.t) ->
          Proto.Email.body_value email p
          |> Option.iter (fun (v : Proto.Email_body.Value.t) ->
              Fmt.pr "@.text part %s (%s, %a bytes%s)@." (or_unknown p.part_id)
                (or_unknown p.type_) bytes p.size
                (if v.is_truncated then ", truncated" else "");
              String.split_on_char '\n' v.value |> List.iter (Fmt.pr "  | %s@.")))
        (Option.value email.text_body ~default:[]);
```

The date is printed by
[`Proto.Date.to_utc_string`](../../lib/proto/proto_date.mli), which writes the
`Z` form that a UTCDate takes on the wire
([RFC 8620 §1.4](https://www.rfc-editor.org/rfc/rfc8620#section-1.4)).

<br>

<pre><code><b>$ dune exec -- examples/5-message/message.exe --allow-insecure</b>
Received  2026-09-02T23:33:10Z
From      alice@example.org
To        user1@example.com
Subject   run-examples oracle seed 2/3

text part 1 (text/plain, 126 bytes)
  | This is seed message 2 for the ocaml-jmap examples. It mentions the word oracle so the search example has something to find.
  | 

attachments 0
</code></pre>

The messages the Cyrus oracle is seeded with are plain single-part text, so
`attachments` is empty and `bodyStructure` would be a single leaf.

<br>

**Next steps:**

- The next example, [**`6-threads`**](../6-threads#readme), follows one message
  out to the whole conversation it belongs to.
- [**`c-import`**](../c-import#readme) goes the other way and turns a raw RFC
  5322 message into an Email.

<br>

**See also:**

- [**`i-reading`**](../i-reading#readme) is the same idea at full size, walking
  `bodyStructure` and downloading an attachment.

<br>

[Up to the tutorial index](../#readme)
