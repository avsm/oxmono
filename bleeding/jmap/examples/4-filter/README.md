# `4-filter`

<br>

The filter of the previous step kept the Emails in one Mailbox. This step asks
three questions of the same Inbox in one request: how many messages it holds,
how many of those are unread, and how many mention a word. The first filter is
a plain FilterCondition and the other two combine conditions with an operator.

The frame is the one from [**`1-session`**](../1-session#readme), here in its
`Cli.main'` form because this step takes an option of its own, and the Inbox
is found with [`Sync.mailbox_id_exn`](../../eio/sync.mli) as in
[**`3-inbox`**](../3-inbox#readme). What is new is the filters and the four
calls:

```ocaml
let doc = "Count the Inbox three ways with three Email/query filters"

let text_term =
  let open Cmdliner in
  let doc = "The word an Email must hold for the third filter to match it." in
  Arg.(value & opt string "oracle" & info [ "text" ] ~docv:"WORD" ~doc)
```

```ocaml
let () =
  Jmap_eio.Cli.main' "filter" ~doc ~args:text_term @@ fun ctx text ->
  let account_id = ctx.account_id in
  let inbox = Sync.mailbox_id_exn ctx.client ~account_id `Inbox in
  let in_inbox = Proto.Email.filter ~in_mailbox:inbox () in
  let both other = Proto.Filter.and_ [ in_inbox; other ] in
  let unseen = both (Proto.Email.filter ~not_keyword:`Seen ()) in
  let mentioning = both (Proto.Email.filter ~text ()) in
  let sort = [ Proto.Email.sort ~ascending:false `Received_at ] in
  let Results.[ all; unread; got; matching ] =
    Client.run_exn ctx.client
      Chain.(
        let* all =
          email_query ~account_id ~filter:in_inbox ~calculate_total:true
            ~limit:1L ()
        in
        let* unread =
          email_query ~account_id ~filter:unseen ~sort ~calculate_total:true
            ~limit:5L ()
        in
        let* g =
          email_get ~account_id ~ids:(from_query unread)
            ~properties:[ `Id; `Subject ] ()
        in
        let+ matching =
          email_query ~account_id ~filter:mentioning ~calculate_total:true
            ~limit:1L ()
        in
        Handles.[ all; unread; g; matching ])
  in
  let row label (r : Proto.Method.query_response) =
    Fmt.pr "%-40s %a@." label Fmt.(option ~none:(any "-") int64) r.total
  in
  row "inMailbox" all;
  row ("inMailbox AND notKeyword " ^ Proto.Keyword.to_string `Seen) unread;
  row (Fmt.str "inMailbox AND text %S" text) matching;
  Fmt.pr "@.The newest unread messages:@.";
  List.iter
    (fun (e : Proto.Email.t) ->
      Fmt.pr "  %s@." (Option.value e.subject ~default:"(no subject)"))
    got.list
```

[`Cli.main'`](../../eio/cli.mli) is [`Cli.main`](../../eio/cli.mli) for a
command with options of its own. It evaluates `text_term` beside the
configuration term and passes its value to the body, so one command line takes
`--text WORD` beside the shared `--url` and `--account`.

<br>

## Conditions and operators

[RFC 8620 §5.5](https://www.rfc-editor.org/rfc/rfc8620#section-5.5) gives the
`filter` argument two shapes, and
[`Proto.Filter.filter`](../../lib/proto/proto_filter.mli) is those two. A
`Condition` holds a FilterCondition, whose members belong to the type being
queried. An `Operator` holds an operator and a list of filters, which may
themselves be operators, so a filter is a tree.
[`Proto.Filter.and_`](../../lib/proto/proto_filter.mli), `or_` and `not_` build
the three operators from a list of filters, as `both` does above. `not_` is a
NOR over the whole list rather than the negation of one condition, so `not_` of
two conditions keeps the Emails that match neither.

[`Proto.Email.filter`](../../lib/mail/mail_email.mli) builds the
FilterCondition of an `Email/query`, one optional argument per member of
[RFC 8621 §4.4.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.1), and
hands it back as a `Condition` ready to pass to `email_query` or to an
operator. An argument left out does not filter, and several given at once are
an implicit AND, so `both` above could have been one call to `filter` and is
written with `and_` to show the shape.
[`Proto.Email.Filter_condition.t`](../../lib/mail/mail_email.mli) is the
record behind it, for a condition assembled field by field.
[`Proto.Mailbox.filter`](../../lib/mail/mail_mailbox.mli) does the same for a
`Mailbox/query`.

`~not_keyword` takes a [`Proto.Keyword.t`](../../lib/mail/mail_keyword.mli),
the keywords of
[RFC 8621 §4.1.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.1). The
registered ones have constructors, so `` `Seen `` is spelled `$seen` on the
wire by the library, and `` `Custom "todo" `` carries any other keyword
verbatim. Unread means "without `$seen`", which is what `~not_keyword` asks
for, and [**`7-keywords`**](../7-keywords#readme) sets and removes one.

`~text` is the free text search of §4.4.1. A server matches it against the
"From, To, Cc, Bcc, and Subject header fields of the message" and should look
inside the textual body parts as well. What counts as a match beyond that is
the server's business, so two servers may disagree about the same word.

[`Proto.Email.sort`](../../lib/mail/mail_email.mli), first seen in
[**`3-inbox`**](../3-inbox#readme), builds a Comparator the same way, from one
of the properties
[RFC 8621 §4.4.2](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.2) allows
an `Email/query` to sort on. Its three keyword sorts carry their keyword with
them, as that section requires.

## Counting

A `/query` answers with at most `limit` ids, so the length of `ids` is not the
size of the result set. `~calculate_total:true` asks for the `total` as well,
which RFC 8620 §5.5 has a server omit unless `calculateTotal` was set, since
counting a whole result set may be expensive. That is why `total` is an
option.

The three queries and the `Email/get` for the subjects go in one request, the
`Email/get` taking its ids from the second query by result reference as in
[**`3-inbox`**](../3-inbox#readme). The server runs them in order, and each
response carries the method call id of the call it answers, so the four
entries of `Results.[ all; unread; got; matching ]` are the four responses in
the order the chain named its handles.

<pre><code><b>$ dune exec -- examples/4-filter/filter.exe --allow-insecure</b>
inMailbox                                440
inMailbox AND notKeyword $seen           440
inMailbox AND text "oracle"              414

The newest unread messages:
  run-examples oracle seed 2/3
  run-examples oracle seed 1/3
  run-examples oracle seed 3/3
  run-examples oracle seed 3/3
  run-examples oracle seed 1/3
</code></pre>

Every message in this test account is unread, so the first two counts agree.
Mark one with [**`7-keywords`**](../7-keywords#readme) and they part. The
third counts the word the seeded messages of `scripts/run-examples.sh --seed`
mention, and `--text WORD` counts another.

<br>

**Next steps:**

- [**`5-message`**](../5-message#readme) stops counting messages and fetches
  one whole, headers, body parts and attachments.
- [**`h-search`**](../h-search#readme) is this step at full size, with dates, senders,
  paging and highlighted snippets.

<br>

**See also:**

- [**`d-paging`**](../d-paging#readme) walks a result set larger than one
  query, using the `limit` the server echoed rather than the one asked for.

<br>

[Up to the tutorial index](../#readme)
