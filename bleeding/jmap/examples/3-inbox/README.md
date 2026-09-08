# `3-inbox`

<br>

This step lists the ten newest messages in the Inbox. That takes two method
calls, a query for the ids and a fetch for the contents, and they travel in
one request because the second names the result of the first.

The frame is the one from [**`1-session`**](../1-session#readme), and the
`using` list of every request from here on is the one
[**`2-mailboxes`**](../2-mailboxes#readme) explains.

```ocaml
let doc = "Print the newest messages in the Inbox, in one request"
let date = Option.fold ~none:"?" ~some:Proto.Date.to_utc_string

let sender (e : Proto.Email.t) =
  match e.from with
  | Some (a :: _) -> Option.value a.Proto.Email_address.name ~default:a.email
  | _ -> "-"

let () =
  Jmap_eio.Cli.main "inbox" ~doc @@ fun ctx ->
  let account_id = ctx.account_id in
  let inbox = Sync.mailbox_id_exn ctx.client ~account_id `Inbox in
  let Results.[ query; got ] =
    Client.run_exn ctx.client
      Chain.(
        let* q =
          email_query ~account_id
            ~filter:(Proto.Email.filter ~in_mailbox:inbox ())
            ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
            ~limit:10L ()
        in
        let+ g =
          email_get ~account_id ~ids:(from_query q)
            ~properties:[ `Id; `Received_at; `From; `Subject ]
            ()
        in
        Handles.[ q; g ])
  in
  if List.is_empty query.ids then Fmt.failwith "the Inbox is empty";
  List.iter
    (fun (e : Proto.Email.t) ->
      Fmt.pr "%-20s  %-24s  %s@." (date e.received_at) (sender e)
        (Option.value e.subject ~default:"(no subject)"))
    (Proto.Method.in_ids_order ~id:Proto.Email.id query.ids got.list);
  Fmt.pr "@.%d of the newest messages in %a@." (List.length query.ids)
    Proto.Id.pp inbox
```

<br>

## Finding the Inbox

A filter names the Inbox by its id, so the id must be in hand before the chain
is built, which makes the lookup a request of its own.
[`Sync.mailbox_id_exn`](../../eio/sync.mli) is that request, a `Mailbox/query`
filtered on the role followed by a `Mailbox/get` of the ids it returned.
[RFC 8621 §2](https://www.rfc-editor.org/rfc/rfc8621#section-2) gives a role
to at most one Mailbox of an account, so the answer is one id, and an account
with no Inbox raises. A role is the IMAP special-use attribute, so this finds
the Inbox whatever it is called in whatever language.

[`Jmap_eio.Sync`](../../eio/sync.mli) holds the requests that are more than
one method call, and [**`d-paging`**](../d-paging#readme) covers the rest of
it. A lookup that can ride in a larger request uses
[`Chain.mailbox_by_role`](../../lib/core/chain.mli) instead.

## The query

[`Chain.email_query`](../../lib/core/chain.mli) is the `/query` of
[RFC 8620 §5.5](https://www.rfc-editor.org/rfc/rfc8620#section-5.5) over the
Email type of
[RFC 8621 §4.4](https://www.rfc-editor.org/rfc/rfc8621#section-4.4). It
answers with ids rather than records. `filter` says which records to keep,
`sort` says in what order, and `limit` says how many ids to take from the top
of that order.

The filter here is one *FilterCondition*, built by
[`Proto.Email.filter`](../../lib/mail/mail_email.mli), which takes an argument
per member of
[RFC 8621 §4.4.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.1) and
keeps every Email when given none. `~in_mailbox` keeps the Emails in the
Inbox, and [**`4-filter`**](../4-filter#readme) is about the rest of the
arguments and about combining conditions.

`sort` is a list of Comparators, built by
[`Proto.Email.sort`](../../lib/mail/mail_email.mli) from the property to sort
on, with `~ascending:false` for newest first.
[RFC 8621 §4.4.2](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.2) lists
the properties an `Email/query` may be sorted on, and a server may refuse any
other with an `unsupportedSort` error, which is why the property is a variant
rather than a string.

## The result reference

[RFC 8620 §3.7](https://www.rfc-editor.org/rfc/rfc8620#section-3.7) lets a
later call in the same request name a JSON pointer into an earlier call's
result, sending the argument as `#ids` rather than `ids`, which the server
resolves before running the method. That saves the second round trip a query
for ids would otherwise cost.

[`Chain.from_query`](../../lib/core/chain.mli) builds the reference. `q` is
the handle of the `Email/query`, and `~ids:(from_query q)` is "the `ids` of
whatever that call returns". The `ids` argument of a `/get` takes an
`id_source`, either such a reference or ids already held with
[`Chain.ids`](../../lib/core/chain.mli). The `let*` of
[`Chain`](../../lib/core/chain.mli) makes a handle available to the calls
after it, and the chain assigns the method call ids, so a reference can never
name a call that is not in the request.

## Reading the two calls

A chain ends in the list of handles whose responses are wanted, written
`Handles.[ q; g ]`, and [`Client.run_exn`](../../eio/client.mli) sends it and
hands back the list of their decoded responses in the same order, taken apart
by the pattern `Results.[ query; got ]`. Both lists are typed rather than
ordinary lists, so the compiler knows how many entries there are and what each
one decodes to. `query.ids` is the ids of the `Email/query` and `got.list` the
records of the `Email/get`, and a pattern of the wrong length does not
compile. A program that reads results aliases
[`Chain.Results`](../../lib/core/chain.mli) beside its other modules, while
the `Handles` constructors are already in scope inside `Chain.( ... )`.

An `Email/get` may return its records in any order, which
[RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1) allows, so
the order of the query is carried by its `ids`.
[`Proto.Method.in_ids_order`](../../lib/proto/proto_method.mli) puts the
records back in that order and drops any record the list does not name, given
[`Proto.Email.id`](../../lib/mail/mail_email.mli) to read the id of a record
with. `receivedAt` is a
[`Proto.Date`](../../lib/proto/proto_date.mli), a `Ptime.t` decoded from the
RFC 3339 string on the wire and printed back as UTC.

<pre><code><b>$ dune exec -- examples/3-inbox/inbox.exe --allow-insecure</b>
2026-09-02T23:33:10Z  Alice                     run-examples oracle seed 2/3
2026-09-02T23:33:10Z  Alice                     run-examples oracle seed 1/3
2026-09-02T23:33:10Z  Alice                     run-examples oracle seed 3/3
2026-09-02T23:15:58Z  Alice                     run-examples oracle seed 3/3
2026-09-02T23:15:58Z  Alice                     run-examples oracle seed 1/3
2026-09-02T23:15:58Z  Alice                     run-examples oracle seed 2/3
2026-09-02T19:42:46Z  Alice                     oracle-1613390-1788378166-1
2026-09-02T19:42:46Z  Alice                     oracle-1613390-1788378166-3
2026-09-02T19:31:06Z  Alice                     oracle-1159494-1788377466-27
2026-09-02T19:31:05Z  Alice                     oracle-1159494-1788377465-23

10 of the newest messages in A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E
</code></pre>

This Inbox holds what the other steps and earlier runs of
`scripts/run-examples.sh` left in it. Several messages above share a
`receivedAt` to the second, and a server may order records with equal sort
keys however it likes, which is why the seeded messages are out of delivery
order. An empty Inbox fails the program, so `scripts/run-examples.sh --seed 3`
delivers three messages over LMTP to the test server first.

<br>

**Next steps:**

- [**`4-filter`**](../4-filter#readme) builds larger filters out of the record
  this step set one field of.
- [**`6-threads`**](../6-threads#readme) chains four calls instead of two, and
  is the worked example of RFC 8620 §3.7 itself.

<br>

**See also:**

- [**`d-paging`**](../d-paging#readme) is what to do when there are more
  results than one query should return.

<br>

[Up to the tutorial index](../#readme)
