# `6-threads`

<br>

This step is the worked example of RFC 8620 itself. Four method calls travel in
one request, each taking its ids from the result of the call before it, so the
client never sees the intermediate ids at all.

The frame is the one from [**`1-session`**](../1-session#readme). The request is
the whole of what is new:

```ocaml
  let Results.[ threads; messages ] =
    Client.run_exn ctx.client
      Chain.(
        let* q =
          email_query ~account_id
            ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
            ~collapse_threads:true ~limit:3L ()
        in
        let* heads =
          email_get ~account_id ~ids:(from_query q) ~properties:[ `Thread_id ]
            ()
        in
        let* threads =
          thread_get ~account_id ~ids:(from_get_field heads Thread_id) ()
        in
        let+ messages =
          email_get ~account_id
            ~ids:(from_get_field threads Email_ids)
            ~properties:[ `Id; `Subject; `Received_at ]
            ()
        in
        Handles.[ threads; messages ])
  in
```

<br>

A Thread is a set of Emails the server considers to be one conversation.
[RFC 8621 §3](https://www.rfc-editor.org/rfc/rfc8621#section-3) gives it exactly
two properties, `id` and `emailIds`, and orders `emailIds` by arrival, so
[`Chain.thread_get`](../../lib/core/chain.mli) rarely needs `properties`.

`~collapse_threads:true` is the extra argument
[RFC 8621 §4.4.3](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.3) gives
`Email/query`. With it the query answers one id per conversation rather than one
per message, so a limit of three asks for three conversations.

The chaining is
[RFC 8620 §3.7](https://www.rfc-editor.org/rfc/rfc8620#section-3.7). An argument
sent under a name prefixed with `#` carries a pointer into the response of an
earlier call in the same request, which the server resolves before running the
method. `from_query` points at the `ids` of a `/query` response, and
[`Chain.from_get_field`](../../lib/core/chain.mli) points at one property of
every record a `/get` returned, here `Thread_id` of the Emails and then
`Email_ids` of the Threads.

Section 3.7 maps a pointer through an array, so the property must hold an Id
or an array of Ids, and
[`Chain.id_property`](../../lib/core/chain.mli) is the handful that do, each
indexed by the record carrying it so that `Email_ids` cannot be asked of an
`Email/get`. The property must also be one the `/get` asked for, since
[RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1) returns
only the properties named and the server would answer the reference with an
`invalidResultReference` error. `from_get_field` compares the two lists as the
request is built and raises rather than send such a reference, which is why
the second call asks for `` `Thread_id ``. `from_get_field_raw` is the same
under a wire name, for a property an extension defines.

<br>

The last call answers with every message of all three conversations at once, and
[RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1) lets a
`/get` return its results in a different order to the ids it was given.
[`Proto.Method.in_ids_order`](../../lib/proto/proto_method.mli) puts a `/get`
list back into the order of an id list, which here is each Thread's own
`emailIds`, so one flat answer is printed as three ordered conversations. The
two responses to read come out of [`Client.run_exn`](../../eio/client.mli) as
`Results.[ threads; messages ]`, in the order the chain named their handles,
as in [**`3-inbox`**](../3-inbox#readme):

```ocaml
  List.iter
    (fun (t : Proto.Thread.t) ->
      let email_ids = Option.value t.email_ids ~default:[] in
      Fmt.pr "Thread %a: %d message(s)@."
        Fmt.(option ~none:(any "?") Proto.Id.pp)
        t.id (List.length email_ids);
      Proto.Method.in_ids_order ~id:Proto.Email.id email_ids messages.list
      |> List.iter (fun (e : Proto.Email.t) ->
          Fmt.pr "  %s  %s@." (date e.received_at)
            (Option.value e.subject ~default:"(no subject)")))
    threads.list
```

Each date is printed with
[`Proto.Date.to_utc_string`](../../lib/proto/proto_date.mli), as in
[**`5-message`**](../5-message#readme).

<br>

<pre><code><b>$ dune exec -- examples/6-threads/threads.exe --allow-insecure</b>
Thread T5a6a64a60b5b74f7: 1 message(s)
  2026-09-02T23:33:10Z  run-examples oracle seed 2/3
Thread T11a5ee740eb3990f: 1 message(s)
  2026-09-02T23:33:10Z  run-examples oracle seed 1/3
Thread Td57519c701a8f6b5: 1 message(s)
  2026-09-02T23:33:10Z  run-examples oracle seed 3/3
</code></pre>

The messages the Cyrus oracle is seeded with are unrelated to each other, so
every Thread here holds one message. A real account prints whole conversations.

<br>

**Next steps:**

- The next example, [**`7-keywords`**](../7-keywords#readme), makes the first
  change to the account.
- [**`d-paging`**](../d-paging#readme) returns to `Email/query` and deals with
  the limits a server puts on it.

<br>

**See also:**

- [**`j-conversation`**](../j-conversation#readme) runs this same chain and then compares an
  `Email/query` with and without `collapseThreads`.

<br>

[Up to the tutorial index](../#readme)
