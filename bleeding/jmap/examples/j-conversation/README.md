# `j-conversation`

<br>

A message list shows one row per conversation, and opening a row shows every
message in it. This step prints the newest conversation in full, then runs the
list query with `collapseThreads` off and on to show what a collapsed row hides.

The frame is the one from [**`1-session`**](../1-session#readme). The
conversation arrives through the four call chain
[**`6-threads`**](../6-threads#readme) walks through, with the properties a
message row needs asked for on the last call:

```ocaml
  let Results.[ threads; messages ] =
    Client.run_exn ctx.client
      Chain.(
        let* q =
          email_query ~account_id ~sort ~collapse_threads:true ~limit:1L ()
        in
        let* head =
          email_get ~account_id ~ids:(from_query q) ~properties:[ `Thread_id ]
            ()
        in
        let* threads =
          thread_get ~account_id ~ids:(from_get_field head Thread_id) ()
        in
        let+ messages =
          email_get ~account_id
            ~ids:(from_get_field threads Email_ids)
            ~properties:
              [ `Id; `Subject; `From; `Received_at; `Keywords; `Preview ]
            ()
        in
        Handles.[ threads; messages ])
  in
```

A Thread is the server's idea of a conversation.
[RFC 8621 §3](https://www.rfc-editor.org/rfc/rfc8621#section-3) gives it two
properties, `id` and `emailIds`, and nothing else, so a client reads a
conversation in two steps, the Thread for the ids and an `Email/get` for the
messages themselves. Both ride in the request that found the Thread.

The properties on the last call are the ones a row displays.
[RFC 8621 §4.1.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.1) defines
`keywords`, which carries the read state, and
[RFC 8621 §4.1.4](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.4) defines
`preview`, a server generated plain text extract of at most 256 characters that
saves a client fetching a body part for the list.

```ocaml
      Proto.Method.in_ids_order ~id:Proto.Email.id email_ids messages.list
      |> List.iteri (fun i e -> print_message (i + 1) e));
```

A `/get` may answer in any order
([RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1)), while the
`emailIds` of a Thread are sorted by `receivedAt`, so
[`Proto.Method.in_ids_order`](../../lib/proto/proto_method.mli) puts the
messages back into the order the conversation was held in, taking each message's
id with [`Proto.Email.id`](../../lib/mail/mail_email.mli).

<br>

The two list queries and the conversation count travel in one request:

```ocaml
  let Results.[ expanded; collapsed; heads ] =
    Client.run_exn ctx.client
      Chain.(
        let* expanded =
          email_query ~account_id ~sort ~limit ~calculate_total:true ()
        in
        let* collapsed =
          email_query ~account_id ~sort ~collapse_threads:true ~limit
            ~calculate_total:true ()
        in
        let+ heads =
          email_get ~account_id ~ids:(from_query expanded)
            ~properties:[ `Thread_id ] ()
        in
        Handles.[ expanded; collapsed; heads ])
  in
```

`~collapse_threads:true` is the extra argument
[RFC 8621 §4.4.3](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.3) gives
`Email/query`. An Email in the same Thread as an earlier Email of the same
result is dropped from the list, so the query answers one id per conversation
and a limit counts conversations rather than messages. The two calls differ in
that argument alone, since a comparison is only meaningful when the filter and
the sort are the same, and neither names the other's result, so both fit in the
one request. `~calculate_total:true` asks for the number of matches beyond the
page ([RFC 8620 §5.5](https://www.rfc-editor.org/rfc/rfc8620#section-5.5)),
which a server may refuse for an expensive query.

The `Email/get` reads the `threadId` of every message on the uncollapsed page,
and the number of distinct ids in it is how many rows those messages occupy once
they are collapsed. Collapsing only removes ids, so every id of the uncollapsed
page that the collapsed page lacks is a message the collapse hid under a row
rather than one that fell off the end. `--limit` sets the size of both pages and
defaults to 20.

<br>

<pre><code><b>$ dune exec -- examples/j-conversation/conversation.exe --allow-insecure</b>
Thread T5a6a64a60b5b74f7: 1 message(s)
   1.* 2026-09-02T23:33:10Z  Alice                run-examples oracle seed 2/3

The newest 20 with collapseThreads false: 20 id(s) of 440 matching
The newest 20 with collapseThreads true : 20 id(s) of 440 matching
Those 20 message(s) belong to 20 conversation(s)
Collapsed away: nothing, so every message here is its own thread
</code></pre>

The messages the Cyrus oracle is seeded with are replies to nothing, so each is
its own Thread and the collapse removes none of them. On an account that holds
conversations the third line is smaller than the second and the last line names
the hidden messages. Cyrus also answers `preview` with an empty string, so no
preview line appears under the message above.

<br>

**Next steps:**

- The next example, [**`k-compose`**](../k-compose#readme), writes a message and
  sends it.
- [**`d-paging`**](../d-paging#readme) deals with the limits a server puts on the
  `Email/query` this step calls twice.

<br>

[Up to the tutorial index](../#readme)
