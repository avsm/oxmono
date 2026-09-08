# `n-resync`

<br>

A mail client keeps a copy of the account on the machine it runs on and brings
that copy back into step rather than fetching the account again. This step holds
a cache of Emails, of Mailboxes and of the message list on screen, and resyncs
all three from the state strings it stored, falling back to a full fetch
wherever the server says it cannot enumerate the changes.

The frame is the one from [**`1-session`**](../1-session#readme), in the
[`Cli.main'`](../../eio/cli.mli) form, since `--since STATE` is what a client
would have read out of its own database at startup.

```ocaml
type cache = {
  emails : (Proto.Id.t, Proto.Email.t) Hashtbl.t;
  mailboxes : (Proto.Id.t, Proto.Mailbox.t) Hashtbl.t;
  mutable ids : Proto.Id.t list;
  mutable email_state : string;
  mutable mailbox_state : string;
  mutable query_state : string;
}
```

A state string belongs to one type of record, so the cache holds one for the
Emails, one for the Mailboxes and one for the query behind the message list, as
[**`e-changes`**](../e-changes#readme) describes. Only `email_state` is
resumable here, from `--since`; the other two start at `0`, the state of a cache
holding nothing. `ids` is the message list, newest first, and `emails` and
`mailboxes` are the records behind it.

<br>

## Fetching afresh

```ocaml
let rebuild client ~account_id cache =
  let Results.[ query; state ] =
    Client.run_exn client
      Chain.(
        let* q = list_query ~account_id in
        let+ s = email_state ~account_id in
        Handles.[ q; s ])
  in
  Hashtbl.reset cache.emails;
  cache.email_state <- state;
  Fmt.pr "  refetching the cache at state %s@." state;
  set_list cache query;
  fetch_emails client ~account_id cache cache.ids
```

Resyncing from nothing is an `Email/query` for the newest ids and an `Email/get`
for the records behind them. [`Chain.email_state`](../../lib/core/chain.mli) is
the state-only get of [**`e-changes`**](../e-changes#readme), an `Email/get` of
no ids read for the `state` string
[RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1) puts on
every `/get` response. It travels in the same request as the query, so the state
recorded is the state the records were fetched at, and a later change falls in
the next `/changes` rather than between the two calls.

`fetch_emails` is [`Sync.get_all`](../../eio/sync.mli) over the ids, which
splits them into `/get` calls no larger than `maxObjectsInGet` and runs them
together, as [**`d-paging`**](../d-paging#readme) covers. It drops every id the
server reports in `notFound`, a record named by a delta being able to disappear
before the `/get` that follows it runs.

<br>

## The record delta

```ocaml
let rec resync_emails ?(fuel = 100) client ~account_id cache =
  Fmt.pr "Email/changes since %s@." cache.email_state;
  match
    Sync.email_changes client ~account_id ~since:cache.email_state
      ~max_changes:8L ~fuel:1 ()
  with
  | Error e -> Fmt.failwith "Email/changes: %a" Sync.pp_error e
  | Ok `Cannot_calculate_changes ->
      Fmt.pr "  cannotCalculateChanges: the cache is too old to patch@.";
      rebuild client ~account_id cache
  | Ok (`Changes c) ->
      Fmt.pr "  created %a  updated %a  destroyed %a@." pp_ids c.created pp_ids
        c.updated pp_ids c.destroyed;
      Fmt.pr "  newState %s hasMore %b@." c.new_state c.has_more;
      List.iter (Hashtbl.remove cache.emails) c.destroyed;
      cache.ids <- List.fold_left (Fun.flip without) cache.ids c.destroyed;
      fetch_emails client ~account_id cache (c.created @ c.updated);
      cache.email_state <- c.new_state;
      if c.has_more then
        if fuel <= 1 then
          Fmt.failwith "request budget exhausted; resume Email/changes from %s"
            cache.email_state
        else resync_emails ~fuel:(fuel - 1) client ~account_id cache
```

[RFC 8620 §5.2](https://www.rfc-editor.org/rfc/rfc8620#section-5.2) caps one
`Email/changes` answer at `maxChanges` and sets `hasMoreChanges` when it did, so
a delta is a loop rather than a call.
[`Sync.email_changes`](../../eio/sync.mli) is that loop, folded to one set of
three id lists under the section's rules. This example passes `~fuel:1` to
expose each partial result. It applies those changes, saves `new_state` and
continues immediately when `has_more` is true. The outer budget is 100 requests;
exhaustion reports the state from which work can resume. A repeated state or a
mismatched `oldState` is an error, not a successful partial result.

The three lists say what to do to a cache and no more. Destroyed ids are dropped
from the records and from the list; created and updated ids are the argument of
one `/get`. `cache.email_state` moves to `newState` last, so a failure anywhere
above leaves the cache resumable from where it was.

`` `Cannot_calculate_changes `` is a value rather than an error. The server is
saying the state given is too old to enumerate from, and the answer is to throw
the cache away and fetch afresh. `--since 0` is that case on demand.

<br>

## The Mailbox delta and `updatedProperties`

```ocaml
let rec resync_mailboxes client ~account_id cache =
  Fmt.pr "@.Mailbox/changes since %s@." cache.mailbox_state;
  let Results.[ delta; created; updated ] =
    Client.run_exn client
      Chain.(
        let* ch =
          mailbox_changes ~account_id ~since_state:cache.mailbox_state
            ~max_changes:3L ()
        in
        let* n =
          mailbox_get ~account_id ~ids:(from_changes_created ch)
            ~properties:mailbox_properties ()
        in
        let+ u =
          mailbox_get ~account_id ~ids:(from_changes_updated ch)
            ~properties_ref:(from_changes_updated_properties ch)
            ()
        in
        Handles.[ ch; n; attempt u ])
  in
```

One request asks what moved and fetches both answers.
[`Chain.from_changes_created`](../../lib/core/chain.mli) and
`from_changes_updated` point the two `Mailbox/get` calls at the id lists of the
`Mailbox/changes` above them, by the result references of
[RFC 8620 §3.7](https://www.rfc-editor.org/rfc/rfc8620#section-3.7) that
[**`6-threads`**](../6-threads#readme) introduces.

The created Mailboxes go into the cache whole. The updated ones are fetched by
[`Chain.from_changes_updated_properties`](../../lib/core/chain.mli), the extra
argument [RFC 8621 §2.2](https://www.rfc-editor.org/rfc/rfc8621#section-2.2)
gives a `Mailbox/changes` response. When nothing but the message counts moved,
the server names those counts in `updatedProperties`, "which may be used
directly via a back-reference in a subsequent `Mailbox/get` call in the same
request". It is `null` when the server cannot narrow the change that way.

A back-reference into a `null` is not a `String[]`, and a server is free to
refuse it with `invalidResultReference`. That is a method error rather than a
failed request, and the other two calls still answered, so the third handle is
wrapped in [`Chain.attempt`](../../lib/core/chain.mli). Its response becomes a
`result`, the read of the request survives the failure, and the properties are
fetched again without the reference.

```ocaml
  (match updated with
  | Error e ->
      Fmt.pr "  the #properties reference was refused (%s)@."
        (Proto.Error.Method_error.to_string e);
      fetch_mailboxes client ~account_id cache c.updated
  | Ok got ->
      let unknown =
        List.filter (fun id -> not (Hashtbl.mem cache.mailboxes id)) c.updated
      in
      List.iter (merge_mailbox cache) got.list;
      fetch_mailboxes client ~account_id cache unknown);
```

A narrowed `/get` answers with the named properties and the `id` and nothing
else, so `merge_mailbox` keeps the fields the answer leaves out rather than
replacing the cached record with a half empty one. A Mailbox the cache has never
held cannot be merged into anything, so its id goes to `fetch_mailboxes`.

The function calls itself while `hasMoreChanges` is set, which is the drain of
§5.2 written out. [`Sync.mailbox_changes`](../../eio/sync.mli) is the same loop
in the library, but it folds the rounds into one delta and the reference above
is built against one round, so a client that wants the narrowed `/get` writes
the loop itself. A round that reports more changes without advancing `newState`
stops it.

<br>

## The message list

```ocaml
let resync_list client ~account_id cache =
  Fmt.pr "@.Email/queryChanges since %s@." cache.query_state;
  match
    Client.call_exn client
      (Chain.attempt_call
         (Chain.email_query_changes ~account_id
            ~since_query_state:cache.query_state ~sort:newest ~max_changes:64L
            ()))
  with
  | Error e ->
      Fmt.pr "  %s: querying the list again@."
        (Proto.Error.Method_error.to_string e);
      set_list cache (Client.call_exn client (list_query ~account_id))
```

A `/changes` follows a set of records and says nothing about where each one
falls in a sorted, filtered list, so the list on screen needs its own method.
[RFC 8620 §5.6](https://www.rfc-editor.org/rfc/rfc8620#section-5.6) and
[RFC 8621 §4.5](https://www.rfc-editor.org/rfc/rfc8621#section-4.5) define
`Email/queryChanges` as `removed`, a list of ids, and `added`, a list of
[`Proto.Filter.added_item`](../../lib/proto/proto_filter.mli) pairing an id with
the index it now occupies. The client removes first, inserts each added id at
its index, then truncates to the window it displays. The filter and the sort
must repeat those of the `Email/query` the `queryState` came from, the indexes
being indexes into that list.

`cannotCalculateChanges` is the usual answer here, so this call goes out under
[`Chain.attempt_call`](../../lib/core/chain.mli), which is `Chain.attempt` for a
chain of one call, leaving the caller a `result` to match on. RFC 8620 §5.5 has `canCalculateChanges` on a `/query`
response say whether the server supports the method at all, and even where it
does the query state expires. The fallback is to run the `Email/query` again,
which costs one request and is why a message list is safe to keep this way.

<br>

## What the cache ends up with

The record delta and the list are answered by different methods at different
states, so the list can name a message the delta never mentioned. The program
ends by fetching whatever the list names and the records lack, which closes that
gap and keeps the display consistent while another client writes to the account.
Its last line is the state to resume from, which a real client writes beside its
cache.

<br>

<pre><code><b>$ dune exec -- examples/n-resync/resync.exe --allow-insecure</b>
Email/changes since 0
  cannotCalculateChanges: the cache is too old to patch
  refetching the cache at state 2122
  10 id(s) at queryState 2122:0
  cached 10 record(s), 0 id(s) gone before the Email/get

Mailbox/changes since 0
  created A0C0EF02-A6A4-11F1-9D97-7E71CA53046E A0C28B00-A6A4-11F1-9D97-7E71CA53046E A0C3158E-A6A4-11F1-9D97-7E71CA53046E  updated -  destroyed -
  newState 15 hasMoreChanges true updatedProperties null

...two more rounds...

Mailbox/changes since 2066
  created -  updated A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E  destroyed -
  newState 2122 hasMoreChanges false updatedProperties totalEmails,unreadEmails,totalThreads,unreadThreads

Email/queryChanges since 2122:0
  removed -  added -

Cache: 10 mailbox(es), 10 record(s), a list of 10
  Archive            total 0 unread 0
  Drafts             total 0 unread 0
  Inbox              total 440 unread 440
  Oracle Test        total 0 unread 0
...six more...
  2026-09-02T23:33:10Z * run-examples oracle seed 2/3
  2026-09-02T23:33:10Z * run-examples oracle seed 1/3
  2026-09-02T23:33:10Z * run-examples oracle seed 3/3
...seven more...

Resume with: --since 2122
<b>$ dune exec examples/n-resync/resync.exe -- --since 2122</b>
Email/changes since 2122
  created -  updated -  destroyed -
  newState 2122 hasMore false
  cached 0 record(s), 0 id(s) gone before the Email/get

...the same four rounds...

Email/queryChanges since 0
  cannotCalculateChanges (invalid query state): querying the list again
  10 id(s) at queryState 2122:0

10 message(s) of the list are not cached
  cached 10 record(s), 0 id(s) gone before the Email/get

Cache: 10 mailbox(es), 10 record(s), a list of 10
...the same ten mailboxes and ten messages...

Resume with: --since 2122
</code></pre>

The first run is a cold cache. Every state is `0`, `Email/changes` refuses to
enumerate from it, and the cache is fetched afresh. The second resumes the Email
delta from the state the first printed, finds nothing to apply, and rebuilds
only the message list, a cache that lives for one run having no query state to
resume from. Deliver a message between the two runs and the second prints it
under `created`.

The Cyrus oracle reports every Mailbox of the account as created when asked from
`0`, and caps each answer at the `maxChanges` asked for, which is three here to
make the rounds visible. The last round of each run is the narrowed one, where
only the Inbox counts moved.

<br>

**Next steps:**

- [**`o-watch`**](../o-watch#readme) is the other half of this loop. It waits
  for the server to say a state has moved, rather than asking.
- [**`f-push`**](../f-push#readme) covers the event source that tells it.

<br>

[Up to the tutorial index](../#readme)
