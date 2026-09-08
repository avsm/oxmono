# `e-changes`

<br>

Every `/get` response carries an opaque `state` string, and handing that string
back to the matching `/changes` method asks the server what has happened since.
This step records two of them, marks a message `$seen` and unmarks it, then
reads the delta back. The frame is the one from
[**`1-session`**](../1-session#readme). `pp_ids` prints an id list, or `-` for
an empty one.

<br>

## Where a state string comes from

[RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1) puts a
`state` on every `/get` response, and §5.2 takes the same string as the
`sinceState` of `/changes`. A `/get` naming no record still carries it, so
[`Chain.email_state`](../../lib/core/chain.mli) sends that *state-only get* and
reads the string out of it, as `mailbox_state` and `thread_state` do for the
other two data types. Both ride in the request that also asks an `Email/query`
for the unread message to change.

```ocaml
  let Results.[ since_email; since_mailbox; unread ] =
    Client.run_exn client
      Chain.(
        let* e = email_state ~account_id in
        let* m = mailbox_state ~account_id in
        let+ q =
          email_query ~account_id
            ~filter:(Proto.Email.filter ~not_keyword:`Seen ())
            ~limit:1L ()
        in
        Handles.[ e; m; q ])
  in
```

## Draining `/changes`

[RFC 8620 §5.2](https://www.rfc-editor.org/rfc/rfc8620#section-5.2) answers
with three id lists and a `hasMoreChanges` flag, since a server may cap one
answer at `maxChanges`. The client calls again from the `newState` it was just
given until the flag clears, folding the rounds together so that a record
created and then destroyed is reported in none of the three lists.
[`Sync.email_changes`](../../eio/sync.mli) is that loop and its
[`changes`](../../eio/sync.mli) record is the fold. `has_more` is set when the
drain exhausted its request fuel before reaching the present. Apply that
partial result, retain its `new_state`, then call again from that state before
waiting for push. A repeated state or a mismatched `oldState` is an error.
The [watch example](../o-watch#readme) demonstrates continuing a partial drain.

Setting `$seen` and removing it again is two changes to one record, and the
delta reports it once, in `updated`. A `/changes` answers about records, not
about edits, so the account ends this step as it was found. Each of the two
writes is a `/set`, which fails per record rather than as a whole
([§5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3)), and
[`Proto.Method.set_failures`](../../lib/proto/proto_method.mli) gathers the
`notCreated`, `notUpdated` and `notDestroyed` entries of one response into a
single list, empty when every record named was dealt with.

```ocaml
let flip client ~account_id ~email entry =
  let update = [ (email, Proto.Patch.v [ entry ]) ] in
  let set = Client.call_exn client (Chain.email_set ~account_id ~update ()) in
  match Proto.Method.set_failures set with
  | f :: _ -> Fmt.failwith "%a" Proto.Method.pp_set_failure f
  | [] -> ()
```

`` `Cannot_calculate_changes `` is a value rather than an error. RFC 8620 §5.2
has a server say it when the client's state is too old to enumerate, and the
answer is to resync from a `/get` rather than to retry. The run below asks with
the state string `stale`, which Cyrus cannot parse.

## `updatedProperties`

[RFC 8621 §2.2](https://www.rfc-editor.org/rfc/rfc8621#section-2.2) gives
`Mailbox/changes` one argument the standard response does not have. When only
`totalEmails`, `unreadEmails`, `totalThreads` or `unreadThreads` moved, the
server says so in `updatedProperties`, "which may be used directly via a
back-reference in a subsequent `Mailbox/get` call in the same request".
[`Chain.from_changes_updated_properties`](../../lib/core/chain.mli) builds that
reference and `mailbox_get ~properties_ref` takes it in place of the typed
`~properties` list. `None` means the server could not narrow it and the
`Mailbox/get` fails with `invalidResultReference`, so that call rides in the
chain under [`Chain.attempt`](../../lib/core/chain.mli), which gives it a
`result` of its own rather than failing the whole read.

```ocaml
  let Results.[ mc; got ] =
    Client.run_exn client
      Chain.(
        let* c = mailbox_changes ~account_id ~since_state:since_mailbox () in
        let+ g =
          mailbox_get ~account_id ~ids:(from_changes_updated c)
            ~properties_ref:(from_changes_updated_properties c)
            ()
        in
        Handles.[ c; attempt g ])
  in
```

## Keeping a list fresh

A `/changes` follows a set of records and says nothing about where they fall in
a sorted, filtered list, so a message list on screen cannot be patched from one.
`Email/queryChanges`
([RFC 8620 §5.6](https://www.rfc-editor.org/rfc/rfc8620#section-5.6),
[RFC 8621 §4.5](https://www.rfc-editor.org/rfc/rfc8621#section-4.5)) reports
which ids left the list and where each new one belongs. Cyrus answers it
`cannotCalculateChanges` once the query state is stale, so a client keeps the
fallback of refetching, as `n-resync` shows.

<pre><code><b>$ export JMAP_SESSION_URL=http://localhost:18080/.well-known/jmap</b>
<b>$ export JMAP_API_KEY=user1:x JMAP_AUTH=basic</b>
<b>$ dune exec -- examples/e-changes/changes.exe --allow-insecure</b>
Email state 1864, Mailbox state 1864
$seen on and off M79907f72d98c7558d36c0df3

Email/changes since that state
  created -  updated M79907f72d98c7558d36c0df3  destroyed -
  newState 1866 hasMore false

Email/changes since a stale state cannotCalculateChanges

Mailbox/changes since that state
  created -  updated A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E
  newState 1866  updatedProperties totalEmails, unreadEmails, totalThreads, unreadThreads
  Mailbox/get returned 1 mailbox(es) with just those
</code></pre>

<br>

**Next steps:**

- [**`f-push`**](../f-push#readme) waits for the server to say that a state has
  moved, so that this step runs only when there is something to fetch.
- [**`g-raw`**](../g-raw#readme) leaves the typed layer for a method this
  library has no builder for.

<br>

**See also:**

- [**`n-resync`**](../n-resync#readme) is this loop written out for a real
  cache, with `Email/queryChanges` and the fallback Cyrus forces.

<br>

[Up to the tutorial index](../#readme)
