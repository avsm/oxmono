# `f-push`

<br>

A JMAP server that lets a client hold a connection open publishes an *event
source*, over which it says which types have moved without saying what changed.
This step opens one, waits twenty seconds for the `Email` state of the account
to move, and fetches the difference when it does.

The frame is the one from [**`1-session`**](../1-session#readme) with one
addition. This step has an option of its own, so it is
[`Cli.main'`](../../eio/cli.mli), which takes a Cmdliner term as `~args` and
passes its value to the body beside the context; `ctx.sw` is the switch
`Cli.main'` opened, and the subscription's fiber belongs to it. `pp_ids` is the
printer from [**`e-changes`**](../e-changes#readme).

```ocaml
let () =
  Jmap_eio.Cli.main' "push" ~doc:"Wait for a StateChange on the event source"
    ~args:poll_term
  @@ fun ctx poll ->
  let client = ctx.client and account_id = ctx.account_id in
  let state = Client.call_exn client (Chain.email_state ~account_id) in
  let poll = if poll > 0. then Some poll else None in
  let sub =
    Push.subscribe ~sw:ctx.sw client ~types:[ "Email" ] ~ping:30 ?poll
      ~last_event_id:"1" ()
  in
  Fmt.pr "Event source %s@."
    (Push.event_source_url client ~types:[ "Email" ]
       ~close_after:(if Option.is_some poll then `State else `No)
       ~ping:30 ());
  Fmt.pr "Waiting 20 s for an Email StateChange after %s@." state;
  let announced =
    Push.wait_for_state sub ~timeout:20. ~since:state ~type_:"Email" ~account_id
      ()
  in
  Push.close sub;
  match announced with
  | None -> Fmt.pr "No change within 20 s.@."
  | Some s -> (
      Fmt.pr "StateChange: Email is now at %s@." s;
      match Sync.email_changes client ~account_id ~since:state () with
      | Ok (`Changes c) ->
          Fmt.pr "  created %a  updated %a  destroyed %a@." pp_ids
            c.Sync.created pp_ids c.Sync.updated pp_ids c.Sync.destroyed
      | Ok `Cannot_calculate_changes -> Fmt.pr "  cannotCalculateChanges@."
      | Error e -> Fmt.failwith "Email/changes: %a" Sync.pp_error e)
```

<br>

## The event source

[RFC 8620 §7.3](https://www.rfc-editor.org/rfc/rfc8620#section-7.3) makes the
event source one long-running HTTP GET whose response is a `text/event-stream`.
The server appends a `state` event whenever data the client asked about
changes, and a `ping` event at an agreed interval so that a live connection can
be told from a stalled one. The URL comes from the session's `eventSourceUrl`
template, which [`Push.event_source_url`](../../eio/push.mli) expands with the
types to watch, the ping interval and whether to close after the first state
event.

[`Push.subscribe`](../../eio/push.mli) forks a fiber under the switch that
keeps that connection up, quoting the last event id it saw as `Last-Event-ID`
so that the server replays what happened while it was away, backing off when
the server is unreachable, and delivering events through `Push.next`. The
`~last_event_id:"1"` above is older than anything the account has done, so the
first connection reports the current state at once.

`Push.wait_for_state` consumes this stream for the common case of waiting for a
particular type. [**`q-events`**](../q-events#readme) shows the general
`Push.next` loop, including pings, extension events and the final result. It
drains queued events before reporting completion, even if the queue was full
when the subscription ended.

## Push is half a client

A `state` event carries a StateChange
([§7.1](https://www.rfc-editor.org/rfc/rfc8620#section-7.1)), which is new state
strings and never the data itself. The other half is the `/changes` call of
[**`e-changes`**](../e-changes#readme), made from the state you were holding to
the one just announced, which is why this step opens with
[`Chain.email_state`](../../lib/core/chain.mli) and hands the string it reads
to [`Push.wait_for_state`](../../eio/push.mli) as `~since`. A StateChange
reporting a state already in hand is skipped like any other event, so the wait
ends on the next one. A server pushes the current state of every type it was
asked about as soon as a connection opens, so without `~since` a program that
has just drained `/changes` and gone back to waiting returns at once.

The announced string is the new state, not the `sinceState` for the next delta.
Apply the delta before saving its `new_state`. If `has_more` is true, continue
from that intermediate state before waiting again, as
[**`o-watch`**](../o-watch#readme) does. `Push.last_event_id` is a reconnect
cursor and can be ahead of events the application has processed; reconcile
from the saved object state after a restart.

## `--poll` and the Cyrus lock

Cyrus holds a per-user lock for as long as an event-source response is open,
which stops mail being delivered to that user, so a connection held open there
never sees the message it is waiting for. `--poll SECONDS`, two by default,
asks `Push.subscribe` for the other mode: each connection asks for
`closeafter=state`, is held for at most that many seconds and is then dropped,
and the next one quotes the last event id and is told what it missed. The
account is unlocked in the gaps, which is when the delivery below got through.
The subscription is closed before the `Email/changes` call for the same reason.
Against a server happy to hold a connection open, `--poll 0` holds one and the
change arrives the moment it happens.

## The other half of push

A client that cannot hold a connection open, a phone woken by its platform's
notification service, registers a PushSubscription
([§7.2](https://www.rfc-editor.org/rfc/rfc8620#section-7.2)) instead: a record
holding a URL the server POSTs each StateChange to, an encryption key, an
expiry and a verification handshake. The payload is the same object, so what
follows it is the `/changes` call above, unchanged. The records are typed in
[`Proto.Push`](../../lib/proto/proto_push.mli), but Cyrus answers
`PushSubscription/get` with `unknownMethod`, so nothing here demonstrates the
round trip.

<pre><code><b>$ export JMAP_SESSION_URL=http://localhost:18080/.well-known/jmap</b>
<b>$ export JMAP_API_KEY=user1:x JMAP_AUTH=basic</b>
<b>$ dune exec -- examples/f-push/push.exe --allow-insecure --poll 2</b>
Event source http://localhost:18080/jmap/eventsource/?types=Email&amp;closeafter=state&amp;ping=30
Waiting 20 s for an Email StateChange after 1868
No change within 20 s.
</code></pre>

Nothing happened to that account, and the step exits 0 all the same. Run it
again and deliver a message from another shell while it waits:

<pre><code><b>$ dune exec -- examples/f-push/push.exe --allow-insecure --poll 2</b>
Event source http://localhost:18080/jmap/eventsource/?types=Email&amp;closeafter=state&amp;ping=30
Waiting 20 s for an Email StateChange after 1868
StateChange: Email is now at 1869
  created M9e4c057e1a700e4faa77779e  updated -  destroyed -
</code></pre>

<br>

**Next steps:**

- [**`g-raw`**](../g-raw#readme) is the last step, and reaches past the typed
  layer to the JSON underneath it.
- [**`e-changes`**](../e-changes#readme) is the other half of this loop, in
  more detail than the four lines here.

<br>

**See also:**

- [**`o-watch`**](../o-watch#readme) runs this loop for as long as it is
  left running, printing a line per message and resuming from a saved state and
  event id.

<br>

[Up to the tutorial index](../#readme)
