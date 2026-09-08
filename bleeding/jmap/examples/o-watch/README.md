# `o-watch`

<br>

A mail client shows a message when it arrives rather than when the reader next
asks for it. This step keeps an event source subscription running and turns
every `Email` state change the server announces into the `Email/changes` and
`Email/get` that say what arrived, printing a line per message until its
deadline passes.

The frame is the one from [**`1-session`**](../1-session#readme).
[**`f-push`**](../f-push#readme) opens an event source and waits once on it;
this step is the loop around that wait, so it has three options of its own.
`--poll SECONDS`, two by default, is the reconnecting mode described below,
`--timeout SECONDS`, twenty by default, is how long to watch so that a run with
no arguments ends, and `--once` stops at the first state change caught up
with.

```ocaml
let () =
  Jmap_eio.Cli.main' "watch" ~doc:"Print a line for every message as it arrives"
    ~args
  @@ fun ctx (poll, timeout, once) ->
  let client = ctx.client and account_id = ctx.account_id in
  let clock = Eio.Stdenv.clock ctx.env in
  let state = ref (Client.call_exn client (Chain.email_state ~account_id)) in
  let sub =
    Push.subscribe ~sw:ctx.sw client ~types:[ "Email" ] ~ping:30
      ?poll:(if poll > 0. then Some poll else None)
      ~last_event_id:"1" ()
  in
  Fmt.pr "Watching Email in %a from state %s for %.0f s@." Proto.Id.pp
    account_id !state timeout;
```

<br>

A watcher holds one thing between rounds, the `Email` state string it has
already accounted for. [`Chain.email_state`](../../lib/core/chain.mli) reads it
in one call, as [**`e-changes`**](../e-changes#readme) describes, and every
round replaces it with the state that round reached. It is read before
subscribing, so that a message delivered while the connection is being made
falls inside the first delta rather than between the two.

[`Push.subscribe`](../../eio/push.mli) forks the fiber that keeps the event
source connected into `ctx.sw`, the switch
[`Cli.main'`](../../eio/cli.mli) opened around the whole command, so the
subscription lives exactly as long as the program does and leaving the body for
any reason stops it. `~last_event_id:"1"` quotes an id older than anything the
account has done, which has the first connection report the current state at
once ([RFC 8620 §7.3](https://www.rfc-editor.org/rfc/rfc8620#section-7.3))
instead of waiting for the server's own change poll.

<br>

```ocaml
  let rec loop ?(draining = false) () =
    let remaining = deadline -. Eio.Time.now clock in
    if remaining > 0. then
      match
        if draining then Some ()
        else
          Option.map (fun _ -> ())
            (Push.wait_for_state sub ~timeout:remaining ~since:!state ~type_:"Email"
               ~account_id ())
      with
      | None -> ()
      | Some () -> (
          match Sync.email_changes client ~account_id ~since:!state () with
          | Error e -> Fmt.failwith "Email/changes: %a" Sync.pp_error e
          | Ok `Cannot_calculate_changes ->
              Fmt.failwith
                "cannotCalculateChanges: the mail cache must be resynchronised"
          | Ok (`Changes c) ->
              List.iter
                (fun (e : Proto.Email.t) ->
                  incr arrived;
                  Fmt.pr "%-20s  %-24s  %s@." (date e.received_at) (sender e)
                    (Option.value e.subject ~default:"(no subject)"))
                (fetch client ~account_id c.created);
              let changed = List.length c.updated
              and removed = List.length c.destroyed in
              if changed + removed > 0 then
                Fmt.pr "%-20s  %d changed, %d removed@." "-" changed removed;
              state := c.new_state;
              if c.has_more then loop ~draining:true ()
              else if not once then loop ())
  in
```

<br>

The round is wait, drain, apply, and round again from the new state. The
deadline is what makes the wait finite, and each round hands
[`Push.wait_for_state`](../../eio/push.mli) whatever is left of it, so a
program that has been woken ten times still stops at the same moment. `~since`
skips a state change reporting the state already held, which matters here
because every reconnection is answered with the current state of the account.

A StateChange
([RFC 8620 §7.1](https://www.rfc-editor.org/rfc/rfc8620#section-7.1)) says that
`Email` moved and nothing more, so the announced string is not enough to print a
line with. [`Sync.email_changes`](../../eio/sync.mli) asks what the difference
is, draining `Email/changes` over as many rounds as
[RFC 8620 §5.2](https://www.rfc-editor.org/rfc/rfc8620#section-5.2) needs. Its
answer replaces the held state, rather than the announced one, since a drain
that stopped short of the present would otherwise skip what it had not
reached.

Each drain has finite request fuel. If its result has `has_more=true`, the
loop continues from that intermediate state without waiting for another push
event, including with `--once`.

`cannotCalculateChanges` is the server saying that the held state is too old to
compute a delta from, and RFC 8620 §5.2 asks the client to resync instead. A
watcher stops with an error so that the gap is visible. A client with a cache
does here what [**`n-resync`**](../n-resync#readme) does.

The created ids are fetched by `fetch`, which is
[`Sync.get_all`](../../eio/sync.mli) outside the chain that produced them, a
watcher woken after an hour away having as many ids as the account gained in
that hour. It cuts them into calls of at most `maxObjectsInGet` ids and runs
those within the server's concurrency limit, as
[**`d-paging`**](../d-paging#readme) shows, and
[`Proto.Method.in_ids_order`](../../lib/proto/proto_method.mli) restores the
order `Email/changes` reported them in. Updated and destroyed ids are counted
rather than fetched, a new-mail indicator caring only about what appeared.

<br>

```ocaml
  Push.close sub;
  Fmt.pr "@.%d new message(s). Email state %s, last event id %s@." !arrived
    !state
    (Option.value (Push.last_event_id sub) ~default:"-")
```

<br>

The Email state is where `Email/changes` resumes after the application applies
the delta. [`Push.last_event_id`](../../eio/push.mli) is a reconnect cursor,
which may already be ahead of events the application has consumed. It is not
a durable acknowledgement: reconcile from the saved Email state on restart
even when resuming push with that cursor.

<br>

## The Cyrus lock

Cyrus holds a per-user lock for as long as an event source response is open,
which stops mail being delivered to that user, so a connection held open there
never sees the message it is waiting for. `--poll SECONDS` is the mode
[**`f-push`**](../f-push#readme) describes. Each connection asks for
`closeafter=state`, is dropped after that many seconds, and the next one quotes
the last event id and is told what it missed; the gaps are when delivery gets
through. Against a server happy to hold a connection open, `--poll 0` holds one
and every change arrives the moment it happens.

<pre><code><b>$ export JMAP_SESSION_URL=http://localhost:18080/.well-known/jmap</b>
<b>$ export JMAP_API_KEY=user1:x JMAP_AUTH=basic</b>
<b>$ dune exec -- examples/o-watch/watch.exe --allow-insecure</b>
Watching Email in user1 from state 2232 for 20 s

0 new message(s). Email state 2232, last event id 2232
</code></pre>

Nothing was delivered to that account in the twenty seconds, and the step exits
0 all the same. Run it again with a longer deadline and deliver a message from
another shell while it waits:

<pre><code><b>$ dune exec -- examples/o-watch/watch.exe --allow-insecure --once --timeout 40</b>
Watching Email in user1 from state 2232 for 40 s
2026-09-03T00:24:26Z  Alice                     a message delivered while o-watch waits

1 new message(s). Email state 2233, last event id 2233
</code></pre>

<br>

**Next steps:**

- The last example, [**`p-stream`**](../p-stream#readme), moves binary data in
  and out of the account without holding it in memory.
- [**`f-push`**](../f-push#readme) is the event source on its own, and covers
  the PushSubscription a client that cannot hold a connection open registers
  instead.

<br>

[Up to the tutorial index](../#readme)
