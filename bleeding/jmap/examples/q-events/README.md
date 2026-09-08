# `q-events`

[`f-push`](../f-push#readme) waits for an Email state change;
[`o-watch`](../o-watch#readme) turns notifications into `/changes` calls.
This example consumes every decoded event with the new
[`Push.next`](../../eio/push.mli), including pings and extension events, and
checks the subscription's final result.

```sh
dune exec -- examples/q-events/events.exe --profile personal --poll 0 --timeout 30
```

With the [oracle profile](../0-profiles#readme), retain polling so Cyrus releases
its account lock between connections:

```sh
dune exec -- examples/q-events/events.exe \
  --profile oracle --allow-insecure --timeout 5
```

The defaults are `--poll 2` and `--timeout 5`. `--poll 0` keeps a connection
open. A poll deadline covers response headers and closes the connection even
when the event queue is full.

## Events and completion

The executable's consumer is:

```ocaml
let rec consume sub =
  match Push.next sub with
  | `Event event ->
      Fmt.pr "%a@." Push.pp_event event;
      consume sub
  | `End -> (
      match Eio.Promise.await (Push.result sub) with
      | Ok () -> Fmt.pr "The subscription ended.@."
      | Error error -> Fmt.failwith "push: %a" Client.pp_error error)
```

`Push.next` drains buffered events before returning `End`. It observes the
subscription's completion even when its bounded queue had no room for an End
item; taking only from `Push.events` can miss that condition. `Push.result`
distinguishes a normal end from a fatal error. A consumer calls `Push.close`
when it is finished. At its monotonic timeout, this example closes the
subscription, consumes the remaining buffered events and checks the result.
`Fun.protect` also closes it if the consumer raises an error.

`Push.pp_event` escapes server-supplied text before printing. An event is
`State_change`, `Ping`, or `Unknown (name, data)`, so an application can handle
extensions without failing its stream.

## Resume from applied state

`~last_event_id:"1"` is an oracle-friendly initial cursor that prompts an
immediate state response. It is not an application's saved sync state.
`Push.last_event_id` records events received by the subscription and can be
ahead of the consumer. Persist each object type's state only after applying its
changes, then reconcile from that state on restart. A pushed state is the new
state; passing it as `sinceState` would skip the change that woke the client.

For a cache, use the delta loop in [**`o-watch`**](../o-watch#readme) or the
resynchronisation path in [**`n-resync`**](../n-resync#readme).
