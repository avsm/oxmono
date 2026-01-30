Polling_state_rpc
=================

`polling_state_rpc` is an RPC library that allows a client to watch a piece of data and
poll the server for updates. The library uses `diff` to send only the relevant updates
between the server and client, instead of sending the entire data structure every time.

## Differences from Other Libraries

The library provides a similar abstraction to the `Pipe_rpc` or `State_rpc` modules that
come built into `async_rpc_kernel`, but with tradeoffs that are more suitable for web
clients. Both `Pipe_rpc` and `State_rpc` operate by pushing streams of updates through the
connection; the client can then consume this stream of events as appropriate. This
approach works great if the client is able to keep up with the stream of events, as is
often the case with native programs.

However, Web UIs are different because they frequently get backgrounded: if the user
switches away from a browser tab, the Bonsai rendering loop, which relies on the browser's
`requestAnimationFrame` function, slows down to a crawl. If the app uses `State_rpc`, when
the tab eventually comes back to the foreground, it might have a large queue of events,
which will cause the browser to freeze while the app catches up.

Even worse, the server might run out of memory while accumulating massive pipes for
backgrounded clients, and crash.

The `polling_state_rpc` library solves this problem by having the client explicitly
poll the server, which responds with a diff from the previously-requested state to the
current data.

## Limitations

`polling_state_rpc` relies on being able to diff the response data structure. For some
types, such as `list`, this is not feasible. As a workaround, you can annotate any lists
as `@diff.atomic`:

```
type t = {
  names: string list [@diff.atomic];
}
```

However, this is likely not what you want! A list annotated as atomic will be treated as a
single value and will not be diffed properly. It's instead recommended that you send
values in a map, which are easier to diff. This is a similar recommendation to [React's
`key` prop](https://react.dev/learn/rendering-lists#keeping-list-items-in-order-with-key).

