# Send one message

This program sends a message to a channel and topic, prints its message ID,
and exits. Edit the three values at the top of [send.ml](send.ml) first:

```ocaml
let channel = "sandbox"
let topic = "OCaml tutorial"
let content = "Hello from OCaml!"
```

Choose an existing channel your account can post to. The program does not
create or subscribe to the channel. Then run it with your chosen profile:

```sh
dune exec example/r-send/send.exe -- --profile tutorial
```

The message appears under that channel and topic. The terminal prints
`Sent message` followed by the ID returned by Zulip. Each invocation sends a
new message.

```ocaml
let message_id =
  Zulip_eio.Messages.send_channel client ~channel ~topic ~content ()
  |> Zulip_eio.Error.or_raise
```

This endpoint call waits for the API response. `Main.run_once` exits after the
function returns, so there is no queued send to abandon on normal exit. A
failure raises a structured error instead of printing success. If a connection
fails after Zulip accepts the message, the call may still report an error;
repeating it can create a duplicate.

Inside a long-running bot, `Event.reply` and `Room.send_text` return a
[`Sent.t`](../../lib/zulip_bot/sent.mli) handle as soon as the message is queued.
Use `Sent.await` when you need to observe its outcome before stopping the bot:

```ocaml
match Sent.await sent with
| `Timed_out -> Format.eprintf "Still sending.@."
| `Done outcome ->
    match outcome with
    | Sent.Sent message_id ->
        Format.printf "Sent %a@." Zulip.Id.Message.pp message_id
    | Sent.Failed error -> Format.eprintf "%a@." Zulip_eio.Error.pp error
    | Sent.Indeterminate _ -> ()
    | Sent.Cancelled -> ()
```

`Timed_out` describes the observation and is not a terminal send outcome.
An `Indeterminate` outcome means the message may already have been accepted.
Releasing the bot context's switch cancels queued sends and makes in-flight
sends indeterminate.

Next: [Mock the same endpoint](../r-mock/README.md) without sending anything.

[All examples and profile setup](../README.md) · [Source](send.ml) ·
[Messages API](../../lib/zulip_eio/messages.mli) · [Build file](dune)
