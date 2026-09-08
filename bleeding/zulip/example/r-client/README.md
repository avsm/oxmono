# Query the client

A program can use the same profile and command line as a bot, do some work,
and exit. This example prints the account's identity and subscribed channels.

```ocaml
let client = Zulip_bot.Context.client context in
let subscriptions =
  Zulip_eio.Channels.get_subscriptions client |> Zulip_eio.Error.or_raise
in
List.iter
  (fun subscription ->
    let channel = Zulip.Channel.Subscription.channel subscription in
    Format.printf "  %s@." (Zulip.Channel.name channel))
  subscriptions
```

[`Main.run_once`](../../lib/zulip_bot_cli/main.mli) opens the profile and calls
the [program's function](client.ml) once. It supplies an authenticated context;
`Context.client` gives access to the endpoint helpers in `Zulip_eio`. This
program does not register a bot event queue or send a message.

```sh
dune exec example/r-client/client.exe -- --profile tutorial
```

You get the account's display name, email, numeric user ID, and one channel
name per line. The values depend on your account's subscriptions.

Endpoint helpers return `Ok value` or `Error error`. `Error.or_raise` unwraps
success and raises the structured error on failure, causing this command-line
program to fail rather than print a misleading partial result. Match on the
result yourself when the application can recover. The returned subscriptions
are typed values decoded by Jsont; no JSON traversal is needed here.

Next: [Send one message](../r-send/README.md), or see how to construct a client
directly in [the mock recipe](../r-mock/README.md).

[All examples and profile setup](../README.md) · [Source](client.ml) ·
[Channels API](../../lib/zulip_eio/channels.mli) · [Build file](dune)
