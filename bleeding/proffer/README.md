# Proffer

Proffer defines HTTP sites, routes and response handlers. `proffer-httpz`
serves them through Eio. `proffer.mock` dispatches requests in memory.

Start with the [server and client examples](../../example/README.md).
The [first Proffer lesson](../../example/proffer/1-hello/README.md)
contains a complete program and its run command.

The main library includes JSON, JSON Lines, CommonMark and HTML codecs.
Timeout and delay values use the external `Duration.t`, also available as
`Proffer.Duration.t`.

The [public interface](lib/proffer.mli) describes the API. The
[repository guide](../../HTTPZ.md) describes library selection, setup
and backend limits. All examples are under the top-level `example/` directory.
