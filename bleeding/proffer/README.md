# Proffer

Proffer defines HTTP sites, routes and response handlers. `proffer-httpz`
serves them through Eio. `proffer.mock` dispatches requests in memory.

The optional [`proffer.dav`](dav/README.md) library provides a WebDAV client
over Fetch and the transport-independent `davz` protocol library.

Start with the [server and client examples](../../example/README.md).
The [first Proffer lesson](../../example/proffer/1-hello/README.md)
contains a complete program and its run command.

The main library includes JSON, JSON Lines, Markdown and HTML codecs.
Timeout and delay values use the external `Duration.t`. Add `duration` to
your Dune libraries when constructing these values.

`Mime.of_path` uses Proffer's built-in extension registry, with no heap
allocation per lookup. The private table in [mime_data.ml](lib/mime_data.ml)
preserves the mappings previously provided by magic-mime 1.3.1, including
Proffer's JavaScript, Markdown, AVIF and webmanifest overrides. To add a
mapping, insert its lowercase extension in sorted order and cover it in
[test_mime.ml](test/test_mime.ml).

The [public interface](lib/proffer.mli) describes the API. The
[repository guide](../../HTTPZ.md) describes library selection, setup
and backend limits. All examples are under the top-level `example/` directory.
