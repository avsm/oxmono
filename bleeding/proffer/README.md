# Proffer

Proffer defines HTTP sites, routes and response handlers. `proffer-httpz`
serves them through Eio. `proffer.mock` dispatches requests in memory.

The optional [`proffer.dav`](dav/README.md) library serves explicitly mounted
WebDAV exports. `proffer.dav.eio` supplies confined read-only directories and a
private transactional writer. No DAV route is enabled by default. Outbound
WebDAV operations belong to [`fetch.dav`](../fetch/dav/README.md).

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

Measure framework allocations with
`dune exec --profile release-check bleeding/proffer/bench/bench_alloc.exe`.
The writer discards output, so this excludes socket I/O. The
`Req.query_param_local` rows measure lookup after subtracting request creation.

Measure a real keep-alive connection with
`dune exec --profile release-check bleeding/proffer/bench/bench_httpz_alloc.exe -- 1000`.
This includes both the loopback client and server, Eio, clocks and socket I/O.
Connection setup and 100 warmup requests are excluded.
