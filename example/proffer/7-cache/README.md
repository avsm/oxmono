# Set cache policy

This example sets HTTP cache headers and caches a computed response in the server.

[cache.ml](cache.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/7-cache/cache.exe
```

With the server running, send requests from another terminal.

```sh
curl -i http://127.0.0.1:8765/logo
curl http://127.0.0.1:8765/report
curl http://127.0.0.1:8765/report
curl -i http://127.0.0.1:8765/clock
```

`Cache_control` describes how clients may retain responses. The logo has
an entity tag and a one-year public lifetime. The clock response uses
`no-store`.

`Cache.memoize` separately stores the report in server memory. Its first
request takes two seconds; later requests reuse it for ten seconds.
`Duration.of_sec 10` sets this server-side lifetime.

Stop the server with Ctrl-C before running another server example.

Continue with [stream a response](../8-stream/README.md), or return to the
[Proffer examples](../../README.md#proffer).
