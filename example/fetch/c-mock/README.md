# Test client code in memory

This example supplies responses without opening a network connection.

[mock.ml](mock.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/c-mock/mock.exe
```

`Fetch_mock.client` takes a function that receives each request and
constructs its response. The program prints the requested method and URL,
then the value returned by `greeting`. `Eio_mock.Backend.run` runs the
example without an operating-system event loop. No Proffer server or
`Localhost` library is used.

Continue with [use libcurl](../d-curl/README.md), or return to the
[Fetch examples](../../README.md#fetch).
