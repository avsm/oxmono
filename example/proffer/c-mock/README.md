# Test a site in memory

This example dispatches requests without a server or network connection.

[mock.ml](mock.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/c-mock/mock.exe
```

`Proffer_mock.request` supplies a method, path, environment and optional
headers and body to the site. The program prints responses for normal,
missing, HEAD and form requests. `Proffer_mock.status`, `header` and `body`
inspect the result. This example exits after printing its responses.

Continue with [set server limits](../d-config/README.md), or return to the
[Proffer examples](../../README.md#proffer).
