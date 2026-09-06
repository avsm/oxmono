# Follow redirects

This example requests `/old`, which redirects to `/hello`.

[redirect.ml](redirect.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/4-redirect/redirect.exe
```

The first request follows the redirect and prints the final URL and body.
The second passes `~redirects:0` and prints the redirect status and
`Location` field. Redirect limits apply to individual requests.

Continue with [handle failures](../5-errors/README.md), or return to the
[Fetch examples](../../README.md#fetch).
