# Send request bodies

This example sends plain text, a URL-encoded form and a multipart body.

[post.ml](post.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/3-post/post.exe
```

`Fetch.Form.urlencoded` and `Fetch.Form.multipart` return request headers
and a body together. The example sends them with `Fetch.post`. `/echo`
returns the submitted bytes, while `/form` prints decoded fields. The file
part is a small string supplied by the program.

Continue with [follow redirects](../4-redirect/README.md), or return to the
[Fetch examples](../../README.md#fetch).
