# Read forms and query parameters

This example reads URL-encoded forms, multipart uploads and query parameters.

[form.ml](form.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/5-form/form.exe
```

With the server running, send requests from another terminal.

```sh
curl -i --data 'name=alice' http://127.0.0.1:8765/greet
curl --form name=alice --form file=@README.md http://127.0.0.1:8765/upload
curl 'http://127.0.0.1:8765/search?q=ocaml'
```

`Req.form_param` reads a submitted field. `Multipart.of_req` decodes file
parts and fields. `Req.query_param` reads a query parameter. The `/greet`
route returns a 303 redirect; the upload route reports the file name and size.

The upload must be small enough to fit the backend's approximately 32 KiB
request buffer, including headers and multipart framing. Opening `/` in a
browser displays the forms.

Stop the server with Ctrl-C before running another server example.

Continue with [set fallback responses](../6-error/README.md), or return to the
[Proffer examples](../../README.md#proffer).
