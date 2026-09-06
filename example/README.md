# HTTP examples

The examples introduce Proffer servers first and Fetch clients second.
Each lesson contains a complete OCaml program, its Dune dependencies and
commands to run from the repository root.

Follow the [repository setup](../HTTPZ.md#build) and select its opam switch
before running the commands.

## First request

Start the greeting server in one terminal.

```sh
dune exec --profile release-check ./example/proffer/1-hello/hello.exe
```

Leave it running, then run the client in another terminal.

```sh
dune exec --profile release-check ./example/fetch/1-read/read.exe
```

The client prints `Hello from Proffer!`. Stop the server with Ctrl-C.
The [server lesson](proffer/1-hello/README.md) and
[client lesson](fetch/1-read/README.md) explain both programs.

## Layout

- `proffer/` contains server examples.
- `fetch/` contains client examples.
- `localhost/` contains the Proffer server shared by the later client examples.

## Proffer

Server examples use loopback port 8765 and run until stopped with Ctrl-C.
Run one at a time. `d-config` chooses an unused port; `c-mock` uses no socket
and exits after printing its results.

- [1-hello](proffer/1-hello/README.md) serves a greeting at `/`.
- [2-router](proffer/2-router/README.md) matches literal paths and captures strings, integers and remaining path segments.
- [3-log](proffer/3-log/README.md) reports each completed request through `on_event`.
- [4-counter](proffer/4-counter/README.md) counts requests using state supplied through `~env`.
- [5-form](proffer/5-form/README.md) reads URL-encoded forms, multipart uploads and query parameters.
- [6-error](proffer/6-error/README.md) adds a fallback response, a permanent redirect and a response header.
- [7-cache](proffer/7-cache/README.md) sets HTTP cache headers and caches a computed response in the server.
- [8-stream](proffer/8-stream/README.md) writes response bytes as they become available.
- [9-auth](proffer/9-auth/README.md) requires an HTTP Basic credential beneath `/admin`.
- [a-negotiate](proffer/a-negotiate/README.md) selects HTML, JSON or plain text from the request's `Accept` field.
- [b-mount](proffer/b-mount/README.md) mounts an API site beneath `/api/v1`.
- [c-mock](proffer/c-mock/README.md) dispatches requests without a server or network connection.
- [d-config](proffer/d-config/README.md) sets connection limits and typed timeouts, and requests an unused port.
- [e-json](proffer/e-json/README.md) stores todo records through a JSON API and exports them as JSON Lines.
- [f-markdown](proffer/f-markdown/README.md) serves a Cmarkit document as HTML or Markdown.

## Fetch

The first client uses the manually started greeting server. Most other
clients start their own server through the example-only
[Localhost library](localhost/README.md), defined in
[localhost.ml](localhost/localhost.ml). Their Dune files link `localhost`
from this checkout. It is not part of the installed Fetch API.

`c-mock` uses no network. `e-https` contacts a public HTTPS URL by default.
`d-curl` uses the local server unless a URL is supplied.

- [1-read](fetch/1-read/README.md) reads the manually started greeting server.
- [2-response](fetch/2-response/README.md) prints the status, URL, headers and body returned by `/json`.
- [3-post](fetch/3-post/README.md) sends plain text, a URL-encoded form and a multipart body.
- [4-redirect](fetch/4-redirect/README.md) requests `/old`, which redirects to `/hello`.
- [5-errors](fetch/5-errors/README.md) distinguishes HTTP error responses from URL, connection and timeout failures.
- [6-restrict](fetch/6-restrict/README.md) limits a client to the local server and then makes it read-only.
- [7-credentials](fetch/7-credentials/README.md) supplies a bearer token for `/secret` and default request headers.
- [8-cookies](fetch/8-cookies/README.md) stores the session cookie set by `/login` and sends it to `/account`.
- [9-retry](fetch/9-retry/README.md) retries `/flaky` until its third request succeeds.
- [a-limits](fetch/a-limits/README.md) runs six requests with a concurrency limit and a delay between starts.
- [b-stream](fetch/b-stream/README.md) downloads four MiB to a temporary-directory file and streams an eight KiB upload.
- [c-mock](fetch/c-mock/README.md) supplies responses without opening a network connection.
- [d-curl](fetch/d-curl/README.md) makes a request with the Curl backend.
- [e-https](fetch/e-https/README.md) makes an HTTPS request with the pure OCaml backend.
- [f-json](fetch/f-json/README.md) decodes a todo, handles a missing item, posts JSON and reads JSON Lines.
- [g-markdown](fetch/g-markdown/README.md) reads `/about` as a Cmarkit document and prints its HTML representation.

The public interfaces are [Proffer](../bleeding/proffer/lib/proffer.mli)
and [Fetch](../bleeding/fetch/lib/fetch.mli).
