# requests - HTTP Client Library for OCaml

A modern HTTP(S) client library for OCaml with Eio support, providing a clean API for making web requests with automatic TLS/CA certificate handling. Inspired by Python's requests library.

## Key Features

- **Clean Eio-style API**: Async I/O using OCaml 5's Eio library
- **Automatic TLS**: Built-in TLS support with automatic CA certificate handling
- **Connection Pooling**: Efficient connection reuse via session API
- **Authentication**: Basic, Bearer, and Digest authentication (RFC 7617, 6750, 7616)
- **Cookies**: Automatic cookie handling with optional persistence
- **Retries**: Exponential backoff with jitter
- **Timeouts**: Configurable connection and read timeouts
- **Proxy Support**: HTTP/HTTPS proxies with CONNECT tunneling

## Installation

```
opam install requests
```

## Usage

### Session API (Recommended)

Use sessions for connection pooling, cookie persistence, and shared configuration:

```ocaml
let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  (* Create a session with connection pooling *)
  let session = Requests.create ~sw env in

  (* Make a GET request *)
  let response = Requests.get session "https://httpbin.org/get" in
  Printf.printf "Status: %d\n" (Requests.Response.status_code response);
  let body = Requests.Response.text response in
  Printf.printf "Body: %s\n" body
```

### POST with JSON

```ocaml
let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let session = Requests.create ~sw env in
  let headers = Requests.Headers.(empty |> content_type Requests.Mime.json) in
  let body = Requests.Body.string ~content_type:Requests.Mime.json
    {|{"key": "value"}|} in
  let response = Requests.post session ~headers ~body
    "https://httpbin.org/post" in
  Printf.printf "Status: %d\n" (Requests.Response.status_code response)
```

### Authentication

```ocaml
let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let session = Requests.create ~sw env in

  (* Basic authentication *)
  let auth = Requests.Auth.basic ~username:"user" ~password:"pass" in
  let response = Requests.get session ~auth "https://httpbin.org/basic-auth/user/pass" in
  Printf.printf "Status: %d\n" (Requests.Response.status_code response);

  (* Bearer token *)
  let auth = Requests.Auth.bearer ~token:"your-token" in
  let response = Requests.get session ~auth "https://api.example.com/resource" in
  ignore response
```

### One-Shot API

For simple, stateless requests without connection pooling:

```ocaml
let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let response = Requests.One.get ~sw
    ~clock:env#clock ~net:env#net
    "https://httpbin.org/get" in
  Printf.printf "Status: %d\n" (Requests.Response.status_code response)
```

## Command-Line Tool

The library includes `ocurl`, a curl-like command-line tool:

```bash
# GET request
ocurl https://httpbin.org/get

# POST with data
ocurl -X POST -d '{"key":"value"}' -H "Content-Type: application/json" https://httpbin.org/post

# With authentication
ocurl -u user:pass https://httpbin.org/basic-auth/user/pass
```

## Documentation

API documentation is available via:

```
opam install requests
odig doc requests
```

Or build locally:

```
opam exec -- dune build @doc
open _build/default/_doc/_html/index.html
```

## Requirements

- OCaml 5.1.0+
- Eio library
- TLS support via tls-eio and ca-certs

## License

ISC
