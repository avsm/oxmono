# Conpool - Protocol-agnostic Connection Pooling for Eio

Conpool is a connection pooling library built on Eio that manages TCP connection lifecycles, validates connection health, and provides per-endpoint resource limiting for any TCP-based protocol.

## Key Features

- **Protocol-agnostic**: Works with HTTP, Redis, PostgreSQL, or any TCP-based protocol
- **Health validation**: Automatically validates connections before reuse
- **Per-endpoint limits**: Independent connection limits and pooling for each endpoint
- **TLS support**: Optional TLS configuration for secure connections
- **Statistics & monitoring**: Track connection usage, hits/misses, and health status
- **Built on Eio**: Leverages Eio's structured concurrency and resource management

## Usage

Basic example establishing a connection pool:

```ocaml
open Eio.Std

let run env =
  Switch.run (fun sw ->
    (* Create a basic connection pool (no protocol state) *)
    let pool = Conpool.create_basic
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env)
      ()
    in

    (* Define an endpoint *)
    let endpoint = Conpool.Endpoint.make ~host:"example.com" ~port:80 in

    (* Use a connection from the pool *)
    Conpool.with_connection pool endpoint (fun conn ->
      Eio.Flow.copy_string "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n" conn;
      let buf = Eio.Buf_read.of_flow conn ~max_size:4096 in
      Eio.Buf_read.take_all buf
    )
  )
```

With TLS configuration:

```ocaml
let run env =
  Switch.run (fun sw ->
    (* Create TLS configuration - SNI servername is automatically set to the endpoint's hostname *)
    let tls_config = Tls.Config.client ~authenticator:(Ca_certs.authenticator ()) () in

    (* Create pool with TLS *)
    let pool = Conpool.create_basic
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env)
      ~tls:tls_config
      ()
    in

    let endpoint = Conpool.Endpoint.make ~host:"example.com" ~port:443 in
    Conpool.with_connection pool endpoint (fun conn ->
      (* Use TLS-encrypted connection *)
      ...
    )
  )
```

Custom pool configuration:

```ocaml
let config = Conpool.Config.make
  ~max_connections_per_endpoint:20
  ~max_idle_per_endpoint:5
  ~connection_timeout:10.0
  ~validation_interval:300.0
  ()
in

let pool = Conpool.create_basic ~sw ~net ~clock ~config ()
```

Monitor pool statistics:

```ocaml
let stats = Conpool.stats pool endpoint in
Printf.printf "Active: %d, Idle: %d, Hits: %d, Misses: %d\n"
  (Conpool.Stats.active_connections stats)
  (Conpool.Stats.idle_connections stats)
  (Conpool.Stats.cache_hits stats)
  (Conpool.Stats.cache_misses stats)
```

## Installation

```
opam install conpool
```

## Documentation

API documentation is available at https://tangled.org/@anil.recoil.org/ocaml-conpool or via:

```
opam install conpool
odig doc conpool
```

## License

ISC
