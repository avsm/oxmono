# Tomlt

Tomlt is a type-safe [TOML 1.1](https://toml.io/en/v1.1.0) codec library for OCaml.

## Design

Tomlt provides bidirectional encoding and decoding using a combinator-based
approach inspired by [Jsont](https://erratique.ch/software/jsont). The design
is based on the [paper](https://github.com/dbuenzli/jsont/tree/main/paper):

> Daniel Bünzli. *An alphabet for your data soups*, 2024.
> Available at: https://github.com/dbuenzli/jsont/tree/main/paper

Each codec `'a t` defines both a decoder (`Toml.t -> ('a, error) result`) and
an encoder (`'a -> Toml.t`), composing through combinators to build complex
types from simple primitives.

## Quick Start

Define a codec for your OCaml types:

```ocaml
type config = { host : string; port : int; debug : bool }

let config_codec =
  Tomlt.(Table.(
    obj (fun host port debug -> { host; port; debug })
    |> mem "host" string ~enc:(fun c -> c.host)
    |> mem "port" int ~enc:(fun c -> c.port)
    |> mem "debug" bool ~enc:(fun c -> c.debug) ~dec_absent:false
    |> finish
  ))
```

For I/O operations (parsing strings, reading files), use `Tomlt_bytesrw`:

```ocaml
let () =
  match Tomlt_bytesrw.decode_string config_codec {|
    host = "localhost"
    port = 8080
  |} with
  | Ok config -> Printf.printf "Host: %s\n" config.host
  | Error e -> prerr_endline (Toml.Error.to_string e)
```

## Packages

- `tomlt` - Core library with value types and codec combinators
- `tomlt.bytesrw` - Streaming parser/encoder using Bytesrw
- `tomlt.eio` - Eio integration with system clock
- `tomlt.unix` - Unix I/O with system clock
- `tomlt.jsont` - Jsont codecs for toml-test JSON format

## License

ISC
