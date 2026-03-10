# ocaml-zarr

A pure OCaml implementation of the [Zarr v3.1](https://zarr-specs.readthedocs.io/en/latest/v3/core/v3.0.html) specification for chunked, compressed, N-dimensional arrays.

## Overview

ocaml-zarr provides a complete Zarr v3 stack split across four opam packages:

| Package | Description |
|---------|-------------|
| `zarr` | Core library: types, codecs, metadata, ndarray, store interface |
| `zarr-sync` | Synchronous memory and filesystem stores (Unix I/O) |
| `zarr-eio` | Async memory and filesystem stores ([Eio](https://github.com/ocaml-multicore/eio)) |
| `zarr-blosc` | Optional [Blosc](https://www.blosc.org/) meta-compressor codec (C bindings) |

## Features

- Full Zarr v3.1 metadata parsing and serialization
- N-dimensional arrays backed by `Bigarray` with support for all Zarr v3 data types
- 6 built-in codecs: `bytes`, `transpose`, `gzip`, `zstd`, `crc32c`, `sharding_indexed`
- Extensible codec registry for third-party codecs (e.g. Blosc)
- Regular chunk grids with configurable chunk key encoding (`default` and `v2`)
- Abstract store interface with pluggable backends
- Slicing and indexing with `Index`, `Range`, `Stepped`, `All`, etc.
- Group hierarchy with metadata and attribute support

## Requirements

- OCaml >= 5.1
- dune >= 3.0

## Installation

### From source

```
git clone https://github.com/mtelvers/ocaml-zarr.git
cd ocaml-zarr
opam install . --deps-only --with-test
dune build
```

To include Blosc support, ensure `libblosc-dev` (or equivalent) is installed on your system.

## Quick Start

### Create and write an array

```ocaml
open Zarr
open Zarr_sync

let () =
  let store = Memory_store.create () in

  (* Create a 100x100 float64 array with 10x10 chunks *)
  let arr = match Memory_array.create store
    ~path:"my_array"
    ~shape:[|100; 100|]
    ~chunks:[|10; 10|]
    ~dtype:Ztypes.Dtype.Float64
    ~fill_value:(Float 0.0)
    () with
    | Ok a -> a
    | Error _ -> failwith "failed to create array"
  in

  (* Write a value *)
  Memory_array.set arr [|5; 10|] (`Float 42.0);

  (* Read it back *)
  match Memory_array.get arr [|5; 10|] with
  | `Float v -> Printf.printf "value: %f\n" v
  | _ -> ()
```

### Use compression codecs

```ocaml
let arr = Memory_array.create store
  ~path:"compressed"
  ~shape:[|1000; 1000|]
  ~chunks:[|100; 100|]
  ~dtype:Ztypes.Dtype.Int32
  ~codecs:[
    bytes_codec ();
    gzip_codec ~level:5 ();
  ]
  ()
```

### Sharding

```ocaml
let arr = Memory_array.create store
  ~path:"sharded"
  ~shape:[|1000; 1000|]
  ~chunks:[|100; 100|]
  ~dtype:Ztypes.Dtype.Float64
  ~codecs:[
    sharding_codec
      ~chunk_shape:[|10; 10|]
      ~codecs:[bytes_codec (); gzip_codec ()]
      ~index_codecs:[bytes_codec (); crc32c_codec ()]
      ();
  ]
  ()
```

### Slicing

```ocaml
(* Read a 50x50 region *)
let slice = Memory_array.get_slice arr [range 0 50; range 0 50] in

(* Write data into a region *)
Memory_array.set_slice arr [range 10 20; all] data
```

### Filesystem store

```ocaml
let store = Filesystem_store.create "/path/to/zarr/store" in
let arr = Filesystem_array.open_ store ~path:"my_array" in
```

### Eio async store

```ocaml
open Zarr_eio

let () = Zarr_eio.run @@ fun ~fs ->
  let store = Filesystem_store.create fs "/path/to/store" in
  (* ... use store ... *)
```

### Blosc codec via extension registry

When `zarr-blosc` is linked, the Blosc codec registers itself automatically and can be used in codec chains:

```ocaml
open Zarr_blosc

let codecs = [
  Zarr.bytes_codec ();
  Blosc.codec_spec ~cname:LZ4 ~clevel:5 ~shuffle:Shuffle ~typesize:4 ~blocksize:0;
]
```

Blosc metadata is serialized to standard Zarr JSON, so arrays compressed with Blosc are interoperable with other Zarr implementations that support the Blosc codec.

## Codec Registry

Third-party codecs can be registered at runtime so the core library can construct them from metadata JSON:

```ocaml
(* Register a custom codec *)
Zarr.Codec_registry.register "my_codec" (fun config dtype chunk_shape ->
  Ok (Zarr.Codec_registry.BytesToBytes {
    encode = (* ... *);
    decode = (* ... *);
    compute_encoded_size = (fun _ -> None);
  })
);

(* Check registration *)
Zarr.Codec_registry.is_registered "my_codec"  (* true *)
```

Registered codecs are represented as `Extension { name; config }` in the codec spec and are fully integrated into codec chain building, JSON parsing, and serialization.

## Architecture

```
zarr (core, no I/O)
 |
 +-- zarr-sync (Unix I/O)
 +-- zarr-eio  (Eio async I/O)
 +-- zarr-blosc (C bindings to libblosc)
```

The core `zarr` package has no I/O dependencies. Store backends are provided by separate packages that implement the `Store.STORE` module type. Array and group operations are functorized over the store implementation.

### Key modules

| Module | Purpose |
|--------|---------|
| `Zarr.Ztypes` | Core type definitions (dtype, codec_spec, metadata, etc.) |
| `Zarr.Ndarray` | N-dimensional array backed by Bigarray |
| `Zarr.Codec` | Codec chain building, encoding, and decoding |
| `Zarr.Codec_registry` | Extensible registry for third-party codecs |
| `Zarr.Metadata` | Zarr v3 metadata JSON parsing and serialization |
| `Zarr.Store` | Abstract store interface (READABLE, WRITABLE, LISTABLE) |
| `Zarr.Array` | Array operations functor (`Make(Store)`) |
| `Zarr.Group` | Group and hierarchy operations |
| `Zarr.Chunk_grid` | Regular chunk grid logic |
| `Zarr.Chunk_key` | Chunk key encoding (default `/`, v2, dot separator) |
| `Zarr.Indexing` | Slice computation and chunk intersection |

## Supported Data Types

`bool`, `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`, `float16`, `float32`, `float64`, `complex64`, `complex128`, `r<N>` (raw bytes)

## Running Tests

```
dune runtest
```

This runs both the main test suite and the Blosc-specific tests, including:

- Unit tests for every codec
- Property-based tests (QCheck)
- Metadata JSON round-trip tests
- Python interoperability fixture tests
- Store integration tests (memory and filesystem)
- Codec chain ordering validation tests

## Benchmarks

```
dune exec bench/bench_codecs.exe
```

Benchmarks cover encode/decode throughput for individual codecs, codec chains, and array-level read/write operations.

## License

MIT

## Contributing

Bug reports and pull requests are welcome on [GitHub](https://github.com/mtelvers/ocaml-zarr).
