# cbort

Native CBOR codec with type-safe combinators for OCaml.

## Overview

cbort provides type-safe CBOR (RFC 8949) encoding and decoding using a combinator-based approach. Define codecs once and use them for both encoding and decoding OCaml values to and from CBOR binary format.

## Features

- **Type-safe codecs**: Define encoding/decoding rules once, use them bidirectionally
- **Combinator-based API**: Compose complex codecs from simple building blocks
- **RFC 8949 compliant**: Full support for CBOR data model including tags, maps, arrays, and all numeric types
- **Arbitrary precision integers**: Uses Zarith for unlimited integer range
- **Detailed error reporting**: Path-aware error messages for debugging decode failures
- **Zero-copy streaming**: Built on bytesrw for efficient I/O

## Installation

```
opam install cbort
```

## Usage

```ocaml
open Cbort

(* Define a codec for a record type *)
type person = { name : string; age : int }

let person_codec =
  let open Obj in
  let* name = mem "name" (fun p -> p.name) string in
  let* age = mem "age" (fun p -> p.age) int in
  return { name; age }
  |> finish

(* Encode to CBOR bytes *)
let encoded = encode_string person_codec { name = "Alice"; age = 30 }

(* Decode from CBOR bytes *)
let decoded = decode_string person_codec encoded
```

## Documentation

API documentation is available at the project homepage.

## License

ISC License. See [LICENSE.md](LICENSE.md) for details.
