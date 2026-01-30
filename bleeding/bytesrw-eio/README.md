# bytesrw-eio - OCaml Bytesrw adapters for Eio

This OCaml library provides adapters to create `Bytesrw.Bytes.Reader.t` and
`Bytesrw.Bytes.Writer.t` from Eio flows, mirroring the API of `Bytesrw_unix`
for Eio's effect-based I/O.

## Usage

```ocaml
open Eio.Std

(* Create a reader from an Eio flow *)
let read_from_flow flow =
  let reader = Bytesrw_eio.bytes_reader_of_flow flow in
  (* Use reader with Bytesrw decoders *)
  reader

(* Create a writer to an Eio flow *)
let write_to_flow flow =
  let writer = Bytesrw_eio.bytes_writer_of_flow flow in
  (* Use writer with Bytesrw encoders *)
  writer
```

For custom slice sizes:

```ocaml
(* Specify custom slice length for reading *)
let reader = Bytesrw_eio.bytes_reader_of_flow ~slice_length:4096 flow in

(* Specify custom slice length for writing *)
let writer = Bytesrw_eio.bytes_writer_of_flow ~slice_length:4096 flow in
()
```

## Installation

```
opam install bytesrw-eio
```

## Documentation

API documentation is available via:

```
opam install bytesrw-eio
odig doc bytesrw-eio
```

## License

ISC
