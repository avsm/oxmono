(** Bytes codec -- converts chunk data to/from bytes with endianness handling.

    See Zarr v3.1 spec, "bytes" codec. This is an array-to-bytes codec. *)

(** [create endian] builds a bytes codec for the given byte order.

    When the endianness matches native order (little-endian on x86),
    [Chunk_data.to_bytes] and [Chunk_data.of_bytes] are simple copies. *)
let create endian : Zarr_codec.array_to_bytes = {
  encode = (fun chunk -> Chunk_data.to_bytes endian chunk);
  decode = (fun shape dtype bytes -> Chunk_data.of_bytes dtype endian shape bytes);
}
