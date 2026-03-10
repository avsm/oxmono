(** {1 Bytes Codec}

    Serialises chunk data to/from bytes with endianness handling.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/codecs/bytes/index.html}
    Zarr v3.1 spec, "bytes" codec}.  This is an {b array-to-bytes} codec.

    Elements are encoded in lexicographic (C/row-major) order.  For
    column-major layout, a transpose codec should precede the bytes codec.

    {b Performance:} When the endianness matches native order
    (little-endian on x86/ARM), [encode] is a simple [Bytes.copy] and
    [decode] avoids byte-swapping entirely.  Raw types
    ({!Zarr_dtype.Raw}) are never byte-swapped regardless of the
    requested endianness. *)

val create : Zarr_dtype.endian -> Zarr_codec.array_to_bytes
(** [create endian] builds a bytes codec for the given byte order. *)
