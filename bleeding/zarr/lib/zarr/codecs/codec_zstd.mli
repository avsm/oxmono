(** {1 Zstd Codec}

    Zstandard compression using the [zstd] package with OxCaml-optimised
    bindings.

    See Zarr v3.1 spec (extension codec, registered name ["zstd"]).
    This is a {b bytes-to-bytes} codec.

    Uses the buffer-passing API ([Zstd.compress_to], [Zstd.decompress_to])
    to avoid intermediate string allocations.

    {b JSON config:} [\\{"level": 1-22, "checksum": true|false\\}].
    Default level is 3, checksum is false. *)

val create : int -> Zarr_codec.bytes_to_bytes
(** [create level] builds a zstd codec at the given compression level
    (1-22).  [decode] raises {!Zarr_codec.Codec_error} on failure. *)
