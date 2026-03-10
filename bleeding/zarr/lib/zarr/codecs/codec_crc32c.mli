(** {1 CRC32C Codec}

    Appends a Castagnoli CRC32C checksum to data for integrity
    verification.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/codecs/crc32c/index.html}
    Zarr v3.1 spec, "crc32c" codec}.  This is a {b bytes-to-bytes} codec.

    Uses the CRC32C polynomial [0x82F63B78] (Castagnoli) with a
    pre-computed 256-entry lookup table.  Encodes by appending a 4-byte
    little-endian unsigned checksum; decodes by verifying and stripping
    it.

    {b Algorithm:} RFC 3720 CRC32C.  Verified against the iSCSI test
    vector: CRC32C("123456789") = [0xe3069283].

    {b JSON config:} None (empty object or omitted). *)

val compute : bytes -> int32
(** [compute bytes] returns the CRC32C checksum of [bytes]. *)

val encode : bytes -> bytes
(** [encode bytes] appends a 4-byte little-endian CRC32C checksum. *)

val decode : bytes -> (bytes, [> `Checksum_mismatch ]) result
(** [decode bytes] verifies and strips the 4-byte CRC32C checksum.
    Returns [Error `Checksum_mismatch] if the stored checksum does not
    match the computed checksum. *)

val create : unit -> Zarr_codec.bytes_to_bytes
(** [create ()] builds a CRC32C checksum codec.
    [compute_encoded_size] returns [Some (size + 4)]. *)
