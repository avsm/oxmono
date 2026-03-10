(** CRC32C codec -- appends Castagnoli CRC32C checksum to data.

    See Zarr v3.1 spec, "crc32c" codec. This is a bytes-to-bytes codec.
    Appends a 4-byte little-endian CRC32C checksum on encode; verifies
    and strips it on decode.

    Uses native [int] for the CRC computation to avoid [int32] boxing
    in the hot loop.  On 64-bit OCaml the 32-bit CRC values fit without
    overflow. *)

let polynomial = 0x82F63B78

(** Pre-computed CRC32C lookup table (256 entries, native int). *)
let crc_table =
  Array.init 256 (fun i ->
    let mutable crc = i in
    for _ = 0 to 7 do
      if crc land 1 <> 0 then
        crc <- (crc lsr 1) lxor polynomial
      else
        crc <- crc lsr 1
    done;
    crc)

(** [compute bytes] returns the CRC32C checksum of [bytes] as a native int
    (lower 32 bits). *)
let[@inline] compute bytes =
  let len = Bytes.length bytes in
  let mutable crc = 0xFFFFFFFF in
  for i = 0 to len - 1 do
    let byte = Char.code (Bytes.unsafe_get bytes i) in
    crc <- (crc lsr 8) lxor Array.unsafe_get crc_table ((crc lxor byte) land 0xFF)
  done;
  crc lxor 0xFFFFFFFF

(** [compute_range bytes off len] computes CRC32C over a sub-range
    without copying. *)
let[@inline] compute_range bytes off len =
  let mutable crc = 0xFFFFFFFF in
  for i = off to off + len - 1 do
    let byte = Char.code (Bytes.unsafe_get bytes i) in
    crc <- (crc lsr 8) lxor Array.unsafe_get crc_table ((crc lxor byte) land 0xFF)
  done;
  crc lxor 0xFFFFFFFF

let encode bytes =
  let len = Bytes.length bytes in
  let crc = compute bytes in
  let result = Bytes.create (len + 4) in
  Bytes.blit bytes 0 result 0 len;
  Bytes.set_int32_le result len (Int32.of_int crc);
  result

let decode bytes =
  let len = Bytes.length bytes in
  if len < 4 then raise Zarr_codec.Checksum_mismatch;
  let data_len = len - 4 in
  let stored = Int32.to_int (Bytes.get_int32_le bytes data_len) land 0xFFFFFFFF in
  let computed = compute_range bytes 0 data_len in
  if stored <> computed then raise Zarr_codec.Checksum_mismatch;
  Bytes.sub bytes 0 data_len

(** [create ()] builds a CRC32C checksum codec. *)
let create () : Zarr_codec.bytes_to_bytes = {
  encode;
  decode;
  compute_encoded_size = (fun size -> Some (size + 4));
}
