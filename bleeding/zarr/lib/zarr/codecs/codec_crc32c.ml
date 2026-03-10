(** CRC32C codec -- appends Castagnoli CRC32C checksum to data.

    See Zarr v3.1 spec, "crc32c" codec. This is a bytes-to-bytes codec.
    Appends a 4-byte little-endian CRC32C checksum on encode; verifies
    and strips it on decode. *)

let polynomial = 0x82F63B78l

(** Pre-computed CRC32C lookup table (256 entries). *)
let crc_table =
  Array.init 256 (fun i ->
    let crc = ref (Int32.of_int i) in
    for _ = 0 to 7 do
      if Int32.(logand !crc 1l <> 0l) then
        crc := Int32.(logxor (shift_right_logical !crc 1) polynomial)
      else
        crc := Int32.shift_right_logical !crc 1
    done;
    !crc)

(** [compute bytes] returns the CRC32C checksum of [bytes]. *)
let compute bytes =
  let crc = ref Int32.minus_one in
  for i = 0 to Bytes.length bytes - 1 do
    let byte = Char.code (Bytes.unsafe_get bytes i) in
    let index = Int32.to_int (Int32.logand (Int32.logxor !crc (Int32.of_int byte)) 0xFFl) in
    crc := Int32.logxor (Int32.shift_right_logical !crc 8) crc_table.(index)
  done;
  Int32.logxor !crc Int32.minus_one

let encode bytes =
  let len = Bytes.length bytes in
  let crc = compute bytes in
  let result = Bytes.create (len + 4) in
  Bytes.blit bytes 0 result 0 len;
  Bytes.set_int32_le result len crc;
  result

let decode bytes =
  let len = Bytes.length bytes in
  if len < 4 then
    Error `Checksum_mismatch
  else begin
    let data_len = len - 4 in
    let data = Bytes.sub bytes 0 data_len in
    let stored = Bytes.get_int32_le bytes data_len in
    let computed = compute data in
    if Int32.equal stored computed then Ok data
    else Error `Checksum_mismatch
  end

(** [create ()] builds a CRC32C checksum codec. *)
let create () : Zarr_codec.bytes_to_bytes = {
  encode;
  decode;
  compute_encoded_size = (fun size -> Some (size + 4));
}
