(** CRC32C codec - appends CRC32C checksum to data *)

(** CRC32C polynomial (Castagnoli) *)
let polynomial = 0x82F63B78l

(** CRC32C lookup table *)
let crc_table =
  Array.init 256 (fun i ->
    let crc = ref (Int32.of_int i) in
    for _ = 0 to 7 do
      if Int32.(logand !crc 1l <> 0l) then
        crc := Int32.(logxor (shift_right_logical !crc 1) polynomial)
      else
        crc := Int32.shift_right_logical !crc 1
    done;
    !crc
  )

(** Compute CRC32C checksum of bytes *)
let compute bytes =
  let crc = ref Int32.minus_one in  (* Start with 0xFFFFFFFF *)
  for i = 0 to Bytes.length bytes - 1 do
    let byte = Char.code (Bytes.get bytes i) in
    let index = Int32.to_int (Int32.logand (Int32.logxor !crc (Int32.of_int byte)) 0xFFl) in
    crc := Int32.logxor (Int32.shift_right_logical !crc 8) crc_table.(index)
  done;
  Int32.logxor !crc Int32.minus_one  (* Final XOR with 0xFFFFFFFF *)

(** Encode: append CRC32C checksum (little-endian) *)
let encode bytes =
  let crc = compute bytes in
  let result = Bytes.create (Bytes.length bytes + 4) in
  Bytes.blit bytes 0 result 0 (Bytes.length bytes);
  Bytes.set_int32_le result (Bytes.length bytes) crc;
  result

(** Decode: verify and strip CRC32C checksum *)
let decode bytes =
  let len = Bytes.length bytes in
  if len < 4 then
    Error `Checksum_mismatch
  else begin
    let data = Bytes.sub bytes 0 (len - 4) in
    let stored_crc = Bytes.get_int32_le bytes (len - 4) in
    let computed_crc = compute data in
    if Int32.equal stored_crc computed_crc then
      Ok data
    else
      Error `Checksum_mismatch
  end

(** Create a CRC32C codec *)
let create () : Codec_intf.bytes_to_bytes = {
  encode;
  decode;
  compute_encoded_size = (fun size -> Some (size + 4));
}
