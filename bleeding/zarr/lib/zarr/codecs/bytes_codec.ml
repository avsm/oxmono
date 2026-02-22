(** Bytes codec - converts arrays to/from bytes with endianness handling *)

(** Create a bytes codec with specified endianness *)
let create endian : Codec_intf.array_to_bytes = {
  encode = (fun arr -> Ndarray.to_bytes endian arr);
  decode = (fun shape dtype bytes -> Ndarray.of_bytes dtype endian shape bytes);
}
