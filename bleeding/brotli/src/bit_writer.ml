(* Variable-width bit writing with little-endian semantics for Brotli *)

type t = {
  dst : bytes;
  dst_len : int;
  mutable byte_pos : int;
  mutable bit_pos : int;  (* 0-7: bits already written to current byte *)
  mutable current_byte : int;  (* Accumulated bits for current byte *)
}

exception Buffer_overflow

let create ~dst ~pos ~len =
  { dst; dst_len = pos + len; byte_pos = pos; bit_pos = 0; current_byte = 0 }

let position t =
  t.byte_pos * 8 + t.bit_pos

let bytes_written t =
  if t.bit_pos = 0 then
    t.byte_pos
  else
    t.byte_pos + 1

(* Flush accumulated bits to output, return number of bytes written *)
let flush t =
  if t.bit_pos > 0 then begin
    if t.byte_pos >= t.dst_len then raise Buffer_overflow;
    Bytes.unsafe_set t.dst t.byte_pos (Char.chr t.current_byte);
    t.byte_pos <- t.byte_pos + 1;
    t.bit_pos <- 0;
    t.current_byte <- 0
  end;
  t.byte_pos

(* Write n bits (1-24) *)
let write_bits t n_bits value =
  if n_bits <= 0 then ()
  else begin
    (* Add bits to current accumulator *)
    t.current_byte <- t.current_byte lor ((value land ((1 lsl n_bits) - 1)) lsl t.bit_pos);
    t.bit_pos <- t.bit_pos + n_bits;

    (* Flush complete bytes *)
    while t.bit_pos >= 8 do
      if t.byte_pos >= t.dst_len then raise Buffer_overflow;
      Bytes.unsafe_set t.dst t.byte_pos (Char.chr (t.current_byte land 0xFF));
      t.byte_pos <- t.byte_pos + 1;
      t.current_byte <- t.current_byte lsr 8;
      t.bit_pos <- t.bit_pos - 8
    done
  end

(* Write a single bit *)
let[@inline] write_bit t value =
  write_bits t 1 value

(* Align to next byte boundary by padding with zeros *)
let align_to_byte t =
  if t.bit_pos > 0 then begin
    if t.byte_pos >= t.dst_len then raise Buffer_overflow;
    Bytes.unsafe_set t.dst t.byte_pos (Char.chr t.current_byte);
    t.byte_pos <- t.byte_pos + 1;
    t.bit_pos <- 0;
    t.current_byte <- 0
  end

(* Copy raw bytes to output, first aligning to byte boundary *)
let copy_bytes t ~src ~src_pos ~len =
  align_to_byte t;
  if len > 0 then begin
    if t.byte_pos + len > t.dst_len then raise Buffer_overflow;
    Bytes.blit src src_pos t.dst t.byte_pos len;
    t.byte_pos <- t.byte_pos + len
  end

(* Write a byte directly (for uncompressed blocks) *)
let write_byte t value =
  write_bits t 8 value

(* Write a 16-bit little-endian value *)
let write_u16 t value =
  write_bits t 16 value

(* Write a 32-bit little-endian value (in two parts to avoid overflow) *)
let write_u32 t value =
  write_bits t 16 (value land 0xFFFF);
  write_bits t 16 ((value lsr 16) land 0xFFFF)
