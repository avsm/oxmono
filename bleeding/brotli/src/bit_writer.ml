(* Variable-width bit writing with little-endian semantics for Brotli *)

(* Use unboxed types for mutable fields.
   bit_pos is always 0-31 - use int8#.
   current_byte holds accumulated bits (up to 32 during writes) - use int32#.
   byte_pos is output position - keep as int.
   Mixed blocks store unboxed fields inline without allocation. *)
type t = {
  dst : bytes;
  dst_len : int;
  mutable byte_pos : int;
  mutable bit_pos : int8#;      (* 0-31: bits already written to current partial *)
  mutable current_byte : int32#; (* Accumulated bits for current partial output *)
}

(* Module aliases for unboxed type operations *)
module I8 = Stdlib_stable.Int8_u
module I32 = Int32_u

(* Convert int32# to int *)
let[@inline always] i32_to_int (x : int32#) : int = Int32.to_int (I32.to_int32 x)

(* Convert int to int32# *)
let[@inline always] i32_of_int (x : int) : int32# = I32.of_int32 (Int32.of_int x)

exception Buffer_overflow

let create ~dst ~pos ~len =
  { dst; dst_len = pos + len; byte_pos = pos; bit_pos = #0s; current_byte = #0l }

let position t =
  t.byte_pos * 8 + I8.to_int t.bit_pos

let bytes_written t =
  let bp = I8.to_int t.bit_pos in
  if bp = 0 then t.byte_pos else t.byte_pos + 1

(* Flush accumulated bits to output, return number of bytes written *)
let flush t =
  let bp = I8.to_int t.bit_pos in
  if bp > 0 then begin
    if t.byte_pos >= t.dst_len then raise Buffer_overflow;
    Bytes.unsafe_set t.dst t.byte_pos (Char.chr (i32_to_int t.current_byte));
    t.byte_pos <- t.byte_pos + 1;
    t.bit_pos <- #0s;
    t.current_byte <- #0l;
    t.byte_pos
  end
  else t.byte_pos

(* Write n bits (1-24) *)
let write_bits t n_bits value =
  if n_bits <= 0 then ()
  else begin
    (* Add bits to current accumulator *)
    let mutable cb = i32_to_int t.current_byte in
    let mutable bp = I8.to_int t.bit_pos in
    let mutable byte_pos = t.byte_pos in
    cb <- cb lor ((value land ((1 lsl n_bits) - 1)) lsl bp);
    bp <- bp + n_bits;

    (* Flush complete bytes *)
    while bp >= 8 do
      if byte_pos >= t.dst_len then raise Buffer_overflow;
      Bytes.unsafe_set t.dst byte_pos (Char.chr (cb land 0xFF));
      byte_pos <- byte_pos + 1;
      cb <- cb lsr 8;
      bp <- bp - 8
    done;
    t.current_byte <- i32_of_int cb;
    t.bit_pos <- I8.of_int bp;
    t.byte_pos <- byte_pos
  end

(* Write a single bit *)
let[@inline] write_bit t value =
  write_bits t 1 value

(* Align to next byte boundary by padding with zeros *)
let align_to_byte t =
  let bp = I8.to_int t.bit_pos in
  if bp > 0 then begin
    if t.byte_pos >= t.dst_len then raise Buffer_overflow;
    Bytes.unsafe_set t.dst t.byte_pos (Char.chr (i32_to_int t.current_byte));
    t.byte_pos <- t.byte_pos + 1;
    t.bit_pos <- #0s;
    t.current_byte <- #0l
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
