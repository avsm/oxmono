(* Variable-width bit reading with little-endian semantics for Brotli *)

type t = {
  src : bytes;
  src_len : int;
  mutable byte_pos : int;
  mutable bit_pos : int;  (* 0-7: bits already read from current byte *)
}

exception End_of_input

(* Bit masks for extracting n bits *)
let[@inline always] bit_mask n =
  (1 lsl n) - 1

(* Get byte at position, returns 0 if past end (zero-padding) *)
let[@inline always] get_byte t pos =
  if pos < t.src_len then
    Char.code (Bytes.unsafe_get t.src pos)
  else
    0

let create ~src ~pos ~len =
  { src; src_len = pos + len; byte_pos = pos; bit_pos = 0 }

let create_from_string s =
  create ~src:(Bytes.unsafe_of_string s) ~pos:0 ~len:(String.length s)

let reset t =
  t.byte_pos <- 0;
  t.bit_pos <- 0

let position t =
  t.byte_pos * 8 + t.bit_pos

let bytes_remaining t =
  let total_bits = (t.src_len - t.byte_pos) * 8 - t.bit_pos in
  (total_bits + 7) / 8

let has_more t =
  t.byte_pos < t.src_len || t.bit_pos > 0

(* Read n bits (1-24) without advancing the position - optimized for common cases *)
let[@inline] peek_bits t n_bits =
  if n_bits = 0 then 0
  else begin
    let bit_offset = t.bit_pos in
    let byte_pos = t.byte_pos in
    let bits_needed = n_bits + bit_offset in
    (* Optimized path for reading up to 24 bits (most common) *)
    if bits_needed <= 24 && byte_pos + 2 < t.src_len then begin
      (* Read 3 bytes at once *)
      let b0 = Char.code (Bytes.unsafe_get t.src byte_pos) in
      let b1 = Char.code (Bytes.unsafe_get t.src (byte_pos + 1)) in
      let b2 = Char.code (Bytes.unsafe_get t.src (byte_pos + 2)) in
      let combined = b0 lor (b1 lsl 8) lor (b2 lsl 16) in
      (combined lsr bit_offset) land bit_mask n_bits
    end
    else begin
      (* Fallback for edge cases and larger reads *)
      let result = ref 0 in
      let bytes_shift = ref 0 in
      let buf_pos = ref byte_pos in
      while !bytes_shift < bits_needed do
        result := !result lor (get_byte t !buf_pos lsl !bytes_shift);
        bytes_shift := !bytes_shift + 8;
        incr buf_pos
      done;
      (!result lsr bit_offset) land bit_mask n_bits
    end
  end

(* Advance by n bits without reading *)
let skip_bits t n_bits =
  if n_bits > 0 then begin
    let next_in_bits = t.bit_pos + n_bits in
    t.bit_pos <- next_in_bits land 7;
    t.byte_pos <- t.byte_pos + (next_in_bits lsr 3)
  end

(* Read n bits (1-24) and advance position *)
let[@inline] read_bits t n_bits =
  let value = peek_bits t n_bits in
  skip_bits t n_bits;
  value

(* Read a single bit *)
let[@inline] read_bit t =
  read_bits t 1

(* Advance to next byte boundary *)
let align_to_byte t =
  if t.bit_pos <> 0 then begin
    t.bit_pos <- 0;
    t.byte_pos <- t.byte_pos + 1
  end

(* Copy n bytes to destination buffer, first aligning to byte boundary *)
let copy_bytes t ~dst ~dst_pos ~len =
  align_to_byte t;
  if len > 0 then begin
    let src_pos = t.byte_pos in
    if src_pos + len > t.src_len then
      raise End_of_input;
    Bytes.blit t.src src_pos dst dst_pos len;
    t.byte_pos <- src_pos + len
  end

(* Check if we have enough bits remaining *)
let check_bits t n_bits =
  let total_bits = (t.src_len - t.byte_pos) * 8 - t.bit_pos in
  total_bits >= n_bits
