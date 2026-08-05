(* High-performance bit reader for Brotli decompression.

   Design based on C Brotli's bit_reader.h:
   - 64-bit accumulator holds pre-fetched bits
   - bit_pos tracks available bits (not consumed bits)
   - Refill when accumulator drops below 32 bits
   - All hot functions are inline always

   Key insight: The bit reader is called millions of times per MB.
   Every saved instruction has massive impact on throughput.

   Optimization strategy:
   1. Keep a 64-bit accumulator with up to 56 bits valid
   2. Refill when bit_pos <= 32 (using 32-bit loads to avoid overflow)
   3. Use pre-computed bit masks for fast masking
   4. All field accesses inline - no function call overhead
*)

(* Use unboxed types for mutable fields to avoid tagging overhead.
   bit_pos is always 0-64, so int8# suffices.
   val_ holds up to 64 bits of accumulated data - use nativeint#.
   next_pos is a byte position - keep as int since it's already efficient.
   Mixed blocks store unboxed fields inline without allocation. *)
type t = {
  src : bytes;
  src_len : int;
  mutable next_pos : int;         (* Next byte position to read from *)
  mutable val_ : nativeint#;      (* Accumulator with pre-fetched bits - unboxed *)
  mutable bit_pos : int8#;        (* Number of valid bits in val_ (0-64) - unboxed int8 *)
}

exception End_of_input

(* Module aliases for unboxed type operations *)
module I8 = Stdlib_stable.Int8_u
module Ni = Nativeint_u

(* Pre-computed bit masks for 0-32 bits - avoids (1 lsl n) - 1 computation.
   Uses nativeint# array for unboxed storage. *)
let bit_masks : nativeint# array = [|
  #0x0n; #0x1n; #0x3n; #0x7n;
  #0xFn; #0x1Fn; #0x3Fn; #0x7Fn;
  #0xFFn; #0x1FFn; #0x3FFn; #0x7FFn;
  #0xFFFn; #0x1FFFn; #0x3FFFn; #0x7FFFn;
  #0xFFFFn; #0x1FFFFn; #0x3FFFFn; #0x7FFFFn;
  #0xFFFFFn; #0x1FFFFFn; #0x3FFFFFn; #0x7FFFFFn;
  #0xFFFFFFn; #0x1FFFFFFn; #0x3FFFFFFn; #0x7FFFFFFn;
  #0xFFFFFFFn; #0x1FFFFFFFn; #0x3FFFFFFFn; #0x7FFFFFFFn;
  #0xFFFFFFFFn
|]

(* Bit mask for n bits - uses lookup table for all common cases (0-32) *)
let[@inline always] bit_mask n =
  if n <= 32 then Ni.to_int_trunc (Oxcaml_arrays.unsafe_get bit_masks n)
  else (1 lsl n) - 1

let create ~src ~pos ~len =
  { src; src_len = pos + len; next_pos = pos; val_ = #0n; bit_pos = #0s }

let create_from_string s =
  create ~src:(Bytes.unsafe_of_string s) ~pos:0 ~len:(String.length s)

let reset t =
  t.next_pos <- 0;
  t.val_ <- #0n;
  t.bit_pos <- #0s

let[@inline always] position t =
  (* Position in bits = bytes consumed * 8 - bits still in accumulator *)
  t.next_pos * 8 - I8.to_int t.bit_pos

let[@inline always] bytes_remaining t =
  let total_bits = (t.src_len - t.next_pos) * 8 + I8.to_int t.bit_pos in
  (total_bits + 7) / 8

let[@inline always] has_more t =
  I8.to_int t.bit_pos > 0 || t.next_pos < t.src_len

(* Fill the bit window - CRITICAL HOT PATH
   Refills accumulator when it has <= 32 bits.
   On 64-bit OCaml, int has 63 usable bits, so we can hold up to 56 bits safely.

   Strategy: Load 32 bits at a time (not 64) to avoid overflow issues when
   shifting into the accumulator. With 32-bit loads and refill threshold of 32,
   we can have at most 32 + 32 = 64 bits, which fits in 63-bit OCaml int since
   we never actually have all 64 bits set simultaneously. *)
let[@inline always] fill_bit_window t =
  let bp = I8.to_int t.bit_pos in
  if bp <= 32 then begin
    let next_pos = t.next_pos in
    let src_len = t.src_len in
    (* Fast path: 4 bytes available - use 32-bit native load *)
    if next_pos + 4 <= src_len then begin
      (* Load 32 bits and merge into accumulator.
         Int32.to_int on 64-bit OCaml preserves all 32 bits.
         The land 0xFFFFFFFF ensures we have an unsigned value. *)
      let new_bits = Int32.to_int (Bytes.get_int32_le t.src next_pos) land 0xFFFFFFFF in
      let v = Ni.to_int_trunc t.val_ in
      t.val_ <- Ni.of_int (v lor (new_bits lsl bp));
      t.bit_pos <- I8.of_int (bp + 32);
      t.next_pos <- next_pos + 4
    end
    (* Slow path: near end of input, load byte by byte *)
    else begin
      let mutable bp = bp in
      let mutable np = next_pos in
      let mutable v = Ni.to_int_trunc t.val_ in
      while bp <= 56 && np < src_len do
        let b = Char.code (Bytes.unsafe_get t.src np) in
        v <- v lor (b lsl bp);
        bp <- bp + 8;
        np <- np + 1
      done;
      t.val_ <- Ni.of_int v;
      t.bit_pos <- I8.of_int bp;
      t.next_pos <- np
    end
  end

(* Peek n bits without consuming - HOT PATH
   Ensures accumulator has enough bits, then masks the value. *)
let[@inline always] peek_bits t n_bits =
  if I8.to_int t.bit_pos < n_bits then fill_bit_window t;
  Ni.to_int_trunc t.val_ land bit_mask n_bits

(* Skip n bits without reading value - HOT PATH
   Simply shifts the accumulator and decrements the count. *)
let[@inline always] skip_bits t n_bits =
  t.val_ <- Ni.of_int (Ni.to_int_trunc t.val_ lsr n_bits);
  t.bit_pos <- I8.of_int (I8.to_int t.bit_pos - n_bits)

(* Read n bits and advance - HOT PATH
   This is the most called function in the entire decoder.
   Combines peek and skip into a single operation. *)
let[@inline always] read_bits t n_bits =
  let bp = I8.to_int t.bit_pos in
  if bp < n_bits then fill_bit_window t;
  let v = Ni.to_int_trunc t.val_ in
  let value = v land bit_mask n_bits in
  t.val_ <- Ni.of_int (v lsr n_bits);
  t.bit_pos <- I8.of_int (I8.to_int t.bit_pos - n_bits);
  value

(* Read a single bit - HOT PATH
   Specialized version that avoids the mask lookup. *)
let[@inline always] read_bit t =
  if I8.to_int t.bit_pos < 1 then fill_bit_window t;
  let v = Ni.to_int_trunc t.val_ in
  let value = v land 1 in
  t.val_ <- Ni.of_int (v lsr 1);
  t.bit_pos <- I8.of_int (I8.to_int t.bit_pos - 1);
  value

(* Align to next byte boundary by discarding partial byte bits from accumulator.
   After this call, bit_pos will be a multiple of 8. *)
let[@inline] align_to_byte t =
  let bp = I8.to_int t.bit_pos in
  let extra = bp land 7 in
  if extra <> 0 then begin
    t.val_ <- Ni.of_int (Ni.to_int_trunc t.val_ lsr extra);
    t.bit_pos <- I8.of_int (bp - extra)
  end

(* Copy n bytes to destination buffer.
   First drains any complete bytes from the accumulator, then does direct copy
   from the source buffer. This is efficient for uncompressed meta-blocks. *)
let copy_bytes t ~dst ~dst_pos ~len =
  align_to_byte t;
  if len > 0 then begin
    let mutable copied = 0 in
    let mutable dpos = dst_pos in
    let mutable bp = I8.to_int t.bit_pos in
    let mutable v = Ni.to_int_trunc t.val_ in
    (* First drain bytes from accumulator *)
    while bp >= 8 && copied < len do
      Bytes.unsafe_set dst dpos (Char.unsafe_chr (v land 0xFF));
      v <- v lsr 8;
      bp <- bp - 8;
      dpos <- dpos + 1;
      copied <- copied + 1
    done;
    t.val_ <- Ni.of_int v;
    t.bit_pos <- I8.of_int bp;
    (* Then copy remaining directly from source *)
    let remaining = len - copied in
    if remaining > 0 then begin
      if t.next_pos + remaining > t.src_len then
        raise End_of_input;
      Bytes.blit t.src t.next_pos dst dpos remaining;
      t.next_pos <- t.next_pos + remaining
    end
  end

(* Check if we have enough bits remaining (including those in accumulator and source) *)
let[@inline always] check_bits t n_bits =
  let total_bits = (t.src_len - t.next_pos) * 8 + I8.to_int t.bit_pos in
  total_bits >= n_bits

(* Get byte at position for compatibility - returns 0 past end.
   Note: This reads from the source buffer directly, not from the accumulator. *)
let[@inline always] get_byte t pos =
  if pos < t.src_len then
    Char.code (Bytes.unsafe_get t.src pos)
  else
    0
