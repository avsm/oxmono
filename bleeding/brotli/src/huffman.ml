(* Canonical Huffman coding with 2-level lookup tables for Brotli

   Packed table format:
   - Each entry is a single int with bits packed as: value << 8 | bits
   - bits (low 8 bits): Number of bits used for this symbol, or total bits for 2nd level
   - value (bits 8-23): Symbol value, or offset to subtable

   This reduces memory footprint by ~50% vs boxed records and improves cache utilization.
   On 64-bit OCaml, each entry uses 8 bytes (int) instead of 24 bytes (record with 2 ints).
*)

module I16 = Stdlib_stable.Int16_u

let max_length = 15

(* A Huffman code entry - kept for API compatibility with existing code *)
type code = {
  bits : int;   (* Number of bits used for this symbol, or bits in subtable *)
  value : int;  (* Symbol value, or offset to subtable *)
}

(* Packed Huffman lookup table - each entry is: (value lsl 8) lor bits
   This is the primary table type for optimized code paths. *)
type table = int array

exception Invalid_huffman_tree

(* Pack bits and value into a single int *)
let[@inline always] pack_entry ~bits ~value =
  (value lsl 8) lor (bits land 0xFF)

(* Unpack bits from a packed entry *)
let[@inline always] unpack_bits entry =
  entry land 0xFF

(* Unpack value from a packed entry *)
let[@inline always] unpack_value entry =
  entry lsr 8

(* Returns reverse(reverse(key, len) + 1, len) for canonical code generation *)
let get_next_key key length =
  let rec loop step =
    if key land step = 0 then
      (key land (step - 1)) + step
    else
      loop (step lsr 1)
  in
  loop (1 lsl (length - 1))

(* Store code in table[i], table[i+step], table[i+2*step], ... *)
let replicate_value table base step table_end code =
  let rec loop index =
    if index >= base then begin
      table.(index) <- code;
      loop (index - step)
    end
  in
  loop (base + table_end - step)

(* Calculate the width of the next 2nd level table.
   This determines how many extra bits beyond root_bits we need for symbols
   starting at the given length. We scan forward through code lengths,
   tracking how much space remains in the subtable. *)
let next_table_bit_size (count : int16# array) length root_bits =
  let mutable left = 1 lsl (length - root_bits) in
  let mutable len = length in
  let mutable done_ = false in
  while not done_ && len < max_length do
    left <- left - I16.to_int (Oxcaml_arrays.unsafe_get count len);
    if left <= 0 then
      done_ <- true  (* Break - current len is the answer *)
    else begin
      len <- len + 1;
      left <- left lsl 1
    end
  done;
  len - root_bits

(* Helper to create an int16# array *)
let[@inline always] make_int16_array len : int16# array =
  Base.Array.create ~len #0S

(* Build a packed Huffman lookup table from code lengths - uses mutable locals *)
let build_table ~code_lengths ~alphabet_size ~root_bits =
  (* Use int16# arrays - count values ≤ alphabet_size (704), symbols ≤ 703 *)
  let count : int16# array = make_int16_array (max_length + 1) in
  let offset : int16# array = make_int16_array (max_length + 1) in
  let sorted_symbols : int16# array = make_int16_array alphabet_size in

  (* Build histogram of code lengths *)
  for symbol = 0 to alphabet_size - 1 do
    let len = code_lengths.(symbol) in
    let cur = Oxcaml_arrays.unsafe_get count len in
    Oxcaml_arrays.unsafe_set count len (I16.add cur #1S)
  done;

  (* Generate offsets into sorted symbol table by code length *)
  Oxcaml_arrays.unsafe_set offset 1 #0S;
  for length = 1 to max_length - 1 do
    let prev = Oxcaml_arrays.unsafe_get offset length in
    let cnt = Oxcaml_arrays.unsafe_get count length in
    Oxcaml_arrays.unsafe_set offset (length + 1) (I16.add prev cnt)
  done;

  (* Sort symbols by length, by symbol order within each length *)
  for symbol = 0 to alphabet_size - 1 do
    let length = code_lengths.(symbol) in
    if length <> 0 then begin
      let off = Oxcaml_arrays.unsafe_get offset length in
      Oxcaml_arrays.unsafe_set sorted_symbols (I16.to_int off) (I16.of_int symbol);
      Oxcaml_arrays.unsafe_set offset length (I16.add off #1S)
    end
  done;

  let mutable table_bits = root_bits in
  let mutable table_size = 1 lsl table_bits in
  let mutable total_size = table_size in

  (* Pre-allocate table with maximum possible size *)
  let max_table_size = table_size * 4 in  (* Conservative estimate *)
  let root_table = Array.make max_table_size 0 in

  (* Special case: code with only one value *)
  if I16.to_int (Oxcaml_arrays.unsafe_get offset max_length) = 1 then begin
    let sym0 = I16.to_int (Oxcaml_arrays.unsafe_get sorted_symbols 0) in
    let entry = pack_entry ~bits:0 ~value:(sym0 land 0xFFFF) in
    for key = 0 to total_size - 1 do
      root_table.(key) <- entry
    done;
    Array.sub root_table 0 total_size
  end
  else begin
    let mutable table = 0 in
    let mutable key = 0 in
    let mutable symbol = 0 in
    let mutable step = 2 in

    (* Fill in root table *)
    for length = 1 to root_bits do
      while I16.to_int (Oxcaml_arrays.unsafe_get count length) > 0 do
        let sym = I16.to_int (Oxcaml_arrays.unsafe_get sorted_symbols symbol) in
        let entry = pack_entry ~bits:(length land 0xFF) ~value:(sym land 0xFFFF) in
        symbol <- symbol + 1;
        replicate_value root_table (table + key) step table_size entry;
        key <- get_next_key key length;
        let cur = Oxcaml_arrays.unsafe_get count length in
        Oxcaml_arrays.unsafe_set count length (I16.sub cur #1S)
      done;
      step <- step lsl 1
    done;

    (* Fill in 2nd level tables and add pointers to root table *)
    let mask = total_size - 1 in
    let mutable low = -1 in
    step <- 2;
    let start_table = 0 in

    for length = root_bits + 1 to max_length do
      while I16.to_int (Oxcaml_arrays.unsafe_get count length) > 0 do
        if (key land mask) <> low then begin
          table <- table + table_size;
          table_bits <- next_table_bit_size count length root_bits;
          table_size <- 1 lsl table_bits;
          total_size <- total_size + table_size;
          low <- key land mask;
          root_table.(start_table + low) <- pack_entry
            ~bits:((table_bits + root_bits) land 0xFF)
            ~value:((table - start_table - low) land 0xFFFF)
        end;
        let sym = I16.to_int (Oxcaml_arrays.unsafe_get sorted_symbols symbol) in
        let entry = pack_entry ~bits:((length - root_bits) land 0xFF) ~value:(sym land 0xFFFF) in
        symbol <- symbol + 1;
        replicate_value root_table (table + (key lsr root_bits)) step table_size entry;
        key <- get_next_key key length;
        let cur = Oxcaml_arrays.unsafe_get count length in
        Oxcaml_arrays.unsafe_set count length (I16.sub cur #1S)
      done;
      step <- step lsl 1
    done;

    Array.sub root_table 0 total_size
  end

(* Read a symbol from the bit stream using a packed Huffman table.
   This is the hot path for Huffman decoding - we provide specialized
   versions for common root_bits values to avoid the variable shift overhead.

   OPTIMIZATION NOTES (following C brotli approach):
   - Inline bit reader operations directly to avoid function call overhead
   - Use unsafe array access since indices are always masked to valid ranges
   - Load bits once and extract everything from the loaded value
   - Update bit reader position only once at the end (avoids multiple writes)
   - Compute total bits to skip to reduce branches
*)

(* Inline bit mask computation - matches Bit_reader.bit_mask *)
let[@inline always] bit_mask n = (1 lsl n) - 1

(* Module aliases for unboxed type conversions in inlined bit reader *)
module I8 = Stdlib_stable.Int8_u
module Ni = Nativeint_u

(* Inline fill_bit_window - CRITICAL: fully inlined to avoid function call overhead.
   This is called for every Huffman symbol decode (millions of times per MB).
   Uses unboxed types matching Bit_reader.t's internal representation. *)
let[@inline always] inline_fill br =
  let bp = I8.to_int br.Bit_reader.bit_pos in
  if bp <= 32 then begin
    let next_pos = br.Bit_reader.next_pos in
    let src_len = br.Bit_reader.src_len in
    if next_pos + 4 <= src_len then begin
      let new_bits = Int32.to_int (Bytes.get_int32_le br.Bit_reader.src next_pos) land 0xFFFFFFFF in
      let v = Ni.to_int_trunc br.Bit_reader.val_ in
      br.Bit_reader.val_ <- Ni.of_int (v lor (new_bits lsl bp));
      br.Bit_reader.bit_pos <- I8.of_int (bp + 32);
      br.Bit_reader.next_pos <- next_pos + 4
    end else begin
      let mutable bp = bp in
      let mutable np = next_pos in
      let mutable v = Ni.to_int_trunc br.Bit_reader.val_ in
      while bp <= 56 && np < src_len do
        let b = Char.code (Bytes.unsafe_get br.Bit_reader.src np) in
        v <- v lor (b lsl bp);
        bp <- bp + 8;
        np <- np + 1
      done;
      br.Bit_reader.val_ <- Ni.of_int v;
      br.Bit_reader.bit_pos <- I8.of_int bp;
      br.Bit_reader.next_pos <- np
    end
  end

(* Inline peek_bits for 15 bits - the hot path.
   Uses the accumulator-based bit reader design with fully inlined fill. *)
let[@inline always] inline_peek_15 br =
  if I8.to_int br.Bit_reader.bit_pos < 15 then inline_fill br;
  Ni.to_int_trunc br.Bit_reader.val_ land 0x7FFF

(* Inline skip_bits - consume bits from the accumulator *)
let[@inline always] inline_skip br n_bits =
  br.Bit_reader.val_ <- Ni.of_int (Ni.to_int_trunc br.Bit_reader.val_ lsr n_bits);
  br.Bit_reader.bit_pos <- I8.of_int (I8.to_int br.Bit_reader.bit_pos - n_bits)

(* Specialized version for root_bits = 8 (literals, block types, context maps).
   This is the most critical hot path - called for every literal byte.

   SAFETY: initial_idx is masked to 0-255, which is always within bounds of
   the root table (size >= 256). Second-level access uses offset from entry
   which was computed during table construction to be valid. *)
let[@inline always] read_symbol_8 table br =
  let bits = inline_peek_15 br in
  let initial_idx = bits land 0xFF in  (* 8-bit mask, always < 256 *)
  let entry = Array.unsafe_get table initial_idx in
  let entry_bits = entry land 0xFF in
  if entry_bits <= 8 then begin
    (* Fast path: symbol found in root table - just skip the bits *)
    inline_skip br entry_bits;
    entry lsr 8
  end
  else begin
    (* 2nd level lookup needed - compute total bits and skip once *)
    let extra_bits = entry_bits - 8 in
    let idx2 = (bits lsr 8) land (bit_mask extra_bits) in
    let entry2 = Array.unsafe_get table (initial_idx + (entry lsr 8) + idx2) in
    let entry2_bits = entry2 land 0xFF in
    (* Skip root_bits + entry2_bits in one operation *)
    inline_skip br (8 + entry2_bits);
    entry2 lsr 8
  end

(* Specialized version for root_bits = 10 (commands).
   SAFETY: initial_idx is masked to 0-1023, which is always within bounds of
   the root table (size >= 1024 for command tables). *)
let[@inline always] read_symbol_10 table br =
  let bits = inline_peek_15 br in
  let initial_idx = bits land 0x3FF in  (* 10-bit mask, always < 1024 *)
  let entry = Array.unsafe_get table initial_idx in
  let entry_bits = entry land 0xFF in
  if entry_bits <= 10 then begin
    inline_skip br entry_bits;
    entry lsr 8
  end
  else begin
    let extra_bits = entry_bits - 10 in
    let idx2 = (bits lsr 10) land (bit_mask extra_bits) in
    let entry2 = Array.unsafe_get table (initial_idx + (entry lsr 8) + idx2) in
    let entry2_bits = entry2 land 0xFF in
    inline_skip br (10 + entry2_bits);
    entry2 lsr 8
  end

(* Specialized version for root_bits = 5 (code length codes).
   SAFETY: initial_idx is masked to 0-31, which is always within bounds of
   the root table (size >= 32 for code length tables). *)
let[@inline always] read_symbol_5 table br =
  let bits = inline_peek_15 br in
  let initial_idx = bits land 0x1F in  (* 5-bit mask, always < 32 *)
  let entry = Array.unsafe_get table initial_idx in
  let entry_bits = entry land 0xFF in
  if entry_bits <= 5 then begin
    inline_skip br entry_bits;
    entry lsr 8
  end
  else begin
    let extra_bits = entry_bits - 5 in
    let idx2 = (bits lsr 5) land (bit_mask extra_bits) in
    let entry2 = Array.unsafe_get table (initial_idx + (entry lsr 8) + idx2) in
    let entry2_bits = entry2 land 0xFF in
    inline_skip br (5 + entry2_bits);
    entry2 lsr 8
  end

(* Generic version for uncommon root_bits values - not on the hot path.
   SAFETY: initial_idx is masked to fit within root_bits, which guarantees it's
   within the root table bounds. *)
let[@inline always] read_symbol table root_bits br =
  let bits = inline_peek_15 br in
  let root_mask = (1 lsl root_bits) - 1 in
  let initial_idx = bits land root_mask in
  let entry = Array.unsafe_get table initial_idx in
  let entry_bits = entry land 0xFF in
  if entry_bits <= root_bits then begin
    inline_skip br entry_bits;
    entry lsr 8
  end
  else begin
    let extra_bits = entry_bits - root_bits in
    let idx2 = (bits lsr root_bits) land (bit_mask extra_bits) in
    let entry2 = Array.unsafe_get table (initial_idx + (entry lsr 8) + idx2) in
    let entry2_bits = entry2 land 0xFF in
    inline_skip br (root_bits + entry2_bits);
    entry2 lsr 8
  end

(* Build packed Huffman table for simple prefix codes (1-4 symbols) *)
let build_simple_table symbols num_symbols =
  let table_size = 1 lsl Constants.huffman_max_table_bits in
  let table = Array.make table_size 0 in

  match num_symbols with
  | 1 ->
    (* Single symbol - use 0 bits *)
    let entry = pack_entry ~bits:0 ~value:symbols.(0) in
    for i = 0 to table_size - 1 do
      table.(i) <- entry
    done;
    table
  | 2 ->
    (* Two symbols - 1 bit each *)
    let half = table_size / 2 in
    let entry0 = pack_entry ~bits:1 ~value:symbols.(0) in
    let entry1 = pack_entry ~bits:1 ~value:symbols.(1) in
    for i = 0 to half - 1 do
      table.(i) <- entry0
    done;
    for i = half to table_size - 1 do
      table.(i) <- entry1
    done;
    table
  | 3 ->
    (* Three symbols: 1, 2, 2 bits *)
    let quarter = table_size / 4 in
    let entry0 = pack_entry ~bits:1 ~value:symbols.(0) in
    let entry1 = pack_entry ~bits:2 ~value:symbols.(1) in
    let entry2 = pack_entry ~bits:2 ~value:symbols.(2) in
    for i = 0 to quarter - 1 do
      table.(i) <- entry0
    done;
    for i = quarter to 2 * quarter - 1 do
      table.(i) <- entry1
    done;
    for i = 2 * quarter to table_size - 1 do
      table.(i) <- entry2
    done;
    table
  | 4 ->
    (* Four symbols: 2 bits each, with tree-select bit *)
    let quarter = table_size / 4 in
    let entry0 = pack_entry ~bits:2 ~value:symbols.(0) in
    let entry1 = pack_entry ~bits:2 ~value:symbols.(1) in
    let entry2 = pack_entry ~bits:2 ~value:symbols.(2) in
    let entry3 = pack_entry ~bits:2 ~value:symbols.(3) in
    for i = 0 to quarter - 1 do
      table.(i) <- entry0
    done;
    for i = quarter to 2 * quarter - 1 do
      table.(i) <- entry1
    done;
    for i = 2 * quarter to 3 * quarter - 1 do
      table.(i) <- entry2
    done;
    for i = 3 * quarter to table_size - 1 do
      table.(i) <- entry3
    done;
    table
  | _ ->
    raise Invalid_huffman_tree

(* Maximum table sizes for different alphabet sizes - values up to 1080 *)
let max_table_sizes : nativeint# array = [|
  #256n; #402n; #436n; #468n; #500n; #534n; #566n; #598n;
  #630n; #662n; #694n; #726n; #758n; #790n; #822n; #854n;
  #886n; #918n; #950n; #982n; #1014n; #1046n; #1078n; #1080n
|]

(* Get maximum table size for a given alphabet size *)
let max_table_size alphabet_size =
  if alphabet_size <= 256 then 256
  else if alphabet_size <= 704 then 1080
  else 2048  (* Large alphabets *)

(* ==========================================================================
   BATCH SYMBOL DECODING FOR TRIVIAL LITERAL CONTEXTS
   ==========================================================================
   Following C brotli's BrotliCopyPreloadedSymbolsToU8 pattern.
   When we have a trivial context (single Huffman tree for all literals),
   we can decode multiple symbols in a tight loop without context overhead.

   Key optimizations:
   - Single table lookup per symbol (no context map indirection)
   - Loop unrolling for better instruction-level parallelism
   - Direct byte writes to output buffer
   ========================================================================== *)

(* Decode multiple 8-bit symbols into a byte buffer.
   Returns the number of symbols actually decoded (may be less than limit if
   bit buffer runs low near end of input).

   SAFETY: Caller must ensure dst has room for at least `limit` bytes starting at pos.
   Uses unsafe byte writes since bounds are checked by caller. *)
let[@inline] decode_symbols_to_bytes_8 table br dst pos limit =
  let mutable p = pos in
  let end_pos = pos + limit in
  while p < end_pos do
    let bits = inline_peek_15 br in
    let initial_idx = bits land 0xFF in
    let entry = Array.unsafe_get table initial_idx in
    let entry_bits = entry land 0xFF in
    if entry_bits <= 8 then begin
      inline_skip br entry_bits;
      Bytes.unsafe_set dst p (Char.unsafe_chr (entry lsr 8));
      p <- p + 1
    end
    else begin
      let extra_bits = entry_bits - 8 in
      let idx2 = (bits lsr 8) land (bit_mask extra_bits) in
      let entry2 = Array.unsafe_get table (initial_idx + (entry lsr 8) + idx2) in
      let entry2_bits = entry2 land 0xFF in
      inline_skip br (8 + entry2_bits);
      Bytes.unsafe_set dst p (Char.unsafe_chr (entry2 lsr 8));
      p <- p + 1
    end
  done;
  p - pos
