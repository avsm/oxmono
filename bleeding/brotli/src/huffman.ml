(* Canonical Huffman coding with 2-level lookup tables for Brotli

   Packed table format:
   - Each entry is a single int with bits packed as: value << 8 | bits
   - bits (low 8 bits): Number of bits used for this symbol, or total bits for 2nd level
   - value (bits 8-23): Symbol value, or offset to subtable

   This reduces memory footprint by ~50% vs boxed records and improves cache utilization.
   On 64-bit OCaml, each entry uses 8 bytes (int) instead of 24 bytes (record with 2 ints).
*)

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

(* Calculate the width of the next 2nd level table *)
let next_table_bit_size count length root_bits =
  let left = ref (1 lsl (length - root_bits)) in
  let len = ref length in
  while !len < max_length do
    left := !left - count.(!len);
    if !left <= 0 then
      len := max_length  (* Break *)
    else begin
      incr len;
      left := !left lsl 1
    end
  done;
  !len - root_bits

(* Build a packed Huffman lookup table from code lengths *)
let build_table ~code_lengths ~alphabet_size ~root_bits =
  let count = Array.make (max_length + 1) 0 in
  let offset = Array.make (max_length + 1) 0 in
  let sorted_symbols = Array.make alphabet_size 0 in

  (* Build histogram of code lengths *)
  for symbol = 0 to alphabet_size - 1 do
    let len = code_lengths.(symbol) in
    count.(len) <- count.(len) + 1
  done;

  (* Generate offsets into sorted symbol table by code length *)
  offset.(1) <- 0;
  for length = 1 to max_length - 1 do
    offset.(length + 1) <- offset.(length) + count.(length)
  done;

  (* Sort symbols by length, by symbol order within each length *)
  for symbol = 0 to alphabet_size - 1 do
    let length = code_lengths.(symbol) in
    if length <> 0 then begin
      sorted_symbols.(offset.(length)) <- symbol;
      offset.(length) <- offset.(length) + 1
    end
  done;

  let table_bits = ref root_bits in
  let table_size = ref (1 lsl !table_bits) in
  let total_size = ref !table_size in

  (* Pre-allocate table with maximum possible size *)
  let max_table_size = !table_size * 4 in  (* Conservative estimate *)
  let root_table = Array.make max_table_size 0 in

  (* Special case: code with only one value *)
  if offset.(max_length) = 1 then begin
    let entry = pack_entry ~bits:0 ~value:(sorted_symbols.(0) land 0xFFFF) in
    for key = 0 to !total_size - 1 do
      root_table.(key) <- entry
    done;
    Array.sub root_table 0 !total_size
  end
  else begin
    let table = ref 0 in
    let key = ref 0 in
    let symbol = ref 0 in
    let step = ref 2 in

    (* Fill in root table *)
    for length = 1 to root_bits do
      while count.(length) > 0 do
        let entry = pack_entry ~bits:(length land 0xFF) ~value:(sorted_symbols.(!symbol) land 0xFFFF) in
        incr symbol;
        replicate_value root_table (!table + !key) !step !table_size entry;
        key := get_next_key !key length;
        count.(length) <- count.(length) - 1
      done;
      step := !step lsl 1
    done;

    (* Fill in 2nd level tables and add pointers to root table *)
    let mask = !total_size - 1 in
    let low = ref (-1) in
    step := 2;
    let start_table = 0 in

    for length = root_bits + 1 to max_length do
      while count.(length) > 0 do
        if (!key land mask) <> !low then begin
          table := !table + !table_size;
          table_bits := next_table_bit_size count length root_bits;
          table_size := 1 lsl !table_bits;
          total_size := !total_size + !table_size;
          low := !key land mask;
          root_table.(start_table + !low) <- pack_entry
            ~bits:((!table_bits + root_bits) land 0xFF)
            ~value:((!table - start_table - !low) land 0xFFFF)
        end;
        let entry = pack_entry ~bits:((length - root_bits) land 0xFF) ~value:(sorted_symbols.(!symbol) land 0xFFFF) in
        incr symbol;
        replicate_value root_table (!table + (!key lsr root_bits)) !step !table_size entry;
        key := get_next_key !key length;
        count.(length) <- count.(length) - 1
      done;
      step := !step lsl 1
    done;

    Array.sub root_table 0 !total_size
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

(* Inline fill_bit_window - CRITICAL: fully inlined to avoid function call overhead.
   This is called for every Huffman symbol decode (millions of times per MB). *)
let[@inline always] inline_fill br =
  if br.Bit_reader.bit_pos <= 32 then begin
    let next_pos = br.Bit_reader.next_pos in
    let src_len = br.Bit_reader.src_len in
    if next_pos + 4 <= src_len then begin
      let new_bits = Int32.to_int (Bytes.get_int32_le br.Bit_reader.src next_pos) land 0xFFFFFFFF in
      br.Bit_reader.val_ <- br.Bit_reader.val_ lor (new_bits lsl br.Bit_reader.bit_pos);
      br.Bit_reader.bit_pos <- br.Bit_reader.bit_pos + 32;
      br.Bit_reader.next_pos <- next_pos + 4
    end else begin
      let mutable bp = br.Bit_reader.bit_pos in
      let mutable np = next_pos in
      let mutable v = br.Bit_reader.val_ in
      while bp <= 56 && np < src_len do
        let b = Char.code (Bytes.unsafe_get br.Bit_reader.src np) in
        v <- v lor (b lsl bp);
        bp <- bp + 8;
        np <- np + 1
      done;
      br.Bit_reader.val_ <- v;
      br.Bit_reader.bit_pos <- bp;
      br.Bit_reader.next_pos <- np
    end
  end

(* Inline peek_bits for 15 bits - the hot path.
   Uses the accumulator-based bit reader design with fully inlined fill. *)
let[@inline always] inline_peek_15 br =
  if br.Bit_reader.bit_pos < 15 then inline_fill br;
  br.Bit_reader.val_ land 0x7FFF

(* Inline skip_bits - consume bits from the accumulator *)
let[@inline always] inline_skip br n_bits =
  br.Bit_reader.val_ <- br.Bit_reader.val_ lsr n_bits;
  br.Bit_reader.bit_pos <- br.Bit_reader.bit_pos - n_bits

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

(* Maximum table sizes for different alphabet sizes *)
let max_table_sizes = [|
  256; 402; 436; 468; 500; 534; 566; 598;
  630; 662; 694; 726; 758; 790; 822; 854;
  886; 918; 950; 982; 1014; 1046; 1078; 1080
|]

(* Get maximum table size for a given alphabet size *)
let max_table_size alphabet_size =
  if alphabet_size <= 256 then 256
  else if alphabet_size <= 704 then 1080
  else 2048  (* Large alphabets *)
