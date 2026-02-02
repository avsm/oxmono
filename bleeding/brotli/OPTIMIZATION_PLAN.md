# Brotli Optimization Plan

This document outlines further optimization opportunities for the pure OCaml Brotli implementation.

## Current State

- **Compression**: ~7.5x slower than C brotli
- **Decompression**: ~4x slower than C brotli
- **Stack-local decoder**: Implemented but ~2% slower than allocating version (needs tuning)

## Phase 1: Memory Layout Optimizations

### 1.1 Unboxed Decoder State Record
Convert `Decoder_state.t` from a regular record to a mixed block with unboxed fields:

```ocaml
type t = {
  (* Unboxed scalars - no indirection *)
  mutable bit_next_pos : int#;
  mutable bit_val : int#;
  mutable bit_pos : int#;
  mutable dist_rb_idx : int#;
  mutable huffman_slab_pos : int#;
  (* ... other mutable state as int# *)

  (* Arrays remain boxed *)
  huffman_slab : int array;
  dist_rb : int array;
  (* ... *)
}
```

**Expected benefit**: Reduced memory indirection, better cache locality.

### 1.2 Packed Huffman Tables with int16# Arrays
Current Huffman entries are packed into `int` (bits in low 8, value in upper bits). Consider:

```ocaml
(* Current: int array with manual packing *)
let entry = table.(idx) in
let bits = entry land 0xFF in
let value = entry lsr 8 in

(* Alternative: int16# array for tighter packing *)
type huffman_entry = #{ bits : int8#; value : int16# }
```

**Expected benefit**: 50% memory reduction for Huffman tables, better cache utilization.

### 1.3 Context Lookup with int8# Throughout
Currently using `int8# array` for context lookup but converting to `int` immediately. Keep as `int8#` longer:

```ocaml
(* Current *)
let v1 = Oxcaml_arrays.unsafe_get lookup (offset1 + prev_byte1) in
let context = Oxcaml_arrays.int8_to_int v1 lor ...

(* Optimized: operate on int8# directly where possible *)
```

## Phase 2: SIMD Optimizations

### 2.1 Match Finding (Encoder)
Use SIMD for comparing 16+ bytes at once during LZ77 match finding:

```ocaml
open Ocaml_simd_sse

let find_match_length ~src ~pos1 ~pos2 ~max_len =
  let mutable len = 0 in
  while len + 16 <= max_len do
    let v1 = Int8x16.Bytes.get src ~byte:(pos1 + len) in
    let v2 = Int8x16.Bytes.get src ~byte:(pos2 + len) in
    let eq = Int8x16.cmpeq v1 v2 in
    let mask = Int8x16.movemask eq in
    if mask = 0xFFFF then
      len <- len + 16
    else begin
      (* Count trailing ones to find first mismatch *)
      len <- len + count_trailing_ones mask;
      return len
    end
  done;
  (* Handle remainder byte-by-byte *)
  ...
```

**Expected benefit**: 4-8x speedup for match finding on large inputs.

### 2.2 Overlapping Copy with SIMD
For backward references with small distances, use SIMD pattern fill:

```ocaml
let copy_overlapping ~dst ~pos ~distance ~len =
  if distance = 1 then
    (* Fill with single byte - already optimized with Bytes.unsafe_fill *)
    ...
  else if distance <= 16 then
    (* Use SIMD shuffle to create repeating pattern *)
    let pattern = Int8x16.Bytes.get dst ~byte:(pos - distance) in
    let shuffled = create_repeat_pattern pattern distance in
    (* Write 16 bytes at a time *)
    ...
```

**Expected benefit**: 2-4x speedup for short-distance copies.

### 2.3 Huffman Gather Operations (Experimental)
Use AVX2 gather for parallel symbol lookups (limited applicability):

```ocaml
(* For batch literal decoding - needs restructuring *)
let decode_literals_batch ~table ~bit_buffer ~count =
  (* Extract multiple indices from bit buffer *)
  let indices = extract_indices bit_buffer count in
  (* Parallel gather from Huffman table *)
  let entries = Int32x8.gather table indices in
  ...
```

**Note**: Requires significant restructuring; may not be practical.

## Phase 3: Algorithmic Optimizations

### 3.1 Bit Reader: Native 64-bit Loads
Use `Bytes.get_int64_le` for filling the bit accumulator:

```ocaml
let[@inline always] fill_bit_window t =
  if t.bit_pos <= 32 && t.bit_next_pos + 8 <= t.src_len then begin
    let new_bits = Bytes.get_int64_le t.src t.bit_next_pos in
    t.bit_val <- t.bit_val lor (Int64.to_int new_bits lsl t.bit_pos);
    t.bit_pos <- t.bit_pos + 64;
    t.bit_next_pos <- t.bit_next_pos + 8
  end else ...
```

**Note**: Need to handle 64-bit overflow carefully on 64-bit OCaml.

### 3.2 Huffman Table: Flatten 2-Level Structure
Current 2-level Huffman tables require conditional branch. Consider:
- Larger root table (10-11 bits) to reduce 2nd-level lookups
- Trade memory for fewer branches in hot path

### 3.3 Block Type Prediction
Add branch prediction hints for common cases:
- Single block type (no type switching)
- Sequential block types

### 3.4 Speculative Literal Decoding
Decode multiple literals speculatively before checking block boundaries:

```ocaml
(* Current: check block length every literal *)
for _ = 0 to insert_length - 1 do
  if block_length.(0) = 0 then ...  (* branch every iteration *)
  ...
done

(* Optimized: batch process when block has enough remaining *)
let batch_size = min insert_length block_length.(0) in
for _ = 0 to batch_size - 1 do
  (* No block check needed *)
  ...
done;
(* Handle remainder with checks *)
```

## Phase 4: Encoder Optimizations

### 4.1 Hash Table with Native int32# Keys
Use unboxed int32# for hash keys to avoid tagging overhead:

```ocaml
type hash_entry = #{
  key : int32#;
  pos : int#;
}
```

### 4.2 Optimal Parser with Memoization
Current optimal parser (quality 10-11) could benefit from:
- Better state caching
- Reduced allocation in cost calculation
- SIMD for cost array operations

### 4.3 Parallel Block Encoding
For large inputs, encode blocks in parallel using domains:

```ocaml
let encode_parallel ~src ~quality =
  let num_blocks = calculate_blocks src in
  let results = Array.make num_blocks "" in
  Domain.parallel_for 0 (num_blocks - 1) (fun i ->
    results.(i) <- encode_block src i quality
  );
  combine_blocks results
```

## Phase 5: Zero-Allocation Path Tuning

### 5.1 Eliminate Array.sub in Hot Path
Current decoder state uses `Array.sub` for block type ring buffers:

```ocaml
(* Current - allocates *)
~block_type_rb:(Array.sub ds.Ds.block_type_rb 0 2)

(* Fix: pass offset instead *)
~block_type_rb:ds.Ds.block_type_rb ~rb_offset:0
```

### 5.2 Inline Huffman Symbol Read
The `read_symbol_slab` function adds call overhead. Consider:
- `[@inline always]` with careful code size management
- Specialized versions for common root_bits values (8, 9, 10)

### 5.3 Reduce Decoder State Size
Current state is ~200KB. Profile to identify:
- Which buffers are actually needed simultaneously
- Opportunities for buffer reuse within a meta-block

## Benchmarking Strategy

### Microbenchmarks
1. Bit reader throughput (bits/second)
2. Huffman decode throughput (symbols/second)
3. Match finding throughput (comparisons/second)
4. Copy operations (bytes/second by distance)

### Macrobenchmarks
1. Canterbury corpus (mixed text)
2. Silesia corpus (various file types)
3. Large binary files
4. Highly compressible data
5. Random/incompressible data

### Profiling Tools
- `perf` for CPU cycles and cache misses
- `memtrace` for allocation hotspots
- `magic-trace` for detailed execution traces

## Priority Order

1. **High Impact, Low Effort**
   - 1.1 Unboxed decoder state fields
   - 3.1 Native 64-bit bit reader loads
   - 5.1 Eliminate Array.sub allocations

2. **High Impact, Medium Effort**
   - 2.1 SIMD match finding
   - 3.4 Speculative literal decoding
   - 5.2 Inline Huffman reads

3. **Medium Impact, High Effort**
   - 2.2 SIMD overlapping copy
   - 4.1 Unboxed hash entries
   - 1.2 Packed Huffman tables

4. **Experimental**
   - 2.3 Huffman gather operations
   - 4.3 Parallel block encoding

## Success Criteria

- Decompression within 2x of C brotli
- Compression within 4x of C brotli (quality 4)
- Zero-allocation decoder faster than allocating version
- No regression in correctness (all tests pass)
