(* LZ77 matching with distance ring buffer support for Brotli *)
(* Implements scoring and parameters matching brotli-c reference *)

(* Configuration *)
let hash_bits = 17
let hash_size = 1 lsl hash_bits
let min_match = 4
let max_match = 258
let window_bits = 22
let max_backward_distance = (1 lsl window_bits) - 16

(* Sentinel value for uninitialized hash/chain entries.
   We use -1 represented as nativeint# for unboxed arrays. *)
let invalid_pos : nativeint# = Nativeint_u.of_int (-1)

(* Helper to create a nativeint# array filled with a value *)
let make_nativeint_u_array len (v : nativeint#) : nativeint# array =
  let arr = Nativeint_u.Array.create_uninitialized ~len in
  for i = 0 to len - 1 do
    Nativeint_u.Array.set arr i v
  done;
  arr

(* Array accessors for nativeint# arrays *)
let[@inline always] na_get (arr : nativeint# array) idx = Nativeint_u.Array.get arr idx
let[@inline always] na_set (arr : nativeint# array) idx v = Nativeint_u.Array.set arr idx v

(* Scoring constants from brotli-c (hash.h) *)
let brotli_literal_byte_score = 135
let brotli_distance_bit_penalty = 30
(* BROTLI_SCORE_BASE = DISTANCE_BIT_PENALTY * 8 * sizeof(size_t) = 1920 on 64-bit *)
let brotli_score_base = brotli_distance_bit_penalty * 8 * 8

(* Block size (ring buffer size per bucket) by quality for H5 hasher.
   In brotli-c: block_bits = quality - 1 for q5-9 *)
let get_block_size quality =
  if quality <= 4 then 1
  else if quality = 5 then 16    (* 1 << 4 *)
  else if quality = 6 then 32    (* 1 << 5 *)
  else if quality = 7 then 64    (* 1 << 6 *)
  else if quality = 8 then 128   (* 1 << 7 *)
  else 256                       (* 1 << 8 for q9+ *)

(* num_last_distances_to_check by quality from brotli-c *)
let get_num_last_distances_to_check quality =
  if quality < 7 then 4
  else if quality < 9 then 10
  else 16

(* Bucket sweep (number of hash slots to check) for lower qualities *)
let get_bucket_sweep quality =
  if quality = 2 then 1         (* H2: sweep = 0, single slot *)
  else if quality = 3 then 2    (* H3: sweep = 1, 2 slots *)
  else if quality = 4 then 4    (* H4: sweep = 2, 4 slots *)
  else 1


(* Distance ring buffer for short distance codes *)
type dist_ring = {
  mutable distances : int array;  (* Last 4 distances *)
  mutable cache : int array;      (* 16-entry precomputed short code distances *)
  mutable idx : int;              (* Current index *)
}

let[@inline always] get_last_distance_raw distances idx n =
  (* n=0: last, n=1: second-to-last, etc. *)
  distances.((idx - 1 - n) land 3)

(* Update the 16-entry cache with precomputed short code distances.
   Called after each push_distance to keep cache in sync. *)
let update_cache ring =
  let d0 = get_last_distance_raw ring.distances ring.idx 0 in
  let d1 = get_last_distance_raw ring.distances ring.idx 1 in
  let d2 = get_last_distance_raw ring.distances ring.idx 2 in
  let d3 = get_last_distance_raw ring.distances ring.idx 3 in
  (* Codes 0-3: exact matches to last 4 distances *)
  ring.cache.(0) <- d0;
  ring.cache.(1) <- d1;
  ring.cache.(2) <- d2;
  ring.cache.(3) <- d3;
  (* Codes 4-9: last distance +/- 1,2,3 *)
  ring.cache.(4) <- d0 - 1;
  ring.cache.(5) <- d0 + 1;
  ring.cache.(6) <- d0 - 2;
  ring.cache.(7) <- d0 + 2;
  ring.cache.(8) <- d0 - 3;
  ring.cache.(9) <- d0 + 3;
  (* Codes 10-15: second-to-last distance +/- 1,2,3 *)
  ring.cache.(10) <- d1 - 1;
  ring.cache.(11) <- d1 + 1;
  ring.cache.(12) <- d1 - 2;
  ring.cache.(13) <- d1 + 2;
  ring.cache.(14) <- d1 - 3;
  ring.cache.(15) <- d1 + 3

let create_dist_ring () =
  let ring = {
    distances = [| 16; 15; 11; 4 |];  (* Initial values per RFC 7932 *)
    cache = Array.make 16 0;
    idx = 0;
  } in
  update_cache ring;
  ring

let push_distance ring dist =
  ring.distances.(ring.idx land 3) <- dist;
  ring.idx <- ring.idx + 1;
  update_cache ring

let get_last_distance ring n =
  (* n=0: last, n=1: second-to-last, etc. *)
  get_last_distance_raw ring.distances ring.idx n

(* Short distance codes (0-15) per RFC 7932:
   0: last distance
   1: second-to-last
   2: third-to-last
   3: fourth-to-last
   4: last - 1
   5: last + 1
   6: last - 2
   7: last + 2
   8: last - 3
   9: last + 3
   10: second-to-last - 1
   11: second-to-last + 1
   12: second-to-last - 2
   13: second-to-last + 2
   14: second-to-last - 3
   15: second-to-last + 3

   Returns the precomputed cache directly (do not mutate). *)
let[@inline always] short_code_distances ring = ring.cache

(* Find short distance code for a distance. Returns -1 if no short code applies.

   Short code mapping (RFC 7932):
   - Codes 0-3: exact match to last 4 distances
   - Codes 4-9: last distance +/- 1,2,3
   - Codes 10-15: second-to-last distance +/- 1,2,3

   Optimized version: check exact matches first (codes 0-3) since they're most common,
   then check +/- variants only if d0/d1 are close to distance. *)
let[@inline always] find_short_code ring distance =
  if distance <= 0 then -1
  else
    let cache = ring.cache in
    (* Check exact matches first (most common case) *)
    if cache.(0) = distance then 0
    else if cache.(1) = distance then 1
    else if cache.(2) = distance then 2
    else if cache.(3) = distance then 3
    else begin
      (* Check +/- 1,2,3 variants for d0 (codes 4-9) *)
      let d0 = cache.(0) in
      let diff0 = distance - d0 in
      if diff0 >= -3 && diff0 <= 3 && diff0 <> 0 then
        (* Map diff to code: -1->4, +1->5, -2->6, +2->7, -3->8, +3->9 *)
        let abs_diff = if diff0 < 0 then -diff0 else diff0 in
        let base_code = (abs_diff - 1) * 2 + 4 in
        if diff0 > 0 then base_code + 1 else base_code
      else begin
        (* Check +/- 1,2,3 variants for d1 (codes 10-15) *)
        let d1 = cache.(1) in
        let diff1 = distance - d1 in
        if diff1 >= -3 && diff1 <= 3 && diff1 <> 0 then
          let abs_diff = if diff1 < 0 then -diff1 else diff1 in
          let base_code = (abs_diff - 1) * 2 + 10 in
          if diff1 > 0 then base_code + 1 else base_code
        else
          -1
      end
    end

(* Command type with optional short distance code.
   dist_code: -1 means no short code, 0-15 are valid short codes *)
type command =
  | InsertCopy of {
      lit_start: int;
      lit_len: int;
      copy_len: int;
      distance: int;
      dist_code: int;  (* -1 = no short code, 0-15 = valid short code *)
    }
  | Literals of { start: int; len: int }

(* Hash function - produces 17-bit hash from 4 bytes *)
let[@inline always] hash4 src pos =
  Constants.hash4_bytes src pos hash_bits

(* ==========================================================================
   Match Length Optimization
   ==========================================================================
   This module provides optimized match length calculation using:

   1. SIMD (SSE2) - Compares 16 bytes at a time using:
      - pcmpeqb: Parallel comparison of 16 bytes
      - pmovmskb: Extract comparison results to a 16-bit mask
      - tzcnt/lookup: Find first differing byte position

   2. 64-bit word comparison - Fallback for remaining bytes:
      - Compares 8 bytes at a time using Bytes.get_int64_le
      - Uses XOR + count trailing zeros to find first difference

   3. Byte-by-byte - Final cleanup for last few bytes

   Expected speedup: 4-8x for match finding on large inputs compared to
   the naive byte-by-byte implementation.
   ========================================================================== *)

(* Count trailing zeros lookup table - used for SIMD movemask results *)
let trailing_zeros_table =
  (* Table: for each byte value (0-255), count trailing zeros *)
  let tbl = Array.make 256 8 in
  for i = 1 to 255 do
    let count = ref 0 in
    let v = ref i in
    while !v land 1 = 0 do
      incr count;
      v := !v lsr 1
    done;
    tbl.(i) <- !count
  done;
  tbl

let[@inline always] count_trailing_zeros_16 x =
  if x = 0 then 16
  else
    let low = x land 0xFF in
    if low <> 0 then trailing_zeros_table.(low)
    else 8 + trailing_zeros_table.((x lsr 8) land 0xFF)

(* Count trailing zeros in a 32-bit value - for AVX2 movemask *)
let[@inline always] count_trailing_zeros_32 x =
  if x = 0 then 32
  else
    let b0 = x land 0xFF in
    if b0 <> 0 then trailing_zeros_table.(b0)
    else
      let b1 = (x lsr 8) land 0xFF in
      if b1 <> 0 then 8 + trailing_zeros_table.(b1)
      else
        let b2 = (x lsr 16) land 0xFF in
        if b2 <> 0 then 16 + trailing_zeros_table.(b2)
        else 24 + trailing_zeros_table.((x lsr 24) land 0xFF)

(* De Bruijn table for 64-bit trailing zeros *)
let tzcnt64_debruijn_table = [|
  0;  1;  2; 53;  3;  7; 54; 27;
  4; 38; 41;  8; 34; 55; 48; 28;
  62;  5; 39; 46; 44; 42; 22;  9;
  24; 35; 59; 56; 49; 18; 29; 11;
  63; 52;  6; 26; 37; 40; 33; 47;
  61; 45; 43; 21; 23; 58; 17; 10;
  51; 25; 36; 32; 60; 20; 57; 16;
  50; 31; 19; 15; 30; 14; 13; 12
|]

(* Count trailing zeros in a 64-bit value using de Bruijn multiplication.
   This is much faster than the iterative approach. *)
let[@inline always] count_trailing_zeros64 (x : int64) =
  if Int64.equal x 0L then 64
  else begin
    (* Isolate lowest set bit: x & (-x) *)
    let isolated = Int64.logand x (Int64.neg x) in
    (* De Bruijn multiplication *)
    let idx = Int64.to_int (Int64.shift_right_logical
      (Int64.mul isolated 0x022fdd63cc95386dL) 58) in
    tzcnt64_debruijn_table.(idx land 63)
  end

(* AVX2/SSE2-accelerated match length finding.
   Compares 32 bytes at a time with AVX2, then 16 bytes with SSE2.
   Falls back to 64-bit comparison for remainder. *)
module Simd_match = struct
  module I8x16 = Ocaml_simd_sse.Int8x16
  module I8x32 = Ocaml_simd_avx.Int8x32

  (* Find first differing position using AVX2 comparison (32 bytes).
     Returns 32 if all bytes match, otherwise the index of first mismatch. *)
  let[@inline always] find_first_diff_32 src a b =
    let v1 = I8x32.Bytes.unsafe_get src ~byte:a in
    let v2 = I8x32.Bytes.unsafe_get src ~byte:b in
    let eq_mask = I8x32.equal v1 v2 in
    let mask_unboxed = I8x32.movemask eq_mask in
    let mask = Int64_u.to_int_trunc mask_unboxed in
    let inverted = (lnot mask) land 0xFFFFFFFF in
    if inverted = 0 then 32
    else count_trailing_zeros_32 inverted

  (* Find first differing position using SSE2 comparison (16 bytes).
     Returns 16 if all bytes match, otherwise the index of first mismatch. *)
  let[@inline always] find_first_diff_16 src a b =
    let v1 = I8x16.Bytes.unsafe_get src ~byte:a in
    let v2 = I8x16.Bytes.unsafe_get src ~byte:b in
    let eq_mask = I8x16.equal v1 v2 in
    let mask_unboxed = I8x16.movemask eq_mask in
    let mask = Int64_u.to_int_trunc mask_unboxed in
    let inverted = (lnot mask) land 0xFFFF in
    if inverted = 0 then 16
    else count_trailing_zeros_16 inverted

  (* Main AVX2/SSE2-accelerated match length finder *)
  let[@inline always] find_match_length src a b limit =
    let max_len = min max_match (limit - b) in
    if max_len < 4 then 0  (* Minimum match length check *)
    else begin
      let len = ref 0 in
      let continue = ref true in
      (* Process 32 bytes at a time with AVX2 *)
      while !continue && !len + 32 <= max_len do
        let diff_pos = find_first_diff_32 src (a + !len) (b + !len) in
        if diff_pos < 32 then begin
          len := !len + diff_pos;
          if !len > max_len then len := max_len;
          continue := false
        end else
          len := !len + 32
      done;
      (* Handle 16-31 remaining bytes with SSE2 *)
      if !continue && !len + 16 <= max_len then begin
        let diff_pos = find_first_diff_16 src (a + !len) (b + !len) in
        if diff_pos < 16 then begin
          len := !len + diff_pos;
          if !len > max_len then len := max_len;
          continue := false
        end else
          len := !len + 16
      end;
      if !continue then begin
        (* Handle remainder with 64-bit comparisons *)
        while !continue && !len + 8 <= max_len do
          let w1 = Bytes.get_int64_le src (a + !len) in
          let w2 = Bytes.get_int64_le src (b + !len) in
          if Int64.equal w1 w2 then
            len := !len + 8
          else begin
            let diff = Int64.logxor w1 w2 in
            let diff_byte_pos = count_trailing_zeros64 diff / 8 in
            len := !len + diff_byte_pos;
            if !len > max_len then len := max_len;
            continue := false
          end
        done
      end;
      (* Final byte-by-byte for last few bytes *)
      while !len < max_len && Bytes.get src (a + !len) = Bytes.get src (b + !len) do
        incr len
      done;
      !len
    end
end

(* 64-bit word comparison fallback (portable, no SIMD required) *)
module Word64_match = struct
  (* Find match length using 64-bit word comparisons *)
  let[@inline always] find_match_length src a b limit =
    let max_len = min max_match (limit - b) in
    if max_len < 4 then 0
    else begin
      let len = ref 0 in
      let continue = ref true in
      (* Process 8 bytes at a time *)
      while !continue && !len + 8 <= max_len do
        let w1 = Bytes.get_int64_le src (a + !len) in
        let w2 = Bytes.get_int64_le src (b + !len) in
        if Int64.equal w1 w2 then
          len := !len + 8
        else begin
          (* Find first differing byte position *)
          let diff = Int64.logxor w1 w2 in
          let diff_byte_pos = count_trailing_zeros64 diff / 8 in
          len := !len + diff_byte_pos;
          if !len > max_len then len := max_len;
          continue := false
        end
      done;
      (* Handle remaining bytes *)
      while !len < max_len && Bytes.get src (a + !len) = Bytes.get src (b + !len) do
        incr len
      done;
      !len
    end
end

(* Use SIMD version by default - it provides the best performance *)
let[@inline always] find_match_length src a b limit =
  Simd_match.find_match_length src a b limit

(* Log2 floor for non-zero values - matches brotli-c Log2FloorNonZero
   Uses de Bruijn sequence for O(1) lookup instead of O(log n) loop.
   This is one of the most called functions in LZ77, so every cycle matters. *)

(* De Bruijn sequence table for 64-bit log2 *)
let log2_debruijn_table = [|
  63;  0; 58;  1; 59; 47; 53;  2;
  60; 39; 48; 27; 54; 33; 42;  3;
  61; 51; 37; 40; 49; 18; 28; 20;
  55; 30; 34; 11; 43; 14; 22;  4;
  62; 57; 46; 52; 38; 26; 32; 41;
  50; 36; 17; 19; 29; 10; 13; 21;
  56; 45; 25; 31; 35; 16;  9; 12;
  44; 24; 15;  8; 23;  7;  6;  5
|]

(* Pre-computed table for small values (0-255) - covers most common cases *)
let log2_small_table =
  Array.init 256 (fun i ->
    if i = 0 then 0
    else
      let rec go v acc = if v <= 1 then acc else go (v lsr 1) (acc + 1) in
      go i 0)

let[@inline always] log2_floor_nonzero v =
  if v < 256 then log2_small_table.(v)
  else begin
    (* Use bit manipulation: isolate highest bit then use de Bruijn multiplication *)
    let v = v lor (v lsr 1) in
    let v = v lor (v lsr 2) in
    let v = v lor (v lsr 4) in
    let v = v lor (v lsr 8) in
    let v = v lor (v lsr 16) in
    let v = v lor (v lsr 32) in
    (* v is now all 1s from the highest bit down *)
    let v = v - (v lsr 1) in  (* isolate highest bit *)
    (* De Bruijn multiplication *)
    let idx = (v * 0x07EDD5E59A4E28C2) lsr 58 in
    log2_debruijn_table.(idx land 63)
  end

(* Pre-computed distance penalties for common distances.
   This avoids log2 computation for the most frequent cases.
   penalty = DISTANCE_BIT_PENALTY * log2_floor(distance) *)
let distance_penalty_table =
  Array.init 4097 (fun d ->
    if d = 0 then 0  (* invalid but avoid crash *)
    else brotli_distance_bit_penalty * log2_floor_nonzero d)

(* BackwardReferenceScore from brotli-c (hash.h line 115-118):
   score = SCORE_BASE + LITERAL_BYTE_SCORE * copy_length
           - DISTANCE_BIT_PENALTY * Log2FloorNonZero(backward_reference_offset)
   This prefers longer matches and shorter distances. *)
let[@inline always] backward_reference_score copy_len backward_distance =
  let penalty =
    if backward_distance <= 4096 then
      distance_penalty_table.(backward_distance)
    else
      brotli_distance_bit_penalty * log2_floor_nonzero backward_distance
  in
  brotli_score_base + brotli_literal_byte_score * copy_len - penalty

(* BackwardReferenceScoreUsingLastDistance from brotli-c (hash.h line 121-124):
   score = LITERAL_BYTE_SCORE * copy_length + SCORE_BASE + 15
   Short code 0 (last distance) gets a bonus. *)
let[@inline always] backward_reference_score_using_last_distance copy_len =
  brotli_literal_byte_score * copy_len + brotli_score_base + 15

(* BackwardReferencePenaltyUsingLastDistance from brotli-c (hash.h line 127-129):
   Penalty for short codes 1-15 (not 0): 39 + lookup(distance_short_code)
   The magic constant 0x1CA10 encodes penalties: codes 1-3 get 0, 4-5 get 2, etc. *)
let[@inline always] backward_reference_penalty_using_last_distance distance_short_code =
  39 + ((0x1CA10 lsr (distance_short_code land 0xE)) land 0xE)

(* Pre-computed penalties for short codes 0-15 to avoid repeated calculation *)
let short_code_penalty_table =
  Array.init 16 (fun code ->
    if code = 0 then 0  (* no penalty for code 0 *)
    else backward_reference_penalty_using_last_distance code)

(* Score function matching brotli-c exactly.
   Optimized with pre-computed penalty table for short codes. *)
let[@inline always] score_match copy_len distance dist_code =
  (* Pre-compute the literal contribution once *)
  let lit_score = brotli_literal_byte_score * copy_len in
  if dist_code >= 0 && dist_code < 16 then
    (* Short code (0-15): use last distance scoring with bonus, minus penalty *)
    (* penalty is 0 for code 0, pre-computed for codes 1-15 *)
    lit_score + brotli_score_base + 15 - short_code_penalty_table.(dist_code)
  else
    (* Explicit distance (dist_code = -1) - standard scoring *)
    backward_reference_score copy_len distance

(* Insert length code tables *)
let insert_length_offset = [|
  0; 1; 2; 3; 4; 5; 6; 8; 10; 14; 18; 26; 34; 50; 66; 98; 130; 194; 322; 578; 1090; 2114; 6210; 22594
|]

(* Get insert length code using binary search for O(log n) instead of O(n) *)
let[@inline always] get_insert_code length =
  (* Binary search for the largest offset <= length *)
  if length < 6 then length  (* Fast path for small values: codes 0-5 *)
  else if length < 130 then begin
    (* Codes 6-16, use small binary search *)
    if length < 18 then
      if length < 10 then
        if length < 8 then 6 else 7
      else
        if length < 14 then 8 else 9
    else
      if length < 50 then
        if length < 26 then 10
        else if length < 34 then 11
        else 12
      else
        if length < 66 then 13
        else if length < 98 then 14
        else 15
  end
  else begin
    (* Codes 16-23, larger values *)
    if length < 1090 then
      if length < 322 then
        if length < 194 then 16 else 17
      else
        if length < 578 then 18 else 19
    else
      if length < 6210 then
        if length < 2114 then 20 else 21
      else
        if length < 22594 then 22 else 23
  end

(* Get max copy_len that fits with a given insert_len.
   Uses pre-computed threshold: insert_code >= 16 means copy_len <= 9 *)
let[@inline always] max_copy_len_for_insert insert_len =
  (* insert_code >= 16 when insert_len >= 130 *)
  if insert_len >= 130 then 9 else max_match

(* Try to find a match at a short code distance.
   num_to_check controls how many short codes to check (4, 10, or 16 based on quality)
   Returns (len, dist, code) or (0, 0, -1) if no match found.

   Optimized: checks codes in priority order (0-3 first, then 4-15).
   Uses early exit when a good enough match is found. *)
let try_short_code_match ?(num_to_check=16) src pos limit ring =
  let candidates = short_code_distances ring in
  let mutable best_len = 0 in
  let mutable best_dist = 0 in
  let mutable best_code = -1 in
  let mutable best_score = 0 in
  (* Unroll the loop for codes 0-3 which are most likely to hit *)
  for code = 0 to min 3 (num_to_check - 1) do
    let dist = candidates.(code) in
    if dist > 0 && pos - dist >= 0 then begin
      let prev = pos - dist in
      let match_len = find_match_length src prev pos limit in
      if match_len >= min_match then begin
        let score = score_match match_len dist code in
        if score > best_score then begin
          best_len <- match_len;
          best_dist <- dist;
          best_code <- code;
          best_score <- score
        end
      end
    end
  done;
  (* Check remaining codes 4-15 if needed *)
  if num_to_check > 4 then begin
    for code = 4 to num_to_check - 1 do
      let dist = candidates.(code) in
      if dist > 0 && pos - dist >= 0 then begin
        let prev = pos - dist in
        let match_len = find_match_length src prev pos limit in
        if match_len >= min_match then begin
          let score = score_match match_len dist code in
          if score > best_score then begin
            best_len <- match_len;
            best_dist <- dist;
            best_code <- code;
            best_score <- score
          end
        end
      end
    done
  end;
  (best_len, best_dist, best_code)

(* Score a dictionary match *)
let score_dict_match copy_len =
  (* Dictionary matches save literals and have no backward reference overhead *)
  copy_len * 140  (* Slightly higher than LZ77's base score of 135 *)

(* Get max chain depth based on quality.
   For Q2-4: uses bucket sweep (limited positions per bucket slot)
   For Q5-9: uses block_size (ring buffer per bucket)
   For Q10-11: uses binary tree with max_tree_search_depth=64 *)
let get_max_chain_depth quality =
  if quality <= 4 then get_bucket_sweep quality
  else get_block_size quality

(* Literal spree skip optimization from brotli-c quality.h:
   When searching for backward references and have not seen matches for a long
   time, we can skip some match lookups. Unsuccessful match lookups are very
   expensive and this kind of heuristic speeds up compression quite a lot.
   At first 8 byte strides are taken and every second byte is put to hasher.
   After 4x more literals stride by 16 bytes, put every 4th byte to hasher.
   Applied only to qualities 2 to 9. *)
let get_literal_spree_length quality =
  if quality < 9 then 64 else 512

(* Find best match using hash chain for higher quality levels.
   Matches brotli-c FindLongestMatch: first checks distance cache (short codes),
   then searches hash chain/bucket.
   chain_table_base is the base offset used for chain_table indexing.
   Uses unboxed nativeint# arrays for hash_table and chain_table for better performance.
   Returns (len, dist, code) or (0, 0, -1) if no match found.

   Optimized version using mutable locals instead of refs for better codegen. *)
let find_best_chain_match src pos src_end (hash_table : nativeint# array) (chain_table : nativeint# array) chain_table_base ring
    ~num_last_distances_to_check ~max_chain_depth =
  if pos + min_match > src_end then (0, 0, -1)
  else begin
    (* Use mutable locals for better codegen (avoids ref allocation) *)
    let mutable best_len = min_match - 1 in  (* Start at min_match-1 so >= min_match wins *)
    let mutable best_dist = 0 in
    let mutable best_score = 0 in
    let mutable best_code = -1 in

    (* First: try short code distances (distance cache) - like brotli-c *)
    let short_dists = short_code_distances ring in
    for code = 0 to num_last_distances_to_check - 1 do
      let dist = short_dists.(code) in
      if dist > 0 && dist <= max_backward_distance then begin
        let prev = pos - dist in
        if prev >= 0 then begin
          let match_len = find_match_length src prev pos src_end in
          (* brotli-c accepts len >= 3 for codes 0-1, >= 4 otherwise *)
          let min_len = if code < 2 then 3 else min_match in
          if match_len >= min_len then begin
            let score = score_match match_len dist code in
            if score > best_score then begin
              best_len <- match_len;
              best_dist <- dist;
              best_score <- score;
              best_code <- code
            end
          end
        end
      end
    done;

    (* Second: search hash chain for more matches *)
    let h = hash4 src pos in
    let chain_table_len = Nativeint_u.Array.length chain_table in
    (* Use mutable for unboxed chain position to avoid ref boxing *)
    let mutable chain_pos_u : nativeint# = na_get hash_table h in
    let mutable chain_count = 0 in

    while Nativeint_u.(chain_pos_u >= #0n) && chain_count < max_chain_depth do
      let chain_pos = Nativeint_u.to_int_trunc chain_pos_u in
      let distance = pos - chain_pos in
      if distance > 0 && distance <= max_backward_distance then begin
        let match_len = find_match_length src chain_pos pos src_end in
        if match_len >= min_match then begin
          let dist_code = find_short_code ring distance in
          let score = score_match match_len distance dist_code in
          if score > best_score then begin
            best_len <- match_len;
            best_dist <- distance;
            best_score <- score;
            best_code <- dist_code
          end
        end
      end;
      (* Follow the chain - index relative to base *)
      let chain_idx = chain_pos - chain_table_base in
      if chain_idx >= 0 && chain_idx < chain_table_len then
        chain_pos_u <- na_get chain_table chain_idx
      else
        chain_pos_u <- invalid_pos;
      chain_count <- chain_count + 1
    done;

    if best_len >= min_match then
      (best_len, best_dist, best_code)
    else
      (0, 0, -1)
  end

(* Update hash chain. chain_table_base is the base offset for indexing.
   Uses unboxed nativeint# arrays for hash_table and chain_table. *)
let update_hash_chain src pos (hash_table : nativeint# array) (chain_table : nativeint# array) chain_table_base =
  let chain_idx = pos - chain_table_base in
  if chain_idx >= 0 && chain_idx < Nativeint_u.Array.length chain_table && pos + min_match <= Bytes.length src then begin
    let h = hash4 src pos in
    na_set chain_table chain_idx (na_get hash_table h);
    na_set hash_table h (Nativeint_u.of_int pos)
  end

(* Generate commands with LZ77 matching, dictionary matching, and distance ring buffer.
   Parameters match brotli-c quality-dependent configuration. *)
let generate_commands ?(use_dict=false) ?(quality=2) src src_pos src_len =
  if src_len < min_match then
    [Literals { start = src_pos; len = src_len }]
  else begin
    let commands = ref [] in
    (* Hash table uses unboxed nativeint# for better performance in hot path *)
    let hash_table : nativeint# array = make_nativeint_u_array hash_size invalid_pos in
    (* Chain table for quality 4+ - each position stores prev position with same hash.
       The table is indexed relative to src_pos. Uses unboxed nativeint# for performance. *)
    let chain_table : nativeint# array =
      if quality >= 4 then make_nativeint_u_array src_len invalid_pos
      else [||]  (* Not used for lower qualities *)
    in
    let chain_table_base = src_pos in  (* Base offset for chain_table indexing *)
    let ring = create_dist_ring () in
    let mutable pos = src_pos in
    let src_end = src_pos + src_len in
    let mutable pending_start = src_pos in
    let mutable output_pos = 0 in
    let max_chain_depth = get_max_chain_depth quality in
    let num_last_distances_to_check = get_num_last_distances_to_check quality in

    (* Cost for lazy matching decision - brotli-c uses heuristic thresholds *)
    let lazy_match_cost = if quality >= 4 then 175 else 0 in

    (* Literal spree skip optimization - track consecutive literals without matches *)
    let mutable literal_spree = 0 in
    let spree_length = get_literal_spree_length quality in
    let use_spree_skip = quality >= 2 && quality <= 9 in

    while pos < src_end - min_match do
      (* Determine if we should skip this position due to literal spree *)
      let skip_this_position =
        if use_spree_skip && literal_spree >= spree_length then begin
          (* In sparse search mode - skip based on spree level *)
          let stride = if literal_spree >= spree_length * 4 then 16 else 8 in
          let relative_pos = pos - pending_start in
          relative_pos mod stride <> 0
        end else false
      in

      if skip_this_position then begin
        (* Still update hash table but with reduced frequency *)
        let hash_update_stride = if literal_spree >= spree_length * 4 then 4 else 2 in
        let relative_pos = pos - pending_start in
        if relative_pos mod hash_update_stride = 0 then begin
          if quality >= 4 then
            update_hash_chain src pos hash_table chain_table chain_table_base
          else
            na_set hash_table (hash4 src pos) (Nativeint_u.of_int pos)
        end;
        pos <- pos + 1;
        literal_spree <- literal_spree + 1
      end else begin
      (* Find best match at current position - returns (len, dist, code) tuple.
         len=0 means no match found, dist_code=-1 means no short code. *)
      let (hash_len, hash_dist, hash_code) =
        if quality >= 4 then
          find_best_chain_match src pos src_end hash_table chain_table chain_table_base ring
            ~num_last_distances_to_check ~max_chain_depth
        else begin
          (* Q2-3: Simple hash lookup with bucket sweep *)
          let h = hash4 src pos in
          let prev_u : nativeint# = na_get hash_table h in
          let prev = Nativeint_u.to_int_trunc prev_u in
          na_set hash_table h (Nativeint_u.of_int pos);
          (* Also check distance cache first *)
          let (slen, sdist, scode) = try_short_code_match ~num_to_check:num_last_distances_to_check
            src pos src_end ring in
          if prev >= src_pos && pos - prev <= max_backward_distance then begin
            let match_len = find_match_length src prev pos src_end in
            if match_len >= min_match then begin
              let distance = pos - prev in
              let dist_code = find_short_code ring distance in
              (* Pick best between short code match and hash match *)
              if slen >= min_match then begin
                let s_score = score_match slen sdist scode in
                let h_score = score_match match_len distance dist_code in
                if s_score >= h_score then (slen, sdist, scode)
                else (match_len, distance, dist_code)
              end else
                (match_len, distance, dist_code)
            end else if slen >= min_match then
              (slen, sdist, scode)
            else
              (0, 0, -1)
          end
          else if slen >= min_match then
            (slen, sdist, scode)
          else
            (0, 0, -1)
        end
      in

      (* Update hash chain for quality 4+ *)
      if quality >= 4 then
        update_hash_chain src pos hash_table chain_table chain_table_base;

      (* Try dictionary match if enabled *)
      let (dict_len, dict_dist) =
        if use_dict then begin
          let pending_lits = pos - pending_start in
          let current_output_pos = output_pos + pending_lits in
          match Dict_match.find_match src pos src_end max_backward_distance ~current_output_pos with
          | Some (len, dist) -> (len, dist)
          | None -> (0, 0)
        end
        else
          (0, 0)
      in

      (* Choose the best match based on score *)
      let (match_len, distance, dist_code) =
        if hash_len >= min_match && dict_len > 0 then begin
          let lz_score = score_match hash_len hash_dist hash_code in
          let dict_score = score_dict_match dict_len in
          if dict_score > lz_score then
            (dict_len, dict_dist, -1)
          else
            (hash_len, hash_dist, hash_code)
        end
        else if hash_len >= min_match then
          (hash_len, hash_dist, hash_code)
        else if dict_len > 0 then
          (dict_len, dict_dist, -1)
        else
          (0, 0, -1)
      in

      if match_len >= min_match then begin
        (* Lazy matching for quality 4+: check if delaying gives better match *)
        let use_match =
          if quality >= 4 && pos + 1 < src_end - min_match && match_len < max_match then begin
            (* Update hash for next position *)
            update_hash_chain src (pos + 1) hash_table chain_table chain_table_base;
            let (next_len, next_dist, next_code) = find_best_chain_match src (pos + 1) src_end
              hash_table chain_table chain_table_base ring
              ~num_last_distances_to_check ~max_chain_depth in
            if next_len >= min_match then begin
              let curr_score = score_match match_len distance dist_code in
              let next_score = score_match next_len next_dist next_code - lazy_match_cost in
              if next_score > curr_score then begin
                (* Skip current position, emit literal *)
                pos <- pos + 1;
                false  (* Don't use current match *)
              end else
                true
            end else
              true
          end else
            true
        in

        if use_match then begin
          let lit_len = pos - pending_start in
          let max_copy = max_copy_len_for_insert lit_len in
          let copy_len = min match_len max_copy in

          commands := InsertCopy {
            lit_start = pending_start;
            lit_len;
            copy_len;
            distance;
            dist_code;
          } :: !commands;

          output_pos <- output_pos + lit_len + copy_len;

          (* Don't update ring for dist_code=0 (reusing last distance) *)
          if dist_code <> 0 then push_distance ring distance;

          (* Update hash for all positions in the match for better chain coverage *)
          let hash_update_count =
            if quality >= 10 then min (copy_len - 1) 16
            else if quality >= 4 then min (copy_len - 1) 8
            else min (copy_len - 1) 2 in
          for i = 1 to hash_update_count do
            if pos + i < src_end - min_match then begin
              if quality >= 4 then
                update_hash_chain src (pos + i) hash_table chain_table chain_table_base
              else
                na_set hash_table (hash4 src (pos + i)) (Nativeint_u.of_int (pos + i))
            end
          done;

          pos <- pos + copy_len;
          pending_start <- pos;
          (* Reset literal spree counter on match *)
          literal_spree <- 0
        end else
          (* Lazy match chose to skip, position already incremented *)
          literal_spree <- literal_spree + 1
      end else begin
        pos <- pos + 1;
        literal_spree <- literal_spree + 1
      end
      end (* end of else begin for skip_this_position *)
    done;

    if pending_start < src_end then
      commands := Literals { start = pending_start; len = src_end - pending_start } :: !commands;

    List.rev !commands
  end
