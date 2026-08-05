(* LZ77 matching with distance ring buffer support for Brotli *)
(* Implements scoring and parameters matching brotli-c reference *)

(* Configuration *)
let hash_bits = 17
let hash_size = 1 lsl hash_bits
let min_match = 4
let max_match = 258
let window_bits = 22
let max_backward_distance = (1 lsl window_bits) - 16

(* Module alias for int16 unboxed operations *)
module I16 = Stdlib_stable.Int16_u

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


(* Module alias for unboxed nativeint operations *)
module Nu = Stdlib_upstream_compatible.Nativeint_u

(* Distance ring buffer for short distance codes - uses unboxed nativeint# arrays *)
type dist_ring = {
  mutable distances : nativeint# array;  (* Last 4 distances *)
  mutable cache : nativeint# array;      (* 16-entry precomputed short code distances *)
  mutable idx : int;                     (* Current index *)
}

let[@inline always] get_last_distance_raw (distances : nativeint# array) idx n =
  (* n=0: last, n=1: second-to-last, etc. *)
  na_get distances ((idx - 1 - n) land 3)

(* Update the 16-entry cache with precomputed short code distances.
   Called after each push_distance to keep cache in sync. *)
let update_cache ring =
  let d0 = get_last_distance_raw ring.distances ring.idx 0 in
  let d1 = get_last_distance_raw ring.distances ring.idx 1 in
  let d2 = get_last_distance_raw ring.distances ring.idx 2 in
  let d3 = get_last_distance_raw ring.distances ring.idx 3 in
  (* Codes 0-3: exact matches to last 4 distances *)
  na_set ring.cache 0 d0;
  na_set ring.cache 1 d1;
  na_set ring.cache 2 d2;
  na_set ring.cache 3 d3;
  (* Codes 4-9: last distance +/- 1,2,3 *)
  na_set ring.cache 4 (Nu.sub d0 #1n);
  na_set ring.cache 5 (Nu.add d0 #1n);
  na_set ring.cache 6 (Nu.sub d0 #2n);
  na_set ring.cache 7 (Nu.add d0 #2n);
  na_set ring.cache 8 (Nu.sub d0 #3n);
  na_set ring.cache 9 (Nu.add d0 #3n);
  (* Codes 10-15: second-to-last distance +/- 1,2,3 *)
  na_set ring.cache 10 (Nu.sub d1 #1n);
  na_set ring.cache 11 (Nu.add d1 #1n);
  na_set ring.cache 12 (Nu.sub d1 #2n);
  na_set ring.cache 13 (Nu.add d1 #2n);
  na_set ring.cache 14 (Nu.sub d1 #3n);
  na_set ring.cache 15 (Nu.add d1 #3n)

let create_dist_ring () =
  let distances = make_nativeint_u_array 4 #0n in
  na_set distances 0 (Nu.of_int 16);
  na_set distances 1 (Nu.of_int 15);
  na_set distances 2 (Nu.of_int 11);
  na_set distances 3 (Nu.of_int 4);
  let ring = {
    distances;
    cache = make_nativeint_u_array 16 #0n;
    idx = 0;
  } in
  update_cache ring;
  ring

let push_distance ring dist =
  na_set ring.distances (ring.idx land 3) (Nu.of_int dist);
  ring.idx <- ring.idx + 1;
  update_cache ring

let get_last_distance ring n =
  (* n=0: last, n=1: second-to-last, etc. *)
  Nu.to_int (get_last_distance_raw ring.distances ring.idx n)

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
    let dist_n = Nu.of_int distance in
    (* Check exact matches first (most common case) *)
    if Nativeint_u.(na_get cache 0 = dist_n) then 0
    else if Nativeint_u.(na_get cache 1 = dist_n) then 1
    else if Nativeint_u.(na_get cache 2 = dist_n) then 2
    else if Nativeint_u.(na_get cache 3 = dist_n) then 3
    else begin
      (* Check +/- 1,2,3 variants for d0 (codes 4-9) *)
      let d0 = Nu.to_int (na_get cache 0) in
      let diff0 = distance - d0 in
      if diff0 >= -3 && diff0 <= 3 && diff0 <> 0 then
        (* Map diff to code: -1->4, +1->5, -2->6, +2->7, -3->8, +3->9 *)
        let abs_diff = if diff0 < 0 then -diff0 else diff0 in
        let base_code = (abs_diff - 1) * 2 + 4 in
        if diff0 > 0 then base_code + 1 else base_code
      else begin
        (* Check +/- 1,2,3 variants for d1 (codes 10-15) *)
        let d1 = Nu.to_int (na_get cache 1) in
        let diff1 = distance - d1 in
        if diff1 >= -3 && diff1 <= 3 && diff1 <> 0 then
          let abs_diff = if diff1 < 0 then -diff1 else diff1 in
          let base_code = (abs_diff - 1) * 2 + 10 in
          if diff1 > 0 then base_code + 1 else base_code
        else
          -1
      end
    end

(* Match result as unboxed labeled tuple for zero-allocation returns. *)
type match_result = #(mr_len: int * mr_dist: int * mr_code: int)

(* No match sentinel value *)
let[@inline always] no_match () : match_result = #(~mr_len:0, ~mr_dist:0, ~mr_code:(-1))

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

let[@inline always] find_match_length src a b limit =
  Match_impl.find_match_length src a b limit

(* Log2 floor for non-zero values - matches brotli-c Log2FloorNonZero
   Uses de Bruijn sequence for O(1) lookup instead of O(log n) loop.
   This is one of the most called functions in LZ77, so every cycle matters. *)

(* De Bruijn sequence table for 64-bit log2 - values 0-63 fit in int8# *)
let log2_debruijn_table : int8# array = [|
  #63s;  #0s; #58s;  #1s; #59s; #47s; #53s;  #2s;
  #60s; #39s; #48s; #27s; #54s; #33s; #42s;  #3s;
  #61s; #51s; #37s; #40s; #49s; #18s; #28s; #20s;
  #55s; #30s; #34s; #11s; #43s; #14s; #22s;  #4s;
  #62s; #57s; #46s; #52s; #38s; #26s; #32s; #41s;
  #50s; #36s; #17s; #19s; #29s; #10s; #13s; #21s;
  #56s; #45s; #25s; #31s; #35s; #16s;  #9s; #12s;
  #44s; #24s; #15s;  #8s; #23s;  #7s;  #6s;  #5s
|]

(* Pre-computed table for small values (0-255) - covers most common cases.
   Values 0-8 fit in int8#. *)
let log2_small_table : int8# array = [|
  #0s; #0s; #1s; #1s; #2s; #2s; #2s; #2s; #3s; #3s; #3s; #3s; #3s; #3s; #3s; #3s;
  #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s; #4s;
  #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s;
  #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s; #5s;
  #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s;
  #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s;
  #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s;
  #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s; #6s;
  #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s;
  #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s;
  #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s;
  #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s;
  #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s;
  #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s;
  #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s;
  #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s; #7s
|]

let[@inline always] log2_floor_nonzero v =
  if v < 256 then Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get log2_small_table v)
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
    Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get log2_debruijn_table (idx land 63))
  end

(* Pre-computed distance penalties for common distances.
   This avoids log2 computation for the most frequent cases.
   penalty = DISTANCE_BIT_PENALTY * log2_floor(distance)
   Max value is 30 * 12 = 360, fits in int16#. *)
let distance_penalty_table : int16# array =
  let arr = Base.Array.create ~len:4097 #0S in
  for d = 1 to 4096 do
    Oxcaml_arrays.unsafe_set arr d (I16.of_int (brotli_distance_bit_penalty * log2_floor_nonzero d))
  done;
  arr

(* BackwardReferenceScore from brotli-c (hash.h line 115-118):
   score = SCORE_BASE + LITERAL_BYTE_SCORE * copy_length
           - DISTANCE_BIT_PENALTY * Log2FloorNonZero(backward_reference_offset)
   This prefers longer matches and shorter distances. *)
let[@inline always] backward_reference_score copy_len backward_distance =
  let penalty =
    if backward_distance <= 4096 then
      I16.to_int (Oxcaml_arrays.unsafe_get distance_penalty_table backward_distance)
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

(* Pre-computed penalties for short codes 0-15 to avoid repeated calculation.
   Values are: 0, 39, 39, 39, 41, 41, 43, 43, 45, 45, 41, 41, 43, 43, 45, 45
   All fit in int8#. *)
let short_code_penalty_table : int8# array = [|
  #0s; #39s; #39s; #39s; #41s; #41s; #43s; #43s;
  #45s; #45s; #41s; #41s; #43s; #43s; #45s; #45s
|]

(* Score function matching brotli-c exactly.
   Optimized with pre-computed penalty table for short codes (int8# array). *)
let[@inline always] score_match copy_len distance dist_code =
  (* Pre-compute the literal contribution once *)
  let lit_score = brotli_literal_byte_score * copy_len in
  if dist_code >= 0 && dist_code < 16 then
    (* Short code (0-15): use last distance scoring with bonus, minus penalty *)
    (* penalty is 0 for code 0, pre-computed for codes 1-15 *)
    let penalty = Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get short_code_penalty_table dist_code) in
    lit_score + brotli_score_base + 15 - penalty
  else
    (* Explicit distance (dist_code = -1) - standard scoring *)
    backward_reference_score copy_len distance

(* Insert length code tables - values up to 22594 need nativeint# *)
let insert_length_offset : nativeint# array = [|
  #0n; #1n; #2n; #3n; #4n; #5n; #6n; #8n; #10n; #14n; #18n; #26n; #34n; #50n; #66n; #98n; #130n; #194n; #322n; #578n; #1090n; #2114n; #6210n; #22594n
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
   Returns local unboxed labeled tuple (avoids heap allocation).

   Optimized: checks codes in priority order (0-3 first, then 4-15).
   Uses early exit when a good enough match is found. *)
let[@inline always] try_short_code_match ?(num_to_check=16) src pos limit ring =
  let candidates = short_code_distances ring in
  let mutable best_len = 0 in
  let mutable best_dist_n : nativeint# = #0n in
  let mutable best_code = -1 in
  let mutable best_score = 0 in
  let pos_n = Nu.of_int pos in
  (* Unroll the loop for codes 0-3 which are most likely to hit *)
  for code = 0 to min 3 (num_to_check - 1) do
    let dist_n = na_get candidates code in
    if Nativeint_u.(dist_n > #0n) && Nativeint_u.(pos_n >= dist_n) then begin
      let prev = Nu.to_int (Nu.sub pos_n dist_n) in
      let dist = Nu.to_int dist_n in
      let match_len = find_match_length src prev pos limit in
      if match_len >= min_match then begin
        let score = score_match match_len dist code in
        if score > best_score then begin
          best_len <- match_len;
          best_dist_n <- dist_n;
          best_code <- code;
          best_score <- score
        end
      end
    end
  done;
  (* Check remaining codes 4-15 if needed *)
  if num_to_check > 4 then begin
    for code = 4 to num_to_check - 1 do
      let dist_n = na_get candidates code in
      if Nativeint_u.(dist_n > #0n) && Nativeint_u.(pos_n >= dist_n) then begin
        let prev = Nu.to_int (Nu.sub pos_n dist_n) in
        let dist = Nu.to_int dist_n in
        let match_len = find_match_length src prev pos limit in
        if match_len >= min_match then begin
          let score = score_match match_len dist code in
          if score > best_score then begin
            best_len <- match_len;
            best_dist_n <- dist_n;
            best_code <- code;
            best_score <- score
          end
        end
      end
    done
  end;
  #(~mr_len:best_len, ~mr_dist:(Nu.to_int best_dist_n), ~mr_code:best_code)

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
   Returns local unboxed labeled tuple (avoids heap allocation).

   Optimized version using mutable locals instead of refs for better codegen. *)
let[@inline always] find_best_chain_match src pos src_end (hash_table : nativeint# array) (chain_table : nativeint# array) chain_table_base ring
    ~num_last_distances_to_check ~max_chain_depth =
  if pos + min_match > src_end then
    no_match ()
  else begin
    (* Use mutable locals for better codegen (avoids ref allocation) *)
    let mutable best_len = min_match - 1 in  (* Start at min_match-1 so >= min_match wins *)
    let mutable best_dist = 0 in
    let mutable best_score = 0 in
    let mutable best_code = -1 in

    (* First: try short code distances (distance cache) - like brotli-c *)
    let short_dists = short_code_distances ring in
    let pos_n = Nu.of_int pos in
    let max_backward_n = Nu.of_int max_backward_distance in
    for code = 0 to num_last_distances_to_check - 1 do
      let dist_n = na_get short_dists code in
      if Nativeint_u.(dist_n > #0n) && Nativeint_u.(dist_n <= max_backward_n) then begin
        let prev = Nu.to_int (Nu.sub pos_n dist_n) in
        if prev >= 0 then begin
          let dist = Nu.to_int dist_n in
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
      #(~mr_len:best_len, ~mr_dist:best_dist, ~mr_code:best_code)
    else
      no_match ()
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
   Parameters match brotli-c quality-dependent configuration.

   OPTIMIZATION: Uses pre-allocated match_result records to avoid tuple allocation
   in the hot loop. All match finding functions write to these buffers instead of
   returning tuples. *)
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

    (* Mutable locals to hold current match result - avoids tuple allocation *)
    let mutable hash_len = 0 in
    let mutable hash_dist = 0 in
    let mutable hash_code = -1 in

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
      (* Find best match at current position using local unboxed tuples.
         mr_len=0 means no match found, mr_code=-1 means no short code. *)
      if quality >= 4 then begin
        let #(~mr_len, ~mr_dist, ~mr_code) =
          find_best_chain_match src pos src_end hash_table chain_table chain_table_base ring
            ~num_last_distances_to_check ~max_chain_depth
        in
        hash_len <- mr_len;
        hash_dist <- mr_dist;
        hash_code <- mr_code
      end else begin
        (* Q2-3: Simple hash lookup with bucket sweep *)
        let h = hash4 src pos in
        let prev_u : nativeint# = na_get hash_table h in
        let prev = Nativeint_u.to_int_trunc prev_u in
        na_set hash_table h (Nativeint_u.of_int pos);
        (* Also check distance cache first *)
        let #(~mr_len:slen, ~mr_dist:sdist, ~mr_code:scode) =
          try_short_code_match ~num_to_check:num_last_distances_to_check
            src pos src_end ring
        in
        if prev >= src_pos && pos - prev <= max_backward_distance then begin
          let match_len = find_match_length src prev pos src_end in
          if match_len >= min_match then begin
            let distance = pos - prev in
            let dist_code = find_short_code ring distance in
            (* Pick best between short code match and hash match *)
            if slen >= min_match then begin
              let s_score = score_match slen sdist scode in
              let h_score = score_match match_len distance dist_code in
              if s_score >= h_score then begin
                hash_len <- slen;
                hash_dist <- sdist;
                hash_code <- scode
              end else begin
                hash_len <- match_len;
                hash_dist <- distance;
                hash_code <- dist_code
              end
            end else begin
              hash_len <- match_len;
              hash_dist <- distance;
              hash_code <- dist_code
            end
          end else if slen >= min_match then begin
            hash_len <- slen;
            hash_dist <- sdist;
            hash_code <- scode
          end else begin
            hash_len <- 0;
            hash_dist <- 0;
            hash_code <- -1
          end
        end
        else if slen >= min_match then begin
          hash_len <- slen;
          hash_dist <- sdist;
          hash_code <- scode
        end else begin
          hash_len <- 0;
          hash_dist <- 0;
          hash_code <- -1
        end
      end;

      (* Update hash chain for quality 4+ *)
      if quality >= 4 then
        update_hash_chain src pos hash_table chain_table chain_table_base;

      (* Try dictionary match if enabled - uses mutable locals *)
      let mutable dict_len = 0 in
      let mutable dict_dist = 0 in
      if use_dict then begin
        let pending_lits = pos - pending_start in
        let current_output_pos = output_pos + pending_lits in
        match Dict_match.find_match src pos src_end max_backward_distance ~current_output_pos with
        | Some (len, dist) ->
          dict_len <- len;
          dict_dist <- dist
        | None -> ()
      end;

      (* Choose the best match based on score - use mutable locals *)
      let mutable match_len = 0 in
      let mutable distance = 0 in
      let mutable dist_code = -1 in
      if hash_len >= min_match && dict_len > 0 then begin
        let lz_score = score_match hash_len hash_dist hash_code in
        let dict_score = score_dict_match dict_len in
        if dict_score > lz_score then begin
          match_len <- dict_len;
          distance <- dict_dist;
          dist_code <- -1
        end else begin
          match_len <- hash_len;
          distance <- hash_dist;
          dist_code <- hash_code
        end
      end
      else if hash_len >= min_match then begin
        match_len <- hash_len;
        distance <- hash_dist;
        dist_code <- hash_code
      end
      else if dict_len > 0 then begin
        match_len <- dict_len;
        distance <- dict_dist;
        dist_code <- -1
      end;

      if match_len >= min_match then begin
        (* Lazy matching for quality 4+: check if delaying gives better match *)
        let use_match =
          if quality >= 4 && pos + 1 < src_end - min_match && match_len < max_match then begin
            (* Update hash for next position *)
            update_hash_chain src (pos + 1) hash_table chain_table chain_table_base;
            let #(~mr_len:next_len, ~mr_dist:next_dist, ~mr_code:next_code) =
              find_best_chain_match src (pos + 1) src_end
                hash_table chain_table chain_table_base ring
                ~num_last_distances_to_check ~max_chain_depth
            in
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
