(* Optimal parsing for Brotli compression (quality 10-11)
   This implements Zopfli-like optimal matching using dynamic programming,
   matching the brotli-c reference implementation in backward_references_hq.c *)

(* Module aliases for unboxed operations *)
module Nu = Stdlib_upstream_compatible.Nativeint_u
module F64 = Stdlib_upstream_compatible.Float_u

(* Configuration constants from brotli-c quality.h *)
let max_zopfli_len_quality_10 = 150
let max_zopfli_len_quality_11 = 325
let max_zopfli_candidates_q10 = 1  (* MaxZopfliCandidates for Q10 *)
let max_zopfli_candidates_q11 = 5  (* MaxZopfliCandidates for Q11 *)
let brotli_long_copy_quick_step = 16384

(* Match parameters *)
let min_match = 4
let max_match = 258
let max_distance = (1 lsl 22) - 16
let hash_bits = 17
let hash_size = 1 lsl hash_bits
let max_tree_search_depth = 64  (* For H10 binary tree hasher *)

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

(* Distance cache index and offset from brotli-c backward_references_hq.c
   Packed as int8# arrays for compact storage (1 byte per element) *)
let distance_cache_index : int8# array =
  [| #0s; #1s; #2s; #3s; #0s; #0s; #0s; #0s; #0s; #0s; #1s; #1s; #1s; #1s; #1s; #1s |]
let distance_cache_offset : int8# array =
  [| #0s; #0s; #0s; #0s; -#1s; #1s; -#2s; #2s; -#3s; #3s; -#1s; #1s; -#2s; #2s; -#3s; #3s |]

(* Infinity for cost comparison *)
let infinity = max_float

(* Fast log2 approximation matching brotli-c FastLog2 - returns boxed for compatibility *)
let[@inline always] fast_log2 v =
  if v <= 0 then 0.0
  else
    let rec log2_floor v acc = if v <= 1 then acc else log2_floor (v lsr 1) (acc + 1) in
    float_of_int (log2_floor v 0)

(* Fast log2 that returns unboxed float# *)
let[@inline always] fast_log2_u v : float# =
  if v <= 0 then #0.0
  else
    let rec log2_floor v acc = if v <= 1 then acc else log2_floor (v lsr 1) (acc + 1) in
    F64.of_int (log2_floor v 0)

(* Pre-computed command cost array - allocated once at module init *)
let precomputed_cost_cmd : float# array =
  (Core.Array.init [@kind float64]) 704 ~f:(fun i -> fast_log2_u (11 + i))

(* Pre-computed distance cost array - allocated once at module init *)
let precomputed_cost_dist : float# array =
  (Core.Array.init [@kind float64]) 544 ~f:(fun i -> fast_log2_u (20 + i))

(* Pre-computed minimum command cost *)
let precomputed_min_cost_cmd : float# = fast_log2_u 11

(* ============================================================
   Cost Model (ZopfliCostModel from brotli-c)
   ============================================================ *)

type cost_model = {
  (* Cost arrays - unboxed float# for better performance *)
  cost_cmd : float# array;           (* Command code costs *)
  cost_dist : float# array;          (* Distance code costs *)
  literal_costs : float# array;      (* Cumulative literal costs *)
  min_cost_cmd : float#;             (* Minimum command cost *)
  num_bytes : int;
}

(* SetCost from brotli-c: calculate Shannon entropy costs from histogram *)
let set_cost histogram histogram_size is_literal : float# array =
  let cost : float# array = (Core.Array.init [@kind float64]) histogram_size ~f:(fun _ -> #0.0) in
  let mutable sum = 0 in
  for i = 0 to histogram_size - 1 do
    sum <- sum + histogram.(i)
  done;
  if sum = 0 then cost
  else begin
    let log2sum = fast_log2_u sum in
    let missing_symbol_sum =
      if is_literal then sum
      else begin
        let mutable zeros = 0 in
        for i = 0 to histogram_size - 1 do
          if histogram.(i) = 0 then zeros <- zeros + 1
        done;
        sum + zeros
      end
    in
    let missing_symbol_cost = F64.add (fast_log2_u missing_symbol_sum) #2.0 in
    for i = 0 to histogram_size - 1 do
      if histogram.(i) = 0 then
        Core.Array.unsafe_set cost i missing_symbol_cost
      else begin
        (* Shannon bits: log2(sum) - log2(count) *)
        let v = F64.sub log2sum (fast_log2_u histogram.(i)) in
        Core.Array.unsafe_set cost i (F64.max #1.0 v)
      end
    done;
    cost
  end

(* UTF-8 position detection from brotli-c literal_cost.c:
   Returns the expected position within a UTF-8 multi-byte sequence.
   0 = single byte or first byte, 1 = second byte, 2 = third byte *)
let utf8_position last_byte current_byte max_utf8 =
  if current_byte < 128 then
    0  (* ASCII - next one is byte 1 again *)
  else if current_byte >= 192 then
    (* Start of multi-byte sequence *)
    min 1 max_utf8
  else begin
    (* Continuation byte - check last byte to determine position *)
    if last_byte < 0xE0 then
      0  (* Completed two-byte sequence *)
    else
      (* Third byte of three-byte sequence *)
      min 2 max_utf8
  end

(* Detect if data is mostly UTF-8 and determine histogram level
   Returns 0 for ASCII, 1 for 2-byte UTF-8, 2 for 3-byte UTF-8 *)
let decide_utf8_level src src_pos len =
  (* Use local unboxed nativeint# array for zero-alloc counting *)
  let local_ counts : nativeint# array = (Core.Array.init [@kind word]) 3 ~f:(fun _ -> #0n) in
  let mutable last_c = 0 in
  for i = 0 to min 2000 len - 1 do
    let c = Char.code (Bytes.get src (src_pos + i)) in
    let utf8_pos = utf8_position last_c c 2 in
    let cur = Core.Array.unsafe_get counts utf8_pos in
    Core.Array.unsafe_set counts utf8_pos (Nu.add cur #1n);
    last_c <- c
  done;
  (* Use 3-byte histograms if >500 third-position bytes,
     2-byte if >25 second/third position bytes combined,
     otherwise single histogram *)
  let c2 = Nu.to_int (Core.Array.unsafe_get counts 2) in
  let c1 = Nu.to_int (Core.Array.unsafe_get counts 1) in
  if c2 < 500 then begin
    if c1 + c2 < 25 then 0
    else 1
  end else 2

(* Sliding window literal cost estimation matching brotli-c literal_cost.c
   Uses a sliding window to estimate per-position literal costs based on
   local byte frequency distribution. For UTF-8 text, uses position-aware
   histograms for better cost estimation. *)
let estimate_literal_costs_sliding_window src src_pos num_bytes : float# array =
  let costs : float# array = (Core.Array.init [@kind float64]) (num_bytes + 2) ~f:(fun _ -> #0.0) in
  if num_bytes = 0 then costs
  else begin
    let max_utf8 = decide_utf8_level src src_pos num_bytes in

    if max_utf8 > 0 then begin
      (* UTF-8 mode: use position-aware histograms
         Flattened into single local unboxed array for better performance.
         Layout: [hist0: 256 elements][hist1: 256 elements][hist2: 256 elements] *)
      let window_half = 495 in  (* Smaller window for UTF-8 from brotli-c *)
      let local_ histograms : nativeint# array = (Core.Array.init [@kind word]) 768 ~f:(fun _ -> #0n) in
      (* Use mutable locals for window counts (max 3 histograms) *)
      let mutable in_win0 = 0 in
      let mutable in_win1 = 0 in
      let mutable in_win2 = 0 in

      (* Bootstrap histograms *)
      let initial_window = min window_half num_bytes in
      let mutable last_c = 0 in
      let mutable utf8_pos_var = 0 in
      for i = 0 to initial_window - 1 do
        let c = Char.code (Bytes.get src (src_pos + i)) in
        let idx = (utf8_pos_var lsl 8) + c in
        let cur = Core.Array.unsafe_get histograms idx in
        Core.Array.unsafe_set histograms idx (Nu.add cur #1n);
        (* Inline increment of in_win for correct histogram *)
        (match utf8_pos_var with 0 -> in_win0 <- in_win0 + 1 | 1 -> in_win1 <- in_win1 + 1 | _ -> in_win2 <- in_win2 + 1);
        utf8_pos_var <- utf8_position last_c c max_utf8;
        last_c <- c
      done;

      Core.Array.unsafe_set costs 0 #0.0;
      let mutable prev1 = 0 in
      let mutable prev2 = 0 in
      let mutable cumulative_cost : float# = #0.0 in
      for i = 0 to num_bytes - 1 do
        (* Slide window: remove byte from past *)
        if i >= window_half then begin
          let past_c = if i < window_half + 1 then 0
            else Char.code (Bytes.get src (src_pos + i - window_half - 1)) in
          let past_last = if i < window_half + 2 then 0
            else Char.code (Bytes.get src (src_pos + i - window_half - 2)) in
          let utf8_pos2 = utf8_position past_last past_c max_utf8 in
          let remove_c = Char.code (Bytes.get src (src_pos + i - window_half)) in
          let idx = (utf8_pos2 lsl 8) + remove_c in
          let cur = Core.Array.unsafe_get histograms idx in
          Core.Array.unsafe_set histograms idx (Nu.sub cur #1n);
          (match utf8_pos2 with 0 -> in_win0 <- in_win0 - 1 | 1 -> in_win1 <- in_win1 - 1 | _ -> in_win2 <- in_win2 - 1)
        end;
        (* Slide window: add byte from future *)
        if i + window_half < num_bytes then begin
          let fut_c = Char.code (Bytes.get src (src_pos + i + window_half - 1)) in
          let fut_last = Char.code (Bytes.get src (src_pos + i + window_half - 2)) in
          let utf8_pos2 = utf8_position fut_last fut_c max_utf8 in
          let add_c = Char.code (Bytes.get src (src_pos + i + window_half)) in
          let idx = (utf8_pos2 lsl 8) + add_c in
          let cur = Core.Array.unsafe_get histograms idx in
          Core.Array.unsafe_set histograms idx (Nu.add cur #1n);
          (match utf8_pos2 with 0 -> in_win0 <- in_win0 + 1 | 1 -> in_win1 <- in_win1 + 1 | _ -> in_win2 <- in_win2 + 1)
        end;

        (* Calculate cost for current byte using UTF-8 position *)
        let c = Char.code (Bytes.get src (src_pos + i)) in
        let utf8_pos = utf8_position prev2 prev1 max_utf8 in
        let idx = (utf8_pos lsl 8) + c in
        let histo = max 1 (Nu.to_int (Core.Array.unsafe_get histograms idx)) in
        let in_win = max 1 (match utf8_pos with 0 -> in_win0 | 1 -> in_win1 | _ -> in_win2) in
        let lit_cost = fast_log2 in_win -. fast_log2 histo +. 0.02905 in
        let lit_cost = if lit_cost < 1.0 then lit_cost *. 0.5 +. 0.5 else lit_cost in
        let prologue_length = 2000 in
        let lit_cost =
          if i < prologue_length then
            lit_cost +. 0.35 +. 0.35 /. float_of_int prologue_length *. float_of_int i
          else lit_cost
        in
        cumulative_cost <- F64.add cumulative_cost (F64.of_float lit_cost);
        Core.Array.unsafe_set costs (i + 1) cumulative_cost;
        prev2 <- prev1;
        prev1 <- c
      done;
      costs
    end else begin
      (* Binary/ASCII mode: single histogram with local unboxed array *)
      let window_half = 2000 in  (* Larger window for non-UTF-8 *)
      let local_ histogram : nativeint# array = (Core.Array.init [@kind word]) 256 ~f:(fun _ -> #0n) in

      (* Bootstrap histogram for first window_half bytes *)
      let initial_window = min window_half num_bytes in
      for i = 0 to initial_window - 1 do
        let c = Char.code (Bytes.get src (src_pos + i)) in
        let cur = Core.Array.unsafe_get histogram c in
        Core.Array.unsafe_set histogram c (Nu.add cur #1n)
      done;
      let mutable in_window = initial_window in

      Core.Array.unsafe_set costs 0 #0.0;
      let mutable cumulative_cost : float# = #0.0 in
      for i = 0 to num_bytes - 1 do
        (* Slide window: remove byte from past *)
        if i >= window_half then begin
          let old_c = Char.code (Bytes.get src (src_pos + i - window_half)) in
          let cur = Core.Array.unsafe_get histogram old_c in
          Core.Array.unsafe_set histogram old_c (Nu.sub cur #1n);
          in_window <- in_window - 1
        end;
        (* Slide window: add byte from future *)
        if i + window_half < num_bytes then begin
          let new_c = Char.code (Bytes.get src (src_pos + i + window_half)) in
          let cur = Core.Array.unsafe_get histogram new_c in
          Core.Array.unsafe_set histogram new_c (Nu.add cur #1n);
          in_window <- in_window + 1
        end;

        (* Calculate cost for current byte *)
        let c = Char.code (Bytes.get src (src_pos + i)) in
        let histo = max 1 (Nu.to_int (Core.Array.unsafe_get histogram c)) in
        let lit_cost = fast_log2 in_window -. fast_log2 histo +. 0.029 in
        let lit_cost = if lit_cost < 1.0 then lit_cost *. 0.5 +. 0.5 else lit_cost in
        let prologue_length = 2000 in
        let lit_cost =
          if i < prologue_length then
            lit_cost +. 0.35 +. 0.35 /. float_of_int prologue_length *. float_of_int i
          else lit_cost
        in
        cumulative_cost <- F64.add cumulative_cost (F64.of_float lit_cost);
        Core.Array.unsafe_set costs (i + 1) cumulative_cost
      done;
      costs
    end
  end

(* Initialize cost model from literal costs (first pass) *)
let init_cost_model_from_literals src src_pos num_bytes =
  (* Use sliding window for accurate per-position literal cost estimation *)
  let literal_costs = estimate_literal_costs_sliding_window src src_pos num_bytes in

  (* Use pre-computed cost arrays to avoid allocation *)
  { cost_cmd = precomputed_cost_cmd;
    cost_dist = precomputed_cost_dist;
    literal_costs;
    min_cost_cmd = precomputed_min_cost_cmd;
    num_bytes }

(* Initialize cost model from command histograms (second pass for Q11) *)
let init_cost_model_from_histograms src src_pos num_bytes
    ~lit_histogram ~cmd_histogram ~dist_histogram =
  (* Literal costs from histogram *)
  let lit_costs = set_cost lit_histogram 256 true in
  let literal_costs : float# array = (Core.Array.init [@kind float64]) (num_bytes + 2) ~f:(fun _ -> #0.0) in
  Core.Array.unsafe_set literal_costs 0 #0.0;
  let mutable cumulative : float# = #0.0 in
  for i = 0 to num_bytes - 1 do
    let c = Char.code (Bytes.get src (src_pos + i)) in
    cumulative <- F64.add cumulative (Core.Array.unsafe_get lit_costs c);
    Core.Array.unsafe_set literal_costs (i + 1) cumulative
  done;

  (* Command costs from histogram *)
  let cost_cmd = set_cost cmd_histogram 704 false in
  let mutable min_cost : float# = F64.of_float infinity in
  for i = 0 to 703 do
    let v = Core.Array.unsafe_get cost_cmd i in
    if F64.compare v min_cost < 0 then min_cost <- v
  done;

  (* Distance costs from histogram *)
  let cost_dist = set_cost dist_histogram 544 false in

  { cost_cmd; cost_dist; literal_costs; min_cost_cmd = min_cost; num_bytes }

let[@inline always] get_literal_cost model from_pos to_pos : float# =
  F64.sub (Core.Array.unsafe_get model.literal_costs to_pos)
          (Core.Array.unsafe_get model.literal_costs from_pos)

let[@inline always] get_command_cost model cmd_code : float# =
  if cmd_code < 704 then Core.Array.unsafe_get model.cost_cmd cmd_code else #20.0

let[@inline always] get_distance_cost model dist_code : float# =
  if dist_code < 544 then Core.Array.unsafe_get model.cost_dist dist_code else #20.0

(* ============================================================
   StartPosQueue - maintains 8 best starting positions
   Uses unboxed nativeint# array for distance cache
   ============================================================ *)

type pos_data = {
  pos : int;
  (* Distance cache as unboxed nativeint# array *)
  dist_cache : nativeint# array;
  costdiff : float#;
  cost : float#;
}

(* Helper to get distance cache value by index *)
let[@inline always] pos_data_get_dc posdata idx =
  Nu.to_int (na_get posdata.dist_cache idx)

type start_pos_queue = {
  mutable q : pos_data array;
  mutable idx : int;
}

let infinity_u : float# = F64.of_float infinity

let create_start_pos_queue () =
  let default_cache = make_nativeint_u_array 4 #0n in
  na_set default_cache 0 (Nu.of_int 16);
  na_set default_cache 1 (Nu.of_int 15);
  na_set default_cache 2 (Nu.of_int 11);
  na_set default_cache 3 (Nu.of_int 4);
  let empty = { pos = 0; dist_cache = default_cache; costdiff = infinity_u; cost = infinity_u } in
  { q = Array.make 8 empty; idx = 0 }

let[@inline always] start_pos_queue_size queue =
  min queue.idx 8

let start_pos_queue_push queue posdata =
  let offset = (lnot queue.idx) land 7 in
  queue.idx <- queue.idx + 1;
  let len = start_pos_queue_size queue in
  queue.q.(offset) <- posdata;
  (* Restore sorted order by costdiff *)
  let q = queue.q in
  for i = 1 to len - 1 do
    let idx1 = (offset + i - 1) land 7 in
    let idx2 = (offset + i) land 7 in
    if F64.compare q.(idx1).costdiff q.(idx2).costdiff > 0 then begin
      let tmp = q.(idx1) in
      q.(idx1) <- q.(idx2);
      q.(idx2) <- tmp
    end
  done

let[@inline always] start_pos_queue_at queue k =
  queue.q.((k - queue.idx) land 7)

(* ============================================================
   Zopfli Node - DP state at each position
   ============================================================ *)

type zopfli_node = {
  mutable length : int;           (* Copy length (lower 25 bits) + len_code modifier *)
  mutable distance : int;         (* Copy distance *)
  mutable dcode_insert_length : int; (* Short code (upper 5 bits) + insert length *)
  mutable cost : float#;          (* Cost or next pointer - unboxed *)
  mutable shortcut : int;         (* Shortcut for distance cache computation *)
}

let create_zopfli_node () =
  { length = 1; distance = 0; dcode_insert_length = 0; cost = infinity_u; shortcut = 0 }

let[@inline always] zopfli_node_copy_length node = node.length land 0x1FFFFFF
let[@inline always] zopfli_node_copy_distance node = node.distance
let[@inline always] zopfli_node_insert_length node = node.dcode_insert_length land 0x7FFFFFF
let[@inline always] zopfli_node_distance_code node =
  let short_code = node.dcode_insert_length lsr 27 in
  if short_code = 0 then zopfli_node_copy_distance node + 16 - 1
  else short_code - 1

let[@inline always] zopfli_node_command_length node =
  zopfli_node_copy_length node + zopfli_node_insert_length node

(* ============================================================
   Hash functions and match finding
   ============================================================ *)

let[@inline always] hash4 src pos =
  (* Use native 32-bit load instead of byte-by-byte loading *)
  let v = Int32.to_int (Bytes.get_int32_le src pos) land 0xFFFFFFFF in
  ((v * 0x1e35a7bd) land 0xFFFFFFFF) lsr (32 - hash_bits)

let[@inline always] find_match_length src a b limit =
  let mutable len = 0 in
  let max_len = min max_match (limit - b) in
  while len < max_len && Bytes.get src (a + len) = Bytes.get src (b + len) do
    len <- len + 1
  done;
  len

(* Backward match structure *)
type backward_match = {
  bm_distance : int;
  bm_length : int;
  bm_len_code : int;
}

(* Pre-allocated match buffer to avoid repeated allocation in hot path.
   Max depth is max_tree_search_depth (64), but we typically find fewer matches.
   Using unboxed nativeint# arrays for better performance. *)
let match_buffer_size = max_tree_search_depth
let match_buffer_dist : nativeint# array = make_nativeint_u_array match_buffer_size #0n
let match_buffer_len : nativeint# array = make_nativeint_u_array match_buffer_size #0n

(* Find all matches at a position, sorted by length.
   Uses unboxed nativeint# arrays for hash_table and chain_table for better performance.
   Optimized: Uses pre-allocated arrays instead of list to reduce allocations. *)
let find_all_matches src pos src_end (hash_table : nativeint# array) (chain_table : nativeint# array) chain_base max_distance =
  if pos + min_match > src_end then []
  else begin
    let mutable match_count = 0 in
    let mutable best_len = min_match - 1 in

    (* Search hash chain *)
    let h = hash4 src pos in
    (* Use mutable for unboxed chain position to avoid ref boxing *)
    let mutable chain_pos_u : nativeint# = na_get hash_table h in
    let mutable chain_count = 0 in

    while Nativeint_u.(chain_pos_u >= #0n) && chain_count < max_tree_search_depth do
      let chain_pos = Nativeint_u.to_int_trunc chain_pos_u in
      let distance = pos - chain_pos in
      if distance > 0 && distance <= max_distance then begin
        let match_len = find_match_length src chain_pos pos src_end in
        if match_len > best_len then begin
          best_len <- match_len;
          (* Store in pre-allocated unboxed buffer *)
          if match_count < match_buffer_size then begin
            na_set match_buffer_dist match_count (Nu.of_int distance);
            na_set match_buffer_len match_count (Nu.of_int match_len);
            match_count <- match_count + 1
          end
        end
      end;
      let chain_idx = chain_pos - chain_base in
      if chain_idx >= 0 && chain_idx < Nativeint_u.Array.length chain_table then
        chain_pos_u <- na_get chain_table chain_idx
      else
        chain_pos_u <- invalid_pos;
      chain_count <- chain_count + 1
    done;

    (* Convert to list and sort by length ascending.
       Note: matches are already mostly sorted by length (descending) due to how
       the hash chain stores positions, so sorting should be fast. *)
    if match_count = 0 then []
    else begin
      (* Build list in reverse (smallest first) since matches are stored largest-first *)
      let matches = ref [] in
      for i = 0 to match_count - 1 do
        let dist = Nu.to_int (na_get match_buffer_dist i) in
        let len = Nu.to_int (na_get match_buffer_len i) in
        matches := { bm_distance = dist;
                     bm_length = len;
                     bm_len_code = len } :: !matches
      done;
      (* Simple insertion sort - fast for small arrays and already mostly sorted *)
      List.sort (fun a b -> compare a.bm_length b.bm_length) !matches
    end
  end

(* ============================================================
   Insert/Copy length encoding (from brotli-c prefix.h)
   ============================================================ *)

(* Pre-computed insert extra bits table - values 0-24 fit in int8# *)
let kInsertExtraBits : int8# array = [| #0s;#0s;#0s;#0s;#0s;#0s;#1s;#1s;#2s;#2s;#3s;#3s;#4s;#4s;#5s;#5s;#6s;#7s;#8s;#9s;#10s;#12s;#14s;#24s |]

(* Pre-computed copy extra bits table - values 0-24 fit in int8# *)
let kCopyExtraBits : int8# array = [| #0s;#0s;#0s;#0s;#0s;#0s;#0s;#0s;#1s;#1s;#2s;#2s;#3s;#3s;#4s;#4s;#5s;#5s;#6s;#7s;#8s;#9s;#10s;#24s |]

let[@inline always] get_insert_length_code insert_len =
  if insert_len < 6 then insert_len
  else if insert_len < 130 then
    let nbits = Lz77.log2_floor_nonzero (insert_len - 2) - 1 in
    (nbits lsl 1) + ((insert_len - 2) lsr nbits) + 2
  else if insert_len < 2114 then
    Lz77.log2_floor_nonzero (insert_len - 66) + 10
  else if insert_len < 6210 then 21
  else if insert_len < 22594 then 22
  else 23

let[@inline always] get_copy_length_code copy_len =
  if copy_len < 10 then copy_len - 2
  else if copy_len < 134 then
    let nbits = Lz77.log2_floor_nonzero (copy_len - 6) - 1 in
    (nbits lsl 1) + ((copy_len - 6) lsr nbits) + 4
  else if copy_len < 2118 then
    Lz77.log2_floor_nonzero (copy_len - 70) + 12
  else 23

let[@inline always] get_insert_extra insert_code =
  if insert_code < 24 then Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get kInsertExtraBits insert_code) else 24

let[@inline always] get_copy_extra copy_code =
  if copy_code < 24 then Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get kCopyExtraBits copy_code) else 24

let[@inline always] combine_length_codes inscode copycode use_last_distance =
  let inscode64 = (inscode land 0x7) lor ((inscode land 0x18) lsl 2) in
  let copycode64 = (copycode land 0x7) lor ((copycode land 0x18) lsl 3) in
  let c = (copycode64 land 0x38) lor inscode64 in
  if use_last_distance && inscode < 8 && copycode < 16 then c
  else if inscode < 8 && copycode < 16 then c lor 64
  else c lor (128 + (if copycode >= 16 then 64 else 0))

(* ============================================================
   Distance encoding
   ============================================================ *)

let[@inline always] prefix_encode_copy_distance dist_code =
  if dist_code < 16 then (dist_code, 0, 0)
  else begin
    let dist = dist_code - 15 in
    let nbits = Lz77.log2_floor_nonzero dist in
    let prefix = (nbits lsl 1) + ((dist lsr (nbits - 1)) land 1) + 12 in
    let extra_bits = nbits - 1 in
    let extra = dist land ((1 lsl extra_bits) - 1) in
    (prefix, extra_bits, extra)
  end

(* ============================================================
   Main Zopfli DP Algorithm
   ============================================================ *)

(* Compute distance cache at a position from the DP path.
   Returns unboxed nativeint# array with 4 distance values. *)
let compute_distance_cache pos starting_dist_cache nodes : nativeint# array =
  (* Use unboxed nativeint# array - distances can be up to 2^22 *)
  let dist_cache : nativeint# array = make_nativeint_u_array 4 #0n in
  let mutable idx = 0 in
  let mutable p = nodes.(pos).shortcut in
  while idx < 4 && p > 0 do
    let node = nodes.(p) in
    let c_len = zopfli_node_copy_length node in
    let i_len = zopfli_node_insert_length node in
    let dist = zopfli_node_copy_distance node in
    na_set dist_cache idx (Nu.of_int dist);
    idx <- idx + 1;
    p <- nodes.(p - c_len - i_len).shortcut
  done;
  for i = idx to 3 do
    na_set dist_cache i (Nu.of_int starting_dist_cache.(i - idx))
  done;
  dist_cache

(* Compute distance shortcut *)
let compute_distance_shortcut block_start pos max_backward_limit nodes =
  if pos = 0 then 0
  else begin
    let node = nodes.(pos) in
    let c_len = zopfli_node_copy_length node in
    let i_len = zopfli_node_insert_length node in
    let dist = zopfli_node_copy_distance node in
    if dist + c_len <= block_start + pos &&
       dist <= max_backward_limit &&
       zopfli_node_distance_code node > 0 then
      pos
    else
      nodes.(pos - c_len - i_len).shortcut
  end

(* Update Zopfli node with new values *)
let[@inline always] update_zopfli_node nodes pos start len len_code dist short_code cost =
  let node = nodes.(pos + len) in
  node.length <- len lor ((len + 9 - len_code) lsl 25);
  node.distance <- dist;
  node.dcode_insert_length <- (short_code lsl 27) lor (pos - start);
  node.cost <- cost

(* Compute minimum copy length that can improve cost *)
let compute_minimum_copy_length (start_cost : float#) nodes num_bytes pos =
  let mutable min_cost : float# = start_cost in
  let mutable len = 2 in
  let mutable next_len_bucket = 4 in
  let mutable next_len_offset = 10 in
  while pos + len <= num_bytes && F64.compare nodes.(pos + len).cost min_cost <= 0 do
    len <- len + 1;
    if len = next_len_offset then begin
      min_cost <- F64.add min_cost #1.0;
      next_len_offset <- next_len_offset + next_len_bucket;
      next_len_bucket <- next_len_bucket * 2
    end
  done;
  len

(* Evaluate node and push to queue if eligible *)
let evaluate_node block_start pos max_backward_limit starting_dist_cache model queue nodes =
  let node_cost = nodes.(pos).cost in
  nodes.(pos).shortcut <- compute_distance_shortcut block_start pos max_backward_limit nodes;
  if F64.compare node_cost (get_literal_cost model 0 pos) <= 0 then begin
    let dist_cache = compute_distance_cache pos starting_dist_cache nodes in
    let posdata = {
      pos;
      dist_cache;
      costdiff = F64.sub node_cost (get_literal_cost model 0 pos);
      cost = node_cost;
    } in
    start_pos_queue_push queue posdata
  end

(* Update nodes at a position - core Zopfli DP step *)
let update_nodes num_bytes block_start pos src src_pos model
    max_backward_limit starting_dist_cache
    num_matches matches queue nodes max_zopfli_len max_iters =
  let cur_ix = block_start + pos in
  let max_distance_here = min cur_ix max_backward_limit in
  let max_len = num_bytes - pos in
  let mutable result = 0 in

  evaluate_node block_start pos max_backward_limit starting_dist_cache model queue nodes;

  (* Compute minimum copy length based on best queue entry *)
  let posdata0 = start_pos_queue_at queue 0 in
  let min_cost = F64.add (F64.add posdata0.cost model.min_cost_cmd) (get_literal_cost model posdata0.pos pos) in
  let min_len = compute_minimum_copy_length min_cost nodes num_bytes pos in

  (* Go over starting positions in order of increasing cost difference *)
  let queue_size = start_pos_queue_size queue in
  for k = 0 to min (max_iters - 1) (queue_size - 1) do
    let posdata = start_pos_queue_at queue k in
    let start = posdata.pos in
    let inscode = get_insert_length_code (pos - start) in
    let start_costdiff = posdata.costdiff in
    let base_cost = F64.add (F64.add start_costdiff (F64.of_int (get_insert_extra inscode)))
                            (get_literal_cost model 0 pos) in

    (* Check distance cache matches first *)
    let mutable best_len = min_len - 1 in
    for j = 0 to 15 do
      if best_len < max_len then begin
        let idx = Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get distance_cache_index j) in
        let cache_off = Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get distance_cache_offset j) in
        let backward = pos_data_get_dc posdata idx + cache_off in
        if backward > 0 && backward <= max_distance_here then begin
          let prev_ix = cur_ix - backward in
          let match_len = find_match_length src prev_ix (src_pos + pos) (src_pos + num_bytes) in
          if match_len >= 2 then begin
            let dist_cost = F64.add base_cost (get_distance_cost model j) in
            for l = best_len + 1 to match_len do
              let copycode = get_copy_length_code l in
              let cmdcode = combine_length_codes inscode copycode (j = 0) in
              let cost = F64.add (F64.add (if cmdcode < 128 then base_cost else dist_cost)
                                          (F64.of_int (get_copy_extra copycode)))
                                 (get_command_cost model cmdcode) in
              if F64.compare cost nodes.(pos + l).cost < 0 then begin
                update_zopfli_node nodes pos start l l backward (j + 1) cost;
                result <- max result l
              end;
              best_len <- l
            done
          end
        end
      end
    done;

    (* For iterations >= 2, only look at distance cache matches *)
    if k < 2 then begin
      (* Loop through all matches *)
      let mutable len = min_len in
      for j = 0 to num_matches - 1 do
        let m = matches.(j) in
        let dist = m.bm_distance in
        let dist_code = dist + 16 - 1 in  (* Add 16 short codes *)
        let (dist_symbol, distnumextra, _) = prefix_encode_copy_distance dist_code in
        let dist_cost = F64.add (F64.add base_cost (F64.of_int distnumextra))
                                (get_distance_cost model dist_symbol) in
        let max_match_len = m.bm_length in

        (* For long matches or dictionary, try only max length *)
        if len < max_match_len && max_match_len > max_zopfli_len then
          len <- max_match_len;

        while len <= max_match_len do
          let len_code = m.bm_len_code in
          let copycode = get_copy_length_code len_code in
          let cmdcode = combine_length_codes inscode copycode false in
          let cost = F64.add (F64.add dist_cost (F64.of_int (get_copy_extra copycode)))
                             (get_command_cost model cmdcode) in
          if F64.compare cost nodes.(pos + len).cost < 0 then begin
            update_zopfli_node nodes pos start len len_code dist 0 cost;
            result <- max result len
          end;
          len <- len + 1
        done
      done
    end
  done;
  result

(* Compute shortest path from nodes *)
let compute_shortest_path_from_nodes num_bytes nodes =
  let mutable index = num_bytes in
  let mutable num_commands = 0 in
  (* Find the actual end position *)
  while zopfli_node_insert_length nodes.(index) = 0 &&
        nodes.(index).length = 1 && index > 0 do
    index <- index - 1
  done;
  nodes.(index).shortcut <- max_int;  (* Mark as end *)
  while index > 0 do
    let len = zopfli_node_command_length nodes.(index) in
    index <- index - len;
    nodes.(index).shortcut <- len;  (* Use shortcut to store next length *)
    num_commands <- num_commands + 1
  done;
  num_commands

(* ============================================================
   Main Zopfli function for Q10
   ============================================================ *)

let zopfli_compute_shortest_path src src_pos num_bytes starting_dist_cache =
  let max_backward_limit = max_distance in
  let max_zopfli_len = max_zopfli_len_quality_10 in
  let max_iters = max_zopfli_candidates_q10 in

  (* Initialize nodes *)
  let nodes = Array.init (num_bytes + 1) (fun _ -> create_zopfli_node ()) in
  nodes.(0).length <- 0;
  nodes.(0).cost <- #0.0;

  (* Initialize cost model from literal costs (first pass) *)
  let model = init_cost_model_from_literals src src_pos num_bytes in

  (* Hash table and chain - use unboxed nativeint# for better performance *)
  let hash_table : nativeint# array = make_nativeint_u_array hash_size invalid_pos in
  let chain_table : nativeint# array = make_nativeint_u_array num_bytes invalid_pos in
  let chain_base = src_pos in

  (* Initialize queue *)
  let queue = create_start_pos_queue () in

  (* Main DP loop *)
  let mutable i = 0 in
  while i + min_match - 1 < num_bytes do
    let pos = src_pos + i in
    let max_distance_here = min pos max_backward_limit in

    (* Update hash table *)
    if pos + min_match <= src_pos + num_bytes then begin
      let h = hash4 src pos in
      let chain_idx = i in
      if chain_idx < Nativeint_u.Array.length chain_table then
        na_set chain_table chain_idx (na_get hash_table h);
      na_set hash_table h (Nativeint_u.of_int pos)
    end;

    (* Find all matches *)
    let matches = find_all_matches src pos (src_pos + num_bytes)
      hash_table chain_table chain_base max_distance_here in
    let matches_arr = Array.of_list matches in
    let num_matches = Array.length matches_arr in

    (* Check for long match to skip *)
    let skip =
      if num_matches > 0 then begin
        let last_match = matches_arr.(num_matches - 1) in
        if last_match.bm_length > max_zopfli_len then begin
          (* Use only longest match *)
          matches_arr.(0) <- last_match;
          last_match.bm_length
        end else 0
      end else 0
    in

    let update_skip = update_nodes num_bytes src_pos i src src_pos model
      max_backward_limit starting_dist_cache
      (if skip > 0 then 1 else num_matches) matches_arr queue nodes
      max_zopfli_len max_iters in

    let actual_skip = if update_skip < brotli_long_copy_quick_step then 0 else update_skip in
    let skip = max skip actual_skip in

    if skip > 1 then begin
      let mutable skip_remaining = skip - 1 in
      while skip_remaining > 0 && i + min_match - 1 < num_bytes do
        i <- i + 1;
        evaluate_node src_pos i max_backward_limit starting_dist_cache model queue nodes;
        skip_remaining <- skip_remaining - 1
      done
    end;
    i <- i + 1
  done;

  (nodes, compute_shortest_path_from_nodes num_bytes nodes)

(* ============================================================
   HQ Zopfli function for Q11 (two passes with histogram refinement)
   ============================================================ *)

(* Build histograms from completed DP nodes for second pass cost refinement.
   This matches brotli-c ZopfliCostModelSetFromCommands in backward_references_hq.c *)
let build_histograms_from_nodes src src_pos num_bytes nodes =
  let lit_histogram = Array.make 256 0 in
  let cmd_histogram = Array.make 704 0 in
  let dist_histogram = Array.make 544 0 in

  (* Reconstruct path from nodes *)
  let idx = ref num_bytes in
  (* Find the actual end position *)
  while zopfli_node_insert_length nodes.(!idx) = 0 &&
        nodes.(!idx).length = 1 && !idx > 0 do
    decr idx
  done;

  let pending_lit_start = ref 0 in
  let end_pos = !idx in

  (* Walk backwards through the path *)
  idx := end_pos;
  let path = ref [] in
  while !idx > 0 do
    let node = nodes.(!idx) in
    let cmd_len = zopfli_node_command_length node in
    if cmd_len > 0 then begin
      path := !idx :: !path;
      idx := !idx - cmd_len
    end else
      idx := 0
  done;

  (* Process path forward to count symbols *)
  pending_lit_start := 0;
  List.iter (fun end_pos ->
    let node = nodes.(end_pos) in
    let copy_len = zopfli_node_copy_length node in
    let _insert_len = zopfli_node_insert_length node in
    let dist_code = zopfli_node_distance_code node in

    let copy_start = end_pos - copy_len in
    let lit_len = copy_start - !pending_lit_start in

    (* Count literals *)
    for i = !pending_lit_start to copy_start - 1 do
      let c = Char.code (Bytes.get src (src_pos + i)) in
      lit_histogram.(c) <- lit_histogram.(c) + 1
    done;

    (* Count command code *)
    let inscode = get_insert_length_code lit_len in
    let copycode = get_copy_length_code copy_len in
    let use_last = dist_code = 0 in
    let cmdcode = combine_length_codes inscode copycode use_last in
    if cmdcode < 704 then
      cmd_histogram.(cmdcode) <- cmd_histogram.(cmdcode) + 1;

    (* Count distance code if explicit *)
    if cmdcode >= 128 then begin
      let dc = if dist_code < 16 then dist_code
               else begin
                 let (symbol, _, _) = prefix_encode_copy_distance (node.distance + 16 - 1) in
                 symbol
               end
      in
      if dc < 544 then
        dist_histogram.(dc) <- dist_histogram.(dc) + 1
    end;

    pending_lit_start := end_pos
  ) !path;

  (* Count remaining literals *)
  for i = !pending_lit_start to num_bytes - 1 do
    let c = Char.code (Bytes.get src (src_pos + i)) in
    lit_histogram.(c) <- lit_histogram.(c) + 1
  done;

  (lit_histogram, cmd_histogram, dist_histogram)

let hq_zopfli_compute_shortest_path src src_pos num_bytes starting_dist_cache =
  let max_backward_limit = max_distance in
  let max_zopfli_len = max_zopfli_len_quality_11 in
  let max_iters = max_zopfli_candidates_q11 in

  (* Pre-compute all matches - use unboxed nativeint# arrays for better performance *)
  let hash_table : nativeint# array = make_nativeint_u_array hash_size invalid_pos in
  let chain_table : nativeint# array = make_nativeint_u_array num_bytes invalid_pos in
  let chain_base = src_pos in
  let all_matches = Array.make num_bytes [||] in
  (* Use unboxed nativeint# array for match counts *)
  let num_matches_arr : nativeint# array = make_nativeint_u_array num_bytes #0n in

  for i = 0 to num_bytes - min_match do
    let pos = src_pos + i in
    let max_distance_here = min pos max_backward_limit in

    (* Update hash *)
    if pos + min_match <= src_pos + num_bytes then begin
      let h = hash4 src pos in
      na_set chain_table i (na_get hash_table h);
      na_set hash_table h (Nativeint_u.of_int pos)
    end;

    let matches = find_all_matches src pos (src_pos + num_bytes)
      hash_table chain_table chain_base max_distance_here in
    let matches_arr = Array.of_list matches in
    all_matches.(i) <- matches_arr;
    na_set num_matches_arr i (Nu.of_int (Array.length matches_arr));

    (* Skip after very long match *)
    if Array.length matches_arr > 0 then begin
      let last = matches_arr.(Array.length matches_arr - 1) in
      if last.bm_length > max_zopfli_len then begin
        let skip = last.bm_length - 1 in
        for j = 1 to min skip (num_bytes - min_match - i) do
          all_matches.(i + j) <- [||];
          na_set num_matches_arr (i + j) #0n
        done
      end
    end
  done;

  (* Do two iterations with histogram refinement *)
  let final_nodes = ref (Array.init (num_bytes + 1) (fun _ -> create_zopfli_node ())) in
  let final_count = ref 0 in
  let first_pass_nodes = ref None in

  for iteration = 0 to 1 do
    let nodes = Array.init (num_bytes + 1) (fun _ -> create_zopfli_node ()) in
    nodes.(0).length <- 0;
    nodes.(0).cost <- #0.0;

    let model =
      if iteration = 0 then
        (* First pass: use sliding window literal cost estimation *)
        init_cost_model_from_literals src src_pos num_bytes
      else begin
        (* Second pass: build histograms from first pass for refined estimation *)
        match !first_pass_nodes with
        | None -> init_cost_model_from_literals src src_pos num_bytes
        | Some prev_nodes ->
          let (lit_hist, cmd_hist, dist_hist) =
            build_histograms_from_nodes src src_pos num_bytes prev_nodes
          in
          init_cost_model_from_histograms src src_pos num_bytes
            ~lit_histogram:lit_hist ~cmd_histogram:cmd_hist ~dist_histogram:dist_hist
      end
    in

    let queue = create_start_pos_queue () in

    (* Main DP loop *)
    let mutable i = 0 in
    while i + min_match - 1 < num_bytes do
      let skip = update_nodes num_bytes src_pos i src src_pos model
        max_backward_limit starting_dist_cache
        (Nu.to_int (na_get num_matches_arr i)) all_matches.(i) queue nodes
        max_zopfli_len max_iters in

      let skip = if skip < brotli_long_copy_quick_step then 0 else skip in

      if skip > 1 then begin
        let mutable skip_remaining = skip - 1 in
        while skip_remaining > 0 && i + min_match - 1 < num_bytes do
          i <- i + 1;
          evaluate_node src_pos i max_backward_limit starting_dist_cache model queue nodes;
          skip_remaining <- skip_remaining - 1
        done
      end;
      i <- i + 1
    done;

    (* Save first pass nodes for histogram building *)
    if iteration = 0 then begin
      let _ = compute_shortest_path_from_nodes num_bytes nodes in
      first_pass_nodes := Some nodes
    end;

    final_nodes := nodes;
    final_count := compute_shortest_path_from_nodes num_bytes nodes
  done;

  (!final_nodes, !final_count)

(* ============================================================
   Create commands from Zopfli nodes
   ============================================================ *)

let zopfli_create_commands num_bytes src_pos nodes =
  let commands = ref [] in
  let ring = Lz77.create_dist_ring () in

  (* First, reconstruct the path using shortcut field *)
  let path = ref [] in
  let idx = ref num_bytes in
  while !idx > 0 && nodes.(!idx).shortcut <> max_int do
    path := !idx :: !path;
    let len = nodes.(!idx).shortcut in
    if len > 0 && len <= !idx then
      idx := !idx - len
    else
      idx := 0
  done;

  (* Now process each command in the path *)
  let pending_lit_start = ref 0 in
  List.iter (fun end_pos ->
    let node = nodes.(end_pos) in
    let copy_len = zopfli_node_copy_length node in
    let _insert_len = zopfli_node_insert_length node in
    let distance = zopfli_node_copy_distance node in
    let dist_code = zopfli_node_distance_code node in

    let copy_start = end_pos - copy_len in
    let lit_len = copy_start - !pending_lit_start in

    (* Determine short code: -1 = no short code, 0-15 = valid short code *)
    let short_code =
      if dist_code < 16 then dist_code
      else -1
    in

    commands := Lz77.InsertCopy {
      lit_start = src_pos + !pending_lit_start;
      lit_len;
      copy_len;
      distance;
      dist_code = short_code;
    } :: !commands;

    (* Update ring buffer *)
    if short_code <> 0 then Lz77.push_distance ring distance;

    pending_lit_start := end_pos
  ) !path;

  (* Handle remaining literals *)
  if !pending_lit_start < num_bytes then
    commands := Lz77.Literals {
      start = src_pos + !pending_lit_start;
      len = num_bytes - !pending_lit_start
    } :: !commands;

  List.rev !commands

(* ============================================================
   Public API
   ============================================================ *)

let generate_commands ?(quality=11) src src_pos src_len =
  if src_len = 0 then []
  else if src_len < min_match then
    [Lz77.Literals { start = src_pos; len = src_len }]
  else begin
    let starting_dist_cache = [| 16; 15; 11; 4 |] in

    let (nodes, _num_commands) =
      if quality >= 11 then
        hq_zopfli_compute_shortest_path src src_pos src_len starting_dist_cache
      else
        zopfli_compute_shortest_path src src_pos src_len starting_dist_cache
    in

    zopfli_create_commands src_len src_pos nodes
  end
