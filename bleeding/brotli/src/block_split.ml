(* Block splitting and entropy analysis for Brotli compression *)
(* This module provides block splitting for improved compression at higher quality levels *)

(* Histogram for entropy calculation *)
type histogram = {
  mutable data : int array;
  mutable total : int;
}

let create_histogram size = { data = Array.make size 0; total = 0 }

let add_sample hist symbol =
  hist.data.(symbol) <- hist.data.(symbol) + 1;
  hist.total <- hist.total + 1

let clear_histogram hist =
  Array.fill hist.data 0 (Array.length hist.data) 0;
  hist.total <- 0

(* Estimate bits needed to encode histogram using Shannon entropy *)
let entropy_bits hist =
  if hist.total = 0 then 0.0
  else begin
    let total = float_of_int hist.total in
    let log2 = log 2.0 in
    let mutable bits = 0.0 in
    for i = 0 to Array.length hist.data - 1 do
      let count = hist.data.(i) in
      if count > 0 then begin
        let p = float_of_int count /. total in
        bits <- bits -. (float_of_int count) *. (log p /. log2)
      end
    done;
    bits
  end

(* Combined cost model: entropy + Huffman code overhead *)
let histogram_cost hist =
  let base_cost = entropy_bits hist in
  (* Add overhead for code definition - roughly 5 bits per unique symbol *)
  let num_symbols = Array.fold_left (fun acc c -> if c > 0 then acc + 1 else acc) 0 hist.data in
  base_cost +. (float_of_int num_symbols *. 5.0)

(* Combine two histograms *)
let combine_histograms h1 h2 =
  let result = create_histogram (Array.length h1.data) in
  for i = 0 to Array.length h1.data - 1 do
    result.data.(i) <- h1.data.(i) + h2.data.(i)
  done;
  result.total <- h1.total + h2.total;
  result

(* Bit cost increase when combining two histograms vs. separate encoding *)
let split_cost_delta h1 h2 =
  let combined = combine_histograms h1 h2 in
  let combined_cost = histogram_cost combined in
  let separate_cost = histogram_cost h1 +. histogram_cost h2 in
  combined_cost -. separate_cost

(* Block split point *)
type split_point = {
  position : int;  (* Byte offset in input *)
  score : float;   (* Score for this split point *)
}

(* Minimum block size for splitting (smaller blocks aren't worth the overhead) *)
let min_block_size = 1024

(* Maximum number of block types *)
let max_block_types = 256

(* Maximum blocks per meta-block *)
let max_blocks = 256

(* Analyze data and find potential split points based on entropy changes *)
let find_split_points_simple src src_pos src_len =
  if src_len < min_block_size * 2 then
    []
  else begin
    let window_size = min 256 (src_len / 8) in
    let stride = max 64 (window_size / 2) in
    let points = ref [] in

    let hist1 = create_histogram 256 in
    let hist2 = create_histogram 256 in

    let mutable pos = src_pos + window_size in
    while pos < src_pos + src_len - window_size do
      (* Build histogram for window before position *)
      clear_histogram hist1;
      for i = pos - window_size to pos - 1 do
        add_sample hist1 (Char.code (Bytes.get src i))
      done;

      (* Build histogram for window after position *)
      clear_histogram hist2;
      for i = pos to min (pos + window_size - 1) (src_pos + src_len - 1) do
        add_sample hist2 (Char.code (Bytes.get src i))
      done;

      (* Calculate cost delta - higher = better split point *)
      let delta = split_cost_delta hist1 hist2 in
      if delta > 50.0 then (* Threshold for significant change *)
        points := { position = pos; score = delta } :: !points;

      pos <- pos + stride
    done;

    (* Sort by score and filter to keep only best split points *)
    let sorted = List.sort (fun a b -> compare b.score a.score) !points in

    (* Keep only non-overlapping splits that improve compression *)
    let rec filter_overlapping acc remaining =
      match remaining with
      | [] -> acc
      | p :: rest ->
        let dominated = List.exists (fun q ->
          abs (p.position - q.position) < min_block_size
        ) acc in
        if dominated then filter_overlapping acc rest
        else filter_overlapping (p :: acc) rest
    in

    let filtered = filter_overlapping [] sorted in

    (* Sort by position and limit to max_blocks - 1 splits *)
    let by_position = List.sort (fun a b -> compare a.position b.position) filtered in
    let limited =
      if List.length by_position >= max_blocks then
        let rec take n lst = if n = 0 then [] else match lst with
          | [] -> []
          | h :: t -> h :: take (n-1) t
        in
        take (max_blocks - 1) by_position
      else
        by_position
    in

    List.map (fun p -> p.position) limited
  end

(* Fast log2 for bit cost calculation *)
let[@inline always] fast_log2 v =
  if v <= 0 then 0.0
  else
    let rec log2_floor v acc = if v <= 1 then acc else log2_floor (v lsr 1) (acc + 1) in
    float_of_int (log2_floor v 0)

(* Bit cost for a symbol given a histogram - matches brotli-c BitCost *)
let bit_cost count =
  if count = 0 then fast_log2 1 +. 2.0  (* Missing symbol penalty *)
  else fast_log2 count

(* Per-position DP block splitting matching brotli-c block_splitter_inc.h FN(FindBlocks)
   This tracks costs for multiple histograms simultaneously and finds optimal switch points *)
let find_blocks_dp src src_pos src_len num_histograms =
  if src_len < min_block_size || num_histograms <= 1 then
    (* Trivial case: single block *)
    Array.make src_len 0
  else begin
    let block_id = Array.make src_len 0 in

    (* Initialize histograms with random samples (matching brotli-c InitialEntropyCodes) *)
    let histograms = Array.init num_histograms (fun _ -> create_histogram 256) in
    let block_length = src_len / num_histograms in
    for i = 0 to num_histograms - 1 do
      let start_pos = i * block_length in
      let sample_len = min 64 block_length in
      for j = 0 to sample_len - 1 do
        if start_pos + j < src_len then begin
          let c = Char.code (Bytes.get src (src_pos + start_pos + j)) in
          add_sample histograms.(i) c
        end
      done
    done;

    (* Compute insert costs for each symbol in each histogram *)
    let insert_cost = Array.make_matrix 256 num_histograms 0.0 in
    for h = 0 to num_histograms - 1 do
      let log2_total = if histograms.(h).total > 0 then
        fast_log2 histograms.(h).total
      else 0.0 in
      for sym = 0 to 255 do
        (* Cost = log2(total) - log2(count) = -log2(probability) *)
        insert_cost.(sym).(h) <- log2_total -. bit_cost histograms.(h).data.(sym)
      done
    done;

    (* DP: cost.(h) = cost difference from minimum for reaching current position with histogram h *)
    let cost = Array.make num_histograms 0.0 in
    let switch_signal = Array.make_matrix src_len num_histograms false in

    (* Block switch cost from brotli-c *)
    let base_block_switch_cost = 28.1 in  (* From brotli-c *)
    let prologue_length = 2000 in

    (* Main DP loop *)
    for byte_ix = 0 to src_len - 1 do
      let sym = Char.code (Bytes.get src (src_pos + byte_ix)) in
      let mutable min_cost = infinity in

      (* Update costs for each histogram *)
      for h = 0 to num_histograms - 1 do
        cost.(h) <- cost.(h) +. insert_cost.(sym).(h);
        if cost.(h) < min_cost then begin
          min_cost <- cost.(h);
          block_id.(byte_ix) <- h
        end
      done;

      (* Normalize costs and mark switch signals *)
      let block_switch_cost =
        if byte_ix < prologue_length then
          base_block_switch_cost *. (0.77 +. 0.07 /. 2000.0 *. float_of_int byte_ix)
        else base_block_switch_cost
      in

      for h = 0 to num_histograms - 1 do
        cost.(h) <- cost.(h) -. min_cost;
        if cost.(h) >= block_switch_cost then begin
          cost.(h) <- block_switch_cost;
          switch_signal.(byte_ix).(h) <- true
        end
      done
    done;

    (* Traceback: find block boundaries *)
    let mutable cur_id = block_id.(src_len - 1) in
    for byte_ix = src_len - 2 downto 0 do
      if switch_signal.(byte_ix).(cur_id) then
        cur_id <- block_id.(byte_ix);
      block_id.(byte_ix) <- cur_id
    done;

    block_id
  end

(* More sophisticated splitting using dynamic programming *)
let find_split_points_dp src src_pos src_len max_splits =
  if src_len < min_block_size * 2 then
    []
  else begin
    (* Build cumulative histograms for O(1) range queries *)
    let cum_hist = Array.make_matrix (src_len + 1) 256 0 in
    for i = 0 to src_len - 1 do
      let c = Char.code (Bytes.get src (src_pos + i)) in
      for j = 0 to 255 do
        cum_hist.(i + 1).(j) <- cum_hist.(i).(j)
      done;
      cum_hist.(i + 1).(c) <- cum_hist.(i + 1).(c) + 1
    done;

    (* Get histogram for range [start, end) *)
    let get_range_histogram start_pos end_pos =
      let hist = create_histogram 256 in
      for j = 0 to 255 do
        hist.data.(j) <- cum_hist.(end_pos).(j) - cum_hist.(start_pos).(j)
      done;
      hist.total <- end_pos - start_pos;
      hist
    in

    (* Compute entropy cost for a block *)
    let block_cost start_pos end_pos =
      if end_pos <= start_pos then 0.0
      else begin
        let hist = get_range_histogram start_pos end_pos in
        histogram_cost hist
      end
    in

    (* DP: find optimal k splits *)
    let n = min (src_len / min_block_size) 32 in (* Candidate positions *)
    if n < 2 then []
    else begin
      let step = src_len / n in
      let positions = Array.init n (fun i -> min ((i + 1) * step) src_len) in

      (* dp.(i).(k) = minimum cost to encode first positions.(i) bytes with k splits *)
      let max_k = min max_splits (n - 1) in
      let dp = Array.make_matrix n (max_k + 1) infinity in
      let parent = Array.make_matrix n (max_k + 1) (-1) in

      (* Base case: no splits *)
      for i = 0 to n - 1 do
        dp.(i).(0) <- block_cost 0 positions.(i)
      done;

      (* Fill DP table *)
      for k = 1 to max_k do
        for i = k to n - 1 do
          for j = k - 1 to i - 1 do
            let prev_cost = dp.(j).(k - 1) in
            let this_block = block_cost positions.(j) positions.(i) in
            let total = prev_cost +. this_block +. 32.0 in (* 32 bits overhead per block *)
            if total < dp.(i).(k) then begin
              dp.(i).(k) <- total;
              parent.(i).(k) <- j
            end
          done
        done
      done;

      (* Find best number of splits for the full input *)
      let last_pos = n - 1 in
      let mutable best_k = 0 in
      let mutable best_cost = dp.(last_pos).(0) in
      for k = 1 to max_k do
        if dp.(last_pos).(k) < best_cost then begin
          best_cost <- dp.(last_pos).(k);
          best_k <- k
        end
      done;

      (* Backtrack to find split positions *)
      let splits = ref [] in
      let rec backtrack i k =
        if k > 0 then begin
          let j = parent.(i).(k) in
          if j >= 0 then begin
            splits := (src_pos + positions.(j)) :: !splits;
            backtrack j (k - 1)
          end
        end
      in
      backtrack last_pos best_k;

      !splits
    end
  end

(* High-level function: find optimal block split points *)
let find_split_points ?(quality=5) src src_pos src_len =
  if quality < 5 || src_len < min_block_size then
    []
  else if quality >= 10 then
    (* Use DP-based splitting for highest quality *)
    find_split_points_dp src src_pos src_len (max_blocks - 1)
  else
    (* Use simpler entropy-based splitting *)
    find_split_points_simple src src_pos src_len

(* Context mode selection for a block *)

(* Score how well a context mode fits the data *)
let score_context_mode mode src src_pos src_len =
  if src_len < 16 then 0.0
  else begin
    (* Create per-context histograms *)
    let num_contexts = 64 in
    let histograms = Array.init num_contexts (fun _ -> create_histogram 256) in

    (* Populate histograms *)
    let mutable prev1 = 0 in
    let mutable prev2 = 0 in
    for i = 0 to src_len - 1 do
      let byte = Char.code (Bytes.get src (src_pos + i)) in
      let context_id = Context.get_context mode ~prev_byte1:prev1 ~prev_byte2:prev2 in
      add_sample histograms.(context_id) byte;
      prev2 <- prev1;
      prev1 <- byte
    done;

    (* Calculate total bits needed with this context mode *)
    let total_bits = Array.fold_left (fun acc h -> acc +. entropy_bits h) 0.0 histograms in

    (* Lower is better, but return negative so higher score = better *)
    -. total_bits
  end

(* Choose the best context mode for a block *)
let choose_context_mode src src_pos src_len =
  if src_len < 32 then
    Context.LSB6 (* Default for small blocks *)
  else begin
    let modes = [| Context.LSB6; Context.MSB6; Context.UTF8; Context.SIGNED |] in
    let mutable best_mode = Context.LSB6 in
    let mutable best_score = neg_infinity in
    for i = 0 to Array.length modes - 1 do
      let mode = modes.(i) in
      let score = score_context_mode mode src src_pos src_len in
      if score > best_score then begin
        best_score <- score;
        best_mode <- mode
      end
    done;
    best_mode
  end

(* Cluster histograms to reduce number of Huffman trees needed *)
type cluster = {
  mutable members : int list;
  mutable histogram : histogram;
}

(* Distance between two histograms (symmetric KL divergence approximation) *)
let histogram_distance h1 h2 =
  if h1.total = 0 || h2.total = 0 then infinity
  else begin
    let t1 = float_of_int h1.total in
    let t2 = float_of_int h2.total in
    let mutable dist = 0.0 in
    for i = 0 to Array.length h1.data - 1 do
      let c1 = float_of_int h1.data.(i) in
      let c2 = float_of_int h2.data.(i) in
      if c1 > 0.0 && c2 > 0.0 then begin
        let p1 = c1 /. t1 in
        let p2 = c2 /. t2 in
        let avg = (p1 +. p2) /. 2.0 in
        (* Jensen-Shannon divergence *)
        dist <- dist +. c1 *. (log (p1 /. avg)) +. c2 *. (log (p2 /. avg))
      end else if c1 > 0.0 || c2 > 0.0 then
        dist <- dist +. 10.0 (* Penalty for mismatched symbols *)
    done;
    dist
  end

(* Cluster context histograms using greedy agglomerative clustering *)
let cluster_histograms histograms max_clusters =
  let n = Array.length histograms in
  if n <= max_clusters then
    (* Each context maps to its own cluster *)
    Array.init n (fun i -> i)
  else begin
    (* Initialize: each histogram is its own cluster *)
    let clusters = Array.init n (fun i ->
      { members = [i]; histogram = histograms.(i) }
    ) in
    let active = Array.make n true in
    let mutable num_active = n in

    (* Merge until we have max_clusters *)
    while num_active > max_clusters do
      (* Find the two closest clusters *)
      let mutable best_i = -1 in
      let mutable best_j = -1 in
      let mutable best_dist = infinity in

      for i = 0 to n - 1 do
        if active.(i) then
          for j = i + 1 to n - 1 do
            if active.(j) then begin
              let dist = histogram_distance clusters.(i).histogram clusters.(j).histogram in
              if dist < best_dist then begin
                best_dist <- dist;
                best_i <- i;
                best_j <- j
              end
            end
          done
      done;

      (* Merge best_j into best_i *)
      if best_i >= 0 && best_j >= 0 then begin
        clusters.(best_i).members <- clusters.(best_j).members @ clusters.(best_i).members;
        clusters.(best_i).histogram <- combine_histograms
          clusters.(best_i).histogram
          clusters.(best_j).histogram;
        active.(best_j) <- false;
        num_active <- num_active - 1
      end else
        num_active <- 0 (* Shouldn't happen, but exit loop *)
    done;

    (* Build context map: context_id -> cluster_id *)
    let context_map = Array.make n 0 in
    let cluster_id = ref 0 in
    for i = 0 to n - 1 do
      if active.(i) then begin
        List.iter (fun ctx -> context_map.(ctx) <- !cluster_id) clusters.(i).members;
        incr cluster_id
      end
    done;

    context_map
  end

(* Build context map for literal encoding *)
let build_literal_context_map mode src src_pos src_len max_trees =
  let num_contexts = 64 in

  (* Build per-context histograms *)
  let histograms = Array.init num_contexts (fun _ -> create_histogram 256) in

  let mutable prev1 = 0 in
  let mutable prev2 = 0 in
  for i = 0 to src_len - 1 do
    let byte = Char.code (Bytes.get src (src_pos + i)) in
    let context_id = Context.get_context mode ~prev_byte1:prev1 ~prev_byte2:prev2 in
    add_sample histograms.(context_id) byte;
    prev2 <- prev1;
    prev1 <- byte
  done;

  (* Cluster histograms *)
  let context_map = cluster_histograms histograms max_trees in

  (* Count actual number of trees used *)
  let max_tree = Array.fold_left max 0 context_map in
  let num_trees = max_tree + 1 in

  (context_map, histograms, num_trees)
