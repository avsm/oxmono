(* LZ77 matching with distance ring buffer support for Brotli *)
(* Implements scoring and parameters matching brotli-c reference *)

(* Configuration *)
let hash_bits = 17
let hash_size = 1 lsl hash_bits
let min_match = 4
let max_match = 258
let window_bits = 22
let max_backward_distance = (1 lsl window_bits) - 16

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
  mutable idx : int;              (* Current index *)
}

let create_dist_ring () = {
  distances = [| 16; 15; 11; 4 |];  (* Initial values per RFC 7932 *)
  idx = 0;
}

let push_distance ring dist =
  ring.distances.(ring.idx land 3) <- dist;
  ring.idx <- ring.idx + 1

let get_last_distance ring n =
  (* n=0: last, n=1: second-to-last, etc. *)
  ring.distances.((ring.idx - 1 - n) land 3)

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
*)
let short_code_distances ring =
  let last = get_last_distance ring 0 in
  let second = get_last_distance ring 1 in
  [|
    get_last_distance ring 0;  (* 0 *)
    get_last_distance ring 1;  (* 1 *)
    get_last_distance ring 2;  (* 2 *)
    get_last_distance ring 3;  (* 3 *)
    last - 1;                  (* 4 *)
    last + 1;                  (* 5 *)
    last - 2;                  (* 6 *)
    last + 2;                  (* 7 *)
    last - 3;                  (* 8 *)
    last + 3;                  (* 9 *)
    second - 1;                (* 10 *)
    second + 1;                (* 11 *)
    second - 2;                (* 12 *)
    second + 2;                (* 13 *)
    second - 3;                (* 14 *)
    second + 3;                (* 15 *)
  |]

(* Find short distance code for a distance, or None if not found.
   Returns the code that requires the fewest extra bits (codes 0-3 are best).

   Short code mapping (RFC 7932):
   - Codes 0-3: exact match to last 4 distances
   - Codes 4-9: last distance +/- 1,2,3
   - Codes 10-15: second-to-last distance +/- 1,2,3 *)
let find_short_code ring distance =
  if distance <= 0 then None
  else
    let last = get_last_distance ring 0 in
    let second = get_last_distance ring 1 in
    (* Build candidate distances for each short code *)
    let candidates = [|
      last;                                         (* 0 *)
      get_last_distance ring 1;                     (* 1 *)
      get_last_distance ring 2;                     (* 2 *)
      get_last_distance ring 3;                     (* 3 *)
      (if last > 1 then last - 1 else -1);          (* 4 *)
      last + 1;                                     (* 5 *)
      (if last > 2 then last - 2 else -1);          (* 6 *)
      last + 2;                                     (* 7 *)
      (if last > 3 then last - 3 else -1);          (* 8 *)
      last + 3;                                     (* 9 *)
      (if second > 1 then second - 1 else -1);      (* 10 *)
      second + 1;                                   (* 11 *)
      (if second > 2 then second - 2 else -1);      (* 12 *)
      second + 2;                                   (* 13 *)
      (if second > 3 then second - 3 else -1);      (* 14 *)
      second + 3;                                   (* 15 *)
    |] in
    (* Find first matching code (lower codes are more efficient) *)
    Array.find_index (fun c -> c = distance) candidates

(* Command type with optional short distance code *)
type command =
  | InsertCopy of {
      lit_start: int;
      lit_len: int;
      copy_len: int;
      distance: int;
      dist_code: int option;  (* Some code for short codes 0-15, None for explicit *)
    }
  | Literals of { start: int; len: int }

(* Hash function - produces 17-bit hash from 4 bytes *)
let[@inline always] hash4 src pos =
  Constants.hash4_bytes src pos hash_bits

(* Find match length *)
let[@inline always] find_match_length src a b limit =
  let len = ref 0 in
  let max_len = min max_match (limit - b) in
  while !len < max_len && Bytes.get src (a + !len) = Bytes.get src (b + !len) do
    incr len
  done;
  !len

(* Log2 floor for non-zero values - matches brotli-c Log2FloorNonZero *)
let[@inline always] log2_floor_nonzero v =
  let rec go v acc = if v <= 1 then acc else go (v lsr 1) (acc + 1) in
  go v 0

(* BackwardReferenceScore from brotli-c (hash.h line 115-118):
   score = SCORE_BASE + LITERAL_BYTE_SCORE * copy_length
           - DISTANCE_BIT_PENALTY * Log2FloorNonZero(backward_reference_offset)
   This prefers longer matches and shorter distances. *)
let backward_reference_score copy_len backward_distance =
  brotli_score_base +
  brotli_literal_byte_score * copy_len -
  brotli_distance_bit_penalty * (log2_floor_nonzero backward_distance)

(* BackwardReferenceScoreUsingLastDistance from brotli-c (hash.h line 121-124):
   score = LITERAL_BYTE_SCORE * copy_length + SCORE_BASE + 15
   Short code 0 (last distance) gets a bonus. *)
let backward_reference_score_using_last_distance copy_len =
  brotli_literal_byte_score * copy_len + brotli_score_base + 15

(* BackwardReferencePenaltyUsingLastDistance from brotli-c (hash.h line 127-129):
   Penalty for short codes 1-15 (not 0): 39 + lookup(distance_short_code)
   The magic constant 0x1CA10 encodes penalties: codes 1-3 get 0, 4-5 get 2, etc. *)
let backward_reference_penalty_using_last_distance distance_short_code =
  39 + ((0x1CA10 lsr (distance_short_code land 0xE)) land 0xE)

(* Score function matching brotli-c exactly *)
let score_match copy_len distance dist_code =
  match dist_code with
  | Some 0 ->
    (* Last distance - use special scoring with bonus *)
    backward_reference_score_using_last_distance copy_len
  | Some code when code < 16 ->
    (* Other short codes - score with last distance bonus minus penalty *)
    let score = backward_reference_score_using_last_distance copy_len in
    score - backward_reference_penalty_using_last_distance code
  | _ ->
    (* Explicit distance - standard scoring *)
    backward_reference_score copy_len distance

(* Insert length code tables *)
let insert_length_offset = [|
  0; 1; 2; 3; 4; 5; 6; 8; 10; 14; 18; 26; 34; 50; 66; 98; 130; 194; 322; 578; 1090; 2114; 6210; 22594
|]

(* Get insert length code *)
let get_insert_code length =
  let rec find i =
    if i >= 23 then 23
    else if length < insert_length_offset.(i + 1) then i
    else find (i + 1)
  in
  find 0

(* Get max copy_len that fits with a given insert_len *)
let max_copy_len_for_insert insert_len =
  let insert_code = get_insert_code insert_len in
  if insert_code >= 16 then 9 else max_match

(* Try to find a match at a short code distance.
   num_to_check controls how many short codes to check (4, 10, or 16 based on quality) *)
let try_short_code_match ?(num_to_check=16) src pos limit ring =
  let candidates = short_code_distances ring in
  let best = ref None in
  let best_score = ref 0 in
  for code = 0 to num_to_check - 1 do
    let dist = candidates.(code) in
    if dist > 0 && pos - dist >= 0 then begin
      let prev = pos - dist in
      let match_len = find_match_length src prev pos limit in
      if match_len >= min_match then begin
        let score = score_match match_len dist (Some code) in
        if score > !best_score then begin
          best := Some (match_len, dist, code);
          best_score := score
        end
      end
    end
  done;
  !best

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
   chain_table_base is the base offset used for chain_table indexing. *)
let find_best_chain_match src pos src_end hash_table chain_table chain_table_base ring
    ~num_last_distances_to_check ~max_chain_depth =
  if pos + min_match > src_end then None
  else begin
    let best_len = ref (min_match - 1) in  (* Start at min_match-1 so >= min_match wins *)
    let best_dist = ref 0 in
    let best_score = ref 0 in
    let best_code = ref None in

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
            let score = score_match match_len dist (Some code) in
            if score > !best_score then begin
              best_len := match_len;
              best_dist := dist;
              best_score := score;
              best_code := Some code
            end
          end
        end
      end
    done;

    (* Second: search hash chain for more matches *)
    let h = hash4 src pos in
    let chain_pos = ref hash_table.(h) in
    let chain_count = ref 0 in

    while !chain_pos >= 0 && !chain_count < max_chain_depth do
      let distance = pos - !chain_pos in
      if distance > 0 && distance <= max_backward_distance then begin
        let match_len = find_match_length src !chain_pos pos src_end in
        if match_len >= min_match then begin
          let dist_code = find_short_code ring distance in
          let score = score_match match_len distance dist_code in
          if score > !best_score then begin
            best_len := match_len;
            best_dist := distance;
            best_score := score;
            best_code := dist_code
          end
        end
      end;
      (* Follow the chain - index relative to base *)
      let chain_idx = !chain_pos - chain_table_base in
      if chain_idx >= 0 && chain_idx < Array.length chain_table then
        chain_pos := chain_table.(chain_idx)
      else
        chain_pos := -1;
      incr chain_count
    done;

    if !best_len >= min_match then
      Some (!best_len, !best_dist, !best_code)
    else
      None
  end

(* Update hash chain. chain_table_base is the base offset for indexing. *)
let update_hash_chain src pos hash_table chain_table chain_table_base =
  let chain_idx = pos - chain_table_base in
  if chain_idx >= 0 && chain_idx < Array.length chain_table && pos + min_match <= Bytes.length src then begin
    let h = hash4 src pos in
    chain_table.(chain_idx) <- hash_table.(h);
    hash_table.(h) <- pos
  end

(* Generate commands with LZ77 matching, dictionary matching, and distance ring buffer.
   Parameters match brotli-c quality-dependent configuration. *)
let generate_commands ?(use_dict=false) ?(quality=2) src src_pos src_len =
  if src_len < min_match then
    [Literals { start = src_pos; len = src_len }]
  else begin
    let commands = ref [] in
    let hash_table = Array.make hash_size (-1) in
    (* Chain table for quality 4+ - each position stores prev position with same hash.
       The table is indexed relative to src_pos. *)
    let chain_table =
      if quality >= 4 then Array.make src_len (-1)
      else [||]  (* Not used for lower qualities *)
    in
    let chain_table_base = src_pos in  (* Base offset for chain_table indexing *)
    let ring = create_dist_ring () in
    let pos = ref src_pos in
    let src_end = src_pos + src_len in
    let pending_start = ref src_pos in
    let output_pos = ref 0 in
    let max_chain_depth = get_max_chain_depth quality in
    let num_last_distances_to_check = get_num_last_distances_to_check quality in

    (* Cost for lazy matching decision - brotli-c uses heuristic thresholds *)
    let lazy_match_cost = if quality >= 4 then 175 else 0 in

    (* Literal spree skip optimization - track consecutive literals without matches *)
    let literal_spree = ref 0 in
    let spree_length = get_literal_spree_length quality in
    let use_spree_skip = quality >= 2 && quality <= 9 in

    while !pos < src_end - min_match do
      (* Determine if we should skip this position due to literal spree *)
      let skip_this_position =
        if use_spree_skip && !literal_spree >= spree_length then begin
          (* In sparse search mode - skip based on spree level *)
          let stride = if !literal_spree >= spree_length * 4 then 16 else 8 in
          let relative_pos = !pos - !pending_start in
          relative_pos mod stride <> 0
        end else false
      in

      if skip_this_position then begin
        (* Still update hash table but with reduced frequency *)
        let hash_update_stride = if !literal_spree >= spree_length * 4 then 4 else 2 in
        let relative_pos = !pos - !pending_start in
        if relative_pos mod hash_update_stride = 0 then begin
          if quality >= 4 then
            update_hash_chain src !pos hash_table chain_table chain_table_base
          else
            hash_table.(hash4 src !pos) <- !pos
        end;
        incr pos;
        incr literal_spree
      end else begin
      (* Find best match at current position *)
      let hash_match =
        if quality >= 4 then
          find_best_chain_match src !pos src_end hash_table chain_table chain_table_base ring
            ~num_last_distances_to_check ~max_chain_depth
        else begin
          (* Q2-3: Simple hash lookup with bucket sweep *)
          let h = hash4 src !pos in
          let prev = hash_table.(h) in
          hash_table.(h) <- !pos;
          (* Also check distance cache first *)
          let short_match = try_short_code_match ~num_to_check:num_last_distances_to_check
            src !pos src_end ring in
          let hash_result =
            if prev >= src_pos && !pos - prev <= max_backward_distance then begin
              let match_len = find_match_length src prev !pos src_end in
              if match_len >= min_match then
                let distance = !pos - prev in
                let dist_code = find_short_code ring distance in
                Some (match_len, distance, dist_code)
              else
                None
            end
            else
              None
          in
          (* Pick best between short code match and hash match *)
          match short_match, hash_result with
          | None, r -> r
          | Some (len, dist, code), None -> Some (len, dist, Some code)
          | Some (slen, sdist, scode), Some (hlen, hdist, hcode) ->
            let s_score = score_match slen sdist (Some scode) in
            let h_score = score_match hlen hdist hcode in
            if s_score >= h_score then Some (slen, sdist, Some scode)
            else Some (hlen, hdist, hcode)
        end
      in

      (* Update hash chain for quality 4+ *)
      if quality >= 4 then
        update_hash_chain src !pos hash_table chain_table chain_table_base;

      (* Try dictionary match if enabled *)
      let dict_match =
        if use_dict then begin
          let pending_lits = !pos - !pending_start in
          let current_output_pos = !output_pos + pending_lits in
          Dict_match.find_match src !pos src_end max_backward_distance ~current_output_pos
        end
        else
          None
      in

      (* Choose the best match based on score *)
      let best_match =
        match hash_match, dict_match with
        | None, None -> None
        | Some m, None -> Some m
        | None, Some (dict_len, dict_dist) ->
          Some (dict_len, dict_dist, None)
        | Some (lz_len, lz_dist, lz_code), Some (dict_len, dict_dist) ->
          let lz_score = score_match lz_len lz_dist lz_code in
          let dict_score = score_dict_match dict_len in
          if dict_score > lz_score then
            Some (dict_len, dict_dist, None)
          else
            Some (lz_len, lz_dist, lz_code)
      in

      match best_match with
      | Some (match_len, distance, dist_code) ->
        (* Lazy matching for quality 4+: check if delaying gives better match *)
        let final_match =
          if quality >= 4 && !pos + 1 < src_end - min_match && match_len < max_match then begin
            (* Update hash for next position *)
            update_hash_chain src (!pos + 1) hash_table chain_table chain_table_base;
            let next_match = find_best_chain_match src (!pos + 1) src_end
              hash_table chain_table chain_table_base ring
              ~num_last_distances_to_check ~max_chain_depth in
            match next_match with
            | Some (next_len, next_dist, next_code) ->
              let curr_score = score_match match_len distance dist_code in
              let next_score = score_match next_len next_dist next_code - lazy_match_cost in
              if next_score > curr_score then begin
                (* Skip current position, emit literal *)
                incr pos;
                pending_start := !pending_start;  (* Keep pending_start *)
                None  (* Signal to continue loop *)
              end else
                Some (match_len, distance, dist_code)
            | None -> Some (match_len, distance, dist_code)
          end else
            Some (match_len, distance, dist_code)
        in

        (match final_match with
        | Some (match_len, distance, dist_code) ->
          let lit_len = !pos - !pending_start in
          let max_copy = max_copy_len_for_insert lit_len in
          let copy_len = min match_len max_copy in

          commands := InsertCopy {
            lit_start = !pending_start;
            lit_len;
            copy_len;
            distance;
            dist_code;
          } :: !commands;

          output_pos := !output_pos + lit_len + copy_len;

          (match dist_code with
           | Some 0 -> ()
           | _ -> push_distance ring distance);

          (* Update hash for all positions in the match for better chain coverage *)
          let hash_update_count =
            if quality >= 10 then min (copy_len - 1) 16
            else if quality >= 4 then min (copy_len - 1) 8
            else min (copy_len - 1) 2 in
          for i = 1 to hash_update_count do
            if !pos + i < src_end - min_match then begin
              if quality >= 4 then
                update_hash_chain src (!pos + i) hash_table chain_table chain_table_base
              else
                hash_table.(hash4 src (!pos + i)) <- !pos + i
            end
          done;

          pos := !pos + copy_len;
          pending_start := !pos;
          (* Reset literal spree counter on match *)
          literal_spree := 0
        | None ->
          (* Lazy match chose to skip, position already incremented *)
          incr literal_spree)
      | None ->
        incr pos;
        incr literal_spree
      end (* end of else begin for skip_this_position *)
    done;

    if !pending_start < src_end then
      commands := Literals { start = !pending_start; len = src_end - !pending_start } :: !commands;

    List.rev !commands
  end
