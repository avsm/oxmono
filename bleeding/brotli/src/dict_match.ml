(* Dictionary matching for Brotli encoder *)

(* Hash table configuration *)
let hash_bits = 17
let hash_size = 1 lsl hash_bits  (* 128K entries *)

(* Dictionary hash table entry: (word_length, word_index) list *)
type dict_entry = (int * int) list

(* Build the dictionary hash table lazily *)
let dict_hash_table : dict_entry array Lazy.t = lazy (
  let table = Array.make hash_size [] in
  (* Index all dictionary words by their first 4 bytes *)
  for length = Dictionary.min_word_length to Dictionary.max_word_length do
    let num_words = 1 lsl Dictionary.size_bits_by_length.(length) in
    for word_idx = 0 to num_words - 1 do
      let offset = Dictionary.offset_by_length.(length) + word_idx * length in
      if offset + 4 <= String.length Dictionary.data then begin
        let h = Constants.hash4_string Dictionary.data offset hash_bits in
        table.(h) <- (length, word_idx) :: table.(h)
      end
    done
  done;
  table
)

(* Check if two byte sequences match *)
let[@inline] bytes_match src src_pos word word_pos len =
  let rec loop i =
    if i >= len then true
    else if Bytes.get src (src_pos + i) <> word.[word_pos + i] then false
    else loop (i + 1)
  in
  loop 0

(* Transform ID 0: Identity - no transformation *)
(* Transform ID 9: UppercaseFirst - uppercase first letter *)
(* Transform ID 44: UppercaseAll - uppercase all letters *)

(* Check if input matches word with identity transform (ID 0) *)
let match_identity src pos src_end word_length word_idx =
  if pos + word_length > src_end then None
  else begin
    let offset = Dictionary.offset_by_length.(word_length) + word_idx * word_length in
    if bytes_match src pos Dictionary.data offset word_length then
      Some (word_length, 0)  (* length, transform_id *)
    else
      None
  end

(* Lowercase a character if uppercase *)
let[@inline] to_lower c =
  if c >= 'A' && c <= 'Z' then Char.chr (Char.code c lor 32)
  else c

(* Check if input matches word with uppercase-first transform (ID 9) *)
let match_uppercase_first src pos src_end word_length word_idx =
  if pos + word_length > src_end || word_length < 1 then None
  else begin
    let offset = Dictionary.offset_by_length.(word_length) + word_idx * word_length in
    (* First byte should be uppercase version of dictionary's first byte *)
    let dict_first = Dictionary.data.[offset] in
    let src_first = Bytes.get src pos in
    if src_first >= 'A' && src_first <= 'Z' && to_lower src_first = dict_first then begin
      (* Rest should match exactly *)
      if word_length = 1 || bytes_match src (pos + 1) Dictionary.data (offset + 1) (word_length - 1) then
        Some (word_length, 9)  (* length, transform_id *)
      else
        None
    end
    else
      None
  end

(* Try to find a dictionary match at the given position.
   current_output_pos is the current position in the output buffer (for distance calculation).
   The decoder uses min(max_backward_distance, output_pos) as the base for dictionary references. *)
let find_match src pos src_end max_backward_distance ~current_output_pos =
  if pos + 4 > src_end then None
  else begin
    let table = Lazy.force dict_hash_table in
    let h = Constants.hash4_bytes src pos hash_bits in
    let candidates = table.(h) in

    let best = ref None in
    let best_score = ref 0 in

    List.iter (fun (word_length, word_idx) ->
      (* Try identity transform first (most common) *)
      (match match_identity src pos src_end word_length word_idx with
       | Some (len, transform_id) ->
         (* Score: longer matches are better, identity transform is preferred *)
         let score = len * 10 in
         if score > !best_score then begin
           best := Some (len, word_idx, transform_id);
           best_score := score
         end
       | None -> ());

      (* Try uppercase-first transform for capitalized words *)
      if word_length >= 1 then
        (match match_uppercase_first src pos src_end word_length word_idx with
         | Some (len, transform_id) ->
           let score = len * 10 - 1 in  (* Slight penalty for transform *)
           if score > !best_score then begin
             best := Some (len, word_idx, transform_id);
             best_score := score
           end
         | None -> ())
    ) candidates;

    match !best with
    | None -> None
    | Some (match_len, word_idx, transform_id) ->
      (* Calculate the dictionary distance code.
         The decoder uses: word_id = distance - max_distance - 1
         where max_distance = min(max_backward_distance, output_pos)
         So we must use the same formula in reverse. *)
      let max_distance = min max_backward_distance current_output_pos in
      let shift = Dictionary.size_bits_by_length.(match_len) in
      let word_id = word_idx lor (transform_id lsl shift) in
      let distance = max_distance + 1 + word_id in
      Some (match_len, distance)
  end

(* Score a dictionary match for comparison with LZ77 matches *)
let score_dict_match match_len =
  (* Dictionary matches save literals but have longer distance encoding *)
  (* Give them a bonus since they're "free" (no backward reference needed) *)
  match_len * 140  (* Slightly higher than LZ77's base score of 135 *)
