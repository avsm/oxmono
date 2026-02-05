(* Dictionary matching for Brotli encoder *)

(* Hash table configuration *)
let hash_bits = 17
let hash_size = 1 lsl hash_bits  (* 128K entries *)

(* Packed dictionary entry: pack (word_length, word_idx) into int16#
   - bits 0-10: word_idx (11 bits, max 2047)
   - bits 11-15: word_length - 4 (5 bits, 0-20 for lengths 4-24)
   This allows the whole entry to fit in 16 bits (2 bytes per entry). *)
let[@inline always] pack_entry word_length word_idx : int16# =
  Stdlib_stable.Int16_u.of_int (((word_length - 4) lsl 11) lor word_idx)

let[@inline always] unpack_length (packed : int16#) =
  (Stdlib_stable.Int16_u.to_int packed lsr 11) + 4

let[@inline always] unpack_idx (packed : int16#) =
  Stdlib_stable.Int16_u.to_int packed land 0x7FF

(* Flat hash table structure:
   - entries: int16# array of all packed (length, idx) pairs (2 bytes each)
   - bucket_info: int array where each entry is (offset << 8) | count
     (offset is the starting index in entries, count is number of entries)
   This avoids allocating lists and tuples per bucket. *)
type dict_table = {
  entries : int16# array;
  bucket_info : int array;
}

(* Build the dictionary hash table lazily *)
let dict_hash_table : dict_table Lazy.t = lazy (
  (* First pass: count entries per bucket *)
  let counts = Array.make hash_size 0 in
  let mutable total_entries = 0 in
  for length = Dictionary.min_word_length to Dictionary.max_word_length do
    let num_words = 1 lsl Dictionary.get_size_bits_by_length length in
    for word_idx = 0 to num_words - 1 do
      let offset = Dictionary.get_offset_by_length length + word_idx * length in
      if offset + 4 <= String.length Dictionary.data then begin
        let h = Constants.hash4_string Dictionary.data offset hash_bits in
        counts.(h) <- counts.(h) + 1;
        total_entries <- total_entries + 1
      end
    done
  done;

  (* Compute bucket offsets using a running sum and create packed bucket_info *)
  let bucket_info = Array.make hash_size 0 in
  let mutable running_offset = 0 in
  for i = 0 to hash_size - 1 do
    bucket_info.(i) <- (running_offset lsl 8) lor counts.(i);
    running_offset <- running_offset + counts.(i)
  done;

  (* Allocate entries array as int16# - 2 bytes per entry *)
  let entries : int16# array = Base.Array.create ~len:total_entries #0S in

  (* Reset counts for second pass *)
  let write_pos = Array.make hash_size 0 in

  (* Second pass: fill entries *)
  for length = Dictionary.min_word_length to Dictionary.max_word_length do
    let num_words = 1 lsl Dictionary.get_size_bits_by_length length in
    for word_idx = 0 to num_words - 1 do
      let offset = Dictionary.get_offset_by_length length + word_idx * length in
      if offset + 4 <= String.length Dictionary.data then begin
        let h = Constants.hash4_string Dictionary.data offset hash_bits in
        let info = bucket_info.(h) in
        let bucket_offset = info lsr 8 in
        let entry_pos = bucket_offset + write_pos.(h) in
        Oxcaml_arrays.unsafe_set entries entry_pos (pack_entry length word_idx);
        write_pos.(h) <- write_pos.(h) + 1
      end
    done
  done;

  { entries; bucket_info }
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
    let offset = Dictionary.get_offset_by_length word_length + word_idx * word_length in
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
    let offset = Dictionary.get_offset_by_length word_length + word_idx * word_length in
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
    let info = table.bucket_info.(h) in
    let bucket_offset = info lsr 8 in
    let count = info land 0xFF in

    let mutable best = None in
    let mutable best_score = 0 in

    for i = 0 to count - 1 do
      let packed = Oxcaml_arrays.unsafe_get table.entries (bucket_offset + i) in
      let word_length = unpack_length packed in
      let word_idx = unpack_idx packed in

      (* Try identity transform first (most common) *)
      (match match_identity src pos src_end word_length word_idx with
       | Some (len, transform_id) ->
         (* Score: longer matches are better, identity transform is preferred *)
         let score = len * 10 in
         if score > best_score then begin
           best <- Some (len, word_idx, transform_id);
           best_score <- score
         end
       | None -> ());

      (* Try uppercase-first transform for capitalized words *)
      if word_length >= 1 then
        (match match_uppercase_first src pos src_end word_length word_idx with
         | Some (len, transform_id) ->
           let score = len * 10 - 1 in  (* Slight penalty for transform *)
           if score > best_score then begin
             best <- Some (len, word_idx, transform_id);
             best_score <- score
           end
         | None -> ())
    done;

    match best with
    | None -> None
    | Some (match_len, word_idx, transform_id) ->
      (* Calculate the dictionary distance code.
         The decoder uses: word_id = distance - max_distance - 1
         where max_distance = min(max_backward_distance, output_pos)
         So we must use the same formula in reverse. *)
      let max_distance = min max_backward_distance current_output_pos in
      let shift = Dictionary.get_size_bits_by_length match_len in
      let word_id = word_idx lor (transform_id lsl shift) in
      let distance = max_distance + 1 + word_id in
      Some (match_len, distance)
  end

(* Score a dictionary match for comparison with LZ77 matches *)
let score_dict_match match_len =
  (* Dictionary matches save literals but have longer distance encoding *)
  (* Give them a bonus since they're "free" (no backward reference needed) *)
  match_len * 140  (* Slightly higher than LZ77's base score of 135 *)
