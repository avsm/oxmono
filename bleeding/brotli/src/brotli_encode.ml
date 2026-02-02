(* Brotli compression implementation *)
(* Supports quality levels 0-11 with context modeling, block splitting, and optimal parsing *)

(* Re-export from LZ77 for backward compatibility *)
let min_match = Lz77.min_match

(* Number of literal contexts *)
let num_literal_contexts = 64

(* Insert length code tables *)
let insert_length_n_bits = [|
  0; 0; 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 7; 8; 9; 10; 12; 14; 24
|]

let insert_length_offset = [|
  0; 1; 2; 3; 4; 5; 6; 8; 10; 14; 18; 26; 34; 50; 66; 98; 130; 194; 322; 578; 1090; 2114; 6210; 22594
|]

(* Get insert length code *)
let[@inline always] get_insert_code length =
  let rec find i =
    if i >= 23 then 23
    else if length < insert_length_offset.(i + 1) then i
    else find (i + 1)
  in
  find 0

(* Get copy length code *)
let[@inline always] get_copy_code length =
  let copy_length_offset = [|
    2; 3; 4; 5; 6; 7; 8; 9; 10; 12; 14; 18; 22; 30; 38; 54; 70; 102; 134; 198; 326; 582; 1094; 2118
  |] in
  let rec find i =
    if i >= 23 then 23
    else if length < copy_length_offset.(i + 1) then i
    else find (i + 1)
  in
  find 0

(* Command code lookup tables from RFC 7932 *)
let insert_range_lut = [| 0; 0; 8; 8; 0; 16; 8; 16; 16 |]
let copy_range_lut = [| 0; 8; 0; 8; 16; 0; 16; 8; 16 |]

(* Build command code from insert_code and copy_code.
   use_implicit_distance: true ONLY for distance code 0 (last distance)

   Per RFC 7932, command codes have range_idx in bits 7-6:
   - range_idx 0-1 (cmd_code 0-127): Distance code 0 is IMPLICIT (not read from stream)
     The decoder automatically uses distance code 0 (last used distance).
   - range_idx 2-8 (cmd_code 128+): Distance code is EXPLICIT (read from stream)
     Short codes 0-15 and long codes >= 16 are all written explicitly.

   IMPORTANT: Only dist_code=Some 0 can use implicit distance (range_idx 0-1).
   For all other short codes (1-15), we must use explicit distance (range_idx >= 2).
*)
exception Found_cmd of int

let get_command_code insert_code copy_code use_implicit_distance =
  try
    (* Only use range_idx 0-1 for implicit distance code 0 *)
    if use_implicit_distance then begin
      for r = 0 to 1 do
        let insert_base = insert_range_lut.(r) in
        let copy_base = copy_range_lut.(r) in
        let insert_delta = insert_code - insert_base in
        let copy_delta = copy_code - copy_base in
        if insert_delta >= 0 && insert_delta < 8 &&
           copy_delta >= 0 && copy_delta < 8 then begin
          let cmd_code = (r lsl 6) lor (insert_delta lsl 3) lor copy_delta in
          raise (Found_cmd cmd_code)
        end
      done
    end;

    (* Use range_idx 2-8 for explicit distance (including short codes 0-15) *)
    for r = 2 to 8 do
      let adjusted_r = r - 2 in
      let insert_base = insert_range_lut.(adjusted_r) in
      let copy_base = copy_range_lut.(adjusted_r) in
      let insert_delta = insert_code - insert_base in
      let copy_delta = copy_code - copy_base in
      if insert_delta >= 0 && insert_delta < 8 &&
         copy_delta >= 0 && copy_delta < 8 then begin
        let cmd_code = (r lsl 6) lor (insert_delta lsl 3) lor copy_delta in
        raise (Found_cmd cmd_code)
      end
    done;

    (* Fallback - shouldn't happen if LZ77 limits copy_len properly *)
    let insert_delta = min insert_code 7 in
    let copy_delta = min copy_code 7 in
    (2 lsl 6) lor (insert_delta lsl 3) lor copy_delta
  with Found_cmd cmd_code -> cmd_code

(* Encode window bits *)
let encode_window_bits bw =
  Bit_writer.write_bits bw 1 1;
  Bit_writer.write_bits bw 3 5  (* 22-bit window *)

(* Write empty last block *)
let write_empty_last_block bw =
  Bit_writer.write_bits bw 1 1;
  Bit_writer.write_bits bw 1 1

(* Write meta-block header *)
let write_meta_block_header bw length is_last is_uncompressed =
  Bit_writer.write_bits bw 1 (if is_last then 1 else 0);
  if is_last then
    Bit_writer.write_bits bw 1 0;  (* ISEMPTY = 0 for non-empty last block *)
  let nibbles = if length - 1 < (1 lsl 16) then 4 else if length - 1 < (1 lsl 20) then 5 else 6 in
  Bit_writer.write_bits bw 2 (nibbles - 4);
  for i = 0 to nibbles - 1 do
    Bit_writer.write_bits bw 4 (((length - 1) lsr (i * 4)) land 0xF)
  done;
  if not is_last then
    Bit_writer.write_bits bw 1 (if is_uncompressed then 1 else 0)

(* Write uncompressed block *)
let write_uncompressed_block bw src src_pos length =
  write_meta_block_header bw length false true;
  Bit_writer.align_to_byte bw;
  Bit_writer.copy_bytes bw ~src ~src_pos ~len:length

(* Count bits needed to represent values 0 to n-1 (ceiling of log2(n)) *)
let[@inline always] count_bits n =
  if n <= 1 then 0
  else
    let rec count v b = if v = 0 then b else count (v lsr 1) (b + 1) in
    count (n - 1) 0

(* Write simple prefix code - 1 to 4 symbols *)
let write_simple_prefix_code bw symbols alphabet_size =
  let n = Array.length symbols in
  Bit_writer.write_bits bw 2 1;  (* HSKIP = 1 means simple code *)
  Bit_writer.write_bits bw 2 (n - 1);  (* NSYM - 1 *)
  let bits = count_bits (alphabet_size - 1) in
  for i = 0 to n - 1 do
    Bit_writer.write_bits bw bits symbols.(i)
  done;
  if n = 4 then Bit_writer.write_bits bw 1 0

(* Static Huffman code for code lengths *)
let write_code_length_symbol bw len =
  match len with
  | 0 -> Bit_writer.write_bits bw 2 0
  | 1 -> Bit_writer.write_bits bw 4 7
  | 2 -> Bit_writer.write_bits bw 3 3
  | 3 -> Bit_writer.write_bits bw 2 2
  | 4 -> Bit_writer.write_bits bw 2 1
  | 5 -> Bit_writer.write_bits bw 4 15
  | _ -> Bit_writer.write_bits bw 2 0

(* Build valid Huffman code lengths using Kraft inequality *)
let build_valid_code_lengths freqs max_len =
  let n = Array.length freqs in
  let lengths = Array.make n 0 in
  (* Note: symbols ref captures into closure, cannot be local *)
  let symbols = ref [] in
  for i = n - 1 downto 0 do
    if freqs.(i) > 0 then
      symbols := (freqs.(i), i) :: !symbols
  done;
  let num_symbols = List.length !symbols in
  if num_symbols = 0 then lengths
  else if num_symbols = 1 then begin
    let (_, sym) = List.hd !symbols in
    lengths.(sym) <- 1;
    lengths
  end
  else begin
    let sorted = List.sort (fun (f1, _) (f2, _) -> compare f2 f1) !symbols in
    let bits_needed = count_bits num_symbols in
    let base_len = min max_len (max bits_needed 1) in
    (* Note: these refs are captured by closure, cannot be local *)
    let len_to_use = ref base_len in
    while (1 lsl !len_to_use) < num_symbols && !len_to_use < max_len do
      incr len_to_use
    done;
    let slots_used = ref num_symbols in
    let total_slots = 1 lsl !len_to_use in
    List.iter (fun (_, sym) ->
      let extra_slots = total_slots - !slots_used in
      if extra_slots > 0 && !len_to_use > 1 then begin
        let shorter_len = !len_to_use - 1 in
        let extra_needed = (1 lsl (!len_to_use - shorter_len)) - 1 in
        if extra_slots >= extra_needed then begin
          lengths.(sym) <- shorter_len;
          slots_used := !slots_used + extra_needed
        end else
          lengths.(sym) <- !len_to_use
      end else
        lengths.(sym) <- !len_to_use
    ) sorted;
    lengths
  end

(* Build canonical Huffman codes from lengths *)
let build_codes lengths =
  let n = Array.length lengths in
  let codes = Array.make n 0 in
  let max_len = Array.fold_left max 0 lengths in
  if max_len = 0 then codes
  else begin
    (* Note: bl_count captured by Array.iter closure, cannot be local *)
    let bl_count = Array.make (max_len + 1) 0 in
    Array.iter (fun l -> if l > 0 then bl_count.(l) <- bl_count.(l) + 1) lengths;
    (* Stack-allocate temporary arrays and refs used only in for loops *)
    let local_ next_code = Array.make (max_len + 1) 0 in
    let local_ code = ref 0 in
    for bits = 1 to max_len do
      code := (!code + bl_count.(bits - 1)) lsl 1;
      next_code.(bits) <- !code
    done;
    for i = 0 to n - 1 do
      let len = lengths.(i) in
      if len > 0 then begin
        codes.(i) <- next_code.(len);
        next_code.(len) <- next_code.(len) + 1
      end
    done;
    codes
  end

(* Reverse bits for canonical Huffman *)
let[@inline always] reverse_bits v n =
  (* Stack-allocate temporary refs that don't escape *)
  let local_ r = ref 0 in
  let local_ v = ref v in
  for _ = 0 to n - 1 do
    r := (!r lsl 1) lor (!v land 1);
    v := !v lsr 1
  done;
  !r

(* Write a Huffman symbol *)
let[@inline always] write_symbol bw codes lengths sym =
  let len = lengths.(sym) in
  if len > 0 then
    Bit_writer.write_bits bw len (reverse_bits codes.(sym) len)

(* RLE encoding for code lengths *)
let emit_zeros_rle symbols_ref extras_ref total_ref run_len =
  if run_len < 3 then begin
    for _ = 1 to run_len do
      symbols_ref := 0 :: !symbols_ref;
      extras_ref := 0 :: !extras_ref;
      incr total_ref
    done
  end else begin
    (* Stack-allocate temporary ref that doesn't escape *)
    let local_ reps = ref (run_len - 3) in
    let rec build acc_codes acc_extras =
      let e = !reps land 7 in
      reps := !reps lsr 3;
      if !reps = 0 then
        (17 :: acc_codes, e :: acc_extras)
      else begin
        reps := !reps - 1;
        build (17 :: acc_codes) (e :: acc_extras)
      end
    in
    let (codes, extras) = build [] [] in
    List.iter2 (fun c e ->
      symbols_ref := c :: !symbols_ref;
      extras_ref := e :: !extras_ref
    ) codes extras;
    total_ref := !total_ref + run_len
  end

let emit_nonzero_rle symbols_ref extras_ref total_ref run_len prev_value_ref value =
  (* Stack-allocate temporary ref that doesn't escape *)
  let local_ to_write = ref run_len in
  if !prev_value_ref <> value then begin
    symbols_ref := value :: !symbols_ref;
    extras_ref := 0 :: !extras_ref;
    prev_value_ref := value;
    decr to_write;
    incr total_ref
  end;
  if !to_write < 3 then begin
    for _ = 1 to !to_write do
      symbols_ref := value :: !symbols_ref;
      extras_ref := 0 :: !extras_ref
    done;
    total_ref := !total_ref + !to_write
  end else begin
    (* Stack-allocate temporary ref that doesn't escape *)
    let local_ reps = ref (!to_write - 3) in
    let rec build acc_codes acc_extras =
      let e = !reps land 3 in
      reps := !reps lsr 2;
      if !reps = 0 then
        (16 :: acc_codes, e :: acc_extras)
      else begin
        reps := !reps - 1;
        build (16 :: acc_codes) (e :: acc_extras)
      end
    in
    let (codes, extras) = build [] [] in
    List.iter2 (fun c e ->
      symbols_ref := c :: !symbols_ref;
      extras_ref := e :: !extras_ref
    ) codes extras;
    total_ref := !total_ref + !to_write
  end

let generate_rle_sequence lengths num_symbols =
  (* Note: these refs are passed to helper functions, cannot be local *)
  let symbols = ref [] in
  let extras = ref [] in
  let prev_value = ref 8 in
  let total = ref 0 in
  let local_ i = ref 0 in
  while !i < num_symbols do
    let value = if !i < Array.length lengths then lengths.(!i) else 0 in
    let run_start = !i in
    while !i < num_symbols &&
          (if !i < Array.length lengths then lengths.(!i) else 0) = value do
      incr i
    done;
    let run_len = !i - run_start in
    if value = 0 then
      emit_zeros_rle symbols extras total run_len
    else
      emit_nonzero_rle symbols extras total run_len prev_value value
  done;
  let syms = Array.of_list (List.rev !symbols) in
  let exts = Array.of_list (List.rev !extras) in
  (syms, exts)

(* Write complex prefix code with RLE encoding *)
let write_complex_prefix_code bw lengths alphabet_size =
  (* Stack-allocate temporary ref that doesn't escape *)
  let local_ last_nonzero = ref (-1) in
  for i = 0 to min (alphabet_size - 1) (Array.length lengths - 1) do
    if lengths.(i) > 0 then last_nonzero := i
  done;
  let num_symbols = !last_nonzero + 1 in
  let (rle_symbols, rle_extra) = generate_rle_sequence lengths num_symbols in
  (* Note: cl_histogram captured by Array.iter closure, cannot be local *)
  let cl_histogram = Array.make Constants.code_length_codes 0 in
  Array.iter (fun sym -> cl_histogram.(sym) <- cl_histogram.(sym) + 1) rle_symbols;
  let cl_depths = build_valid_code_lengths cl_histogram Constants.huffman_max_code_length_code_length in
  let local_ num_codes = ref 0 in
  for i = 0 to Constants.code_length_codes - 1 do
    if cl_histogram.(i) > 0 then incr num_codes
  done;
  let skip_some =
    if cl_depths.(Constants.code_length_code_order.(0)) = 0 &&
       cl_depths.(Constants.code_length_code_order.(1)) = 0 then
      if cl_depths.(Constants.code_length_code_order.(2)) = 0 then 3
      else 2
    else 0
  in
  let local_ codes_to_store = ref Constants.code_length_codes in
  if !num_codes > 1 then begin
    while !codes_to_store > 0 &&
          cl_depths.(Constants.code_length_code_order.(!codes_to_store - 1)) = 0 do
      decr codes_to_store
    done
  end;
  Bit_writer.write_bits bw 2 skip_some;
  let local_ space = ref 32 in
  for i = skip_some to !codes_to_store - 1 do
    if !space > 0 then begin
      let idx = Constants.code_length_code_order.(i) in
      let l = cl_depths.(idx) in
      write_code_length_symbol bw l;
      if l <> 0 then
        space := !space - (32 lsr l)
    end
  done;
  let cl_codes = build_codes cl_depths in
  for i = 0 to Array.length rle_symbols - 1 do
    let sym = rle_symbols.(i) in
    if !num_codes > 1 then
      write_symbol bw cl_codes cl_depths sym;
    if sym = 16 then
      Bit_writer.write_bits bw 2 rle_extra.(i)
    else if sym = 17 then
      Bit_writer.write_bits bw 3 rle_extra.(i)
  done

(* Write Huffman code definition - choose simple or complex *)
let write_huffman_code bw lengths alphabet_size =
  (* Stack-allocate temporary ref that doesn't escape *)
  let local_ symbols = ref [] in
  for i = 0 to min (alphabet_size - 1) (Array.length lengths - 1) do
    if i < Array.length lengths && lengths.(i) > 0 then
      symbols := (i, lengths.(i)) :: !symbols
  done;
  let sorted = List.sort (fun (s1, l1) (s2, l2) ->
    let c = compare l1 l2 in
    if c <> 0 then c else compare s1 s2
  ) !symbols in
  let symbols = Array.of_list (List.map fst sorted) in
  let num_symbols = Array.length symbols in
  if num_symbols = 0 then
    write_simple_prefix_code bw [|0|] alphabet_size
  else if num_symbols <= 4 then
    write_simple_prefix_code bw symbols alphabet_size
  else
    write_complex_prefix_code bw lengths alphabet_size

(* Count used symbols in frequency array *)
let count_used_symbols freqs =
  (* Note: count captured by Array.iter closure, cannot be local *)
  let count = ref 0 in
  Array.iter (fun f -> if f > 0 then incr count) freqs;
  !count

(* Write context map using RLE and IMTF encoding *)
(* Encode a variable length uint8 (matches decode_var_len_uint8 in decoder) *)
let write_var_len_uint8 bw n =
  if n = 0 then
    Bit_writer.write_bits bw 1 0
  else if n = 1 then begin
    Bit_writer.write_bits bw 1 1;
    Bit_writer.write_bits bw 3 0  (* nbits = 0 means value 1 *)
  end else begin
    Bit_writer.write_bits bw 1 1;
    (* Find nbits such that (1 << nbits) <= n < (1 << (nbits + 1)) *)
    let rec find_nbits nb =
      if n < (1 lsl (nb + 1)) then nb
      else find_nbits (nb + 1)
    in
    let nbits = find_nbits 1 in
    Bit_writer.write_bits bw 3 nbits;
    Bit_writer.write_bits bw nbits (n - (1 lsl nbits))
  end

let write_context_map bw context_map num_trees =
  (* Write NTREES - 1 using variable length encoding *)
  write_var_len_uint8 bw (num_trees - 1);

  if num_trees > 1 then begin
    (* Write RLEMAX flag: 0 = no RLE *)
    Bit_writer.write_bits bw 1 0;

    (* With RLEMAX=0, alphabet size is just num_trees, symbols are values directly *)
    let map_len = Array.length context_map in
    (* Note: freq is passed to count_used_symbols, cannot be local *)
    let freq = Array.make num_trees 0 in
    for i = 0 to map_len - 1 do
      freq.(context_map.(i)) <- freq.(context_map.(i)) + 1
    done;

    (* Build Huffman code for context map values *)
    let lengths = build_valid_code_lengths freq 15 in
    let codes = build_codes lengths in

    (* Write the Huffman code for num_trees symbols *)
    write_huffman_code bw lengths num_trees;

    (* Write the context map values *)
    let num_symbols = count_used_symbols freq in
    for i = 0 to map_len - 1 do
      if num_symbols > 1 then
        write_symbol bw codes lengths context_map.(i)
    done;

    (* Write IMTF flag: 0 = no inverse move-to-front *)
    Bit_writer.write_bits bw 1 0
  end

(* Copy length extra bits table *)
let copy_length_n_bits = [|
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 7; 8; 9; 10; 24
|]

let copy_length_offset = [|
  2; 3; 4; 5; 6; 7; 8; 9; 10; 12; 14; 18; 22; 30; 38; 54; 70; 102; 134; 198; 326; 582; 1094; 2118
|]

(* Encode distance for NPOSTFIX=0, NDIRECT=0 *)
let[@inline always] encode_distance distance =
  if distance < 1 then
    (16, 1, 0)
  else begin
    let d = distance - 1 in
    (* Stack-allocate temporary refs that don't escape *)
    let local_ nbits = ref 1 in
    let local_ range_start = ref 0 in
    while d >= !range_start + (1 lsl (!nbits + 1)) && !nbits < 24 do
      range_start := !range_start + (1 lsl (!nbits + 1));
      incr nbits
    done;
    let half_size = 1 lsl !nbits in
    let d_in_range = d - !range_start in
    let lcode = if d_in_range >= half_size then 1 else 0 in
    let dc = 2 * (!nbits - 1) + lcode in
    let code = 16 + dc in
    let extra = d_in_range - (lcode * half_size) in
    (code, !nbits, extra)
  end

(* Quality level for dictionary matching *)
let current_quality = ref 1

(* Prepared command type for single-pass encoding.
   Stores all precomputed values to avoid recomputing during encoding pass.
   OPTIMIZED: Uses indices into source and shared tree_ids buffer instead of per-command arrays. *)
type prepared_cmd_context = {
  cmd_code: int;
  insert_code: int;
  copy_code: int;
  range_idx: int;
  insert_extra: int;
  copy_extra: int;
  (* Literals reference ranges in source and tree_ids buffer *)
  lit_src_start: int;  (* Start index in source buffer *)
  lit_len: int;        (* Number of literals *)
  tree_ids_start: int; (* Start index in shared tree_ids buffer *)
  (* Distance info for InsertCopy *)
  dist_info: (int * int * int * int) option;  (* (dist_tree, dist_code, nbits, extra) *)
}

(* Prepared command type for simple encoding without context (quality < 5).
   Uses indices into source buffer instead of per-command arrays. *)
type prepared_cmd_simple = {
  s_cmd_code: int;
  s_insert_code: int;
  s_copy_code: int;
  s_insert_extra: int;
  s_copy_extra: int;
  s_lit_start: int;   (* Start index in source buffer *)
  s_lit_len: int;     (* Number of literals *)
  s_dist_info: (int * int * int) option;  (* (dist_code_val, nbits, extra) *)
}

(* Write a compressed block with context modeling for quality >= 5 *)
(* OPTIMIZED: Single-pass frequency counting with shared tree_ids buffer - no per-command array allocations *)
let write_compressed_block_with_context bw src _src_pos src_len is_last context_mode context_map num_lit_trees num_dist_trees dist_context_map commands =
  let num_distance_codes = 16 + 48 in

  (* Initialize frequency arrays *)
  let lit_freqs = Array.init num_lit_trees (fun _ -> Array.make 256 0) in
  let cmd_freq = Array.make 704 0 in
  let dist_freqs = Array.init num_dist_trees (fun _ -> Array.make num_distance_codes 0) in

  (* Shared buffer for tree IDs - allocated once, sized to fit all literals *)
  let tree_ids = Array.make src_len 0 in
  let tree_ids_pos = ref 0 in

  (* Track previous bytes for context calculation *)
  let prev1 = ref 0 in
  let prev2 = ref 0 in

  (* Helper to get distance code value *)
  let get_dist_code_val dist_code distance =
    if dist_code >= 0 then (dist_code, 0, 0)
    else
      let dist_code_val, nbits, extra = encode_distance distance in
      (min dist_code_val (num_distance_codes - 1), nbits, extra)
  in

  (* SINGLE PASS: Prepare commands, count frequencies, store tree IDs in shared buffer *)
  let total_len = ref 0 in
  let prepared_commands = List.map (fun cmd ->
    match cmd with
    | Lz77.Literals { start; len } ->
      total_len := !total_len + len;
      let tree_start = !tree_ids_pos in
      (* Count frequencies and store tree IDs - read literals from source *)
      for idx = 0 to len - 1 do
        let c = Char.code (Bytes.get src (start + idx)) in
        let ctx_id = Context.get_context context_mode ~prev_byte1:!prev1 ~prev_byte2:!prev2 in
        let tree_id = context_map.(ctx_id) in
        lit_freqs.(tree_id).(c) <- lit_freqs.(tree_id).(c) + 1;
        tree_ids.(!tree_ids_pos) <- tree_id;
        incr tree_ids_pos;
        prev2 := !prev1;
        prev1 := c
      done;
      let insert_code = get_insert_code len in
      let copy_code = 0 in
      let cmd_code = get_command_code insert_code copy_code false in
      let range_idx = cmd_code lsr 6 in
      cmd_freq.(cmd_code) <- cmd_freq.(cmd_code) + 1;
      (* Literals command always has range_idx >= 2, needs distance *)
      let dist_tree = dist_context_map.(0) in
      dist_freqs.(dist_tree).(0) <- dist_freqs.(dist_tree).(0) + 1;
      let insert_extra = if insert_length_n_bits.(insert_code) > 0
        then len - insert_length_offset.(insert_code) else 0 in
      {
        cmd_code; insert_code; copy_code; range_idx;
        insert_extra; copy_extra = 0;
        lit_src_start = start; lit_len = len; tree_ids_start = tree_start;
        dist_info = None;  (* Literals command uses implicit dist 0 *)
      }
    | Lz77.InsertCopy { lit_start; lit_len; copy_len; distance; dist_code } ->
      total_len := !total_len + lit_len + copy_len;
      let tree_start = !tree_ids_pos in
      (* Count frequencies and store tree IDs - read literals from source *)
      for idx = 0 to lit_len - 1 do
        let c = Char.code (Bytes.get src (lit_start + idx)) in
        let ctx_id = Context.get_context context_mode ~prev_byte1:!prev1 ~prev_byte2:!prev2 in
        let tree_id = context_map.(ctx_id) in
        lit_freqs.(tree_id).(c) <- lit_freqs.(tree_id).(c) + 1;
        tree_ids.(!tree_ids_pos) <- tree_id;
        incr tree_ids_pos;
        prev2 := !prev1;
        prev1 := c
      done;
      let insert_code = get_insert_code lit_len in
      let copy_code = get_copy_code copy_len in
      let use_implicit = dist_code = 0 in
      let cmd_code = get_command_code insert_code copy_code use_implicit in
      let range_idx = cmd_code lsr 6 in
      cmd_freq.(cmd_code) <- cmd_freq.(cmd_code) + 1;
      (* Compute distance info if needed *)
      let dist_info = if range_idx >= 2 then begin
        let dist_ctx = Context.distance_context copy_len in
        let dist_tree = dist_context_map.(dist_ctx) in
        let (code_val, nbits, extra) = get_dist_code_val dist_code distance in
        dist_freqs.(dist_tree).(code_val) <- dist_freqs.(dist_tree).(code_val) + 1;
        Some (dist_tree, code_val, nbits, extra)
      end else None in
      let insert_extra = if insert_length_n_bits.(insert_code) > 0
        then lit_len - insert_length_offset.(insert_code) else 0 in
      let copy_extra = if copy_length_n_bits.(copy_code) > 0
        then copy_len - copy_length_offset.(copy_code) else 0 in
      {
        cmd_code; insert_code; copy_code; range_idx;
        insert_extra; copy_extra;
        lit_src_start = lit_start; lit_len; tree_ids_start = tree_start;
        dist_info;
      }
  ) commands in

  (* Build Huffman codes for each literal tree *)
  let lit_lengths_arr = Array.init num_lit_trees (fun i ->
    build_valid_code_lengths lit_freqs.(i) 15
  ) in
  let lit_codes_arr = Array.init num_lit_trees (fun i ->
    build_codes lit_lengths_arr.(i)
  ) in
  let cmd_lengths = build_valid_code_lengths cmd_freq 15 in
  let cmd_codes = build_codes cmd_lengths in
  (* Build Huffman codes for each distance tree *)
  let dist_lengths_arr = Array.init num_dist_trees (fun i ->
    build_valid_code_lengths dist_freqs.(i) 15
  ) in
  let dist_codes_arr = Array.init num_dist_trees (fun i ->
    build_codes dist_lengths_arr.(i)
  ) in

  (* Write meta-block header *)
  write_meta_block_header bw !total_len is_last false;

  (* Block type counts: 1 for each category *)
  Bit_writer.write_bits bw 1 0;  (* NBLTYPESL = 1 *)
  Bit_writer.write_bits bw 1 0;  (* NBLTYPESI = 1 *)
  Bit_writer.write_bits bw 1 0;  (* NBLTYPESD = 1 *)

  (* Distance parameters: NPOSTFIX=0, NDIRECT=0 *)
  Bit_writer.write_bits bw 2 0;
  Bit_writer.write_bits bw 4 0;

  (* Context mode for literal block type 0 *)
  Bit_writer.write_bits bw 2 (Context.int_of_mode context_mode);

  (* Literal context map *)
  write_context_map bw context_map num_lit_trees;

  (* Distance context map: 4 contexts per block type *)
  write_context_map bw dist_context_map num_dist_trees;

  (* Write Huffman codes for all literal trees *)
  for i = 0 to num_lit_trees - 1 do
    write_huffman_code bw lit_lengths_arr.(i) 256
  done;
  write_huffman_code bw cmd_lengths 704;
  (* Write Huffman codes for all distance trees *)
  for i = 0 to num_dist_trees - 1 do
    write_huffman_code bw dist_lengths_arr.(i) num_distance_codes
  done;

  (* ENCODING PASS: Read literals from source using stored indices *)
  let num_cmd_symbols = count_used_symbols cmd_freq in

  List.iter (fun pcmd ->
    (* Write command code *)
    if num_cmd_symbols > 1 then
      write_symbol bw cmd_codes cmd_lengths pcmd.cmd_code;
    (* Write insert extra bits *)
    if insert_length_n_bits.(pcmd.insert_code) > 0 then
      Bit_writer.write_bits bw insert_length_n_bits.(pcmd.insert_code) pcmd.insert_extra;
    (* Write copy extra bits if this is an InsertCopy *)
    if pcmd.copy_code > 0 && copy_length_n_bits.(pcmd.copy_code) > 0 then
      Bit_writer.write_bits bw copy_length_n_bits.(pcmd.copy_code) pcmd.copy_extra;
    (* Write literals by reading from source and looking up tree_id from shared buffer *)
    for i = 0 to pcmd.lit_len - 1 do
      let c = Char.code (Bytes.get src (pcmd.lit_src_start + i)) in
      let tree_id = tree_ids.(pcmd.tree_ids_start + i) in
      let num_symbols = count_used_symbols lit_freqs.(tree_id) in
      if num_symbols > 1 then
        write_symbol bw lit_codes_arr.(tree_id) lit_lengths_arr.(tree_id) c
    done;
    (* Write distance info if present *)
    match pcmd.dist_info with
    | Some (dist_tree, dist_code_val, nbits, extra) ->
      let num_dist_symbols = count_used_symbols dist_freqs.(dist_tree) in
      if num_dist_symbols > 1 then
        write_symbol bw dist_codes_arr.(dist_tree) dist_lengths_arr.(dist_tree) dist_code_val;
      if nbits > 0 then
        Bit_writer.write_bits bw nbits extra
    | None -> ()
  ) prepared_commands

(* Write a compressed block with LZ77 commands *)
let write_compressed_block bw src src_pos src_len is_last =
  (* Dictionary matching provides additional compression for text content *)
  let use_dict = !current_quality >= 3 in
  let quality = !current_quality in

  (* Generate commands using LZ77 or optimal parsing *)
  let commands =
    if quality >= 10 then
      (* Use optimal greedy parsing with lazy matching for quality 10-11 *)
      Optimal.generate_commands ~quality src src_pos src_len
    else
      (* Standard LZ77 for lower quality levels *)
      Lz77.generate_commands ~use_dict ~quality src src_pos src_len
  in

  (* Use context modeling for quality >= 5 *)
  if quality >= 5 then begin
    let context_mode = Block_split.choose_context_mode src src_pos src_len in
    (* For quality >= 7 with enough data, use multiple literal trees *)
    let (context_map, num_lit_trees) =
      if quality >= 7 && src_len >= 1024 then begin
        let max_trees = if quality >= 9 then 4 else 2 in
        let (cmap, _histograms, ntrees) =
          Block_split.build_literal_context_map context_mode src src_pos src_len max_trees
        in
        (cmap, ntrees)
      end else
        (Array.make 64 0, 1)
    in
    (* Distance context map: 4 contexts based on copy_length *)
    (* For now, use single distance tree (infrastructure ready for multiple) *)
    let dist_context_map = Array.make 4 0 in
    let num_dist_trees = 1 in
    write_compressed_block_with_context bw src src_pos src_len is_last
      context_mode context_map num_lit_trees num_dist_trees dist_context_map commands
  end else begin
    (* Simple encoding for quality < 5 *)
    (* OPTIMIZED: Uses indices into source buffer instead of per-command arrays *)

    (* Initialize frequency arrays *)
    let lit_freq = Array.make 256 0 in
    let cmd_freq = Array.make 704 0 in
    let num_distance_codes = 16 + 48 in
    let dist_freq = Array.make num_distance_codes 0 in

    (* SINGLE PASS: Prepare commands with indices, count frequencies by reading source once *)
    let total_len = ref 0 in
    let prepared_commands = List.map (fun cmd ->
      match cmd with
      | Lz77.Literals { start; len } ->
        total_len := !total_len + len;
        (* Count literal frequencies by reading from source *)
        for idx = 0 to len - 1 do
          let c = Char.code (Bytes.get src (start + idx)) in
          lit_freq.(c) <- lit_freq.(c) + 1
        done;
        let insert_code = get_insert_code len in
        let copy_code = 0 in
        let cmd_code = get_command_code insert_code copy_code false in
        cmd_freq.(cmd_code) <- cmd_freq.(cmd_code) + 1;
        dist_freq.(0) <- dist_freq.(0) + 1;
        let insert_extra = if insert_length_n_bits.(insert_code) > 0
          then len - insert_length_offset.(insert_code) else 0 in
        { s_cmd_code = cmd_code; s_insert_code = insert_code; s_copy_code = copy_code;
          s_insert_extra = insert_extra; s_copy_extra = 0;
          s_lit_start = start; s_lit_len = len; s_dist_info = None }
      | Lz77.InsertCopy { lit_start; lit_len; copy_len; distance; dist_code } ->
        total_len := !total_len + lit_len + copy_len;
        (* Count literal frequencies by reading from source *)
        for idx = 0 to lit_len - 1 do
          let c = Char.code (Bytes.get src (lit_start + idx)) in
          lit_freq.(c) <- lit_freq.(c) + 1
        done;
        let insert_code = get_insert_code lit_len in
        let copy_code = get_copy_code copy_len in
        let use_implicit = dist_code = 0 in
        let cmd_code = get_command_code insert_code copy_code use_implicit in
        let range_idx = cmd_code lsr 6 in
        cmd_freq.(cmd_code) <- cmd_freq.(cmd_code) + 1;
        (* Compute and count distance info if needed *)
        let dist_info = if range_idx >= 2 then begin
          if dist_code >= 0 then begin
            dist_freq.(dist_code) <- dist_freq.(dist_code) + 1;
            Some (dist_code, 0, 0)
          end else begin
            let dist_code_val, nbits, extra = encode_distance distance in
            let capped_code = if dist_code_val < num_distance_codes
              then dist_code_val else num_distance_codes - 1 in
            dist_freq.(capped_code) <- dist_freq.(capped_code) + 1;
            Some (dist_code_val, nbits, extra)
          end
        end else None in
        let insert_extra = if insert_length_n_bits.(insert_code) > 0
          then lit_len - insert_length_offset.(insert_code) else 0 in
        let copy_extra = if copy_length_n_bits.(copy_code) > 0
          then copy_len - copy_length_offset.(copy_code) else 0 in
        { s_cmd_code = cmd_code; s_insert_code = insert_code; s_copy_code = copy_code;
          s_insert_extra = insert_extra; s_copy_extra = copy_extra;
          s_lit_start = lit_start; s_lit_len = lit_len; s_dist_info = dist_info }
    ) commands in

    (* Build Huffman codes *)
    let lit_lengths = build_valid_code_lengths lit_freq 15 in
    let lit_codes = build_codes lit_lengths in
    let cmd_lengths = build_valid_code_lengths cmd_freq 15 in
    let cmd_codes = build_codes cmd_lengths in
    let dist_lengths = build_valid_code_lengths dist_freq 15 in
    let dist_codes = build_codes dist_lengths in

    (* Write meta-block header *)
    write_meta_block_header bw !total_len is_last false;

    (* Block type counts: 1 for each category *)
    Bit_writer.write_bits bw 1 0;
    Bit_writer.write_bits bw 1 0;
    Bit_writer.write_bits bw 1 0;

    (* Distance parameters: NPOSTFIX=0, NDIRECT=0 *)
    Bit_writer.write_bits bw 2 0;
    Bit_writer.write_bits bw 4 0;

    (* Context mode for literal block type 0: LSB6 = 0 *)
    Bit_writer.write_bits bw 2 0;

    (* Literal context map: NTREESL = 1 tree *)
    Bit_writer.write_bits bw 1 0;

    (* Distance context map: NTREESD = 1 tree *)
    Bit_writer.write_bits bw 1 0;

    (* Write Huffman codes *)
    write_huffman_code bw lit_lengths 256;
    write_huffman_code bw cmd_lengths 704;
    write_huffman_code bw dist_lengths num_distance_codes;

    (* ENCODING PASS: Read literals from source using stored indices *)
    let num_lit_symbols = count_used_symbols lit_freq in
    let num_cmd_symbols = count_used_symbols cmd_freq in
    let num_dist_symbols = count_used_symbols dist_freq in

    List.iter (fun pcmd ->
      (* Write command code *)
      if num_cmd_symbols > 1 then
        write_symbol bw cmd_codes cmd_lengths pcmd.s_cmd_code;
      (* Write insert extra bits *)
      if insert_length_n_bits.(pcmd.s_insert_code) > 0 then
        Bit_writer.write_bits bw insert_length_n_bits.(pcmd.s_insert_code) pcmd.s_insert_extra;
      (* Write copy extra bits if this is an InsertCopy *)
      if pcmd.s_copy_code > 0 && copy_length_n_bits.(pcmd.s_copy_code) > 0 then
        Bit_writer.write_bits bw copy_length_n_bits.(pcmd.s_copy_code) pcmd.s_copy_extra;
      (* Write literals by reading from source *)
      if num_lit_symbols > 1 then
        for i = 0 to pcmd.s_lit_len - 1 do
          let c = Char.code (Bytes.get src (pcmd.s_lit_start + i)) in
          write_symbol bw lit_codes lit_lengths c
        done;
      (* Write distance info if present *)
      match pcmd.s_dist_info with
      | Some (dist_code_val, nbits, extra) ->
        if num_dist_symbols > 1 then
          write_symbol bw dist_codes dist_lengths dist_code_val;
        if nbits > 0 then
          Bit_writer.write_bits bw nbits extra
      | None -> ()
    ) prepared_commands
  end

(* Write a compressed block with only literals *)
(* OPTIMIZED: No per-command array allocation - reads from source directly *)
let write_literals_only_block bw src src_pos src_len is_last =
  write_meta_block_header bw src_len is_last false;
  Bit_writer.write_bits bw 1 0;
  Bit_writer.write_bits bw 1 0;
  Bit_writer.write_bits bw 1 0;
  Bit_writer.write_bits bw 2 0;
  Bit_writer.write_bits bw 4 0;
  Bit_writer.write_bits bw 2 0;
  Bit_writer.write_bits bw 1 0;
  Bit_writer.write_bits bw 1 0;

  (* Count frequencies by reading from source *)
  let lit_freq = Array.make 256 0 in
  for i = 0 to src_len - 1 do
    let c = Char.code (Bytes.get src (src_pos + i)) in
    lit_freq.(c) <- lit_freq.(c) + 1
  done;
  let num_lit_symbols = count_used_symbols lit_freq in
  let lit_lengths = build_valid_code_lengths lit_freq 15 in
  let lit_codes = build_codes lit_lengths in

  let insert_code = get_insert_code src_len in
  let copy_code = 0 in
  let cmd_code = get_command_code insert_code copy_code false in
  let cmd_freq = Array.make 704 0 in
  cmd_freq.(cmd_code) <- 1;
  let cmd_lengths = build_valid_code_lengths cmd_freq 15 in

  let num_distance_codes = 16 + 48 in
  let dist_freq = Array.make num_distance_codes 0 in
  dist_freq.(0) <- 1;
  let dist_lengths = build_valid_code_lengths dist_freq 15 in

  write_huffman_code bw lit_lengths 256;
  write_huffman_code bw cmd_lengths 704;
  write_huffman_code bw dist_lengths num_distance_codes;

  if insert_length_n_bits.(insert_code) > 0 then begin
    let extra = src_len - insert_length_offset.(insert_code) in
    Bit_writer.write_bits bw insert_length_n_bits.(insert_code) extra
  end;

  (* ENCODING PASS: Read literals from source *)
  if num_lit_symbols > 1 then
    for i = 0 to src_len - 1 do
      let c = Char.code (Bytes.get src (src_pos + i)) in
      write_symbol bw lit_codes lit_lengths c
    done

(* Main compression function *)
let compress_into ?(quality=1) ~src ~src_pos ~src_len ~dst ~dst_pos () =
  current_quality := quality;
  let bw = Bit_writer.create ~dst ~pos:dst_pos ~len:(Bytes.length dst - dst_pos) in
  encode_window_bits bw;

  if src_len = 0 then begin
    write_empty_last_block bw;
    Bit_writer.flush bw - dst_pos
  end
  else if quality = 0 || src_len < 16 then begin
    write_uncompressed_block bw src src_pos src_len;
    write_empty_last_block bw;
    Bit_writer.flush bw - dst_pos
  end
  else begin
    try
      if quality >= 2 && src_len >= min_match then
        write_compressed_block bw src src_pos src_len true
      else
        write_literals_only_block bw src src_pos src_len true;
      Bit_writer.flush bw - dst_pos
    with _ ->
      let bw = Bit_writer.create ~dst ~pos:dst_pos ~len:(Bytes.length dst - dst_pos) in
      encode_window_bits bw;
      write_uncompressed_block bw src src_pos src_len;
      write_empty_last_block bw;
      Bit_writer.flush bw - dst_pos
  end

let max_compressed_length input_len =
  input_len + input_len / 8 + 64

(* Streaming encoder state *)
type streaming_encoder = {
  mutable quality : int;
  mutable dst : bytes;
  mutable dst_pos : int;
  mutable header_written : bool;
  mutable finished : bool;
}

let create_streaming_encoder ?(quality=1) ~dst ~dst_pos () =
  { quality; dst; dst_pos; header_written = false; finished = false }

(* Write a chunk of data to the streaming encoder *)
let streaming_write encoder ~src ~src_pos ~src_len ~is_last =
  if encoder.finished then
    invalid_arg "streaming encoder already finished";

  current_quality := encoder.quality;
  let bw = Bit_writer.create ~dst:encoder.dst ~pos:encoder.dst_pos
    ~len:(Bytes.length encoder.dst - encoder.dst_pos) in

  (* Write header on first chunk *)
  if not encoder.header_written then begin
    encode_window_bits bw;
    encoder.header_written <- true
  end;

  if src_len = 0 then begin
    if is_last then begin
      write_empty_last_block bw;
      encoder.finished <- true
    end
  end
  else if encoder.quality = 0 || src_len < 16 then begin
    (* For low quality or small blocks, write uncompressed *)
    if is_last then begin
      write_uncompressed_block bw src src_pos src_len;
      write_empty_last_block bw;
      encoder.finished <- true
    end else begin
      (* Non-last uncompressed block *)
      write_meta_block_header bw src_len false true;
      Bit_writer.align_to_byte bw;
      Bit_writer.copy_bytes bw ~src ~src_pos ~len:src_len
    end
  end
  else begin
    try
      if encoder.quality >= 2 && src_len >= min_match then
        write_compressed_block bw src src_pos src_len is_last
      else
        write_literals_only_block bw src src_pos src_len is_last;
      if is_last then encoder.finished <- true
    with _ ->
      (* Fallback to uncompressed *)
      if is_last then begin
        write_uncompressed_block bw src src_pos src_len;
        write_empty_last_block bw;
        encoder.finished <- true
      end else begin
        write_meta_block_header bw src_len false true;
        Bit_writer.align_to_byte bw;
        Bit_writer.copy_bytes bw ~src ~src_pos ~len:src_len
      end
  end;

  let written = Bit_writer.flush bw - encoder.dst_pos in
  encoder.dst_pos <- encoder.dst_pos + written;
  written

let streaming_finish encoder =
  if not encoder.finished then begin
    let result = streaming_write encoder ~src:(Bytes.create 0) ~src_pos:0 ~src_len:0 ~is_last:true in
    encoder.finished <- true;
    result
  end else 0

let streaming_bytes_written encoder =
  encoder.dst_pos

(* Re-export command type for Debug module *)
type command = Lz77.command =
  | InsertCopy of { lit_start: int; lit_len: int; copy_len: int; distance: int; dist_code: int }
  | Literals of { start: int; len: int }

let generate_commands src src_pos src_len =
  Lz77.generate_commands src src_pos src_len
