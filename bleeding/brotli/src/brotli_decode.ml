(* Brotli decompression implementation (RFC 7932) *)

type error =
  | Invalid_stream_header
  | Invalid_meta_block_header
  | Invalid_huffman_code
  | Invalid_distance
  | Invalid_backward_reference
  | Invalid_context_map
  | Truncated_input
  | Output_overrun

exception Brotli_error of error

let error_to_string = function
  | Invalid_stream_header -> "Invalid stream header"
  | Invalid_meta_block_header -> "Invalid meta-block header"
  | Invalid_huffman_code -> "Invalid Huffman code"
  | Invalid_distance -> "Invalid distance"
  | Invalid_backward_reference -> "Invalid backward reference"
  | Invalid_context_map -> "Invalid context map"
  | Truncated_input -> "Truncated input"
  | Output_overrun -> "Output buffer overrun"

(* Distance short code lookup tables - packed as int8# arrays (1 byte per element) *)
let distance_short_code_index_offset : int8# array =
  [| #3s; #2s; #1s; #0s; #3s; #3s; #3s; #3s; #3s; #3s; #2s; #2s; #2s; #2s; #2s; #2s |]
let distance_short_code_value_offset : int8# array =
  [| #0s; #0s; #0s; #0s; -#1s; #1s; -#2s; #2s; -#3s; #3s; -#1s; #1s; -#2s; #2s; -#3s; #3s |]

(* Static Huffman code for code length code lengths - packed format: (value lsl 8) lor bits
   Uses nativeint# array for unboxed access. Values range 0x002-0x504. *)
let code_length_huff : nativeint# array = [|
  (* bits=2, value=0 *) #0x002n; (* bits=2, value=4 *) #0x402n;
  (* bits=2, value=3 *) #0x302n; (* bits=3, value=2 *) #0x203n;
  (* bits=2, value=0 *) #0x002n; (* bits=2, value=4 *) #0x402n;
  (* bits=2, value=3 *) #0x302n; (* bits=4, value=1 *) #0x104n;
  (* bits=2, value=0 *) #0x002n; (* bits=2, value=4 *) #0x402n;
  (* bits=2, value=3 *) #0x302n; (* bits=3, value=2 *) #0x203n;
  (* bits=2, value=0 *) #0x002n; (* bits=2, value=4 *) #0x402n;
  (* bits=2, value=3 *) #0x302n; (* bits=4, value=5 *) #0x504n
|]

(* Decode window bits from stream header *)
let decode_window_bits br =
  if Bit_reader.read_bits br 1 = 0 then 16
  else begin
    let n = Bit_reader.read_bits br 3 in
    if n > 0 then 17 + n
    else begin
      let n = Bit_reader.read_bits br 3 in
      if n > 0 then 8 + n
      else 17
    end
  end

(* Decode a variable length uint8 (0-255) *)
let decode_var_len_uint8 br =
  match Bit_reader.read_bits br 1 with
  | 0 -> 0
  | _ ->
    match Bit_reader.read_bits br 3 with
    | 0 -> 1
    | nbits -> Bit_reader.read_bits br nbits + (1 lsl nbits)

(* Meta-block header *)
type meta_block_header = {
  meta_block_length : int;
  input_end : bool;
  is_uncompressed : bool;
  is_metadata : bool;
}

(* Decode meta-block length - uses mutable locals to avoid ref allocation *)
let decode_meta_block_length br =
  let input_end = Bit_reader.read_bits br 1 = 1 in
  if input_end && Bit_reader.read_bits br 1 = 1 then
    { meta_block_length = 0; input_end = true; is_uncompressed = false; is_metadata = false }
  else begin
    let size_nibbles = Bit_reader.read_bits br 2 + 4 in
    if size_nibbles = 7 then begin
      (* Metadata block *)
      if Bit_reader.read_bits br 1 <> 0 then
        raise (Brotli_error Invalid_meta_block_header);
      let size_bytes = Bit_reader.read_bits br 2 in
      if size_bytes = 0 then
        { meta_block_length = 0; input_end; is_uncompressed = false; is_metadata = true }
      else begin
        let mutable length = 0 in
        for i = 0 to size_bytes - 1 do
          let next_byte = Bit_reader.read_bits br 8 in
          if i + 1 = size_bytes && size_bytes > 1 && next_byte = 0 then
            raise (Brotli_error Invalid_meta_block_header);
          length <- length lor (next_byte lsl (i * 8))
        done;
        { meta_block_length = length + 1; input_end; is_uncompressed = false; is_metadata = true }
      end
    end
    else begin
      let mutable length = 0 in
      for i = 0 to size_nibbles - 1 do
        let next_nibble = Bit_reader.read_bits br 4 in
        if i + 1 = size_nibbles && size_nibbles > 4 && next_nibble = 0 then
          raise (Brotli_error Invalid_meta_block_header);
        length <- length lor (next_nibble lsl (i * 4))
      done;
      let is_uncompressed =
        if not input_end then Bit_reader.read_bits br 1 = 1
        else false
      in
      { meta_block_length = length + 1; input_end; is_uncompressed; is_metadata = false }
    end
  end

(* Read Huffman code lengths - uses mutable locals to avoid ref allocation *)
let read_huffman_code_lengths code_length_code_lengths num_symbols code_lengths br =
  let mutable symbol = 0 in
  let mutable prev_code_len = 8 in
  let mutable repeat = 0 in
  let mutable repeat_code_len = 0 in
  let mutable space = 32768 in

  (* Build table for code length codes *)
  let table = Huffman.build_table ~code_lengths:code_length_code_lengths
      ~alphabet_size:Constants.code_length_codes ~root_bits:5 in

  while symbol < num_symbols && space > 0 do
    let code_len = Huffman.read_symbol_5 table br in
    if code_len < Constants.repeat_previous_code_length then begin
      repeat <- 0;
      code_lengths.(symbol) <- code_len;
      symbol <- symbol + 1;
      if code_len <> 0 then begin
        prev_code_len <- code_len;
        space <- space - (0x8000 lsr code_len)
      end
    end
    else begin
      let extra_bits = code_len - 14 in
      let new_len = if code_len = Constants.repeat_previous_code_length then prev_code_len else 0 in
      if repeat_code_len <> new_len then begin
        repeat <- 0;
        repeat_code_len <- new_len
      end;
      let old_repeat = repeat in
      if repeat > 0 then
        repeat <- (repeat - 2) lsl extra_bits;
      repeat <- repeat + Bit_reader.read_bits br extra_bits + 3;
      let repeat_delta = repeat - old_repeat in
      if symbol + repeat_delta > num_symbols then
        raise (Brotli_error Invalid_huffman_code);
      for _ = 0 to repeat_delta - 1 do
        code_lengths.(symbol) <- repeat_code_len;
        symbol <- symbol + 1
      done;
      if repeat_code_len <> 0 then
        space <- space - (repeat_delta lsl (15 - repeat_code_len))
    end
  done;

  if space <> 0 then
    raise (Brotli_error Invalid_huffman_code);

  for i = symbol to num_symbols - 1 do
    code_lengths.(i) <- 0
  done

(* Read a Huffman code from the stream - uses mutable locals *)
let read_huffman_code_with_bits alphabet_size root_bits br =
  let code_lengths = Array.make alphabet_size 0 in
  let simple_code_or_skip = Bit_reader.read_bits br 2 in

  if simple_code_or_skip = 1 then begin
    (* Simple prefix code *)
    let mutable max_bits = 0 in
    let mutable max_bits_counter = alphabet_size - 1 in
    while max_bits_counter > 0 do
      max_bits_counter <- max_bits_counter lsr 1;
      max_bits <- max_bits + 1
    done;

    let symbols = Array.make 4 0 in
    let num_symbols = Bit_reader.read_bits br 2 + 1 in

    for i = 0 to num_symbols - 1 do
      symbols.(i) <- Bit_reader.read_bits br max_bits mod alphabet_size;
      code_lengths.(symbols.(i)) <- 2
    done;
    code_lengths.(symbols.(0)) <- 1;

    (match num_symbols with
    | 2 ->
      if symbols.(0) = symbols.(1) then
        raise (Brotli_error Invalid_huffman_code);
      code_lengths.(symbols.(1)) <- 1
    | 4 ->
      if Bit_reader.read_bits br 1 = 1 then begin
        code_lengths.(symbols.(2)) <- 3;
        code_lengths.(symbols.(3)) <- 3
      end else
        code_lengths.(symbols.(0)) <- 2
    | _ -> ());

    Huffman.build_table ~code_lengths ~alphabet_size ~root_bits
  end
  else begin
    (* Complex prefix code *)
    let code_length_code_lengths = Array.make Constants.code_length_codes 0 in
    let mutable space = 32 in
    let mutable num_codes = 0 in

    for i = simple_code_or_skip to Constants.code_length_codes - 1 do
      if space > 0 then begin
        let code_len_idx = Constants.get_code_length_code_order(i) in
        let p = Bit_reader.peek_bits br 4 in
        let entry = Nativeint_u.to_int_trunc (Oxcaml_arrays.unsafe_get code_length_huff p) in
        Bit_reader.skip_bits br (entry land 0xFF);
        let v = entry lsr 8 in
        code_length_code_lengths.(code_len_idx) <- v;
        if v <> 0 then begin
          space <- space - (32 lsr v);
          num_codes <- num_codes + 1
        end
      end
    done;

    if num_codes <> 1 && space <> 0 then
      raise (Brotli_error Invalid_huffman_code);

    read_huffman_code_lengths code_length_code_lengths alphabet_size code_lengths br;

    (* Debug output removed for cleaner test output *)

    Huffman.build_table ~code_lengths ~alphabet_size ~root_bits
  end

let read_huffman_code alphabet_size br =
  read_huffman_code_with_bits alphabet_size Constants.huffman_max_table_bits br

(* Read block length *)
let read_block_length table br =
  let code = Huffman.read_symbol_8 table br in
  Prefix.decode_block_length br code

(* Translate distance short codes *)
let translate_short_codes code dist_rb dist_rb_idx =
  if code < Constants.num_distance_short_codes then begin
    let idx_off = Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get distance_short_code_index_offset code) in
    let val_off = Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get distance_short_code_value_offset code) in
    let index = (dist_rb_idx + idx_off) land 3 in
    dist_rb.(index) + val_off
  end
  else
    code - Constants.num_distance_short_codes + 1

(* Inverse move-to-front transform *)
let inverse_move_to_front_transform v v_len =
  let mtf = Array.init 256 (fun i -> i) in
  for i = 0 to v_len - 1 do
    let index = v.(i) in
    v.(i) <- mtf.(index);
    if index > 0 then begin
      let value = mtf.(index) in
      for j = index downto 1 do
        mtf.(j) <- mtf.(j - 1)
      done;
      mtf.(0) <- value
    end
  done

(* Decode context map *)
let decode_context_map context_map_size br =
  let num_trees = decode_var_len_uint8 br + 1 in
  let context_map = Array.make context_map_size 0 in

  if num_trees <= 1 then
    (num_trees, context_map)
  else begin
    let use_rle = Bit_reader.read_bits br 1 = 1 in
    let max_rle_prefix = if use_rle then Bit_reader.read_bits br 4 + 1 else 0 in
    let table = read_huffman_code (num_trees + max_rle_prefix) br in

    let mutable i = 0 in
    while i < context_map_size do
      let code = Huffman.read_symbol_8 table br in
      if code = 0 then begin
        context_map.(i) <- 0;
        i <- i + 1
      end
      else if code <= max_rle_prefix then begin
        let reps = (1 lsl code) + Bit_reader.read_bits br code in
        for _ = 0 to reps - 1 do
          if i >= context_map_size then
            raise (Brotli_error Invalid_context_map);
          context_map.(i) <- 0;
          i <- i + 1
        done
      end
      else begin
        context_map.(i) <- code - max_rle_prefix;
        i <- i + 1
      end
    done;

    if Bit_reader.read_bits br 1 = 1 then
      inverse_move_to_front_transform context_map context_map_size;

    (num_trees, context_map)
  end

(* Decode block type *)
let decode_block_type max_block_type table block_type_rb block_type_rb_idx br =
  let type_code = Huffman.read_symbol_8 table br in
  let block_type = match type_code with
    | 0 -> block_type_rb.((!block_type_rb_idx) land 1)
    | 1 -> block_type_rb.(((!block_type_rb_idx) - 1) land 1) + 1
    | _ -> type_code - 2
  in
  let block_type = block_type mod max_block_type in
  block_type_rb.((!block_type_rb_idx) land 1) <- block_type;
  incr block_type_rb_idx;
  block_type

(* Main decompression function *)
let decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos =
  let br = Bit_reader.create ~src ~pos:src_pos ~len:src_len in
  let mutable pos = dst_pos in

  (* Distance ring buffer *)
  let dist_rb = [| 16; 15; 11; 4 |] in
  let mutable dist_rb_idx = 0 in

  (* Decode window bits *)
  let window_bits = decode_window_bits br in
  let max_backward_distance = (1 lsl window_bits) - Constants.window_gap in

  let mutable input_end = false in

  while not input_end do
    (* Decode meta-block header *)
    let header = decode_meta_block_length br in
    input_end <- header.input_end;

    if header.is_metadata then begin
      (* Skip metadata block *)
      Bit_reader.align_to_byte br;
      for _ = 1 to header.meta_block_length do
        ignore (Bit_reader.read_bits br 8)
      done
    end
    else if header.meta_block_length > 0 then begin
      if header.is_uncompressed then begin
        (* Uncompressed block *)
        Bit_reader.copy_bytes br ~dst ~dst_pos:pos ~len:header.meta_block_length;
        pos <- pos + header.meta_block_length
      end
      else begin
        (* Compressed block *)
        let mutable meta_block_remaining = header.meta_block_length in

        (* Decode block type counts and trees *)
        let num_block_types = Array.make 3 1 in
        let block_type = Array.make 3 0 in
        let block_length = Array.make 3 (1 lsl 28) in
        let block_type_rb = [| [| 0; 1 |]; [| 0; 1 |]; [| 0; 1 |] |] in
        let block_type_rb_idx = [| ref 0; ref 0; ref 0 |] in
        let block_type_trees = Array.make 3 [||] in
        let block_len_trees = Array.make 3 [||] in

        for i = 0 to 2 do
          num_block_types.(i) <- decode_var_len_uint8 br + 1;
          if num_block_types.(i) >= 2 then begin
            block_type_trees.(i) <- read_huffman_code (num_block_types.(i) + 2) br;
            block_len_trees.(i) <- read_huffman_code Constants.num_block_len_symbols br;
            block_length.(i) <- read_block_length block_len_trees.(i) br;
            block_type_rb_idx.(i) := 1
          end
        done;

        (* Distance parameters *)
        let distance_postfix_bits = Bit_reader.read_bits br 2 in
        let num_direct_distance_codes =
          Constants.num_distance_short_codes + (Bit_reader.read_bits br 4 lsl distance_postfix_bits) in
        let distance_postfix_mask = (1 lsl distance_postfix_bits) - 1 in
        let num_distance_codes = num_direct_distance_codes + (48 lsl distance_postfix_bits) in

        (* Context modes for literal blocks *)
        let context_modes = Array.make num_block_types.(0) 0 in
        for i = 0 to num_block_types.(0) - 1 do
          context_modes.(i) <- Bit_reader.read_bits br 2 lsl 1
        done;

        (* Decode context maps *)
        let num_literal_trees, literal_context_map =
          decode_context_map (num_block_types.(0) lsl Constants.literal_context_bits) br in
        let num_dist_trees, dist_context_map =
          decode_context_map (num_block_types.(2) lsl Constants.distance_context_bits) br in

        (* Decode Huffman tree groups *)
        let literal_trees = Array.init num_literal_trees (fun _ ->
          read_huffman_code Constants.num_literal_symbols br) in
        let command_trees = Array.init num_block_types.(1) (fun _ ->
          read_huffman_code_with_bits Constants.num_command_symbols
            Constants.huffman_max_command_table_bits br) in
        let distance_trees = Array.init num_dist_trees (fun _ ->
          read_huffman_code num_distance_codes br) in

        (* ====================================================================
           TRIVIAL LITERAL CONTEXT DETECTION (following C brotli approach)
           ====================================================================
           When all 64 context entries for a block type map to the same tree,
           we can skip the context lookup entirely and use that single tree.
           This is a significant optimization for many real-world files. *)
        let trivial_literal_contexts =
          let bits = Array.make ((num_block_types.(0) + 31) / 32) 0 in
          for i = 0 to num_block_types.(0) - 1 do
            let offset = i lsl Constants.literal_context_bits in
            let sample = literal_context_map.(offset) in
            let mutable all_same = true in
            let mutable j = 1 in
            while all_same && j < 64 do
              if literal_context_map.(offset + j) <> sample then
                all_same <- false;
              j <- j + 1
            done;
            if all_same then
              bits.(i lsr 5) <- bits.(i lsr 5) lor (1 lsl (i land 31))
          done;
          bits
        in
        (* Check if a block type has trivial literal context *)
        let[@inline always] is_trivial_context block_type =
          let word = Array.unsafe_get trivial_literal_contexts (block_type lsr 5) in
          (word lsr (block_type land 31)) land 1 = 1
        in

        (* Main decode loop *)
        let mutable context_map_slice = 0 in
        let mutable dist_context_map_slice = 0 in
        let mutable context_mode = context_modes.(block_type.(0)) in
        (* Precompute context LUT offsets for fast context lookup *)
        let mutable context_lut = Context.get_context_lut (context_mode lsr 1) in
        (* Track trivial context state and cached tree for fast path *)
        let mutable trivial_context = is_trivial_context block_type.(0) in
        let mutable literal_htree = literal_trees.(literal_context_map.(context_map_slice)) in
        let mutable huff_tree_command = command_trees.(0) in

        while meta_block_remaining > 0 do
          (* Check/update command block *)
          if block_length.(1) = 0 then begin
            block_type.(1) <- decode_block_type num_block_types.(1)
              block_type_trees.(1) block_type_rb.(1) block_type_rb_idx.(1) br;
            block_length.(1) <- read_block_length block_len_trees.(1) br;
            huff_tree_command <- command_trees.(block_type.(1))
          end;
          block_length.(1) <- block_length.(1) - 1;

          (* Read command code *)
          let cmd_code = Huffman.read_symbol_10 huff_tree_command br in
          let range_idx = cmd_code lsr 6 in
          let mutable distance_code = if range_idx >= 2 then -1 else 0 in
          let range_idx = if range_idx >= 2 then range_idx - 2 else range_idx in

          (* Decode insert and copy lengths *)
          let insert_code = Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get Prefix.insert_range_lut range_idx) + ((cmd_code lsr 3) land 7) in
          let copy_code = Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get Prefix.copy_range_lut range_idx) + (cmd_code land 7) in
          let insert_length = Prefix.decode_insert_length br insert_code in
          let copy_length = Prefix.decode_copy_length br copy_code in

          (* Get context bytes *)
          let mutable prev_byte1 = if pos > dst_pos then Char.code (Bytes.get dst (pos - 1)) else 0 in
          let mutable prev_byte2 = if pos > dst_pos + 1 then Char.code (Bytes.get dst (pos - 2)) else 0 in

          (* ================================================================
             OPTIMIZED LITERAL DECODING LOOP
             ================================================================
             Two-path optimization following C brotli:
             1. TRIVIAL PATH: When all context entries map to same tree,
                skip context lookup entirely and decode directly.
             2. CONTEXT PATH: Use precomputed LUT offsets to inline context
                lookup, avoiding mode_of_int/int_of_mode overhead.
             ================================================================ *)
          let mutable insert_remaining = insert_length in
          while insert_remaining > 0 do
            (* Check if we need to switch literal block type *)
            if block_length.(0) = 0 then begin
              block_type.(0) <- decode_block_type num_block_types.(0)
                block_type_trees.(0) block_type_rb.(0) block_type_rb_idx.(0) br;
              block_length.(0) <- read_block_length block_len_trees.(0) br;
              context_map_slice <- block_type.(0) lsl Constants.literal_context_bits;
              context_mode <- context_modes.(block_type.(0));
              (* Update trivial context flag and cached tree *)
              trivial_context <- is_trivial_context block_type.(0);
              literal_htree <- literal_trees.(literal_context_map.(context_map_slice));
              (* Update context LUT for non-trivial path *)
              context_lut <- Context.get_context_lut (context_mode lsr 1)
            end;
            (* Batch decode: process min(remaining, block_length) literals without block checks *)
            let batch_size = min insert_remaining block_length.(0) in
            (* Check output buffer has room for entire batch *)
            if pos + batch_size > Bytes.length dst then
              raise (Brotli_error Output_overrun);

            (* ============================================================
               FAST PATH: Trivial literal context
               ============================================================
               When all 64 context entries map to the same tree, we skip
               context computation entirely. Use batch decode for maximum
               throughput - this writes directly to the output buffer. *)
            if trivial_context then begin
              let _decoded = Huffman.decode_symbols_to_bytes_8 literal_htree br dst pos batch_size in
              pos <- pos + batch_size;
              (* Update prev_bytes for next batch (only last 2 matter) *)
              if batch_size >= 2 then begin
                prev_byte1 <- Char.code (Bytes.unsafe_get dst (pos - 1));
                prev_byte2 <- Char.code (Bytes.unsafe_get dst (pos - 2))
              end else if batch_size = 1 then begin
                prev_byte2 <- prev_byte1;
                prev_byte1 <- Char.code (Bytes.unsafe_get dst (pos - 1))
              end
            end
            (* ============================================================
               CONTEXT PATH: Non-trivial literal context
               ============================================================
               Use precomputed LUT offsets for fast context lookup.
               Inline the context computation to avoid function call overhead. *)
            else begin
              let offset1, offset2 = context_lut in
              for _ = 0 to batch_size - 1 do
                (* Inline context lookup: BROTLI_CONTEXT(p1, p2, lut) *)
                let context = Context.get_context_fast ~offset1 ~offset2
                  ~prev_byte1 ~prev_byte2 in
                let tree_idx = Array.unsafe_get literal_context_map (context_map_slice + context) in
                let literal = Huffman.read_symbol_8 (Array.unsafe_get literal_trees tree_idx) br in
                prev_byte2 <- prev_byte1;
                prev_byte1 <- literal;
                Bytes.unsafe_set dst pos (Char.chr literal);
                pos <- pos + 1
              done
            end;
            block_length.(0) <- block_length.(0) - batch_size;
            insert_remaining <- insert_remaining - batch_size
          done;

          meta_block_remaining <- meta_block_remaining - insert_length;
          if meta_block_remaining <= 0 then
            ()  (* Break from loop *)
          else begin
            (* Decode distance if needed *)
            if distance_code < 0 then begin
              if block_length.(2) = 0 then begin
                block_type.(2) <- decode_block_type num_block_types.(2)
                  block_type_trees.(2) block_type_rb.(2) block_type_rb_idx.(2) br;
                block_length.(2) <- read_block_length block_len_trees.(2) br;
                dist_context_map_slice <- block_type.(2) lsl Constants.distance_context_bits
              end;
              block_length.(2) <- block_length.(2) - 1;
              let context = Context.distance_context copy_length in
              let tree_idx = dist_context_map.(dist_context_map_slice + context) in
              distance_code <- Huffman.read_symbol_8 distance_trees.(tree_idx) br;

              if distance_code >= num_direct_distance_codes then begin
                distance_code <- distance_code - num_direct_distance_codes;
                let postfix = distance_code land distance_postfix_mask in
                distance_code <- distance_code lsr distance_postfix_bits;
                let nbits = (distance_code lsr 1) + 1 in
                let offset = ((2 + (distance_code land 1)) lsl nbits) - 4 in
                distance_code <- num_direct_distance_codes +
                  ((offset + Bit_reader.read_bits br nbits) lsl distance_postfix_bits) + postfix
              end
            end;

            (* Convert distance code to actual distance *)
            let distance = translate_short_codes distance_code dist_rb dist_rb_idx in
            if distance < 0 then
              raise (Brotli_error Invalid_distance);

            let max_distance = min max_backward_distance (pos - dst_pos) in

            if distance > max_distance then begin
              (* Dictionary reference *)
              if copy_length >= Constants.min_dictionary_word_length &&
                 copy_length <= Constants.max_dictionary_word_length then begin
                let word_id = distance - max_distance - 1 in
                let shift = Dictionary.get_size_bits_by_length copy_length in
                let mask = (1 lsl shift) - 1 in
                let word_idx = word_id land mask in
                let transform_idx = word_id lsr shift in
                if transform_idx < Transform.num_transforms then begin
                  if pos + copy_length > Bytes.length dst then
                    raise (Brotli_error Output_overrun);
                  let length = Transform.transform_dictionary_word
                    ~dst ~dst_pos:pos ~word_index:word_idx
                    ~word_length:copy_length ~transform_id:transform_idx in
                  pos <- pos + length;
                  meta_block_remaining <- meta_block_remaining - length
                end
                else
                  raise (Brotli_error Invalid_backward_reference)
              end
              else
                raise (Brotli_error Invalid_backward_reference)
            end
            else begin
              (* Regular backward reference *)
              if distance_code > 0 then begin
                dist_rb.(dist_rb_idx land 3) <- distance;
                dist_rb_idx <- dist_rb_idx + 1
              end;

              if copy_length > meta_block_remaining then
                raise (Brotli_error Invalid_backward_reference);

              if pos + copy_length > Bytes.length dst then
                raise (Brotli_error Output_overrun);

              (* Optimized copy: use blit when distance >= copy_length *)
              if distance >= copy_length then begin
                Bytes.blit dst (pos - distance) dst pos copy_length;
                pos <- pos + copy_length;
                meta_block_remaining <- meta_block_remaining - copy_length
              end else begin
                (* Overlapping copy - use SIMD for small distances (1-16) *)
                if Copy_impl.copy_overlapping dst pos distance copy_length then begin
                  pos <- pos + copy_length;
                  meta_block_remaining <- meta_block_remaining - copy_length
                end else begin
                  (* Fallback: byte-by-byte for distances > 16.
                     SAFETY: bounds verified above with:
                     - `if pos + copy_length > Bytes.length dst` check
                     - `distance <= max_distance <= pos - dst_pos` ensures pos - distance >= dst_pos >= 0 *)
                  for _ = 0 to copy_length - 1 do
                    Bytes.unsafe_set dst pos (Bytes.unsafe_get dst (pos - distance));
                    pos <- pos + 1;
                    meta_block_remaining <- meta_block_remaining - 1
                  done
                end
              end
            end
          end
        done
      end
    end
  done;

  pos - dst_pos
