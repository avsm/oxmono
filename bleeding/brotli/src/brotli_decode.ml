(* Brotli decompression implementation (RFC 7932) *)

(* ==========================================================================
   SIMD-accelerated overlapping copy for backward references with small distances
   ==========================================================================
   For small distances (1-16 bytes), SIMD pattern fill is much faster than
   byte-by-byte copying. We create a 16-byte repeating pattern and write
   it using SIMD stores.

   Distance 1: Fill with a single byte (Bytes.unsafe_fill)
   Distance 2-16: Create pattern by replicating the source bytes using
                  SIMD shuffle to extend to 16 bytes and store
   ========================================================================== *)

module Simd_copy = struct
  module I8x16 = Ocaml_simd_sse.Int8x16

  (* SIMD-accelerated overlapping copy for backward references.

     The key insight is that when the pattern length (distance) divides 16,
     we can create a single 16-byte pattern and repeat it. This works because
     the pattern tiles exactly into 16-byte chunks.

     For distances that don't divide 16, the pattern doesn't tile evenly,
     so we'd need to rotate the pattern on each write. For simplicity,
     we only SIMD-optimize distances 1, 2, 4, 8, and 16. *)

  (* Distance 2: pattern [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1] *)
  let shuffle_mask_2 = I8x16.const
    #0s #1s #0s #1s #0s #1s #0s #1s
    #0s #1s #0s #1s #0s #1s #0s #1s

  (* Distance 4: pattern [0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3] *)
  let shuffle_mask_4 = I8x16.const
    #0s #1s #2s #3s #0s #1s #2s #3s
    #0s #1s #2s #3s #0s #1s #2s #3s

  (* Distance 8: pattern [0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7] *)
  let shuffle_mask_8 = I8x16.const
    #0s #1s #2s #3s #4s #5s #6s #7s
    #0s #1s #2s #3s #4s #5s #6s #7s

  (* Copy with distance 1: fill with a single repeated byte *)
  let[@inline always] copy_distance_1 dst dst_pos copy_length =
    let byte_val = Bytes.unsafe_get dst (dst_pos - 1) in
    Bytes.unsafe_fill dst dst_pos copy_length byte_val

  (* Copy with distance 2: SIMD pattern fill *)
  let[@inline always] copy_distance_2 dst dst_pos copy_length =
    let src_pos = dst_pos - 2 in
    let src_vec = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern = I8x16.shuffle ~pattern:shuffle_mask_2 src_vec in
    let remaining = ref copy_length in
    let pos = ref dst_pos in
    while !remaining >= 16 do
      I8x16.Bytes.unsafe_set dst ~byte:!pos pattern;
      pos := !pos + 16;
      remaining := !remaining - 16
    done;
    (* Handle remaining bytes *)
    if !remaining > 0 then begin
      for i = 0 to !remaining - 1 do
        Bytes.unsafe_set dst (!pos + i) (Bytes.unsafe_get dst (src_pos + (i land 1)))
      done
    end

  (* Copy with distance 4: SIMD pattern fill *)
  let[@inline always] copy_distance_4 dst dst_pos copy_length =
    let src_pos = dst_pos - 4 in
    let src_vec = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern = I8x16.shuffle ~pattern:shuffle_mask_4 src_vec in
    let remaining = ref copy_length in
    let pos = ref dst_pos in
    while !remaining >= 16 do
      I8x16.Bytes.unsafe_set dst ~byte:!pos pattern;
      pos := !pos + 16;
      remaining := !remaining - 16
    done;
    if !remaining > 0 then begin
      for i = 0 to !remaining - 1 do
        Bytes.unsafe_set dst (!pos + i) (Bytes.unsafe_get dst (src_pos + (i land 3)))
      done
    end

  (* Copy with distance 8: SIMD pattern fill *)
  let[@inline always] copy_distance_8 dst dst_pos copy_length =
    let src_pos = dst_pos - 8 in
    let src_vec = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern = I8x16.shuffle ~pattern:shuffle_mask_8 src_vec in
    let remaining = ref copy_length in
    let pos = ref dst_pos in
    while !remaining >= 16 do
      I8x16.Bytes.unsafe_set dst ~byte:!pos pattern;
      pos := !pos + 16;
      remaining := !remaining - 16
    done;
    if !remaining > 0 then begin
      for i = 0 to !remaining - 1 do
        Bytes.unsafe_set dst (!pos + i) (Bytes.unsafe_get dst (src_pos + (i land 7)))
      done
    end

  (* Copy with distance 16: load 16 bytes and repeat *)
  let[@inline always] copy_distance_16 dst dst_pos copy_length =
    let src_pos = dst_pos - 16 in
    let pattern = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let remaining = ref copy_length in
    let pos = ref dst_pos in
    while !remaining >= 16 do
      I8x16.Bytes.unsafe_set dst ~byte:!pos pattern;
      pos := !pos + 16;
      remaining := !remaining - 16
    done;
    if !remaining > 0 then begin
      for i = 0 to !remaining - 1 do
        Bytes.unsafe_set dst (!pos + i) (Bytes.unsafe_get dst (src_pos + i))
      done
    end

  (* Main entry point: SIMD-accelerated overlapping copy for small distances.
     Returns true if SIMD copy was used, false if caller should use fallback.

     Only optimizes distances 1, 2, 4, 8, 16 where the pattern tiles exactly
     into 16-byte chunks. Other distances fall back to byte-by-byte copy. *)
  let[@inline always] copy_overlapping dst dst_pos distance copy_length =
    match distance with
    | 1 ->
      copy_distance_1 dst dst_pos copy_length;
      true
    | 2 ->
      copy_distance_2 dst dst_pos copy_length;
      true
    | 4 ->
      copy_distance_4 dst dst_pos copy_length;
      true
    | 8 ->
      copy_distance_8 dst dst_pos copy_length;
      true
    | 16 ->
      copy_distance_16 dst dst_pos copy_length;
      true
    | _ ->
      false
end

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

(* Distance short code lookup tables *)
let distance_short_code_index_offset = [| 3; 2; 1; 0; 3; 3; 3; 3; 3; 3; 2; 2; 2; 2; 2; 2 |]
let distance_short_code_value_offset = [| 0; 0; 0; 0; -1; 1; -2; 2; -3; 3; -1; 1; -2; 2; -3; 3 |]

(* Static Huffman code for code length code lengths - packed format: (value lsl 8) lor bits *)
let code_length_huff = [|
  (* bits=2, value=0 *) 0x002; (* bits=2, value=4 *) 0x402;
  (* bits=2, value=3 *) 0x302; (* bits=3, value=2 *) 0x203;
  (* bits=2, value=0 *) 0x002; (* bits=2, value=4 *) 0x402;
  (* bits=2, value=3 *) 0x302; (* bits=4, value=1 *) 0x104;
  (* bits=2, value=0 *) 0x002; (* bits=2, value=4 *) 0x402;
  (* bits=2, value=3 *) 0x302; (* bits=3, value=2 *) 0x203;
  (* bits=2, value=0 *) 0x002; (* bits=2, value=4 *) 0x402;
  (* bits=2, value=3 *) 0x302; (* bits=4, value=5 *) 0x504;
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
  if Bit_reader.read_bits br 1 = 1 then begin
    let nbits = Bit_reader.read_bits br 3 in
    if nbits = 0 then 1
    else Bit_reader.read_bits br nbits + (1 lsl nbits)
  end
  else 0

(* Meta-block header *)
type meta_block_header = {
  meta_block_length : int;
  input_end : bool;
  is_uncompressed : bool;
  is_metadata : bool;
}

(* Decode meta-block length *)
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
        let length = ref 0 in
        for i = 0 to size_bytes - 1 do
          let next_byte = Bit_reader.read_bits br 8 in
          if i + 1 = size_bytes && size_bytes > 1 && next_byte = 0 then
            raise (Brotli_error Invalid_meta_block_header);
          length := !length lor (next_byte lsl (i * 8))
        done;
        { meta_block_length = !length + 1; input_end; is_uncompressed = false; is_metadata = true }
      end
    end
    else begin
      let length = ref 0 in
      for i = 0 to size_nibbles - 1 do
        let next_nibble = Bit_reader.read_bits br 4 in
        if i + 1 = size_nibbles && size_nibbles > 4 && next_nibble = 0 then
          raise (Brotli_error Invalid_meta_block_header);
        length := !length lor (next_nibble lsl (i * 4))
      done;
      let is_uncompressed =
        if not input_end then Bit_reader.read_bits br 1 = 1
        else false
      in
      { meta_block_length = !length + 1; input_end; is_uncompressed; is_metadata = false }
    end
  end

(* Read Huffman code lengths *)
let read_huffman_code_lengths code_length_code_lengths num_symbols code_lengths br =
  let symbol = ref 0 in
  let prev_code_len = ref 8 in
  let repeat = ref 0 in
  let repeat_code_len = ref 0 in
  let space = ref 32768 in

  (* Build table for code length codes *)
  let table = Huffman.build_table ~code_lengths:code_length_code_lengths
      ~alphabet_size:Constants.code_length_codes ~root_bits:5 in

  while !symbol < num_symbols && !space > 0 do
    let code_len = Huffman.read_symbol_5 table br in
    if code_len < Constants.repeat_previous_code_length then begin
      repeat := 0;
      code_lengths.(!symbol) <- code_len;
      incr symbol;
      if code_len <> 0 then begin
        prev_code_len := code_len;
        space := !space - (0x8000 lsr code_len)
      end
    end
    else begin
      let extra_bits = code_len - 14 in
      let new_len = if code_len = Constants.repeat_previous_code_length then !prev_code_len else 0 in
      if !repeat_code_len <> new_len then begin
        repeat := 0;
        repeat_code_len := new_len
      end;
      let old_repeat = !repeat in
      if !repeat > 0 then
        repeat := (!repeat - 2) lsl extra_bits;
      repeat := !repeat + Bit_reader.read_bits br extra_bits + 3;
      let repeat_delta = !repeat - old_repeat in
      if !symbol + repeat_delta > num_symbols then
        raise (Brotli_error Invalid_huffman_code);
      for _ = 0 to repeat_delta - 1 do
        code_lengths.(!symbol) <- !repeat_code_len;
        incr symbol
      done;
      if !repeat_code_len <> 0 then
        space := !space - (repeat_delta lsl (15 - !repeat_code_len))
    end
  done;

  if !space <> 0 then
    raise (Brotli_error Invalid_huffman_code);

  for i = !symbol to num_symbols - 1 do
    code_lengths.(i) <- 0
  done

(* Read a Huffman code from the stream *)
let read_huffman_code_with_bits alphabet_size root_bits br =
  let code_lengths = Array.make alphabet_size 0 in
  let simple_code_or_skip = Bit_reader.read_bits br 2 in

  if simple_code_or_skip = 1 then begin
    (* Simple prefix code *)
    let max_bits = ref 0 in
    let max_bits_counter = ref (alphabet_size - 1) in
    while !max_bits_counter > 0 do
      max_bits_counter := !max_bits_counter lsr 1;
      incr max_bits
    done;

    let symbols = Array.make 4 0 in
    let num_symbols = Bit_reader.read_bits br 2 + 1 in

    for i = 0 to num_symbols - 1 do
      symbols.(i) <- Bit_reader.read_bits br !max_bits mod alphabet_size;
      code_lengths.(symbols.(i)) <- 2
    done;
    code_lengths.(symbols.(0)) <- 1;

    if num_symbols = 2 then begin
      if symbols.(0) = symbols.(1) then
        raise (Brotli_error Invalid_huffman_code);
      code_lengths.(symbols.(1)) <- 1
    end
    else if num_symbols = 4 then begin
      if Bit_reader.read_bits br 1 = 1 then begin
        code_lengths.(symbols.(2)) <- 3;
        code_lengths.(symbols.(3)) <- 3
      end
      else
        code_lengths.(symbols.(0)) <- 2
    end;

    Huffman.build_table ~code_lengths ~alphabet_size ~root_bits
  end
  else begin
    (* Complex prefix code *)
    let code_length_code_lengths = Array.make Constants.code_length_codes 0 in
    let space = ref 32 in
    let num_codes = ref 0 in

    for i = simple_code_or_skip to Constants.code_length_codes - 1 do
      if !space > 0 then begin
        let code_len_idx = Constants.code_length_code_order.(i) in
        let p = Bit_reader.peek_bits br 4 in
        let entry = code_length_huff.(p) in
        Bit_reader.skip_bits br (entry land 0xFF);
        let v = entry lsr 8 in
        code_length_code_lengths.(code_len_idx) <- v;
        if v <> 0 then begin
          space := !space - (32 lsr v);
          incr num_codes
        end
      end
    done;

    if !num_codes <> 1 && !space <> 0 then
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
    let index = (dist_rb_idx + distance_short_code_index_offset.(code)) land 3 in
    dist_rb.(index) + distance_short_code_value_offset.(code)
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

    let i = ref 0 in
    while !i < context_map_size do
      let code = Huffman.read_symbol_8 table br in
      if code = 0 then begin
        context_map.(!i) <- 0;
        incr i
      end
      else if code <= max_rle_prefix then begin
        let reps = (1 lsl code) + Bit_reader.read_bits br code in
        for _ = 0 to reps - 1 do
          if !i >= context_map_size then
            raise (Brotli_error Invalid_context_map);
          context_map.(!i) <- 0;
          incr i
        done
      end
      else begin
        context_map.(!i) <- code - max_rle_prefix;
        incr i
      end
    done;

    if Bit_reader.read_bits br 1 = 1 then
      inverse_move_to_front_transform context_map context_map_size;

    (num_trees, context_map)
  end

(* Decode block type *)
let decode_block_type max_block_type table block_type_rb block_type_rb_idx br =
  let type_code = Huffman.read_symbol_8 table br in
  let block_type =
    if type_code = 0 then
      block_type_rb.((!block_type_rb_idx) land 1)
    else if type_code = 1 then
      block_type_rb.(((!block_type_rb_idx) - 1) land 1) + 1
    else
      type_code - 2
  in
  let block_type =
    if block_type >= max_block_type then block_type - max_block_type
    else block_type
  in
  block_type_rb.((!block_type_rb_idx) land 1) <- block_type;
  incr block_type_rb_idx;
  block_type

(* Main decompression function *)
let decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos =
  let br = Bit_reader.create ~src ~pos:src_pos ~len:src_len in
  let pos = ref dst_pos in
  let max_backward_distance = ref 0 in

  (* Distance ring buffer *)
  let dist_rb = [| 16; 15; 11; 4 |] in
  let dist_rb_idx = ref 0 in

  (* Decode window bits *)
  let window_bits = decode_window_bits br in
  max_backward_distance := (1 lsl window_bits) - Constants.window_gap;

  let input_end = ref false in

  while not !input_end do
    (* Decode meta-block header *)
    let header = decode_meta_block_length br in
    input_end := header.input_end;

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
        Bit_reader.copy_bytes br ~dst ~dst_pos:!pos ~len:header.meta_block_length;
        pos := !pos + header.meta_block_length
      end
      else begin
        (* Compressed block *)
        let meta_block_remaining = ref header.meta_block_length in

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

        (* Main decode loop *)
        let context_map_slice = ref 0 in
        let dist_context_map_slice = ref 0 in
        let context_mode = ref context_modes.(block_type.(0)) in
        let huff_tree_command = ref command_trees.(0) in

        while !meta_block_remaining > 0 do
          (* Check/update command block *)
          if block_length.(1) = 0 then begin
            block_type.(1) <- decode_block_type num_block_types.(1)
              block_type_trees.(1) block_type_rb.(1) block_type_rb_idx.(1) br;
            block_length.(1) <- read_block_length block_len_trees.(1) br;
            huff_tree_command := command_trees.(block_type.(1))
          end;
          block_length.(1) <- block_length.(1) - 1;

          (* Read command code *)
          let cmd_code = Huffman.read_symbol_10 !huff_tree_command br in
          let range_idx = cmd_code lsr 6 in
          let distance_code = ref (if range_idx >= 2 then -1 else 0) in
          let range_idx = if range_idx >= 2 then range_idx - 2 else range_idx in

          (* Decode insert and copy lengths *)
          let insert_code = Prefix.insert_range_lut.(range_idx) + ((cmd_code lsr 3) land 7) in
          let copy_code = Prefix.copy_range_lut.(range_idx) + (cmd_code land 7) in
          let insert_length = Prefix.decode_insert_length br insert_code in
          let copy_length = Prefix.decode_copy_length br copy_code in

          (* Get context bytes *)
          let prev_byte1 = if !pos > dst_pos then Char.code (Bytes.get dst (!pos - 1)) else 0 in
          let prev_byte2 = if !pos > dst_pos + 1 then Char.code (Bytes.get dst (!pos - 2)) else 0 in
          let prev_byte1 = ref prev_byte1 in
          let prev_byte2 = ref prev_byte2 in

          (* Insert literals - with speculative batching optimization *)
          (* When block_length.(0) > 0, we can decode that many literals without
             checking for block boundary on each iteration *)
          let insert_remaining = ref insert_length in
          while !insert_remaining > 0 do
            (* Check if we need to switch literal block type *)
            if block_length.(0) = 0 then begin
              block_type.(0) <- decode_block_type num_block_types.(0)
                block_type_trees.(0) block_type_rb.(0) block_type_rb_idx.(0) br;
              block_length.(0) <- read_block_length block_len_trees.(0) br;
              context_map_slice := block_type.(0) lsl Constants.literal_context_bits;
              context_mode := context_modes.(block_type.(0))
            end;
            (* Batch decode: process min(remaining, block_length) literals without block checks *)
            let batch_size = min !insert_remaining block_length.(0) in
            (* Check output buffer has room for entire batch *)
            if !pos + batch_size > Bytes.length dst then
              raise (Brotli_error Output_overrun);
            (* Fast inner loop: no block boundary checks needed *)
            for _ = 0 to batch_size - 1 do
              let context = Context.get_context (Context.mode_of_int (!context_mode lsr 1))
                ~prev_byte1:!prev_byte1 ~prev_byte2:!prev_byte2 in
              let tree_idx = literal_context_map.(!context_map_slice + context) in
              prev_byte2 := !prev_byte1;
              let literal = Huffman.read_symbol_8 literal_trees.(tree_idx) br in
              prev_byte1 := literal;
              Bytes.set dst !pos (Char.chr literal);
              incr pos
            done;
            block_length.(0) <- block_length.(0) - batch_size;
            insert_remaining := !insert_remaining - batch_size
          done;

          meta_block_remaining := !meta_block_remaining - insert_length;
          if !meta_block_remaining <= 0 then
            ()  (* Break from loop *)
          else begin
            (* Decode distance if needed *)
            if !distance_code < 0 then begin
              if block_length.(2) = 0 then begin
                block_type.(2) <- decode_block_type num_block_types.(2)
                  block_type_trees.(2) block_type_rb.(2) block_type_rb_idx.(2) br;
                block_length.(2) <- read_block_length block_len_trees.(2) br;
                dist_context_map_slice := block_type.(2) lsl Constants.distance_context_bits
              end;
              block_length.(2) <- block_length.(2) - 1;
              let context = Context.distance_context copy_length in
              let tree_idx = dist_context_map.(!dist_context_map_slice + context) in
              distance_code := Huffman.read_symbol_8 distance_trees.(tree_idx) br;

              if !distance_code >= num_direct_distance_codes then begin
                distance_code := !distance_code - num_direct_distance_codes;
                let postfix = !distance_code land distance_postfix_mask in
                distance_code := !distance_code lsr distance_postfix_bits;
                let nbits = (!distance_code lsr 1) + 1 in
                let offset = ((2 + (!distance_code land 1)) lsl nbits) - 4 in
                distance_code := num_direct_distance_codes +
                  ((offset + Bit_reader.read_bits br nbits) lsl distance_postfix_bits) + postfix
              end
            end;

            (* Convert distance code to actual distance *)
            let distance = translate_short_codes !distance_code dist_rb !dist_rb_idx in
            if distance < 0 then
              raise (Brotli_error Invalid_distance);

            let max_distance = min !max_backward_distance (!pos - dst_pos) in

            if distance > max_distance then begin
              (* Dictionary reference *)
              if copy_length >= Constants.min_dictionary_word_length &&
                 copy_length <= Constants.max_dictionary_word_length then begin
                let word_id = distance - max_distance - 1 in
                let shift = Dictionary.size_bits_by_length.(copy_length) in
                let mask = (1 lsl shift) - 1 in
                let word_idx = word_id land mask in
                let transform_idx = word_id lsr shift in
                if transform_idx < Transform.num_transforms then begin
                  if !pos + copy_length > Bytes.length dst then
                    raise (Brotli_error Output_overrun);
                  let length = Transform.transform_dictionary_word
                    ~dst ~dst_pos:!pos ~word_index:word_idx
                    ~word_length:copy_length ~transform_id:transform_idx in
                  pos := !pos + length;
                  meta_block_remaining := !meta_block_remaining - length
                end
                else
                  raise (Brotli_error Invalid_backward_reference)
              end
              else
                raise (Brotli_error Invalid_backward_reference)
            end
            else begin
              (* Regular backward reference *)
              if !distance_code > 0 then begin
                dist_rb.(!dist_rb_idx land 3) <- distance;
                incr dist_rb_idx
              end;

              if copy_length > !meta_block_remaining then
                raise (Brotli_error Invalid_backward_reference);

              if !pos + copy_length > Bytes.length dst then
                raise (Brotli_error Output_overrun);

              (* Optimized copy: use blit when distance >= copy_length *)
              if distance >= copy_length then begin
                Bytes.blit dst (!pos - distance) dst !pos copy_length;
                pos := !pos + copy_length;
                meta_block_remaining := !meta_block_remaining - copy_length
              end else begin
                (* Overlapping copy - use SIMD for small distances (1-16) *)
                if Simd_copy.copy_overlapping dst !pos distance copy_length then begin
                  pos := !pos + copy_length;
                  meta_block_remaining := !meta_block_remaining - copy_length
                end else begin
                  (* Fallback: byte-by-byte for distances > 16 *)
                  for _ = 0 to copy_length - 1 do
                    Bytes.set dst !pos (Bytes.get dst (!pos - distance));
                    incr pos;
                    decr meta_block_remaining
                  done
                end
              end
            end
          end
        done
      end
    end
  done;

  !pos - dst_pos
