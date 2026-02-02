(* Stack-local decoder state for Brotli decompression.
   Pre-allocates all scratch buffers to enable zero-allocation decoding.
   Uses OxCaml unboxed nativeint# fields for hot-path scalar state. *)

(* Maximum sizes based on Brotli spec *)
let max_alphabet_size = 704  (* max for command symbols *)
let max_block_types = 256
let max_literal_trees = 256
let max_distance_trees = 256
let max_context_map_size = max_block_types lsl 6  (* 16384 *)
let huffman_slab_size = 131072  (* 128KB - enough for all tables in a meta-block *)

(* Decoder scratch buffers - allocated once, reused across meta-blocks.
   Mixed block: boxed fields first, unboxed nativeint# fields at the end. *)
type t = {
  (* === Boxed reference fields (must come first in mixed block) === *)

  (* Source bytes - reference only *)
  mutable src : bytes;

  (* Huffman building scratch *)
  count : int array;                    (* 16 entries *)
  huff_offset : int array;              (* 16 entries - renamed from offset *)
  sorted_symbols : int array;           (* max_alphabet_size *)
  code_lengths : int array;             (* max_alphabet_size *)
  code_length_code_lengths : int array; (* 18 entries *)

  (* Huffman table slab - all tables allocated from here *)
  huffman_slab : int array;

  (* Block management - fixed 3 entries *)
  num_block_types : int array;
  block_type : int array;
  block_length : int array;
  block_type_rb : int array;            (* 6 = 3*2 flattened: [bt0_0; bt0_1; bt1_0; bt1_1; bt2_0; bt2_1] *)
  block_type_rb_idx : int array;        (* 3 entries *)

  (* Huffman table offsets into slab (offset, size pairs) *)
  block_type_tree_info : int array;     (* 6 = 3*2: [off0, size0, off1, size1, off2, size2] *)
  block_len_tree_info : int array;      (* 6 = 3*2 *)

  (* Variable-size Huffman table offsets *)
  literal_tree_info : int array;        (* max_literal_trees * 2 *)
  command_tree_info : int array;        (* max_block_types * 2 *)
  distance_tree_info : int array;       (* max_distance_trees * 2 *)

  (* Context maps *)
  literal_context_map : int array;
  dist_context_map : int array;
  context_modes : int array;

  (* Distance ring buffer *)
  dist_rb : int array;

  (* MTF transform scratch *)
  mtf : int array;

  (* === Unboxed scalar fields (must come at the end of mixed block) === *)

  (* Source length *)
  mutable src_len : nativeint#;

  (* Bit reader state - mutable fields for hot path *)
  mutable bit_next_pos : nativeint#;
  mutable bit_val : nativeint#;
  mutable bit_pos : nativeint#;

  (* Huffman slab allocation position *)
  mutable huffman_slab_pos : nativeint#;

  (* Tree counts *)
  mutable num_literal_trees : nativeint#;
  mutable num_distance_trees : nativeint#;

  (* Context map sizes *)
  mutable literal_context_map_size : nativeint#;
  mutable dist_context_map_size : nativeint#;

  (* Distance parameters per meta-block *)
  mutable distance_postfix_bits : nativeint#;
  mutable num_direct_distance_codes : nativeint#;
  mutable distance_postfix_mask : nativeint#;
  mutable num_distance_codes : nativeint#;

  (* Distance ring buffer index *)
  mutable dist_rb_idx : nativeint#;

  (* Window/stream state *)
  mutable max_backward_distance : nativeint#;
  mutable window_bits : nativeint#;
}

(* Create decoder state - call once, reuse for multiple decompressions *)
let create () = {
  src = Bytes.empty;

  count = Array.make 16 0;
  huff_offset = Array.make 16 0;
  sorted_symbols = Array.make max_alphabet_size 0;
  code_lengths = Array.make max_alphabet_size 0;
  code_length_code_lengths = Array.make Constants.code_length_codes 0;

  huffman_slab = Array.make huffman_slab_size 0;

  num_block_types = Array.make 3 1;
  block_type = Array.make 3 0;
  block_length = Array.make 3 0;
  block_type_rb = Array.make 6 0;  (* 3 * 2 flattened *)
  block_type_rb_idx = Array.make 3 0;

  block_type_tree_info = Array.make 6 0;
  block_len_tree_info = Array.make 6 0;

  literal_tree_info = Array.make (max_literal_trees * 2) 0;
  command_tree_info = Array.make (max_block_types * 2) 0;
  distance_tree_info = Array.make (max_distance_trees * 2) 0;

  literal_context_map = Array.make max_context_map_size 0;
  dist_context_map = Array.make (max_block_types * 4) 0;
  context_modes = Array.make max_block_types 0;

  dist_rb = [| 16; 15; 11; 4 |];

  mtf = Array.init 256 (fun i -> i);

  (* Unboxed scalar fields *)
  src_len = #0n;
  bit_next_pos = #0n;
  bit_val = #0n;
  bit_pos = #0n;
  huffman_slab_pos = #0n;
  num_literal_trees = #0n;
  num_distance_trees = #0n;
  literal_context_map_size = #0n;
  dist_context_map_size = #0n;
  distance_postfix_bits = #0n;
  num_direct_distance_codes = #0n;
  distance_postfix_mask = #0n;
  num_distance_codes = #0n;
  dist_rb_idx = #0n;
  max_backward_distance = #0n;
  window_bits = #0n;
}

(* Reset for new decompression *)
let reset t ~src ~pos ~len =
  t.src <- src;
  t.src_len <- Nativeint_u.of_int (pos + len);
  t.bit_next_pos <- Nativeint_u.of_int pos;
  t.bit_val <- #0n;
  t.bit_pos <- #0n;
  t.dist_rb.(0) <- 16;
  t.dist_rb.(1) <- 15;
  t.dist_rb.(2) <- 11;
  t.dist_rb.(3) <- 4;
  t.dist_rb_idx <- #0n;
  t.max_backward_distance <- #0n;
  t.window_bits <- #0n

(* Reset Huffman slab for new meta-block *)
let[@inline always] reset_huffman_slab t =
  t.huffman_slab_pos <- #0n

(* Allocate space in Huffman slab, returns offset *)
let[@inline always] alloc_huffman_table t size =
  let off = Nativeint_u.to_int_trunc t.huffman_slab_pos in
  t.huffman_slab_pos <- Nativeint_u.of_int (off + size);
  if off + size > huffman_slab_size then
    failwith "Huffman slab overflow";
  off

(* Reset block state for new meta-block *)
let reset_block_state t =
  for i = 0 to 2 do
    t.num_block_types.(i) <- 1;
    t.block_type.(i) <- 0;
    t.block_length.(i) <- 1 lsl 28;
    t.block_type_rb.(i * 2) <- 0;
    t.block_type_rb.(i * 2 + 1) <- 1;
    t.block_type_rb_idx.(i) <- 0
  done

(* Bit reader operations on decoder state - CRITICAL HOT PATH *)

let bit_masks = [|
  0x0; 0x1; 0x3; 0x7;
  0xF; 0x1F; 0x3F; 0x7F;
  0xFF; 0x1FF; 0x3FF; 0x7FF;
  0xFFF; 0x1FFF; 0x3FFF; 0x7FFF;
  0xFFFF; 0x1FFFF; 0x3FFFF; 0x7FFFF;
  0xFFFFF; 0x1FFFFF; 0x3FFFFF; 0x7FFFFF;
  0xFFFFFF; 0x1FFFFFF;
|]

let[@inline always] bit_mask n =
  if n <= 24 then Array.unsafe_get bit_masks n
  else (1 lsl n) - 1

let[@inline always] fill_bit_window t =
  let bit_pos = Nativeint_u.to_int_trunc t.bit_pos in
  if bit_pos <= 32 then begin
    let next_pos = Nativeint_u.to_int_trunc t.bit_next_pos in
    let src_len = Nativeint_u.to_int_trunc t.src_len in
    if next_pos + 4 <= src_len then begin
      let b0 = Char.code (Bytes.unsafe_get t.src next_pos) in
      let b1 = Char.code (Bytes.unsafe_get t.src (next_pos + 1)) in
      let b2 = Char.code (Bytes.unsafe_get t.src (next_pos + 2)) in
      let b3 = Char.code (Bytes.unsafe_get t.src (next_pos + 3)) in
      let new_bits = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
      let bit_val = Nativeint_u.to_int_trunc t.bit_val in
      t.bit_val <- Nativeint_u.of_int (bit_val lor (new_bits lsl bit_pos));
      t.bit_pos <- Nativeint_u.of_int (bit_pos + 32);
      t.bit_next_pos <- Nativeint_u.of_int (next_pos + 4)
    end else begin
      let mutable bp = bit_pos in
      let mutable np = next_pos in
      let mutable bv = Nativeint_u.to_int_trunc t.bit_val in
      while bp <= 56 && np < src_len do
        let b = Char.code (Bytes.unsafe_get t.src np) in
        bv <- bv lor (b lsl bp);
        bp <- bp + 8;
        np <- np + 1
      done;
      t.bit_val <- Nativeint_u.of_int bv;
      t.bit_pos <- Nativeint_u.of_int bp;
      t.bit_next_pos <- Nativeint_u.of_int np
    end
  end

let[@inline always] peek_bits t n_bits =
  let bit_pos = Nativeint_u.to_int_trunc t.bit_pos in
  if bit_pos < n_bits then fill_bit_window t;
  (Nativeint_u.to_int_trunc t.bit_val) land bit_mask n_bits

let[@inline always] skip_bits t n_bits =
  let bit_val = Nativeint_u.to_int_trunc t.bit_val in
  let bit_pos = Nativeint_u.to_int_trunc t.bit_pos in
  t.bit_val <- Nativeint_u.of_int (bit_val lsr n_bits);
  t.bit_pos <- Nativeint_u.of_int (bit_pos - n_bits)

let[@inline always] read_bits t n_bits =
  let bit_pos = Nativeint_u.to_int_trunc t.bit_pos in
  if bit_pos < n_bits then fill_bit_window t;
  let bit_val = Nativeint_u.to_int_trunc t.bit_val in
  let value = bit_val land bit_mask n_bits in
  t.bit_val <- Nativeint_u.of_int (bit_val lsr n_bits);
  t.bit_pos <- Nativeint_u.of_int (Nativeint_u.to_int_trunc t.bit_pos - n_bits);
  value

let[@inline always] read_bit t =
  let bit_pos = Nativeint_u.to_int_trunc t.bit_pos in
  if bit_pos < 1 then fill_bit_window t;
  let bit_val = Nativeint_u.to_int_trunc t.bit_val in
  let value = bit_val land 1 in
  t.bit_val <- Nativeint_u.of_int (bit_val lsr 1);
  t.bit_pos <- Nativeint_u.of_int (Nativeint_u.to_int_trunc t.bit_pos - 1);
  value

let[@inline] align_to_byte t =
  let bit_pos = Nativeint_u.to_int_trunc t.bit_pos in
  let extra = bit_pos land 7 in
  if extra <> 0 then begin
    let bit_val = Nativeint_u.to_int_trunc t.bit_val in
    t.bit_val <- Nativeint_u.of_int (bit_val lsr extra);
    t.bit_pos <- Nativeint_u.of_int (bit_pos - extra)
  end

let copy_bytes t ~dst ~dst_pos ~len =
  align_to_byte t;
  if len > 0 then begin
    let mutable copied = 0 in
    let mutable dpos = dst_pos in
    let mutable bit_pos = Nativeint_u.to_int_trunc t.bit_pos in
    let mutable bit_val = Nativeint_u.to_int_trunc t.bit_val in
    while bit_pos >= 8 && copied < len do
      Bytes.unsafe_set dst dpos (Char.unsafe_chr (bit_val land 0xFF));
      bit_val <- bit_val lsr 8;
      bit_pos <- bit_pos - 8;
      dpos <- dpos + 1;
      copied <- copied + 1
    done;
    t.bit_val <- Nativeint_u.of_int bit_val;
    t.bit_pos <- Nativeint_u.of_int bit_pos;
    let remaining = len - copied in
    if remaining > 0 then begin
      let next_pos = Nativeint_u.to_int_trunc t.bit_next_pos in
      Bytes.blit t.src next_pos dst dpos remaining;
      t.bit_next_pos <- Nativeint_u.of_int (next_pos + remaining)
    end
  end
