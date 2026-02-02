(* Brotli format constants from RFC 7932 *)

(* Specification: 2. Compressed representation overview *)
let max_number_of_block_types = 256

(* Specification: 3.3. Alphabet sizes *)
let num_literal_symbols = 256
let num_command_symbols = 704
let num_block_len_symbols = 26
let num_ins_copy_codes = 24

(* Specification: 3.5. Complex prefix codes *)
let repeat_previous_code_length = 16
let repeat_zero_code_length = 17
let code_length_codes = 18  (* repeat_zero_code_length + 1 *)
let initial_repeated_code_length = 8

(* Specification: 7.3. Encoding of the context map *)
let context_map_max_rle = 16
let max_context_map_symbols = max_number_of_block_types + context_map_max_rle
let max_block_type_symbols = max_number_of_block_types + 2

(* Specification: 7.1. Context modes and context ID lookup for literals *)
let literal_context_bits = 6
let num_literal_contexts = 1 lsl literal_context_bits  (* 64 *)

(* Specification: 7.2. Context ID for distances *)
let distance_context_bits = 2
let num_distance_contexts = 1 lsl distance_context_bits  (* 4 *)

(* Specification: 4. Encoding of distances *)
let num_distance_short_codes = 16
let max_npostfix = 3
let max_ndirect = 120
let max_distance_bits = 24

(* Large window brotli *)
let large_max_distance_bits = 62
let large_min_wbits = 10
let large_max_wbits = 30

(* Calculate distance alphabet size *)
let distance_alphabet_size ~npostfix ~ndirect ~max_nbits =
  num_distance_short_codes + ndirect + (max_nbits lsl (npostfix + 1))

(* Standard distance alphabet size *)
let num_distance_symbols =
  distance_alphabet_size ~npostfix:max_npostfix ~ndirect:max_ndirect
    ~max_nbits:large_max_distance_bits

(* Maximum expressible distance with NPOSTFIX=0, NDIRECT=0 *)
let max_distance = 0x3FFFFFC  (* (1 lsl 26) - 4 *)

(* Specification: 9.1. Format of the Stream Header *)
let window_gap = 16
let min_window_bits = 10
let max_window_bits = 24

let max_backward_limit wbits = (1 lsl wbits) - window_gap

(* Huffman coding constants *)
let huffman_max_code_length = 15
let huffman_max_code_length_code_length = 5
let huffman_max_table_bits = 8  (* Root table size for literals *)
let huffman_max_command_table_bits = 10  (* Root table size for commands *)

(* Code length code order (RFC 7932 section 3.5) *)
let code_length_code_order = [|
  1; 2; 3; 4; 0; 5; 17; 6; 16; 7; 8; 9; 10; 11; 12; 13; 14; 15
|]

(* Minimum dictionary word length *)
let min_dictionary_word_length = 4
let max_dictionary_word_length = 24

(* Number of transforms *)
let num_transforms = 121

(* ============================================================
   Shared utility functions
   ============================================================ *)

(* Hash multiplier for 4-byte hash functions (from brotli-c) *)
let hash_multiplier = 0x1e35a7bd

(* Fast log2 approximation matching brotli-c FastLog2.
   Returns floor(log2(v)) as a float, or 0.0 for v <= 0. *)
let[@inline always] fast_log2 v =
  if v <= 0 then 0.0
  else
    let rec log2_floor v acc = if v <= 1 then acc else log2_floor (v lsr 1) (acc + 1) in
    float_of_int (log2_floor v 0)

(* Hash a 4-byte sequence from a bytes buffer.
   Returns a hash value with the specified number of bits. *)
let[@inline always] hash4_bytes src pos bits =
  (* Use native 32-bit load instead of byte-by-byte loading *)
  let v = Int32.to_int (Bytes.get_int32_le src pos) land 0xFFFFFFFF in
  ((v * hash_multiplier) land 0xFFFFFFFF) lsr (32 - bits)

(* Hash a 4-byte sequence from a string.
   Returns a hash value with the specified number of bits. *)
let[@inline always] hash4_string s pos bits =
  (* Use native 32-bit load instead of byte-by-byte loading *)
  let v = Int32.to_int (String.get_int32_le s pos) land 0xFFFFFFFF in
  ((v * hash_multiplier) land 0xFFFFFFFF) lsr (32 - bits)
