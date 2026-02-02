(* Context modeling lookup tables for Brotli (RFC 7932 Section 7) *)

(* Context modes *)
type mode = LSB6 | MSB6 | UTF8 | SIGNED

let[@inline always] mode_of_int = function
  | 0 -> LSB6
  | 1 -> MSB6
  | 2 -> UTF8
  | 3 -> SIGNED
  | _ -> invalid_arg "Invalid context mode"

let int_of_mode = function
  | LSB6 -> 0
  | MSB6 -> 1
  | UTF8 -> 2
  | SIGNED -> 3

(* The master lookup table - all context modes combined
   Source data as int array, converted to Bytes for compact storage *)
let lookup_data = [|
  (* CONTEXT_UTF8, last byte (offset 0) *)
  (* ASCII range *)
  0;  0;  0;  0;  0;  0;  0;  0;  0;  4;  4;  0;  0;  4;  0;  0;
  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;
  8; 12; 16; 12; 12; 20; 12; 16; 24; 28; 12; 12; 32; 12; 36; 12;
  44; 44; 44; 44; 44; 44; 44; 44; 44; 44; 32; 32; 24; 40; 28; 12;
  12; 48; 52; 52; 52; 48; 52; 52; 52; 48; 52; 52; 52; 52; 52; 48;
  52; 52; 52; 52; 52; 48; 52; 52; 52; 52; 52; 24; 12; 28; 12; 12;
  12; 56; 60; 60; 60; 56; 60; 60; 60; 56; 60; 60; 60; 60; 60; 56;
  60; 60; 60; 60; 60; 56; 60; 60; 60; 60; 60; 24; 12; 28; 12;  0;
  (* UTF8 continuation byte range *)
  0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1;
  0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1;
  0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1;
  0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1;
  (* UTF8 lead byte range *)
  2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3;
  2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3;
  2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3;
  2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3;

  (* CONTEXT_UTF8 second last byte (offset 256) *)
  (* ASCII range *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 1; 1; 1; 1; 1;
  1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 1; 1; 1; 1;
  1; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 1; 1; 1; 1; 0;
  (* UTF8 continuation byte range *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  (* UTF8 lead byte range *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;

  (* CONTEXT_SIGNED, second last byte (offset 512) *)
  0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
  4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
  4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
  4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
  5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5;
  5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5;
  5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5;
  6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 7;

  (* CONTEXT_SIGNED, last byte (offset 768) - same as above shifted by 3 bits *)
  0;  8;  8;  8;  8;  8;  8;  8;  8;  8;  8;  8;  8;  8;  8;  8;
  16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16;
  16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16;
  16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16;
  24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24;
  24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24;
  24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24;
  24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24;
  32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32;
  32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32;
  32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32;
  32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32; 32;
  40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40;
  40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40;
  40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40; 40;
  48; 48; 48; 48; 48; 48; 48; 48; 48; 48; 48; 48; 48; 48; 48; 56;

  (* CONTEXT_LSB6, last byte (offset 1024) *)
   0;  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12; 13; 14; 15;
  16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31;
  32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47;
  48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58; 59; 60; 61; 62; 63;
   0;  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12; 13; 14; 15;
  16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31;
  32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47;
  48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58; 59; 60; 61; 62; 63;
   0;  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12; 13; 14; 15;
  16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31;
  32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47;
  48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58; 59; 60; 61; 62; 63;
   0;  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12; 13; 14; 15;
  16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31;
  32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47;
  48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58; 59; 60; 61; 62; 63;

  (* CONTEXT_MSB6, last byte (offset 1280) *)
   0;  0;  0;  0;  1;  1;  1;  1;  2;  2;  2;  2;  3;  3;  3;  3;
   4;  4;  4;  4;  5;  5;  5;  5;  6;  6;  6;  6;  7;  7;  7;  7;
   8;  8;  8;  8;  9;  9;  9;  9; 10; 10; 10; 10; 11; 11; 11; 11;
  12; 12; 12; 12; 13; 13; 13; 13; 14; 14; 14; 14; 15; 15; 15; 15;
  16; 16; 16; 16; 17; 17; 17; 17; 18; 18; 18; 18; 19; 19; 19; 19;
  20; 20; 20; 20; 21; 21; 21; 21; 22; 22; 22; 22; 23; 23; 23; 23;
  24; 24; 24; 24; 25; 25; 25; 25; 26; 26; 26; 26; 27; 27; 27; 27;
  28; 28; 28; 28; 29; 29; 29; 29; 30; 30; 30; 30; 31; 31; 31; 31;
  32; 32; 32; 32; 33; 33; 33; 33; 34; 34; 34; 34; 35; 35; 35; 35;
  36; 36; 36; 36; 37; 37; 37; 37; 38; 38; 38; 38; 39; 39; 39; 39;
  40; 40; 40; 40; 41; 41; 41; 41; 42; 42; 42; 42; 43; 43; 43; 43;
  44; 44; 44; 44; 45; 45; 45; 45; 46; 46; 46; 46; 47; 47; 47; 47;
  48; 48; 48; 48; 49; 49; 49; 49; 50; 50; 50; 50; 51; 51; 51; 51;
  52; 52; 52; 52; 53; 53; 53; 53; 54; 54; 54; 54; 55; 55; 55; 55;
  56; 56; 56; 56; 57; 57; 57; 57; 58; 58; 58; 58; 59; 59; 59; 59;
  60; 60; 60; 60; 61; 61; 61; 61; 62; 62; 62; 62; 63; 63; 63; 63;

  (* CONTEXT_{M,L}SB6, second last byte (offset 1536) - all zeros *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
|]

(* Convert lookup_data to Bytes for compact storage (1 byte per value instead of 8) *)
let lookup : bytes =
  let n = Array.length lookup_data in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.unsafe_set b i (Char.chr lookup_data.(i))
  done;
  b

(* Offsets into lookup table for each mode - stored as int8# for compact access *)
let lookup_offset1 : int8# array =
  let open struct
    let i n = Oxcaml_arrays.int_to_int8 n
  end in
  [| i 4; i 5; i 0; i 3 |]  (* offset1 / 256: LSB6=4, MSB6=5, UTF8=0, SIGNED=3 *)

let lookup_offset2 : int8# array =
  let open struct
    let i n = Oxcaml_arrays.int_to_int8 n
  end in
  [| i 6; i 6; i 1; i 2 |]  (* offset2 / 256: LSB6=6, MSB6=6, UTF8=1, SIGNED=2 *)

(* Get context ID from previous two bytes
   Optimization: Uses Bytes lookup (1 byte per entry) and int8# offset arrays.
   The lor combines two 6-bit values into the final context ID. *)
let[@inline always] get_context mode ~prev_byte1 ~prev_byte2 =
  let mode_idx = int_of_mode mode in
  (* Get offsets from int8# arrays - values are offset/256, multiply by 256 = lsl 8 *)
  let off1 = Oxcaml_arrays.int8_to_int (Oxcaml_arrays.unsafe_get lookup_offset1 mode_idx) lsl 8 in
  let off2 = Oxcaml_arrays.int8_to_int (Oxcaml_arrays.unsafe_get lookup_offset2 mode_idx) lsl 8 in
  let v1 = Char.code (Bytes.unsafe_get lookup (off1 + prev_byte1)) in
  let v2 = Char.code (Bytes.unsafe_get lookup (off2 + prev_byte2)) in
  v1 lor v2

(* Distance context based on copy length *)
let[@inline always] distance_context copy_length =
  if copy_length > 4 then 3 else copy_length - 2

(* ==========================================================================
   OPTIMIZED CONTEXT LOOKUP FOR DECODER HOT PATH
   ==========================================================================
   Following C brotli's approach: use a pre-computed "context_lut" pointer
   that combines mode-specific offset into a single base.

   The C brotli macro: BROTLI_CONTEXT(P1, P2, LUT) = LUT[P1] | (LUT+256)[P2]

   We provide:
   1. get_context_lut: mode_int -> (offset1, offset2) - called once per block switch
   2. get_context_fast: inline context computation using precomputed offsets
   ========================================================================== *)

(* Context lookup table offsets indexed by mode (0=LSB6, 1=MSB6, 2=UTF8, 3=SIGNED)
   Returns (offset1 * 256, offset2 * 256) to avoid multiplication in hot path *)
let context_lut_offsets = [|
  (* LSB6: offset1=4*256=1024, offset2=6*256=1536 *)
  (1024, 1536);
  (* MSB6: offset1=5*256=1280, offset2=6*256=1536 *)
  (1280, 1536);
  (* UTF8: offset1=0*256=0, offset2=1*256=256 *)
  (0, 256);
  (* SIGNED: offset1=3*256=768, offset2=2*256=512 *)
  (768, 512);
|]

(* Get context lookup offsets for a mode. Called once when block type changes.
   mode_int is context_mode value from decoder (0-3 for LSB6/MSB6/UTF8/SIGNED) *)
let[@inline always] get_context_lut mode_int =
  Array.unsafe_get context_lut_offsets (mode_int land 3)

(* Fast context computation using precomputed offsets.
   This is the hot path version - avoids mode_of_int/int_of_mode overhead.

   SAFETY: offset1 and offset2 are always valid table indices (0-1536 range),
   prev_byte1 and prev_byte2 are 0-255, so all lookups are within the 1792-byte table. *)
let[@inline always] get_context_fast ~offset1 ~offset2 ~prev_byte1 ~prev_byte2 =
  let v1 = Char.code (Bytes.unsafe_get lookup (offset1 + prev_byte1)) in
  let v2 = Char.code (Bytes.unsafe_get lookup (offset2 + prev_byte2)) in
  v1 lor v2
