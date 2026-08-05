(* Prefix code tables for Brotli variable-length values *)

(* Prefix tables are packed as (offset << 8) | nbits for unboxed access.
   offset: up to 22594 (fits in upper bits)
   nbits: 0-24 (fits in lower 8 bits) *)

(* Lookup tables for command code -> insert/copy code indices (values 0-16) *)
let insert_range_lut : int8# array = [| #0s; #0s; #8s; #8s; #0s; #16s; #8s; #16s; #16s |]
let copy_range_lut : int8# array = [| #0s; #8s; #0s; #8s; #16s; #0s; #16s; #8s; #16s |]

(* Pack offset and nbits into single int: (offset << 8) | nbits *)
let[@inline always] pack_prefix offset nbits = (offset lsl 8) lor nbits
let[@inline always] unpack_offset packed = packed lsr 8
let[@inline always] unpack_nbits packed = packed land 0xFF

(* Block length prefix codes (26 entries) - packed as nativeint# *)
let block_length_prefix : nativeint# array = [|
  #0x102n; #0x502n; #0x902n; #0xD02n;   (* offset 1,5,9,13 nbits 2 *)
  #0x1103n; #0x1903n; #0x2103n; #0x2903n; (* offset 17,25,33,41 nbits 3 *)
  #0x3104n; #0x4104n; #0x5104n; #0x6104n; (* offset 49,65,81,97 nbits 4 *)
  #0x7105n; #0x9105n; #0xB105n; #0xD105n; (* offset 113,145,177,209 nbits 5 *)
  #0xF106n; #0x13106n; (* offset 241,305 nbits 6 *)
  #0x17107n; #0x1F108n; (* offset 369,497 nbits 7,8 *)
  #0x2F109n; #0x4F10An; (* offset 753,1265 nbits 9,10 *)
  #0x8F10Bn; #0x10F10Cn; (* offset 2289,4337 nbits 11,12 *)
  #0x20F10Dn; #0x40F118n  (* offset 8433,16625 nbits 13,24 *)
|]

(* Insert length prefix codes (24 entries) - packed as nativeint#
   Packed format: (offset << 8) | nbits *)
let insert_length_prefix : nativeint# array = [|
  #0x000n; #0x100n; #0x200n; #0x300n; #0x400n; #0x500n; (* offset 0-5 nbits 0 *)
  #0x601n; #0x801n; (* offset 6,8 nbits 1 *)
  #0xA02n; #0xE02n; (* offset 10,14 nbits 2 *)
  #0x1203n; #0x1A03n; (* offset 18,26 nbits 3 *)
  #0x2204n; #0x3204n; (* offset 34,50 nbits 4 *)
  #0x4205n; #0x6205n; (* offset 66,98 nbits 5 *)
  #0x8206n; #0xC207n; (* offset 130,194 nbits 6,7 *)
  #0x14208n; #0x24209n; (* offset 322,578 nbits 8,9 *)
  #0x4420An; #0x8420Cn; (* offset 1090,2114 nbits 10,12 *)
  #0x18420En; #0x584218n  (* offset 6210,22594 nbits 14,24 *)
|]

(* Copy length prefix codes (24 entries) - packed as nativeint# *)
let copy_length_prefix : nativeint# array = [|
  #0x200n; #0x300n; #0x400n; #0x500n; #0x600n; #0x700n; #0x800n; #0x900n; (* offset 2-9 nbits 0 *)
  #0xA01n; #0xC01n; (* offset 10,12 nbits 1 *)
  #0xE02n; #0x1202n; (* offset 14,18 nbits 2 *)
  #0x1603n; #0x1E03n; (* offset 22,30 nbits 3 *)
  #0x2604n; #0x3604n; (* offset 38,54 nbits 4 *)
  #0x4605n; #0x6605n; (* offset 70,102 nbits 5 *)
  #0x8606n; #0xC607n; (* offset 134,198 nbits 6,7 *)
  #0x14608n; #0x24609n; (* offset 326,582 nbits 8,9 *)
  #0x4460An; #0x84618n  (* offset 1094,2118 nbits 10,24 *)
|]

(* Helper to get prefix entry from packed nativeint# array *)
let[@inline always] get_prefix_entry (arr : nativeint# array) code =
  Nativeint_u.to_int_trunc (Oxcaml_arrays.unsafe_get arr code)

(* Helper to get range lut entry from int8# array *)
let[@inline always] get_range_lut (arr : int8# array) idx =
  Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get arr idx)

(* Decode a block length from prefix code *)
let[@inline always] decode_block_length br code =
  let packed = get_prefix_entry block_length_prefix code in
  let offset = unpack_offset packed in
  let nbits = unpack_nbits packed in
  if nbits = 0 then offset
  else offset + Bit_reader.read_bits br nbits

(* Decode insert length from prefix code *)
let[@inline always] decode_insert_length br code =
  let packed = get_prefix_entry insert_length_prefix code in
  let offset = unpack_offset packed in
  let nbits = unpack_nbits packed in
  if nbits = 0 then offset
  else offset + Bit_reader.read_bits br nbits

(* Decode copy length from prefix code *)
let[@inline always] decode_copy_length br code =
  let packed = get_prefix_entry copy_length_prefix code in
  let offset = unpack_offset packed in
  let nbits = unpack_nbits packed in
  if nbits = 0 then offset
  else offset + Bit_reader.read_bits br nbits

(* Get insert and copy length codes from command code *)
let[@inline] get_insert_length_code cmd_code =
  let range_idx = cmd_code lsr 6 in
  let insert_code_base = get_range_lut insert_range_lut range_idx in
  insert_code_base + ((cmd_code lsr 3) land 7)

let[@inline] get_copy_length_code cmd_code =
  let range_idx = cmd_code lsr 6 in
  let copy_code_base = get_range_lut copy_range_lut range_idx in
  copy_code_base + (cmd_code land 7)

(* Command code to distance context mapping *)
let[@inline] command_code_to_distance_context cmd_code =
  if cmd_code < 128 then 0
  else if cmd_code < 256 then 1
  else if cmd_code < 384 then 2
  else 3
