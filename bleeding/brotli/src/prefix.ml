(* Prefix code tables for Brotli variable-length values *)

(* A prefix code range: [offset, offset + 2^nbits) *)
type prefix = {
  offset : int;
  nbits : int;
}

(* Lookup tables for command code -> insert/copy code indices *)
let insert_range_lut = [| 0; 0; 8; 8; 0; 16; 8; 16; 16 |]
let copy_range_lut = [| 0; 8; 0; 8; 16; 0; 16; 8; 16 |]

(* Block length prefix codes (26 entries) *)
let block_length_prefix = [|
  { offset = 1; nbits = 2 };     { offset = 5; nbits = 2 };
  { offset = 9; nbits = 2 };     { offset = 13; nbits = 2 };
  { offset = 17; nbits = 3 };    { offset = 25; nbits = 3 };
  { offset = 33; nbits = 3 };    { offset = 41; nbits = 3 };
  { offset = 49; nbits = 4 };    { offset = 65; nbits = 4 };
  { offset = 81; nbits = 4 };    { offset = 97; nbits = 4 };
  { offset = 113; nbits = 5 };   { offset = 145; nbits = 5 };
  { offset = 177; nbits = 5 };   { offset = 209; nbits = 5 };
  { offset = 241; nbits = 6 };   { offset = 305; nbits = 6 };
  { offset = 369; nbits = 7 };   { offset = 497; nbits = 8 };
  { offset = 753; nbits = 9 };   { offset = 1265; nbits = 10 };
  { offset = 2289; nbits = 11 }; { offset = 4337; nbits = 12 };
  { offset = 8433; nbits = 13 }; { offset = 16625; nbits = 24 };
|]

(* Insert length prefix codes (24 entries) *)
let insert_length_prefix = [|
  { offset = 0; nbits = 0 };     { offset = 1; nbits = 0 };
  { offset = 2; nbits = 0 };     { offset = 3; nbits = 0 };
  { offset = 4; nbits = 0 };     { offset = 5; nbits = 0 };
  { offset = 6; nbits = 1 };     { offset = 8; nbits = 1 };
  { offset = 10; nbits = 2 };    { offset = 14; nbits = 2 };
  { offset = 18; nbits = 3 };    { offset = 26; nbits = 3 };
  { offset = 34; nbits = 4 };    { offset = 50; nbits = 4 };
  { offset = 66; nbits = 5 };    { offset = 98; nbits = 5 };
  { offset = 130; nbits = 6 };   { offset = 194; nbits = 7 };
  { offset = 322; nbits = 8 };   { offset = 578; nbits = 9 };
  { offset = 1090; nbits = 10 }; { offset = 2114; nbits = 12 };
  { offset = 6210; nbits = 14 }; { offset = 22594; nbits = 24 };
|]

(* Copy length prefix codes (24 entries) *)
let copy_length_prefix = [|
  { offset = 2; nbits = 0 };     { offset = 3; nbits = 0 };
  { offset = 4; nbits = 0 };     { offset = 5; nbits = 0 };
  { offset = 6; nbits = 0 };     { offset = 7; nbits = 0 };
  { offset = 8; nbits = 0 };     { offset = 9; nbits = 0 };
  { offset = 10; nbits = 1 };    { offset = 12; nbits = 1 };
  { offset = 14; nbits = 2 };    { offset = 18; nbits = 2 };
  { offset = 22; nbits = 3 };    { offset = 30; nbits = 3 };
  { offset = 38; nbits = 4 };    { offset = 54; nbits = 4 };
  { offset = 70; nbits = 5 };    { offset = 102; nbits = 5 };
  { offset = 134; nbits = 6 };   { offset = 198; nbits = 7 };
  { offset = 326; nbits = 8 };   { offset = 582; nbits = 9 };
  { offset = 1094; nbits = 10 }; { offset = 2118; nbits = 24 };
|]

(* Decode a block length from prefix code *)
let[@inline always] decode_block_length br code =
  let p = block_length_prefix.(code) in
  if p.nbits = 0 then p.offset
  else p.offset + Bit_reader.read_bits br p.nbits

(* Decode insert length from prefix code *)
let[@inline always] decode_insert_length br code =
  let p = insert_length_prefix.(code) in
  if p.nbits = 0 then p.offset
  else p.offset + Bit_reader.read_bits br p.nbits

(* Decode copy length from prefix code *)
let[@inline always] decode_copy_length br code =
  let p = copy_length_prefix.(code) in
  if p.nbits = 0 then p.offset
  else p.offset + Bit_reader.read_bits br p.nbits

(* Get insert and copy length codes from command code *)
let[@inline] get_insert_length_code cmd_code =
  let range_idx = cmd_code lsr 6 in
  let insert_code_base = insert_range_lut.(range_idx) in
  insert_code_base + ((cmd_code lsr 3) land 7)

let[@inline] get_copy_length_code cmd_code =
  let range_idx = cmd_code lsr 6 in
  let copy_code_base = copy_range_lut.(range_idx) in
  copy_code_base + (cmd_code land 7)

(* Command code to distance context mapping *)
let[@inline] command_code_to_distance_context cmd_code =
  if cmd_code < 128 then 0
  else if cmd_code < 256 then 1
  else if cmd_code < 384 then 2
  else 3
