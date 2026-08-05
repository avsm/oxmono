(* Snappy compression/decompression - Pure OCaml implementation
   Optimized for minimal allocations and high performance *)

(* Tag types - lower 2 bits of tag byte *)
let tag_literal = 0
let tag_copy1 = 1  (* 1-byte offset *)
let tag_copy2 = 2  (* 2-byte offset *)
let tag_copy4 = 3  (* 4-byte offset *)

(* Constants *)
let max_hash_table_bits = 14
let max_hash_table_size = 1 lsl max_hash_table_bits
let max_offset = 1 lsl 15  (* 32KB window *)

(* Error type *)
type error =
  | Truncated_input
  | Invalid_literal_length
  | Invalid_copy_offset
  | Output_overrun
  | Invalid_varint

let error_to_string = function
  | Truncated_input -> "Truncated input"
  | Invalid_literal_length -> "Invalid literal length"
  | Invalid_copy_offset -> "Invalid copy offset (zero or exceeds output position)"
  | Output_overrun -> "Output buffer overrun"
  | Invalid_varint -> "Invalid varint encoding"

exception Snappy_error of error

(* ============================================================
   Low-level byte access - use unsafe for verified hot paths
   ============================================================ *)

let[@inline always] get_u8 b i =
  Char.code (Bytes.unsafe_get b i)

let[@inline always] set_u8 b i v =
  Bytes.unsafe_set b i (Char.unsafe_chr v)

let[@inline always] get_u16_le b i =
  get_u8 b i lor (get_u8 b (i + 1) lsl 8)

let[@inline always] get_u32_le b i =
  get_u8 b i
  lor (get_u8 b (i + 1) lsl 8)
  lor (get_u8 b (i + 2) lsl 16)
  lor (get_u8 b (i + 3) lsl 24)

(* 64-bit load for match length comparison *)
let[@inline always] get_u64_le b i =
  let lo = get_u32_le b i in
  let hi = get_u32_le b (i + 4) in
  Int64.(logor (of_int lo) (shift_left (of_int hi) 32))

let[@inline always] set_u16_le b i v =
  set_u8 b i (v land 0xFF);
  set_u8 b (i + 1) ((v lsr 8) land 0xFF)

let[@inline always] set_u32_le b i v =
  set_u8 b i (v land 0xFF);
  set_u8 b (i + 1) ((v lsr 8) land 0xFF);
  set_u8 b (i + 2) ((v lsr 16) land 0xFF);
  set_u8 b (i + 3) ((v lsr 24) land 0xFF)

(* Count trailing zeros in a 64-bit value - optimized for finding first differing byte *)
let[@inline] ctz64 x =
  (* This is the de Bruijn multiplication method for counting trailing zeros *)
  if x = 0L then 64
  else
    (* On OCaml 5.x we could use Int64.count_trailing_zeros, but for portability: *)
    let open Int64 in
    let n = ref 0 in
    let x = ref x in
    if logand !x 0xFFFFFFFFL = 0L then begin n := !n + 32; x := shift_right_logical !x 32 end;
    if logand !x 0xFFFFL = 0L then begin n := !n + 16; x := shift_right_logical !x 16 end;
    if logand !x 0xFFL = 0L then begin n := !n + 8; x := shift_right_logical !x 8 end;
    if logand !x 0xFL = 0L then begin n := !n + 4; x := shift_right_logical !x 4 end;
    if logand !x 0x3L = 0L then begin n := !n + 2; x := shift_right_logical !x 2 end;
    if logand !x 0x1L = 0L then n := !n + 1;
    !n

(* ============================================================
   Varint encoding/decoding
   ============================================================ *)

(* Decode varint, returns (value, bytes_consumed) or raises *)
let decode_varint src ~pos ~len =
  let limit = pos + len in
  let rec loop acc shift i =
    if i >= limit then raise (Snappy_error Truncated_input)
    else
      let b = get_u8 src i in
      let acc = acc lor ((b land 0x7F) lsl shift) in
      if b land 0x80 = 0 then (acc, i - pos + 1)
      else if shift >= 28 then raise (Snappy_error Invalid_varint)
      else loop acc (shift + 7) (i + 1)
  in
  loop 0 0 pos

(* Encode varint, returns bytes written. dst must have space for 5 bytes *)
let[@inline] encode_varint dst ~pos value =
  let rec loop v i =
    if v < 0x80 then begin
      set_u8 dst i v;
      i - pos + 1
    end else begin
      set_u8 dst i ((v land 0x7F) lor 0x80);
      loop (v lsr 7) (i + 1)
    end
  in
  loop value pos

(* Compute varint length without encoding *)
let[@inline] varint_length v =
  if v < 1 lsl 7 then 1
  else if v < 1 lsl 14 then 2
  else if v < 1 lsl 21 then 3
  else if v < 1 lsl 28 then 4
  else 5

(* ============================================================
   Decompression
   ============================================================ *)

(* Pattern extension for small offsets - used by copy operations.
   For offset < 8, we can optimize by extending the pattern first,
   then copying in larger chunks. This is a key optimization from snappy-c. *)
let[@inline] extend_pattern_small dst ~dst_pos ~offset =
  (* Replicate pattern to fill at least 8 bytes *)
  let src_pos = dst_pos - offset in
  match offset with
  | 1 ->
    (* Repeat single byte 8 times - common RLE case *)
    let b = get_u8 dst src_pos in
    set_u8 dst dst_pos b;
    set_u8 dst (dst_pos + 1) b;
    set_u8 dst (dst_pos + 2) b;
    set_u8 dst (dst_pos + 3) b;
    set_u8 dst (dst_pos + 4) b;
    set_u8 dst (dst_pos + 5) b;
    set_u8 dst (dst_pos + 6) b;
    set_u8 dst (dst_pos + 7) b
  | 2 ->
    let b0 = get_u8 dst src_pos in
    let b1 = get_u8 dst (src_pos + 1) in
    set_u8 dst dst_pos b0;
    set_u8 dst (dst_pos + 1) b1;
    set_u8 dst (dst_pos + 2) b0;
    set_u8 dst (dst_pos + 3) b1;
    set_u8 dst (dst_pos + 4) b0;
    set_u8 dst (dst_pos + 5) b1;
    set_u8 dst (dst_pos + 6) b0;
    set_u8 dst (dst_pos + 7) b1
  | 3 ->
    set_u8 dst dst_pos (get_u8 dst src_pos);
    set_u8 dst (dst_pos + 1) (get_u8 dst (src_pos + 1));
    set_u8 dst (dst_pos + 2) (get_u8 dst (src_pos + 2));
    set_u8 dst (dst_pos + 3) (get_u8 dst src_pos);
    set_u8 dst (dst_pos + 4) (get_u8 dst (src_pos + 1));
    set_u8 dst (dst_pos + 5) (get_u8 dst (src_pos + 2));
    set_u8 dst (dst_pos + 6) (get_u8 dst src_pos);
    set_u8 dst (dst_pos + 7) (get_u8 dst (src_pos + 1))
  | 4 ->
    set_u8 dst dst_pos (get_u8 dst src_pos);
    set_u8 dst (dst_pos + 1) (get_u8 dst (src_pos + 1));
    set_u8 dst (dst_pos + 2) (get_u8 dst (src_pos + 2));
    set_u8 dst (dst_pos + 3) (get_u8 dst (src_pos + 3));
    set_u8 dst (dst_pos + 4) (get_u8 dst src_pos);
    set_u8 dst (dst_pos + 5) (get_u8 dst (src_pos + 1));
    set_u8 dst (dst_pos + 6) (get_u8 dst (src_pos + 2));
    set_u8 dst (dst_pos + 7) (get_u8 dst (src_pos + 3))
  | _ ->
    (* 5, 6, 7 - copy pattern byte by byte *)
    for i = 0 to 7 do
      set_u8 dst (dst_pos + i) (get_u8 dst (src_pos + (i mod offset)))
    done

(* Copy with overlap handling - critical for RLE-style copies.
   Optimized for small offsets using pattern extension. *)
let[@inline never] copy_overlapping dst ~dst_pos ~offset ~length =
  if offset < 8 && length >= 8 then begin
    (* Use pattern extension for small offsets *)
    extend_pattern_small dst ~dst_pos ~offset;
    (* Now copy the rest using the extended pattern *)
    let i = ref 8 in
    (* Copy in chunks of the offset size, using the extended 8-byte pattern *)
    while !i < length do
      (* Use the extended pattern at dst_pos to copy. For i bytes written,
         the pattern repeats, so we copy from dst_pos + (i mod 8) position
         but we need to maintain the original pattern cycling. *)
      let src_offset = (!i mod offset) in
      set_u8 dst (dst_pos + !i) (get_u8 dst (dst_pos + src_offset));
      incr i
    done
  end else begin
    (* Generic byte-by-byte copy for overlapping regions *)
    let src_pos = dst_pos - offset in
    for i = 0 to length - 1 do
      set_u8 dst (dst_pos + i) (get_u8 dst (src_pos + i))
    done
  end

let[@inline] copy_non_overlapping dst ~dst_pos ~offset ~length =
  let src_pos = dst_pos - offset in
  Bytes.blit dst src_pos dst dst_pos length

(* Main decompression function - returns decompressed data *)
let decompress_to_bytes src ~pos ~len =
  if len < 1 then raise (Snappy_error Truncated_input);

  (* Decode uncompressed length *)
  let uncompressed_len, varint_bytes = decode_varint src ~pos ~len in

  (* Allocate output buffer *)
  let dst = Bytes.create uncompressed_len in
  let src_pos = ref (pos + varint_bytes) in
  let src_end = pos + len in
  let dst_pos = ref 0 in

  while !src_pos < src_end && !dst_pos < uncompressed_len do
    let tag = get_u8 src !src_pos in
    let tag_type = tag land 3 in

    match tag_type with
    | 0 -> (* Literal *)
      let lit_len =
        let len_minus_1 = tag lsr 2 in
        if len_minus_1 < 60 then begin
          incr src_pos;
          len_minus_1 + 1
        end else
          let extra_bytes = len_minus_1 - 59 in (* 1, 2, 3, or 4 *)
          if !src_pos + 1 + extra_bytes > src_end then
            raise (Snappy_error Truncated_input);
          incr src_pos;
          let len_val =
            match extra_bytes with
            | 1 -> get_u8 src !src_pos
            | 2 -> get_u16_le src !src_pos
            | 3 -> get_u8 src !src_pos
                   lor (get_u8 src (!src_pos + 1) lsl 8)
                   lor (get_u8 src (!src_pos + 2) lsl 16)
            | 4 -> get_u32_le src !src_pos
            | _ -> raise (Snappy_error Invalid_literal_length)
          in
          src_pos := !src_pos + extra_bytes;
          len_val + 1
      in
      (* Copy literal bytes *)
      if !src_pos + lit_len > src_end then
        raise (Snappy_error Truncated_input);
      if !dst_pos + lit_len > uncompressed_len then
        raise (Snappy_error Output_overrun);
      Bytes.blit src !src_pos dst !dst_pos lit_len;
      src_pos := !src_pos + lit_len;
      dst_pos := !dst_pos + lit_len

    | 1 -> (* Copy with 1-byte offset *)
      if !src_pos + 2 > src_end then raise (Snappy_error Truncated_input);
      let length = ((tag lsr 2) land 7) + 4 in
      let offset = ((tag lsr 5) lsl 8) lor (get_u8 src (!src_pos + 1)) in
      src_pos := !src_pos + 2;
      if offset = 0 || offset > !dst_pos then
        raise (Snappy_error Invalid_copy_offset);
      if !dst_pos + length > uncompressed_len then
        raise (Snappy_error Output_overrun);
      if offset >= length then
        copy_non_overlapping dst ~dst_pos:!dst_pos ~offset ~length
      else
        copy_overlapping dst ~dst_pos:!dst_pos ~offset ~length;
      dst_pos := !dst_pos + length

    | 2 -> (* Copy with 2-byte offset *)
      if !src_pos + 3 > src_end then raise (Snappy_error Truncated_input);
      let length = (tag lsr 2) + 1 in
      let offset = get_u16_le src (!src_pos + 1) in
      src_pos := !src_pos + 3;
      if offset = 0 || offset > !dst_pos then
        raise (Snappy_error Invalid_copy_offset);
      if !dst_pos + length > uncompressed_len then
        raise (Snappy_error Output_overrun);
      if offset >= length then
        copy_non_overlapping dst ~dst_pos:!dst_pos ~offset ~length
      else
        copy_overlapping dst ~dst_pos:!dst_pos ~offset ~length;
      dst_pos := !dst_pos + length

    | 3 -> (* Copy with 4-byte offset *)
      if !src_pos + 5 > src_end then raise (Snappy_error Truncated_input);
      let length = (tag lsr 2) + 1 in
      let offset = get_u32_le src (!src_pos + 1) in
      src_pos := !src_pos + 5;
      if offset = 0 || offset > !dst_pos then
        raise (Snappy_error Invalid_copy_offset);
      if !dst_pos + length > uncompressed_len then
        raise (Snappy_error Output_overrun);
      if offset >= length then
        copy_non_overlapping dst ~dst_pos:!dst_pos ~offset ~length
      else
        copy_overlapping dst ~dst_pos:!dst_pos ~offset ~length;
      dst_pos := !dst_pos + length

    | _ -> assert false (* unreachable, tag_type is 0-3 *)
  done;

  if !dst_pos <> uncompressed_len then
    raise (Snappy_error Truncated_input);
  dst

(* Decompress into pre-allocated buffer. Returns bytes written. *)
let decompress_into ~src ~src_pos ~src_len ~dst ~dst_pos =
  if src_len < 1 then raise (Snappy_error Truncated_input);

  (* Decode uncompressed length *)
  let uncompressed_len, varint_bytes = decode_varint src ~pos:src_pos ~len:src_len in

  if dst_pos + uncompressed_len > Bytes.length dst then
    raise (Snappy_error Output_overrun);

  let src_i = ref (src_pos + varint_bytes) in
  let src_end = src_pos + src_len in
  let dst_i = ref dst_pos in
  let dst_end = dst_pos + uncompressed_len in

  while !src_i < src_end && !dst_i < dst_end do
    let tag = get_u8 src !src_i in
    let tag_type = tag land 3 in

    match tag_type with
    | 0 -> (* Literal *)
      let lit_len =
        let len_minus_1 = tag lsr 2 in
        if len_minus_1 < 60 then begin
          incr src_i;
          len_minus_1 + 1
        end else
          let extra_bytes = len_minus_1 - 59 in
          if !src_i + 1 + extra_bytes > src_end then
            raise (Snappy_error Truncated_input);
          incr src_i;
          let len_val =
            match extra_bytes with
            | 1 -> get_u8 src !src_i
            | 2 -> get_u16_le src !src_i
            | 3 -> get_u8 src !src_i
                   lor (get_u8 src (!src_i + 1) lsl 8)
                   lor (get_u8 src (!src_i + 2) lsl 16)
            | 4 -> get_u32_le src !src_i
            | _ -> raise (Snappy_error Invalid_literal_length)
          in
          src_i := !src_i + extra_bytes;
          len_val + 1
      in
      if !src_i + lit_len > src_end then
        raise (Snappy_error Truncated_input);
      if !dst_i + lit_len > dst_end then
        raise (Snappy_error Output_overrun);
      Bytes.blit src !src_i dst !dst_i lit_len;
      src_i := !src_i + lit_len;
      dst_i := !dst_i + lit_len

    | 1 -> (* Copy with 1-byte offset *)
      if !src_i + 2 > src_end then raise (Snappy_error Truncated_input);
      let length = ((tag lsr 2) land 7) + 4 in
      let offset = ((tag lsr 5) lsl 8) lor (get_u8 src (!src_i + 1)) in
      src_i := !src_i + 2;
      if offset = 0 || offset > (!dst_i - dst_pos) then
        raise (Snappy_error Invalid_copy_offset);
      if !dst_i + length > dst_end then
        raise (Snappy_error Output_overrun);
      if offset >= length then
        copy_non_overlapping dst ~dst_pos:!dst_i ~offset ~length
      else
        copy_overlapping dst ~dst_pos:!dst_i ~offset ~length;
      dst_i := !dst_i + length

    | 2 -> (* Copy with 2-byte offset *)
      if !src_i + 3 > src_end then raise (Snappy_error Truncated_input);
      let length = (tag lsr 2) + 1 in
      let offset = get_u16_le src (!src_i + 1) in
      src_i := !src_i + 3;
      if offset = 0 || offset > (!dst_i - dst_pos) then
        raise (Snappy_error Invalid_copy_offset);
      if !dst_i + length > dst_end then
        raise (Snappy_error Output_overrun);
      if offset >= length then
        copy_non_overlapping dst ~dst_pos:!dst_i ~offset ~length
      else
        copy_overlapping dst ~dst_pos:!dst_i ~offset ~length;
      dst_i := !dst_i + length

    | 3 -> (* Copy with 4-byte offset *)
      if !src_i + 5 > src_end then raise (Snappy_error Truncated_input);
      let length = (tag lsr 2) + 1 in
      let offset = get_u32_le src (!src_i + 1) in
      src_i := !src_i + 5;
      if offset = 0 || offset > (!dst_i - dst_pos) then
        raise (Snappy_error Invalid_copy_offset);
      if !dst_i + length > dst_end then
        raise (Snappy_error Output_overrun);
      if offset >= length then
        copy_non_overlapping dst ~dst_pos:!dst_i ~offset ~length
      else
        copy_overlapping dst ~dst_pos:!dst_i ~offset ~length;
      dst_i := !dst_i + length

    | _ -> assert false
  done;

  if !dst_i <> dst_end then
    raise (Snappy_error Truncated_input);
  uncompressed_len

(* Get uncompressed length without full decompression *)
let get_uncompressed_length src ~pos ~len =
  if len < 1 then None
  else
    match decode_varint src ~pos ~len with
    | (v, _) -> Some v
    | exception Snappy_error _ -> None

(* ============================================================
   Compression
   ============================================================ *)

(* Hash function - takes 4 bytes, returns hash for table lookup
   Using a multiplicative hash with a good mixing constant from the original Snappy.
   The constant 0x1e35a7bd was chosen to distribute bits well. *)
let[@inline always] hash_4bytes v shift =
  let kMul = 0x1e35a7bd in
  ((v * kMul) lsr shift) land (max_hash_table_size - 1)

(* Faster match length finding - compare 8 bytes at a time using XOR + CTZ
   This is the technique from snappy-c: XOR two 8-byte chunks, then find
   the first non-zero byte using count-trailing-zeros. *)
let[@inline] find_match_length_fast src a b limit =
  let len = ref 0 in
  let remaining = limit - b in
  let found = ref false in

  (* Compare 8 bytes at a time while we can *)
  while not !found && !len + 8 <= remaining do
    let a1 = get_u64_le src (a + !len) in
    let b1 = get_u64_le src (b + !len) in
    let xorval = Int64.logxor a1 b1 in
    if xorval = 0L then
      len := !len + 8
    else begin
      (* Find first differing byte using CTZ *)
      let matched_bytes = ctz64 xorval / 8 in
      len := !len + matched_bytes;
      found := true
    end
  done;

  if not !found then begin
    (* Compare 4 bytes if we can *)
    if !len + 4 <= remaining then begin
      let a1 = get_u32_le src (a + !len) in
      let b1 = get_u32_le src (b + !len) in
      if a1 = b1 then len := !len + 4
      else begin
        (* XOR and find first differing byte *)
        let xorval = a1 lxor b1 in
        if xorval land 0xFF = 0 then begin
          incr len;
          if xorval land 0xFFFF = 0 then begin
            incr len;
            if xorval land 0xFFFFFF = 0 then
              incr len
          end
        end;
        found := true
      end
    end
  end;

  (* Compare remaining bytes one at a time *)
  if not !found then
    while b + !len < limit && get_u8 src (a + !len) = get_u8 src (b + !len) do
      incr len
    done;
  !len

(* Emit a literal. Returns new dst position. *)
let emit_literal dst dst_pos src src_pos length =
  let n = length - 1 in
  let dst_pos =
    if n < 60 then begin
      set_u8 dst dst_pos ((n lsl 2) lor tag_literal);
      dst_pos + 1
    end else if n < 0x100 then begin
      set_u8 dst dst_pos ((60 lsl 2) lor tag_literal);
      set_u8 dst (dst_pos + 1) n;
      dst_pos + 2
    end else if n < 0x10000 then begin
      set_u8 dst dst_pos ((61 lsl 2) lor tag_literal);
      set_u16_le dst (dst_pos + 1) n;
      dst_pos + 3
    end else if n < 0x1000000 then begin
      set_u8 dst dst_pos ((62 lsl 2) lor tag_literal);
      set_u8 dst (dst_pos + 1) (n land 0xFF);
      set_u8 dst (dst_pos + 2) ((n lsr 8) land 0xFF);
      set_u8 dst (dst_pos + 3) ((n lsr 16) land 0xFF);
      dst_pos + 4
    end else begin
      set_u8 dst dst_pos ((63 lsl 2) lor tag_literal);
      set_u32_le dst (dst_pos + 1) n;
      dst_pos + 5
    end
  in
  Bytes.blit src src_pos dst dst_pos length;
  dst_pos + length

(* Emit a copy. Returns new dst position. *)
let emit_copy dst dst_pos offset length =
  (* Try 1-byte offset encoding: length 4-11, offset 0-2047 *)
  (* IMPORTANT: length must be >= 4 for 1-byte offset encoding! *)
  if length >= 4 && length <= 11 && offset < 2048 then begin
    let tag = tag_copy1
              lor ((length - 4) lsl 2)
              lor ((offset lsr 8) lsl 5) in
    set_u8 dst dst_pos tag;
    set_u8 dst (dst_pos + 1) (offset land 0xFF);
    dst_pos + 2
  end
  (* 2-byte offset encoding: length 1-64, offset 0-65535 *)
  else if length <= 64 && offset < 65536 then begin
    let tag = tag_copy2 lor ((length - 1) lsl 2) in
    set_u8 dst dst_pos tag;
    set_u16_le dst (dst_pos + 1) offset;
    dst_pos + 3
  end
  (* 4-byte offset encoding *)
  else begin
    let tag = tag_copy4 lor ((length - 1) lsl 2) in
    set_u8 dst dst_pos tag;
    set_u32_le dst (dst_pos + 1) offset;
    dst_pos + 5
  end

(* Emit a copy that may need to be split (for long matches) *)
let rec emit_copy_split dst dst_pos offset length =
  if length <= 64 then
    emit_copy dst dst_pos offset length
  else begin
    (* Emit max length copy, then continue *)
    let dst_pos = emit_copy dst dst_pos offset 64 in
    emit_copy_split dst dst_pos offset (length - 64)
  end

(* Maximum compressed length for input of given size *)
let max_compressed_length input_len =
  (* Worst case: 1 byte overhead per 6 bytes + varint header *)
  varint_length input_len + input_len + (input_len / 6) + 32

(* Compress src[src_pos..src_pos+src_len) into dst starting at dst_pos.
   Returns bytes written. Assumes dst has enough space.

   Performance optimizations:
   - Uses fast 8-byte-at-a-time match length comparison with XOR+CTZ
   - Skip-bytes heuristic for incompressible data (from snappy-c)
   - Skips hashing for long matches (hash every Nth byte)
   - Inlined hot path operations *)
let compress_into ~src ~src_pos ~src_len ~dst ~dst_pos =
  if src_len = 0 then begin
    (* Empty input *)
    let written = encode_varint dst ~pos:dst_pos 0 in
    written
  end else begin
    (* Write uncompressed length *)
    let header_len = encode_varint dst ~pos:dst_pos src_len in
    let dst_i = ref (dst_pos + header_len) in

    (* Hash table: stores position of last occurrence of each 4-byte sequence *)
    let shift = 32 - max_hash_table_bits in
    let table = Array.make max_hash_table_size (-1) in

    let src_end = src_pos + src_len in
    let src_limit = src_end - 4 in (* Need 4 bytes to read hash *)
    let lit_start = ref src_pos in
    let i = ref src_pos in

    (* Skip-bytes heuristic from snappy-c:
       After 32 bytes scanned without a match, start skipping bytes.
       This helps with incompressible data like JPEG or random bytes. *)
    let skip = ref 32 in

    while !i < src_limit do
      (* Get 4 bytes at current position *)
      let cur4 = get_u32_le src !i in
      let h = hash_4bytes cur4 shift in

      (* Check for match *)
      let candidate = Array.unsafe_get table h in
      Array.unsafe_set table h !i;

      if candidate >= src_pos
         && candidate < !i
         && !i - candidate <= max_offset
         && get_u32_le src candidate = cur4 then begin
        (* Found a match! Emit pending literals first *)
        if !lit_start < !i then begin
          dst_i := emit_literal dst !dst_i src !lit_start (!i - !lit_start)
        end;

        (* Find match length using fast 8-byte comparison *)
        let match_len = 4 + find_match_length_fast src (candidate + 4) (!i + 4) src_end in
        let offset = !i - candidate in

        (* Emit copy *)
        dst_i := emit_copy_split dst !dst_i offset match_len;

        (* Reset skip counter after finding a match *)
        skip := 32;

        (* Skip matched bytes, using sparse hashing for long matches *)
        let match_end = !i + match_len in
        if match_len > 16 then begin
          (* For long matches, only hash every 4th byte to save time *)
          i := !i + 1;
          while !i < match_end - 3 && !i < src_limit do
            let h = hash_4bytes (get_u32_le src !i) shift in
            Array.unsafe_set table h !i;
            i := !i + 4
          done;
          i := match_end
        end else begin
          (* For short matches, hash every byte *)
          i := !i + 1;
          while !i < match_end && !i < src_limit do
            let h = hash_4bytes (get_u32_le src !i) shift in
            Array.unsafe_set table h !i;
            incr i
          done;
          i := match_end
        end;
        lit_start := !i
      end else begin
        (* No match - use skip-bytes heuristic for incompressible data.
           The skip value grows slowly: we only start skipping after 32 failed
           lookups, then skip 1 byte, then 2, etc. This is more conservative
           than snappy-c to preserve compression ratio on mixed data. *)
        incr skip;
        let bytes_to_skip = !skip lsr 6 in  (* Use 64 instead of 32 for more conservative skipping *)
        i := !i + bytes_to_skip + 1
      end
    done;

    (* Emit remaining literals *)
    if !lit_start < src_end then
      dst_i := emit_literal dst !dst_i src !lit_start (src_end - !lit_start);

    !dst_i - dst_pos
  end

(* Compress and return new bytes *)
let compress_to_bytes src ~pos ~len =
  let dst = Bytes.create (max_compressed_length len) in
  let written = compress_into ~src ~src_pos:pos ~src_len:len ~dst ~dst_pos:0 in
  if written = Bytes.length dst then dst
  else Bytes.sub dst 0 written

(* ============================================================
   Convenience API
   ============================================================ *)

let compress s =
  let src = Bytes.unsafe_of_string s in
  Bytes.unsafe_to_string (compress_to_bytes src ~pos:0 ~len:(String.length s))

let decompress s =
  let src = Bytes.unsafe_of_string s in
  try Ok (Bytes.unsafe_to_string (decompress_to_bytes src ~pos:0 ~len:(String.length s)))
  with Snappy_error e -> Error (error_to_string e)

let decompress_exn s =
  let src = Bytes.unsafe_of_string s in
  Bytes.unsafe_to_string (decompress_to_bytes src ~pos:0 ~len:(String.length s))

(* Check if data looks like valid snappy compressed data *)
let is_valid_compressed s =
  String.length s >= 1 &&
  Option.is_some (get_uncompressed_length (Bytes.unsafe_of_string s) ~pos:0 ~len:(String.length s))

(* ============================================================
   Reusable Compression Context
   For applications that compress many messages, reusing the hash table
   avoids allocation overhead.
   ============================================================ *)

type compress_ctx = {
  ctx_table : int array;
  ctx_shift : int;
}

let create_compress_ctx () =
  {
    ctx_table = Array.make max_hash_table_size (-1);
    ctx_shift = 32 - max_hash_table_bits;
  }

(* Compress using a reusable context. The hash table is reset between calls. *)
let compress_with_ctx ctx ~src ~src_pos ~src_len ~dst ~dst_pos =
  if src_len = 0 then begin
    let written = encode_varint dst ~pos:dst_pos 0 in
    written
  end else begin
    (* Reset hash table *)
    Array.fill ctx.ctx_table 0 max_hash_table_size (-1);

    (* Write uncompressed length *)
    let header_len = encode_varint dst ~pos:dst_pos src_len in
    let dst_i = ref (dst_pos + header_len) in
    let shift = ctx.ctx_shift in
    let table = ctx.ctx_table in

    let src_end = src_pos + src_len in
    let src_limit = src_end - 4 in
    let lit_start = ref src_pos in
    let i = ref src_pos in
    let skip = ref 32 in

    while !i < src_limit do
      let cur4 = get_u32_le src !i in
      let h = hash_4bytes cur4 shift in
      let candidate = Array.unsafe_get table h in
      Array.unsafe_set table h !i;

      if candidate >= src_pos
         && candidate < !i
         && !i - candidate <= max_offset
         && get_u32_le src candidate = cur4 then begin
        if !lit_start < !i then
          dst_i := emit_literal dst !dst_i src !lit_start (!i - !lit_start);
        let match_len = 4 + find_match_length_fast src (candidate + 4) (!i + 4) src_end in
        let offset = !i - candidate in
        dst_i := emit_copy_split dst !dst_i offset match_len;
        skip := 32;
        let match_end = !i + match_len in
        if match_len > 16 then begin
          i := !i + 1;
          while !i < match_end - 3 && !i < src_limit do
            let h = hash_4bytes (get_u32_le src !i) shift in
            Array.unsafe_set table h !i;
            i := !i + 4
          done;
          i := match_end
        end else begin
          i := !i + 1;
          while !i < match_end && !i < src_limit do
            let h = hash_4bytes (get_u32_le src !i) shift in
            Array.unsafe_set table h !i;
            incr i
          done;
          i := match_end
        end;
        lit_start := !i
      end else begin
        incr skip;
        let bytes_to_skip = !skip lsr 6 in
        i := !i + bytes_to_skip + 1
      end
    done;

    if !lit_start < src_end then
      dst_i := emit_literal dst !dst_i src !lit_start (src_end - !lit_start);
    !dst_i - dst_pos
  end

(* ============================================================
   CRC32-C Implementation
   ============================================================ *)

(* CRC32-C polynomial: 0x82F63B78 (bit-reversed 0x1EDC6F41) *)
(* Pre-computed table for CRC32-C *)
let crc32c_table =
  let table = Array.make 256 0l in
  for i = 0 to 255 do
    let crc = ref (Int32.of_int i) in
    for _ = 0 to 7 do
      if Int32.logand !crc 1l <> 0l then
        crc := Int32.logxor (Int32.shift_right_logical !crc 1) 0x82F63B78l
      else
        crc := Int32.shift_right_logical !crc 1
    done;
    table.(i) <- !crc
  done;
  table

let crc32c_update crc buf ~pos ~len =
  let crc = ref crc in
  for i = pos to pos + len - 1 do
    let b = Char.code (Bytes.unsafe_get buf i) in
    let idx = Int32.to_int (Int32.logand (Int32.logxor !crc (Int32.of_int b)) 0xFFl) in
    crc := Int32.logxor (Int32.shift_right_logical !crc 8) (Array.unsafe_get crc32c_table idx)
  done;
  !crc

let crc32c buf ~pos ~len =
  let crc = crc32c_update 0xFFFFFFFFl buf ~pos ~len in
  Int32.logxor crc 0xFFFFFFFFl

(* Mask checksum as per Snappy framing format *)
let mask_checksum crc =
  let rotated = Int32.logor
    (Int32.shift_right_logical crc 15)
    (Int32.shift_left crc 17) in
  Int32.add rotated 0xa282ead8l

let unmask_checksum masked =
  let unrotated = Int32.sub masked 0xa282ead8l in
  Int32.logor
    (Int32.shift_left unrotated 15)
    (Int32.shift_right_logical unrotated 17)

(* ============================================================
   Streaming API
   ============================================================ *)

(* Block size for streaming - 64KB as per Snappy framing format *)
let stream_block_size = 65536

(* Streaming compression state *)
type compress_stream = {
  cs_buffer : bytes;
  mutable cs_buffer_pos : int;
  cs_output : bytes -> int -> int -> unit;  (* callback: buf, pos, len *)
  mutable cs_started : bool;  (* whether stream identifier has been written *)
}

(* Streaming decompression state *)
type decompress_stream = {
  ds_buffer : bytes;
  mutable ds_buffer_pos : int;
  ds_output : bytes -> int -> int -> unit;  (* callback: buf, pos, len *)
  mutable ds_state : decompress_state;
  mutable ds_chunk_type : int;
  mutable ds_chunk_len : int;
  mutable ds_chunk_pos : int;
  mutable ds_chunk_buf : bytes;
}

and decompress_state =
  | DS_Header          (* Waiting for stream identifier *)
  | DS_ChunkHeader     (* Reading chunk type + length *)
  | DS_ChunkData       (* Reading chunk data *)

(* Stream identifier for framing format *)
let stream_identifier = "\xff\x06\x00\x00sNaPpY"

(* Chunk types *)
let chunk_compressed = 0x00
let chunk_uncompressed = 0x01
let chunk_padding = 0xfe
let chunk_stream_id = 0xff

(* Create a new compression stream *)
let create_compress_stream ~output =
  {
    cs_buffer = Bytes.create stream_block_size;
    cs_buffer_pos = 0;
    cs_output = output;
    cs_started = false;
  }

(* Write the stream identifier *)
let write_stream_identifier cs =
  if not cs.cs_started then begin
    cs.cs_output (Bytes.unsafe_of_string stream_identifier) 0 10;
    cs.cs_started <- true
  end

(* Flush a block of data *)
let flush_block cs =
  if cs.cs_buffer_pos > 0 then begin
    write_stream_identifier cs;

    let src = cs.cs_buffer in
    let src_len = cs.cs_buffer_pos in

    (* Calculate CRC of uncompressed data *)
    let crc = crc32c src ~pos:0 ~len:src_len in
    let masked_crc = mask_checksum crc in

    (* Compress the block *)
    let max_len = max_compressed_length src_len in
    let compressed = Bytes.create max_len in
    let compressed_len = compress_into ~src ~src_pos:0 ~src_len ~dst:compressed ~dst_pos:0 in

    (* Choose compressed or uncompressed based on size *)
    if compressed_len < src_len then begin
      (* Write compressed chunk: type (1) + length (3) + crc (4) + data *)
      let chunk_len = 4 + compressed_len in
      let header = Bytes.create 8 in
      set_u8 header 0 chunk_compressed;
      set_u8 header 1 (chunk_len land 0xFF);
      set_u8 header 2 ((chunk_len lsr 8) land 0xFF);
      set_u8 header 3 ((chunk_len lsr 16) land 0xFF);
      set_u32_le header 4 (Int32.to_int masked_crc);
      cs.cs_output header 0 8;
      cs.cs_output compressed 0 compressed_len
    end else begin
      (* Write uncompressed chunk *)
      let chunk_len = 4 + src_len in
      let header = Bytes.create 8 in
      set_u8 header 0 chunk_uncompressed;
      set_u8 header 1 (chunk_len land 0xFF);
      set_u8 header 2 ((chunk_len lsr 8) land 0xFF);
      set_u8 header 3 ((chunk_len lsr 16) land 0xFF);
      set_u32_le header 4 (Int32.to_int masked_crc);
      cs.cs_output header 0 8;
      cs.cs_output src 0 src_len
    end;

    cs.cs_buffer_pos <- 0
  end

(* Feed data to compression stream *)
let compress_stream_feed cs data ~pos ~len =
  let remaining = ref len in
  let src_pos = ref pos in

  while !remaining > 0 do
    let space = stream_block_size - cs.cs_buffer_pos in
    let to_copy = min space !remaining in
    Bytes.blit data !src_pos cs.cs_buffer cs.cs_buffer_pos to_copy;
    cs.cs_buffer_pos <- cs.cs_buffer_pos + to_copy;
    src_pos := !src_pos + to_copy;
    remaining := !remaining - to_copy;

    if cs.cs_buffer_pos >= stream_block_size then
      flush_block cs
  done

(* Finish compression stream *)
let compress_stream_finish cs =
  (* Always write stream identifier even for empty input *)
  write_stream_identifier cs;
  flush_block cs

(* Compress stream from string *)
let compress_stream_string ~output s =
  let cs = create_compress_stream ~output in
  compress_stream_feed cs (Bytes.unsafe_of_string s) ~pos:0 ~len:(String.length s);
  compress_stream_finish cs

(* ============================================================
   Streaming Decompression
   ============================================================ *)

(* Framing format errors *)
type framing_error =
  | Missing_stream_identifier
  | Invalid_stream_identifier
  | Invalid_chunk_type of int
  | Checksum_mismatch
  | Chunk_too_large
  | Decompression_failed of error

exception Framing_error of framing_error

let framing_error_to_string = function
  | Missing_stream_identifier -> "Missing stream identifier"
  | Invalid_stream_identifier -> "Invalid stream identifier"
  | Invalid_chunk_type n -> Printf.sprintf "Invalid chunk type: 0x%02x" n
  | Checksum_mismatch -> "Checksum mismatch"
  | Chunk_too_large -> "Chunk too large"
  | Decompression_failed e -> "Decompression failed: " ^ error_to_string e

(* Create a new decompression stream *)
let create_decompress_stream ~output =
  {
    ds_buffer = Bytes.create 16;  (* For reading headers *)
    ds_buffer_pos = 0;
    ds_output = output;
    ds_state = DS_Header;
    ds_chunk_type = 0;
    ds_chunk_len = 0;
    ds_chunk_pos = 0;
    ds_chunk_buf = Bytes.create 0;
  }

(* Process buffered chunk data *)
let process_chunk ds =
  match ds.ds_chunk_type with
  | t when t = chunk_stream_id ->
    (* Verify stream identifier *)
    if ds.ds_chunk_len <> 6 then
      raise (Framing_error Invalid_stream_identifier);
    let expected = "sNaPpY" in
    for i = 0 to 5 do
      if Bytes.get ds.ds_chunk_buf i <> String.get expected i then
        raise (Framing_error Invalid_stream_identifier)
    done;
    ds.ds_state <- DS_ChunkHeader

  | t when t = chunk_compressed ->
    if ds.ds_chunk_len < 4 then
      raise (Framing_error (Decompression_failed Truncated_input));

    (* Read masked CRC *)
    let masked_crc = Int32.of_int (get_u32_le ds.ds_chunk_buf 0) in
    let expected_crc = unmask_checksum masked_crc in

    (* Decompress *)
    let compressed_len = ds.ds_chunk_len - 4 in
    begin
      try
        let decompressed = decompress_to_bytes ds.ds_chunk_buf ~pos:4 ~len:compressed_len in

        (* Verify CRC *)
        let actual_crc = crc32c decompressed ~pos:0 ~len:(Bytes.length decompressed) in
        if actual_crc <> expected_crc then
          raise (Framing_error Checksum_mismatch);

        (* Output decompressed data *)
        ds.ds_output decompressed 0 (Bytes.length decompressed)
      with Snappy_error e ->
        raise (Framing_error (Decompression_failed e))
    end;
    ds.ds_state <- DS_ChunkHeader

  | t when t = chunk_uncompressed ->
    if ds.ds_chunk_len < 4 then
      raise (Framing_error (Decompression_failed Truncated_input));

    (* Read masked CRC *)
    let masked_crc = Int32.of_int (get_u32_le ds.ds_chunk_buf 0) in
    let expected_crc = unmask_checksum masked_crc in

    let data_len = ds.ds_chunk_len - 4 in

    (* Verify CRC *)
    let actual_crc = crc32c ds.ds_chunk_buf ~pos:4 ~len:data_len in
    if actual_crc <> expected_crc then
      raise (Framing_error Checksum_mismatch);

    (* Output uncompressed data *)
    ds.ds_output ds.ds_chunk_buf 4 data_len;
    ds.ds_state <- DS_ChunkHeader

  | t when t = chunk_padding ->
    (* Ignore padding *)
    ds.ds_state <- DS_ChunkHeader

  | t when t >= 0x02 && t <= 0x7f ->
    (* Reserved unskippable chunk *)
    raise (Framing_error (Invalid_chunk_type t))

  | t when t >= 0x80 && t <= 0xfd ->
    (* Reserved skippable chunk - ignore *)
    ds.ds_state <- DS_ChunkHeader

  | t ->
    raise (Framing_error (Invalid_chunk_type t))

(* Feed data to decompression stream *)
let decompress_stream_feed ds data ~pos ~len =
  let remaining = ref len in
  let src_pos = ref pos in

  while !remaining > 0 do
    match ds.ds_state with
    | DS_Header ->
      (* Need 10 bytes for stream identifier *)
      let need = 10 - ds.ds_buffer_pos in
      let to_copy = min need !remaining in
      Bytes.blit data !src_pos ds.ds_buffer ds.ds_buffer_pos to_copy;
      ds.ds_buffer_pos <- ds.ds_buffer_pos + to_copy;
      src_pos := !src_pos + to_copy;
      remaining := !remaining - to_copy;

      if ds.ds_buffer_pos >= 10 then begin
        (* Verify stream identifier *)
        let expected = stream_identifier in
        for i = 0 to 9 do
          if Bytes.get ds.ds_buffer i <> String.get expected i then
            raise (Framing_error Invalid_stream_identifier)
        done;
        ds.ds_buffer_pos <- 0;
        ds.ds_state <- DS_ChunkHeader
      end

    | DS_ChunkHeader ->
      (* Need 4 bytes for chunk header *)
      let need = 4 - ds.ds_buffer_pos in
      let to_copy = min need !remaining in
      Bytes.blit data !src_pos ds.ds_buffer ds.ds_buffer_pos to_copy;
      ds.ds_buffer_pos <- ds.ds_buffer_pos + to_copy;
      src_pos := !src_pos + to_copy;
      remaining := !remaining - to_copy;

      if ds.ds_buffer_pos >= 4 then begin
        ds.ds_chunk_type <- get_u8 ds.ds_buffer 0;
        ds.ds_chunk_len <- get_u8 ds.ds_buffer 1
                           lor (get_u8 ds.ds_buffer 2 lsl 8)
                           lor (get_u8 ds.ds_buffer 3 lsl 16);

        (* Check chunk size limits *)
        if ds.ds_chunk_len > 16777215 then
          raise (Framing_error Chunk_too_large);

        (* For stream identifier, chunk_len should be 6 *)
        if ds.ds_chunk_type = chunk_stream_id && ds.ds_chunk_len <> 6 then
          raise (Framing_error Invalid_stream_identifier);

        if ds.ds_chunk_len = 0 then begin
          (* Empty chunk *)
          ds.ds_buffer_pos <- 0;
          process_chunk ds
        end else begin
          (* Allocate buffer for chunk data *)
          if Bytes.length ds.ds_chunk_buf < ds.ds_chunk_len then
            ds.ds_chunk_buf <- Bytes.create (max ds.ds_chunk_len (stream_block_size + 1024));
          ds.ds_chunk_pos <- 0;
          ds.ds_buffer_pos <- 0;
          ds.ds_state <- DS_ChunkData
        end
      end

    | DS_ChunkData ->
      let need = ds.ds_chunk_len - ds.ds_chunk_pos in
      let to_copy = min need !remaining in
      Bytes.blit data !src_pos ds.ds_chunk_buf ds.ds_chunk_pos to_copy;
      ds.ds_chunk_pos <- ds.ds_chunk_pos + to_copy;
      src_pos := !src_pos + to_copy;
      remaining := !remaining - to_copy;

      if ds.ds_chunk_pos >= ds.ds_chunk_len then
        process_chunk ds
  done

(* Check if decompression stream is in a valid final state *)
let decompress_stream_is_complete ds =
  ds.ds_state = DS_ChunkHeader

(* ============================================================
   Framing Format: Complete String API
   ============================================================ *)

(* Compress with framing format *)
let compress_framed s =
  let buf = Buffer.create (String.length s + 100) in
  let output bytes pos len =
    Buffer.add_subbytes buf bytes pos len
  in
  compress_stream_string ~output s;
  Buffer.contents buf

(* Decompress framed data *)
let decompress_framed s =
  let buf = Buffer.create (String.length s) in
  let output bytes pos len =
    Buffer.add_subbytes buf bytes pos len
  in
  let ds = create_decompress_stream ~output in
  try
    decompress_stream_feed ds (Bytes.unsafe_of_string s) ~pos:0 ~len:(String.length s);
    if decompress_stream_is_complete ds then
      Ok (Buffer.contents buf)
    else
      Error "Incomplete framed data"
  with
  | Framing_error e -> Error (framing_error_to_string e)
  | Snappy_error e -> Error (error_to_string e)

(* Check if data looks like framed snappy data *)
let is_framed_format s =
  String.length s >= 10 &&
  String.sub s 0 10 = stream_identifier
