(* SIMD-accelerated match length finding (x86-64 SSE2/AVX2) *)

let max_match = 258

(* Lookup table for trailing zeros in a byte (0-255).
   For each byte value, stores the count of trailing zeros (0-8).
   Uses int8# array for compact 1-byte-per-entry storage. *)
let trailing_zeros_table : int8# array =
  [| #8s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #4s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #5s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #4s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #6s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #4s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #5s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #4s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #7s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #4s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #5s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #4s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #6s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #4s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #5s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s;
     #4s; #0s; #1s; #0s; #2s; #0s; #1s; #0s; #3s; #0s; #1s; #0s; #2s; #0s; #1s; #0s |]

let[@inline always] get_tz idx =
  Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get trailing_zeros_table idx)

let[@inline always] count_trailing_zeros_16 x =
  if x = 0 then 16
  else
    let low = x land 0xFF in
    if low <> 0 then get_tz low
    else 8 + get_tz ((x lsr 8) land 0xFF)

let[@inline always] count_trailing_zeros_32 x =
  if x = 0 then 32
  else
    let b0 = x land 0xFF in
    if b0 <> 0 then get_tz b0
    else
      let b1 = (x lsr 8) land 0xFF in
      if b1 <> 0 then 8 + get_tz b1
      else
        let b2 = (x lsr 16) land 0xFF in
        if b2 <> 0 then 16 + get_tz b2
        else 24 + get_tz ((x lsr 24) land 0xFF)

let tzcnt64_debruijn_table : int8# array = [|
  #0s;  #1s;  #2s; #53s;  #3s;  #7s; #54s; #27s;
  #4s; #38s; #41s;  #8s; #34s; #55s; #48s; #28s;
  #62s;  #5s; #39s; #46s; #44s; #42s; #22s;  #9s;
  #24s; #35s; #59s; #56s; #49s; #18s; #29s; #11s;
  #63s; #52s;  #6s; #26s; #37s; #40s; #33s; #47s;
  #61s; #45s; #43s; #21s; #23s; #58s; #17s; #10s;
  #51s; #25s; #36s; #32s; #60s; #20s; #57s; #16s;
  #50s; #31s; #19s; #15s; #30s; #14s; #13s; #12s
|]

let[@inline always] count_trailing_zeros64 (x : int64) =
  if Int64.equal x 0L then 64
  else begin
    let isolated = Int64.logand x (Int64.neg x) in
    let idx = Int64.to_int (Int64.shift_right_logical
      (Int64.mul isolated 0x022fdd63cc95386dL) 58) in
    Stdlib_stable.Int8_u.to_int (Oxcaml_arrays.unsafe_get tzcnt64_debruijn_table (idx land 63))
  end

module I8x16 = Ocaml_simd_sse.Int8x16
module I8x32 = Ocaml_simd_avx.Int8x32

let[@inline always] find_first_diff_32 src a b =
  let v1 = I8x32.Bytes.unsafe_get src ~byte:a in
  let v2 = I8x32.Bytes.unsafe_get src ~byte:b in
  let eq_mask = I8x32.equal v1 v2 in
  let mask_unboxed = I8x32.movemask eq_mask in
  let mask = Int64_u.to_int_trunc mask_unboxed in
  let inverted = (lnot mask) land 0xFFFFFFFF in
  if inverted = 0 then 32
  else count_trailing_zeros_32 inverted

let[@inline always] find_first_diff_16 src a b =
  let v1 = I8x16.Bytes.unsafe_get src ~byte:a in
  let v2 = I8x16.Bytes.unsafe_get src ~byte:b in
  let eq_mask = I8x16.equal v1 v2 in
  let mask_unboxed = I8x16.movemask eq_mask in
  let mask = Int64_u.to_int_trunc mask_unboxed in
  let inverted = (lnot mask) land 0xFFFF in
  if inverted = 0 then 16
  else count_trailing_zeros_16 inverted

let[@inline always] find_match_length src a b limit =
  let max_len = min max_match (limit - b) in
  if max_len < 4 then 0
  else begin
    let mutable len = 0 in
    let mutable continue = true in
    (* Process 32 bytes at a time with AVX2 *)
    while continue && len + 32 <= max_len do
      let diff_pos = find_first_diff_32 src (a + len) (b + len) in
      if diff_pos < 32 then begin
        len <- len + diff_pos;
        if len > max_len then len <- max_len;
        continue <- false
      end else
        len <- len + 32
    done;
    (* Handle 16-31 remaining bytes with SSE2 *)
    if continue && len + 16 <= max_len then begin
      let diff_pos = find_first_diff_16 src (a + len) (b + len) in
      if diff_pos < 16 then begin
        len <- len + diff_pos;
        if len > max_len then len <- max_len;
        continue <- false
      end else
        len <- len + 16
    end;
    if continue then begin
      (* Handle remainder with 64-bit comparisons *)
      while continue && len + 8 <= max_len do
        let w1 = Bytes.get_int64_le src (a + len) in
        let w2 = Bytes.get_int64_le src (b + len) in
        if Int64.equal w1 w2 then
          len <- len + 8
        else begin
          let diff = Int64.logxor w1 w2 in
          let diff_byte_pos = count_trailing_zeros64 diff / 8 in
          len <- len + diff_byte_pos;
          if len > max_len then len <- max_len;
          continue <- false
        end
      done
    end;
    (* Final byte-by-byte for last few bytes *)
    while len < max_len && Bytes.get src (a + len) = Bytes.get src (b + len) do
      len <- len + 1
    done;
    len
  end
