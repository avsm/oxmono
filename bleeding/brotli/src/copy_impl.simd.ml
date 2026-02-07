(* SIMD-accelerated overlapping copy for backward references (x86-64 SSE2/AVX2) *)

module I8x16 = Ocaml_simd_sse.Int8x16
module I8x32 = Ocaml_simd_avx.Int8x32

let shuffle_mask_2 = I8x16.const
  #0s #1s #0s #1s #0s #1s #0s #1s
  #0s #1s #0s #1s #0s #1s #0s #1s

let shuffle_mask_4 = I8x16.const
  #0s #1s #2s #3s #0s #1s #2s #3s
  #0s #1s #2s #3s #0s #1s #2s #3s

let shuffle_mask_8 = I8x16.const
  #0s #1s #2s #3s #4s #5s #6s #7s
  #0s #1s #2s #3s #4s #5s #6s #7s

let shuffle_mask_2_avx = I8x32.const
  #0s #1s #0s #1s #0s #1s #0s #1s
  #0s #1s #0s #1s #0s #1s #0s #1s
  #0s #1s #0s #1s #0s #1s #0s #1s
  #0s #1s #0s #1s #0s #1s #0s #1s

let shuffle_mask_4_avx = I8x32.const
  #0s #1s #2s #3s #0s #1s #2s #3s
  #0s #1s #2s #3s #0s #1s #2s #3s
  #0s #1s #2s #3s #0s #1s #2s #3s
  #0s #1s #2s #3s #0s #1s #2s #3s

let shuffle_mask_8_avx = I8x32.const
  #0s #1s #2s #3s #4s #5s #6s #7s
  #0s #1s #2s #3s #4s #5s #6s #7s
  #0s #1s #2s #3s #4s #5s #6s #7s
  #0s #1s #2s #3s #4s #5s #6s #7s

let[@inline always] copy_distance_1 dst dst_pos copy_length =
  let byte_val = Bytes.unsafe_get dst (dst_pos - 1) in
  Bytes.unsafe_fill dst dst_pos copy_length byte_val

let[@inline always] copy_distance_2 dst dst_pos copy_length =
  let src_pos = dst_pos - 2 in
  let mutable remaining = copy_length in
  let mutable pos = dst_pos in
  if remaining >= 32 then begin
    let src_vec_16 = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern_16 = I8x16.shuffle ~pattern:shuffle_mask_2 src_vec_16 in
    let pattern_32 = I8x32.set_lanes pattern_16 pattern_16 in
    while remaining >= 32 do
      I8x32.Bytes.unsafe_set dst ~byte:pos pattern_32;
      pos <- pos + 32;
      remaining <- remaining - 32
    done
  end;
  if remaining >= 16 then begin
    let src_vec = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern = I8x16.shuffle ~pattern:shuffle_mask_2 src_vec in
    I8x16.Bytes.unsafe_set dst ~byte:pos pattern;
    pos <- pos + 16;
    remaining <- remaining - 16
  end;
  if remaining > 0 then begin
    for i = 0 to remaining - 1 do
      Bytes.unsafe_set dst (pos + i) (Bytes.unsafe_get dst (src_pos + (i land 1)))
    done
  end

let[@inline always] copy_distance_4 dst dst_pos copy_length =
  let src_pos = dst_pos - 4 in
  let mutable remaining = copy_length in
  let mutable pos = dst_pos in
  if remaining >= 32 then begin
    let src_vec_16 = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern_16 = I8x16.shuffle ~pattern:shuffle_mask_4 src_vec_16 in
    let pattern_32 = I8x32.set_lanes pattern_16 pattern_16 in
    while remaining >= 32 do
      I8x32.Bytes.unsafe_set dst ~byte:pos pattern_32;
      pos <- pos + 32;
      remaining <- remaining - 32
    done
  end;
  if remaining >= 16 then begin
    let src_vec = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern = I8x16.shuffle ~pattern:shuffle_mask_4 src_vec in
    I8x16.Bytes.unsafe_set dst ~byte:pos pattern;
    pos <- pos + 16;
    remaining <- remaining - 16
  end;
  if remaining > 0 then begin
    for i = 0 to remaining - 1 do
      Bytes.unsafe_set dst (pos + i) (Bytes.unsafe_get dst (src_pos + (i land 3)))
    done
  end

let[@inline always] copy_distance_8 dst dst_pos copy_length =
  let src_pos = dst_pos - 8 in
  let mutable remaining = copy_length in
  let mutable pos = dst_pos in
  if remaining >= 32 then begin
    let src_vec_16 = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern_16 = I8x16.shuffle ~pattern:shuffle_mask_8 src_vec_16 in
    let pattern_32 = I8x32.set_lanes pattern_16 pattern_16 in
    while remaining >= 32 do
      I8x32.Bytes.unsafe_set dst ~byte:pos pattern_32;
      pos <- pos + 32;
      remaining <- remaining - 32
    done
  end;
  if remaining >= 16 then begin
    let src_vec = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    let pattern = I8x16.shuffle ~pattern:shuffle_mask_8 src_vec in
    I8x16.Bytes.unsafe_set dst ~byte:pos pattern;
    pos <- pos + 16;
    remaining <- remaining - 16
  end;
  if remaining > 0 then begin
    for i = 0 to remaining - 1 do
      Bytes.unsafe_set dst (pos + i) (Bytes.unsafe_get dst (src_pos + (i land 7)))
    done
  end

let[@inline always] copy_distance_16 dst dst_pos copy_length =
  let src_pos = dst_pos - 16 in
  let pattern_16 = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
  let mutable remaining = copy_length in
  let mutable pos = dst_pos in
  if remaining >= 32 then begin
    let pattern_32 = I8x32.set_lanes pattern_16 pattern_16 in
    while remaining >= 32 do
      I8x32.Bytes.unsafe_set dst ~byte:pos pattern_32;
      pos <- pos + 32;
      remaining <- remaining - 32
    done
  end;
  if remaining >= 16 then begin
    I8x16.Bytes.unsafe_set dst ~byte:pos pattern_16;
    pos <- pos + 16;
    remaining <- remaining - 16
  end;
  if remaining > 0 then begin
    for i = 0 to remaining - 1 do
      Bytes.unsafe_set dst (pos + i) (Bytes.unsafe_get dst (src_pos + i))
    done
  end

let[@inline always] copy_distance_32 dst dst_pos copy_length =
  let src_pos = dst_pos - 32 in
  let pattern_32 = I8x32.Bytes.unsafe_get dst ~byte:src_pos in
  let mutable remaining = copy_length in
  let mutable pos = dst_pos in
  while remaining >= 32 do
    I8x32.Bytes.unsafe_set dst ~byte:pos pattern_32;
    pos <- pos + 32;
    remaining <- remaining - 32
  done;
  if remaining >= 16 then begin
    let pattern_16 = I8x16.Bytes.unsafe_get dst ~byte:src_pos in
    I8x16.Bytes.unsafe_set dst ~byte:pos pattern_16;
    pos <- pos + 16;
    remaining <- remaining - 16
  end;
  if remaining > 0 then begin
    for i = 0 to remaining - 1 do
      Bytes.unsafe_set dst (pos + i) (Bytes.unsafe_get dst (src_pos + i))
    done
  end

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
  | 32 ->
    copy_distance_32 dst dst_pos copy_length;
    true
  | _ ->
    false
