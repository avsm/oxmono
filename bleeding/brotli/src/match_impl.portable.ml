(* Portable match length finding using 64-bit word comparisons (no SIMD) *)

let max_match = 258

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

let[@inline always] find_match_length src a b limit =
  let max_len = min max_match (limit - b) in
  if max_len < 4 then 0
  else begin
    let mutable len = 0 in
    let mutable continue = true in
    (* Process 8 bytes at a time *)
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
    done;
    (* Handle remaining bytes *)
    while len < max_len && Bytes.get src (a + len) = Bytes.get src (b + len) do
      len <- len + 1
    done;
    len
  end
