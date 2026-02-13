(* zero_copy.ml -- Showcase zero-copy binary access *)

open Chdb

let get_int64_le (buf : Result.bigstring) pos =
  let b0 = Char.code (Bigarray.Array1.get buf pos) in
  let b1 = Char.code (Bigarray.Array1.get buf (pos+1)) in
  let b2 = Char.code (Bigarray.Array1.get buf (pos+2)) in
  let b3 = Char.code (Bigarray.Array1.get buf (pos+3)) in
  let b4 = Char.code (Bigarray.Array1.get buf (pos+4)) in
  let b5 = Char.code (Bigarray.Array1.get buf (pos+5)) in
  let b6 = Char.code (Bigarray.Array1.get buf (pos+6)) in
  let b7 = Char.code (Bigarray.Array1.get buf (pos+7)) in
  Int64.(add (of_int b0)
    (add (shift_left (of_int b1) 8)
    (add (shift_left (of_int b2) 16)
    (add (shift_left (of_int b3) 24)
    (add (shift_left (of_int b4) 32)
    (add (shift_left (of_int b5) 40)
    (add (shift_left (of_int b6) 48)
         (shift_left (of_int b7) 56))))))))

let () =
  Connection.with_connection ~f:(fun conn ->
    (* Zero-copy UInt64 access *)
    Printf.printf "=== Zero-Copy UInt64 ===\n";
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(number) FROM numbers(10)" ~f:(fun buf ->
      Printf.printf "Buffer size: %d bytes\n" (Bigarray.Array1.dim buf);
      for i = 0 to 9 do
        let v = get_int64_le buf (i * 8) in
        Printf.printf "  [%d] = %Ld\n" i v
      done);

    (* Sum large column in zero-copy mode *)
    Printf.printf "\n=== Zero-Copy Sum (1M rows) ===\n";
    Query.with_buffer conn ~format:RowBinary
      "SELECT toUInt64(number) FROM numbers(1000000)" ~f:(fun buf ->
      let n = Bigarray.Array1.dim buf / 8 in
      let sum = ref 0L in
      for i = 0 to n - 1 do
        sum := Int64.add !sum (get_int64_le buf (i * 8))
      done;
      Printf.printf "Sum of 0..999999 = %Ld\n" !sum;
      Printf.printf "Expected:          %Ld\n" 499999500000L);

    (* Query statistics *)
    Printf.printf "\n=== Query Statistics ===\n";
    Query.with_result conn
      "SELECT number FROM numbers(100000)" ~f:(fun r ->
      let stats = Result.get_stats r in
      Printf.printf "Elapsed > 0: %b\n" (stats.elapsed > 0.0);
      Printf.printf "Rows read: %Ld\n" stats.rows_read;
      Printf.printf "Bytes read: %Ld\n" stats.bytes_read);

    (* Multiple result types from same query *)
    Printf.printf "\n=== String vs Bigstring ===\n";
    let result = Query.execute conn ~format:TSV
                   "SELECT number FROM numbers(5)" in
    let as_string = Result.to_string result in
    let as_buf = Result.to_bigstring result in
    Printf.printf "String length: %d\n" (String.length as_string);
    Printf.printf "Buffer length: %d\n" (Bigarray.Array1.dim as_buf);
    Printf.printf "Equal lengths: %b\n"
      (String.length as_string = Bigarray.Array1.dim as_buf);
    Result.destroy result
  ) ()
