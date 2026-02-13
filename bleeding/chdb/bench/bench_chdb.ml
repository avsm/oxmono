(* bench_chdb.ml -- Performance benchmarks for chdb OCaml bindings *)

open Chdb

let with_conn f = Connection.with_connection ~f ()

let time_it name iterations f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  for _ = 1 to iterations do
    f ()
  done;
  let t1 = Unix.gettimeofday () in
  let elapsed = t1 -. t0 in
  let per_op = elapsed /. float_of_int iterations in
  Printf.printf "%-40s %8d iters  %9.3f ms total  %9.3f us/op\n%!"
    name iterations (elapsed *. 1000.0) (per_op *. 1e6)

let time_throughput name total_bytes f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  let elapsed = t1 -. t0 in
  let mb_per_s = float_of_int total_bytes /. elapsed /. (1024.0 *. 1024.0) in
  Printf.printf "%-40s %9.3f ms  %8.1f MB/s  (%d bytes)\n%!"
    name (elapsed *. 1000.0) mb_per_s total_bytes

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
  Printf.printf "chdb OCaml bindings benchmark suite\n";
  Printf.printf "====================================\n\n";

  Printf.printf "--- Connection ---\n";

  time_it "connect + close" 100 (fun () ->
    let conn = Connection.connect () in
    Connection.close conn);

  time_it "with_connection (noop)" 100 (fun () ->
    with_conn (fun _conn -> ()));

  Printf.printf "\n--- Simple Queries ---\n";

  with_conn (fun conn ->
    for _ = 1 to 10 do
      ignore (Query.execute_string conn "SELECT 1")
    done;

    time_it "SELECT 1 (string)" 1000 (fun () ->
      ignore (Query.execute_string conn "SELECT 1"));

    time_it "SELECT 1 (result + destroy)" 1000 (fun () ->
      let r = Query.execute conn "SELECT 1" in
      Result.destroy r);

    time_it "SELECT number FROM numbers(100)" 500 (fun () ->
      ignore (Query.execute_string conn ~format:TSV
                "SELECT number FROM numbers(100)"));

    time_it "SELECT number FROM numbers(1000)" 200 (fun () ->
      ignore (Query.execute_string conn ~format:TSV
                "SELECT number FROM numbers(1000)"));

    time_it "SELECT number FROM numbers(10000)" 100 (fun () ->
      ignore (Query.execute_string conn ~format:TSV
                "SELECT number FROM numbers(10000)"));
  );

  Printf.printf "\n--- Result Access ---\n";

  with_conn (fun conn ->
    let query = "SELECT number FROM numbers(10000)" in

    time_it "10k rows: to_string (copy)" 200 (fun () ->
      Query.with_result conn ~format:TSV query ~f:(fun r ->
        ignore (Result.to_string r)));

    time_it "10k rows: to_bigstring (zero-copy)" 200 (fun () ->
      Query.with_result conn ~format:TSV query ~f:(fun r ->
        ignore (Result.to_bigstring r)));

    time_it "10k rows: with_buffer" 200 (fun () ->
      Query.with_buffer conn ~format:TSV query ~f:(fun buf ->
        ignore (Bigarray.Array1.dim buf)));

    time_it "stats access" 1000 (fun () ->
      Query.with_result conn
        "SELECT number FROM numbers(1000)"
        ~f:(fun r -> ignore (Result.get_stats r)));
  );

  Printf.printf "\n--- Binary Format (RowBinary) ---\n";

  with_conn (fun conn ->
    let n = 100000 in
    let query = Printf.sprintf
                  "SELECT toUInt64(number) FROM numbers(%d)" n in

    let result = Query.execute conn ~format:RowBinary query in
    let buf = Result.to_bigstring result in
    let total_bytes = Bigarray.Array1.dim buf in

    time_throughput
      (Printf.sprintf "sum %dk UInt64 (zero-copy)" (n / 1000))
      total_bytes
      (fun () ->
        let sum = ref 0L in
        for i = 0 to n - 1 do
          let v = get_int64_le buf (i * 8) in
          sum := Int64.add !sum v
        done;
        ignore !sum);

    time_throughput
      (Printf.sprintf "sum %dk UInt64 (Binary.sum)" (n / 1000))
      total_bytes
      (fun () ->
        ignore (Binary.sum_uint64 buf));

    Result.destroy result;

    time_it "query + parse 100k UInt64" 50 (fun () ->
      Query.with_buffer conn ~format:RowBinary query ~f:(fun buf ->
        let sum = ref 0L in
        for i = 0 to n - 1 do
          let v = get_int64_le buf (i * 8) in
          sum := Int64.add !sum v
        done;
        ignore !sum));
  );

  Printf.printf "\n--- Streaming ---\n";

  with_conn (fun conn ->
    time_it "stream 10k rows (count chunks)" 100 (fun () ->
      let stream = Stream.start conn ~format:TSV
                     "SELECT number FROM numbers(10000)" in
      ignore (Stream.count stream));

    time_it "stream 100k rows (fold bytes)" 20 (fun () ->
      let stream = Stream.start conn ~format:TSV
                     "SELECT number FROM numbers(100000)" in
      ignore (Stream.fold stream ~init:0 ~f:(fun acc chunk ->
        acc + Result.length chunk)));

    time_it "stream 100k rows (collect string)" 20 (fun () ->
      let stream = Stream.start conn ~format:TSV
                     "SELECT number FROM numbers(100000)" in
      ignore (Stream.collect_string stream));
  );

  Printf.printf "\n--- Large Results ---\n";

  with_conn (fun conn ->
    let sizes = [10_000; 100_000; 1_000_000] in
    List.iter (fun n ->
      let query = Printf.sprintf
                    "SELECT number FROM numbers(%d)" n in
      let label = Printf.sprintf "%dk rows (TSV string)" (n / 1000) in
      time_it label (max 1 (1_000_000 / n)) (fun () ->
        ignore (Query.execute_string conn ~format:TSV query))
    ) sizes;

    Printf.printf "\n";

    List.iter (fun n ->
      let query = Printf.sprintf
                    "SELECT toUInt64(number) FROM numbers(%d)" n in
      let label = Printf.sprintf "%dk rows (RowBinary bigstring)"
                    (n / 1000) in
      time_it label (max 1 (1_000_000 / n)) (fun () ->
        Query.with_buffer conn ~format:RowBinary query ~f:(fun buf ->
          ignore (Bigarray.Array1.dim buf)))
    ) sizes;
  );

  Printf.printf "\nDone.\n"
