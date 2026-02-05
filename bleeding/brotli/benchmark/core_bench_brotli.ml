(* Core_bench based benchmarks for Brotli *)

open Core
open Core_bench

(* Test data generators *)
let make_text_data size =
  let chars = "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ\n\t" in
  String.init size ~f:(fun _ -> chars.[Random.int (String.length chars)])

let make_repetitive_data size =
  let pattern = "Hello, World! This is a test pattern that repeats. " in
  let buf = Buffer.create size in
  while Buffer.length buf < size do
    Buffer.add_string buf pattern
  done;
  String.prefix (Buffer.contents buf) size

let make_random_data size =
  String.init size ~f:(fun _ -> Char.of_int_exn (Random.int 256))

(* Pre-generate test data *)
let text_1k = make_text_data 1024
let text_10k = make_text_data 10240
let text_100k = make_text_data 102400
let repetitive_1k = make_repetitive_data 1024
let repetitive_10k = make_repetitive_data 10240
let repetitive_100k = make_repetitive_data 102400

(* Pre-compress for decompression benchmarks *)
let compressed_text_1k = Brotli.compress ~quality:4 text_1k
let compressed_text_10k = Brotli.compress ~quality:4 text_10k
let compressed_text_100k = Brotli.compress ~quality:4 text_100k
let compressed_repetitive_1k = Brotli.compress ~quality:4 repetitive_1k
let compressed_repetitive_10k = Brotli.compress ~quality:4 repetitive_10k
let compressed_repetitive_100k = Brotli.compress ~quality:4 repetitive_100k

(* ======== Compression Benchmarks ======== *)

let bench_compress quality data () =
  ignore (Brotli.compress ~quality data)

(* ======== Decompression Benchmarks ======== *)

let bench_decompress compressed original_size () =
  let dst = Bytes.create (original_size + 1024) in
  ignore (Brotli.decompress_into
    ~src:(Bytes.of_string compressed)
    ~src_pos:0 ~src_len:(String.length compressed)
    ~dst ~dst_pos:0)

(* ======== Roundtrip Benchmarks ======== *)

let bench_roundtrip quality data () =
  let compressed = Brotli.compress ~quality data in
  let dst = Bytes.create (String.length data * 2) in
  ignore (Brotli.decompress_into
    ~src:(Bytes.of_string compressed)
    ~src_pos:0 ~src_len:(String.length compressed)
    ~dst ~dst_pos:0)

let () =
  Random.init 42;

  (* Print compression ratios *)
  Printf.printf "=== Compression Ratios ===\n";
  Printf.printf "text_1k: %.2f%% (%d -> %d)\n"
    (100.0 *. Float.of_int (String.length compressed_text_1k) /. 1024.0)
    1024 (String.length compressed_text_1k);
  Printf.printf "text_10k: %.2f%% (%d -> %d)\n"
    (100.0 *. Float.of_int (String.length compressed_text_10k) /. 10240.0)
    10240 (String.length compressed_text_10k);
  Printf.printf "text_100k: %.2f%% (%d -> %d)\n"
    (100.0 *. Float.of_int (String.length compressed_text_100k) /. 102400.0)
    102400 (String.length compressed_text_100k);
  Printf.printf "repetitive_10k: %.2f%% (%d -> %d)\n"
    (100.0 *. Float.of_int (String.length compressed_repetitive_10k) /. 10240.0)
    10240 (String.length compressed_repetitive_10k);
  Printf.printf "repetitive_100k: %.2f%% (%d -> %d)\n"
    (100.0 *. Float.of_int (String.length compressed_repetitive_100k) /. 102400.0)
    102400 (String.length compressed_repetitive_100k);
  Printf.printf "\n";

  Command_unix.run (Bench.make_command [
    (* Compression at different qualities and sizes *)
    Bench.Test.create ~name:"compress/q1_text_1k" (bench_compress 1 text_1k);
    Bench.Test.create ~name:"compress/q2_text_1k" (bench_compress 2 text_1k);
    Bench.Test.create ~name:"compress/q4_text_1k" (bench_compress 4 text_1k);
    Bench.Test.create ~name:"compress/q4_text_10k" (bench_compress 4 text_10k);
    Bench.Test.create ~name:"compress/q4_text_100k" (bench_compress 4 text_100k);
    Bench.Test.create ~name:"compress/q4_repetitive_1k" (bench_compress 4 repetitive_1k);
    Bench.Test.create ~name:"compress/q4_repetitive_10k" (bench_compress 4 repetitive_10k);
    Bench.Test.create ~name:"compress/q4_repetitive_100k" (bench_compress 4 repetitive_100k);
    Bench.Test.create ~name:"compress/q6_text_10k" (bench_compress 6 text_10k);

    (* Decompression *)
    Bench.Test.create ~name:"decompress/text_1k"
      (bench_decompress compressed_text_1k 1024);
    Bench.Test.create ~name:"decompress/text_10k"
      (bench_decompress compressed_text_10k 10240);
    Bench.Test.create ~name:"decompress/text_100k"
      (bench_decompress compressed_text_100k 102400);
    Bench.Test.create ~name:"decompress/repetitive_1k"
      (bench_decompress compressed_repetitive_1k 1024);
    Bench.Test.create ~name:"decompress/repetitive_10k"
      (bench_decompress compressed_repetitive_10k 10240);
    Bench.Test.create ~name:"decompress/repetitive_100k"
      (bench_decompress compressed_repetitive_100k 102400);

    (* Full roundtrip *)
    Bench.Test.create ~name:"roundtrip/q4_text_1k" (bench_roundtrip 4 text_1k);
    Bench.Test.create ~name:"roundtrip/q4_text_10k" (bench_roundtrip 4 text_10k);
    Bench.Test.create ~name:"roundtrip/q4_repetitive_10k" (bench_roundtrip 4 repetitive_10k);
  ])
