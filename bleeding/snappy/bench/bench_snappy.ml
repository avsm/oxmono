(* Snappy benchmark suite *)

(* Generate test data *)
let make_repeated n pattern =
  let plen = String.length pattern in
  let buf = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set buf i (String.get pattern (i mod plen))
  done;
  Bytes.unsafe_to_string buf

let make_random n seed =
  let buf = Bytes.create n in
  let state = ref seed in
  for i = 0 to n - 1 do
    state := !state * 1103515245 + 12345;
    Bytes.set buf i (Char.chr ((!state lsr 16) land 0xFF))
  done;
  Bytes.unsafe_to_string buf

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Timing utilities *)
let time_ns () = Unix.gettimeofday () *. 1e9

let benchmark name iterations data f =
  (* Warmup *)
  for _ = 1 to 5 do
    ignore (f data)
  done;

  (* Actual benchmark *)
  let start = time_ns () in
  for _ = 1 to iterations do
    ignore (f data)
  done;
  let elapsed = time_ns () -. start in

  let total_bytes = iterations * String.length data in
  let bytes_per_sec = float_of_int total_bytes /. (elapsed /. 1e9) in
  let mb_per_sec = bytes_per_sec /. (1024.0 *. 1024.0) in

  Printf.printf "%-30s %8.2f MB/s (%d iterations, %.3f ms total)\n%!"
    name mb_per_sec iterations (elapsed /. 1e6)

let benchmark_compression name data =
  let iterations =
    (* Adjust iterations based on data size for reasonable run time *)
    let size = String.length data in
    if size < 1000 then 10000
    else if size < 10000 then 1000
    else if size < 100000 then 100
    else 10
  in
  benchmark (name ^ " compress") iterations data Snappy.compress;

  let compressed = Snappy.compress data in
  let ratio = float_of_int (String.length compressed) /.
              float_of_int (String.length data) *. 100.0 in
  Printf.printf "  Compression ratio: %.1f%% (%d -> %d bytes)\n%!"
    ratio (String.length data) (String.length compressed);

  benchmark (name ^ " decompress") iterations compressed Snappy.decompress_exn

let benchmark_framed name data =
  let iterations =
    let size = String.length data in
    if size < 10000 then 100
    else if size < 100000 then 50
    else 10
  in
  benchmark (name ^ " framed compress") iterations data Snappy.compress_framed;

  let compressed = Snappy.compress_framed data in
  benchmark (name ^ " framed decompress") iterations compressed
    (fun s -> match Snappy.decompress_framed s with
      | Ok x -> x
      | Error e -> failwith e)

let () =
  Printf.printf "Snappy OCaml Benchmark Suite\n";
  Printf.printf "============================\n\n";

  (* Test data *)
  let data_1k_repeated = make_repeated 1024 "ABCDEFGH" in
  let data_10k_repeated = make_repeated 10240 "Hello World! " in
  let data_100k_repeated = make_repeated 102400 "Repeated pattern for compression test. " in

  let data_1k_random = make_random 1024 12345 in
  let data_10k_random = make_random 10240 67890 in
  let data_100k_random = make_random 102400 11111 in

  Printf.printf "=== Highly Compressible Data ===\n";
  benchmark_compression "1KB repeated" data_1k_repeated;
  Printf.printf "\n";
  benchmark_compression "10KB repeated" data_10k_repeated;
  Printf.printf "\n";
  benchmark_compression "100KB repeated" data_100k_repeated;
  Printf.printf "\n";

  Printf.printf "=== Random (Incompressible) Data ===\n";
  benchmark_compression "1KB random" data_1k_random;
  Printf.printf "\n";
  benchmark_compression "10KB random" data_10k_random;
  Printf.printf "\n";
  benchmark_compression "100KB random" data_100k_random;
  Printf.printf "\n";

  Printf.printf "=== Framing Format ===\n";
  benchmark_framed "100KB repeated" data_100k_repeated;
  Printf.printf "\n";
  benchmark_framed "100KB random" data_100k_random;
  Printf.printf "\n";

  (* Try to load corpus files if available *)
  Printf.printf "=== Corpus Files ===\n";
  let corpus_files = [
    "test/testdata/alice29.txt";
    "test/testdata/html";
    "test/testdata/urls.10K";
  ] in
  List.iter (fun path ->
    if Sys.file_exists path then begin
      let data = read_file path in
      benchmark_compression path data;
      Printf.printf "\n"
    end else
      Printf.printf "Skipping %s (not found)\n\n" path
  ) corpus_files;

  (* Benchmark reusable compression context *)
  Printf.printf "=== Reusable Context vs Fresh Allocation ===\n";
  let ctx = Snappy.create_compress_ctx () in
  let dst = Bytes.create (Snappy.max_compressed_length 1024) in
  let test_msgs = Array.init 1000 (fun i ->
    Bytes.of_string (make_repeated 1024 (Printf.sprintf "msg%d" i))
  ) in

  (* Fresh allocation *)
  let iterations = 10 in
  let start = time_ns () in
  for _ = 1 to iterations do
    for j = 0 to 999 do
      ignore (Snappy.compress_into
        ~src:test_msgs.(j) ~src_pos:0 ~src_len:1024
        ~dst ~dst_pos:0)
    done
  done;
  let elapsed_fresh = time_ns () -. start in
  let mb_fresh = float_of_int (iterations * 1000 * 1024) /. (elapsed_fresh /. 1e9) /. (1024.0 *. 1024.0) in
  Printf.printf "Fresh allocation:   %8.2f MB/s (%d x 1000 messages)\n%!" mb_fresh iterations;

  (* Reusable context *)
  let start = time_ns () in
  for _ = 1 to iterations do
    for j = 0 to 999 do
      ignore (Snappy.compress_with_ctx ctx
        ~src:test_msgs.(j) ~src_pos:0 ~src_len:1024
        ~dst ~dst_pos:0)
    done
  done;
  let elapsed_ctx = time_ns () -. start in
  let mb_ctx = float_of_int (iterations * 1000 * 1024) /. (elapsed_ctx /. 1e9) /. (1024.0 *. 1024.0) in
  Printf.printf "Reusable context:   %8.2f MB/s (%d x 1000 messages)\n%!" mb_ctx iterations;
  Printf.printf "Speedup: %.2fx\n\n%!" (mb_ctx /. mb_fresh);

  Printf.printf "Benchmark complete.\n"
