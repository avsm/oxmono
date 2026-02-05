(* Simple benchmark for Brotli compression/decompression *)

let random_data size =
  let data = Bytes.create size in
  for i = 0 to size - 1 do
    Bytes.set data i (Char.chr (Random.int 256))
  done;
  Bytes.to_string data

let text_like_data size =
  let chars = "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ\n\t" in
  let data = Bytes.create size in
  for i = 0 to size - 1 do
    Bytes.set data i chars.[Random.int (String.length chars)]
  done;
  Bytes.to_string data

let repetitive_data size =
  let pattern = "Hello, World! This is a test pattern that repeats. " in
  let buf = Buffer.create size in
  while Buffer.length buf < size do
    Buffer.add_string buf pattern
  done;
  Buffer.sub buf 0 size

let time_it name iterations f =
  let start = Unix.gettimeofday () in
  for _ = 1 to iterations do
    ignore (f ())
  done;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%s: %.3f ms/iter (total %.3f s for %d iterations)\n%!"
    name (elapsed *. 1000.0 /. float_of_int iterations) elapsed iterations

let bench_compress ?(quality=Brotli.Q4) data iterations =
  time_it (Printf.sprintf "Compress (quality=%d, %d bytes)" (Brotli.quality_to_int quality) (String.length data))
    iterations (fun () ->
      Brotli.compress ~quality data
    )

let bench_decompress compressed original_size iterations =
  let dst_len = original_size + 1024 in  (* Add some buffer *)
  let dst = Bytes.create dst_len in
  time_it (Printf.sprintf "Decompress (%d bytes -> %d)" (String.length compressed) original_size)
    iterations (fun () ->
      ignore (Brotli.decompress_into ~src:(Bytes.of_string compressed)
        ~src_pos:0 ~src_len:(String.length compressed)
        ~dst ~dst_pos:0)
    )

let bench_roundtrip ?(quality=Brotli.Q4) data iterations =
  time_it (Printf.sprintf "Roundtrip (quality=%d, %d bytes)" (Brotli.quality_to_int quality) (String.length data))
    iterations (fun () ->
      let compressed = Brotli.compress ~quality data in
      let dst = Bytes.create (String.length data * 2) in
      ignore (Brotli.decompress_into ~src:(Bytes.of_string compressed)
        ~src_pos:0 ~src_len:(String.length compressed)
        ~dst ~dst_pos:0)
    )

let () =
  Random.self_init ();
  print_endline "=== Brotli Benchmark ===\n";

  (* Test different data types *)
  let sizes = [1024; 10_240; 102_400; 1_024_000] in
  let iterations = [1000; 500; 100; 20] in

  print_endline "--- Text-like data ---";
  List.iter2 (fun size iters ->
    let data = text_like_data size in
    bench_compress ~quality:Brotli.Q4 data iters;
    let compressed = Brotli.compress ~quality:Brotli.Q4 data in
    Printf.printf "  Compression ratio: %.2f%%\n"
      (100.0 *. float_of_int (String.length compressed) /. float_of_int size);
    bench_decompress compressed size (iters * 5);  (* Decompress is faster, run more *)
    print_newline ()
  ) sizes iterations;

  print_endline "--- Repetitive data ---";
  List.iter2 (fun size iters ->
    let data = repetitive_data size in
    bench_compress ~quality:Brotli.Q4 data iters;
    let compressed = Brotli.compress ~quality:Brotli.Q4 data in
    Printf.printf "  Compression ratio: %.2f%%\n"
      (100.0 *. float_of_int (String.length compressed) /. float_of_int size);
    bench_decompress compressed size (iters * 5);
    print_newline ()
  ) sizes iterations;

  print_endline "--- Random data ---";
  List.iter2 (fun size iters ->
    let data = random_data size in
    bench_compress ~quality:Brotli.Q4 data iters;
    let compressed = Brotli.compress ~quality:Brotli.Q4 data in
    Printf.printf "  Compression ratio: %.2f%%\n"
      (100.0 *. float_of_int (String.length compressed) /. float_of_int size);
    bench_decompress compressed size (iters * 5);
    print_newline ()
  ) sizes iterations;

  print_endline "=== Benchmark complete ==="
