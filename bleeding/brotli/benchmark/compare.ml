(* Comparison benchmark with same test data as C brotli *)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let data = really_input_string ic n in
  close_in ic;
  data

let time_it name iterations f =
  let start = Unix.gettimeofday () in
  for _ = 1 to iterations do
    ignore (f ())
  done;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%s: %.2f ms/iter\n%!" name (elapsed *. 1000.0 /. float_of_int iterations)

let benchmark name file comp_iters decomp_iters =
  Printf.printf "\n--- %s ---\n%!" name;
  let data = read_file file in
  let orig_size = String.length data in

  (* Compression *)
  let compressed = ref "" in
  time_it "Compress" comp_iters (fun () ->
    compressed := Brotli.compress ~quality:Brotli.Q4 data;
    !compressed
  );
  let comp_size = String.length !compressed in
  Printf.printf "Ratio: %.2f%%\n%!" (100.0 *. float_of_int comp_size /. float_of_int orig_size);

  (* Decompression *)
  let dst = Bytes.create (orig_size + 1024) in
  let comp_bytes = Bytes.of_string !compressed in
  time_it "Decompress" decomp_iters (fun () ->
    ignore (Brotli.decompress_into
      ~src:comp_bytes ~src_pos:0 ~src_len:(String.length !compressed)
      ~dst ~dst_pos:0)
  )

let () =
  Printf.printf "=== OCaml Brotli Benchmark (quality 4) ===\n%!";
  benchmark "Text-like 1MB" "/tmp/test_text_1mb.txt" 20 100;
  benchmark "Repetitive 1MB" "/tmp/test_rep_1mb.txt" 20 100;
  benchmark "Random 1MB" "/tmp/test_rand_1mb.bin" 20 100
