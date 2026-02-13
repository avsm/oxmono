(* streaming.ml -- Showcase streaming query support *)

open Chdb

let () =
  Connection.with_connection ~f:(fun conn ->
    Printf.printf "=== Stream Fold (sum bytes) ===\n";
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(10000)" in
    let total_bytes = Stream.fold stream ~init:0 ~f:(fun acc chunk ->
      acc + Result.length chunk) in
    Printf.printf "Total bytes streamed: %d\n" total_bytes;

    Printf.printf "\n=== Stream Chunk Count ===\n";
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(50000)" in
    let chunks = Stream.count stream in
    Printf.printf "Number of chunks: %d\n" chunks;

    Printf.printf "\n=== Stream Collect (small) ===\n";
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(5)" in
    let data = Stream.collect_string stream in
    print_string data;

    Printf.printf "\n=== Stream Cancel ===\n";
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(1000000)" in
    let first = Stream.fetch stream in
    (match first with
     | Some chunk ->
       Printf.printf "First chunk: %d bytes\n" (Result.length chunk)
     | None ->
       Printf.printf "No first chunk\n");
    Stream.cancel stream;
    let after = Stream.fetch stream in
    Printf.printf "After cancel: %s\n"
      (if Option.is_none after then "exhausted" else "still data");

    Printf.printf "\n=== Stream to Seq ===\n";
    let stream = Stream.start conn ~format:TSV
                   "SELECT number FROM numbers(100)" in
    let seq = Stream.to_seq stream in
    let total = Seq.fold_left (fun acc chunk ->
      acc + Result.length chunk) 0 seq in
    Printf.printf "Seq total bytes: %d\n" total
  ) ()
