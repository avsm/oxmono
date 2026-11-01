(* Per-operation allocation for HTTP conditional-request fields and chunking.

   Words are measured with [Gc.minor_words] around a loop, so the figure
   includes any garbage the operation produces, not just what it returns.
   [Sys.opaque_identity] keeps the result live so the loop is not optimised
   away. Note this measures the {e minor} heap only: allocations of 256 words
   or more go straight to the major heap and will read as zero here. *)

open Base

let i16 = Httpz.Buf_read.i16

let words_per_op ~iterations f =
  (* Warm up so any first-call setup is not charged to the measurement. *)
  for _ = 1 to 100 do
    ignore (Sys.opaque_identity (f ()) : int)
  done;
  let before = Stdlib.Gc.minor_words () in
  for _ = 1 to iterations do
    ignore (Sys.opaque_identity (f ()) : int)
  done;
  let after = Stdlib.Gc.minor_words () in
  (after -. before) /. Float.of_int iterations
;;

let time_per_op ~iterations f =
  for _ = 1 to 100 do
    ignore (Sys.opaque_identity (f ()) : int)
  done;
  let t0 = Unix.gettimeofday () in
  for _ = 1 to iterations do
    ignore (Sys.opaque_identity (f ()) : int)
  done;
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9 /. Float.of_int iterations
;;

let report name ~iterations f =
  let w = words_per_op ~iterations f in
  let t = time_per_op ~iterations f in
  Stdio.printf "  %-34s %8.2f ns %8.2f words\n" name t w
;;

(* ----- Conditional requests ----- *)

let date_bench () =
  Stdio.printf "\nDate\n";
  let dst = Bytes.make 128 '\000' in
  let ts = Stdlib_upstream_compatible.Float_u.of_float 1_780_000_000.0 in
  report "Date.format" ~iterations:200_000 (fun () ->
    String.length (Httpz.Date.format ts));
  report "Date.write_http_date" ~iterations:500_000 (fun () ->
    Httpz.Buf_read.to_int (Httpz.Date.write_http_date dst ~off:(i16 0) ts));
  let imf = "Sun, 06 Nov 1994 08:49:37 GMT" in
  let buf = Bytes.of_string imf in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 (String.length imf)) in
  report "Date.parse" ~iterations:500_000 (fun () ->
    let #(status, _) = Httpz.Date.parse buf sp in
    match status with
    | Httpz.Date.Valid -> 1
    | Httpz.Date.Invalid -> 0)
;;

let etag_bench () =
  Stdio.printf "\nETag\n";
  let hdr = {|W/"abc123", "def456", W/"ghi789"|} in
  let buf = Bytes.of_string hdr in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 (String.length hdr)) in
  let tags =
    Array.create ~len:(Httpz.Buf_read.to_int Httpz.Etag.max_tags) Httpz.Etag.empty
  in
  report "Etag.parse_match_header" ~iterations:500_000 (fun () ->
    let #(_, count) = Httpz.Etag.parse_match_header buf sp tags in
    Httpz.Buf_read.to_int count)
;;

let range_bench () =
  Stdio.printf "\nRange\n";
  let hdr = "bytes=1024-20479" in
  let ranges =
    Array.create ~len:(Httpz.Buf_read.to_int Httpz.Range.max_ranges) Httpz.Range.empty
  in
  report "Range.parse_string" ~iterations:500_000 (fun () ->
    let #(_, count) = Httpz.Range.parse_string hdr ranges in
    Httpz.Buf_read.to_int count)
;;

(* ----- Chunked decoding ----- *)

let chunk_bench () =
  Stdio.printf "\nChunked\n";
  let body =
    let b = Buffer.create 4096 in
    for _ = 1 to 4 do
      Buffer.add_string b (Printf.sprintf "%x\r\n" 1000);
      Buffer.add_string b (String.make 1000 'x');
      Buffer.add_string b "\r\n"
    done;
    Buffer.add_string b "0\r\n\r\n";
    Buffer.contents b
  in
  let buf = Bytes.of_string body in
  let len = i16 (String.length body) in
  report "Chunk.parse x4" ~iterations:200_000 (fun () ->
    let mutable off = i16 0 in
    let mutable n = 0 in
    let mutable go = true in
    while go do
      let #(status, c) = Httpz.Chunk.parse buf ~off ~len in
      match status with
      | Httpz.Chunk.Complete ->
        n <- n + 1;
        off <- c.#next_off
      | _ -> go <- false
    done;
    n)
;;

let () =
  Stdio.printf "Per-operation cost for HTTP fields and chunking.\n";
  date_bench ();
  etag_bench ();
  range_bench ();
  chunk_bench ();
  Stdio.printf "\n"
;;
