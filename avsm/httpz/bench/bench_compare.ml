(* bench_compare.ml - Comparative benchmark: httpz (OxCaml) vs httpe (Eio)

   Compares parsing performance for HTTP/1.1 requests across:
   - Small requests (minimal GET)
   - Medium requests (typical browser request with ~10 headers)
   - Large requests (many headers, long values)

   Measures:
   - Throughput (operations per second)
   - Memory allocation (words per operation)
*)

open Base

(* ============================================
   Benchmark Infrastructure
   ============================================ *)

module Bench = struct
  [@@@warning "-69"] (* Suppress unused field warnings *)

  type result =
    { name : string
    ; iterations : int
    ; total_time : float
    ; avg_time_ns : float
    ; ops_per_sec : float
    ; minor_words_per_op : float
    ; major_words_per_op : float
    ; promoted_words_per_op : float
    }

  let run ~name ~iterations f =
    (* Warmup *)
    for _ = 1 to Int.min 1000 iterations do
      ignore (f ())
    done;
    (* Force GC to get clean baseline *)
    Stdlib.Gc.full_major ();
    Stdlib.Gc.compact ();
    (* Get GC stats before *)
    let gc_before = Stdlib.Gc.quick_stat () in
    (* Actual benchmark *)
    let start = Unix.gettimeofday () in
    for _ = 1 to iterations do
      ignore (f ())
    done;
    let stop = Unix.gettimeofday () in
    (* Get GC stats after *)
    let gc_after = Stdlib.Gc.quick_stat () in
    let total_time = stop -. start in
    let avg_time_ns = total_time /. Float.of_int iterations *. 1_000_000_000.0 in
    let ops_per_sec = Float.of_int iterations /. total_time in
    (* Calculate allocations per operation *)
    let iters = Float.of_int iterations in
    let minor_words_per_op = (gc_after.minor_words -. gc_before.minor_words) /. iters in
    let major_words_per_op = (gc_after.major_words -. gc_before.major_words) /. iters in
    let promoted_words_per_op =
      (gc_after.promoted_words -. gc_before.promoted_words) /. iters
    in
    { name
    ; iterations
    ; total_time
    ; avg_time_ns
    ; ops_per_sec
    ; minor_words_per_op
    ; major_words_per_op
    ; promoted_words_per_op
    }
  ;;

  let total_alloc r = r.minor_words_per_op +. r.major_words_per_op

  let print_result r =
    Stdio.printf
      "  %-35s %8.0f ns/op  %10.0f ops/s  %6.0f words/op\n"
      r.name
      r.avg_time_ns
      r.ops_per_sec
      (total_alloc r)
  ;;

  let print_comparison ~baseline ~test =
    let speedup = baseline.avg_time_ns /. test.avg_time_ns in
    let alloc_ratio = total_alloc baseline /. Float.max 1.0 (total_alloc test) in
    let faster_slower = if Float.(speedup > 1.0) then "faster" else "slower" in
    Stdio.printf
      "    -> %s is %.2fx %s, %.1fx %s allocations\n\n"
      test.name
      speedup
      faster_slower
      alloc_ratio
      (if Float.(alloc_ratio > 1.0) then "fewer" else "more")
  ;;
end

(* ============================================
   Test Data - Small, Medium, Large requests
   ============================================ *)

(* Small: Minimal GET request *)
let small_request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"

(* Medium: Typical browser request *)
let medium_request =
  "GET /index.html HTTP/1.1\r\n\
   Host: www.example.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: en-US,en;q=0.9\r\n\
   Accept-Encoding: gzip, deflate, br\r\n\
   Connection: keep-alive\r\n\
   Cache-Control: max-age=0\r\n\
   Cookie: session=abc123; tracking=xyz789\r\n\
   If-None-Match: \"abc123\"\r\n\
   If-Modified-Since: Mon, 01 Jan 2024 00:00:00 GMT\r\n\
   \r\n"
;;

(* Large: Many headers with long values *)
let large_request =
  "POST /api/v1/users/12345/documents HTTP/1.1\r\n\
   Host: api.example.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 \
   (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36\r\n\
   Accept: application/json, text/plain, */*\r\n\
   Accept-Language: en-US,en;q=0.9,de;q=0.8,fr;q=0.7\r\n\
   Accept-Encoding: gzip, deflate, br\r\n\
   Content-Type: application/json; charset=utf-8\r\n\
   Content-Length: 256\r\n\
   Authorization: Bearer \
   eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c\r\n\
   X-Request-ID: 550e8400-e29b-41d4-a716-446655440000\r\n\
   X-Correlation-ID: 7c9e6679-7425-40de-944b-e07fc1f90ae7\r\n\
   X-Forwarded-For: 192.168.1.1, 10.0.0.1\r\n\
   X-Forwarded-Proto: https\r\n\
   X-Forwarded-Host: www.example.com\r\n\
   Origin: https://www.example.com\r\n\
   Referer: https://www.example.com/dashboard\r\n\
   Cookie: session=abc123def456; csrf_token=xyz789; \
   preferences=dark_mode%3Dtrue%26lang%3Den; _ga=GA1.2.123456789.1234567890\r\n\
   Cache-Control: no-cache, no-store, must-revalidate\r\n\
   Pragma: no-cache\r\n\
   Connection: keep-alive\r\n\
   Sec-Fetch-Dest: empty\r\n\
   Sec-Fetch-Mode: cors\r\n\
   Sec-Fetch-Site: same-origin\r\n\
   \r\n"
;;

(* ============================================
   Httpz (OxCaml) Benchmarks
   ============================================ *)

module Httpz_bench = struct
  let httpz_buf = Bytes.create Httpz.buffer_size
  let limits = Httpz.default_limits
  let i16 = Httpz.Buf_read.i16

  let copy_to_buffer s =
    let len = String.length s in
    for i = 0 to len - 1 do
      Bytes.set httpz_buf i (String.get s i)
    done;
    len
  ;;

  let parse_request data =
    let len = copy_to_buffer data in
    let #(status, req, headers) = Httpz.parse httpz_buf ~len:(i16 len) ~limits in
    (* Touch results to prevent optimization *)
    let _ = req.#body_off in
    let _ = headers in
    status
  ;;
end

(* ============================================
   Httpe (Eio) Benchmarks
   ============================================ *)

module Httpe_bench = struct
  let parse_request data =
    let reader = Eio.Buf_read.of_string data in
    let req = Httpe.Read.request reader in
    (* Touch results to prevent optimization *)
    let _ = req.Httpe.Request.target in
    let _ = req.Httpe.Request.headers in
    req
  ;;
end

(* ============================================
   Run Benchmarks
   ============================================ *)

let run_size_benchmark ~name ~data ~iterations =
  Stdio.printf "\n%s (%d bytes, %d iterations)\n" name (String.length data) iterations;
  Stdio.print_string (String.make 60 '-');
  Stdio.print_endline "";
  let httpz_result =
    Bench.run ~name:"httpz (OxCaml)" ~iterations (fun () ->
      Httpz_bench.parse_request data)
  in
  Bench.print_result httpz_result;
  let httpe_result =
    Bench.run ~name:"httpe (Eio)" ~iterations (fun () -> Httpe_bench.parse_request data)
  in
  Bench.print_result httpe_result;
  Bench.print_comparison ~baseline:httpe_result ~test:httpz_result;
  httpz_result, httpe_result
;;

let run_throughput_benchmark () =
  Stdio.printf "\n\nTHROUGHPUT TEST (1000 iterations per call)\n";
  Stdio.print_string (String.make 60 '=');
  Stdio.print_endline "";
  let iterations = 1000 in
  let outer_iterations = 1000 in
  Stdio.printf "\nSmall request throughput:\n";
  let httpz_tp =
    Bench.run ~name:"httpz throughput" ~iterations:outer_iterations (fun () ->
      for _ = 1 to iterations do
        ignore (Httpz_bench.parse_request small_request)
      done)
  in
  let httpz_total_ops = Float.of_int (outer_iterations * iterations) in
  let httpz_ops_per_sec = httpz_total_ops /. httpz_tp.total_time in
  Stdio.printf "  httpz: %.0f requests/sec\n" httpz_ops_per_sec;
  let httpe_tp =
    Bench.run ~name:"httpe throughput" ~iterations:outer_iterations (fun () ->
      for _ = 1 to iterations do
        ignore (Httpe_bench.parse_request small_request)
      done)
  in
  let httpe_total_ops = Float.of_int (outer_iterations * iterations) in
  let httpe_ops_per_sec = httpe_total_ops /. httpe_tp.total_time in
  Stdio.printf "  httpe: %.0f requests/sec\n" httpe_ops_per_sec;
  let ratio = httpz_ops_per_sec /. httpe_ops_per_sec in
  Stdio.printf
    "  -> httpz is %.2fx %s\n"
    ratio
    (if Float.(ratio > 1.0) then "faster" else "slower")
;;

let print_summary results =
  Stdio.printf "\n\nSUMMARY\n";
  Stdio.print_string (String.make 60 '=');
  Stdio.print_endline "";
  Stdio.printf
    "\n%-15s %12s %12s %10s %10s\n"
    "Size"
    "httpz ns/op"
    "httpe ns/op"
    "Speedup"
    "Alloc Ratio";
  Stdio.print_string (String.make 60 '-');
  Stdio.print_endline "";
  List.iter results ~f:(fun (name, (httpz, httpe)) ->
    let speedup = httpe.Bench.avg_time_ns /. httpz.Bench.avg_time_ns in
    let alloc_ratio =
      Bench.total_alloc httpe /. Float.max 1.0 (Bench.total_alloc httpz)
    in
    Stdio.printf
      "%-15s %12.0f %12.0f %9.2fx %9.1fx\n"
      name
      httpz.avg_time_ns
      httpe.avg_time_ns
      speedup
      alloc_ratio)
;;

let () =
  Stdio.print_endline "";
  Stdio.print_endline "============================================================";
  Stdio.print_endline "  HTTP Parser Comparison: httpz (OxCaml) vs httpe (Eio)";
  Stdio.print_endline "============================================================";
  Stdio.print_endline "";
  Stdio.print_endline "httpz: Stack-allocated parser using OxCaml unboxed types";
  Stdio.print_endline "httpe: Eio-based parser using Buf_read streaming";
  Stdio.print_endline "";
  (* Run benchmarks in order (use let to force left-to-right evaluation) *)
  let small =
    ( "Small"
    , run_size_benchmark ~name:"SMALL REQUEST" ~data:small_request ~iterations:500_000 )
  in
  let medium =
    ( "Medium"
    , run_size_benchmark ~name:"MEDIUM REQUEST" ~data:medium_request ~iterations:200_000 )
  in
  let large =
    ( "Large"
    , run_size_benchmark ~name:"LARGE REQUEST" ~data:large_request ~iterations:100_000 )
  in
  let results = [ small; medium; large ] in
  run_throughput_benchmark ();
  print_summary results;
  Stdio.print_endline "";
  Stdio.print_endline "============================================================";
  Stdio.print_endline "  Benchmark Complete";
  Stdio.print_endline "============================================================";
  Stdio.print_endline ""
;;
