(* bench_alloc.ml - per-operation allocation for the paths core_bench misses.

   [bench_httpz] covers parsing and response writing but nothing in the router,
   the conditional-request modules, or chunked decoding. Those are exactly the
   places where an allocation can appear unnoticed, since the library's headline
   claim is that the hot paths are allocation-free.

   Words are measured with [Gc.minor_words] around a loop, so the figure
   includes any garbage the operation produces, not just what it returns.
   [Sys.opaque_identity] keeps the result live so the loop is not optimised
   away. Note this measures the {e minor} heap only: allocations of 256 words
   or more go straight to the major heap and will read as zero here. *)

open Base
module R = Httpz_route
module I64 = Stdlib_upstream_compatible.Int64_u

let i16 = Httpz.Buf_read.i16
let limits = Httpz.default_limits

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

(* ----- Router ----- *)

let routes =
  R.of_list
    [ R.get_ [] (fun _ctx respond -> R.plain respond "root")
    ; R.get_ [ "api"; "status" ] (fun _ctx respond -> R.plain respond "status")
    ; R.get_ [ "api"; "v1"; "health" ] (fun _ctx respond -> R.plain respond "health")
    ; R.get (R.( / ) "users" (R.seg R.root)) (fun (_id, ()) _ctx respond ->
        R.plain respond "user")
    ; R.get (R.( / ) "static" R.tail) (fun _segs _ctx respond -> R.plain respond "static")
    ]
;;

let make_request target =
  let text = Printf.sprintf "GET %s HTTP/1.1\r\nHost: x\r\n\r\n" target in
  let buf = Bytes.make Httpz.buffer_size '\000' in
  let len = String.length text in
  Bytes.From_string.blit ~src:text ~src_pos:0 ~dst:buf ~dst_pos:0 ~len;
  buf, len
;;

(* The parser's header list is [local_] and so cannot be captured here. None
   of the routes above declares a header requirement, so dispatching with an
   empty list measures the walk and capture cost on its own. *)
let bench_dispatch target =
  let buf, len = make_request target in
  let #(_, req, _) = Httpz.parse buf ~len:(i16 len) ~limits in
  let path = req.#path in
  let query = req.#query in
  let meth = req.#meth in
  let empty = Httpz.Span.make ~off:(i16 0) ~len:(i16 0) in
  fun () ->
    let matched =
      R.dispatch
        buf
        ~meth
        ~path
        ~query
        ~body:empty
        ~content_length:(I64.of_int64 (-1L))
        ~headers:[]
        routes
        ~respond:(fun ~status:_ ~headers:_ _body -> ())
    in
    if matched then 1 else 0
;;

let router_bench () =
  Stdio.printf "\nRouter dispatch\n";
  List.iter
    [ "/"; "/api/status"; "/api/v1/health"; "/users/12345"; "/static/a/b/c.css"; "/nope" ]
    ~f:(fun target -> report target ~iterations:200_000 (bench_dispatch target))
;;

(* Parse and dispatch one request, as a server does per keep-alive round trip.
   The target is split once, by the parser, and the split is carried on the
   request rather than recomputed here. *)
let bench_parse_dispatch target =
  let buf, len = make_request target in
  let len16 = i16 len in
  fun () ->
    let #(_status, req, headers) = Httpz.parse buf ~len:len16 ~limits in
    let empty = Httpz.Span.make ~off:(i16 0) ~len:(i16 0) in
    let matched =
      R.dispatch
        buf
        ~meth:req.#meth
        ~path:req.#path ~query:req.#query
        ~body:empty
        ~content_length:(I64.of_int64 (-1L))
        ~headers
        routes
        ~respond:(fun ~status:_ ~headers:_ _body -> ())
    in
    if matched then 1 else 0
;;

let parse_dispatch_bench () =
  Stdio.printf "\nParse + dispatch\n";
  List.iter
    [ "/api/status"; "/users/12345"; "/static/a/b/c.css?x=1" ]
    ~f:(fun target ->
      report target ~iterations:200_000 (bench_parse_dispatch target))
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
  Stdio.printf "Per-operation cost for the paths core_bench does not cover.\n";
  router_bench ();
  parse_dispatch_bench ();
  date_bench ();
  etag_bench ();
  range_bench ();
  chunk_bench ();
  Stdio.printf "\n"
;;
