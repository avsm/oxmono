(* Crowbar fuzz testing for all public Httpz.Chunk parsers: arbitrary bytes at
   arbitrary in-bounds offsets must never raise, and every offset or span a
   successful parse reports must stay within the parsed window.
   To run: dune exec httpz/fuzz/fuzz_chunk.exe
   With AFL: afl-fuzz -i fuzz/corpus -o fuzz/findings -- ./_build/default/httpz/fuzz/fuzz_chunk.exe @@ *)
open Crowbar

let i16 = Httpz.Buf_read.i16
let to_int = Httpz.Buf_read.to_int

(* Generate a buffer together with an in-bounds [off, len) window, so a
   crash can only be attributed to the chunk parser, never to reading past
   memory the parser was never given. *)
let windowed_bytes =
  dynamic_bind bytes (fun s ->
    let n = String.length s in
    map [ range (n + 1); range (n + 1) ] (fun a b ->
      let off = min a b
      and len = max a b in
      s, off, len))
;;

let max_chunk_size_gen =
  choose
    [ const Httpz.Chunk.default_max_chunk_size; range 1_000_000; const 0 ]
;;

let in_window ~off ~len span =
  let first = Httpz.Span.off span
  and size = Httpz.Span.len span in
  first >= off && size >= 0 && first <= len && size <= len - first
;;

let check_chunk ~off ~len (t : Httpz.Chunk.t) =
  let data_off = to_int t.#data_off
  and data_len = to_int t.#data_len
  and next_off = to_int t.#next_off in
  check (data_off >= off);
  check (data_len >= 0);
  check (data_off <= len);
  check (data_len <= len - data_off);
  check (next_off >= data_off);
  check (next_off <= len)
;;

let test_parse (s, off, len) =
  let buf = Bytes.of_string s in
  try
    let #(status, chunk) =
      Httpz.Chunk.parse buf ~off:(i16 off) ~len:(i16 len)
    in
    match status with
    | Httpz.Chunk.Complete | Httpz.Chunk.Done ->
        check_chunk ~off ~len chunk
    | Httpz.Chunk.Partial
    | Httpz.Chunk.Malformed
    | Httpz.Chunk.Chunk_too_large -> check true
  with e -> failf "Httpz.Chunk.parse raised: %s" (Printexc.to_string e)
;;

let test_parse_with_limit (s, off, len) max_chunk_size =
  let buf = Bytes.of_string s in
  try
    let #(status, chunk) =
      Httpz.Chunk.parse_with_limit buf ~off:(i16 off) ~len:(i16 len)
        ~max_chunk_size
    in
    match status with
    | Httpz.Chunk.Complete | Httpz.Chunk.Done ->
        check_chunk ~off ~len chunk
    | Httpz.Chunk.Partial
    | Httpz.Chunk.Malformed
    | Httpz.Chunk.Chunk_too_large -> check true
  with e ->
    failf "Httpz.Chunk.parse_with_limit raised: %s" (Printexc.to_string e)
;;

let test_parse_header (s, off, len) max_chunk_size =
  let buf = Bytes.of_string s in
  try
    let #(status, size, data_off) =
      Httpz.Chunk.parse_header buf ~off:(i16 off) ~len:(i16 len)
        ~max_chunk_size
    in
    match status with
    | Httpz.Chunk.Complete | Httpz.Chunk.Done ->
        check (to_int data_off >= off);
        check (to_int data_off <= len);
        check (size >= 0)
    | Httpz.Chunk.Partial
    | Httpz.Chunk.Malformed
    | Httpz.Chunk.Chunk_too_large -> check true
  with e -> failf "Httpz.Chunk.parse_header raised: %s" (Printexc.to_string e)
;;

let header_count_gen = choose [ const 0; const 1; range 128 ]

let rec check_header_spans ~off ~len
    (headers : Httpz.Header.t list @ local) =
  match headers with
  | [] -> ()
  | header :: rest ->
      check (in_window ~off ~len header.name_span);
      check (in_window ~off ~len header.value);
      check_header_spans ~off ~len rest
;;

let test_parse_trailers (s, off, len) max_header_count =
  let buf = Bytes.of_string s in
  try
    let #(status, end_off, headers) =
      Httpz.Chunk.parse_trailers buf ~off:(i16 off) ~len:(i16 len)
        ~max_header_count:(i16 max_header_count)
    in
    check (to_int end_off >= off);
    check (to_int end_off <= len);
    match status with
    | Httpz.Chunk.Trailer_complete ->
        check_header_spans ~off ~len headers
    | Httpz.Chunk.Trailer_partial
    | Httpz.Chunk.Trailer_malformed
    | Httpz.Chunk.Trailer_bare_cr -> check true
  with e ->
    failf "Httpz.Chunk.parse_trailers raised: %s" (Printexc.to_string e)
;;

let () =
  add_test
    ~name:"Httpz.Chunk.parse: never raises, offsets in bounds"
    [ windowed_bytes ] test_parse;
  add_test
    ~name:"Httpz.Chunk.parse_with_limit: never raises, offsets in bounds"
    [ windowed_bytes; max_chunk_size_gen ]
    test_parse_with_limit;
  add_test
    ~name:"Httpz.Chunk.parse_header: never raises, offsets in bounds"
    [ windowed_bytes; max_chunk_size_gen ]
    test_parse_header;
  add_test
    ~name:"Httpz.Chunk.parse_trailers: never raises, spans in bounds"
    [ windowed_bytes; header_count_gen ]
    test_parse_trailers
;;
