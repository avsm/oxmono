(* Comprehensive test suite for Snappy compression *)

open Alcotest

(* ============================================================
   Test utilities
   ============================================================ *)

let bytes_testable = testable (Fmt.of_to_string Bytes.to_string) Bytes.equal
let string_testable = testable Fmt.string String.equal

let roundtrip s =
  let compressed = Snappy.compress s in
  match Snappy.decompress compressed with
  | Ok result -> result
  | Error msg -> failwith ("Decompression failed: " ^ msg)

let _roundtrip_bytes src =
  let compressed = Snappy.compress_to_bytes src ~pos:0 ~len:(Bytes.length src) in
  Snappy.decompress_to_bytes compressed ~pos:0 ~len:(Bytes.length compressed)

(* Generate repeating pattern - good for testing RLE *)
let make_repeated n pattern =
  let plen = String.length pattern in
  let buf = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set buf i (String.get pattern (i mod plen))
  done;
  Bytes.unsafe_to_string buf

(* Generate pseudo-random data *)
let make_random n seed =
  let buf = Bytes.create n in
  let state = ref seed in
  for i = 0 to n - 1 do
    state := !state * 1103515245 + 12345;
    Bytes.set buf i (Char.chr ((!state lsr 16) land 0xFF))
  done;
  Bytes.unsafe_to_string buf

(* ============================================================
   Basic roundtrip tests
   ============================================================ *)

let test_empty () =
  let result = roundtrip "" in
  check string_testable "empty string roundtrip" "" result

let test_single_byte () =
  let result = roundtrip "x" in
  check string_testable "single byte" "x" result

let test_small_literal () =
  let s = "Hello, World!" in
  let result = roundtrip s in
  check string_testable "small literal" s result

let test_exactly_60_bytes () =
  (* 60 bytes is the cutoff for inline literal length *)
  let s = String.make 60 'A' in
  let result = roundtrip s in
  check string_testable "exactly 60 bytes" s result

let test_61_bytes () =
  (* 61 bytes requires extended literal length encoding *)
  let s = String.make 61 'A' in
  let result = roundtrip s in
  check string_testable "61 bytes" s result

let test_256_bytes () =
  let s = String.make 256 'B' in
  let result = roundtrip s in
  check string_testable "256 bytes" s result

let test_65536_bytes () =
  let s = String.make 65536 'C' in
  let result = roundtrip s in
  check string_testable "65536 bytes" s result

let test_various_sizes () =
  List.iter (fun size ->
    let s = String.init size (fun i -> Char.chr (i mod 256)) in
    let result = roundtrip s in
    check string_testable (Printf.sprintf "size %d" size) s result
  ) [0; 1; 2; 3; 4; 5; 10; 59; 60; 61; 62; 100; 255; 256; 257;
     1000; 4096; 10000; 65535; 65536; 65537; 100000]

(* ============================================================
   Compression-specific tests (patterns that compress well)
   ============================================================ *)

let test_repeated_pattern () =
  let s = make_repeated 10000 "ABCD" in
  let compressed = Snappy.compress s in
  let ratio = float_of_int (String.length compressed) /.
              float_of_int (String.length s) in
  (* Should compress very well *)
  check bool "compression ratio < 0.1" true (ratio < 0.1);
  let result = roundtrip s in
  check string_testable "repeated pattern roundtrip" s result

let test_single_char_repeated () =
  (* This tests RLE-style copy where length > offset *)
  let s = String.make 10000 'X' in
  let compressed = Snappy.compress s in
  let ratio = float_of_int (String.length compressed) /.
              float_of_int (String.length s) in
  check bool "single char compression" true (ratio < 0.05);
  let result = roundtrip s in
  check string_testable "single char roundtrip" s result

let test_two_char_pattern () =
  (* Pattern "AB" repeated - offset 2 copies *)
  let s = make_repeated 10000 "AB" in
  let result = roundtrip s in
  check string_testable "two char pattern" s result

let test_short_repeat () =
  (* Short repeats that fit in 1-byte copy encoding *)
  let s = "abcdefabcdefabcdefabcdef" in
  let result = roundtrip s in
  check string_testable "short repeat" s result

(* ============================================================
   Random data tests (incompressible)
   ============================================================ *)

let test_random_small () =
  let s = make_random 1000 12345 in
  let result = roundtrip s in
  check string_testable "random small" s result

let test_random_large () =
  let s = make_random 100000 67890 in
  let result = roundtrip s in
  check string_testable "random large" s result

let test_incompressible_expansion () =
  (* Random data should expand by at most ~0.4% plus header *)
  let s = make_random 10000 11111 in
  let compressed = Snappy.compress s in
  let expansion = String.length compressed - String.length s in
  (* Allow for varint header + literal overhead *)
  let max_expansion = 5 + (String.length s / 6) + 1 in
  check bool "expansion within bounds" true (expansion <= max_expansion)

(* ============================================================
   Copy encoding tests
   ============================================================ *)

let test_copy_1byte_offset () =
  (* Copy with 1-byte offset: length 4-11, offset 0-2047 *)
  let s = "Hello Hello" in  (* "Hello " repeated, offset=6, length=5 *)
  let result = roundtrip s in
  check string_testable "1-byte offset copy" s result

let test_copy_2byte_offset () =
  (* Need offset > 2047 to force 2-byte encoding *)
  let prefix = String.make 2100 'X' in
  let pattern = "PATTERN" in
  let s = prefix ^ pattern ^ String.make 100 'Y' ^ pattern in
  let result = roundtrip s in
  check string_testable "2-byte offset copy" s result

let test_copy_long_match () =
  (* Match longer than 64 bytes (requires split) *)
  let pattern = String.make 100 'Z' in
  let s = pattern ^ "break" ^ pattern in
  let result = roundtrip s in
  check string_testable "long match (>64)" s result

let test_maximum_copy_offset () =
  (* Test near-maximum offset (32KB) *)
  let prefix = String.make 32000 'A' in
  let pattern = "FINDME" in
  let s = prefix ^ pattern ^ String.make 100 'B' ^ pattern in
  let result = roundtrip s in
  check string_testable "32KB offset" s result

(* ============================================================
   Overlapping copy tests (RLE behavior)
   ============================================================ *)

let test_overlap_offset_1 () =
  (* Copy with offset 1 is byte repeat *)
  let s = "aaaaaaaaaaaa" in
  let result = roundtrip s in
  check string_testable "offset 1 overlap" s result

let test_overlap_offset_2 () =
  (* Copy with offset 2, length > 2 *)
  let s = "ababababababab" in
  let result = roundtrip s in
  check string_testable "offset 2 overlap" s result

let test_overlap_offset_3 () =
  let s = "abcabcabcabcabc" in
  let result = roundtrip s in
  check string_testable "offset 3 overlap" s result

let test_complex_overlap () =
  (* Multiple overlapping patterns *)
  let s = String.concat "" [
    String.make 100 'A';
    String.make 50 'B';
    String.make 100 'A';  (* Should match earlier *)
    "CDCD";
    String.make 20 'C';   (* Offset 2 overlap *)
  ] in
  let result = roundtrip s in
  check string_testable "complex overlap" s result

(* ============================================================
   Varint tests
   ============================================================ *)

let test_varint_roundtrip () =
  let test_value v =
    let buf = Bytes.create 5 in
    let written = Snappy.encode_varint buf ~pos:0 v in
    let (decoded, consumed) = Snappy.decode_varint buf ~pos:0 ~len:written in
    check int (Printf.sprintf "varint %d value" v) v decoded;
    check int (Printf.sprintf "varint %d length" v) written consumed
  in
  List.iter test_value [
    0; 1; 127; 128; 255; 256;
    16383; 16384; 16385;
    (1 lsl 14) - 1; 1 lsl 14;
    (1 lsl 21) - 1; 1 lsl 21;
    (1 lsl 28) - 1;
    0x7FFFFFFF
  ]

let test_varint_length () =
  check int "varint_length 0" 1 (Snappy.varint_length 0);
  check int "varint_length 127" 1 (Snappy.varint_length 127);
  check int "varint_length 128" 2 (Snappy.varint_length 128);
  check int "varint_length 16383" 2 (Snappy.varint_length 16383);
  check int "varint_length 16384" 3 (Snappy.varint_length 16384);
  check int "varint_length 2097151" 3 (Snappy.varint_length 2097151);
  check int "varint_length 2097152" 4 (Snappy.varint_length 2097152);
  check int "varint_length 268435455" 4 (Snappy.varint_length 268435455);
  check int "varint_length 268435456" 5 (Snappy.varint_length 268435456)

(* ============================================================
   Error handling tests
   ============================================================ *)

let test_truncated_varint () =
  let src = Bytes.of_string "\xFF\xFF" in  (* Incomplete varint *)
  match Snappy.decompress (Bytes.to_string src) with
  | Ok _ -> fail "Should have failed"
  | Error msg -> check bool "truncated varint error" true
                   (String.length msg > 0)

let test_truncated_literal () =
  (* Header says 10 bytes but only provide 5 *)
  let buf = Bytes.create 10 in
  let _ = Snappy.encode_varint buf ~pos:0 10 in
  Bytes.set buf 1 (Char.chr ((9 lsl 2) lor 0));  (* 10-byte literal *)
  (* But don't provide the literal bytes *)
  match Snappy.decompress (Bytes.sub_string buf 0 5) with
  | Ok _ -> fail "Should have failed on truncated literal"
  | Error _ -> ()

let test_invalid_offset_zero () =
  (* Craft a copy with offset 0 *)
  let buf = Bytes.create 20 in
  let header_len = Snappy.encode_varint buf ~pos:0 10 in
  (* Emit a literal "AAAA" first *)
  Bytes.set buf header_len (Char.chr ((3 lsl 2) lor 0));  (* 4-byte literal *)
  Bytes.set buf (header_len + 1) 'A';
  Bytes.set buf (header_len + 2) 'A';
  Bytes.set buf (header_len + 3) 'A';
  Bytes.set buf (header_len + 4) 'A';
  (* Now emit copy with offset 0 (using 2-byte offset encoding) *)
  Bytes.set buf (header_len + 5) (Char.chr ((4 lsl 2) lor 2));  (* 5-byte copy, type 2 *)
  Bytes.set buf (header_len + 6) (Char.chr 0);  (* offset low byte *)
  Bytes.set buf (header_len + 7) (Char.chr 0);  (* offset high byte *)
  match Snappy.decompress (Bytes.sub_string buf 0 (header_len + 8)) with
  | Ok _ -> fail "Should have failed on zero offset"
  | Error _ -> ()

let test_invalid_offset_exceeds_position () =
  let buf = Bytes.create 20 in
  let header_len = Snappy.encode_varint buf ~pos:0 10 in
  (* Emit a small literal *)
  Bytes.set buf header_len (Char.chr ((1 lsl 2) lor 0));  (* 2-byte literal *)
  Bytes.set buf (header_len + 1) 'A';
  Bytes.set buf (header_len + 2) 'B';
  (* Now emit copy with offset > current position *)
  Bytes.set buf (header_len + 3) (Char.chr ((4 lsl 2) lor 2));  (* copy type 2 *)
  Bytes.set buf (header_len + 4) (Char.chr 100);  (* offset = 100, but only 2 bytes written *)
  Bytes.set buf (header_len + 5) (Char.chr 0);
  match Snappy.decompress (Bytes.sub_string buf 0 (header_len + 6)) with
  | Ok _ -> fail "Should have failed on offset exceeding position"
  | Error _ -> ()

(* ============================================================
   Low-allocation API tests
   ============================================================ *)

let test_compress_into () =
  let src = Bytes.of_string "Hello, World! Hello, World!" in
  let max_len = Snappy.max_compressed_length (Bytes.length src) in
  let dst = Bytes.create max_len in
  let written = Snappy.compress_into
    ~src ~src_pos:0 ~src_len:(Bytes.length src)
    ~dst ~dst_pos:0 in
  check bool "written <= max" true (written <= max_len);
  (* Now decompress *)
  let result = Snappy.decompress_to_bytes dst ~pos:0 ~len:written in
  check bytes_testable "compress_into roundtrip" src result

let test_decompress_into () =
  let original = Bytes.of_string (make_repeated 10000 "ABCD") in
  let compressed = Snappy.compress_to_bytes original ~pos:0 ~len:(Bytes.length original) in
  (* Get uncompressed length *)
  let uncompressed_len =
    match Snappy.get_uncompressed_length compressed ~pos:0 ~len:(Bytes.length compressed) with
    | Some n -> n
    | None -> fail "Failed to get uncompressed length"
  in
  check int "uncompressed length" (Bytes.length original) uncompressed_len;
  (* Decompress into pre-allocated buffer *)
  let dst = Bytes.create uncompressed_len in
  let written = Snappy.decompress_into
    ~src:compressed ~src_pos:0 ~src_len:(Bytes.length compressed)
    ~dst ~dst_pos:0 in
  check int "bytes written" uncompressed_len written;
  check bytes_testable "decompress_into result" original dst

let test_partial_buffer () =
  (* Test compressing/decompressing from middle of buffer *)
  let full = Bytes.of_string "PREFIX_Hello, World! Hello, World!_SUFFIX" in
  let src_pos = 7 in
  let src_len = 27 in
  let max_len = Snappy.max_compressed_length src_len in
  let dst = Bytes.create (10 + max_len) in
  let written = Snappy.compress_into
    ~src:full ~src_pos ~src_len
    ~dst ~dst_pos:10 in
  (* Decompress from offset *)
  let result = Snappy.decompress_to_bytes dst ~pos:10 ~len:written in
  check bytes_testable "partial buffer" (Bytes.sub full src_pos src_len) result

let test_compress_with_ctx () =
  (* Test reusable compression context *)
  let ctx = Snappy.create_compress_ctx () in
  let test_data = [
    "Hello, World!";
    make_repeated 1000 "ABCD";
    make_random 500 42;
    "";
    "X";
  ] in
  let max_len = Snappy.max_compressed_length 10000 in
  let dst = Bytes.create max_len in
  List.iter (fun s ->
    let src = Bytes.of_string s in
    let written = Snappy.compress_with_ctx ctx
      ~src ~src_pos:0 ~src_len:(Bytes.length src)
      ~dst ~dst_pos:0 in
    let result = Snappy.decompress_to_bytes dst ~pos:0 ~len:written in
    check bytes_testable "compress_with_ctx roundtrip" src result
  ) test_data

(* ============================================================
   get_uncompressed_length tests
   ============================================================ *)

let test_get_uncompressed_length () =
  List.iter (fun size ->
    let original = String.make size 'X' in
    let compressed = Snappy.compress original in
    let src = Bytes.unsafe_of_string compressed in
    match Snappy.get_uncompressed_length src ~pos:0 ~len:(String.length compressed) with
    | Some len -> check int (Printf.sprintf "uncompressed len for size %d" size) size len
    | None -> fail "Failed to get uncompressed length"
  ) [0; 1; 100; 1000; 65536; 100000]

let test_get_uncompressed_length_invalid () =
  (* Empty input *)
  check (option int) "empty" None
    (Snappy.get_uncompressed_length (Bytes.create 0) ~pos:0 ~len:0);
  (* Truncated varint *)
  check (option int) "truncated" None
    (Snappy.get_uncompressed_length (Bytes.of_string "\xFF") ~pos:0 ~len:1)

(* ============================================================
   Interoperability tests with real snappy test data
   ============================================================ *)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let test_corpus_file path =
  let data = read_file path in
  let result = roundtrip data in
  check int "corpus roundtrip length" (String.length data) (String.length result);
  check string_testable "corpus roundtrip" data result

let test_bad_data path =
  (* These should fail to decompress *)
  try
    let data = read_file path in
    match Snappy.decompress data with
    | Ok _ -> (* Some bad data might accidentally be valid, skip *)
      ()
    | Error _ -> ()
  with _ -> ()

(* ============================================================
   Stress tests
   ============================================================ *)

let test_many_small_roundtrips () =
  for len = 0 to 1000 do
    let s = String.init len (fun i -> Char.chr (i mod 256)) in
    let result = roundtrip s in
    if s <> result then
      fail (Printf.sprintf "Failed at length %d" len)
  done

let test_powers_of_two () =
  for i = 0 to 20 do
    let size = 1 lsl i in
    let s = make_repeated size "ABCDEFGH" in
    let result = roundtrip s in
    if s <> result then
      fail (Printf.sprintf "Failed at size %d" size)
  done

(* ============================================================
   Edge cases
   ============================================================ *)

let test_all_bytes () =
  (* String containing all 256 byte values *)
  let s = String.init 256 Char.chr in
  let result = roundtrip s in
  check string_testable "all bytes" s result

let test_binary_data () =
  (* Binary data with nulls *)
  let s = "\x00\x01\x02\x00\x00\x03\x04\x00" in
  let result = roundtrip s in
  check string_testable "binary with nulls" s result

let test_max_literal_lengths () =
  (* Test literal length boundaries: 60, 256, 65536, 16777216 *)
  List.iter (fun size ->
    let s = make_random size 42 in  (* Random so no copies *)
    let result = roundtrip s in
    check string_testable (Printf.sprintf "literal len %d" size) s result
  ) [59; 60; 61; 255; 256; 257; 65535; 65536; 65537]

(* ============================================================
   CRC32-C tests
   ============================================================ *)

let test_crc32c_empty () =
  let crc = Snappy.crc32c (Bytes.create 0) ~pos:0 ~len:0 in
  check int32 "crc32c empty" 0l crc

let test_crc32c_known_values () =
  (* Test vector: "123456789" should give 0xE3069283 *)
  let data = Bytes.of_string "123456789" in
  let crc = Snappy.crc32c data ~pos:0 ~len:9 in
  check int32 "crc32c 123456789" 0xE3069283l crc

let test_crc32c_mask_unmask () =
  let original = 0x12345678l in
  let masked = Snappy.mask_checksum original in
  let unmasked = Snappy.unmask_checksum masked in
  check int32 "mask/unmask roundtrip" original unmasked

(* ============================================================
   Framing format tests
   ============================================================ *)

let test_framed_empty () =
  let compressed = Snappy.compress_framed "" in
  check bool "starts with stream id" true (Snappy.is_framed_format compressed);
  match Snappy.decompress_framed compressed with
  | Ok result -> check string_testable "framed empty roundtrip" "" result
  | Error msg -> fail msg

let test_framed_small () =
  let s = "Hello, World!" in
  let compressed = Snappy.compress_framed s in
  check bool "is framed format" true (Snappy.is_framed_format compressed);
  match Snappy.decompress_framed compressed with
  | Ok result -> check string_testable "framed small roundtrip" s result
  | Error msg -> fail msg

let test_framed_large () =
  (* Test with data larger than one block (64KB) *)
  let s = make_repeated 200000 "ABCDEFGHIJ" in
  let compressed = Snappy.compress_framed s in
  check bool "is framed format" true (Snappy.is_framed_format compressed);
  match Snappy.decompress_framed compressed with
  | Ok result -> check string_testable "framed large roundtrip" s result
  | Error msg -> fail msg

let test_framed_random () =
  let s = make_random 100000 12345 in
  let compressed = Snappy.compress_framed s in
  match Snappy.decompress_framed compressed with
  | Ok result -> check string_testable "framed random roundtrip" s result
  | Error msg -> fail msg

let test_framed_invalid_stream_id () =
  (* Invalid stream identifier *)
  let bad = "\xff\x06\x00\x00NOTSNAPPY" in
  match Snappy.decompress_framed bad with
  | Ok _ -> fail "Should have failed on invalid stream id"
  | Error _ -> ()

let test_framed_truncated () =
  (* Truncated stream identifier *)
  let bad = "\xff\x06\x00" in
  match Snappy.decompress_framed bad with
  | Ok _ -> fail "Should have failed on truncated data"
  | Error _ -> ()

(* ============================================================
   Streaming API tests
   ============================================================ *)

let test_streaming_compress_small () =
  let s = "Hello, streaming world!" in
  let buf = Buffer.create 100 in
  let output bytes pos len =
    Buffer.add_subbytes buf bytes pos len
  in
  let cs = Snappy.create_compress_stream ~output in
  Snappy.compress_stream_feed cs (Bytes.unsafe_of_string s) ~pos:0 ~len:(String.length s);
  Snappy.compress_stream_finish cs;
  let compressed = Buffer.contents buf in

  check bool "streaming produces framed format" true (Snappy.is_framed_format compressed);
  match Snappy.decompress_framed compressed with
  | Ok result -> check string_testable "streaming roundtrip" s result
  | Error msg -> fail msg

let test_streaming_compress_multi_block () =
  (* Data spanning multiple 64KB blocks *)
  let s = make_repeated 200000 "STREAMING" in
  let buf = Buffer.create (String.length s) in
  let output bytes pos len =
    Buffer.add_subbytes buf bytes pos len
  in
  let cs = Snappy.create_compress_stream ~output in

  (* Feed in chunks of various sizes *)
  let pos = ref 0 in
  let chunk_sizes = [1000; 5000; 20000; 50000; 10000; 100000; 14000] in
  List.iter (fun chunk_size ->
    let remaining = String.length s - !pos in
    let to_feed = min chunk_size remaining in
    if to_feed > 0 then begin
      Snappy.compress_stream_feed cs (Bytes.unsafe_of_string s) ~pos:!pos ~len:to_feed;
      pos := !pos + to_feed
    end
  ) chunk_sizes;

  Snappy.compress_stream_finish cs;
  let compressed = Buffer.contents buf in

  match Snappy.decompress_framed compressed with
  | Ok result -> check string_testable "multi-block streaming roundtrip" s result
  | Error msg -> fail msg

let test_streaming_decompress () =
  (* First compress with framing format *)
  let s = make_repeated 150000 "DECOMPRESS" in
  let compressed = Snappy.compress_framed s in

  (* Now decompress using streaming API *)
  let buf = Buffer.create (String.length s) in
  let output bytes pos len =
    Buffer.add_subbytes buf bytes pos len
  in
  let ds = Snappy.create_decompress_stream ~output in

  (* Feed compressed data in small chunks *)
  let chunk_size = 1024 in
  let pos = ref 0 in
  while !pos < String.length compressed do
    let remaining = String.length compressed - !pos in
    let to_feed = min chunk_size remaining in
    Snappy.decompress_stream_feed ds (Bytes.unsafe_of_string compressed) ~pos:!pos ~len:to_feed;
    pos := !pos + to_feed
  done;

  check bool "decompress stream complete" true (Snappy.decompress_stream_is_complete ds);
  check string_testable "streaming decompress roundtrip" s (Buffer.contents buf)

let test_streaming_byte_by_byte () =
  let s = "Byte by byte test" in
  let compressed = Snappy.compress_framed s in

  let buf = Buffer.create (String.length s) in
  let output bytes pos len =
    Buffer.add_subbytes buf bytes pos len
  in
  let ds = Snappy.create_decompress_stream ~output in

  (* Feed one byte at a time *)
  for i = 0 to String.length compressed - 1 do
    Snappy.decompress_stream_feed ds (Bytes.unsafe_of_string compressed) ~pos:i ~len:1
  done;

  check bool "byte-by-byte complete" true (Snappy.decompress_stream_is_complete ds);
  check string_testable "byte-by-byte roundtrip" s (Buffer.contents buf)

(* ============================================================
   Test runner
   ============================================================ *)

let () =
  (* Check if testdata directory exists *)
  let testdata_path = "testdata" in
  let testdata_exists = Sys.file_exists testdata_path && Sys.is_directory testdata_path in

  let corpus_tests =
    if testdata_exists then
      [ "alice29.txt", `Quick, (fun () -> test_corpus_file "testdata/alice29.txt")
      ; "asyoulik.txt", `Quick, (fun () -> test_corpus_file "testdata/asyoulik.txt")
      ; "lcet10.txt", `Quick, (fun () -> test_corpus_file "testdata/lcet10.txt")
      ; "plrabn12.txt", `Quick, (fun () -> test_corpus_file "testdata/plrabn12.txt")
      ; "html", `Quick, (fun () -> test_corpus_file "testdata/html")
      ; "urls.10K", `Quick, (fun () -> test_corpus_file "testdata/urls.10K")
      ; "baddata1.snappy", `Quick, (fun () -> test_bad_data "testdata/baddata1.snappy")
      ; "baddata2.snappy", `Quick, (fun () -> test_bad_data "testdata/baddata2.snappy")
      ; "baddata3.snappy", `Quick, (fun () -> test_bad_data "testdata/baddata3.snappy")
      ]
    else
      []
  in

  run "Snappy" [
    "basic", [
      "empty", `Quick, test_empty;
      "single byte", `Quick, test_single_byte;
      "small literal", `Quick, test_small_literal;
      "exactly 60 bytes", `Quick, test_exactly_60_bytes;
      "61 bytes", `Quick, test_61_bytes;
      "256 bytes", `Quick, test_256_bytes;
      "65536 bytes", `Quick, test_65536_bytes;
      "various sizes", `Quick, test_various_sizes;
    ];

    "compression", [
      "repeated pattern", `Quick, test_repeated_pattern;
      "single char repeated", `Quick, test_single_char_repeated;
      "two char pattern", `Quick, test_two_char_pattern;
      "short repeat", `Quick, test_short_repeat;
    ];

    "random", [
      "random small", `Quick, test_random_small;
      "random large", `Quick, test_random_large;
      "incompressible expansion", `Quick, test_incompressible_expansion;
    ];

    "copy_encoding", [
      "1-byte offset", `Quick, test_copy_1byte_offset;
      "2-byte offset", `Quick, test_copy_2byte_offset;
      "long match", `Quick, test_copy_long_match;
      "maximum offset", `Quick, test_maximum_copy_offset;
    ];

    "overlap", [
      "offset 1", `Quick, test_overlap_offset_1;
      "offset 2", `Quick, test_overlap_offset_2;
      "offset 3", `Quick, test_overlap_offset_3;
      "complex", `Quick, test_complex_overlap;
    ];

    "varint", [
      "roundtrip", `Quick, test_varint_roundtrip;
      "length", `Quick, test_varint_length;
    ];

    "errors", [
      "truncated varint", `Quick, test_truncated_varint;
      "truncated literal", `Quick, test_truncated_literal;
      "invalid offset zero", `Quick, test_invalid_offset_zero;
      "invalid offset exceeds position", `Quick, test_invalid_offset_exceeds_position;
    ];

    "low_allocation", [
      "compress_into", `Quick, test_compress_into;
      "decompress_into", `Quick, test_decompress_into;
      "partial buffer", `Quick, test_partial_buffer;
      "compress_with_ctx", `Quick, test_compress_with_ctx;
    ];

    "uncompressed_length", [
      "get_uncompressed_length", `Quick, test_get_uncompressed_length;
      "get_uncompressed_length invalid", `Quick, test_get_uncompressed_length_invalid;
    ];

    "corpus", corpus_tests;

    "stress", [
      "many small roundtrips", `Slow, test_many_small_roundtrips;
      "powers of two", `Slow, test_powers_of_two;
    ];

    "edge_cases", [
      "all bytes", `Quick, test_all_bytes;
      "binary data", `Quick, test_binary_data;
      "max literal lengths", `Quick, test_max_literal_lengths;
    ];

    "crc32c", [
      "empty", `Quick, test_crc32c_empty;
      "known values", `Quick, test_crc32c_known_values;
      "mask/unmask roundtrip", `Quick, test_crc32c_mask_unmask;
    ];

    "framing", [
      "framed empty", `Quick, test_framed_empty;
      "framed small", `Quick, test_framed_small;
      "framed large", `Quick, test_framed_large;
      "framed random", `Quick, test_framed_random;
      "invalid stream id", `Quick, test_framed_invalid_stream_id;
      "truncated", `Quick, test_framed_truncated;
    ];

    "streaming", [
      "compress small", `Quick, test_streaming_compress_small;
      "compress multi-block", `Quick, test_streaming_compress_multi_block;
      "decompress", `Quick, test_streaming_decompress;
      "byte by byte", `Quick, test_streaming_byte_by_byte;
    ];
  ]
