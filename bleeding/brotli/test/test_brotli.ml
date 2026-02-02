(* Brotli compression/decompression tests *)

let test_empty () =
  let compressed = Brotli.compress "" in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "empty roundtrip" "" result
  | Error msg -> Alcotest.fail msg

let test_small_literal () =
  let input = "Hello, World!" in
  let compressed = Brotli.compress input in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "small literal roundtrip" input result
  | Error msg -> Alcotest.fail msg

let test_repeated_pattern () =
  let input = String.make 1000 'A' in
  let compressed = Brotli.compress input in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "repeated pattern roundtrip" input result
  | Error msg -> Alcotest.fail msg

let test_binary_data () =
  let input = String.init 256 (fun i -> Char.chr i) in
  let compressed = Brotli.compress input in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "binary data roundtrip" input result
  | Error msg -> Alcotest.fail msg

let test_medium_text () =
  let words = ["The"; "quick"; "brown"; "fox"; "jumps"; "over"; "the"; "lazy"; "dog"] in
  let input = String.concat " " (List.init 100 (fun i -> List.nth words (i mod 9))) in
  let compressed = Brotli.compress input in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "medium text roundtrip" input result
  | Error msg -> Alcotest.fail msg

let test_various_sizes () =
  List.iter (fun size ->
    let input = String.init size (fun i -> Char.chr (i mod 256)) in
    let compressed = Brotli.compress input in
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check int) (Printf.sprintf "size %d length" size) size (String.length result);
      Alcotest.(check string) (Printf.sprintf "size %d content" size) input result
    | Error msg -> Alcotest.fail (Printf.sprintf "size %d: %s" size msg)
  ) [0; 1; 10; 100; 1000; 10000]

let test_max_compressed_length () =
  let size = 1000 in
  let max_len = Brotli.max_compressed_length size in
  Alcotest.(check bool) "max_compressed_length > input"
    true (max_len >= size)

(* Quality level tests *)

let test_quality_0 () =
  (* Quality 0 should use uncompressed blocks *)
  let inputs = [
    "Hello, World!";
    String.make 100 'A';
    String.init 256 (fun i -> Char.chr i);
  ] in
  List.iter (fun input ->
    let compressed = Brotli.compress ~quality:0 input in
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check string) "quality 0 roundtrip" input result
    | Error msg -> Alcotest.fail msg
  ) inputs

let test_quality_1 () =
  (* Quality 1 uses Huffman-only compression *)
  let inputs = [
    "";
    "a";
    "Hello, World!";
    String.make 100 'A';
    String.make 1000 'B';
    String.init 256 (fun i -> Char.chr i);
    String.concat " " (List.init 50 (fun _ -> "Hello, World!"));
  ] in
  List.iter (fun input ->
    let compressed = Brotli.compress ~quality:1 input in
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check string) "quality 1 roundtrip" input result
    | Error msg -> Alcotest.fail (Printf.sprintf "quality 1 failed on %d bytes: %s" (String.length input) msg)
  ) inputs

let test_quality_2 () =
  (* Quality 2 uses LZ77 + Huffman *)
  let inputs = [
    "";
    "a";
    "Hello, World!";
    String.make 100 'A';
    String.make 1000 'B';
    String.init 256 (fun i -> Char.chr i);
    String.concat " " (List.init 50 (fun _ -> "Hello, World!"));
    String.concat " " (List.init 100 (fun _ -> "Hello, World!"));
    String.concat "" (List.init 100 (fun _ -> "abcdefghij"));
  ] in
  List.iter (fun input ->
    let compressed = Brotli.compress ~quality:2 input in
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check string) "quality 2 roundtrip" input result
    | Error msg -> Alcotest.fail (Printf.sprintf "quality 2 failed on %d bytes: %s" (String.length input) msg)
  ) inputs

let test_quality_3 () =
  (* Quality 3 uses LZ77 + Dictionary + Huffman *)
  let inputs = [
    "";
    "a";
    "Hello, World!";
    String.make 100 'A';
    String.make 1000 'B';
    String.init 256 (fun i -> Char.chr i);
    String.concat " " (List.init 50 (fun _ -> "Hello, World!"));
    String.concat " " (List.init 100 (fun _ -> "Hello, World!"));
    String.concat "" (List.init 100 (fun _ -> "abcdefghij"));
    (* Text with common English words that might be in dictionary *)
    "The quick brown fox jumps over the lazy dog.";
    "This is a test of the Brotli compression algorithm.";
    String.concat " " (List.init 50 (fun _ -> "the quick brown fox"));
  ] in
  List.iter (fun input ->
    let compressed = Brotli.compress ~quality:3 input in
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check string) "quality 3 roundtrip" input result
    | Error msg -> Alcotest.fail (Printf.sprintf "quality 3 failed on %d bytes: %s" (String.length input) msg)
  ) inputs

let test_quality_4 () =
  (* Quality 4 uses hash chains + lazy matching *)
  let inputs = [
    "";
    "a";
    "Hello, World!";
    String.make 100 'A';
    String.make 1000 'B';
    String.init 256 (fun i -> Char.chr i);
    String.concat " " (List.init 50 (fun _ -> "Hello, World!"));
    String.concat " " (List.init 100 (fun _ -> "Hello, World!"));
    String.concat "" (List.init 100 (fun _ -> "abcdefghij"));
    (* More complex patterns that benefit from lazy matching *)
    String.concat "" (List.init 200 (fun i ->
      if i mod 10 < 5 then "abc" else "xyz"));
    String.concat "" (List.init 100 (fun _ -> "ababababab"));
  ] in
  List.iter (fun input ->
    let compressed = Brotli.compress ~quality:4 input in
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check string) "quality 4 roundtrip" input result
    | Error msg -> Alcotest.fail (Printf.sprintf "quality 4 failed on %d bytes: %s" (String.length input) msg)
  ) inputs

let test_quality_5 () =
  (* Quality 5 uses deeper hash chains *)
  let inputs = [
    "";
    "a";
    "Hello, World!";
    String.make 100 'A';
    String.make 1000 'B';
    String.init 256 (fun i -> Char.chr i);
    String.concat " " (List.init 50 (fun _ -> "Hello, World!"));
    String.concat " " (List.init 100 (fun _ -> "Hello, World!"));
    (* Large input to test performance *)
    String.init 10000 (fun i ->
      let v = (i * 7 + 13) mod 256 in
      Char.chr v);
  ] in
  List.iter (fun input ->
    let compressed = Brotli.compress ~quality:5 input in
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check string) "quality 5 roundtrip" input result
    | Error msg -> Alcotest.fail (Printf.sprintf "quality 5 failed on %d bytes: %s" (String.length input) msg)
  ) inputs

let test_quality_6_9 () =
  (* Quality 6-9 use deeper matching - testing they work correctly *)
  let inputs = [
    "";
    "Hello, World!";
    String.make 100 'A';
    String.make 1000 'B';
    String.concat " " (List.init 100 (fun _ -> "Hello, World!"));
    (* Text with various patterns *)
    "The quick brown fox jumps over the lazy dog. " ^
    "Pack my box with five dozen liquor jugs. " ^
    "How vexingly quick daft zebras jump!";
    (* Binary-like data *)
    String.init 1000 (fun i -> Char.chr (i mod 256));
  ] in
  List.iter (fun q ->
    List.iter (fun input ->
      let compressed = Brotli.compress ~quality:q input in
      match Brotli.decompress compressed with
      | Ok result ->
        Alcotest.(check string) (Printf.sprintf "quality %d roundtrip" q) input result
      | Error msg ->
        Alcotest.fail (Printf.sprintf "quality %d failed on %d bytes: %s" q (String.length input) msg)
    ) inputs
  ) [6; 7; 8; 9]

let test_quality_10_11 () =
  (* Quality 10-11 use maximum compression *)
  let inputs = [
    "";
    "Hello, World!";
    String.make 100 'A';
    String.concat " " (List.init 50 (fun _ -> "Hello, World!"));
    (* Text with various patterns *)
    "The quick brown fox jumps over the lazy dog. " ^
    "Pack my box with five dozen liquor jugs.";
  ] in
  List.iter (fun q ->
    List.iter (fun input ->
      let compressed = Brotli.compress ~quality:q input in
      match Brotli.decompress compressed with
      | Ok result ->
        Alcotest.(check string) (Printf.sprintf "quality %d roundtrip" q) input result
      | Error msg ->
        Alcotest.fail (Printf.sprintf "quality %d failed on %d bytes: %s" q (String.length input) msg)
    ) inputs
  ) [10; 11]

let test_quality_comparison () =
  (* Quality 2 should generally compress better than quality 1 for repetitive data *)
  let input = String.concat " " (List.init 100 (fun _ -> "Hello, World!")) in
  let c1 = Brotli.compress ~quality:1 input in
  let c2 = Brotli.compress ~quality:2 input in
  (* Both should decompress correctly *)
  (match Brotli.decompress c1 with
   | Ok r -> Alcotest.(check string) "q1 decompresses" input r
   | Error msg -> Alcotest.fail msg);
  (match Brotli.decompress c2 with
   | Ok r -> Alcotest.(check string) "q2 decompresses" input r
   | Error msg -> Alcotest.fail msg);
  (* Quality 2 should be smaller for this repetitive input *)
  Alcotest.(check bool) "quality 2 compresses better"
    true (String.length c2 < String.length c1)

let test_all_qualities_large () =
  (* Test all qualities with larger inputs *)
  let sizes = [100; 500; 1000; 5000; 10000] in
  let qualities = [0; 1; 2] in
  List.iter (fun size ->
    let input = String.init size (fun i ->
      (* Mix of patterns to test various encoding paths *)
      if i mod 100 < 50 then Char.chr (i mod 26 + 65)  (* A-Z *)
      else Char.chr (i mod 10 + 48)  (* 0-9 *)
    ) in
    List.iter (fun q ->
      let compressed = Brotli.compress ~quality:q input in
      match Brotli.decompress compressed with
      | Ok result ->
        Alcotest.(check int) (Printf.sprintf "q%d size %d" q size) size (String.length result);
        Alcotest.(check string) (Printf.sprintf "q%d size %d content" q size) input result
      | Error msg ->
        Alcotest.fail (Printf.sprintf "q%d size %d: %s" q size msg)
    ) qualities
  ) sizes

let test_compress_into () =
  (* Test the low-allocation API *)
  let input = "Hello, World! This is a test of the compress_into API." in
  let src = Bytes.of_string input in
  let max_len = Brotli.max_compressed_length (String.length input) in
  let dst = Bytes.create max_len in
  let compressed_len = Brotli.compress_into
    ~src ~src_pos:0 ~src_len:(String.length input)
    ~dst ~dst_pos:0 () in
  let compressed = Bytes.sub_string dst 0 compressed_len in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "compress_into roundtrip" input result
  | Error msg -> Alcotest.fail msg

let test_compress_into_quality () =
  (* Test compress_into with different qualities *)
  let input = String.concat "" (List.init 100 (fun _ -> "test pattern ")) in
  let src = Bytes.of_string input in
  let max_len = Brotli.max_compressed_length (String.length input) in
  List.iter (fun q ->
    let dst = Bytes.create max_len in
    let compressed_len = Brotli.compress_into ~quality:q
      ~src ~src_pos:0 ~src_len:(String.length input)
      ~dst ~dst_pos:0 () in
    let compressed = Bytes.sub_string dst 0 compressed_len in
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check string) (Printf.sprintf "compress_into q%d" q) input result
    | Error msg -> Alcotest.fail (Printf.sprintf "compress_into q%d: %s" q msg)
  ) [0; 1; 2]

let test_partial_buffer () =
  (* Test compressing from middle of buffer *)
  let full = "prefix_Hello, World!_suffix" in
  let src = Bytes.of_string full in
  let max_len = Brotli.max_compressed_length 13 in
  let dst = Bytes.create max_len in
  let compressed_len = Brotli.compress_into
    ~src ~src_pos:7 ~src_len:13  (* "Hello, World!" *)
    ~dst ~dst_pos:0 () in
  let compressed = Bytes.sub_string dst 0 compressed_len in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "partial buffer" "Hello, World!" result
  | Error msg -> Alcotest.fail msg

let test_edge_cases () =
  (* Various edge cases *)
  let cases = [
    (* Single bytes *)
    "\x00";
    "\xff";
    (* All same byte *)
    String.make 10 '\x00';
    String.make 10 '\xff';
    (* Alternating *)
    String.init 100 (fun i -> if i mod 2 = 0 then 'a' else 'b');
    (* All printable ASCII *)
    String.init 95 (fun i -> Char.chr (i + 32));
    (* Long runs followed by varied data *)
    String.make 500 'X' ^ String.init 500 (fun i -> Char.chr (i mod 256));
  ] in
  List.iteri (fun i input ->
    List.iter (fun q ->
      let compressed = Brotli.compress ~quality:q input in
      match Brotli.decompress compressed with
      | Ok result ->
        Alcotest.(check string) (Printf.sprintf "edge case %d q%d" i q) input result
      | Error msg ->
        Alcotest.fail (Printf.sprintf "edge case %d q%d: %s" i q msg)
    ) [0; 1; 2]
  ) cases

let roundtrip_tests = [
  "empty", `Quick, test_empty;
  "small_literal", `Quick, test_small_literal;
  "repeated_pattern", `Quick, test_repeated_pattern;
  "binary_data", `Quick, test_binary_data;
  "medium_text", `Quick, test_medium_text;
  "various_sizes", `Quick, test_various_sizes;
  "max_compressed_length", `Quick, test_max_compressed_length;
]

let quality_tests = [
  "quality_0", `Quick, test_quality_0;
  "quality_1", `Quick, test_quality_1;
  "quality_2", `Quick, test_quality_2;
  "quality_3", `Quick, test_quality_3;
  "quality_4", `Quick, test_quality_4;
  "quality_5", `Quick, test_quality_5;
  "quality_6_9", `Quick, test_quality_6_9;
  "quality_10_11", `Quick, test_quality_10_11;
  "quality_comparison", `Quick, test_quality_comparison;
  "all_qualities_large", `Quick, test_all_qualities_large;
]

let api_tests = [
  "compress_into", `Quick, test_compress_into;
  "compress_into_quality", `Quick, test_compress_into_quality;
  "partial_buffer", `Quick, test_partial_buffer;
  "edge_cases", `Quick, test_edge_cases;
]

(* Streaming API tests *)

let test_streaming_single_chunk () =
  let input = "Hello, World! This is a test of the streaming API." in
  let src = Bytes.of_string input in
  let max_len = Brotli.max_compressed_length (String.length input) in
  let dst = Bytes.create max_len in
  let encoder = Brotli.create_streaming_encoder ~dst ~dst_pos:0 () in
  let _ = Brotli.streaming_write encoder ~src ~src_pos:0 ~src_len:(String.length input) ~is_last:true in
  let compressed_len = Brotli.streaming_bytes_written encoder in
  let compressed = Bytes.sub_string dst 0 compressed_len in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "streaming single chunk" input result
  | Error msg -> Alcotest.fail msg

let test_streaming_multiple_chunks () =
  let chunks = [| "First chunk. "; "Second chunk. "; "Third chunk. "; "Fourth and final chunk!" |] in
  let input = String.concat "" (Array.to_list chunks) in
  let max_len = Brotli.max_compressed_length (String.length input) * 2 in
  let dst = Bytes.create max_len in
  let encoder = Brotli.create_streaming_encoder ~quality:2 ~dst ~dst_pos:0 () in
  for i = 0 to Array.length chunks - 1 do
    let chunk = chunks.(i) in
    let src = Bytes.of_string chunk in
    let is_last = i = Array.length chunks - 1 in
    let _ = Brotli.streaming_write encoder ~src ~src_pos:0 ~src_len:(String.length chunk) ~is_last in
    ()
  done;
  let compressed_len = Brotli.streaming_bytes_written encoder in
  let compressed = Bytes.sub_string dst 0 compressed_len in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "streaming multiple chunks" input result
  | Error msg -> Alcotest.fail msg

let test_streaming_empty () =
  let max_len = Brotli.max_compressed_length 0 + 16 in
  let dst = Bytes.create max_len in
  let encoder = Brotli.create_streaming_encoder ~dst ~dst_pos:0 () in
  let _ = Brotli.streaming_finish encoder in
  let compressed_len = Brotli.streaming_bytes_written encoder in
  let compressed = Bytes.sub_string dst 0 compressed_len in
  match Brotli.decompress compressed with
  | Ok result -> Alcotest.(check string) "streaming empty" "" result
  | Error msg -> Alcotest.fail msg

let streaming_tests = [
  "single_chunk", `Quick, test_streaming_single_chunk;
  "multiple_chunks", `Quick, test_streaming_multiple_chunks;
  "empty", `Quick, test_streaming_empty;
]

(* Test that higher quality levels produce better or equal compression *)
let test_quality_improves_compression () =
  (* Create compressible test data *)
  let test_data = String.concat "" (List.init 100 (fun i ->
    Printf.sprintf "Hello World %d! This is a test of Brotli compression quality.\n" i
  )) in
  let sizes = Array.make 12 0 in
  for q = 0 to 11 do
    let compressed = Brotli.compress ~quality:q test_data in
    sizes.(q) <- String.length compressed;
    (* Verify roundtrip *)
    match Brotli.decompress compressed with
    | Ok result ->
      Alcotest.(check string) (Printf.sprintf "quality %d roundtrip" q) test_data result
    | Error msg ->
      Alcotest.fail (Printf.sprintf "quality %d failed: %s" q msg)
  done;
  (* Verify quality 11 is better than quality 0 *)
  Alcotest.(check bool) "quality 11 <= quality 0"
    true (sizes.(11) <= sizes.(0));
  (* Verify quality 4+ is better than quality 1 (with LZ77) *)
  Alcotest.(check bool) "quality 4 <= quality 1"
    true (sizes.(4) <= sizes.(1))

let compression_tests = [
  "quality_improves_compression", `Quick, test_quality_improves_compression;
]

let () =
  Alcotest.run "Brotli" [
    "roundtrip", roundtrip_tests;
    "quality", quality_tests;
    "api", api_tests;
    "streaming", streaming_tests;
    "compression", compression_tests;
  ]
