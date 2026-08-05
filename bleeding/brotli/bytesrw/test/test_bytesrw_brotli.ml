(* Tests for bytesrw_brotli *)

open Bytesrw

let test_compress_reads_empty () =
  let r = Bytes.Reader.of_string "" in
  let cr = Bytesrw_brotli.compress_reads () r in
  let result = Bytes.Reader.to_string cr in
  (* Compressed empty input should still produce some output (header) *)
  Alcotest.(check bool) "non-empty output" true (String.length result > 0);
  (* Decompress to verify *)
  let s = Brotli.decompress result in
  Alcotest.(check string) "roundtrip" "" s

let test_compress_reads_simple () =
  let input = "Hello, World!" in
  let r = Bytes.Reader.of_string input in
  let cr = Bytesrw_brotli.compress_reads () r in
  let compressed = Bytes.Reader.to_string cr in
  let s = Brotli.decompress compressed in
  Alcotest.(check string) "roundtrip" input s

let test_decompress_reads_simple () =
  let input = "Hello, World!" in
  let compressed = Brotli.compress input in
  let r = Bytes.Reader.of_string compressed in
  let dr = Bytesrw_brotli.decompress_reads () r in
  let result = Bytes.Reader.to_string dr in
  Alcotest.(check string) "decompress" input result

let test_roundtrip_reads () =
  let input = String.make 1000 'X' ^ String.init 1000 (fun i -> Char.chr (i mod 256)) in
  let r = Bytes.Reader.of_string input in
  let cr = Bytesrw_brotli.compress_reads () r in
  let dr = Bytesrw_brotli.decompress_reads () cr in
  let result = Bytes.Reader.to_string dr in
  Alcotest.(check int) "length" (String.length input) (String.length result);
  Alcotest.(check string) "content" input result

let test_compress_writes_simple () =
  let input = "Hello, World!" in
  let b = Buffer.create 256 in
  let w = Bytes.Writer.of_buffer b in
  let cw = Bytesrw_brotli.compress_writes () ~eod:true w in
  Bytes.Writer.write_string cw input;
  Bytes.Writer.write_eod cw;
  let compressed = Buffer.contents b in
  let s = Brotli.decompress compressed in
  Alcotest.(check string) "roundtrip" input s

let test_decompress_writes_simple () =
  let input = "Hello, World!" in
  let compressed = Brotli.compress input in
  let b = Buffer.create 256 in
  let w = Bytes.Writer.of_buffer b in
  let dw = Bytesrw_brotli.decompress_writes () ~eod:true w in
  Bytes.Writer.write_string dw compressed;
  Bytes.Writer.write_eod dw;
  let result = Buffer.contents b in
  Alcotest.(check string) "decompress" input result

let test_roundtrip_writes () =
  let input = String.make 1000 'Y' ^ String.init 1000 (fun i -> Char.chr (i mod 256)) in

  (* Compress *)
  let b1 = Buffer.create 256 in
  let w1 = Bytes.Writer.of_buffer b1 in
  let cw = Bytesrw_brotli.compress_writes () ~eod:true w1 in
  Bytes.Writer.write_string cw input;
  Bytes.Writer.write_eod cw;
  let compressed = Buffer.contents b1 in

  (* Decompress *)
  let b2 = Buffer.create 256 in
  let w2 = Bytes.Writer.of_buffer b2 in
  let dw = Bytesrw_brotli.decompress_writes () ~eod:true w2 in
  Bytes.Writer.write_string dw compressed;
  Bytes.Writer.write_eod dw;
  let result = Buffer.contents b2 in

  Alcotest.(check int) "length" (String.length input) (String.length result);
  Alcotest.(check string) "content" input result

let test_slice_length () =
  (* Test with different slice lengths *)
  let input = String.init 10000 (fun i -> Char.chr (i mod 256)) in
  let slice_lengths = [64; 256; 1024; 8192] in
  List.iter (fun slice_length ->
    let r = Bytes.Reader.of_string input in
    let cr = Bytesrw_brotli.compress_reads ~slice_length () r in
    let dr = Bytesrw_brotli.decompress_reads ~slice_length () cr in
    let result = Bytes.Reader.to_string dr in
    Alcotest.(check int) (Printf.sprintf "length@%d" slice_length)
      (String.length input) (String.length result);
    Alcotest.(check string) (Printf.sprintf "content@%d" slice_length) input result
  ) slice_lengths

let test_quality_levels () =
  (* Test different quality levels *)
  let input = String.init 1000 (fun i -> Char.chr (i mod 256)) in
  List.iter (fun quality ->
    let r = Bytes.Reader.of_string input in
    let cr = Bytesrw_brotli.compress_reads ~quality () r in
    let dr = Bytesrw_brotli.decompress_reads () cr in
    let result = Bytes.Reader.to_string dr in
    Alcotest.(check string) (Printf.sprintf "quality %d" (Brotli.quality_to_int quality)) input result
  ) [Brotli.Q1; Brotli.Q2; Brotli.Q3]

(* Brotli-C compatibility tests *)

let testdata_dir = "../../vendor/git/brotli-c/tests/testdata"

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let file_exists path =
  try ignore (Unix.stat path); true
  with Unix.Unix_error _ -> false

let test_brotli_c_decompress () =
  (* Test decompressing official brotli-c test vectors *)
  let test_cases = [
    "empty";
    "10x10y";
    "64x";
    "backward65536";
  ] in
  List.iter (fun name ->
    let original_path = Filename.concat testdata_dir name in
    let compressed_path = Filename.concat testdata_dir (name ^ ".compressed") in
    if file_exists original_path && file_exists compressed_path then begin
      let original = read_file original_path in
      let compressed = read_file compressed_path in
      let r = Bytes.Reader.of_string compressed in
      let dr = Bytesrw_brotli.decompress_reads () r in
      let result = Bytes.Reader.to_string dr in
      Alcotest.(check int) (name ^ " length")
        (String.length original) (String.length result);
      Alcotest.(check string) (name ^ " content") original result
    end
  ) test_cases

let test_brotli_c_roundtrip () =
  (* Test that our compression produces valid output that brotli-c test files
     can be compared against *)
  let test_cases = [
    "10x10y";
    "64x";
  ] in
  List.iter (fun name ->
    let original_path = Filename.concat testdata_dir name in
    if file_exists original_path then begin
      let original = read_file original_path in
      (* Compress with our encoder *)
      let r = Bytes.Reader.of_string original in
      let cr = Bytesrw_brotli.compress_reads () r in
      let compressed = Bytes.Reader.to_string cr in
      (* Decompress with our decoder *)
      let dr = Bytesrw_brotli.decompress_reads ()
                 (Bytes.Reader.of_string compressed) in
      let result = Bytes.Reader.to_string dr in
      Alcotest.(check string) (name ^ " roundtrip") original result
    end
  ) test_cases

let test_brotli_c_text_files () =
  (* Test with larger text files from brotli-c test suite *)
  let test_cases = [
    "alice29.txt";
    "asyoulik.txt";
  ] in
  List.iter (fun name ->
    let original_path = Filename.concat testdata_dir name in
    let compressed_path = Filename.concat testdata_dir (name ^ ".compressed") in
    if file_exists original_path && file_exists compressed_path then begin
      let original = read_file original_path in
      let compressed = read_file compressed_path in
      (* Test decompressing official brotli-c output *)
      let r = Bytes.Reader.of_string compressed in
      let dr = Bytesrw_brotli.decompress_reads () r in
      let result = Bytes.Reader.to_string dr in
      Alcotest.(check int) (name ^ " length")
        (String.length original) (String.length result);
      Alcotest.(check string) (name ^ " content") original result
    end
  ) test_cases

let () =
  Alcotest.run "bytesrw_brotli" [
    "compress_reads", [
      Alcotest.test_case "empty" `Quick test_compress_reads_empty;
      Alcotest.test_case "simple" `Quick test_compress_reads_simple;
    ];
    "decompress_reads", [
      Alcotest.test_case "simple" `Quick test_decompress_reads_simple;
    ];
    "roundtrip_reads", [
      Alcotest.test_case "large" `Quick test_roundtrip_reads;
    ];
    "compress_writes", [
      Alcotest.test_case "simple" `Quick test_compress_writes_simple;
    ];
    "decompress_writes", [
      Alcotest.test_case "simple" `Quick test_decompress_writes_simple;
    ];
    "roundtrip_writes", [
      Alcotest.test_case "large" `Quick test_roundtrip_writes;
    ];
    "parameters", [
      Alcotest.test_case "slice_length" `Quick test_slice_length;
      Alcotest.test_case "quality_levels" `Quick test_quality_levels;
    ];
    "brotli_c_compat", [
      Alcotest.test_case "decompress_test_vectors" `Quick test_brotli_c_decompress;
      Alcotest.test_case "roundtrip_test_vectors" `Quick test_brotli_c_roundtrip;
      Alcotest.test_case "text_files" `Quick test_brotli_c_text_files;
    ];
  ]
