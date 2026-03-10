(** Tests for codecs *)

open Alcotest
open Zarr

(* Helper to unwrap decode result *)
let decode_ok chain shape dtype bytes =
  match Codec.decode chain shape dtype bytes with
  | Ok arr -> arr
  | Error (`Codec_error msg) -> Alcotest.fail ("decode error: " ^ msg)
  | Error `Checksum_mismatch -> Alcotest.fail "decode checksum mismatch"

(* === Bytes codec tests === *)

let test_bytes_little_endian_int32 () =
  let arr = Chunk_data.create_zero Dtype.Int32 [|3|] in
  Chunk_data.set arr [|0|] (`Int32 1l);
  Chunk_data.set arr [|1|] (`Int32 2l);
  Chunk_data.set arr [|2|] (`Int32 256l);

  let codec = Codec_bytes.create Dtype.Little in
  let encoded = codec.encode arr in

  check bytes "encoded bytes"
    (Bytes.of_string "\x01\x00\x00\x00\x02\x00\x00\x00\x00\x01\x00\x00")
    encoded

let test_bytes_big_endian_int32 () =
  let arr = Chunk_data.create_zero Dtype.Int32 [|3|] in
  Chunk_data.set arr [|0|] (`Int32 1l);
  Chunk_data.set arr [|1|] (`Int32 2l);
  Chunk_data.set arr [|2|] (`Int32 256l);

  let codec = Codec_bytes.create Dtype.Big in
  let encoded = codec.encode arr in

  check bytes "encoded bytes"
    (Bytes.of_string "\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x01\x00")
    encoded

let test_bytes_roundtrip () =
  let arr = Chunk_data.create_zero Dtype.Float64 [|5|] in
  for i = 0 to 4 do
    Chunk_data.set arr [|i|] (`Float (Float.of_int i *. 1.5))
  done;

  let codec = Codec_bytes.create Dtype.Little in
  let encoded = codec.encode arr in
  let decoded = codec.decode [|5|] Dtype.Float64 encoded in

  for i = 0 to 4 do
    match Chunk_data.get decoded [|i|] with
    | `Float f -> check (float 0.001) (Printf.sprintf "element %d" i) (Float.of_int i *. 1.5) f
    | _ -> fail "expected float"
  done

(* === CRC32C codec tests === *)

let test_crc32c_known_value () =
  (* iSCSI CRC32C test vector from RFC 3720 *)
  let input = Bytes.of_string "123456789" in
  let with_checksum = Codec_crc32c.encode input in
  check int "length increased by 4" (Bytes.length input + 4) (Bytes.length with_checksum);
  (* Verify checksum *)
  let checksum = Bytes.get_int32_le with_checksum (Bytes.length input) in
  check int32 "CRC32C of '123456789'" 0xe3069283l checksum

let test_crc32c_roundtrip () =
  let input = Bytes.of_string "Hello, World!" in
  let encoded = Codec_crc32c.encode input in
  match Codec_crc32c.decode encoded with
  | Ok decoded -> check bytes "roundtrip" input decoded
  | Error _ -> fail "decode failed"

let test_crc32c_corruption () =
  let input = Bytes.of_string "Hello, World!" in
  let encoded = Codec_crc32c.encode input in
  (* Corrupt one byte *)
  Bytes.set encoded 5 'X';
  match Codec_crc32c.decode encoded with
  | Ok _ -> fail "should detect corruption"
  | Error `Checksum_mismatch -> ()
  | Error _ -> fail "wrong error type"

(* === Gzip codec tests === *)

let test_gzip_roundtrip () =
  let input = Bytes.of_string "Hello, World! This is some test data to compress." in
  let codec = Codec_gzip.create 6 in
  let compressed = codec.encode input in
  (* Compressed should generally be smaller or at least not much larger *)
  check bool "compression works" true (Bytes.length compressed > 0);
  match codec.decode compressed with
  | Ok decompressed -> check bytes "roundtrip" input decompressed
  | Error _ -> fail "decompress failed"

let test_gzip_empty () =
  let input = Bytes.empty in
  let codec = Codec_gzip.create 6 in
  let compressed = codec.encode input in
  match codec.decode compressed with
  | Ok decompressed -> check bytes "empty roundtrip" input decompressed
  | Error _ -> fail "decompress empty failed"

let test_gzip_levels () =
  let input = Bytes.of_string (String.make 1000 'a') in
  let codec1 = Codec_gzip.create 1 in
  let codec9 = Codec_gzip.create 9 in
  let compressed1 = codec1.encode input in
  let compressed9 = codec9.encode input in
  (* Higher compression level should generally produce smaller output for compressible data *)
  check bool "level 9 <= level 1"
    true (Bytes.length compressed9 <= Bytes.length compressed1)

(* === Transpose codec tests === *)

let test_transpose_2d () =
  let arr = Chunk_data.create_zero Dtype.Int32 [|2; 3|] in
  (* Set values: [[1,2,3], [4,5,6]] *)
  Chunk_data.set arr [|0; 0|] (`Int32 1l);
  Chunk_data.set arr [|0; 1|] (`Int32 2l);
  Chunk_data.set arr [|0; 2|] (`Int32 3l);
  Chunk_data.set arr [|1; 0|] (`Int32 4l);
  Chunk_data.set arr [|1; 1|] (`Int32 5l);
  Chunk_data.set arr [|1; 2|] (`Int32 6l);

  let codec = Codec_transpose.create [|1; 0|] in
  let transposed = codec.encode arr in

  check (array int) "transposed shape" [|3; 2|] (Chunk_data.shape transposed);
  (* Check transposed values *)
  check int "element 0,0" 1
    (match Chunk_data.get transposed [|0; 0|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "element 0,1" 4
    (match Chunk_data.get transposed [|0; 1|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "element 2,1" 6
    (match Chunk_data.get transposed [|2; 1|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_transpose_roundtrip () =
  let arr = Chunk_data.create_zero Dtype.Int32 [|2; 3; 4|] in
  for i = 0 to 1 do
    for j = 0 to 2 do
      for k = 0 to 3 do
        Chunk_data.set arr [|i; j; k|] (`Int32 (Int32.of_int (i * 12 + j * 4 + k)))
      done
    done
  done;

  let codec = Codec_transpose.create [|2; 0; 1|] in
  let encoded = codec.encode arr in
  let decoded = codec.decode encoded in

  check (array int) "roundtrip shape" [|2; 3; 4|] (Chunk_data.shape decoded);
  for i = 0 to 1 do
    for j = 0 to 2 do
      for k = 0 to 3 do
        let expected = i * 12 + j * 4 + k in
        match Chunk_data.get decoded [|i; j; k|] with
        | `Int32 v -> check int (Printf.sprintf "element %d,%d,%d" i j k) expected (Int32.to_int v)
        | _ -> fail "expected int32"
      done
    done
  done

(* === Zstd codec tests === *)

let test_zstd_roundtrip () =
  let input = Bytes.of_string "Hello, World! This is some test data to compress with zstd." in
  let codec = Codec_zstd.create 3 in
  let compressed = codec.encode input in
  check bool "compression works" true (Bytes.length compressed > 0);
  match codec.decode compressed with
  | Ok decompressed -> check bytes "roundtrip" input decompressed
  | Error _ -> fail "decompress failed"

let test_zstd_empty () =
  let input = Bytes.empty in
  let codec = Codec_zstd.create 3 in
  let compressed = codec.encode input in
  match codec.decode compressed with
  | Ok decompressed -> check bytes "empty roundtrip" input decompressed
  | Error _ -> fail "decompress empty failed"

let test_zstd_levels () =
  let input = Bytes.of_string (String.make 10000 'a') in
  let codec1 = Codec_zstd.create 1 in
  let codec19 = Codec_zstd.create 19 in
  let compressed1 = codec1.encode input in
  let compressed19 = codec19.encode input in
  check bool "level 19 <= level 1"
    true (Bytes.length compressed19 <= Bytes.length compressed1)

let test_zstd_large_data () =
  let input = Bytes.init 100000 (fun i -> Char.chr (i mod 256)) in
  let codec = Codec_zstd.create 3 in
  let compressed = codec.encode input in
  match codec.decode compressed with
  | Ok decompressed -> check bytes "large roundtrip" input decompressed
  | Error _ -> fail "decompress large failed"

let test_zstd_decode_invalid () =
  let input = Bytes.of_string "this is not valid zstd data" in
  let codec = Codec_zstd.create 3 in
  match codec.decode input with
  | Ok _ -> fail "should fail on invalid data"
  | Error (`Codec_error _) -> ()
  | Error _ -> fail "wrong error type"

(* === Codec chain tests === *)

let test_codec_chain_bytes_only () =
  match Codec.build_chain_default [Codec.Bytes { endian = Some Dtype.Little }] Dtype.Int32 [|10|] with
  | Error (`Codec_error msg) -> fail ("should build: " ^ msg)
  | Error _ -> fail "should build: unknown error"
  | Ok chain ->
    let arr = Chunk_data.create_zero Dtype.Int32 [|3|] in
    Chunk_data.set arr [|0|] (`Int32 1l);
    Chunk_data.set arr [|1|] (`Int32 2l);
    Chunk_data.set arr [|2|] (`Int32 3l);

    let encoded = Codec.encode chain arr in
    let decoded = decode_ok chain [|3|] Dtype.Int32 encoded in

    check int "first" 1
      (match Chunk_data.get decoded [|0|] with `Int32 i -> Int32.to_int i | _ -> -1);
    check int "second" 2
      (match Chunk_data.get decoded [|1|] with `Int32 i -> Int32.to_int i | _ -> -1);
    check int "third" 3
      (match Chunk_data.get decoded [|2|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_codec_chain_with_gzip () =
  match Codec.build_chain_default [Codec.Bytes { endian = Some Dtype.Little }; Codec.Gzip { level = 5 }] Dtype.Int32 [|10|] with
  | Error _ -> fail "should build"
  | Ok chain ->
    let arr = Chunk_data.create_zero Dtype.Int32 [|100|] in
    for i = 0 to 99 do
      Chunk_data.set arr [|i|] (`Int32 (Int32.of_int i))
    done;

    let encoded = Codec.encode chain arr in
    let decoded = decode_ok chain [|100|] Dtype.Int32 encoded in

    for i = 0 to 99 do
      match Chunk_data.get decoded [|i|] with
      | `Int32 v -> check int (Printf.sprintf "element %d" i) i (Int32.to_int v)
      | _ -> fail "expected int32"
    done

let test_codec_chain_with_zstd () =
  match Codec.build_chain_default [Codec.Bytes { endian = Some Dtype.Little }; Codec.Zstd { level = 3; checksum = false }] Dtype.Int32 [|10|] with
  | Error _ -> fail "should build"
  | Ok chain ->
    let arr = Chunk_data.create_zero Dtype.Int32 [|100|] in
    for i = 0 to 99 do
      Chunk_data.set arr [|i|] (`Int32 (Int32.of_int i))
    done;

    let encoded = Codec.encode chain arr in
    let decoded = decode_ok chain [|100|] Dtype.Int32 encoded in

    for i = 0 to 99 do
      match Chunk_data.get decoded [|i|] with
      | `Int32 v -> check int (Printf.sprintf "element %d" i) i (Int32.to_int v)
      | _ -> fail "expected int32"
    done

let test_codec_chain_no_array_to_bytes () =
  match Codec.build_chain_default [Codec.Gzip { level = 5 }] Dtype.Int32 [|10|] with
  | Error (`Codec_error _) -> ()
  | Ok _ -> fail "should fail without array->bytes"
  | Error _ -> fail "wrong error type"

(* === Chain ordering error tests === *)

let test_codec_chain_a2a_after_a2b () =
  (* Transpose (a2a) after Bytes (a2b) should fail *)
  match Codec.build_chain_default
    [Codec.Bytes { endian = Some Dtype.Little }; Codec.Transpose { order = [|0|] }]
    Dtype.Int32 [|10|] with
  | Error (`Codec_error msg) ->
    check bool "mentions ordering" true (String.length msg > 0)
  | Ok _ -> fail "should fail: a2a after a2b"
  | Error _ -> fail "wrong error type"

let test_codec_chain_multiple_a2b () =
  (* Two Bytes (a2b) codecs should fail *)
  match Codec.build_chain_default
    [Codec.Bytes { endian = Some Dtype.Little }; Codec.Bytes { endian = Some Dtype.Big }]
    Dtype.Int32 [|10|] with
  | Error (`Codec_error msg) ->
    check bool "mentions ordering" true (String.length msg > 0)
  | Ok _ -> fail "should fail: multiple a2b"
  | Error _ -> fail "wrong error type"

let test_codec_chain_b2b_before_a2b () =
  (* Gzip (b2b) before Bytes (a2b) should fail *)
  match Codec.build_chain_default
    [Codec.Gzip { level = 5 }; Codec.Bytes { endian = Some Dtype.Little }]
    Dtype.Int32 [|10|] with
  | Error (`Codec_error msg) ->
    check bool "mentions ordering" true (String.length msg > 0)
  | Ok _ -> fail "should fail: b2b before a2b"
  | Error _ -> fail "wrong error type"

(* === Decode error propagation tests === *)

let test_decode_corrupt_gzip () =
  match Codec.build_chain_default [Codec.Bytes { endian = Some Dtype.Little }; Codec.Gzip { level = 5 }] Dtype.Int32 [|10|] with
  | Error _ -> fail "should build"
  | Ok chain ->
    let corrupt_data = Bytes.of_string "this is not valid gzip data at all!" in
    match Codec.decode chain [|10|] Dtype.Int32 corrupt_data with
    | Ok _ -> fail "should fail on corrupt gzip data"
    | Error (`Codec_error _) -> ()
    | Error _ -> fail "wrong error type"

let tests = [
  "bytes little endian int32", `Quick, test_bytes_little_endian_int32;
  "bytes big endian int32", `Quick, test_bytes_big_endian_int32;
  "bytes roundtrip", `Quick, test_bytes_roundtrip;
  "crc32c known value", `Quick, test_crc32c_known_value;
  "crc32c roundtrip", `Quick, test_crc32c_roundtrip;
  "crc32c corruption", `Quick, test_crc32c_corruption;
  "gzip roundtrip", `Quick, test_gzip_roundtrip;
  "gzip empty", `Quick, test_gzip_empty;
  "gzip levels", `Quick, test_gzip_levels;
  "transpose 2d", `Quick, test_transpose_2d;
  "transpose roundtrip", `Quick, test_transpose_roundtrip;
  "zstd roundtrip", `Quick, test_zstd_roundtrip;
  "zstd empty", `Quick, test_zstd_empty;
  "zstd levels", `Quick, test_zstd_levels;
  "zstd large data", `Quick, test_zstd_large_data;
  "zstd decode invalid", `Quick, test_zstd_decode_invalid;
  "codec chain bytes only", `Quick, test_codec_chain_bytes_only;
  "codec chain with gzip", `Quick, test_codec_chain_with_gzip;
  "codec chain with zstd", `Quick, test_codec_chain_with_zstd;
  "codec chain no array->bytes", `Quick, test_codec_chain_no_array_to_bytes;
  "chain ordering: a2a after a2b", `Quick, test_codec_chain_a2a_after_a2b;
  "chain ordering: multiple a2b", `Quick, test_codec_chain_multiple_a2b;
  "chain ordering: b2b before a2b", `Quick, test_codec_chain_b2b_before_a2b;
  "decode error: corrupt gzip", `Quick, test_decode_corrupt_gzip;
]
