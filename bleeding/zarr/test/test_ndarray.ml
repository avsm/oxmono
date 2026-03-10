(** Tests for N-dimensional array (Chunk_data) operations *)

open Alcotest
open Zarr

let test_create_and_shape () =
  let arr = Chunk_data.create_zero Dtype.Int32 [|10; 20; 30|] in
  check (array int) "shape" [|10; 20; 30|] (Chunk_data.shape arr);
  check int "ndim" 3 (Chunk_data.ndim arr);
  check int "numel" 6000 (Chunk_data.numel arr)

let test_create_various_dtypes () =
  let _ = Chunk_data.create_zero Dtype.Bool [|5|] in
  let _ = Chunk_data.create_zero Dtype.Int8 [|5|] in
  let _ = Chunk_data.create_zero Dtype.Int16 [|5|] in
  let _ = Chunk_data.create_zero Dtype.Int32 [|5|] in
  let _ = Chunk_data.create_zero Dtype.Int64 [|5|] in
  let _ = Chunk_data.create_zero Dtype.Uint8 [|5|] in
  let _ = Chunk_data.create_zero Dtype.Uint16 [|5|] in
  let _ = Chunk_data.create_zero Dtype.Float32 [|5|] in
  let _ = Chunk_data.create_zero Dtype.Float64 [|5|] in
  ()

let test_fill () =
  let arr = Chunk_data.create Dtype.Int32 [|3; 3|] (Fill.Int 42L) in
  check int "first element" 42
    (match Chunk_data.get arr [|0; 0|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "last element" 42
    (match Chunk_data.get arr [|2; 2|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_get_set () =
  let arr = Chunk_data.create_zero Dtype.Int32 [|5; 5|] in
  Chunk_data.set arr [|2; 3|] (`Int32 123l);
  check int "get after set" 123
    (match Chunk_data.get arr [|2; 3|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "other still zero" 0
    (match Chunk_data.get arr [|0; 0|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_to_bytes_from_bytes () =
  let arr = Chunk_data.create_zero Dtype.Int32 [|3|] in
  Chunk_data.set arr [|0|] (`Int32 1l);
  Chunk_data.set arr [|1|] (`Int32 2l);
  Chunk_data.set arr [|2|] (`Int32 256l);

  let bytes_le = Chunk_data.to_bytes Dtype.Little arr in
  check int "bytes length" 12 (Bytes.length bytes_le);
  check bytes "first int32 LE"
    (Bytes.of_string "\x01\x00\x00\x00")
    (Bytes.sub bytes_le 0 4);

  let bytes_be = Chunk_data.to_bytes Dtype.Big arr in
  check bytes "first int32 BE"
    (Bytes.of_string "\x00\x00\x00\x01")
    (Bytes.sub bytes_be 0 4);

  let arr2 = Chunk_data.of_bytes Dtype.Int32 Dtype.Little [|3|] bytes_le in
  check int "roundtrip first" 1
    (match Chunk_data.get arr2 [|0|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "roundtrip second" 2
    (match Chunk_data.get arr2 [|1|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "roundtrip third" 256
    (match Chunk_data.get arr2 [|2|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_float64_bytes () =
  let arr = Chunk_data.create_zero Dtype.Float64 [|2|] in
  Chunk_data.set arr [|0|] (`Float 1.5);
  Chunk_data.set arr [|1|] (`Float 2.5);

  let bytes = Chunk_data.to_bytes Dtype.Little arr in
  check int "float64 bytes length" 16 (Bytes.length bytes);

  let arr2 = Chunk_data.of_bytes Dtype.Float64 Dtype.Little [|2|] bytes in
  (match Chunk_data.get arr2 [|0|] with
   | `Float f -> check (float 0.001) "first float" 1.5 f
   | _ -> fail "expected float");
  (match Chunk_data.get arr2 [|1|] with
   | `Float f -> check (float 0.001) "second float" 2.5 f
   | _ -> fail "expected float")

let test_transpose () =
  let arr = Chunk_data.create_zero Dtype.Int32 [|2; 3|] in
  Chunk_data.set arr [|0; 0|] (`Int32 1l);
  Chunk_data.set arr [|0; 1|] (`Int32 2l);
  Chunk_data.set arr [|0; 2|] (`Int32 3l);
  Chunk_data.set arr [|1; 0|] (`Int32 4l);
  Chunk_data.set arr [|1; 1|] (`Int32 5l);
  Chunk_data.set arr [|1; 2|] (`Int32 6l);

  let arr2 = Chunk_data.transpose arr [|1; 0|] in
  check (array int) "transposed shape" [|3; 2|] (Chunk_data.shape arr2);
  check int "element 0,0" 1
    (match Chunk_data.get arr2 [|0; 0|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "element 0,1" 4
    (match Chunk_data.get arr2 [|0; 1|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "element 2,0" 3
    (match Chunk_data.get arr2 [|2; 0|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_index_conversions () =
  let dims = [|3; 4; 5|] in
  (* Test index_to_offset and offset_to_index are inverses *)
  let idx = [|1; 2; 3|] in
  let offset = Chunk_data.index_to_offset dims idx in
  let idx2 = Chunk_data.offset_to_index dims offset in
  check (array int) "roundtrip index" idx idx2;

  (* Test specific offset calculation *)
  let offset = Chunk_data.index_to_offset [|10; 10|] [|2; 3|] in
  check int "2D offset" 23 offset  (* 2*10 + 3 = 23 *)

let tests = [
  "create and shape", `Quick, test_create_and_shape;
  "create various dtypes", `Quick, test_create_various_dtypes;
  "fill", `Quick, test_fill;
  "get/set", `Quick, test_get_set;
  "to_bytes/from_bytes", `Quick, test_to_bytes_from_bytes;
  "float64 bytes", `Quick, test_float64_bytes;
  "transpose", `Quick, test_transpose;
  "index conversions", `Quick, test_index_conversions;
]
