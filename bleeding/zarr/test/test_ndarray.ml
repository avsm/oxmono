(** Tests for N-dimensional array operations *)

open Alcotest
open Zarr

let test_create_and_shape () =
  let arr = Ndarray.create Int32 [|10; 20; 30|] in
  check (array int) "shape" [|10; 20; 30|] (Ndarray.shape arr);
  check int "ndim" 3 (Ndarray.ndim arr);
  check int "numel" 6000 (Ndarray.numel arr)

let test_create_various_dtypes () =
  let _ = Ndarray.create Bool [|5|] in
  let _ = Ndarray.create Int8 [|5|] in
  let _ = Ndarray.create Int16 [|5|] in
  let _ = Ndarray.create Int32 [|5|] in
  let _ = Ndarray.create Int64 [|5|] in
  let _ = Ndarray.create Uint8 [|5|] in
  let _ = Ndarray.create Uint16 [|5|] in
  let _ = Ndarray.create Float32 [|5|] in
  let _ = Ndarray.create Float64 [|5|] in
  let _ = Ndarray.create Complex64 [|5|] in
  let _ = Ndarray.create Complex128 [|5|] in
  ()

let test_fill () =
  let arr = Ndarray.create Int32 [|3; 3|] in
  Ndarray.fill arr (Int 42L);
  check int "first element" 42
    (match Ndarray.get arr [|0; 0|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "last element" 42
    (match Ndarray.get arr [|2; 2|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_get_set () =
  let arr = Ndarray.create Int32 [|5; 5|] in
  Ndarray.set arr [|2; 3|] (`Int32 123l);
  check int "get after set" 123
    (match Ndarray.get arr [|2; 3|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "other still zero" 0
    (match Ndarray.get arr [|0; 0|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_to_bytes_from_bytes () =
  let arr = Ndarray.create Int32 [|3|] in
  Ndarray.set arr [|0|] (`Int32 1l);
  Ndarray.set arr [|1|] (`Int32 2l);
  Ndarray.set arr [|2|] (`Int32 256l);

  let bytes_le = Ndarray.to_bytes Little arr in
  check int "bytes length" 12 (Bytes.length bytes_le);
  check bytes "first int32 LE"
    (Bytes.of_string "\x01\x00\x00\x00")
    (Bytes.sub bytes_le 0 4);

  let bytes_be = Ndarray.to_bytes Big arr in
  check bytes "first int32 BE"
    (Bytes.of_string "\x00\x00\x00\x01")
    (Bytes.sub bytes_be 0 4);

  let arr2 = Ndarray.of_bytes Int32 Little [|3|] bytes_le in
  check int "roundtrip first" 1
    (match Ndarray.get arr2 [|0|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "roundtrip second" 2
    (match Ndarray.get arr2 [|1|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "roundtrip third" 256
    (match Ndarray.get arr2 [|2|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_float64_bytes () =
  let arr = Ndarray.create Float64 [|2|] in
  Ndarray.set arr [|0|] (`Float 1.5);
  Ndarray.set arr [|1|] (`Float 2.5);

  let bytes = Ndarray.to_bytes Little arr in
  check int "float64 bytes length" 16 (Bytes.length bytes);

  let arr2 = Ndarray.of_bytes Float64 Little [|2|] bytes in
  (match Ndarray.get arr2 [|0|] with
   | `Float f -> check (float 0.001) "first float" 1.5 f
   | _ -> fail "expected float");
  (match Ndarray.get arr2 [|1|] with
   | `Float f -> check (float 0.001) "second float" 2.5 f
   | _ -> fail "expected float")

let test_reshape () =
  let arr = Ndarray.create Int32 [|6|] in
  for i = 0 to 5 do
    Ndarray.set arr [|i|] (`Int32 (Int32.of_int i))
  done;
  let arr2 = Ndarray.reshape arr [|2; 3|] in
  check (array int) "new shape" [|2; 3|] (Ndarray.shape arr2);
  check int "element 0,0" 0
    (match Ndarray.get arr2 [|0; 0|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "element 0,2" 2
    (match Ndarray.get arr2 [|0; 2|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "element 1,0" 3
    (match Ndarray.get arr2 [|1; 0|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_transpose () =
  let arr = Ndarray.create Int32 [|2; 3|] in
  Ndarray.set arr [|0; 0|] (`Int32 1l);
  Ndarray.set arr [|0; 1|] (`Int32 2l);
  Ndarray.set arr [|0; 2|] (`Int32 3l);
  Ndarray.set arr [|1; 0|] (`Int32 4l);
  Ndarray.set arr [|1; 1|] (`Int32 5l);
  Ndarray.set arr [|1; 2|] (`Int32 6l);

  let arr2 = Ndarray.transpose arr [|1; 0|] in
  check (array int) "transposed shape" [|3; 2|] (Ndarray.shape arr2);
  check int "element 0,0" 1
    (match Ndarray.get arr2 [|0; 0|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "element 0,1" 4
    (match Ndarray.get arr2 [|0; 1|] with `Int32 i -> Int32.to_int i | _ -> -1);
  check int "element 2,0" 3
    (match Ndarray.get arr2 [|2; 0|] with `Int32 i -> Int32.to_int i | _ -> -1)

let test_index_conversions () =
  let dims = [|3; 4; 5|] in
  (* Test index_to_offset and offset_to_index are inverses *)
  let idx = [|1; 2; 3|] in
  let offset = Ndarray.index_to_offset dims idx in
  let idx2 = Ndarray.offset_to_index dims offset in
  check (array int) "roundtrip index" idx idx2;

  (* Test specific offset calculation *)
  let offset = Ndarray.index_to_offset [|10; 10|] [|2; 3|] in
  check int "2D offset" 23 offset  (* 2*10 + 3 = 23 *)

let tests = [
  "create and shape", `Quick, test_create_and_shape;
  "create various dtypes", `Quick, test_create_various_dtypes;
  "fill", `Quick, test_fill;
  "get/set", `Quick, test_get_set;
  "to_bytes/from_bytes", `Quick, test_to_bytes_from_bytes;
  "float64 bytes", `Quick, test_float64_bytes;
  "reshape", `Quick, test_reshape;
  "transpose", `Quick, test_transpose;
  "index conversions", `Quick, test_index_conversions;
]
