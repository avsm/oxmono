(** Tests for array operations *)

open Alcotest
open Zarr
open Zarr_sync

let none = Jsont.Meta.none
let jstr s = Jsont.String (s, none)
let jobj ms = Jsont.Object (ms, none)
let jmem n v : Jsont.mem = ((n, none), v)

let test_create_array () =
  let store = Memory_store.create () in
  let arr = Memory_array.create store
    ~path:"test"
    ~shape:[|100; 100|]
    ~chunks:[|10; 10|]
    ~dtype:Dtype.Float64
    ~fill_value:(Fill.Float 0.0)
    ~codecs:[Codec.Bytes { endian = Some Dtype.Little }]
    () in
  check (array int) "shape" [|100; 100|] (Memory_array.shape arr);
  check (array int) "chunks" [|10; 10|] (Memory_array.chunks arr);
  (* Check metadata was written *)
  check bool "metadata exists"
    true (Memory_store.exists store "test/zarr.json")

let test_open_array () =
  let store = Memory_store.create () in
  ignore (Memory_array.create store
    ~path:"test"
    ~shape:[|50; 50|]
    ~chunks:[|10; 10|]
    ~dtype:Dtype.Int32
    ());

  let arr = Memory_array.open_ store ~path:"test" in
  check (array int) "shape" [|50; 50|] (Memory_array.shape arr)

let test_get_set_scalar () =
  let store = Memory_store.create () in
  let arr = Memory_array.create store
    ~path:"test"
    ~shape:[|10; 10|]
    ~chunks:[|5; 5|]
    ~dtype:Dtype.Int32
    ~fill_value:(Fill.Int 0L)
    () in
  Memory_array.set arr [|3; 4|] (`Int32 42l);
  match Memory_array.get arr [|3; 4|] with
  | `Int32 v -> check int32 "get after set" 42l v
  | _ -> fail "expected int32"

let test_fill_value () =
  let store = Memory_store.create () in
  let arr = Memory_array.create store
    ~path:"test"
    ~shape:[|10; 10|]
    ~chunks:[|5; 5|]
    ~dtype:Dtype.Float64
    ~fill_value:Fill.NaN
    () in
  (* Unwritten chunk should return fill value *)
  match Memory_array.get arr [|0; 0|] with
  | `Float f -> check bool "is nan" true (Float.is_nan f)
  | _ -> fail "expected float"

let test_get_set_slice () =
  let store = Memory_store.create () in
  let arr = Memory_array.create store
    ~path:"test"
    ~shape:[|20; 20|]
    ~chunks:[|5; 5|]
    ~dtype:Dtype.Int32
    ~fill_value:(Fill.Int 0L)
    () in
  (* Create a 5x5 array to write *)
  let data = Chunk_data.create_zero Dtype.Int32 [|5; 5|] in
  for i = 0 to 4 do
    for j = 0 to 4 do
      Chunk_data.set data [|i; j|] (`Int32 (Int32.of_int (i * 5 + j)))
    done
  done;

  Memory_array.set_slice arr [Slice.Range (0, 5); Slice.Range (0, 5)] data;

  (* Read back *)
  let read_data = Memory_array.get_slice arr [Slice.Range (0, 5); Slice.Range (0, 5)] in
  check (array int) "shape" [|5; 5|] (Chunk_data.shape read_data);

  for i = 0 to 4 do
    for j = 0 to 4 do
      match Chunk_data.get read_data [|i; j|] with
      | `Int32 v ->
        check int32 (Printf.sprintf "element %d,%d" i j)
          (Int32.of_int (i * 5 + j)) v
      | _ -> fail "expected int32"
    done
  done

let test_cross_chunk_slice () =
  let store = Memory_store.create () in
  let arr = Memory_array.create store
    ~path:"test"
    ~shape:[|20; 20|]
    ~chunks:[|5; 5|]
    ~dtype:Dtype.Int32
    ~fill_value:(Fill.Int 0L)
    () in
  (* Write across chunk boundaries *)
  let data = Chunk_data.create_zero Dtype.Int32 [|8; 8|] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      Chunk_data.set data [|i; j|] (`Int32 (Int32.of_int (i * 8 + j + 100)))
    done
  done;

  Memory_array.set_slice arr [Slice.Range (3, 11); Slice.Range (3, 11)] data;

  (* Read back *)
  let read_data = Memory_array.get_slice arr [Slice.Range (3, 11); Slice.Range (3, 11)] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      match Chunk_data.get read_data [|i; j|] with
      | `Int32 v ->
        check int32 (Printf.sprintf "element %d,%d" i j)
          (Int32.of_int (i * 8 + j + 100)) v
      | _ -> fail "expected int32"
    done
  done

let test_array_with_gzip () =
  let store = Memory_store.create () in
  let arr = Memory_array.create store
    ~path:"test"
    ~shape:[|100; 100|]
    ~chunks:[|10; 10|]
    ~dtype:Dtype.Float64
    ~codecs:[Codec.Bytes { endian = Some Dtype.Little }; Codec.Gzip { level = 5 }]
    () in
  (* Write some data *)
  let data = Chunk_data.create_zero Dtype.Float64 [|10; 10|] in
  for i = 0 to 9 do
    for j = 0 to 9 do
      Chunk_data.set data [|i; j|] (`Float (Float.of_int (i * 10 + j)))
    done
  done;

  Memory_array.set_slice arr [Slice.Range (0, 10); Slice.Range (0, 10)] data;

  (* Read back *)
  let read_data = Memory_array.get_slice arr [Slice.Range (0, 10); Slice.Range (0, 10)] in
  for i = 0 to 9 do
    for j = 0 to 9 do
      match Chunk_data.get read_data [|i; j|] with
      | `Float v ->
        check (float 0.001) (Printf.sprintf "element %d,%d" i j)
          (Float.of_int (i * 10 + j)) v
      | _ -> fail "expected float"
    done
  done

let test_array_attributes () =
  let store = Memory_store.create () in
  let arr = Memory_array.create store
    ~path:"test"
    ~shape:[|10|]
    ~chunks:[|10|]
    ~dtype:Dtype.Int32
    () in
  Memory_array.set_attrs arr (jobj [jmem "key" (jstr "value")]);
  (* Reopen and check *)
  let arr2 = Memory_array.open_ store ~path:"test" in
  let attrs = Memory_array.attrs arr2 in
  match attrs with
  | Jsont.Object ([(("key", _), Jsont.String ("value", _))], _) -> ()
  | _ -> fail "wrong attributes"

let tests = [
  "create array", `Quick, test_create_array;
  "open array", `Quick, test_open_array;
  "get/set scalar", `Quick, test_get_set_scalar;
  "fill value", `Quick, test_fill_value;
  "get/set slice", `Quick, test_get_set_slice;
  "cross chunk slice", `Quick, test_cross_chunk_slice;
  "array with gzip", `Quick, test_array_with_gzip;
  "array attributes", `Quick, test_array_attributes;
]
