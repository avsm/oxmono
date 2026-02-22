(** Tests for array operations *)

open Alcotest
open Zarr
open Zarr_sync

(* Module aliases for nested types *)
module D = Zarr.Ztypes.Dtype
module E = Zarr.Ztypes.Endianness
module FV = Zarr.Ztypes.Fill_value

let test_create_array () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|100; 100|]
    ~chunks:[|10; 10|]
    ~dtype:D.Float64
    ~fill_value:(FV.Float 0.0)
    ~codecs:[Zarr.Bytes { endian = Some E.Little }]
    () with
  | Error _ -> fail "should create array"
  | Ok arr ->
    check (array int) "shape" [|100; 100|] (Memory_array.shape arr);
    check (array int) "chunks" [|10; 10|] (Memory_array.chunks arr);
    (* Check metadata was written *)
    check bool "metadata exists"
      true (Memory_store.exists store "test/zarr.json")

let test_open_array () =
  let store = Memory_store.create () in
  (match Memory_array.create store
    ~path:"test"
    ~shape:[|50; 50|]
    ~chunks:[|10; 10|]
    ~dtype:D.Int32
    () with
  | Error _ -> fail "should create array"
  | Ok _ -> ());

  match Memory_array.open_ store ~path:"test" with
  | Error _ -> fail "should open array"
  | Ok arr ->
    check (array int) "shape" [|50; 50|] (Memory_array.shape arr)

let test_get_set_scalar () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|10; 10|]
    ~chunks:[|5; 5|]
    ~dtype:D.Int32
    ~fill_value:(FV.Int 0L)
    () with
  | Error _ -> fail "should create array"
  | Ok arr ->
    Memory_array.set arr [|3; 4|] (`Int32 42l);
    match Memory_array.get arr [|3; 4|] with
    | `Int32 v -> check int32 "get after set" 42l v
    | _ -> fail "expected int32"

let test_fill_value () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|10; 10|]
    ~chunks:[|5; 5|]
    ~dtype:D.Float64
    ~fill_value:FV.NaN
    () with
  | Error _ -> fail "should create array"
  | Ok arr ->
    (* Unwritten chunk should return fill value *)
    match Memory_array.get arr [|0; 0|] with
    | `Float f -> check bool "is nan" true (Float.is_nan f)
    | _ -> fail "expected float"

let test_get_set_slice () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|20; 20|]
    ~chunks:[|5; 5|]
    ~dtype:D.Int32
    ~fill_value:(FV.Int 0L)
    () with
  | Error _ -> fail "should create array"
  | Ok arr ->
    (* Create a 5x5 array to write *)
    let data = Ndarray.create D.Int32 [|5; 5|] in
    for i = 0 to 4 do
      for j = 0 to 4 do
        Ndarray.set data [|i; j|] (`Int32 (Int32.of_int (i * 5 + j)))
      done
    done;

    Memory_array.set_slice arr [Zarr.Range (0, 5); Zarr.Range (0, 5)] data;

    (* Read back *)
    let read_data = Memory_array.get_slice arr [Zarr.Range (0, 5); Zarr.Range (0, 5)] in
    check (array int) "shape" [|5; 5|] (Ndarray.shape read_data);

    for i = 0 to 4 do
      for j = 0 to 4 do
        match Ndarray.get read_data [|i; j|] with
        | `Int32 v ->
          check int32 (Printf.sprintf "element %d,%d" i j)
            (Int32.of_int (i * 5 + j)) v
        | _ -> fail "expected int32"
      done
    done

let test_cross_chunk_slice () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|20; 20|]
    ~chunks:[|5; 5|]
    ~dtype:D.Int32
    ~fill_value:(FV.Int 0L)
    () with
  | Error _ -> fail "should create array"
  | Ok arr ->
    (* Write across chunk boundaries *)
    let data = Ndarray.create D.Int32 [|8; 8|] in
    for i = 0 to 7 do
      for j = 0 to 7 do
        Ndarray.set data [|i; j|] (`Int32 (Int32.of_int (i * 8 + j + 100)))
      done
    done;

    Memory_array.set_slice arr [Zarr.Range (3, 11); Zarr.Range (3, 11)] data;

    (* Read back *)
    let read_data = Memory_array.get_slice arr [Zarr.Range (3, 11); Zarr.Range (3, 11)] in
    for i = 0 to 7 do
      for j = 0 to 7 do
        match Ndarray.get read_data [|i; j|] with
        | `Int32 v ->
          check int32 (Printf.sprintf "element %d,%d" i j)
            (Int32.of_int (i * 8 + j + 100)) v
        | _ -> fail "expected int32"
      done
    done

let test_array_with_gzip () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|100; 100|]
    ~chunks:[|10; 10|]
    ~dtype:D.Float64
    ~codecs:[Zarr.Bytes { endian = Some E.Little }; Zarr.Gzip { level = 5 }]
    () with
  | Error _ -> fail "should create array"
  | Ok arr ->
    (* Write some data *)
    let data = Ndarray.create D.Float64 [|10; 10|] in
    for i = 0 to 9 do
      for j = 0 to 9 do
        Ndarray.set data [|i; j|] (`Float (Float.of_int (i * 10 + j)))
      done
    done;

    Memory_array.set_slice arr [Zarr.Range (0, 10); Zarr.Range (0, 10)] data;

    (* Read back *)
    let read_data = Memory_array.get_slice arr [Zarr.Range (0, 10); Zarr.Range (0, 10)] in
    for i = 0 to 9 do
      for j = 0 to 9 do
        match Ndarray.get read_data [|i; j|] with
        | `Float v ->
          check (float 0.001) (Printf.sprintf "element %d,%d" i j)
            (Float.of_int (i * 10 + j)) v
        | _ -> fail "expected float"
      done
    done

let test_array_attributes () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|10|]
    ~chunks:[|10|]
    ~dtype:D.Int32
    () with
  | Error _ -> fail "should create array"
  | Ok arr ->
    Memory_array.set_attrs arr (`Assoc [("key", `String "value")]);
    (* Reopen and check *)
    match Memory_array.open_ store ~path:"test" with
    | Error _ -> fail "should open array"
    | Ok arr2 ->
      let attrs = Memory_array.attrs arr2 in
      match attrs with
      | `Assoc [("key", `String "value")] -> ()
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
