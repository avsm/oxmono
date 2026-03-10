(** Integration tests for Zarr *)

open Alcotest
open Zarr
open Zarr_sync

let none = Jsont.Meta.none
let jstr s = Jsont.String (s, none)
let jfloat f = Jsont.Number (f, none)
let jobj ms = Jsont.Object (ms, none)
let jmem n v : Jsont.mem = ((n, none), v)

let test_end_to_end_memory () =
  let store = Memory_store.create () in

  (* Create root group *)
  ignore (Memory_group.create store ~path:""
    ~attributes:(jobj [jmem "description" (jstr "Test hierarchy")]) ());

  (* Create data group *)
  ignore (Memory_group.create store ~path:"data" ());

  (* Create a 2D array *)
  let arr = Memory_array.create store
    ~path:"data/temperatures"
    ~shape:[|365; 24|]  (* Days x Hours *)
    ~chunks:[|30; 24|]
    ~dtype:Dtype.Float32
    ~fill_value:Fill.NaN
    ~codecs:[Codec.Bytes { endian = Some Dtype.Little }; Codec.Gzip { level = 5 }]
    () in
  (* Write some data *)
  let data = Chunk_data.create_zero Dtype.Float32 [|30; 24|] in
  for i = 0 to 29 do
    for j = 0 to 23 do
      Chunk_data.set data [|i; j|] (`Float (20.0 +. Float.of_int i *. 0.1 +. Float.of_int j *. 0.05))
    done
  done;
  Memory_array.set_slice arr [Slice.Range (0, 30); Slice.Range (0, 24)] data;

  (* Create a 1D array *)
  let arr2 = Memory_array.create store
    ~path:"data/timestamps"
    ~shape:[|365|]
    ~chunks:[|100|]
    ~dtype:Dtype.Int64
    ~fill_value:(Fill.Int 0L)
    () in
  let data = Chunk_data.create_zero Dtype.Int64 [|100|] in
  for i = 0 to 99 do
    Chunk_data.set data [|i|] (`Int64 (Int64.of_int (1609459200 + i * 86400)))
  done;
  Memory_array.set_slice arr2 [Slice.Range (0, 100)] data;

  (* Create a group for metadata *)
  ignore (Memory_group.create store ~path:"metadata"
    ~attributes:(jobj [
      jmem "units" (jstr "celsius");
      jmem "location" (jobj [jmem "lat" (jfloat 51.5); jmem "lon" (jfloat (-0.1))]);
    ]) ());

  (* Walk hierarchy *)
  let nodes = ref [] in
  Memory_hierarchy.walk store (fun path node_type ->
    nodes := (path, node_type) :: !nodes
  );

  check int "total nodes" 5 (List.length !nodes);

  (* Read back data *)
  let arr3 = Memory_array.open_ store ~path:"data/temperatures" in
  let data = Memory_array.get_slice arr3 [Slice.Range (0, 5); Slice.Range (0, 5)] in
  match Chunk_data.get data [|0; 0|] with
  | `Float f -> check (float 0.01) "first temp" 20.0 f
  | _ -> fail "expected float"

let test_end_to_end_filesystem () =
  let test_dir = "/tmp/zarr_integration_" ^ string_of_int (Random.int 100000) in

  (* Clean up if exists *)
  if Sys.file_exists test_dir then
    ignore (Sys.command ("rm -rf " ^ test_dir));

  let store = Filesystem_store.create test_dir in

  (* Create array *)
  let arr = Filesystem_array.create store
    ~path:"myarray"
    ~shape:[|100; 100|]
    ~chunks:[|20; 20|]
    ~dtype:Dtype.Float64
    ~fill_value:(Fill.Float 0.0)
    () in
  let data = Chunk_data.create_zero Dtype.Float64 [|20; 20|] in
  for i = 0 to 19 do
    for j = 0 to 19 do
      Chunk_data.set data [|i; j|] (`Float (Float.of_int (i * 20 + j)))
    done
  done;
  Filesystem_array.set_slice arr [Slice.Range (0, 20); Slice.Range (0, 20)] data;

  (* Verify files exist on disk *)
  check bool "metadata exists" true
    (Sys.file_exists (Filename.concat test_dir "myarray/zarr.json"));
  check bool "chunk exists" true
    (Sys.file_exists (Filename.concat test_dir "myarray/c/0/0"));

  (* Reopen and read *)
  let store2 = Filesystem_store.create test_dir in
  let arr2 = Filesystem_array.open_ store2 ~path:"myarray" in
  let data = Filesystem_array.get_slice arr2 [Slice.Range (0, 5); Slice.Range (0, 5)] in
  (match Chunk_data.get data [|0; 0|] with
  | `Float f -> check (float 0.001) "first value" 0.0 f
  | _ -> fail "expected float");

  (* Cleanup *)
  ignore (Sys.command ("rm -rf " ^ test_dir))

let test_large_array () =
  let store = Memory_store.create () in

  (* Create a reasonably large array *)
  let arr = Memory_array.create store
    ~path:"large"
    ~shape:[|1000; 1000|]
    ~chunks:[|100; 100|]
    ~dtype:Dtype.Float32
    ~fill_value:(Fill.Float 0.0)
    ~codecs:[Codec.Bytes { endian = Some Dtype.Little }]
    () in
  (* Write to a few chunks *)
  for chunk_i = 0 to 2 do
    for chunk_j = 0 to 2 do
      let data = Chunk_data.create_zero Dtype.Float32 [|50; 50|] in
      for i = 0 to 49 do
        for j = 0 to 49 do
          Chunk_data.set data [|i; j|] (`Float (Float.of_int (chunk_i * 1000 + chunk_j * 10 + i + j)))
        done
      done;
      let start_i = chunk_i * 100 in
      let start_j = chunk_j * 100 in
      Memory_array.set_slice arr
        [Slice.Range (start_i, start_i + 50); Slice.Range (start_j, start_j + 50)]
        data
    done
  done;

  (* Read back *)
  let data = Memory_array.get_slice arr [Slice.Range (50, 100); Slice.Range (50, 100)] in
  check (array int) "read shape" [|50; 50|] (Chunk_data.shape data)

let test_various_dtypes () =
  let store = Memory_store.create () in

  (* Test various data types *)
  let test_dtype name dtype fill_val set_val get_check =
    let arr = Memory_array.create store
      ~path:name
      ~shape:[|10|]
      ~chunks:[|10|]
      ~dtype
      ~fill_value:fill_val
      () in
    Memory_array.set arr [|0|] set_val;
    get_check (Memory_array.get arr [|0|])
  in

  test_dtype "bool" Dtype.Bool (Fill.Bool false) (`Bool true)
    (function `Bool true -> () | _ -> fail "bool check");

  test_dtype "int8" Dtype.Int8 (Fill.Int 0L) (`Int (-42))
    (function `Int (-42) -> () | _ -> fail "int8 check");

  test_dtype "uint8" Dtype.Uint8 (Fill.Uint 0L) (`Int 200)
    (function `Int 200 -> () | _ -> fail "uint8 check");

  test_dtype "int32" Dtype.Int32 (Fill.Int 0L) (`Int32 12345l)
    (function `Int32 12345l -> () | _ -> fail "int32 check");

  test_dtype "int64" Dtype.Int64 (Fill.Int 0L) (`Int64 9876543210L)
    (function `Int64 9876543210L -> () | _ -> fail "int64 check");

  test_dtype "float32" Dtype.Float32 (Fill.Float 0.0) (`Float 3.14)
    (function `Float f when abs_float (f -. 3.14) < 0.01 -> () | _ -> fail "float32 check");

  test_dtype "float64" Dtype.Float64 (Fill.Float 0.0) (`Float 2.718281828)
    (function `Float f when abs_float (f -. 2.718281828) < 0.0001 -> () | _ -> fail "float64 check")

let tests = [
  "end to end memory", `Quick, test_end_to_end_memory;
  "end to end filesystem", `Quick, test_end_to_end_filesystem;
  "large array", `Slow, test_large_array;
  "various dtypes", `Quick, test_various_dtypes;
]
