(** Integration tests for Zarr *)

open Alcotest
open Zarr
open Zarr_sync

let test_end_to_end_memory () =
  let store = Memory_store.create () in

  (* Create root group *)
  (match Memory_group.create store ~path:""
    ~attributes:(Some (`Assoc [("description", `String "Test hierarchy")])) () with
  | Error _ -> fail "should create root"
  | Ok _ -> ());

  (* Create data group *)
  (match Memory_group.create store ~path:"data" () with
  | Error _ -> fail "should create data group"
  | Ok _ -> ());

  (* Create a 2D array *)
  (match Memory_array.create store
    ~path:"data/temperatures"
    ~shape:[|365; 24|]  (* Days x Hours *)
    ~chunks:[|30; 24|]
    ~dtype:Float32
    ~fill_value:NaN
    ~codecs:[Bytes { endian = Some Little }; Gzip { level = 5 }]
    () with
  | Error _ -> fail "should create temperatures array"
  | Ok arr ->
    (* Write some data *)
    let data = Ndarray.create Float32 [|30; 24|] in
    for i = 0 to 29 do
      for j = 0 to 23 do
        Ndarray.set data [|i; j|] (`Float (20.0 +. Float.of_int i *. 0.1 +. Float.of_int j *. 0.05))
      done
    done;
    Memory_array.set_slice arr [Range (0, 30); Range (0, 24)] data);

  (* Create a 1D array *)
  (match Memory_array.create store
    ~path:"data/timestamps"
    ~shape:[|365|]
    ~chunks:[|100|]
    ~dtype:Int64
    ~fill_value:(Int 0L)
    () with
  | Error _ -> fail "should create timestamps array"
  | Ok arr ->
    let data = Ndarray.create Int64 [|100|] in
    for i = 0 to 99 do
      Ndarray.set data [|i|] (`Int64 (Int64.of_int (1609459200 + i * 86400)))
    done;
    Memory_array.set_slice arr [Range (0, 100)] data);

  (* Create a group for metadata *)
  (match Memory_group.create store ~path:"metadata"
    ~attributes:(Some (`Assoc [
      ("units", `String "celsius");
      ("location", `Assoc [("lat", `Float 51.5); ("lon", `Float (-0.1))]);
    ])) () with
  | Error _ -> fail "should create metadata group"
  | Ok _ -> ());

  (* Walk hierarchy *)
  let nodes = ref [] in
  Memory_hierarchy.walk store (fun path node_type ->
    nodes := (path, node_type) :: !nodes
  );

  check int "total nodes" 5 (List.length !nodes);

  (* Read back data *)
  match Memory_array.open_ store ~path:"data/temperatures" with
  | Error _ -> fail "should open temperatures"
  | Ok arr ->
    let data = Memory_array.get_slice arr [Range (0, 5); Range (0, 5)] in
    match Ndarray.get data [|0; 0|] with
    | `Float f -> check (float 0.01) "first temp" 20.0 f
    | _ -> fail "expected float"

let test_end_to_end_filesystem () =
  let test_dir = "/tmp/zarr_integration_" ^ string_of_int (Random.int 100000) in

  (* Clean up if exists *)
  if Sys.file_exists test_dir then
    ignore (Sys.command ("rm -rf " ^ test_dir));

  let store = Filesystem_store.create test_dir in

  (* Create array *)
  (match Filesystem_array.create store
    ~path:"myarray"
    ~shape:[|100; 100|]
    ~chunks:[|20; 20|]
    ~dtype:Float64
    ~fill_value:(Float 0.0)
    () with
  | Error _ -> fail "should create array"
  | Ok arr ->
    let data = Ndarray.create Float64 [|20; 20|] in
    for i = 0 to 19 do
      for j = 0 to 19 do
        Ndarray.set data [|i; j|] (`Float (Float.of_int (i * 20 + j)))
      done
    done;
    Filesystem_array.set_slice arr [Range (0, 20); Range (0, 20)] data);

  (* Verify files exist on disk *)
  check bool "metadata exists" true
    (Sys.file_exists (Filename.concat test_dir "myarray/zarr.json"));
  check bool "chunk exists" true
    (Sys.file_exists (Filename.concat test_dir "myarray/c/0/0"));

  (* Reopen and read *)
  let store2 = Filesystem_store.create test_dir in
  (match Filesystem_array.open_ store2 ~path:"myarray" with
  | Error _ -> fail "should reopen array"
  | Ok arr ->
    let data = Filesystem_array.get_slice arr [Range (0, 5); Range (0, 5)] in
    match Ndarray.get data [|0; 0|] with
    | `Float f -> check (float 0.001) "first value" 0.0 f
    | _ -> fail "expected float");

  (* Cleanup *)
  ignore (Sys.command ("rm -rf " ^ test_dir))

let test_large_array () =
  let store = Memory_store.create () in

  (* Create a reasonably large array *)
  match Memory_array.create store
    ~path:"large"
    ~shape:[|1000; 1000|]
    ~chunks:[|100; 100|]
    ~dtype:Float32
    ~fill_value:(Float 0.0)
    ~codecs:[Bytes { endian = Some Little }]
    () with
  | Error _ -> fail "should create large array"
  | Ok arr ->
    (* Write to a few chunks *)
    for chunk_i = 0 to 2 do
      for chunk_j = 0 to 2 do
        let data = Ndarray.create Float32 [|50; 50|] in
        for i = 0 to 49 do
          for j = 0 to 49 do
            Ndarray.set data [|i; j|] (`Float (Float.of_int (chunk_i * 1000 + chunk_j * 10 + i + j)))
          done
        done;
        let start_i = chunk_i * 100 in
        let start_j = chunk_j * 100 in
        Memory_array.set_slice arr
          [Range (start_i, start_i + 50); Range (start_j, start_j + 50)]
          data
      done
    done;

    (* Read back *)
    let data = Memory_array.get_slice arr [Range (50, 100); Range (50, 100)] in
    check (array int) "read shape" [|50; 50|] (Ndarray.shape data)

let test_various_dtypes () =
  let store = Memory_store.create () in

  (* Test various data types *)
  let test_dtype name dtype fill_val set_val get_check =
    match Memory_array.create store
      ~path:name
      ~shape:[|10|]
      ~chunks:[|10|]
      ~dtype
      ~fill_value:fill_val
      () with
    | Error _ -> fail ("should create " ^ name ^ " array")
    | Ok arr ->
      Memory_array.set arr [|0|] set_val;
      get_check (Memory_array.get arr [|0|])
  in

  test_dtype "bool" Bool (Bool false) (`Int 1)
    (function `Int 1 -> () | _ -> fail "bool check");

  test_dtype "int8" Int8 (Int 0L) (`Int (-42))
    (function `Int (-42) -> () | _ -> fail "int8 check");

  test_dtype "uint8" Uint8 (Uint 0L) (`Int 200)
    (function `Int 200 -> () | _ -> fail "uint8 check");

  test_dtype "int32" Int32 (Int 0L) (`Int32 12345l)
    (function `Int32 12345l -> () | _ -> fail "int32 check");

  test_dtype "int64" Int64 (Int 0L) (`Int64 9876543210L)
    (function `Int64 9876543210L -> () | _ -> fail "int64 check");

  test_dtype "float32" Float32 (Float 0.0) (`Float 3.14)
    (function `Float f when abs_float (f -. 3.14) < 0.01 -> () | _ -> fail "float32 check");

  test_dtype "float64" Float64 (Float 0.0) (`Float 2.718281828)
    (function `Float f when abs_float (f -. 2.718281828) < 0.0001 -> () | _ -> fail "float64 check")

let tests = [
  "end to end memory", `Quick, test_end_to_end_memory;
  "end to end filesystem", `Quick, test_end_to_end_filesystem;
  "large array", `Slow, test_large_array;
  "various dtypes", `Quick, test_various_dtypes;
]
