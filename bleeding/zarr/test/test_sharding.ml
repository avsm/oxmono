(** Tests for sharding codec *)

open Alcotest
open Zarr
open Zarr_sync

let test_sharding_empty_marker () =
  check int64 "empty marker is -1" Int64.minus_one Codec_sharding.empty_marker

let test_sharding_basic () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|32; 32|]
    ~chunks:[|16; 16|]  (* Shard shape *)
    ~dtype:Dtype.Int32
    ~fill_value:(Fill.Int 0L)
    ~codecs:[
      Codec.Sharding {
        chunk_shape = [|4; 4|];  (* Inner chunk shape *)
        codecs = [Codec.Bytes { endian = Some Dtype.Little }];
        index_codecs = [Codec.Bytes { endian = Some Dtype.Little }];
        index_location = Codec.End;
      }
    ]
    () with
  | Error e ->
    fail ("should create sharded array: " ^ e)
  | Ok arr ->
    (* Write some data *)
    let data = Chunk_data.create_zero Dtype.Int32 [|8; 8|] in
    for i = 0 to 7 do
      for j = 0 to 7 do
        Chunk_data.set data [|i; j|] (`Int32 (Int32.of_int (i * 8 + j)))
      done
    done;

    Memory_array.set_slice arr [Slice.Range (0, 8); Slice.Range (0, 8)] data;

    (* Read back *)
    let read_data = Memory_array.get_slice arr [Slice.Range (0, 8); Slice.Range (0, 8)] in

    for i = 0 to 7 do
      for j = 0 to 7 do
        match Chunk_data.get read_data [|i; j|] with
        | `Int32 v ->
          check int32 (Printf.sprintf "element %d,%d" i j)
            (Int32.of_int (i * 8 + j)) v
        | _ -> fail "expected int32"
      done
    done

let test_sharding_with_crc32c () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|32; 32|]
    ~chunks:[|16; 16|]
    ~dtype:Dtype.Float64
    ~fill_value:(Fill.Float 0.0)
    ~codecs:[
      Codec.Sharding {
        chunk_shape = [|4; 4|];
        codecs = [Codec.Bytes { endian = Some Dtype.Little }];
        index_codecs = [Codec.Bytes { endian = Some Dtype.Little }; Codec.Crc32c];
        index_location = Codec.End;
      }
    ]
    () with
  | Error _ -> fail "should create sharded array with crc32c"
  | Ok arr ->
    let data = Chunk_data.create_zero Dtype.Float64 [|4; 4|] in
    for i = 0 to 3 do
      for j = 0 to 3 do
        Chunk_data.set data [|i; j|] (`Float (Float.of_int i +. Float.of_int j *. 0.1))
      done
    done;

    Memory_array.set_slice arr [Slice.Range (0, 4); Slice.Range (0, 4)] data;

    let read_data = Memory_array.get_slice arr [Slice.Range (0, 4); Slice.Range (0, 4)] in

    for i = 0 to 3 do
      for j = 0 to 3 do
        match Chunk_data.get read_data [|i; j|] with
        | `Float v ->
          check (float 0.001) (Printf.sprintf "element %d,%d" i j)
            (Float.of_int i +. Float.of_int j *. 0.1) v
        | _ -> fail "expected float"
      done
    done

let test_sharding_index_start () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|16; 16|]
    ~chunks:[|16; 16|]
    ~dtype:Dtype.Int32
    ~fill_value:(Fill.Int 0L)
    ~codecs:[
      Codec.Sharding {
        chunk_shape = [|4; 4|];
        codecs = [Codec.Bytes { endian = Some Dtype.Little }];
        index_codecs = [Codec.Bytes { endian = Some Dtype.Little }];
        index_location = Codec.Start;
      }
    ]
    () with
  | Error _ -> fail "should create sharded array with index at start"
  | Ok arr ->
    let data = Chunk_data.create_zero Dtype.Int32 [|4; 4|] in
    for i = 0 to 3 do
      for j = 0 to 3 do
        Chunk_data.set data [|i; j|] (`Int32 (Int32.of_int (i + j * 10)))
      done
    done;

    Memory_array.set_slice arr [Slice.Range (0, 4); Slice.Range (0, 4)] data;

    let read_data = Memory_array.get_slice arr [Slice.Range (0, 4); Slice.Range (0, 4)] in

    for i = 0 to 3 do
      for j = 0 to 3 do
        match Chunk_data.get read_data [|i; j|] with
        | `Int32 v ->
          check int32 (Printf.sprintf "element %d,%d" i j)
            (Int32.of_int (i + j * 10)) v
        | _ -> fail "expected int32"
      done
    done

let test_sharding_3d () =
  let store = Memory_store.create () in
  match Memory_array.create store
    ~path:"test"
    ~shape:[|16; 16; 16|]
    ~chunks:[|8; 8; 8|]
    ~dtype:Dtype.Int32
    ~fill_value:(Fill.Int 0L)
    ~codecs:[
      Codec.Sharding {
        chunk_shape = [|2; 2; 2|];
        codecs = [Codec.Bytes { endian = Some Dtype.Little }];
        index_codecs = [Codec.Bytes { endian = Some Dtype.Little }];
        index_location = Codec.End;
      }
    ]
    () with
  | Error _ -> fail "should create 3D sharded array"
  | Ok arr ->
    let data = Chunk_data.create_zero Dtype.Int32 [|4; 4; 4|] in
    for i = 0 to 3 do
      for j = 0 to 3 do
        for k = 0 to 3 do
          Chunk_data.set data [|i; j; k|] (`Int32 (Int32.of_int (i * 100 + j * 10 + k)))
        done
      done
    done;

    Memory_array.set_slice arr [Slice.Range (0, 4); Slice.Range (0, 4); Slice.Range (0, 4)] data;

    let read_data = Memory_array.get_slice arr [Slice.Range (0, 4); Slice.Range (0, 4); Slice.Range (0, 4)] in

    for i = 0 to 3 do
      for j = 0 to 3 do
        for k = 0 to 3 do
          match Chunk_data.get read_data [|i; j; k|] with
          | `Int32 v ->
            check int32 (Printf.sprintf "element %d,%d,%d" i j k)
              (Int32.of_int (i * 100 + j * 10 + k)) v
          | _ -> fail "expected int32"
        done
      done
    done

let test_sharding_codec_spec_json () =
  let spec = Codec.Sharding {
    chunk_shape = [|4; 4|];
    codecs = [Codec.Bytes { endian = Some Dtype.Little }];
    index_codecs = [Codec.Bytes { endian = Some Dtype.Little }; Codec.Crc32c];
    index_location = Codec.End;
  } in
  let json = Codec.spec_to_json spec in
  let json_str = match Jsont_bytesrw.encode_string Jsont.json json with Ok s -> s | Error _ -> "" in
  check bool "has sharding_indexed" true
    (String.length json_str > 0 &&
     (let open String in
      let rec contains s sub i =
        if i > length s - length sub then false
        else if sub = String.sub s i (length sub) then true
        else contains s sub (i + 1)
      in contains json_str "sharding_indexed" 0))

let tests = [
  "empty marker", `Quick, test_sharding_empty_marker;
  "basic sharding", `Quick, test_sharding_basic;
  "sharding with crc32c", `Quick, test_sharding_with_crc32c;
  "sharding index at start", `Quick, test_sharding_index_start;
  "3D sharding", `Quick, test_sharding_3d;
  "codec spec json", `Quick, test_sharding_codec_spec_json;
]
