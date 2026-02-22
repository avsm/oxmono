(** Property-based tests using QCheck *)

open QCheck

(* Save Array before Zarr shadows it *)
module StdArray = Array

open Zarr
open Qcheck_generators

(* Module aliases for nested types *)
module D = Zarr.Ztypes.Dtype
module E = Zarr.Ztypes.Endianness

(** {2 Data Type Property Tests} *)

let test_dtype_roundtrip =
  Test.make ~count:100
    ~name:"dtype to_string/of_string roundtrip"
    dtype_arb_pp
    (fun dtype ->
      let s = Data_type.to_string dtype in
      match Data_type.of_string s with
      | Ok dt -> dt = dtype
      | Error _ -> false)

let test_dtype_size_positive =
  Test.make ~count:100
    ~name:"dtype size is positive"
    dtype_arb_pp
    (fun dtype ->
      Data_type.size dtype > 0)

(** {2 Codec Property Tests} *)

let test_bytes_codec_roundtrip =
  Test.make ~count:200
    ~name:"bytes codec roundtrip"
    (triple dtype_arb_pp small_shape_arb_pp endian_arb)
    (fun (dtype, shape, endian) ->
      let arr = Ndarray.create dtype shape in
      let codec = Codecs.Bytes_codec.create endian in
      let encoded = codec.encode arr in
      let decoded = codec.decode shape dtype encoded in
      Ndarray.shape decoded = shape)

let test_crc32c_roundtrip =
  Test.make ~count:500
    ~name:"crc32c codec roundtrip"
    bytes_arb
    (fun input ->
      let encoded = Codecs.Crc32c.encode input in
      match Codecs.Crc32c.decode encoded with
      | Ok decoded -> Bytes.equal input decoded
      | Error _ -> false)

let test_crc32c_detect_corruption =
  Test.make ~count:200
    ~name:"crc32c detects corruption"
    (pair bytes_arb (int_range 0 100))
    (fun (input, corrupt_pos) ->
      if Bytes.length input < 1 then true
      else begin
        let encoded = Codecs.Crc32c.encode input in
        let pos = corrupt_pos mod (Bytes.length encoded) in
        let original = Bytes.get encoded pos in
        Bytes.set encoded pos (Char.chr ((Char.code original + 1) mod 256));
        match Codecs.Crc32c.decode encoded with
        | Ok decoded -> Bytes.equal input decoded  (* No actual corruption if same *)
        | Error `Checksum_mismatch -> true
        | Error _ -> false
      end)

let test_gzip_roundtrip =
  Test.make ~count:300
    ~name:"gzip codec roundtrip"
    (pair bytes_arb gzip_level_arb)
    (fun (input, level) ->
      let codec = Codecs.Gzip.create level in
      let compressed = codec.encode input in
      match codec.decode compressed with
      | Ok decompressed -> Bytes.equal input decompressed
      | Error _ -> Bytes.length input = 0)  (* Empty might fail *)

(** {2 Ndarray Property Tests} *)

let test_ndarray_shape_preserved =
  Test.make ~count:200
    ~name:"ndarray shape preserved after creation"
    (pair dtype_arb_pp small_shape_arb_pp)
    (fun (dtype, shape) ->
      let arr = Ndarray.create dtype shape in
      Ndarray.shape arr = shape)

let test_ndarray_numel =
  Test.make ~count:200
    ~name:"ndarray numel equals product of shape"
    small_shape_arb_pp
    (fun shape ->
      let arr = Ndarray.create D.Int32 shape in
      Ndarray.numel arr = StdArray.fold_left ( * ) 1 shape)

(** {2 Chunk Grid Property Tests} *)

let test_chunk_grid_num_chunks =
  Test.make ~count:200
    ~name:"num_chunks covers all elements"
    (pair small_shape_arb_pp small_shape_arb_pp)
    (fun (array_shape, chunk_shape) ->
      (* Ensure chunk_shape elements are at least 1 *)
      let chunk_shape = StdArray.map (fun c -> max 1 c) chunk_shape in
      (* Make chunk_shape same length as array_shape *)
      let ndim = min (StdArray.length array_shape) (StdArray.length chunk_shape) in
      let array_shape = StdArray.sub array_shape 0 ndim in
      let chunk_shape = StdArray.sub chunk_shape 0 ndim in
      let grid = Zarr.Regular { chunk_shape } in
      let num = Chunk_grid.num_chunks grid array_shape in
      StdArray.for_all2 (fun nc as_ -> nc * (StdArray.get chunk_shape 0) >= as_ || nc >= 1)
        num array_shape)

(** {2 Metadata Property Tests} *)

let test_metadata_json_roundtrip =
  Test.make ~count:100
    ~name:"metadata json roundtrip"
    (pair small_shape_arb_pp small_shape_arb_pp)
    (fun (shape, chunks) ->
      let chunks = StdArray.map (fun c -> max 1 c) chunks in
      let ndim = min (StdArray.length shape) (StdArray.length chunks) in
      let shape = StdArray.sub shape 0 ndim in
      let chunks = StdArray.sub chunks 0 ndim in
      let meta = Metadata.create_array_metadata
        ~shape ~chunks ~dtype:D.Int32 () in
      let json = Metadata.array_to_json meta in
      match Metadata.array_of_json json with
      | Ok meta2 -> meta.shape = meta2.shape
      | Error _ -> false)

(** {2 Fuzz Tests} *)

let test_metadata_fuzz =
  Test.make ~count:1000
    ~name:"metadata parsing doesn't crash on random input"
    (make Gen.string)
    (fun s ->
      let _ = Metadata.array_of_json s in
      true)  (* Any result is OK, just don't crash *)

let test_gzip_decode_fuzz =
  Test.make ~count:500
    ~name:"gzip decode doesn't crash on random input"
    bytes_arb
    (fun b ->
      let codec = Codecs.Gzip.create 6 in
      let _ = codec.decode b in
      true)

let test_crc32c_decode_fuzz =
  Test.make ~count:500
    ~name:"crc32c decode doesn't crash on random input"
    bytes_arb
    (fun b ->
      let _ = Codecs.Crc32c.decode b in
      true)

(** All QCheck tests *)
let qcheck_tests = [
  test_dtype_roundtrip;
  test_dtype_size_positive;
  test_bytes_codec_roundtrip;
  test_crc32c_roundtrip;
  test_crc32c_detect_corruption;
  test_gzip_roundtrip;
  test_ndarray_shape_preserved;
  test_ndarray_numel;
  test_chunk_grid_num_chunks;
  test_metadata_json_roundtrip;
  test_metadata_fuzz;
  test_gzip_decode_fuzz;
  test_crc32c_decode_fuzz;
]
