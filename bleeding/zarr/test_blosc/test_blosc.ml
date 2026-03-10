(** Tests for Blosc codec *)

open Alcotest
open Zarr_blosc

(* Force Zarr module initialization *)
let () = ignore Zarr.default_codecs

let test_blosc_roundtrip () =
  let input = Bytes.of_string "Hello, World! This is some test data to compress with blosc." in
  let codec = Blosc.create
    ~cname:Blosc.LZ4
    ~clevel:5
    ~shuffle:Blosc.NoShuffle
    ~typesize:1
    ~blocksize:0 in
  let compressed = codec.encode input in
  check bool "compression works" true (Bytes.length compressed > 0);
  let decompressed = codec.decode compressed in
  check bytes "roundtrip" input decompressed

let test_blosc_shuffle () =
  (* Create data with 4-byte integers - shuffle works well with typed data *)
  let n = 1000 in
  let input = Bytes.create (n * 4) in
  for i = 0 to n - 1 do
    Bytes.set_int32_le input (i * 4) (Int32.of_int i)
  done;
  let codec = Blosc.create
    ~cname:Blosc.LZ4
    ~clevel:5
    ~shuffle:Blosc.Shuffle
    ~typesize:4
    ~blocksize:0 in
  let compressed = codec.encode input in
  let decompressed = codec.decode compressed in
  check bytes "shuffle roundtrip" input decompressed

let test_blosc_bitshuffle () =
  let n = 1000 in
  let input = Bytes.create (n * 4) in
  for i = 0 to n - 1 do
    Bytes.set_int32_le input (i * 4) (Int32.of_int i)
  done;
  let codec = Blosc.create
    ~cname:Blosc.LZ4
    ~clevel:5
    ~shuffle:Blosc.BitShuffle
    ~typesize:4
    ~blocksize:0 in
  let compressed = codec.encode input in
  let decompressed = codec.decode compressed in
  check bytes "bitshuffle roundtrip" input decompressed

let test_blosc_compressors () =
  let input = Bytes.init 10000 (fun i -> Char.chr (i mod 256)) in
  let compressors = [
    ("lz4", Blosc.LZ4);
    ("lz4hc", Blosc.LZ4HC);
    ("blosclz", Blosc.BloscLZ);
    ("zstd", Blosc.Zstd);
    ("zlib", Blosc.Zlib);
  ] in
  List.iter (fun (name, cname) ->
    let codec = Blosc.create
      ~cname
      ~clevel:5
      ~shuffle:Blosc.Shuffle
      ~typesize:1
      ~blocksize:0 in
    let compressed = codec.encode input in
    let decompressed = codec.decode compressed in
    check bytes (name ^ " roundtrip") input decompressed
  ) compressors

let test_blosc_levels () =
  let input = Bytes.of_string (String.make 10000 'a') in
  let codec1 = Blosc.create
    ~cname:Blosc.LZ4
    ~clevel:1
    ~shuffle:Blosc.NoShuffle
    ~typesize:1
    ~blocksize:0 in
  let codec9 = Blosc.create
    ~cname:Blosc.LZ4
    ~clevel:9
    ~shuffle:Blosc.NoShuffle
    ~typesize:1
    ~blocksize:0 in
  let compressed1 = codec1.encode input in
  let compressed9 = codec9.encode input in
  check bool "level 9 <= level 1"
    true (Bytes.length compressed9 <= Bytes.length compressed1)

let test_blosc_large_data () =
  let input = Bytes.init 100000 (fun i -> Char.chr (i mod 256)) in
  let codec = Blosc.create
    ~cname:Blosc.LZ4
    ~clevel:5
    ~shuffle:Blosc.Shuffle
    ~typesize:1
    ~blocksize:0 in
  let compressed = codec.encode input in
  let decompressed = codec.decode compressed in
  check bytes "large roundtrip" input decompressed

let test_blosc_decode_invalid () =
  let input = Bytes.of_string "this is not valid blosc data!!" in
  let codec = Blosc.create
    ~cname:Blosc.LZ4
    ~clevel:5
    ~shuffle:Blosc.NoShuffle
    ~typesize:1
    ~blocksize:0 in
  (try
    let _ = codec.decode input in
    fail "should fail on invalid data"
  with Zarr.Codec.Codec_error _ -> ())

let test_blosc_shuffle_modes_string () =
  let shuffle_testable = testable
    (fun fmt _ -> Format.pp_print_string fmt "<shuffle>")
    (fun _ _ -> true) in
  check shuffle_testable "noshuffle" Blosc.NoShuffle (Blosc.shuffle_of_string "noshuffle");
  check shuffle_testable "shuffle" Blosc.Shuffle (Blosc.shuffle_of_string "shuffle");
  check string "noshuffle to_string" "noshuffle" (Blosc.shuffle_to_string Blosc.NoShuffle);
  check string "shuffle to_string" "shuffle" (Blosc.shuffle_to_string Blosc.Shuffle);
  check string "bitshuffle to_string" "bitshuffle" (Blosc.shuffle_to_string Blosc.BitShuffle)

(* === Registry integration tests === *)

let test_blosc_registered () =
  check bool "blosc is registered" true (Zarr.Codec.is_registered "blosc")

let test_blosc_codec_in_chain () =
  let spec = Blosc.codec_spec ~cname:Blosc.LZ4 ~clevel:5
    ~shuffle:Blosc.NoShuffle ~typesize:4 ~blocksize:0 in
  let codecs = [Zarr.Codec.Bytes { endian = Some Zarr.Dtype.Little }; spec] in
  let chain = Zarr.Codec.build_chain_default codecs Zarr.Dtype.Int32 [|10|] in
  let arr = Zarr.Chunk_data.create_zero Zarr.Dtype.Int32 [|10|] in
  for i = 0 to 9 do
    Zarr.Chunk_data.set arr [|i|] (`Int32 (Int32.of_int (i * 100)))
  done;
  let encoded = Zarr.Codec.encode chain arr in
  let decoded = Zarr.Codec.decode chain [|10|] Zarr.Dtype.Int32 encoded in
  for i = 0 to 9 do
    match Zarr.Chunk_data.get decoded [|i|] with
    | `Int32 v ->
      check int (Printf.sprintf "element %d" i) (i * 100) (Int32.to_int v)
    | _ -> fail "expected int32"
  done

let test_blosc_json_roundtrip () =
  let spec = Blosc.codec_spec ~cname:Blosc.LZ4 ~clevel:5
    ~shuffle:Blosc.Shuffle ~typesize:4 ~blocksize:0 in
  let json = Zarr.Codec.spec_to_json spec in
  (* Check JSON structure *)
  let find_member name = function
    | Jsont.Object (mems, _) ->
      (match List.find_opt (fun ((n, _), _) -> n = name) mems with
       | Some (_, v) -> v | None -> Jsont.Null ((), Jsont.Meta.none))
    | _ -> Jsont.Null ((), Jsont.Meta.none)
  in
  let to_str = function Jsont.String (s, _) -> s | _ -> failwith "expected string" in
  let to_int_j = function Jsont.Number (f, _) -> int_of_float f | _ -> failwith "expected number" in
  let name = json |> find_member "name" |> to_str in
  check string "codec name" "blosc" name;
  let config = json |> find_member "configuration" in
  let cname = config |> find_member "cname" |> to_str in
  check string "compressor" "lz4" cname;
  let clevel = config |> find_member "clevel" |> to_int_j in
  check int "level" 5 clevel;
  let shuffle = config |> find_member "shuffle" |> to_str in
  check string "shuffle" "shuffle" shuffle;
  let typesize = config |> find_member "typesize" |> to_int_j in
  check int "typesize" 4 typesize;
  let blocksize = config |> find_member "blocksize" |> to_int_j in
  check int "blocksize" 0 blocksize;
  (* Parse it back *)
  let specs = Zarr.Codec.specs_of_json [json] in
  check int "one spec" 1 (List.length specs);
  match List.hd specs with
  | Zarr.Codec.Extension { name = n; config = _ } ->
    check string "extension name" "blosc" n
  | _ -> fail "expected Extension variant"

let () =
  run "zarr-blosc" [
    "blosc", [
      "roundtrip", `Quick, test_blosc_roundtrip;
      "shuffle", `Quick, test_blosc_shuffle;
      "bitshuffle", `Quick, test_blosc_bitshuffle;
      "compressors", `Quick, test_blosc_compressors;
      "levels", `Quick, test_blosc_levels;
      "large data", `Quick, test_blosc_large_data;
      "decode invalid", `Quick, test_blosc_decode_invalid;
      "shuffle modes string", `Quick, test_blosc_shuffle_modes_string;
    ];
    "blosc-registry", [
      "blosc registered", `Quick, test_blosc_registered;
      "blosc codec in chain", `Quick, test_blosc_codec_in_chain;
      "blosc JSON roundtrip", `Quick, test_blosc_json_roundtrip;
    ]
  ]
