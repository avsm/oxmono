(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Codec tests. Expectations come from the zarrs oracle, see
   ../fixtures/README.md for the provenance of every golden file. *)

module Byte_range = Zarrz.Byte_range
module Byte_source = Zarrz.Byte_source
module Chunk_grid = Zarrz.Chunk_grid
module Codec = Zarrz.Codec
module Dtype = Zarrz.Dtype
module Error = Zarrz.Error
module Ext = Zarrz.Ext
module Fill_value = Zarrz.Fill_value
module Metadata = Zarrz.Metadata
module Slab = Zarrz.Slab
module Subset = Zarrz.Subset
module Ia = Stdlib_stable.Iarray
module I16u = Stdlib_stable.Int16_u
module I32u = Stdlib_upstream_compatible.Int32_u
module I64u = Stdlib_upstream_compatible.Int64_u
module F32u = Stdlib_stable.Float32_u

(* {1 Helpers} *)

(* Without this a failing test reports a bare [Zarrz__Error.E(_)]. *)
let () =
  Printexc.register_printer (function
    | Error.E e -> Some ("Zarrz.Error.E: " ^ Error.to_string e)
    | _ -> None)

let read_file p =
  let ic = open_in_bin p in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let json_of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error m -> Alcotest.failf "test JSON is invalid: %s" m

let exts_of_string s =
  match Jsont_bytesrw.decode_string (Jsont.list Ext.jsont) s with
  | Ok l -> l
  | Error m -> Alcotest.failf "test codec metadata is invalid: %s" m

let bs = Base_bigstring.of_string
let str = Base_bigstring.to_string
let zeros dt = Fill_value.of_bytes (String.make (Dtype.size dt) '\000')

let chain ?resolver ~dtype ?fill json =
  let fill_value = match fill with Some f -> f | None -> zeros dtype in
  match Codec.chain_of_exts ?resolver ~dtype ~fill_value (exts_of_string json)
  with
  | Ok c -> c
  | Error m -> Alcotest.failf "chain %s: %s" json m

let chain_error ~dtype json =
  match
    Codec.chain_of_exts ~dtype ~fill_value:(zeros dtype) (exts_of_string json)
  with
  | Ok _ -> Alcotest.failf "chain %s: expected an error" json
  | Error m -> m

let repr dtype shape = { Codec.dtype; shape }
let slab_of dt shape s = Slab.of_bigstring dt (Ia.of_array shape) (bs s)

(* A whole chunk through a chain and back, as bytes. *)
let round_trip ~dtype ~shape json data =
  let c = chain ~dtype json in
  let enc = Codec.encode_chunk c (slab_of dtype shape data) in
  (str enc, str (Slab.bigstring (Codec.decode_chunk c (repr dtype shape) enc)))

let rand_string n = String.init n (fun _ -> Char.chr (Random.int 256))

(* {1 The bytes codec} *)

let all_dtypes =
  Dtype.
    [
      Bool; Int8; Int16; Int32; Int64; Uint8; Uint16; Uint32; Uint64; Float16;
      Bfloat16; Float32; Float64; Complex64; Complex128; Raw 3; Raw 8;
    ]

(* The width the declared endianness applies to, spelled out here rather
   than taken from the implementation under test. *)
let component (dt : Dtype.t) =
  match dt with
  | Complex64 -> 4
  | Complex128 -> 8
  | Bool | Raw _ | Int8 | Uint8 -> 1
  | d -> Dtype.size d

let swap_components dt s =
  let c = component dt in
  if c = 1 then s
  else
    String.init (String.length s) (fun i ->
        s.[(i / c * c) + c - 1 - (i mod c)])

let bytes_json endian =
  Printf.sprintf {|[{"name":"bytes","configuration":{"endian":"%s"}}]|} endian

let test_bytes_round_trip () =
  List.iter
    (fun dt ->
      let shape = [| 2; 3 |] in
      let orig = rand_string (6 * Dtype.size dt) in
      List.iter
        (fun (endian, big) ->
          let enc, dec = round_trip ~dtype:dt ~shape (bytes_json endian) orig in
          let want =
            if big <> Sys.big_endian then swap_components dt orig else orig
          in
          let what = Dtype.name dt ^ " " ^ endian in
          Alcotest.(check string) (what ^ " encode") want enc;
          Alcotest.(check string) (what ^ " decode") orig dec)
        [ ("little", false); ("big", true) ])
    all_dtypes

(* A NaN payload and a complex value with two different halves survive
   both directions bit for bit. *)
let test_bytes_bit_exact () =
  let f32 bits =
    String.init 4 (fun i -> Char.chr ((bits lsr (8 * i)) land 0xff))
  in
  let nan_bits = [ 0x7fc00001; 0xffc0dead; 0x7f800000 ] in
  let payload = String.concat "" (List.map f32 nan_bits) in
  List.iter
    (fun endian ->
      let enc, dec =
        round_trip ~dtype:Dtype.Float32 ~shape:[| 3 |] (bytes_json endian)
          payload
      in
      ignore enc;
      Alcotest.(check string) ("float32 nan " ^ endian) payload dec)
    [ "little"; "big" ];
  (* complex64 swaps each half on its own, never the whole element. *)
  let c = "\x01\x02\x03\x04\x05\x06\x07\x08" in
  let enc, dec =
    round_trip ~dtype:Dtype.Complex64 ~shape:[| 1 |] (bytes_json "big") c
  in
  let want = if Sys.big_endian then c else "\x04\x03\x02\x01\x08\x07\x06\x05" in
  Alcotest.(check string) "complex64 halves" want enc;
  Alcotest.(check string) "complex64 round trip" c dec

let test_bytes_endian_required () =
  Alcotest.(check bool)
    "uint8 without endian" true
    (match
       Codec.chain_of_exts ~dtype:Dtype.Uint8 ~fill_value:(zeros Dtype.Uint8)
         (exts_of_string {|[{"name":"bytes"}]|})
     with
    | Ok _ -> true
    | Error _ -> false);
  Alcotest.(check string)
    "float64 without endian"
    "codec \"bytes\": endian is required for data type float64"
    (chain_error ~dtype:Dtype.Float64 {|[{"name":"bytes"}]|});
  Alcotest.(check string)
    "bad endian"
    "codec \"bytes\": endian \"middle\" is not \"little\" or \"big\""
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes","configuration":{"endian":"middle"}}]|});
  Alcotest.(check string)
    "unknown member"
    "codec \"bytes\": unknown configuration member \"order\""
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes","configuration":{"endian":"little","order":[0]}}]|})

let test_bytes_length () =
  let c = chain ~dtype:Dtype.Uint16 (bytes_json "little") in
  Alcotest.check_raises "short chunk"
    (Error.E (Error.Codec "bytes: 7 encoded bytes for a 8 byte chunk"))
    (fun () ->
      ignore (Codec.decode_chunk c (repr Dtype.Uint16 [| 4 |]) (bs "1234567")))

let bytes_tests =
  [
    ("round trip", `Quick, test_bytes_round_trip);
    ("bit exact", `Quick, test_bytes_bit_exact);
    ("endian required", `Quick, test_bytes_endian_required);
    ("length", `Quick, test_bytes_length);
  ]

(* {1 The transpose codec} *)

(* An independent transpose over single byte elements: the element at
   destination coordinate [c] comes from source coordinate [p] where
   [p.(order.(i)) = c.(i)]. *)
let transpose_ref order shape s =
  let n = Array.length order in
  let dst_shape = Array.init n (fun i -> shape.(order.(i))) in
  let sstride = Array.make n 1 and dstride = Array.make n 1 in
  for d = n - 2 downto 0 do
    sstride.(d) <- sstride.(d + 1) * shape.(d + 1);
    dstride.(d) <- dstride.(d + 1) * dst_shape.(d + 1)
  done;
  let out = Bytes.create (String.length s) in
  let coord = Array.make n 0 in
  let rec go d =
    if d = n then begin
      let di = ref 0 and si = ref 0 in
      for i = 0 to n - 1 do
        di := !di + (coord.(i) * dstride.(i));
        si := !si + (coord.(i) * sstride.(order.(i)))
      done;
      Bytes.set out !di s.[!si]
    end
    else
      for i = 0 to dst_shape.(d) - 1 do
        coord.(d) <- i;
        go (d + 1)
      done
  in
  go 0;
  Bytes.to_string out

let transpose_json order =
  Printf.sprintf
    {|[{"name":"transpose","configuration":{"order":[%s]}},
       {"name":"bytes","configuration":{"endian":"little"}}]|}
    (String.concat "," (List.map string_of_int (Array.to_list order)))

let test_transpose () =
  let cases =
    [
      ([| 2; 3 |], [| 0; 1 |]);
      ([| 2; 3 |], [| 1; 0 |]);
      ([| 4; 4 |], [| 1; 0 |]);
      ([| 2; 3; 4 |], [| 0; 1; 2 |]);
      ([| 2; 3; 4 |], [| 2; 1; 0 |]);
      ([| 2; 3; 4 |], [| 1; 2; 0 |]);
      ([| 2; 3; 4 |], [| 2; 0; 1 |]);
      ([| 2; 3; 4 |], [| 0; 2; 1 |]);
    ]
  in
  List.iter
    (fun (shape, order) ->
      let n = Array.fold_left ( * ) 1 shape in
      let data = String.init n Char.chr in
      let enc, dec =
        round_trip ~dtype:Dtype.Uint8 ~shape (transpose_json order) data
      in
      let what =
        Printf.sprintf "%s by %s"
          (String.concat "x" (List.map string_of_int (Array.to_list shape)))
          (String.concat "," (List.map string_of_int (Array.to_list order)))
      in
      Alcotest.(check string)
        (what ^ " encode") (transpose_ref order shape data) enc;
      Alcotest.(check string) (what ^ " decode") data dec)
    cases

let test_transpose_errors () =
  Alcotest.(check string)
    "not a permutation" "codec \"transpose\": order is not a permutation of \
                         its own indices"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"transpose","configuration":{"order":[0,2]}},
          {"name":"bytes"}]|});
  Alcotest.(check string)
    "missing order" "codec \"transpose\": order is required"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"transpose","configuration":{}},{"name":"bytes"}]|});
  let c = chain ~dtype:Dtype.Uint8 (transpose_json [| 1; 0 |]) in
  Alcotest.check_raises "rank mismatch"
    (Error.E
       (Error.Codec "transpose: order has 2 entries for a rank 3 array"))
    (fun () ->
      ignore (Codec.encoded_size c (repr Dtype.Uint8 [| 1; 2; 3 |])))

let transpose_tests =
  [
    ("round trip", `Quick, test_transpose);
    ("errors", `Quick, test_transpose_errors);
  ]

(* {1 The gzip and zstd codecs} *)

let gzip_json level =
  Printf.sprintf
    {|[{"name":"bytes","configuration":{"endian":"little"}},
       {"name":"gzip","configuration":{"level":%d}}]|}
    level

let zstd_json level checksum =
  Printf.sprintf
    {|[{"name":"bytes","configuration":{"endian":"little"}},
       {"name":"zstd","configuration":{"level":%d,"checksum":%b}}]|}
    level checksum

let payloads =
  lazy
    [
      ("empty", "");
      ("one", "z");
      ("text", String.concat "" (List.init 400 (fun i -> string_of_int i)));
      ("random 64k", rand_string 65536);
      ("one mib random", rand_string (1024 * 1024));
      ("one mib zeros", String.make (1024 * 1024) '\000');
    ]

let test_compressor json name =
  List.iter
    (fun (what, data) ->
      let n = String.length data in
      let _, dec = round_trip ~dtype:Dtype.Uint8 ~shape:[| n |] json data in
      Alcotest.(check string) (name ^ " " ^ what) data dec)
    (Lazy.force payloads)

let test_gzip () =
  List.iter
    (fun level ->
      test_compressor (gzip_json level) (Printf.sprintf "gzip %d" level))
    [ 0; 1; 6; 9 ]

let test_zstd () =
  List.iter
    (fun (level, checksum) ->
      test_compressor (zstd_json level checksum)
        (Printf.sprintf "zstd %d %b" level checksum))
    [ (0, false); (1, false); (3, true); (9, true); (-3, false); (-5, true) ]

let test_compressor_config () =
  Alcotest.(check string)
    "gzip level required" "codec \"gzip\": level is required"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes"},{"name":"gzip"}]|});
  Alcotest.(check string)
    "gzip level range" "codec \"gzip\": level 10 is outside [0, 9]"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes"},{"name":"gzip","configuration":{"level":10}}]|});
  Alcotest.(check string)
    "zstd level required" "codec \"zstd\": level is required"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes"},
          {"name":"zstd","configuration":{"checksum":true}}]|});
  Alcotest.(check string)
    "zstd level range" "codec \"zstd\": level 23 is outside [-131072, 22]"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes"},{"name":"zstd","configuration":{"level":23}}]|});
  (* checksum defaults to false, as the oracle does when it lifts a
     numcodecs configuration. *)
  Alcotest.(check bool)
    "zstd without checksum" true
    (match
       Codec.chain_of_exts ~dtype:Dtype.Uint8 ~fill_value:(zeros Dtype.Uint8)
         (exts_of_string
            {|[{"name":"bytes"},{"name":"zstd","configuration":{"level":3}}]|})
     with
    | Ok _ -> true
    | Error _ -> false)

let test_gzip_corrupt () =
  let data = String.make 1000 'a' in
  let c = chain ~dtype:Dtype.Uint8 (gzip_json 6) in
  let enc = str (Codec.encode_chunk c (slab_of Dtype.Uint8 [| 1000 |] data)) in
  let flip i =
    let b = Bytes.of_string enc in
    Bytes.set b i (Char.chr (Char.code (Bytes.get b i) lxor 0xff));
    Bytes.to_string b
  in
  let fails what s =
    Alcotest.(check bool)
      what true
      (match
         Codec.decode_chunk c (repr Dtype.Uint8 [| 1000 |]) (bs s)
       with
      | _ -> false
      | exception Error.E _ -> true)
  in
  fails "magic" (flip 0);
  fails "trailer crc" (flip (String.length enc - 5))

(* Every optional header field RFC 1952 defines is skipped on decode,
   whatever a foreign encoder chose to write. *)
let test_gzip_header_fields () =
  let data = String.make 500 'q' in
  let c = chain ~dtype:Dtype.Uint8 (gzip_json 6) in
  let enc = str (Codec.encode_chunk c (slab_of Dtype.Uint8 [| 500 |] data)) in
  let n = String.length enc in
  let body = String.sub enc 10 (n - 18) in
  let trailer = String.sub enc (n - 8) 8 in
  let header flg fields =
    Printf.sprintf "\x1f\x8b\x08%c\x00\x00\x00\x00\x00\xff%s"
      (Char.chr flg) fields
  in
  List.iter
    (fun (what, h) ->
      let s = h ^ body ^ trailer in
      Alcotest.(check string)
        what data
        (str
           (Slab.bigstring
              (Codec.decode_chunk c (repr Dtype.Uint8 [| 500 |]) (bs s)))))
    [
      ("fextra", header 0x04 "\x03\x00abc");
      ("fname", header 0x08 "chunk\000");
      ("fcomment", header 0x10 "a comment\000");
      ("fhcrc", header 0x02 "\x12\x34");
      ("all four", header 0x1e "\x03\x00abcchunk\000c\000\x12\x34");
    ]

let compressor_tests =
  [
    ("gzip", `Quick, test_gzip);
    ("gzip header fields", `Quick, test_gzip_header_fields);
    ("zstd", `Slow, test_zstd);
    ("configuration", `Quick, test_compressor_config);
    ("gzip corruption", `Quick, test_gzip_corrupt);
  ]

(* {1 The blosc codec}

   Blosc frames are not reproducible across builds of the C library, so
   nothing here compares encoded bytes. What is checked is that a frame
   this codec writes reads back, that the parameters reach blosc, and
   that the frames the oracle and the Tessera store wrote decode to the
   values they were written from. *)

let blosc_json ?(cname = "zstd") ?(clevel = 5) ?(shuffle = "shuffle")
    ?typesize ?(blocksize = 0) () =
  Printf.sprintf
    {|[{"name":"bytes","configuration":{"endian":"little"}},
       {"name":"blosc","configuration":{"cname":"%s","clevel":%d,
        "shuffle":"%s"%s,"blocksize":%d}}]|}
    cname clevel shuffle
    (match typesize with
    | None -> ""
    | Some t -> Printf.sprintf {|,"typesize":%d|} t)
    blocksize

(* Elements rather than bytes, so that a chunk shape can be derived for
   any data type. Values that repeat across elements are what the
   shuffle filter is for, so the payloads include some. *)
let blosc_payloads =
  lazy
    [
      ("empty", "");
      ("one element", "\x01\x02\x03\x04\x05\x06\x07\x08");
      ( "a ramp",
        String.concat ""
          (List.init 4096 (fun i ->
               let b n = Char.chr ((i lsr n) land 0xff) in
               String.init 8 (fun k -> b (8 * k)))) );
      ("random 64k", rand_string 65536);
      ("zeros", String.make 65536 '\000');
    ]

let blosc_round_trip ~dtype ~cname ~shuffle =
  let esize = Dtype.size dtype in
  List.iter
    (fun (what, data) ->
      let n = String.length data in
      if n mod esize = 0 then begin
        let json = blosc_json ~cname ~shuffle ~typesize:esize () in
        let _, dec = round_trip ~dtype ~shape:[| n / esize |] json data in
        Alcotest.(check string)
          (Printf.sprintf "%s %s %s %s" cname shuffle (Dtype.name dtype) what)
          data dec
      end)
    (Lazy.force blosc_payloads)

(* The chains the Tessera store uses: zstd inside blosc, with a shuffle
   over a four byte element and a bitshuffle over a one byte one. *)
let test_blosc_round_trip () =
  List.iter
    (fun shuffle ->
      blosc_round_trip ~dtype:Dtype.Float32 ~cname:"zstd" ~shuffle;
      blosc_round_trip ~dtype:Dtype.Int8 ~cname:"zstd" ~shuffle)
    [ "shuffle"; "bitshuffle"; "noshuffle" ]

(* Every build of the C library has blosclz, and lz4 is all but
   universal. A build without one is skipped rather than failed, but a
   build without zstd is not: the Tessera store needs it and the round
   trips above would pass without noticing. *)
let test_blosc_compressors () =
  Alcotest.(check bool)
    (Printf.sprintf "zstd is in [%s]"
       (String.concat ", " (Bloscz.compressors ())))
    true
    (List.mem "zstd" (Bloscz.compressors ()));
  List.iter
    (fun cname ->
      if List.mem cname (Bloscz.compressors ()) then
        blosc_round_trip ~dtype:Dtype.Float32 ~cname ~shuffle:"shuffle")
    [ "blosclz"; "lz4" ]

(* typesize is optional in the metadata and defaults to the size of one
   element of the data type, so a configuration without it still
   shuffles the right width. *)
let test_blosc_typesize_default () =
  let data = rand_string 4096 in
  let _, dec =
    round_trip ~dtype:Dtype.Float32 ~shape:[| 1024 |]
      (blosc_json ~shuffle:"bitshuffle" ())
      data
  in
  Alcotest.(check string) "no typesize" data dec;
  (* A blocksize blosc is told rather than one it picks. *)
  let _, dec =
    round_trip ~dtype:Dtype.Float32 ~shape:[| 1024 |]
      (blosc_json ~typesize:4 ~blocksize:1024 ())
      data
  in
  Alcotest.(check string) "an explicit blocksize" data dec

let test_blosc_config () =
  Alcotest.(check string)
    "cname required" "codec \"blosc\": cname is required"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes"},{"name":"blosc","configuration":{"clevel":5}}]|});
  Alcotest.(check string)
    "unknown cname"
    "codec \"blosc\": cname \"lzma\" is not one of blosclz, lz4, lz4hc, \
     snappy, zlib, zstd"
    (chain_error ~dtype:Dtype.Uint8
       (blosc_json ~cname:"lzma" ()));
  Alcotest.(check string)
    "clevel required" "codec \"blosc\": clevel is required"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes"},
          {"name":"blosc","configuration":{"cname":"zstd"}}]|});
  Alcotest.(check string)
    "clevel range" "codec \"blosc\": clevel 10 is outside [0, 9]"
    (chain_error ~dtype:Dtype.Uint8 (blosc_json ~clevel:10 ()));
  Alcotest.(check string)
    "clevel range below" "codec \"blosc\": clevel -1 is outside [0, 9]"
    (chain_error ~dtype:Dtype.Uint8 (blosc_json ~clevel:(-1) ()));
  Alcotest.(check string)
    "unknown shuffle"
    "codec \"blosc\": shuffle \"auto\" is not \"noshuffle\", \"shuffle\" or \
     \"bitshuffle\""
    (chain_error ~dtype:Dtype.Uint8 (blosc_json ~shuffle:"auto" ()));
  Alcotest.(check string)
    "typesize range" "codec \"blosc\": typesize 0 is below 1"
    (chain_error ~dtype:Dtype.Uint8 (blosc_json ~typesize:0 ()));
  Alcotest.(check string)
    "negative blocksize" "codec \"blosc\": blocksize -1 is negative"
    (chain_error ~dtype:Dtype.Uint8 (blosc_json ~blocksize:(-1) ()));
  Alcotest.(check string)
    "unknown member"
    "codec \"blosc\": unknown configuration member \"nthreads\""
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes"},
          {"name":"blosc","configuration":{"cname":"zstd","clevel":5,
           "nthreads":4}}]|});
  (* shuffle defaults to noshuffle, as the oracle's serde default does. *)
  Alcotest.(check bool)
    "no shuffle member" true
    (match
       Codec.chain_of_exts ~dtype:Dtype.Uint8 ~fill_value:(zeros Dtype.Uint8)
         (exts_of_string
            {|[{"name":"bytes"},
               {"name":"blosc","configuration":{"cname":"zstd","clevel":5}}]|})
     with
    | Ok _ -> true
    | Error _ -> false)

(* A frame from a stranger must fail rather than read past its own
   bytes. The header records the frame length and the block offsets, so
   each of these is a different lie about them. *)
let test_blosc_corrupt () =
  let data = String.make 4096 'a' in
  let c = chain ~dtype:Dtype.Uint8 (blosc_json ~typesize:1 ()) in
  let enc = str (Codec.encode_chunk c (slab_of Dtype.Uint8 [| 4096 |] data)) in
  let fails what s =
    Alcotest.(check bool)
      what true
      (match Codec.decode_chunk c (repr Dtype.Uint8 [| 4096 |]) (bs s) with
      | _ -> false
      | exception Error.E _ -> true)
  in
  let n = String.length enc in
  fails "empty" "";
  fails "a partial header" (String.sub enc 0 8);
  fails "garbage" (String.make 200 '\xab');
  fails "a truncated frame" (String.sub enc 0 (n / 2));
  fails "a header alone" (String.sub enc 0 16);
  let flip i =
    let b = Bytes.of_string enc in
    Bytes.set b i (Char.chr (Char.code (Bytes.get b i) lxor 0xff));
    Bytes.to_string b
  in
  fails "a flipped header byte" (flip 4);
  fails "a flipped body byte" (flip (n - 3))

(* The decoded size the chain asks for is checked against the size the
   frame records, so a chunk of the wrong shape is caught before the
   decompressor writes anything. *)
let test_blosc_wrong_size () =
  let data = String.make 4096 'a' in
  let c = chain ~dtype:Dtype.Uint8 (blosc_json ~typesize:1 ()) in
  let enc = Codec.encode_chunk c (slab_of Dtype.Uint8 [| 4096 |] data) in
  Alcotest.(check bool)
    "a chunk of another shape" true
    (match Codec.decode_chunk c (repr Dtype.Uint8 [| 2048 |]) enc with
    | _ -> false
    | exception Error.E _ -> true)

(* blosc adds at most a header to an input it cannot compress. *)
let test_blosc_size () =
  let c = chain ~dtype:Dtype.Uint8 (blosc_json ~typesize:1 ()) in
  Alcotest.(check bool)
    "bounded by the overhead" true
    (match Codec.encoded_size c (repr Dtype.Uint8 [| 1000 |]) with
    | Codec.Bounded n -> n = 1000 + Bloscz.max_overhead
    | _ -> false);
  let data = rand_string 8192 in
  let enc = Codec.encode_chunk c (slab_of Dtype.Uint8 [| 8192 |] data) in
  Alcotest.(check bool)
    "a random chunk stays inside the bound" true
    (Base_bigstring.length enc <= 8192 + Bloscz.max_overhead)

let blosc_tests =
  [
    ("round trip", `Quick, test_blosc_round_trip);
    ("compressors", `Quick, test_blosc_compressors);
    ("typesize and blocksize", `Quick, test_blosc_typesize_default);
    ("configuration", `Quick, test_blosc_config);
    ("corrupt frames", `Quick, test_blosc_corrupt);
    ("the wrong decoded size", `Quick, test_blosc_wrong_size);
    ("encoded size", `Quick, test_blosc_size);
  ]

(* {1 The crc32c codec} *)

let crc32c_json =
  {|[{"name":"bytes","configuration":{"endian":"little"}},{"name":"crc32c"}]|}

let test_crc32c_vector () =
  let data = "123456789" in
  let c = chain ~dtype:Dtype.Uint8 crc32c_json in
  let enc = str (Codec.encode_chunk c (slab_of Dtype.Uint8 [| 9 |] data)) in
  Alcotest.(check int) "length" 13 (String.length enc);
  Alcotest.(check string) "payload" data (String.sub enc 0 9);
  (* CRC32C of "123456789" is 0xe3069283, appended little endian. *)
  Alcotest.(check string) "checksum" "\x83\x92\x06\xe3" (String.sub enc 9 4);
  Alcotest.(check string)
    "round trip" data
    (str
       (Slab.bigstring
          (Codec.decode_chunk c (repr Dtype.Uint8 [| 9 |]) (bs enc))))

let test_crc32c_sizes () =
  let c = chain ~dtype:Dtype.Uint32 crc32c_json in
  Alcotest.(check bool)
    "fixed plus four" true
    (match Codec.encoded_size c (repr Dtype.Uint32 [| 5 |]) with
    | Codec.Fixed 24 -> true
    | _ -> false)

let test_crc32c_mismatch () =
  let data = "123456789" in
  let c = chain ~dtype:Dtype.Uint8 crc32c_json in
  let enc = str (Codec.encode_chunk c (slab_of Dtype.Uint8 [| 9 |] data)) in
  let bad = Bytes.of_string enc in
  Bytes.set bad 3 'X';
  match
    Codec.decode_chunk c (repr Dtype.Uint8 [| 9 |]) (bs (Bytes.to_string bad))
  with
  | _ -> Alcotest.fail "a corrupt payload decoded"
  | exception Error.E (Error.Checksum_mismatch { expected; got }) ->
      Alcotest.(check int32) "expected" 0xe3069283l expected;
      Alcotest.(check bool) "got differs" true (not (Int32.equal expected got))

let test_crc32c_short () =
  let c = chain ~dtype:Dtype.Uint8 crc32c_json in
  Alcotest.check_raises "three bytes"
    (Error.E (Error.Codec "crc32c: 3 bytes cannot hold a checksum"))
    (fun () ->
      ignore (Codec.decode_chunk c (repr Dtype.Uint8 [| 0 |]) (bs "abc")))

let crc32c_tests =
  [
    ("known vector", `Quick, test_crc32c_vector);
    ("encoded size", `Quick, test_crc32c_sizes);
    ("mismatch", `Quick, test_crc32c_mismatch);
    ("truncated", `Quick, test_crc32c_short);
  ]

(* {1 Whole chains} *)

let test_full_chains () =
  let data = rand_string 4096 in
  List.iter
    (fun (what, json) ->
      let _, dec = round_trip ~dtype:Dtype.Uint8 ~shape:[| 4096 |] json data in
      Alcotest.(check string) what data dec)
    [
      ( "bytes zstd crc32c",
        {|[{"name":"bytes","configuration":{"endian":"little"}},
           {"name":"zstd","configuration":{"level":5,"checksum":true}},
           {"name":"crc32c"}]|} );
      ( "bytes gzip crc32c",
        {|[{"name":"bytes","configuration":{"endian":"little"}},
           {"name":"gzip","configuration":{"level":9}},
           {"name":"crc32c"}]|} );
      ( "crc32c before bytes in the document",
        {|[{"name":"crc32c"},
           {"name":"bytes","configuration":{"endian":"big"}}]|} );
      (* gzip below zstd sees a bounded rather than a fixed decoded
         size, which is the other arm of its output sizing. *)
      ( "bytes zstd gzip",
        {|[{"name":"bytes","configuration":{"endian":"little"}},
           {"name":"zstd","configuration":{"level":1,"checksum":false}},
           {"name":"gzip","configuration":{"level":6}}]|} );
      ( "transpose bytes gzip crc32c",
        {|[{"name":"transpose","configuration":{"order":[0]}},
           {"name":"bytes","configuration":{"endian":"little"}},
           {"name":"gzip","configuration":{"level":1}},
           {"name":"crc32c"}]|} );
    ]

let test_chain_shape () =
  Alcotest.(check string)
    "unknown codec" "unknown codec \"nope\""
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"bytes"},{"name":"nope"}]|});
  Alcotest.(check string)
    "missing array to bytes" "missing array to bytes codec"
    (chain_error ~dtype:Dtype.Uint8 {|[{"name":"crc32c"}]|});
  Alcotest.(check string)
    "two array to bytes" "multiple array to bytes codecs"
    (chain_error ~dtype:Dtype.Uint8 {|[{"name":"bytes"},{"name":"bytes"}]|});
  (* An unknown codec that need not be understood is dropped, and the
     chain metadata records the drop. *)
  let c =
    chain ~dtype:Dtype.Uint8
      {|[{"name":"nope","must_understand":false},{"name":"bytes"}]|}
  in
  Alcotest.(check (list string))
    "kept extensions" [ "bytes" ]
    (List.map (fun (e : Ext.t) -> e.name) (Codec.chain_exts c));
  let data = "hello" in
  let enc = Codec.encode_chunk c (slab_of Dtype.Uint8 [| 5 |] data) in
  Alcotest.(check string) "skipped chain still works" data (str enc)

let chain_tests =
  [
    ("full chains", `Quick, test_full_chains);
    ("chain shape", `Quick, test_chain_shape);
  ]

(* {1 Sharding} *)

let shard_json ?(loc = "end") ?(inner = bytes_json "little") chunk_shape =
  Printf.sprintf
    {|[{"name":"sharding_indexed","configuration":{
         "chunk_shape":[%s],
         "codecs":%s,
         "index_codecs":[{"name":"bytes","configuration":{"endian":"little"}},
                         {"name":"crc32c"}],
         "index_location":"%s"}}]|}
    (String.concat "," (List.map string_of_int (Array.to_list chunk_shape)))
    inner loc

(* The index of an encoded shard, as raw [int64] entries. Its byte size
   is two [uint64] per inner chunk plus the four crc32c bytes. *)
let shard_entries ~loc ~chunks s =
  let m = (16 * chunks) + 4 in
  let base = if loc = "start" then 0 else String.length s - m in
  Array.init (2 * chunks) (fun i -> String.get_int64_le s (base + (8 * i)))

let counting src =
  let reads = ref 0 and many = ref 0 and ranges = ref [] in
  let t =
    {
      Byte_source.size = src.Byte_source.size;
      read =
        (fun r ->
          incr reads;
          ranges := !ranges @ [ r ];
          src.Byte_source.read r);
      read_many =
        (fun rs ->
          incr many;
          ranges := !ranges @ rs;
          src.Byte_source.read_many rs);
    }
  in
  (t, reads, many, ranges)

let shard_data dt shape f =
  let n = Array.fold_left ( * ) 1 shape in
  let size = Dtype.size dt in
  let b = Bytes.create (n * size) in
  for i = 0 to n - 1 do
    let v = f i in
    for k = 0 to size - 1 do
      Bytes.set b ((i * size) + k) (Char.chr ((v lsr (8 * k)) land 0xff))
    done
  done;
  Bytes.to_string b

let test_shard_round_trip () =
  List.iter
    (fun loc ->
      List.iter
        (fun dt ->
          let shape = [| 4; 4 |] in
          let data = shard_data dt shape (fun i -> i + 1) in
          let json = shard_json ~loc [| 2; 2 |] in
          let _, dec = round_trip ~dtype:dt ~shape json data in
          Alcotest.(check string)
            (Dtype.name dt ^ " " ^ loc)
            data dec)
        Dtype.[ Uint8; Uint16; Uint32; Uint64; Float32; Float64 ])
    [ "end"; "start" ]

let test_shard_omits_fill () =
  let shape = [| 4; 4 |] in
  let dt = Dtype.Uint8 in
  (* The top left 2 by 2 inner chunk is all fill value, so it is left
     out of the shard and its index entry is the absent sentinel. *)
  let data =
    shard_data dt shape (fun i ->
        let r = i / 4 and c = i mod 4 in
        if r < 2 && c < 2 then 0 else i + 1)
  in
  List.iter
    (fun loc ->
      let json = shard_json ~loc [| 2; 2 |] in
      let c = chain ~dtype:dt json in
      let enc = str (Codec.encode_chunk c (slab_of dt shape data)) in
      let e = shard_entries ~loc ~chunks:4 enc in
      Alcotest.(check int64) (loc ^ " absent offset") (-1L) e.(0);
      Alcotest.(check int64) (loc ^ " absent length") (-1L) e.(1);
      for i = 1 to 3 do
        Alcotest.(check bool)
          (Printf.sprintf "%s chunk %d present" loc i)
          true
          (not (Int64.equal e.(2 * i) (-1L)))
      done;
      (* Three inner chunks of four bytes each, plus a 68 byte index. *)
      Alcotest.(check int)
        (loc ^ " shard length")
        (12 + 68) (String.length enc);
      let first = Int64.to_int e.(2) in
      Alcotest.(check int)
        (loc ^ " first payload offset")
        (if loc = "start" then 68 else 0)
        first;
      let back =
        str
          (Slab.bigstring
             (Codec.decode_chunk c (repr dt shape) (bs enc)))
      in
      Alcotest.(check string) (loc ^ " decode") data back)
    [ "end"; "start" ]

let test_shard_all_fill () =
  let shape = [| 4; 4 |] in
  let dt = Dtype.Uint8 in
  let data = String.make 16 '\000' in
  let c = chain ~dtype:dt (shard_json [| 2; 2 |]) in
  let enc = str (Codec.encode_chunk c (slab_of dt shape data)) in
  Alcotest.(check int) "index only" 68 (String.length enc);
  Alcotest.(check string)
    "decodes as fill" data
    (str (Slab.bigstring (Codec.decode_chunk c (repr dt shape) (bs enc))))

let test_shard_partial () =
  let shape = [| 4; 4 |] in
  let dt = Dtype.Uint8 in
  let data = shard_data dt shape (fun i -> i + 1) in
  let c = chain ~dtype:dt (shard_json [| 2; 2 |]) in
  Alcotest.(check bool) "supports partial" true (Codec.supports_partial c);
  let enc = str (Codec.encode_chunk c (slab_of dt shape data)) in
  let entries = shard_entries ~loc:"end" ~chunks:4 enc in
  let src, reads, many, ranges = counting (Byte_source.of_bigstring (bs enc)) in
  let sub =
    { Subset.start = Ia.of_array [| 0; 0 |]; shape = Ia.of_array [| 2; 2 |] }
  in
  let out =
    match Codec.partial_decode c (repr dt shape) src sub with
    | Some s -> s
    | None -> Alcotest.fail "partial_decode declined"
  in
  Alcotest.(check int) "one index read" 1 !reads;
  Alcotest.(check int) "one batched read" 1 !many;
  Alcotest.(check int) "two ranges in all" 2 (List.length !ranges);
  (match !ranges with
  | [ Byte_range.Suffix 68; Byte_range.From_start { off; len = Some len } ] ->
      Alcotest.(check int) "chunk offset" (Int64.to_int entries.(0)) off;
      Alcotest.(check int) "chunk length" (Int64.to_int entries.(1)) len
  | _ -> Alcotest.fail "unexpected range sequence");
  Alcotest.(check string)
    "top left quadrant" "\001\002\005\006"
    (str (Slab.bigstring out));
  (* A subset spanning two inner chunks batches both, and one that lands
     inside an absent chunk needs no read at all. *)
  let src, reads, many, ranges =
    counting (Byte_source.of_bigstring (bs enc))
  in
  let sub =
    { Subset.start = Ia.of_array [| 1; 1 |]; shape = Ia.of_array [| 2; 2 |] }
  in
  let out = Option.get (Codec.partial_decode c (repr dt shape) src sub) in
  Alcotest.(check int) "index read" 1 !reads;
  Alcotest.(check int) "one batch" 1 !many;
  Alcotest.(check int) "four chunks plus index" 5 (List.length !ranges);
  Alcotest.(check string)
    "centre" "\006\007\010\011"
    (str (Slab.bigstring out))

let test_shard_partial_absent () =
  let shape = [| 4; 4 |] in
  let dt = Dtype.Uint8 in
  let data =
    shard_data dt shape (fun i ->
        let r = i / 4 and c = i mod 4 in
        if r < 2 && c < 2 then 0 else i + 1)
  in
  let c = chain ~dtype:dt (shard_json [| 2; 2 |]) in
  let enc = str (Codec.encode_chunk c (slab_of dt shape data)) in
  let src, reads, many, ranges = counting (Byte_source.of_bigstring (bs enc)) in
  let sub =
    { Subset.start = Ia.of_array [| 0; 0 |]; shape = Ia.of_array [| 2; 2 |] }
  in
  let out = Option.get (Codec.partial_decode c (repr dt shape) src sub) in
  Alcotest.(check int) "index read" 1 !reads;
  Alcotest.(check int) "no batch" 0 !many;
  Alcotest.(check int) "index only" 1 (List.length !ranges);
  Alcotest.(check string) "fill" "\000\000\000\000" (str (Slab.bigstring out))

let test_shard_nested () =
  let shape = [| 4; 4 |] in
  let dt = Dtype.Uint16 in
  let data = shard_data dt shape (fun i -> 1000 + i) in
  let inner = shard_json [| 1; 1 |] in
  let json = shard_json ~inner [| 2; 2 |] in
  let _, dec = round_trip ~dtype:dt ~shape json data in
  Alcotest.(check string) "nested shard" data dec

let test_shard_errors () =
  let c = chain ~dtype:Dtype.Uint8 (shard_json [| 3; 3 |]) in
  Alcotest.check_raises "chunk shape does not divide"
    (Error.E
       (Error.Codec
          "sharding_indexed: dimension 0 of the shard is 4, which 3 does not \
           divide"))
    (fun () ->
      ignore (Codec.encoded_size c (repr Dtype.Uint8 [| 4; 4 |])));
  Alcotest.(check string)
    "missing chunk_shape" "codec \"sharding_indexed\": chunk_shape is required"
    (chain_error ~dtype:Dtype.Uint8
       {|[{"name":"sharding_indexed","configuration":{}}]|});
  Alcotest.(check string)
    "bad index location"
    "codec \"sharding_indexed\": index_location \"middle\" is not \"start\" \
     or \"end\""
    (chain_error ~dtype:Dtype.Uint8 (shard_json ~loc:"middle" [| 2; 2 |]));
  (* A variable size index chain cannot be located by a ranged read. *)
  let c =
    chain ~dtype:Dtype.Uint8
      {|[{"name":"sharding_indexed","configuration":{
           "chunk_shape":[2,2],
           "codecs":[{"name":"bytes","configuration":{"endian":"little"}}],
           "index_codecs":[{"name":"bytes","configuration":{"endian":"little"}},
                           {"name":"gzip","configuration":{"level":1}}]}}]|}
  in
  Alcotest.check_raises "variable index"
    (Error.E
       (Error.Codec
          "sharding_indexed: index_codecs must have a fixed encoded size"))
    (fun () -> ignore (Codec.encoded_size c (repr Dtype.Uint8 [| 4; 4 |])))

let shard_tests =
  [
    ("round trip", `Quick, test_shard_round_trip);
    ("omits fill chunks", `Quick, test_shard_omits_fill);
    ("all fill", `Quick, test_shard_all_fill);
    ("partial decode", `Quick, test_shard_partial);
    ("partial decode of an absent chunk", `Quick, test_shard_partial_absent);
    ("nested", `Quick, test_shard_nested);
    ("errors", `Quick, test_shard_errors);
  ]

(* {1 Golden fixtures} *)

let fixture p = "../fixtures/" ^ p

let array_meta p =
  match Metadata.array_of_json (json_of_string (read_file (fixture p))) with
  | Ok m -> m
  | Error e -> Alcotest.failf "%s: %s" p e

let bind_meta (m : Metadata.array_meta) =
  let dt =
    match Dtype.of_name m.data_type.Ext.name with
    | Some d -> d
    | None -> Alcotest.failf "unknown data type %S" m.data_type.Ext.name
  in
  let fill =
    match Fill_value.of_json dt m.fill_value with
    | Ok f -> f
    | Error e -> Alcotest.failf "fill value: %s" e
  in
  let c =
    match Codec.chain_of_exts ~dtype:dt ~fill_value:fill m.codecs with
    | Ok c -> c
    | Error e -> Alcotest.failf "codecs: %s" e
  in
  let grid =
    match Chunk_grid.of_ext m.chunk_grid ~array_shape:m.shape with
    | Ok g -> g
    | Error e -> Alcotest.failf "chunk grid: %s" e
  in
  (dt, c, Chunk_grid.chunk_shape grid)

let f32 slab i =
  Int32.float_of_bits (I32u.to_int32 (F32u.to_bits (Slab.F32.get slab i)))

let u16 slab i = I16u.to_int (Slab.U16.get slab i) land 0xffff

(* The 10 by 10 float32 fixtures all hold [a.(i).(j) = 10 * i + j] in 5
   by 5 chunks, whatever their codec chain. *)
let check_10x10 dir key =
  let m = array_meta (dir ^ "/zarr.json") in
  Alcotest.(check (array int)) (dir ^ " shape") [| 10; 10 |] m.shape;
  let dt, c, cs = bind_meta m in
  Alcotest.(check string) (dir ^ " data type") "float32" (Dtype.name dt);
  Alcotest.(check (array int)) (dir ^ " chunk shape") [| 5; 5 |] cs;
  for ci = 0 to 1 do
    for cj = 0 to 1 do
      let buf = bs (read_file (fixture (dir ^ "/" ^ key ci cj))) in
      let slab = Codec.decode_chunk c (repr dt cs) buf in
      for i = 0 to 4 do
        for j = 0 to 4 do
          let want = float_of_int ((10 * ((5 * ci) + i)) + (5 * cj) + j) in
          Alcotest.(check (float 0.))
            (Printf.sprintf "%s c%d.%d [%d][%d]" dir ci cj i j)
            want
            (f32 slab ((i * 5) + j))
        done
      done
    done
  done

let v2_key ci cj = Printf.sprintf "%d.%d" ci cj
let default_key ci cj = Printf.sprintf "c/%d/%d" ci cj

let test_fixture_v3 () =
  List.iter
    (fun d -> check_10x10 ("v3/" ^ d ^ ".zarr") v2_key)
    [
      "array_none"; "array_gzip"; "array_zstd"; "array_none_transpose";
      "array_blosc"; "array_blosc_transpose";
    ]

let test_fixture_zarr_python () =
  List.iter
    (fun d -> check_10x10 ("v3_zarr_python/" ^ d ^ ".zarr") default_key)
    [ "array_none"; "array_gzip"; "array_zstd" ]

(* The transposed fixture stores each chunk column major, so its raw
   bytes differ from the plain one while its values do not. *)
let test_fixture_transpose_bytes () =
  let plain = read_file (fixture "v3/array_none.zarr/0.0") in
  let tr = read_file (fixture "v3/array_none_transpose.zarr/0.0") in
  Alcotest.(check int) "same length" (String.length plain) (String.length tr);
  Alcotest.(check bool) "different bytes" true (plain <> tr);
  let want = Bytes.create 100 in
  for i = 0 to 4 do
    for j = 0 to 4 do
      Bytes.blit_string plain (4 * ((i * 5) + j)) want (4 * ((j * 5) + i)) 4
    done
  done;
  Alcotest.(check string) "column major on disk" (Bytes.to_string want) tr

let test_fixture_shard () =
  let dir = "sharded_array_write_read.zarr/group/array" in
  let m = array_meta (dir ^ "/zarr.json") in
  Alcotest.(check (array int)) "shape" [| 8; 8 |] m.shape;
  let dt, c, cs = bind_meta m in
  Alcotest.(check string) "data type" "uint16" (Dtype.name dt);
  Alcotest.(check (array int)) "shard shape" [| 4; 8 |] cs;
  Alcotest.(check bool) "supports partial" true (Codec.supports_partial c);
  for s = 0 to 1 do
    let buf = bs (read_file (fixture (Printf.sprintf "%s/c/%d/0" dir s))) in
    let slab = Codec.decode_chunk c (repr dt cs) buf in
    for i = 0 to 3 do
      for j = 0 to 7 do
        Alcotest.(check int)
          (Printf.sprintf "shard %d [%d][%d]" s i j)
          ((32 * s) + (8 * i) + j)
          (u16 slab ((i * 8) + j))
      done
    done
  done;
  (* One inner chunk of the first shard, read through ranges alone. *)
  let buf = bs (read_file (fixture (dir ^ "/c/0/0"))) in
  let src, reads, many, ranges = counting (Byte_source.of_bigstring buf) in
  let sub =
    { Subset.start = Ia.of_array [| 0; 4 |]; shape = Ia.of_array [| 4; 4 |] }
  in
  let out = Option.get (Codec.partial_decode c (repr dt cs) src sub) in
  Alcotest.(check int) "one index read" 1 !reads;
  Alcotest.(check int) "one batched read" 1 !many;
  Alcotest.(check int) "two ranges" 2 (List.length !ranges);
  (match List.hd !ranges with
  | Byte_range.Suffix 36 -> ()
  | _ -> Alcotest.fail "the index is not a 36 byte suffix");
  for i = 0 to 3 do
    for j = 0 to 3 do
      Alcotest.(check int)
        (Printf.sprintf "right half [%d][%d]" i j)
        ((8 * i) + j + 4)
        (u16 out ((i * 4) + j))
    done
  done

(* Every fixture chunk re-encodes to its own bytes under the chains
   whose encoding is deterministic. *)
let test_fixture_reencode () =
  List.iter
    (fun (dir, key) ->
      let m = array_meta (dir ^ "/zarr.json") in
      let dt, c, cs = bind_meta m in
      for ci = 0 to 1 do
        for cj = 0 to 1 do
          let want = read_file (fixture (dir ^ "/" ^ key ci cj)) in
          let slab = Codec.decode_chunk c (repr dt cs) (bs want) in
          Alcotest.(check string)
            (Printf.sprintf "%s %d.%d" dir ci cj)
            want
            (str (Codec.encode_chunk c slab))
        done
      done)
    [
      ("v3/array_none.zarr", v2_key);
      ("v3/array_none_transpose.zarr", v2_key);
      ("v3_zarr_python/array_none.zarr", default_key);
    ]

(* A chunk of a real store rather than of a test fixture: the band
   coordinate of the Tessera embeddings, whose chain is [bytes] little
   endian then [blosc] with zstd inside and a byte shuffle over its four
   byte elements. The 128 values are the band indices themselves, 0 to
   127, which is what a coordinate array of a 128 band embedding holds. *)
let test_fixture_tessera () =
  let dir = "tessera_band" in
  let m = array_meta (dir ^ "/zarr.json") in
  Alcotest.(check (array int)) "shape" [| 128 |] m.shape;
  let dt, c, cs = bind_meta m in
  Alcotest.(check string) "data type" "int32" (Dtype.name dt);
  Alcotest.(check (array int)) "chunk shape" [| 128 |] cs;
  Alcotest.(check (list string))
    "chain" [ "blosc" ]
    (List.filter_map
       (fun (e : Ext.t) -> if e.name = "blosc" then Some e.name else None)
       (Codec.chain_exts c));
  let buf = bs (read_file (fixture (dir ^ "/c/0"))) in
  let slab = Codec.decode_chunk c (repr dt cs) buf in
  Alcotest.(check int) "elements" 128 (Slab.num_elements slab);
  for i = 0 to 127 do
    Alcotest.(check int)
      (Printf.sprintf "band %d" i)
      i
      (Int32.to_int (I32u.to_int32 (Slab.I32.get slab i)))
  done

let fixture_tests =
  [
    ("zarrs v3", `Quick, test_fixture_v3);
    ("tessera band", `Quick, test_fixture_tessera);
    ("zarr python v3", `Quick, test_fixture_zarr_python);
    ("transposed bytes", `Quick, test_fixture_transpose_bytes);
    ("sharded", `Quick, test_fixture_shard);
    ("re-encode", `Quick, test_fixture_reencode);
  ]

let () =
  Random.init 20260824;
  Alcotest.run "zarrz codec"
    [
      ("bytes", bytes_tests);
      ("transpose", transpose_tests);
      ("compressors", compressor_tests);
      ("blosc", blosc_tests);
      ("crc32c", crc32c_tests);
      ("chain", chain_tests);
      ("sharding", shard_tests);
      ("fixtures", fixture_tests);
    ]
