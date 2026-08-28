(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Metadata layer tests. Expectations come from the zarrs oracle, see
   ../fixtures/README.md. *)

open Zarrz

let json_of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error m -> Alcotest.failf "test JSON is invalid: %s" m

let string_of_json j =
  match Jsont_bytesrw.encode_string Jsont.json j with
  | Ok s -> s
  | Error m -> Alcotest.failf "cannot encode JSON: %s" m

let read_file p =
  let ic = open_in_bin p in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let json = Alcotest.testable Jsont.Json.pp Jsont.Json.equal
let ext = Alcotest.testable Ext.pp Ext.equal
let ints = Alcotest.(array int)

let fixture name = json_of_string (read_file ("../fixtures/" ^ name))

(* Bytes as lowercase hex, so that a failure names the bytes. *)
let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.init
    (String.length s / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

(* Fill value expectations are written big endian. Element images are
   native endian, so reverse each scalar component on a little endian
   host. *)
let native_of_be dt s =
  let comp =
    match dt with
    | Dtype.Complex64 -> 4
    | Dtype.Complex128 -> 8
    | Dtype.Bool | Dtype.Raw _ -> 1
    | d -> Dtype.size d
  in
  if Sys.big_endian || comp = 1 then s
  else
    String.concat ""
      (List.init
         (String.length s / comp)
         (fun i ->
           let c = String.sub s (i * comp) comp in
           String.init comp (fun k -> c.[comp - 1 - k])))

(* Ext *)

let ext_of_string s = Jsont_bytesrw.decode_string Ext.jsont s

let ext_dec name s expect =
  ( "decode " ^ name,
    `Quick,
    fun () ->
      match ext_of_string s with
      | Ok e -> Alcotest.check ext s expect e
      | Error m -> Alcotest.failf "%s: %s" s m )

let ext_dec_fails name s =
  ( "decode " ^ name,
    `Quick,
    fun () ->
      match ext_of_string s with
      | Ok _ -> Alcotest.failf "%s: expected a decode error" s
      | Error _ -> () )

let ext_enc name e expect =
  ( "encode " ^ name,
    `Quick,
    fun () ->
      match Jsont.Json.encode Ext.jsont e with
      | Ok j -> Alcotest.check json expect (json_of_string expect) j
      | Error m -> Alcotest.failf "%s: %s" expect m )

let obj mems = Some (json_of_string mems)

let ext_tests =
  [
    ext_dec "bare string" {|"bytes"|} (Ext.v "bytes");
    ext_dec "name only object" {|{"name":"bytes"}|} (Ext.v "bytes");
    ext_dec "empty configuration"
      {|{"name":"bytes","configuration":{}}|}
      (Ext.v "bytes" ?config:(obj "{}"));
    ext_dec "object with configuration"
      {|{"name":"gzip","configuration":{"level":5}}|}
      (Ext.v "gzip" ?config:(obj {|{"level":5}|}));
    ext_dec "must_understand true"
      {|{"name":"x","must_understand":true}|}
      (Ext.v "x");
    ext_dec "must_understand false"
      {|{"name":"x","must_understand":false}|}
      (Ext.v "x" ~must_understand:false);
    ext_dec "null configuration"
      {|{"name":"x","configuration":null}|}
      (Ext.v "x");
    ext_dec_fails "unknown member" {|{"name":"x","other":1}|};
    ext_dec_fails "missing name" {|{"configuration":{}}|};
    ext_dec_fails "non object configuration" {|{"name":"x","configuration":1}|};
    ext_dec_fails "number" {|1|};
    ext_dec_fails "empty bare name" {|""|};
    ext_dec_fails "empty object name" {|{"name":""}|};
    ext_enc "no configuration is a bare string" (Ext.v "bytes") {|"bytes"|};
    ext_enc "empty configuration keeps the object"
      (Ext.v "bytes" ?config:(obj "{}"))
      {|{"name":"bytes"}|};
    ext_enc "configuration"
      (Ext.v "gzip" ?config:(obj {|{"level":5}|}))
      {|{"name":"gzip","configuration":{"level":5}}|};
    ext_enc "must_understand false is written"
      (Ext.v "gzip" ~must_understand:false ?config:(obj {|{"level":5}|}))
      {|{"name":"gzip","configuration":{"level":5},"must_understand":false}|};
    ext_enc "must_understand true is omitted"
      (Ext.v "gzip" ~must_understand:true ?config:(obj {|{"level":5}|}))
      {|{"name":"gzip","configuration":{"level":5}}|};
    (* A short form means must_understand true, so a false one keeps the
       object form whatever the configuration is. The zarrs oracle drops
       the member in both of these cases, which promotes the extension
       to must_understand true. *)
    ext_enc "empty configuration keeps must_understand"
      (Ext.v "x" ~must_understand:false ?config:(obj "{}"))
      {|{"name":"x","configuration":{},"must_understand":false}|};
    ext_enc "no configuration keeps must_understand"
      (Ext.v "x" ~must_understand:false)
      {|{"name":"x","must_understand":false}|};
    ( "must_understand false round trips",
      `Quick,
      fun () ->
        let round e =
          match Jsont.Json.encode Ext.jsont e with
          | Error m -> Alcotest.failf "%s" m
          | Ok j -> (
              match Jsont.Json.decode Ext.jsont j with
              | Ok e -> e
              | Error m -> Alcotest.failf "%s" m)
        in
        let e = Ext.v "x" ~must_understand:false in
        Alcotest.check ext "no configuration" e (round e);
        let e = Ext.v "x" ~must_understand:false ?config:(obj "{}") in
        Alcotest.check ext "empty configuration" e (round e) );
    ( "config_mem",
      `Quick,
      fun () ->
        let e = Ext.v "gzip" ?config:(obj {|{"level":5}|}) in
        Alcotest.(check bool)
          "level" true
          (match Ext.config_mem e "level" with
          | Some (Jsont.Number (5.0, _)) -> true
          | _ -> false);
        Alcotest.(check bool)
          "absent" true
          (Ext.config_mem e "other" = None);
        Alcotest.(check bool)
          "no configuration" true
          (Ext.config_mem (Ext.v "gzip") "level" = None) );
  ]

(* Array and group metadata *)

let array_doc ?(format = "3") ?(node = {|"array"|}) ?(shape = ",\"shape\":[4]")
    ?(dtype = {|"float64"|}) ?(fill = "0") ?(codecs = {|["bytes"]|})
    ?(extra = "") () =
  String.concat ""
    [
      {|{"zarr_format":|};
      format;
      {|,"node_type":|};
      node;
      shape;
      {|,"data_type":|};
      dtype;
      {|,"chunk_grid":{"name":"regular",|};
      {|"configuration":{"chunk_shape":[2]}}|};
      {|,"chunk_key_encoding":"default"|};
      {|,"fill_value":|};
      fill;
      {|,"codecs":|};
      codecs;
      extra;
      "}";
    ]

let decode_array s = Jsont_bytesrw.decode_string Metadata.array_jsont s
let decode_group s = Jsont_bytesrw.decode_string Metadata.group_jsont s

let ok_array s =
  match decode_array s with
  | Ok m -> m
  | Error m -> Alcotest.failf "expected a decode: %s" m

let test_array_fixture () =
  let m = Metadata.array_of_json (fixture "array_metadata.json") in
  let m = match m with Ok m -> m | Error e -> Alcotest.failf "%s" e in
  Alcotest.check ints "shape" [| 10000; 1000 |] m.Metadata.shape;
  Alcotest.check ext "data_type" (Ext.v "float64") m.Metadata.data_type;
  Alcotest.check ext "chunk_grid"
    (Ext.v "regular" ?config:(obj {|{"chunk_shape":[1000,100]}|}))
    m.Metadata.chunk_grid;
  Alcotest.check ext "chunk_key_encoding"
    (Ext.v "default" ?config:(obj {|{"separator":"/"}|}))
    m.Metadata.chunk_key_encoding;
  Alcotest.check json "fill_value" (json_of_string {|"NaN"|})
    m.Metadata.fill_value;
  Alcotest.(check int) "codec count" 2 (List.length m.Metadata.codecs);
  Alcotest.check ext "codec 0"
    (Ext.v "bytes" ?config:(obj {|{"endian":"little"}|}))
    (List.nth m.Metadata.codecs 0);
  Alcotest.check ext "codec 1"
    (Ext.v "gzip" ?config:(obj {|{"level":1}|}))
    (List.nth m.Metadata.codecs 1);
  Alcotest.check json "attributes"
    (json_of_string {|{"foo":42,"bar":"apples","baz":[1,2,3,4]}|})
    (Option.get m.Metadata.attributes);
  Alcotest.(check (option (list (option string))))
    "dimension_names"
    (Some [ Some "rows"; Some "columns" ])
    m.Metadata.dimension_names;
  Alcotest.(check int)
    "storage_transformers" 0
    (List.length m.Metadata.storage_transformers);
  Alcotest.(check int) "unknown" 0 (List.length m.Metadata.unknown)

let test_array_round_trip () =
  let src = fixture "array_metadata.json" in
  let m = Result.get_ok (Metadata.array_of_json src) in
  let out = Metadata.array_to_json m in
  Alcotest.check json "encode matches the source" src out;
  let m' = Result.get_ok (Metadata.array_of_json out) in
  Alcotest.check json "second encode" out (Metadata.array_to_json m')

let test_codec_forms () =
  let codecs = {|["bytes",{"name":"gzip","configuration":{"level":5}}]|} in
  let doc = array_doc ~codecs () in
  let m = ok_array doc in
  Alcotest.check ext "bare string codec" (Ext.v "bytes")
    (List.nth m.Metadata.codecs 0);
  Alcotest.check ext "object codec"
    (Ext.v "gzip" ?config:(obj {|{"level":5}|}))
    (List.nth m.Metadata.codecs 1);
  Alcotest.check json "round trip" (json_of_string doc)
    (Metadata.array_to_json m)

let test_dimension_names_null () =
  let doc =
    array_doc ~shape:{|,"shape":[4,4]|}
      ~extra:{|,"dimension_names":["x",null]|} ()
  in
  let m = ok_array doc in
  Alcotest.(check (option (list (option string))))
    "names"
    (Some [ Some "x"; None ])
    m.Metadata.dimension_names;
  Alcotest.check json "round trip" (json_of_string doc)
    (Metadata.array_to_json m)

(* The spec requires one dimension name per dimension of the shape, and
   a codec list that is not empty. *)
let test_array_list_lengths () =
  Alcotest.(check bool)
    "too few dimension names" true
    (Result.is_error
       (decode_array
          (array_doc ~shape:{|,"shape":[4,4]|}
             ~extra:{|,"dimension_names":["x"]|} ())));
  Alcotest.(check bool)
    "too many dimension names" true
    (Result.is_error
       (decode_array
          (array_doc ~extra:{|,"dimension_names":["x",null]|} ())));
  Alcotest.(check bool)
    "a matching length decodes" true
    (Result.is_ok
       (decode_array (array_doc ~extra:{|,"dimension_names":["x"]|} ())));
  Alcotest.(check bool)
    "empty codecs" true
    (Result.is_error (decode_array (array_doc ~codecs:"[]" ())))

let test_unknown_members () =
  let understood =
    array_doc ~extra:{|,"x":{"k":1,"must_understand":false}|} ()
  in
  let m = ok_array understood in
  Alcotest.(check int) "kept" 1 (List.length m.Metadata.unknown);
  Alcotest.check json "round trip" (json_of_string understood)
    (Metadata.array_to_json m);
  let must = array_doc ~extra:{|,"x":{"k":1,"must_understand":true}|} () in
  Alcotest.(check bool)
    "must_understand true rejected" true
    (Result.is_error (decode_array must));
  let implicit = array_doc ~extra:{|,"x":{"k":1}|} () in
  Alcotest.(check bool)
    "implicit must_understand rejected" true
    (Result.is_error (decode_array implicit));
  let scalar = array_doc ~extra:{|,"x":"test"|} () in
  Alcotest.(check bool)
    "non object rejected" true
    (Result.is_error (decode_array scalar));
  let arr = array_doc ~extra:{|,"x":[]|} () in
  Alcotest.(check bool)
    "array rejected" true
    (Result.is_error (decode_array arr))

let test_array_header () =
  Alcotest.(check bool)
    "zarr_format 2 rejected" true
    (Result.is_error (decode_array (array_doc ~format:"2" ())));
  Alcotest.(check bool)
    "node_type group rejected" true
    (Result.is_error (decode_array (array_doc ~node:{|"group"|} ())));
  Alcotest.(check bool)
    "missing shape rejected" true
    (Result.is_error (decode_array (array_doc ~shape:"" ())));
  (* Jsont.int truncates and coerces, so the strict decoder must catch
     what it would let through. *)
  Alcotest.(check bool)
    "fractional zarr_format rejected" true
    (Result.is_error (decode_array (array_doc ~format:"3.9" ())));
  Alcotest.(check bool)
    "string zarr_format rejected" true
    (Result.is_error (decode_array (array_doc ~format:{|"3"|} ())));
  Alcotest.(check bool)
    "fractional shape element rejected" true
    (Result.is_error
       (decode_array (array_doc ~shape:{|,"shape":[1.9,4]|} ())));
  Alcotest.(check bool)
    "string shape element rejected" true
    (Result.is_error
       (decode_array (array_doc ~shape:{|,"shape":["2",4]|} ())))

let test_group_fixture () =
  let src = fixture "group_metadata.json" in
  let m = Result.get_ok (Metadata.group_of_json src) in
  Alcotest.check json "attributes"
    (json_of_string {|{"spam":"ham","eggs":42}|})
    (Option.get m.Metadata.group_attributes);
  Alcotest.(check int) "unknown" 0 (List.length m.Metadata.group_unknown);
  Alcotest.check json "round trip" src (Metadata.group_to_json m)

let test_group_consolidated_null () =
  let doc =
    {|{"attributes":{},"zarr_format":3,|}
    ^ {|"consolidated_metadata":null,"node_type":"group"}|}
  in
  let m =
    match decode_group doc with
    | Ok m -> m
    | Error e -> Alcotest.failf "%s" e
  in
  Alcotest.(check int)
    "consolidated_metadata dropped" 0
    (List.length m.Metadata.group_unknown);
  Alcotest.check json "round trip"
    (json_of_string {|{"zarr_format":3,"node_type":"group","attributes":{}}|})
    (Metadata.group_to_json m);
  Alcotest.(check bool)
    "a non null consolidated_metadata is still checked" true
    (Result.is_error
       (decode_group
          {|{"zarr_format":3,"node_type":"group","consolidated_metadata":{}}|}))

let test_group_minimal () =
  let m = Result.get_ok (Metadata.group_of_json
    (json_of_string {|{"zarr_format":3,"node_type":"group"}|})) in
  Alcotest.(check bool)
    "no attributes" true
    (m.Metadata.group_attributes = None);
  Alcotest.check json "round trip"
    (json_of_string {|{"zarr_format":3,"node_type":"group"}|})
    (Metadata.group_to_json m)

let metadata_tests =
  [
    ("array fixture", `Quick, test_array_fixture);
    ("array round trip", `Quick, test_array_round_trip);
    ("codec forms", `Quick, test_codec_forms);
    ("dimension names null", `Quick, test_dimension_names_null);
    ("list lengths", `Quick, test_array_list_lengths);
    ("unknown members", `Quick, test_unknown_members);
    ("array header", `Quick, test_array_header);
    ("group fixture", `Quick, test_group_fixture);
    ("group consolidated_metadata null", `Quick, test_group_consolidated_null);
    ("group minimal", `Quick, test_group_minimal);
  ]

(* Fill values *)

let fv_dec name dt src be =
  ( Printf.sprintf "decode %s %s" (Dtype.name dt) name,
    `Quick,
    fun () ->
      match Fill_value.of_json dt (json_of_string src) with
      | Ok v ->
          Alcotest.(check string)
            src
            (hex (native_of_be dt (unhex be)))
            (hex (Fill_value.to_bytes v))
      | Error m -> Alcotest.failf "%s: %s" src m )

let fv_dec_fails name dt src =
  ( Printf.sprintf "reject %s %s" (Dtype.name dt) name,
    `Quick,
    fun () ->
      match Fill_value.of_json dt (json_of_string src) with
      | Ok v ->
          Alcotest.failf "%s: expected an error, got %s" src
            (hex (Fill_value.to_bytes v))
      | Error _ -> () )

let fv_enc name dt be expect =
  ( Printf.sprintf "encode %s %s" (Dtype.name dt) name,
    `Quick,
    fun () ->
      let v = Fill_value.of_bytes (native_of_be dt (unhex be)) in
      Alcotest.(check string) be expect
        (string_of_json (Fill_value.to_json dt v))
  )

(* A decode followed by an encode returns the canonical spelling. *)
let fv_round name dt src expect =
  ( Printf.sprintf "round %s %s" (Dtype.name dt) name,
    `Quick,
    fun () ->
      match Fill_value.of_json dt (json_of_string src) with
      | Ok v ->
          Alcotest.(check string) src expect
            (string_of_json (Fill_value.to_json dt v))
      | Error m -> Alcotest.failf "%s: %s" src m )

let fill_value_tests =
  [
    (* bool *)
    fv_dec "true" Dtype.Bool "true" "01";
    fv_dec "false" Dtype.Bool "false" "00";
    fv_enc "true" Dtype.Bool "01" "true";
    fv_enc "false" Dtype.Bool "00" "false";
    fv_dec_fails "number" Dtype.Bool "1";
    fv_dec_fails "string" Dtype.Bool {|"true"|};
    (* integers *)
    fv_dec "positive" Dtype.Int8 "7" "07";
    fv_dec "negative" Dtype.Int8 "-7" "f9";
    fv_dec "min" Dtype.Int8 "-128" "80";
    fv_dec "max" Dtype.Int8 "127" "7f";
    fv_dec_fails "above max" Dtype.Int8 "128";
    fv_dec_fails "below min" Dtype.Int8 "-129";
    fv_dec_fails "fraction" Dtype.Int8 "1.5";
    fv_dec "max" Dtype.Uint8 "255" "ff";
    fv_dec_fails "negative" Dtype.Uint8 "-1";
    fv_dec_fails "above max" Dtype.Uint8 "256";
    fv_dec "value" Dtype.Int16 "-2" "fffe";
    fv_dec_fails "above max" Dtype.Int16 "32768";
    fv_dec "max" Dtype.Uint16 "65535" "ffff";
    fv_dec "value" Dtype.Int32 "-2" "fffffffe";
    fv_dec_fails "above max" Dtype.Int32 "2147483648";
    fv_dec "max" Dtype.Uint32 "4294967295" "ffffffff";
    fv_dec_fails "above max" Dtype.Uint32 "4294967296";
    fv_dec "value" Dtype.Int64 "-2" "fffffffffffffffe";
    fv_dec "2^53" Dtype.Int64 "9007199254740992" "0020000000000000";
    fv_dec_fails "above 2^53" Dtype.Int64 "9007199254740994";
    fv_dec "2^53" Dtype.Uint64 "9007199254740992" "0020000000000000";
    fv_dec_fails "above 2^53" Dtype.Uint64 "18446744073709551615";
    fv_enc "signed" Dtype.Int8 "f9" "-7";
    fv_enc "unsigned" Dtype.Uint8 "f9" "249";
    fv_enc "signed" Dtype.Int64 "fffffffffffffffe" "-2";
    fv_enc "unsigned" Dtype.Uint64 "0020000000000000" "9007199254740992";
    (* float64 *)
    fv_dec "number" Dtype.Float64 "7.5" "401e000000000000";
    fv_dec "infinity" Dtype.Float64 {|"Infinity"|} "7ff0000000000000";
    fv_dec "-infinity" Dtype.Float64 {|"-Infinity"|} "fff0000000000000";
    fv_dec "nan" Dtype.Float64 {|"NaN"|} "7ff8000000000000";
    fv_dec "hex lower" Dtype.Float64 {|"0x3ff0000000000000"|}
      "3ff0000000000000";
    fv_dec "hex upper" Dtype.Float64 {|"0X3FF0000000000000"|}
      "3ff0000000000000";
    fv_dec_fails "hex too short" Dtype.Float64 {|"0x3ff00000"|};
    fv_dec_fails "hex too long" Dtype.Float64 {|"0x3ff000000000000000"|};
    fv_dec_fails "not hex" Dtype.Float64 {|"0xzz00000000000000"|};
    fv_dec_fails "no prefix" Dtype.Float64 {|"3ff0000000000000"|};
    fv_enc "number" Dtype.Float64 "401e000000000000" "7.5";
    fv_enc "integral number" Dtype.Float64 "4024000000000000" "10";
    fv_enc "infinity" Dtype.Float64 "7ff0000000000000" {|"Infinity"|};
    fv_enc "-infinity" Dtype.Float64 "fff0000000000000" {|"-Infinity"|};
    fv_enc "canonical nan" Dtype.Float64 "7ff8000000000000" {|"NaN"|};
    fv_enc "non canonical nan" Dtype.Float64 "7ff8000000000001"
      {|"0x7ff8000000000001"|};
    fv_enc "negative nan" Dtype.Float64 "fff8000000000000"
      {|"0xfff8000000000000"|};
    (* float32 *)
    fv_dec "number" Dtype.Float32 "1.5" "3fc00000";
    fv_dec "nan" Dtype.Float32 {|"NaN"|} "7fc00000";
    fv_dec "hex one" Dtype.Float32 {|"0x3F800000"|} "3f800000";
    fv_dec_fails "hex too long" Dtype.Float32 {|"0x3ff0000000000000"|};
    fv_enc "canonical nan" Dtype.Float32 "7fc00000" {|"NaN"|};
    fv_enc "non canonical nan" Dtype.Float32 "7fc00001" {|"0x7fc00001"|};
    fv_enc "number" Dtype.Float32 "3fc00000" "1.5";
    (* float16 *)
    fv_dec "half" Dtype.Float16 "0.5" "3800";
    fv_dec "one" Dtype.Float16 "1.0" "3c00";
    fv_dec "minus two" Dtype.Float16 "-2.0" "c000";
    fv_dec "infinity" Dtype.Float16 {|"Infinity"|} "7c00";
    fv_dec "nan" Dtype.Float16 {|"NaN"|} "7e00";
    fv_dec "hex" Dtype.Float16 {|"0x3c00"|} "3c00";
    fv_dec_fails "hex too long" Dtype.Float16 {|"0x3c000000"|};
    fv_dec "overflow to infinity" Dtype.Float16 "1e300" "7c00";
    fv_dec "underflow to zero" Dtype.Float16 "1e-300" "0000";
    fv_dec "subnormal" Dtype.Float16 "5.9604644775390625e-8" "0001";
    fv_dec "ties to even rounds down" Dtype.Float16 "1.00048828125" "3c00";
    fv_dec "ties to even rounds up" Dtype.Float16 "1.00146484375" "3c02";
    fv_enc "half" Dtype.Float16 "3800" "0.5";
    fv_enc "one" Dtype.Float16 "3c00" "1";
    fv_enc "minus two" Dtype.Float16 "c000" "-2";
    fv_enc "infinity" Dtype.Float16 "7c00" {|"Infinity"|};
    fv_enc "canonical nan" Dtype.Float16 "7e00" {|"NaN"|};
    fv_enc "non canonical nan" Dtype.Float16 "7e01" {|"0x7e01"|};
    fv_round "half" Dtype.Float16 "0.5" "0.5";
    fv_round "one" Dtype.Float16 "1.0" "1";
    fv_round "minus two" Dtype.Float16 "-2.0" "-2";
    fv_round "infinity" Dtype.Float16 {|"Infinity"|} {|"Infinity"|};
    fv_round "nan" Dtype.Float16 {|"NaN"|} {|"NaN"|};
    (* bfloat16 *)
    fv_dec "half" Dtype.Bfloat16 "0.5" "3f00";
    fv_dec "one" Dtype.Bfloat16 "1.0" "3f80";
    fv_dec "minus two" Dtype.Bfloat16 "-2.0" "c000";
    fv_dec "infinity" Dtype.Bfloat16 {|"Infinity"|} "7f80";
    fv_dec "nan" Dtype.Bfloat16 {|"NaN"|} "7fc0";
    fv_dec "hex" Dtype.Bfloat16 {|"0x3f80"|} "3f80";
    fv_enc "canonical nan" Dtype.Bfloat16 "7fc0" {|"NaN"|};
    fv_enc "non canonical nan" Dtype.Bfloat16 "7fc1" {|"0x7fc1"|};
    fv_round "half" Dtype.Bfloat16 "0.5" "0.5";
    fv_round "one" Dtype.Bfloat16 "1.0" "1";
    fv_round "minus two" Dtype.Bfloat16 "-2.0" "-2";
    fv_round "infinity" Dtype.Bfloat16 {|"Infinity"|} {|"Infinity"|};
    fv_round "nan" Dtype.Bfloat16 {|"NaN"|} {|"NaN"|};
    (* complex *)
    fv_dec "pair" Dtype.Complex64 {|["0x3F800000","NaN"]|} "3f8000007fc00000";
    fv_dec "numbers" Dtype.Complex64 "[1.5,-1.5]" "3fc00000bfc00000";
    fv_dec_fails "one element" Dtype.Complex64 "[1.5]";
    fv_dec_fails "three elements" Dtype.Complex64 "[1.5,1.5,1.5]";
    fv_dec_fails "not an array" Dtype.Complex64 "1.5";
    fv_dec "numbers" Dtype.Complex128 {|[1.5,"Infinity"]|}
      "3ff80000000000007ff0000000000000";
    fv_enc "pair" Dtype.Complex64 "3f8000007fc00000" {|[1,"NaN"]|};
    fv_enc "pair" Dtype.Complex128 "3ff80000000000007ff0000000000000"
      {|[1.5,"Infinity"]|};
    (* raw *)
    fv_dec "array" (Dtype.Raw 3) "[0,1,255]" "0001ff";
    fv_dec_fails "short array" (Dtype.Raw 3) "[0,1]";
    fv_dec_fails "long array" (Dtype.Raw 3) "[0,1,2,3]";
    fv_dec_fails "byte out of range" (Dtype.Raw 3) "[0,1,256]";
    fv_dec_fails "fraction" (Dtype.Raw 3) "[0,1,1.5]";
    fv_dec "base64" (Dtype.Raw 3) {|"AAH/"|} "0001ff";
    fv_dec "base64 padded" (Dtype.Raw 4) {|"AAECAw=="|} "00010203";
    fv_dec_fails "base64 wrong length" (Dtype.Raw 3) {|"AAECAw=="|};
    fv_dec_fails "not base64" (Dtype.Raw 3) {|"!!!"|};
    fv_dec_fails "base64 unpadded" (Dtype.Raw 3) {|"AAH"|};
    fv_dec_fails "base64 with whitespace" (Dtype.Raw 3) {|"AA H/"|};
    fv_dec_fails "base64 padding in the middle" (Dtype.Raw 6) {|"AAH=AAH/"|};
    fv_dec_fails "base64 empty" (Dtype.Raw 3) {|""|};
    fv_enc "array" (Dtype.Raw 3) "0001ff" "[0,1,255]";
    fv_round "base64 encodes as an array" (Dtype.Raw 3) {|"AAH/"|} "[0,1,255]";
  ]

(* Chunk grid *)

let grid array_shape chunk_shape =
  match Chunk_grid.v ~array_shape ~chunk_shape with
  | Ok g -> g
  | Error m -> Alcotest.failf "%s" m

let test_grid_shape () =
  Alcotest.check ints "exact"
    [| 2; 2 |]
    (Chunk_grid.grid_shape (grid [| 10; 10 |] [| 5; 5 |]));
  Alcotest.check ints "ragged"
    [| 3; 1 |]
    (Chunk_grid.grid_shape (grid [| 11; 5 |] [| 5; 5 |]));
  Alcotest.check ints "zero dimension"
    [| 0; 4 |]
    (Chunk_grid.grid_shape (grid [| 0; 20 |] [| 3; 5 |]));
  Alcotest.check ints "zero dimensional" [||]
    (Chunk_grid.grid_shape (grid [||] [||]))

let test_grid_errors () =
  Alcotest.(check bool)
    "dimension mismatch" true
    (Result.is_error
       (Chunk_grid.v ~array_shape:[| 10 |] ~chunk_shape:[| 5; 5 |]));
  Alcotest.(check bool)
    "zero chunk length" true
    (Result.is_error (Chunk_grid.v ~array_shape:[| 10 |] ~chunk_shape:[| 0 |]));
  Alcotest.(check bool)
    "negative array length" true
    (Result.is_error (Chunk_grid.v ~array_shape:[| -1 |] ~chunk_shape:[| 5 |]));
  let g = grid [| 10; 10 |] [| 5; 5 |] in
  Alcotest.check_raises "wrong dimensionality" (Invalid_argument
    "Chunk_grid: chunk index has 1 dimensions, the grid has 2")
    (fun () -> ignore (Chunk_grid.chunk_origin g [| 0 |]))

let test_grid_arithmetic () =
  let g = grid [| 11; 5 |] [| 5; 5 |] in
  Alcotest.check ints "origin" [| 10; 0 |]
    (Chunk_grid.chunk_origin g [| 2; 0 |]);
  Alcotest.check ints "indices" [| 2; 0 |]
    (Chunk_grid.chunk_indices g [| 10; 4 |]);
  Alcotest.check ints "indices low" [| 0; 0 |]
    (Chunk_grid.chunk_indices g [| 4; 4 |])

let test_grid_clip () =
  let g = grid [| 11; 5 |] [| 5; 5 |] in
  (match Chunk_grid.clip g [| 0; 0 |] with
  | Some (start, shape) ->
      Alcotest.check ints "interior start" [| 0; 0 |] start;
      Alcotest.check ints "interior shape" [| 5; 5 |] shape
  | None -> Alcotest.fail "interior chunk is absent");
  (match Chunk_grid.clip g [| 2; 0 |] with
  | Some (start, shape) ->
      Alcotest.check ints "edge start" [| 10; 0 |] start;
      Alcotest.check ints "edge shape" [| 1; 5 |] shape
  | None -> Alcotest.fail "edge chunk is absent");
  Alcotest.(check bool)
    "outside the grid" true
    (Chunk_grid.clip g [| 3; 0 |] = None);
  let z = grid [| 0; 20 |] [| 3; 5 |] in
  Alcotest.(check bool)
    "zero length dimension" true
    (Chunk_grid.clip z [| 0; 0 |] = None)

let collect g ~start ~shape =
  let acc = ref [] in
  Chunk_grid.chunks_overlapping g ~start ~shape (fun i ->
      acc := Array.to_list i :: !acc);
  List.rev !acc

let test_grid_overlap () =
  let g = grid [| 11; 5 |] [| 5; 5 |] in
  Alcotest.(check (list (list int)))
    "straddling"
    [ [ 0; 0 ]; [ 1; 0 ] ]
    (collect g ~start:[| 3; 0 |] ~shape:[| 4; 5 |]);
  Alcotest.(check (list (list int)))
    "whole array"
    [ [ 0; 0 ]; [ 1; 0 ]; [ 2; 0 ] ]
    (collect g ~start:[| 0; 0 |] ~shape:[| 11; 5 |]);
  Alcotest.(check (list (list int)))
    "single chunk" [ [ 1; 0 ] ]
    (collect g ~start:[| 5; 1 |] ~shape:[| 1; 1 |]);
  Alcotest.(check (list (list int)))
    "empty subset" []
    (collect g ~start:[| 0; 0 |] ~shape:[| 0; 5 |]);
  Alcotest.(check (list (list int)))
    "past the array end"
    [ [ 2; 0 ] ]
    (collect g ~start:[| 10; 0 |] ~shape:[| 20; 5 |]);
  let two = grid [| 4; 4 |] [| 2; 2 |] in
  Alcotest.(check (list (list int)))
    "c order"
    [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ] ]
    (collect two ~start:[| 0; 0 |] ~shape:[| 4; 4 |]);
  let z = grid [| 0; 20 |] [| 3; 5 |] in
  Alcotest.(check (list (list int)))
    "zero length dimension" []
    (collect z ~start:[| 0; 0 |] ~shape:[| 0; 20 |]);
  let nil = grid [||] [||] in
  Alcotest.(check (list (list int)))
    "zero dimensional" [ [] ]
    (collect nil ~start:[||] ~shape:[||])

let test_grid_ext () =
  let e = Ext.v "regular" ?config:(obj {|{"chunk_shape":[1000,100]}|}) in
  let g =
    match Chunk_grid.of_ext e ~array_shape:[| 10000; 1000 |] with
    | Ok g -> g
    | Error m -> Alcotest.failf "%s" m
  in
  Alcotest.check ints "chunk shape" [| 1000; 100 |] (Chunk_grid.chunk_shape g);
  Alcotest.check ints "grid shape" [| 10; 10 |] (Chunk_grid.grid_shape g);
  Alcotest.check ext "to_ext" e (Chunk_grid.to_ext g);
  let bad name =
    Result.is_error (Chunk_grid.of_ext name ~array_shape:[| 10 |])
  in
  Alcotest.(check bool)
    "unknown name" true
    (bad (Ext.v "rectangular" ?config:(obj {|{"chunk_shape":[2]}|})));
  Alcotest.(check bool) "no configuration" true (bad (Ext.v "regular"));
  Alcotest.(check bool)
    "unknown member" true
    (bad (Ext.v "regular" ?config:(obj {|{"chunk_shape":[2],"x":1}|})));
  Alcotest.(check bool)
    "zero chunk length" true
    (bad (Ext.v "regular" ?config:(obj {|{"chunk_shape":[0]}|})));
  Alcotest.(check bool)
    "dimension mismatch" true
    (bad (Ext.v "regular" ?config:(obj {|{"chunk_shape":[2,2]}|})));
  (* The spec does not allow the chunk grid to be ignorable. *)
  Alcotest.(check bool)
    "must_understand false" true
    (bad
       (Ext.v "regular" ~must_understand:false
          ?config:(obj {|{"chunk_shape":[2]}|})))

let chunk_grid_tests =
  [
    ("grid shape", `Quick, test_grid_shape);
    ("errors", `Quick, test_grid_errors);
    ("arithmetic", `Quick, test_grid_arithmetic);
    ("clip", `Quick, test_grid_clip);
    ("overlap", `Quick, test_grid_overlap);
    ("extension point", `Quick, test_grid_ext);
  ]

(* Chunk keys *)

let test_key_encode () =
  Alcotest.(check string)
    "default slash" "c/1/23/45"
    (Chunk_key.encode Chunk_key.default [| 1; 23; 45 |]);
  Alcotest.(check string)
    "default dot" "c.1.23.45"
    (Chunk_key.encode (Chunk_key.Default { separator = '.' }) [| 1; 23; 45 |]);
  Alcotest.(check string)
    "default zero dimensional" "c"
    (Chunk_key.encode Chunk_key.default [||]);
  Alcotest.(check string)
    "v2 dot" "1.23.45"
    (Chunk_key.encode Chunk_key.v2 [| 1; 23; 45 |]);
  Alcotest.(check string)
    "v2 slash" "1/23/45"
    (Chunk_key.encode (Chunk_key.V2 { separator = '/' }) [| 1; 23; 45 |]);
  Alcotest.(check string)
    "v2 zero dimensional" "0"
    (Chunk_key.encode Chunk_key.v2 [||])

let test_key_ext () =
  let ok s expect =
    match Chunk_key.of_ext (json_of_string s |> fun j ->
        match Jsont.Json.decode Ext.jsont j with
        | Ok e -> e
        | Error m -> Alcotest.failf "%s" m)
    with
    | Ok k -> Alcotest.(check bool) s true (k = expect)
    | Error m -> Alcotest.failf "%s: %s" s m
  in
  ok {|"default"|} Chunk_key.default;
  ok {|{"name":"default"}|} Chunk_key.default;
  ok {|{"name":"default","configuration":{}}|} Chunk_key.default;
  ok {|{"name":"default","configuration":{"separator":"."}}|}
    (Chunk_key.Default { separator = '.' });
  ok {|"v2"|} Chunk_key.v2;
  ok {|{"name":"v2","configuration":{"separator":"/"}}|}
    (Chunk_key.V2 { separator = '/' });
  let bad e = Result.is_error (Chunk_key.of_ext e) in
  Alcotest.(check bool) "unknown name" true (bad (Ext.v "other"));
  Alcotest.(check bool)
    "bad separator" true
    (bad (Ext.v "default" ?config:(obj {|{"separator":"-"}|})));
  Alcotest.(check bool)
    "separator not a string" true
    (bad (Ext.v "default" ?config:(obj {|{"separator":1}|})));
  Alcotest.(check bool)
    "unknown configuration member" true
    (bad (Ext.v "default" ?config:(obj {|{"other":1}|})));
  (* The spec does not allow the chunk key encoding to be ignorable. *)
  Alcotest.(check bool)
    "must_understand false" true
    (bad (Ext.v "default" ~must_understand:false));
  Alcotest.check ext "to_ext default"
    (Ext.v "default" ?config:(obj {|{"separator":"/"}|}))
    (Chunk_key.to_ext Chunk_key.default);
  Alcotest.check ext "to_ext v2"
    (Ext.v "v2" ?config:(obj {|{"separator":"."}|}))
    (Chunk_key.to_ext Chunk_key.v2)

let test_store_keys () =
  Alcotest.(check string)
    "nested path" "foo/baz/c/1/0"
    (Chunk_key.data_key ~path:"/foo/baz"
       (Chunk_key.encode Chunk_key.default [| 1; 0 |]));
  Alcotest.(check string)
    "root path" "c/1/0"
    (Chunk_key.data_key ~path:"/"
       (Chunk_key.encode Chunk_key.default [| 1; 0 |]));
  Alcotest.(check string)
    "root meta" "zarr.json"
    (Chunk_key.meta_key ~path:"/");
  Alcotest.(check string)
    "nested meta" "foo/bar/zarr.json"
    (Chunk_key.meta_key ~path:"/foo/bar")

let chunk_key_tests =
  [
    ("encode", `Quick, test_key_encode);
    ("extension point", `Quick, test_key_ext);
    ("store keys", `Quick, test_store_keys);
  ]

(* Byte ranges *)

let test_byte_range () =
  let check name expect r =
    let got = Byte_range.resolve ~size:10 r in
    Alcotest.(check (pair int int)) name expect got
  in
  check "prefix" (0, 4) (Byte_range.From_start { off = 0; len = Some 4 });
  check "middle" (3, 4) (Byte_range.From_start { off = 3; len = Some 4 });
  check "to the end" (3, 7) (Byte_range.From_start { off = 3; len = None });
  check "truncated" (8, 2) (Byte_range.From_start { off = 8; len = Some 5 });
  check "beyond the end" (10, 0)
    (Byte_range.From_start { off = 12; len = None });
  check "suffix" (6, 4) (Byte_range.Suffix 4);
  check "suffix longer than the object" (0, 10) (Byte_range.Suffix 20);
  Alcotest.(check string)
    "pp" "bytes -4"
    (Format.asprintf "%a" Byte_range.pp (Byte_range.Suffix 4))

let byte_range_tests = [ ("resolve", `Quick, test_byte_range) ]

(* Errors *)

let test_error () =
  Alcotest.(check string)
    "metadata" "metadata: bad"
    (Error.to_string (Error.Metadata "bad"));
  Alcotest.(check string)
    "checksum" "checksum mismatch: expected 0000002a, got 000000ff"
    (Error.to_string
       (Error.Checksum_mismatch { expected = 42l; got = 255l }));
  Alcotest.(check bool)
    "raise_" true
    (try
       Error.raise_ (Error.Store "gone")
     with
    | Error.E (Error.Store "gone") -> true
    | _ -> false)

let error_tests = [ ("errors", `Quick, test_error) ]

let () =
  Alcotest.run "zarrz metadata"
    [
      ("error", error_tests);
      ("byte_range", byte_range_tests);
      ("ext", ext_tests);
      ("metadata", metadata_tests);
      ("fill_value", fill_value_tests);
      ("chunk_grid", chunk_grid_tests);
      ("chunk_key", chunk_key_tests);
    ]
