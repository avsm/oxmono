(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Geoembeddings convention tests. Expectations are read off the
   fixtures, see fixtures/README.md. *)

open Zarrz_geoemb

let read_file p =
  let ic = open_in_bin p in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let json_of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error m -> Alcotest.failf "test JSON is invalid: %s" m

let fixture name = json_of_string (read_file ("fixtures/" ^ name))

(* The three repository examples are whole group documents. The live
   store fixture is already the attributes object. *)
let attributes j =
  match j with
  | Jsont.Object (mems, _) -> (
      match Jsont.Json.find_mem "attributes" mems with
      | Some (_, a) -> a
      | None -> Alcotest.fail "fixture has no attributes member")
  | _ -> Alcotest.fail "fixture is not an object"

let obj_mems j =
  match j with
  | Jsont.Object (mems, _) -> mems
  | _ -> Alcotest.fail "not a JSON object"

let map_mem n f j =
  let mems =
    List.map
      (fun (((k, _) as name), v) ->
        if String.equal k n then (name, f v) else (name, v))
      (obj_mems j)
  in
  Jsont.Json.object' mems

let remove_mem n j =
  let keep ((k, _), _) = not (String.equal k n) in
  Jsont.Json.object' (List.filter keep (obj_mems j))

let set_mem n v j =
  let mems = obj_mems (remove_mem n j) in
  Jsont.Json.object' (mems @ [ (Jsont.Json.name n, v) ])

let mem n j =
  match Jsont.Json.find_mem n (obj_mems j) with
  | Some (_, v) -> v
  | None -> Alcotest.failf "no member %S" n

let json = Alcotest.testable Jsont.Json.pp Jsont.Json.equal
let geoemb = Alcotest.testable pp equal
let convention = Alcotest.testable Convention.pp Convention.equal
let chip_layout = Alcotest.testable Chip_layout.pp Chip_layout.equal
let quantization = Alcotest.testable Quantization.pp Quantization.equal
let strings = Alcotest.(list string)
let pair = Alcotest.(pair int int)

let kind_name = function Pixel -> "pixel" | Chip -> "chip"

let layout_name = function
  | Chip_layout.Regular_grid -> "regular_grid"
  | Chip_layout.Irregular -> "irregular"

let spatial_name = function Utm_zones -> "utm_zones" | Global -> "global"

let decode name j =
  match of_json j with
  | Ok t -> t
  | Error m -> Alcotest.failf "%s: %s" name m

(* The unknown members as a JSON object, so that a failure prints
   them. *)
let unknown_json t = Jsont.Json.object' t.unknown

(* Fixtures *)

let aef = attributes (fixture "aef_example.json")
let clay = attributes (fixture "clay_example.json")
let tessera = attributes (fixture "tessera_example.json")
let live = fixture "tessera_attributes.json"

(* Decoding the examples *)

let test_aef () =
  let t = decode "aef" aef in
  Alcotest.(check string) "type" "pixel" (kind_name t.kind);
  Alcotest.(check int) "dimensions" 64 t.dimensions;
  Alcotest.(check string) "model" "https://arxiv.org/abs/2507.22291" t.model;
  Alcotest.(check strings)
    "source_data"
    [
      "https://developers.google.com/earth-engine/datasets/catalog/\
       GOOGLE_SATELLITE_EMBEDDING_V1_ANNUAL";
    ]
    t.source_data;
  Alcotest.(check string) "data_type" "int8" t.data_type;
  Alcotest.(check (option (float 0.))) "gsd" (Some 10.0) t.gsd;
  Alcotest.(check (option quantization))
    "quantization"
    (Some
       (Quantization.v ~method_:"linear" ~original_dtype:"float32"
          ~quantized_dtype:"int8"
          ~scale:(Quantization.Scale.scalar ~offset:0.0 0.0078125)
          ()))
    t.quantization;
  Alcotest.(check bool) "no chip_layout" true (t.chip_layout = None);
  Alcotest.(check bool) "no benchmark" true (t.benchmark = None);
  Alcotest.(check bool) "no build_version" true (t.build_version = None);
  Alcotest.(check bool) "no spatial_layout" true (t.spatial_layout = None);
  Alcotest.(check int) "no unknown members" 0 (List.length t.unknown);
  Alcotest.(check (list convention))
    "conventions" [ Convention.geoemb ] t.conventions

let test_clay () =
  let t = decode "clay" clay in
  Alcotest.(check string) "type" "chip" (kind_name t.kind);
  Alcotest.(check int) "dimensions" 768 t.dimensions;
  Alcotest.(check string)
    "model" "https://huggingface.co/made-with-clay/Clay" t.model;
  Alcotest.(check string) "data_type" "float32" t.data_type;
  Alcotest.(check (option chip_layout))
    "chip_layout"
    (Some
       (Chip_layout.v ~layout_type:Chip_layout.Regular_grid
          ~chip_size:(256, 256) ~stride:(256, 256) ()))
    t.chip_layout;
  (match t.chip_layout with
  | None -> Alcotest.fail "clay has a chip layout"
  | Some c ->
      Alcotest.(check string) "layout_type" "regular_grid"
        (layout_name c.Chip_layout.layout_type));
  Alcotest.(check (option strings))
    "benchmark"
    (Some [ "https://clay-foundation.github.io/model/benchmarks" ])
    t.benchmark;
  Alcotest.(check bool) "no quantization" true (t.quantization = None);
  Alcotest.(check int) "no unknown members" 0 (List.length t.unknown)

let test_tessera_example () =
  let t = decode "tessera example" tessera in
  Alcotest.(check string) "type" "pixel" (kind_name t.kind);
  Alcotest.(check int) "dimensions" 128 t.dimensions;
  Alcotest.(check string) "model" "https://geotessera.org/model/1.0" t.model;
  Alcotest.(check strings)
    "source_data"
    [
      "https://sentinel.esa.int/web/sentinel/missions/sentinel-1";
      "https://sentinel.esa.int/web/sentinel/missions/sentinel-2";
    ]
    t.source_data;
  Alcotest.(check string) "data_type" "int8" t.data_type;
  Alcotest.(check string)
    "spatial_layout" "utm_zones"
    (match t.spatial_layout with
    | Some s -> spatial_name s
    | None -> "absent");
  Alcotest.(check (option string)) "build_version" (Some "0.7.5")
    t.build_version;
  Alcotest.(check int) "three conventions" 3 (List.length t.conventions);
  Alcotest.(check bool)
    "spatial convention found" true
    (Convention.find ~uuid:"689b58e2-cf7b-45e0-9fff-9cfc0883d6b4"
       t.conventions
    <> None);
  (* The members of the proj: and spatial: conventions are kept. *)
  Alcotest.(check (list string))
    "unknown members"
    [
      "proj:code";
      "spatial:dimensions";
      "spatial:transform";
      "spatial:shape";
      "spatial:bbox";
      "spatial:registration";
    ]
    (List.map (fun ((n, _), _) -> n) t.unknown)

let test_live () =
  let t = decode "tessera store" live in
  Alcotest.(check string) "type" "pixel" (kind_name t.kind);
  Alcotest.(check int) "dimensions" 128 t.dimensions;
  Alcotest.(check string) "data_type" "int8" t.data_type;
  Alcotest.(check (option (float 0.))) "gsd" (Some 10.0) t.gsd;
  Alcotest.(check (option string)) "build_version" (Some "0.9.1")
    t.build_version;
  Alcotest.(check (list convention))
    "conventions" [ Convention.geoemb ] t.conventions;
  match t.quantization with
  | None -> Alcotest.fail "the store is quantized"
  | Some q -> (
      Alcotest.(check string) "method" "per_pixel_scale" q.Quantization.method_;
      Alcotest.(check string)
        "original_dtype" "float32" q.Quantization.original_dtype;
      Alcotest.(check (option string))
        "quantized_dtype" (Some "int8") q.Quantization.quantized_dtype;
      match q.Quantization.scale with
      | Some (Quantization.Scale.Array a) ->
          Alcotest.(check string)
            "array_name" "scales" a.Quantization.Scale.array_name;
          Alcotest.(check bool)
            "nodata" true
            (a.Quantization.Scale.nodata
            = Some (Quantization.Scale.String "+inf"));
          (* The case member is consumed by the case map, not kept. *)
          Alcotest.(check int)
            "scale keeps nothing" 0
            (List.length a.Quantization.Scale.unknown)
      | Some (Quantization.Scale.Scalar _) ->
          Alcotest.fail "the store uses a per pixel scale"
      | None -> Alcotest.fail "the store has a scale")

let test_live_stretch () =
  (* geoemb:stretch is not in the schema. It must survive untouched. *)
  let t = decode "tessera store" live in
  Alcotest.(check (list string))
    "unknown members" [ "geoemb:stretch" ]
    (List.map (fun ((n, _), _) -> n) t.unknown);
  let j = to_json t in
  Alcotest.check json "stretch is unchanged" (mem "geoemb:stretch" live)
    (mem "geoemb:stretch" j);
  let t' = decode "tessera store, re-decoded" j in
  Alcotest.check geoemb "round trip" t t';
  Alcotest.check json "unknown members" (unknown_json t) (unknown_json t')

let decode_tests =
  [
    ("aef example", `Quick, test_aef);
    ("clay example", `Quick, test_clay);
    ("tessera example", `Quick, test_tessera_example);
    ("tessera store", `Quick, test_live);
    ("tessera store stretch", `Quick, test_live_stretch);
  ]

(* Round trips *)

let round_trip name j () =
  let t = decode name j in
  let t' = decode (name ^ ", re-decoded") (to_json t) in
  Alcotest.check geoemb "value" t t';
  Alcotest.check json "unknown members" (unknown_json t) (unknown_json t')

let round_trip_tests =
  [
    ("aef example", `Quick, round_trip "aef" aef);
    ("clay example", `Quick, round_trip "clay" clay);
    ("tessera example", `Quick, round_trip "tessera example" tessera);
    ("tessera store", `Quick, round_trip "tessera store" live);
  ]

(* Errors *)

let fails name j =
  ( name,
    `Quick,
    fun () ->
      match of_json j with
      | Ok _ -> Alcotest.failf "%s: expected a decode error" name
      | Error _ -> () )

let missing n = fails ("missing " ^ n) (remove_mem n live)

let error_tests =
  [
    missing "zarr_conventions";
    missing "geoemb:type";
    missing "geoemb:dimensions";
    missing "geoemb:model";
    missing "geoemb:source_data";
    missing "geoemb:data_type";
    fails "dimensions 0"
      (set_mem "geoemb:dimensions" (Jsont.Json.int 0) live);
    fails "dimensions -1"
      (set_mem "geoemb:dimensions" (Jsont.Json.int (-1)) live);
    fails "bad type" (set_mem "geoemb:type" (Jsont.Json.string "voxel") live);
    fails "gsd 0" (set_mem "geoemb:gsd" (Jsont.Json.number 0.) live);
    fails "gsd negative"
      (set_mem "geoemb:gsd" (Jsont.Json.number (-1.)) live);
    fails "empty source_data"
      (set_mem "geoemb:source_data" (Jsont.Json.list []) live);
    fails "bad spatial_layout"
      (set_mem "geoemb:spatial_layout" (Jsont.Json.string "quadkey") live);
    fails "chip without a chip layout" (remove_mem "geoemb:chip_layout" clay);
    fails "bad layout_type"
      (map_mem "geoemb:chip_layout"
         (set_mem "layout_type" (Jsont.Json.string "hexagons"))
         clay);
    fails "chip_size of one element"
      (map_mem "geoemb:chip_layout"
         (set_mem "chip_size" (Jsont.Json.list [ Jsont.Json.int 256 ]))
         clay);
    fails "chip_size of zero"
      (map_mem "geoemb:chip_layout"
         (set_mem "chip_size"
            (Jsont.Json.list [ Jsont.Json.int 0; Jsont.Json.int 256 ]))
         clay);
    fails "unknown scale type"
      (map_mem "geoemb:quantization"
         (map_mem "scale" (set_mem "type" (Jsont.Json.string "codebook")))
         live);
    fails "quantization without a method"
      (map_mem "geoemb:quantization" (remove_mem "method") live);
    fails "conventions without this one"
      (set_mem "zarr_conventions" (Jsont.Json.list []) live);
  ]

(* Probing an attributes object *)

let drop_geoemb_convention j =
  let keep e =
    match Jsont.Json.find_mem "uuid" (obj_mems e) with
    | Some (_, Jsont.String (u, _)) ->
        not (String.equal u Convention.geoemb_uuid)
    | Some _ | None -> true
  in
  map_mem "zarr_conventions"
    (function
      | Jsont.Array (l, _) -> Jsont.Json.list (List.filter keep l)
      | _ -> Alcotest.fail "zarr_conventions is not an array")
    j

let test_of_attributes () =
  Alcotest.(check bool)
    "no zarr_conventions" true
    (of_attributes (Jsont.Json.object' []) = None);
  Alcotest.(check bool)
    "not an object" true
    (of_attributes (Jsont.Json.string "attributes") = None);
  Alcotest.(check bool)
    "another convention only" true
    (of_attributes (drop_geoemb_convention tessera) = None);
  match of_attributes live with
  | None -> Alcotest.fail "the store declares this convention"
  | Some (Error m) -> Alcotest.failf "tessera store: %s" m
  | Some (Ok t) -> Alcotest.(check int) "dimensions" 128 t.dimensions

(* A name-only entry still identifies the convention, and a uuid that
   is present and different does not. *)
let test_is_geoemb () =
  Alcotest.(check bool)
    "by uuid" true
    (Convention.is_geoemb (Convention.v ~uuid:Convention.geoemb_uuid ()));
  Alcotest.(check bool)
    "by name" true
    (Convention.is_geoemb (Convention.v ~name:Convention.geoemb_name ()));
  Alcotest.(check bool)
    "another uuid" false
    (Convention.is_geoemb
       (Convention.v ~uuid:"689b58e2-cf7b-45e0-9fff-9cfc0883d6b4"
          ~name:Convention.geoemb_name ()));
  Alcotest.(check bool) "nothing" false (Convention.is_geoemb (Convention.v ()))

let test_effective_stride () =
  let c =
    Chip_layout.v ~layout_type:Chip_layout.Regular_grid ~chip_size:(256, 256)
      ()
  in
  Alcotest.check pair "default" (256, 256) (Chip_layout.effective_stride c);
  let c = { c with Chip_layout.stride = Some (128, 64) } in
  Alcotest.check pair "explicit" (128, 64) (Chip_layout.effective_stride c)

let probe_tests =
  [
    ("of_attributes", `Quick, test_of_attributes);
    ("is_geoemb", `Quick, test_is_geoemb);
    ("effective_stride", `Quick, test_effective_stride);
  ]

let () =
  Alcotest.run "zarrz geoemb"
    [
      ("decode", decode_tests);
      ("round trip", round_trip_tests);
      ("errors", error_tests);
      ("probe", probe_tests);
    ]
