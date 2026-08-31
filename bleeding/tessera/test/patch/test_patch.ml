(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Patch and npy tests over synthetic stores in memory.

   The patch cases are the table of ../geotessera/tests/store_check.py,
   ported case for case: the same 12 by 12 zone, the same pair of zones
   meeting at the lon 0 seam, the same expected samples. The fake zone
   builder is the one of ../store/test_store_tessera.ml, copied rather
   than shared, since a test directory is not a library. *)

module A1 = Bigarray.Array1
module Arr = Zarrz.Arr
module Dtype = Zarrz.Dtype
module Fill_value = Zarrz.Fill_value
module Group = Zarrz.Group
module Slab = Zarrz.Slab
module Store = Zarrz.Store
module Subset = Zarrz.Subset
open Tessera

(* -- Test plumbing ------------------------------------------------- *)

let json_of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error m -> Alcotest.failf "test JSON is invalid: %s" m

let f32 s =
  Bigarray.reshape_1 (Slab.to_genarray s Bigarray.float32)
    (Slab.num_elements s)

let i8 s =
  Bigarray.reshape_1 (Slab.to_genarray s Bigarray.int8_signed)
    (Slab.num_elements s)

let i32 s =
  Bigarray.reshape_1 (Slab.to_genarray s Bigarray.int32)
    (Slab.num_elements s)

let fill dt j =
  match Fill_value.of_json dt (json_of_string j) with
  | Ok f -> f
  | Error m -> Alcotest.failf "fill value: %s" m

let substring needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0

(* -- Synthetic stores ---------------------------------------------- *)

let root_attributes =
  {|{ "zarr_conventions":
      [ { "uuid": "61c12cc5-0e28-4056-999a-480cf3fb7e4c",
          "name": "geoemb:" } ],
      "geoemb:type": "pixel",
      "geoemb:dimensions": 4,
      "geoemb:model": "https://geotessera.org/model/1.0",
      "geoemb:source_data": [ "https://sentinel.esa.int/" ],
      "geoemb:data_type": "int8",
      "geoemb:gsd": 10.0,
      "geoemb:spatial_layout": "utm_zones",
      "geoemb:build_version": "0.9.1" }|}

let bands = 4

let fake_root store =
  ignore
    (Group.create ~attributes:(json_of_string root_attributes) store ~path:"/")

(* The fake zone of store_check.py: four bands, every embedding
   [1;2;3;4], and the caller's scale grid. *)
let fake_zone ?(px = 10.) ?(ox = 300000.) ?(oy = 4050000.) store ~zone ~scales =
  let h = Array.length scales and w = Array.length scales.(0) in
  let path = Printf.sprintf "/utm%02d" zone in
  let attrs =
    Printf.sprintf
      {|{"proj:code":"EPSG:%d",
         "spatial:transform":[%.8g,0.0,%.8g,0.0,%.8g,%.8g]}|}
      (32600 + zone) px ox (-.px) oy
  in
  ignore (Group.create ~attributes:(json_of_string attrs) store ~path);
  let emb =
    Arr.create ~shape:[| 1; bands; h; w |] ~chunk_shape:[| 1; bands; h; w |]
      ~dtype:Dtype.Int8
      ~fill_value:(fill Dtype.Int8 "0")
      store ~path:(path ^ "/embeddings")
  in
  let s = Slab.create Dtype.Int8 [: 1; bands; h; w :] in
  let v = i8 s in
  for b = 0 to bands - 1 do
    for r = 0 to h - 1 do
      for c = 0 to w - 1 do
        A1.set v ((b * h * w) + (r * w) + c) (b + 1)
      done
    done
  done;
  Arr.write emb
    { Subset.start = [: 0; 0; 0; 0 :]; shape = [: 1; bands; h; w :] }
    s;
  let sca =
    Arr.create ~shape:[| 1; h; w |] ~chunk_shape:[| 1; h; w |]
      ~dtype:Dtype.Float32
      ~fill_value:(fill Dtype.Float32 {|"Infinity"|})
      store ~path:(path ^ "/scales")
  in
  let s = Slab.create Dtype.Float32 [: 1; h; w :] in
  let v = f32 s in
  for r = 0 to h - 1 do
    for c = 0 to w - 1 do
      A1.set v ((r * w) + c) scales.(r).(c)
    done
  done;
  Arr.write sca { Subset.start = [: 0; 0; 0 :]; shape = [: 1; h; w :] } s;
  let tm =
    Arr.create ~shape:[| 1 |] ~chunk_shape:[| 1 |] ~dtype:Dtype.Int32
      ~fill_value:(fill Dtype.Int32 "0")
      store ~path:(path ^ "/time")
  in
  let s = Slab.create Dtype.Int32 [: 1 :] in
  A1.set (i32 s) 0 2024l;
  Arr.write tm { Subset.start = [: 0 :]; shape = [: 1 :] } s

let grid h w v = Array.init h (fun _ -> Array.make w v)

(* -- Patch plumbing ------------------------------------------------- *)

let px = 10.
let vec ~scale = Array.init bands (fun b -> float_of_int (b + 1) *. scale)

let patch_dims p =
  let g = Slab.to_genarray p.Patch.data Bigarray.float32 in
  (Bigarray.Genarray.nth_dim g 0, Bigarray.Genarray.nth_dim g 1,
   Bigarray.Genarray.nth_dim g 2)

let pixel p ~size ~row ~col =
  let v = f32 p.Patch.data in
  Array.init bands (fun b -> A1.get v ((((row * size) + col) * bands) + b))

let check_pixel name expect got =
  Array.iteri
    (fun i x ->
      Alcotest.(check (float 1e-6))
        (Printf.sprintf "%s: [%d]" name i)
        x got.(i))
    expect

(* A pixel with any finite band is covered, as the reference counts
   coverage. *)
let covered p ~size ~row ~col =
  Array.exists Float.is_finite (pixel p ~size ~row ~col)

let coverage p ~size =
  let n = ref 0 in
  for row = 0 to size - 1 do
    for col = 0 to size - 1 do
      if covered p ~size ~row ~col then incr n
    done
  done;
  !n

(* -- One zone: the native path -------------------------------------- *)

(* store_check's [flat]: a 12 by 12 zone of a single scale, whose
   EPSG:32653 makes it the zone the centre longitude routes to. *)
let flat_store () =
  let store = Store.memory () in
  fake_root store;
  fake_zone store ~zone:53 ~scales:(grid 12 12 0.05);
  store

(* The centre of pixel (6, 6) of that grid, as a WGS84 point. *)
let flat_centre () =
  Crs.inverse (Crs.utm_north ~zone:53) ~e:300065. ~n:4049935.

let test_native_patch () =
  let t = of_store (flat_store ()) in
  let lon, lat = flat_centre () in
  Alcotest.(check int) "the centre routes to zone 53" 53 (Zone.for_lon lon);
  let p = read_patch t ~lon ~lat ~year:2024 ~size_px:6 in
  Alcotest.(check (triple int int int))
    "the exact shape asked for" (6, 6, bands) (patch_dims p);
  Alcotest.(check bool)
    "the zone's own CRS" true
    (p.Patch.crs = `Epsg 32653);
  Alcotest.(check string) "named as the reference names it" "EPSG:32653"
    (Patch.crs_name p.Patch.crs);
  for row = 0 to 5 do
    for col = 0 to 5 do
      check_pixel
        (Printf.sprintf "native (%d, %d)" row col)
        (vec ~scale:0.05)
        (pixel p ~size:6 ~row ~col)
    done
  done;
  (* The transform must place the requested point on the centre of the
     centre pixel, within half a pixel. *)
  let ce, cn = Crs.forward (Crs.utm_north ~zone:53) ~lon ~lat in
  let x = Affine.x_of_col p.Patch.transform ~col:3. in
  let y = Affine.y_of_row p.Patch.transform ~row:3. in
  Alcotest.(check bool)
    (Printf.sprintf "centre pixel at (%.3f, %.3f) for (%.3f, %.3f)" x y ce cn)
    true
    (Float.abs (x -. ce) <= px /. 2. && Float.abs (y -. cn) <= px /. 2.)

let test_native_patch_is_nan_padded () =
  let t = of_store (flat_store ()) in
  let lon, lat = flat_centre () in
  let p = read_patch t ~lon ~lat ~year:2024 ~size_px:20 in
  Alcotest.(check (triple int int int))
    "the shape is kept, never truncated" (20, 20, bands) (patch_dims p);
  Alcotest.(check int) "the 12 by 12 of data survives" 144
    (coverage p ~size:20);
  for row = 0 to 19 do
    for col = 0 to 19 do
      if covered p ~size:20 ~row ~col then
        check_pixel
          (Printf.sprintf "padded (%d, %d)" row col)
          (vec ~scale:0.05)
          (pixel p ~size:20 ~row ~col)
    done
  done

(* -- Two zones: the merged path ------------------------------------- *)

(* store_check's [_seam_zone]: a zone whose data stops at the lon 0
   seam, as a real zone's tiles do. [shift] moves the data edge east,
   an overhang past the seam for the western zone and a gap after it
   for the eastern one. *)
let seam_zone store ~zone ~west ~scale ~shift =
  let h = 140 and w = 110 in
  let seam_e, seam_n =
    Crs.forward (Crs.utm_north ~zone) ~lon:0. ~lat:52.
  in
  let ox, width =
    if west then (seam_e -. (float_of_int w *. px), w + shift)
    else (seam_e +. (float_of_int shift *. px), w)
  in
  let oy = seam_n +. (float_of_int h *. px /. 2.) in
  fake_zone ~px ~ox ~oy store ~zone ~scales:(grid h width scale)

let seam_store ~west_shift ~east_shift =
  let store = Store.memory () in
  fake_root store;
  seam_zone store ~zone:30 ~west:true ~scale:0.05 ~shift:west_shift;
  seam_zone store ~zone:31 ~west:false ~scale:0.07 ~shift:east_shift;
  store

let seam_patch ~west_shift ~east_shift =
  let t = of_store (seam_store ~west_shift ~east_shift) in
  read_patch t ~lon:0. ~lat:52. ~year:2024 ~size_px:64

let test_seam_patch () =
  let p = seam_patch ~west_shift:0 ~east_shift:0 in
  Alcotest.(check (triple int int int))
    "the exact shape asked for" (64, 64, bands) (patch_dims p);
  let name = Patch.crs_name p.Patch.crs in
  Alcotest.(check bool)
    ("a patch-centred CRS: " ^ name)
    true
    (substring "+proj=tmerc" name);
  let n = coverage p ~size:64 in
  Alcotest.(check bool)
    (Printf.sprintf "covered from both zones: %d of 4096" n)
    true
    (float_of_int n /. 4096. > 0.98);
  check_pixel "west of the seam" (vec ~scale:0.05)
    (pixel p ~size:64 ~row:32 ~col:5);
  check_pixel "east of the seam" (vec ~scale:0.07)
    (pixel p ~size:64 ~row:32 ~col:58);
  (* Both paths centre the requested point on pixel [size / 2]. *)
  let ce, cn = Crs.forward (Crs.patch ~lon:0. ~lat:52.) ~lon:0. ~lat:52. in
  let x, y = Affine.apply p.Patch.transform ~col:32.5 ~row:32.5 in
  Alcotest.(check (float 1e-6)) "the centre pixel's easting" ce x;
  Alcotest.(check (float 1e-6)) "the centre pixel's northing" cn y

let test_seam_patch_overlap () =
  (* Zone 30's data overhangs 5 px past the seam, zone 31's starts 3 px
     after it, so a sliver either side of the seam is covered by the
     wrong zone alone or by both. *)
  let p = seam_patch ~west_shift:5 ~east_shift:3 in
  check_pixel "a sliver the owner lacks is filled by its neighbour"
    (vec ~scale:0.05)
    (pixel p ~size:64 ~row:32 ~col:33);
  check_pixel "where the zones overlap the owner wins" (vec ~scale:0.07)
    (pixel p ~size:64 ~row:32 ~col:36)

(* -- npy ------------------------------------------------------------ *)

(* What [numpy.save] writes for a 2 by 2 by 1 float32 array: the magic,
   version 1.0, a 118 byte header padded to a 128 byte preamble, then
   the elements little endian. *)
let test_npy_golden () =
  let s = Slab.create Dtype.Float32 [: 2; 2; 1 :] in
  let v = f32 s in
  for i = 0 to 3 do
    A1.set v i (float_of_int i)
  done;
  let out = Npy.to_string s in
  let body =
    "{'descr': '<f4', 'fortran_order': False, 'shape': (2, 2, 1), }"
  in
  let expect =
    "\147NUMPY\001\000\118\000" ^ body
    ^ String.make (117 - String.length body) ' '
    ^ "\n"
    ^ "\000\000\000\000\000\000\128\063\000\000\000\064\000\000\064\064"
  in
  Alcotest.(check int) "the preamble is 64 byte aligned" 0
    (String.length (Npy.header s) mod 64);
  Alcotest.(check int) "the whole file" 144 (String.length out);
  Alcotest.(check string) "byte for byte" expect out

let test_npy_shapes () =
  let header shape =
    let s = Slab.create Dtype.Float32 shape in
    Npy.header s
  in
  Alcotest.(check bool)
    "a one-dimensional shape keeps its comma" true
    (substring "'shape': (3,), }" (header [: 3 :]));
  Alcotest.(check bool)
    "a two-dimensional shape does not" true
    (substring "'shape': (3, 4), }" (header [: 3; 4 :]));
  Alcotest.(check int) "and stays aligned" 0
    (String.length (header [: 3; 4 :]) mod 64)

let test_npy_rejects_other_types () =
  let s = Slab.create Dtype.Int8 [: 4 :] in
  match Npy.header s with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument m ->
      Alcotest.(check bool)
        ("names the type: " ^ m)
        true (substring "float32" m)

(* A patch is exactly what the CLI writes out. *)
let test_npy_of_a_patch () =
  let t = of_store (flat_store ()) in
  let lon, lat = flat_centre () in
  let p = read_patch t ~lon ~lat ~year:2024 ~size_px:6 in
  let out = Npy.to_string p.Patch.data in
  Alcotest.(check bool)
    "the shape reaches the header" true
    (substring "'shape': (6, 6, 4), }" out);
  Alcotest.(check int) "header and elements" ((64 * 2) + (6 * 6 * 4 * 4))
    (String.length out)

(* -- Edges ---------------------------------------------------------- *)

let test_zero_size_is_refused () =
  let t = of_store (flat_store ()) in
  let lon, lat = flat_centre () in
  match read_patch t ~lon ~lat ~year:2024 ~size_px:0 with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument m ->
      Alcotest.(check bool)
        ("names the argument: " ^ m)
        true (substring "size_px" m)

let test_absent_centre_zone_raises () =
  let t = of_store (flat_store ()) in
  match read_patch t ~lon:0. ~lat:52. ~year:2024 ~size_px:4 with
  | _ -> Alcotest.fail "expected Error.E"
  | exception Zarrz.Error.E e ->
      Alcotest.(check bool)
        ("names the zone: " ^ Zarrz.Error.to_string e)
        true
        (substring "utm31" (Zarrz.Error.to_string e))

(* Beyond store_check: a patch on the corner of the grid keeps its
   shape and pads the two sides that run off it. The centre pixel of the
   patch is the corner pixel of the grid, so a quarter of the patch has
   data. *)
let test_patch_at_a_corner () =
  let t = of_store (flat_store ()) in
  let lon, lat = Crs.inverse (Crs.utm_north ~zone:53) ~e:300005. ~n:4049995. in
  let p = read_patch t ~lon ~lat ~year:2024 ~size_px:6 in
  Alcotest.(check (triple int int int))
    "the shape is kept" (6, 6, bands) (patch_dims p);
  Alcotest.(check int) "a quarter of it has data" 9 (coverage p ~size:6);
  check_pixel "the corner pixel is the centre pixel" (vec ~scale:0.05)
    (pixel p ~size:6 ~row:3 ~col:3);
  Alcotest.(check bool)
    "and its western neighbour is off the grid" false
    (covered p ~size:6 ~row:3 ~col:2)

(* -- Suite ---------------------------------------------------------- *)

let () =
  Alcotest.run "tessera patch"
    [
      ( "native",
        [
          Alcotest.test_case "one zone" `Quick test_native_patch;
          Alcotest.test_case "nan padding" `Quick
            test_native_patch_is_nan_padded;
          Alcotest.test_case "a grid corner" `Quick test_patch_at_a_corner;
        ] );
      ( "merged",
        [
          Alcotest.test_case "seam" `Quick test_seam_patch;
          Alcotest.test_case "overlap" `Quick test_seam_patch_overlap;
        ] );
      ( "npy",
        [
          Alcotest.test_case "golden bytes" `Quick test_npy_golden;
          Alcotest.test_case "shape tuples" `Quick test_npy_shapes;
          Alcotest.test_case "other types" `Quick test_npy_rejects_other_types;
          Alcotest.test_case "a patch" `Quick test_npy_of_a_patch;
        ] );
      ( "errors",
        [
          Alcotest.test_case "zero size" `Quick test_zero_size_is_refused;
          Alcotest.test_case "absent zone" `Quick
            test_absent_centre_zone_raises;
        ] );
    ]
