(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Store, dataset and probe tests over synthetic stores in memory.

   The probe cases are the table of ../geotessera/tests/store_check.py,
   ported case for case: the same 5 by 5 zone, the same three pixel
   states, the same expected vectors. Anything beyond that table is
   marked as such, and the counting store below turns the design's
   request-count claims into assertions. *)

module A1 = Bigarray.Array1
module Arr = Zarrz.Arr
module Dtype = Zarrz.Dtype
module Error = Zarrz.Error
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

let string_of_json j =
  match Jsont_bytesrw.encode_string Jsont.json j with
  | Ok s -> s
  | Error m -> Alcotest.failf "cannot encode JSON: %s" m

let bigstring_of_string s =
  Bigarray.Array1.of_array Bigarray.char Bigarray.c_layout
    (Array.init (String.length s) (String.get s))

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

let ints = Alcotest.(list int)

let status =
  Alcotest.testable
    (fun ppf s ->
      Format.pp_print_string ppf
        (match s with
        | Valid -> "valid"
        | Water -> "water"
        | Nodata -> "nodata"
        | Outside -> "outside"))
    ( = )

let check_vec name expect got =
  match got with
  | None -> Alcotest.failf "%s: expected a vector, got none" name
  | Some v ->
      Alcotest.(check int)
        (name ^ ": length") (Array.length expect) (Array.length v);
      Array.iteri
        (fun i x ->
          Alcotest.(check (float 1e-6))
            (Printf.sprintf "%s: [%d]" name i)
            x v.(i))
        expect

let raises_error name f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Error.E" name
  | exception Error.E e -> e

let substring needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0

(* -- Synthetic stores ---------------------------------------------- *)

(* The root attributes of the published store, cut to the members the
   convention requires and to four bands. The registration entry keeps
   only the uuid and the prefix, which is what identifies the
   convention. zarrz's own geoemb suite is where the full document is
   checked. *)
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
   [1;2;3;4], and the caller's scale grid. [chunk] splits the grid so
   that a tile read has to assemble, [write_scales] false leaves the
   scales chunks absent so they read as the [+inf] fill. *)
let fake_zone ?(px = 10.) ?(ox = 300000.) ?(oy = 4050000.) ?chunk
    ?(write_scales = true) store ~zone ~scales =
  let h = Array.length scales and w = Array.length scales.(0) in
  let chy, chx = match chunk with Some c -> c | None -> (h, w) in
  let path = Printf.sprintf "/utm%02d" zone in
  let attrs =
    Printf.sprintf
      {|{"proj:code":"EPSG:%d",
         "spatial:transform":[%.8g,0.0,%.8g,0.0,%.8g,%.8g]}|}
      (32600 + zone) px ox (-.px) oy
  in
  ignore (Group.create ~attributes:(json_of_string attrs) store ~path);
  let emb =
    Arr.create ~shape:[| 1; bands; h; w |]
      ~chunk_shape:[| 1; bands; chy; chx |] ~dtype:Dtype.Int8
      ~fill_value:(fill Dtype.Int8 "0") store ~path:(path ^ "/embeddings")
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
    Arr.create ~shape:[| 1; h; w |] ~chunk_shape:[| 1; chy; chx |]
      ~dtype:Dtype.Float32
      ~fill_value:(fill Dtype.Float32 {|"Infinity"|})
      store ~path:(path ^ "/scales")
  in
  if write_scales then begin
    let s = Slab.create Dtype.Float32 [: 1; h; w :] in
    let v = f32 s in
    for r = 0 to h - 1 do
      for c = 0 to w - 1 do
        A1.set v ((r * w) + c) scales.(r).(c)
      done
    done;
    Arr.write sca
      { Subset.start = [: 0; 0; 0 :]; shape = [: 1; h; w :] }
      s
  end;
  let tm =
    Arr.create ~shape:[| 1 |] ~chunk_shape:[| 1 |] ~dtype:Dtype.Int32
      ~fill_value:(fill Dtype.Int32 "0") store ~path:(path ^ "/time")
  in
  let s = Slab.create Dtype.Int32 [: 1 :] in
  A1.set (i32 s) 0 2024l;
  Arr.write tm { Subset.start = [: 0 :]; shape = [: 1 :] } s

let grid h w v = Array.init h (fun _ -> Array.make w v)
let inf = Float.infinity
let nan = Float.nan

(* The 5 by 5 zone of store_check.py, in UTM zone 53 so that its
   EPSG:32653 is the canonical code of its group name. *)
let zone53 ?chunk ?write_scales scales =
  let store = Store.memory () in
  fake_root store;
  fake_zone ?chunk ?write_scales store ~zone:53 ~scales;
  store

let dataset_of store zone = Dataset.open_ store ~zone

(* store_check's [cx] and [cy]: the centre pixel of the 5 by 5 grid. *)
let cx = 300025.
let cy = 4049975.

(* -- Probe: the store_check.py table -------------------------------- *)

(* The artifact found at tile corners in the published v1 store: a patch
   of data with a single unwritten pixel at its centre. *)
let holed () =
  let sc = grid 5 5 0.05 in
  sc.(2).(2) <- inf;
  sc

let vec ~scale = Array.init bands (fun b -> float_of_int (b + 1) *. scale)

let test_hole_without_repair () =
  let d = dataset_of (zone53 (holed ())) 53 in
  let v, st = Dataset.probe d ~e:cx ~n:cy ~year:2024 ~search_px:0 () in
  Alcotest.(check bool) "no vector" true (v = None);
  Alcotest.check status "nodata" Nodata st

let test_hole_with_repair () =
  let d = dataset_of (zone53 (holed ())) 53 in
  let v, st = Dataset.probe d ~e:cx ~n:cy ~year:2024 ~search_px:1 () in
  Alcotest.check status "valid" Valid st;
  check_vec "repaired" (vec ~scale:0.05) v

let test_water_is_never_repaired () =
  let sc = grid 5 5 0.05 in
  sc.(2).(2) <- nan;
  let d = dataset_of (zone53 sc) 53 in
  let v, st = Dataset.probe d ~e:cx ~n:cy ~year:2024 ~search_px:1 () in
  Alcotest.(check bool) "no vector" true (v = None);
  Alcotest.check status "water" Water st

let test_blank_window_stays_nodata () =
  let d = dataset_of (zone53 (grid 5 5 inf)) 53 in
  let v, st = Dataset.probe d ~e:cx ~n:cy ~year:2024 ~search_px:2 () in
  Alcotest.(check bool) "no vector" true (v = None);
  Alcotest.check status "nodata" Nodata st

(* Beyond store_check: the published store leaves a never written
   region without any chunk at all, so the fill value is what a reader
   sees. It must decide the same way an explicit [+inf] does. *)
let test_unwritten_chunk_is_nodata () =
  let d = dataset_of (zone53 ~write_scales:false (grid 5 5 0.05)) 53 in
  let v, st = Dataset.probe d ~e:cx ~n:cy ~year:2024 ~search_px:2 () in
  Alcotest.(check bool) "no vector" true (v = None);
  Alcotest.check status "nodata" Nodata st

let test_far_outside_the_grid () =
  let d = dataset_of (zone53 (holed ())) 53 in
  let v, st =
    Dataset.probe d ~e:(300005. -. 10_000.) ~n:cy ~year:2024 ()
  in
  Alcotest.(check bool) "no vector" true (v = None);
  Alcotest.check status "outside" Outside st

let test_nearest_valid_pixel_wins () =
  let sc = grid 5 5 inf in
  sc.(2).(3) <- 0.07;
  (* immediately east of centre *)
  sc.(0).(0) <- 0.09;
  (* further away *)
  let d = dataset_of (zone53 sc) 53 in
  let v, st = Dataset.probe d ~e:cx ~n:cy ~year:2024 ~search_px:2 () in
  Alcotest.check status "valid" Valid st;
  check_vec "nearest wins" (vec ~scale:0.07) v

let test_sample_collapses_status () =
  let sc = grid 5 5 0.05 in
  sc.(2).(2) <- nan;
  let watery = dataset_of (zone53 sc) 53 in
  Alcotest.(check bool)
    "water samples to nothing" true
    (Dataset.sample watery ~e:cx ~n:cy ~year:2024 () = None);
  let d = dataset_of (zone53 (holed ())) 53 in
  check_vec "sample repairs as probe does" (vec ~scale:0.05)
    (Dataset.sample d ~e:cx ~n:cy ~year:2024 ());
  let p, _ = Dataset.probe d ~e:cx ~n:cy ~year:2024 () in
  check_vec "probe and sample agree" (vec ~scale:0.05) p

(* -- Probe: edges the Python table does not reach ------------------- *)

let test_residual_bound () =
  let d = dataset_of (zone53 (grid 5 5 0.05)) 53 in
  let edge = 300045. in
  (* the centre of the last column *)
  let _, st = Dataset.probe d ~e:(edge +. 9.999) ~n:cy ~year:2024 () in
  Alcotest.check status "just inside one pixel" Valid st;
  let _, st = Dataset.probe d ~e:(edge +. 10.001) ~n:cy ~year:2024 () in
  Alcotest.check status "just past one pixel" Outside st

(* A point exactly on a pixel boundary is a tie between two centres, and
   [numpy.argmin] gives it to the lower index. *)
let test_tie_takes_the_lower_index () =
  let sc = grid 5 5 inf in
  sc.(2).(1) <- 0.05;
  sc.(2).(2) <- 0.07;
  let d = dataset_of (zone53 sc) 53 in
  let v, st = Dataset.probe d ~e:300020. ~n:cy ~year:2024 ~search_px:0 () in
  Alcotest.check status "valid" Valid st;
  check_vec "western pixel wins the tie" (vec ~scale:0.05) v

(* A window at a tile corner spans four 32 by 32 tiles. Each pixel is
   fetched from its own tile, so the assembled window must still be the
   one the affine names, and the row-major tie break must still hold
   across the seam. *)
let test_window_spans_four_tiles () =
  let sc = grid 40 40 inf in
  sc.(31).(31) <- 0.05;
  (* the tile before the corner *)
  sc.(33).(33) <- 0.07;
  (* the tile after it, the same distance away *)
  let store = Store.memory () in
  fake_root store;
  fake_zone ~chunk:(8, 8) store ~zone:53 ~scales:sc;
  let d = dataset_of store 53 in
  let t = Dataset.transform d in
  (* Probe the pixel on the corner itself, so the repair has to reach
     into all four tiles around it. *)
  let e = Affine.x_of_col t ~col:32. and n = Affine.y_of_row t ~row:32. in
  let v, st = Dataset.probe d ~e ~n ~year:2024 ~search_px:1 () in
  Alcotest.check status "valid" Valid st;
  check_vec "the first of the tied candidates" (vec ~scale:0.05) v

let test_unknown_year () =
  let d = dataset_of (zone53 (grid 5 5 0.05)) 53 in
  match Dataset.probe d ~e:cx ~n:cy ~year:2017 () with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument m ->
      Alcotest.(check bool)
        ("the message lists the years: " ^ m)
        true (substring "2024" m)

(* Eviction is observable: a cache of one entry cannot hold two tiles,
   so revisiting the first one has to read it again. *)
let test_tile_cache_evicts () =
  let store = Store.memory () in
  fake_root store;
  fake_zone ~chunk:(8, 8) store ~zone:53 ~scales:(grid 40 40 0.05);
  let reads = ref 0 in
  let counted =
    { store with Store.get = (fun ~key -> incr reads; store.Store.get ~key) }
  in
  let d = Dataset.open_ ~cache_capacity:1 counted ~zone:53 in
  let t = Dataset.transform d in
  let at col row =
    ignore
      (Dataset.probe d
         ~e:(Affine.x_of_col t ~col)
         ~n:(Affine.y_of_row t ~row)
         ~year:2024 ~search_px:0 ())
  in
  at 0. 0.;
  at 0. 0.;
  let warm = !reads in
  at 33. 33.;
  Alcotest.(check bool) "a second tile is read" true (!reads > warm);
  let cold = !reads in
  at 0. 0.;
  Alcotest.(check bool)
    "the evicted tile is read again" true (!reads > cold)

(* -- Dataset properties -------------------------------------------- *)

let test_dataset_properties () =
  let d = dataset_of (zone53 (grid 5 5 0.05)) 53 in
  Alcotest.(check int) "zone" 53 (Dataset.zone d);
  Alcotest.(check int) "epsg" 32653 (Dataset.epsg d);
  Alcotest.(check (pair int int)) "shape" (5, 5) (Dataset.shape d);
  Alcotest.(check int) "bands" bands (Dataset.bands d);
  Alcotest.(check (float 0.)) "pixel size" 10. (Dataset.pixel_size d);
  Alcotest.check ints "years" [ 2024 ] (Dataset.years d);
  Alcotest.(check string) "crs" "EPSG:32653" (Crs.name (Dataset.crs d));
  Alcotest.(check bool)
    "transform" true
    (Affine.equal (Dataset.transform d)
       (Affine.of_spatial [| 10.; 0.; 300000.; 0.; -10.; 4050000. |]))

(* The store files both hemispheres of a zone under the northern code,
   so a group saying otherwise is not the grid [crs] projects onto. *)
let test_non_canonical_epsg () =
  let store = Store.memory () in
  fake_root store;
  fake_zone store ~zone:53 ~scales:(grid 5 5 0.05);
  let key = "utm53/zarr.json" in
  let southern =
    json_of_string
      {|{"proj:code":"EPSG:32753",
         "spatial:transform":[10,0,300000,0,-10,4050000]}|}
  in
  let j =
    match Store.get_json store ~key with
    | Jsont.Object (o, m) ->
        Jsont.Object
          ( List.map
              (fun ((n, nm), v) ->
                if n = "attributes" then ((n, nm), southern) else ((n, nm), v))
              o,
            m )
    | _ -> Alcotest.fail "the zone document is not an object"
  in
  (Option.get store.Store.set) ~key (bigstring_of_string (string_of_json j));
  let e = raises_error "epsg" (fun () -> Dataset.open_ store ~zone:53) in
  Alcotest.(check bool)
    ("names the canonical code: " ^ Error.to_string e)
    true
    (substring "32653" (Error.to_string e))

(* -- Region reads --------------------------------------------------- *)

let test_read_region () =
  let d = dataset_of (zone53 (grid 8 8 0.05)) 53 in
  let r =
    Dataset.read_region d ~e_min:300010. ~e_max:300045. ~n_min:4049955.
      ~n_max:4049990. ~year:2024
  in
  let v = f32 r.Dataset.Region.data in
  Alcotest.(check int)
    "shape" (4 * 4 * bands)
    (Slab.num_elements r.Dataset.Region.data);
  Alcotest.(check int) "epsg" 32653 r.Dataset.Region.epsg;
  (* store.py builds the affine from the first selected pixel centre
     less half a pixel, which is the corner [ox + col0 * px]. *)
  Alcotest.(check bool)
    "corner transform" true
    (Affine.equal r.Dataset.Region.transform
       (Affine.of_spatial [| 10.; 0.; 300010.; 0.; -10.; 4049990. |]));
  for i = 0 to (4 * 4) - 1 do
    for b = 0 to bands - 1 do
      Alcotest.(check (float 1e-6))
        (Printf.sprintf "pixel %d band %d" i b)
        (float_of_int (b + 1) *. 0.05)
        (A1.get v ((i * bands) + b))
    done
  done

let test_read_region_nan_row () =
  let sc = grid 8 8 0.05 in
  sc.(2).(2) <- nan;
  let d = dataset_of (zone53 sc) 53 in
  let r =
    Dataset.read_region d ~e_min:300010. ~e_max:300045. ~n_min:4049955.
      ~n_max:4049990. ~year:2024
  in
  let v = f32 r.Dataset.Region.data in
  (* The window starts at row 1, column 1, so grid (2,2) is (1,1). *)
  for b = 0 to bands - 1 do
    Alcotest.(check bool)
      (Printf.sprintf "nan row band %d" b)
      true
      (Float.is_nan (A1.get v ((((1 * 4) + 1) * bands) + b)))
  done;
  Alcotest.(check (float 1e-6))
    "its neighbour is untouched" 0.05
    (A1.get v ((((1 * 4) + 2) * bands) + 0))

let test_read_region_empty () =
  let d = dataset_of (zone53 (grid 8 8 0.05)) 53 in
  let r =
    Dataset.read_region d ~e_min:400000. ~e_max:400100. ~n_min:4049955.
      ~n_max:4049990. ~year:2024
  in
  Alcotest.(check int) "no elements" 0
    (Slab.num_elements r.Dataset.Region.data);
  Alcotest.(check bool)
    "clamped to the eastern edge" true
    (Affine.equal r.Dataset.Region.transform
       (Affine.of_spatial [| 10.; 0.; 300080.; 0.; -10.; 4049990. |]))

(* -- Cross-zone routing --------------------------------------------- *)

(* Two zones whose grids both straddle the lon 0 seam, as real tiles do
   where a zone's grid runs past its own boundary. *)
let seam_store ~west ~east =
  let store = Store.memory () in
  fake_root store;
  List.iter
    (fun (zone, scales) ->
      let e, n = Crs.forward (Crs.utm_north ~zone) ~lon:0. ~lat:52. in
      fake_zone ~ox:(e -. 160.) ~oy:(n +. 160.) store ~zone ~scales)
    [ (30, west); (31, east) ];
  store

let seam_lon = -0.001
let seam_lat = 52.0

let test_cross_zone_finds_the_neighbour () =
  let t =
    of_store (seam_store ~west:(grid 32 32 inf) ~east:(grid 32 32 0.05))
  in
  Alcotest.check ints "zone routing" [ 30 ] [ Zone.for_lon seam_lon ];
  Alcotest.check ints "the neighbour is east" [ 31 ]
    (Zone.seam_neighbours seam_lon);
  let v, st = probe t ~lon:seam_lon ~lat:seam_lat ~year:2024 () in
  Alcotest.check status "the neighbour serves it" Valid st;
  check_vec "the neighbour's values" (vec ~scale:0.05) v;
  let v, st =
    probe t ~lon:seam_lon ~lat:seam_lat ~year:2024 ~cross_zone:false ()
  in
  Alcotest.(check bool) "no vector" true (v = None);
  Alcotest.check status "its own zone alone has nothing" Nodata st

(* Water is a real answer about the location, so it outranks nodata. *)
let test_status_precedence () =
  let t =
    of_store (seam_store ~west:(grid 32 32 nan) ~east:(grid 32 32 inf))
  in
  let _, st = probe t ~lon:seam_lon ~lat:seam_lat ~year:2024 () in
  Alcotest.check status "water outranks nodata" Water st

let test_outside_when_no_grid_reaches () =
  let t =
    of_store (seam_store ~west:(grid 32 32 0.05) ~east:(grid 32 32 0.05))
  in
  let _, st = probe t ~lon:0. ~lat:40. ~year:2024 () in
  Alcotest.check status "outside" Outside st

let test_empty_store_is_outside () =
  let store = Store.memory () in
  fake_root store;
  let t = of_store store in
  Alcotest.check ints "no zones" [] (zones t);
  Alcotest.check ints "no years" [] (years t);
  let _, st = probe t ~lon:seam_lon ~lat:seam_lat ~year:2024 () in
  Alcotest.check status "outside" Outside st

let test_absent_zone_raises () =
  let t = of_store (zone53 (grid 5 5 0.05)) in
  Alcotest.(check bool) "absent" true (zone_opt t 12 = None);
  let e = raises_error "zone 12" (fun () -> zone t 12) in
  Alcotest.(check bool)
    ("names the zone: " ^ Error.to_string e)
    true
    (substring "utm12" (Error.to_string e))

let test_sample_points_keeps_order () =
  let t =
    of_store (seam_store ~west:(grid 32 32 0.05) ~east:(grid 32 32 0.07))
  in
  (* Interleaved zones, so a grouped walk really does reorder them. *)
  let pts =
    [| (-0.001, 52.); (0.001, 52.); (-0.0005, 52.); (0.0005, 52.); (0., 40.) |]
  in
  let out = sample_points t pts ~year:2024 ~cross_zone:false () in
  Alcotest.(check int) "one row per point" 5 (Array.length out);
  check_vec "point 0 is in zone 30" (vec ~scale:0.05) out.(0);
  check_vec "point 1 is in zone 31" (vec ~scale:0.07) out.(1);
  check_vec "point 2 is in zone 30" (vec ~scale:0.05) out.(2);
  check_vec "point 3 is in zone 31" (vec ~scale:0.07) out.(3);
  Alcotest.(check bool) "point 4 is nowhere" true (out.(4) = None)

let test_top_level_read_region () =
  let t = of_store (zone53 (grid 8 8 0.05)) in
  let d = zone t 53 in
  let lon, lat = Crs.inverse (Dataset.crs d) ~e:300040. ~n:4049960. in
  Alcotest.(check int) "the bbox centre routes to zone 53" 53
    (Zone.for_lon lon);
  let r =
    read_region t
      ~bbox:(lon -. 0.0002, lat -. 0.0002, lon +. 0.0002, lat +. 0.0002)
      ~year:2024
  in
  Alcotest.(check int) "the zone's own crs" 32653 r.Dataset.Region.epsg;
  Alcotest.(check bool)
    "a non-empty window" true
    (Slab.num_elements r.Dataset.Region.data > 0)

(* -- Consolidated metadata ------------------------------------------ *)

(* Rewrite the root document with the node map zarr-python would have
   written: every descendant's own [zarr.json], keyed by its path. *)
let consolidate store paths =
  let mems =
    List.map
      (fun p ->
        (Jsont.Json.name p, Store.get_json store ~key:(p ^ "/zarr.json")))
      paths
  in
  let cm =
    Jsont.Json.object'
      [
        (Jsont.Json.name "kind", Jsont.Json.string "inline");
        (Jsont.Json.name "must_understand", Jsont.Json.bool false);
        (Jsont.Json.name "metadata", Jsont.Json.object' mems);
      ]
  in
  let root =
    match Store.get_json store ~key:"zarr.json" with
    | Jsont.Object (o, m) ->
        Jsont.Object (o @ [ (Jsont.Json.name "consolidated_metadata", cm) ], m)
    | _ -> Alcotest.fail "the root document is not an object"
  in
  (Option.get store.Store.set) ~key:"zarr.json"
    (bigstring_of_string (string_of_json root))

let zone_paths zone =
  let p = Printf.sprintf "utm%02d" zone in
  [ p; p ^ "/embeddings"; p ^ "/scales"; p ^ "/time" ]

let consolidated_store zones_ =
  let store = Store.memory () in
  fake_root store;
  List.iter
    (fun z -> fake_zone store ~zone:z ~scales:(holed ()))
    zones_;
  consolidate store (List.concat_map zone_paths zones_);
  store

(* Every store request, split by whether it asks for metadata. *)
let counting store =
  let meta = ref 0 and data = ref 0 in
  let bump key =
    if Filename.basename key = "zarr.json" then incr meta else incr data
  in
  let t =
    {
      store with
      Store.get = (fun ~key -> bump key; store.Store.get ~key);
      get_range = (fun ~key r -> bump key; store.Store.get_range ~key r);
      get_ranges = (fun ~key rs -> bump key; store.Store.get_ranges ~key rs);
      size = (fun ~key -> bump key; store.Store.size ~key);
    }
  in
  (t, meta, data)

let test_consolidated_zones () =
  let store = consolidated_store [ 53; 5; 9 ] in
  let c =
    match Consolidated.of_group (Group.metadata (Group.open_ store ~path:"/"))
    with
    | Some c -> c
    | None -> Alcotest.fail "no consolidated metadata"
  in
  Alcotest.check ints "ascending, groups only" [ 5; 9; 53 ]
    (Consolidated.zones c);
  Alcotest.(check bool)
    "a zone group is there" true
    (Consolidated.node c "utm53" <> None);
  Alcotest.(check bool)
    "a leading slash is tolerated" true
    (Consolidated.node c "/utm53/embeddings" <> None);
  Alcotest.(check bool)
    "an absent node" true
    (Consolidated.node c "utm07" = None);
  Alcotest.(check int) "every node is kept" 12
    (List.length (Consolidated.paths c))

let test_consolidated_costs_one_request () =
  let store, meta, data = counting (consolidated_store [ 53 ]) in
  let t = of_store store in
  Alcotest.(check int) "of_store reads the root once" 1 !meta;
  Alcotest.(check int) "and no chunk" 0 !data;
  Alcotest.check ints "zones come from the map" [ 53 ] (zones t);
  Alcotest.(check int) "still one metadata request" 1 !meta;
  let d = zone t 53 in
  Alcotest.(check int) "opening a zone reads no metadata" 1 !meta;
  let v, st = Dataset.probe d ~e:cx ~n:cy ~year:2024 () in
  Alcotest.check status "and still probes" Valid st;
  check_vec "the repaired value" (vec ~scale:0.05) v;
  Alcotest.(check int) "no metadata request at all" 1 !meta;
  (* The time chunk that resolves the year, then one tile of each of
     the two arrays. *)
  Alcotest.(check int) "three chunk requests" 3 !data

let test_tile_cache_spares_a_second_probe () =
  let store, _meta, data = counting (consolidated_store [ 53 ]) in
  let t = of_store store in
  let d = zone t 53 in
  ignore (Dataset.probe d ~e:cx ~n:cy ~year:2024 ());
  let warm = !data in
  Alcotest.(check bool) "the first probe fetched" true (warm > 0);
  (* One pixel east, the same 32 by 32 tile of both arrays. *)
  let v, st = Dataset.probe d ~e:(cx +. 10.) ~n:cy ~year:2024 () in
  Alcotest.check status "valid" Valid st;
  check_vec "its own value" (vec ~scale:0.05) v;
  Alcotest.(check int) "a cached tile costs nothing" warm !data

let test_lazy_zone_probe () =
  let store, meta, _ = counting (zone53 (grid 5 5 0.05)) in
  let t = of_store store in
  Alcotest.(check bool) "no node map" true (consolidated t = None);
  Alcotest.check ints "found by probing" [ 53 ] (zones t);
  Alcotest.(check int)
    "sixty probes plus the root" 61 !meta;
  Alcotest.check ints "the answer is kept" [ 53 ] (zones t);
  Alcotest.(check int) "and not probed again" 61 !meta;
  Alcotest.check ints "years come from the first zone" [ 2024 ] (years t)

(* -- Suite ---------------------------------------------------------- *)

let () =
  Alcotest.run "tessera store"
    [
      ( "probe",
        [
          Alcotest.test_case "hole without repair" `Quick
            test_hole_without_repair;
          Alcotest.test_case "hole with repair" `Quick test_hole_with_repair;
          Alcotest.test_case "water is never repaired" `Quick
            test_water_is_never_repaired;
          Alcotest.test_case "blank window" `Quick
            test_blank_window_stays_nodata;
          Alcotest.test_case "unwritten chunk" `Quick
            test_unwritten_chunk_is_nodata;
          Alcotest.test_case "far outside" `Quick test_far_outside_the_grid;
          Alcotest.test_case "nearest valid pixel" `Quick
            test_nearest_valid_pixel_wins;
          Alcotest.test_case "sample collapses" `Quick
            test_sample_collapses_status;
          Alcotest.test_case "residual bound" `Quick test_residual_bound;
          Alcotest.test_case "tie takes the lower index" `Quick
            test_tie_takes_the_lower_index;
          Alcotest.test_case "window spans four tiles" `Quick
            test_window_spans_four_tiles;
          Alcotest.test_case "unknown year" `Quick test_unknown_year;
          Alcotest.test_case "tile cache evicts" `Quick test_tile_cache_evicts;
        ] );
      ( "dataset",
        [
          Alcotest.test_case "properties" `Quick test_dataset_properties;
          Alcotest.test_case "non-canonical epsg" `Quick
            test_non_canonical_epsg;
          Alcotest.test_case "read_region" `Quick test_read_region;
          Alcotest.test_case "read_region nan row" `Quick
            test_read_region_nan_row;
          Alcotest.test_case "read_region empty" `Quick test_read_region_empty;
        ] );
      ( "routing",
        [
          Alcotest.test_case "cross zone" `Quick
            test_cross_zone_finds_the_neighbour;
          Alcotest.test_case "status precedence" `Quick test_status_precedence;
          Alcotest.test_case "outside" `Quick test_outside_when_no_grid_reaches;
          Alcotest.test_case "empty store" `Quick test_empty_store_is_outside;
          Alcotest.test_case "absent zone" `Quick test_absent_zone_raises;
          Alcotest.test_case "sample_points order" `Quick
            test_sample_points_keeps_order;
          Alcotest.test_case "read_region" `Quick test_top_level_read_region;
        ] );
      ( "consolidated",
        [
          Alcotest.test_case "zones" `Quick test_consolidated_zones;
          Alcotest.test_case "one request" `Quick
            test_consolidated_costs_one_request;
          Alcotest.test_case "tile cache" `Quick
            test_tile_cache_spares_a_second_probe;
          Alcotest.test_case "lazy probe" `Quick test_lazy_zone_probe;
        ] );
    ]
