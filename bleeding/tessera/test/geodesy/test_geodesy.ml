(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Zone routing against the table in ../geotessera/tests/store_check.py,
   affine index arithmetic, and the Crs golden vectors of
   crs_vectors.ml.  The Crs cases are also the guard tests for the
   vendored ocaml-proj copy, which vendor/ocaml-proj/README.md names. *)

open Tessera

let ints = Alcotest.(list int)

let substring needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0

(* -- Zone ---------------------------------------------------------- *)

let test_seam_neighbours () =
  let case lon expect =
    Alcotest.check ints
      (Printf.sprintf "seam_neighbours %g" lon)
      expect
      (Zone.seam_neighbours lon)
  in
  case 3.0 [];
  case 138.03 [ 53 ];
  case 137.97 [ 54 ];
  case 138.2 [];
  case 138.0 [ 53 ];
  case (-179.97) [ 60 ];
  case 179.97 [ 1 ];
  (* Seams west of Greenwich exercise the sign of the remainder: OCaml
     leaves it negative where Python does not. Values checked against
     geotessera.store._seam_neighbours. *)
  case (-6.03) [ 30 ];
  case (-5.97) [ 29 ];
  case (-6.0) [ 29 ];
  case (-3.0) [];
  case (-0.03) [ 31 ];
  case 0.03 [ 30 ]

let test_for_lon () =
  let case lon expect =
    Alcotest.(check int) (Printf.sprintf "for_lon %g" lon) expect
      (Zone.for_lon lon)
  in
  case 2.35 31;
  case (-120.5) 10;
  case (-180.0) 1;
  case 180.0 60;
  case (-181.0) 1;
  case 181.0 60;
  case 0.0 31;
  case (-0.01) 30

let test_canonical () =
  Alcotest.(check int) "zone 1" 32601 (Zone.canonical_epsg 1);
  Alcotest.(check int) "zone 60" 32660 (Zone.canonical_epsg 60);
  let close name expect got =
    Alcotest.(check (float 1e-12)) name expect got
  in
  close "centre 1" (-177.0) (Zone.centre_lon 1);
  close "centre 30" (-3.0) (Zone.centre_lon 30);
  close "centre 31" 3.0 (Zone.centre_lon 31);
  close "centre 60" 177.0 (Zone.centre_lon 60);
  (* Every centre must route back to its own zone. *)
  for z = 1 to 60 do
    Alcotest.(check int)
      (Printf.sprintf "centre of %d routes home" z)
      z
      (Zone.for_lon (Zone.centre_lon z))
  done

let test_spanned () =
  let case lons centre expect =
    Alcotest.check ints
      (Printf.sprintf "spanned ~centre_lon:%g" centre)
      expect
      (Zone.spanned lons ~centre_lon:centre)
  in
  case [ 2.0; 2.5 ] 2.2 [ 31 ];
  case [ -0.01; 0.01 ] 0.0 [ 30; 31 ];
  case [ 179.97; -179.97 ] 179.99 [ 60; 1 ];
  case [ 3.0 ] 3.0 [ 31 ];
  Alcotest.check_raises "no longitudes"
    (Invalid_argument "Zone.spanned: no longitudes") (fun () ->
        ignore (Zone.spanned [] ~centre_lon:0.))

(* -- Affine -------------------------------------------------------- *)

let store = Affine.of_spatial [| 10.; 0.; 300000.; 0.; -10.; 4050000. |]
let eps = Alcotest.float 1e-9

let test_of_spatial () =
  Alcotest.(check (float 1e-12)) "a" 10. store.Affine.a;
  Alcotest.(check (float 1e-12)) "c" 300000. store.Affine.c;
  Alcotest.(check (float 1e-12)) "e" (-10.) store.Affine.e;
  Alcotest.check
    Alcotest.(array (float 1e-12))
    "to_spatial round trips"
    [| 10.; 0.; 300000.; 0.; -10.; 4050000. |]
    (Affine.to_spatial store);
  Alcotest.(check bool) "equal to itself" true (Affine.equal store store);
  let n = ref 0 in
  List.iter
    (fun len ->
      match Affine.of_spatial (Array.make len 1.) with
      | _ -> Alcotest.fail "accepted the wrong length"
      | exception Invalid_argument _ -> incr n)
    [ 0; 5; 7 ];
  Alcotest.(check int) "three lengths rejected" 3 !n

let test_centres () =
  Alcotest.check eps "x of column 0" 300005. (Affine.x_of_col store ~col:0.);
  Alcotest.check eps "y of row 0" 4049995. (Affine.y_of_row store ~row:0.);
  Alcotest.check eps "x of column 7" 300075. (Affine.x_of_col store ~col:7.);
  Alcotest.check eps "col of 300005" 0. (Affine.col_of_x store ~x:300005.);
  Alcotest.check eps "row of 4049995" 0. (Affine.row_of_y store ~y:4049995.);
  List.iter
    (fun col ->
      let x = Affine.x_of_col store ~col in
      Alcotest.check eps
        (Printf.sprintf "column %g round trips" col)
        col
        (Affine.col_of_x store ~x))
    [ 0.; 1.; 7.25; 4095.5; -3. ];
  List.iter
    (fun row ->
      let y = Affine.y_of_row store ~row in
      Alcotest.check eps
        (Printf.sprintf "row %g round trips" row)
        row
        (Affine.row_of_y store ~y))
    [ 0.; 1.; 7.25; 4095.5; -3. ]

let test_apply_invert () =
  (* Corners are pixel corners, not centres, so the origin maps to the
     transform's own translation. *)
  let x, y = Affine.apply store ~col:0. ~row:0. in
  Alcotest.check eps "origin x" 300000. x;
  Alcotest.check eps "origin y" 4050000. y;
  (* A rotated transform, so the inverse exercises b and d as well. *)
  let rot = Affine.of_spatial [| 3.; 1.; 100.; -2.; 5.; -40. |] in
  List.iter
    (fun t ->
      let inv = Affine.invert t in
      List.iter
        (fun (col, row) ->
          let x, y = Affine.apply t ~col ~row in
          let col', row' = Affine.apply inv ~col:x ~row:y in
          Alcotest.check eps "col survives" col col';
          Alcotest.check eps "row survives" row row')
        [ (0., 0.); (1., 1.); (12.5, -7.25); (4095., 2048.) ])
    [ store; rot ]

let test_singular () =
  let case name v =
    Alcotest.check_raises name
      (Invalid_argument "Affine.invert: singular transform") (fun () ->
        ignore (Affine.invert (Affine.of_spatial v)))
  in
  case "all zero" [| 0.; 0.; 0.; 0.; 0.; 0. |];
  case "no x scale" [| 0.; 0.; 10.; 0.; -10.; 20. |];
  case "collinear rows" [| 1.; 2.; 0.; 2.; 4.; 0. |]

(* -- Crs ----------------------------------------------------------- *)

let metre = Alcotest.float 1e-6
let degree = Alcotest.float 1e-9

let test_utm_vectors () =
  List.iter
    (fun v ->
      let open Crs_vectors in
      let crs = Crs.utm_north ~zone:v.zone in
      let tag what =
        Printf.sprintf "zone %d at (%g, %g) %s" v.zone v.lon v.lat what
      in
      Alcotest.(check string)
        (tag "name")
        (Printf.sprintf "EPSG:%d" (32600 + v.zone))
        (Crs.name crs);
      let e, n = Crs.forward crs ~lon:v.lon ~lat:v.lat in
      Alcotest.check metre (tag "easting") v.e e;
      Alcotest.check metre (tag "northing") v.n n;
      let lon, lat = Crs.inverse crs ~e:v.e ~n:v.n in
      Alcotest.check degree (tag "inverse lon") v.ilon lon;
      Alcotest.check degree (tag "inverse lat") v.ilat lat;
      Alcotest.check degree (tag "round trip lon") v.lon lon;
      Alcotest.check degree (tag "round trip lat") v.lat lat)
    Crs_vectors.utm

let test_southern_northing () =
  (* Both hemispheres go through the northern code, so a southern point
     lands at a negative northing on a continuous axis rather than at
     the EPSG:327xx false northing of ten million metres. *)
  let southern =
    List.filter (fun v -> v.Crs_vectors.lat < 0.) Crs_vectors.utm
  in
  Alcotest.(check bool) "the table has southern points" true (southern <> []);
  List.iter
    (fun v ->
      let open Crs_vectors in
      let _, n = Crs.forward (Crs.utm_north ~zone:v.zone) ~lon:v.lon ~lat:v.lat
      in
      Alcotest.(check bool)
        (Printf.sprintf "zone %d at lat %g is a negative northing" v.zone v.lat)
        true (n < 0.))
    southern

let test_patch_vectors () =
  List.iter
    (fun v ->
      let open Crs_vectors in
      let crs = Crs.patch ~lon:v.p_lon ~lat:v.p_lat in
      let tag what =
        Printf.sprintf "patch (%g, %g) at (%g, %g) %s" v.p_lon v.p_lat
          v.p_lon_in v.p_lat_in what
      in
      Alcotest.(check string) (tag "proj string") v.p_crs (Crs.name crs);
      let e, n = Crs.forward crs ~lon:v.p_lon_in ~lat:v.p_lat_in in
      Alcotest.check metre (tag "easting") v.p_e e;
      Alcotest.check metre (tag "northing") v.p_n n;
      let lon, lat = Crs.inverse crs ~e:v.p_e ~n:v.p_n in
      Alcotest.check degree (tag "inverse lon") v.p_ilon lon;
      Alcotest.check degree (tag "inverse lat") v.p_ilat lat;
      Alcotest.check degree (tag "round trip lon") v.p_lon_in lon;
      Alcotest.check degree (tag "round trip lat") v.p_lat_in lat)
    Crs_vectors.patches

let test_patch_identities () =
  (* The two identities store_check.py checks: the central meridian
     lands on the false easting, and a southern patch keeps its
     northing positive through the ten million metre offset. *)
  let north = Crs.patch ~lon:0.5 ~lat:52.0 in
  let e, _ = Crs.forward north ~lon:0.5 ~lat:52.0 in
  Alcotest.check (Alcotest.float 1e-3) "the meridian is on the patch" 500000. e;
  let south = Crs.patch ~lon:0.5 ~lat:(-30.0) in
  let _, n = Crs.forward south ~lon:0.5 ~lat:(-30.0) in
  Alcotest.(check bool) "a southern patch is offset north" true (n > 5e6);
  Alcotest.(check bool) "the offset is in the string" true
    (substring "+y_0=10000000" (Crs.name south));
  Alcotest.(check bool) "a northern patch has none" true
    (substring "+y_0=0" (Crs.name north))

let test_zone_validation () =
  List.iter
    (fun zone ->
      Alcotest.check_raises
        (Printf.sprintf "zone %d" zone)
        (Invalid_argument
           (Printf.sprintf "Crs.utm_north: zone %d is outside 1..60" zone))
        (fun () -> ignore (Crs.utm_north ~zone)))
    [ 0; -1; 61; 100 ]

let test_bad_crs () =
  (* A CRS PROJ cannot parse fails when the transformation is first
     built, inside forward, and the message names the CRS. *)
  let crs = Crs.patch ~lon:Float.nan ~lat:0. in
  match Crs.forward crs ~lon:0. ~lat:0. with
  | _ -> Alcotest.fail "PROJ accepted a NaN central meridian"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (Printf.sprintf "the message names the CRS: %s" msg)
        true
        (substring (Crs.name crs) msg)

let () =
  Alcotest.run "geodesy"
    [
      ( "zone",
        [
          Alcotest.test_case "seam neighbours" `Quick test_seam_neighbours;
          Alcotest.test_case "zone for longitude" `Quick test_for_lon;
          Alcotest.test_case "codes and centres" `Quick test_canonical;
          Alcotest.test_case "zones spanned" `Quick test_spanned;
        ] );
      ( "affine",
        [
          Alcotest.test_case "of_spatial" `Quick test_of_spatial;
          Alcotest.test_case "pixel centres" `Quick test_centres;
          Alcotest.test_case "apply and invert" `Quick test_apply_invert;
          Alcotest.test_case "singular transforms" `Quick test_singular;
        ] );
      ( "crs",
        [
          Alcotest.test_case "utm golden vectors" `Quick test_utm_vectors;
          Alcotest.test_case "southern northings" `Quick test_southern_northing;
          Alcotest.test_case "patch golden vectors" `Quick test_patch_vectors;
          Alcotest.test_case "patch identities" `Quick test_patch_identities;
          Alcotest.test_case "zone validation" `Quick test_zone_validation;
          Alcotest.test_case "unparsable crs" `Quick test_bad_crs;
        ] );
    ]
