module Proj = Proj_c

let test_invalid_create () =
  let def = "Hello, world!" in
  Alcotest.check_raises "Garbage definition"
    (Failure "Invalid PROJ string syntax") (fun () ->
      let _ = Proj.CRS.of_string def in
      ())

let test_create_with_epsg () =
  let crs = Proj.CRS.of_string "epsg:4326" in
  Alcotest.(check (option string))
    "check auth name" (Some "EPSG")
    (Proj.CRS.id_auth_name crs 0);
  Alcotest.(check (option string))
    "check id" (Some "4326") (Proj.CRS.id_code crs 0)

let test_create_with_name () =
  let crs = Proj.CRS.of_string "WGS 84" in
  Alcotest.(check (option string))
    "check auth name" (Some "EPSG")
    (Proj.CRS.id_auth_name crs 0);
  Alcotest.(check (option string))
    "check id" (Some "4326") (Proj.CRS.id_code crs 0)

let test_get_invalid_auth_name_index () =
  let crs = Proj.CRS.of_string "WGS 84" in
  Alcotest.(check (option string))
    "check auth name" None
    (Proj.CRS.id_auth_name crs 42);
  Alcotest.(check (option string)) "check id" None (Proj.CRS.id_code crs 42)

let test_create_with_wkt () =
  let wkt =
    {|PROJCS["ETRS89-SWE [SWEREF 99 TM]",GEOGCS["ETRS89-SWE [SWEREF 99]",DATUM["SWEREF_99",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],AUTHORITY["EPSG","6619"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4619"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",15],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Northing",NORTH],AXIS["Easting",EAST],AUTHORITY["EPSG","3006"]]|}
  in
  ignore (Proj.CRS.of_string wkt)

let test_invalid_wkt () =
  let wkt = "Hello, world!" in
  Alcotest.check_raises "Garbage wkt" (Failure "Unknown error (code 4096)")
    (fun () ->
      let _ = Proj.CRS.of_wkt wkt in
      ())

let test_idenfity_epsg_3006 () =
  let wkt =
    {|PROJCS["ETRS89-SWE [SWEREF 99 TM]",GEOGCS["ETRS89-SWE [SWEREF 99]",DATUM["SWEREF_99",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],AUTHORITY["EPSG","6619"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4619"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",15],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Northing",NORTH],AXIS["Easting",EAST],AUTHORITY["EPSG","3006"]]|}
  in
  let t = Proj.CRS.of_wkt wkt in
  let codes = Proj.CRS.identify t "epsg" in
  Alcotest.(check (list Alcotest.(pair string int)))
    "expected results"
    [ ("3006", 100) ]
    codes

let test_idenfity_esri_53009 () =
  let wkt =
    {|PROJCS["Sphere_Mollweide",GEOGCS["Unknown datum based upon the Authalic Sphere",DATUM["Not_specified_based_on_Authalic_Sphere",SPHEROID["Sphere",6371000,0],AUTHORITY["EPSG","6035"]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Mollweide"],PARAMETER["central_meridian",0],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["ESRI","53009"]]|}
  in
  let t = Proj.CRS.of_wkt wkt in
  (* Top level is ESRI authority, not EPSG *)
  let codes = Proj.CRS.identify t "epsg" in
  Alcotest.(check (list Alcotest.(pair string int))) "expected results" [] codes;
  let codes = Proj.CRS.identify t "esri" in
  Alcotest.(check (list Alcotest.(pair string int)))
    "expected results"
    [ ("53009", 100) ]
    codes

let test_idenfity_invalid_authority () =
  let wkt =
    {|PROJCS["ETRS89-SWE [SWEREF 99 TM]",GEOGCS["ETRS89-SWE [SWEREF 99]",DATUM["SWEREF_99",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],AUTHORITY["EPSG","6619"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4619"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",15],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Northing",NORTH],AXIS["Easting",EAST],AUTHORITY["EPSG","3006"]]|}
  in
  let t = Proj.CRS.of_wkt wkt in
  let codes = Proj.CRS.identify t "Hello, world!" in
  Alcotest.(check (list Alcotest.(pair string int))) "expected results" [] codes

let () =
  Alcotest.run "PROJ CRS tests"
    [
      ( "CRS object tests",
        [
          Alcotest.test_case "invalid definition" `Quick test_invalid_create;
          Alcotest.test_case "create with epsg" `Quick test_create_with_epsg;
          Alcotest.test_case "create with wkt" `Quick test_create_with_wkt;
          Alcotest.test_case "create with name" `Quick test_create_with_name;
          Alcotest.test_case "invalid index for auth name" `Quick
            test_get_invalid_auth_name_index;
          Alcotest.test_case "invalid WKT" `Quick test_invalid_wkt;
          Alcotest.test_case "identify epsg:3006" `Quick test_idenfity_epsg_3006;
          Alcotest.test_case "identify esri:53009" `Quick
            test_idenfity_esri_53009;
          Alcotest.test_case "Invalid authority" `Quick
            test_idenfity_invalid_authority;
        ] );
    ]
