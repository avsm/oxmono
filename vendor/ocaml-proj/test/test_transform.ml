module Proj = Proj_c

let test_create_from_string () =
  ignore (Proj.Transformation.of_string ~src:"epsg:4326" "epsg:3006")

let test_create_from_string_invalid_src () =
  Alcotest.check_raises "Garbage wkt" (Failure "Invalid PROJ string syntax")
    (fun () ->
      let _ = Proj.Transformation.of_string ~src:"Hello, world!" "epsg:3006" in
      ())

let test_create_from_string_invalid_target () =
  Alcotest.check_raises "Garbage wkt" (Failure "Invalid PROJ string syntax")
    (fun () ->
      let _ = Proj.Transformation.of_string ~src:"Hello, world!" "epsg:3006" in
      ())

let test_create_from_crs () =
  let src = Proj.CRS.of_string "epsg:4326"
  and tgt = Proj.CRS.of_string "epsg:3006" in
  ignore (Proj.Transformation.of_crs ~src tgt)

let test_inverse_round_trip_transform () =
  let crs = Proj.Transformation.of_string ~src:"epsg:4326" "esri:53009" in
  let copenhagen = Proj.Coord.make ~x:12. ~y:55. ~z:0. ~t:0. in
  let mollweide = Proj.Transformation.transform crs copenhagen in
  let restored =
    Proj.Transformation.transform ~direction:Inverse crs mollweide
  in
  let x, y = (Proj.Coord.x restored, Proj.Coord.y restored) in
  Alcotest.(check (float 1e-6)) "check x" 12. x;
  Alcotest.(check (float 1e-6)) "check y" 55. y

let test_manual_round_trip_transform () =
  let t1 = Proj.Transformation.of_string ~src:"epsg:4326" "esri:53009"
  and t2 = Proj.Transformation.of_string ~src:"esri:53009" "epsg:4326" in
  let copenhagen = Proj.Coord.make ~x:12. ~y:55. ~z:0. ~t:0. in
  let mollweide = Proj.Transformation.transform t1 copenhagen in
  let restored = Proj.Transformation.transform t2 mollweide in
  let x, y = (Proj.Coord.x restored, Proj.Coord.y restored) in
  Alcotest.(check (float 1e-6)) "check x" 12. x;
  Alcotest.(check (float 1e-6)) "check y" 55. y

let () =
  Alcotest.run " PROJ Transform tests"
    [
      ( "Transformation object tests",
        [
          Alcotest.test_case "simple create from string" `Quick
            test_create_from_string;
          Alcotest.test_case "create from string but invalid src" `Quick
            test_create_from_string_invalid_src;
          Alcotest.test_case "create from string but invalid target" `Quick
            test_create_from_string_invalid_target;
          Alcotest.test_case "simple create from CRS.t" `Quick
            test_create_from_crs;
          Alcotest.test_case "test inverse round trip" `Quick
            test_inverse_round_trip_transform;
          Alcotest.test_case "test manual round trip" `Quick
            test_manual_round_trip_transform;
        ] );
    ]
