(* This example is from https://proj.org/en/stable/development/quickstart.html# *)

let example () =
  let src = "EPSG:4326" in
  let tgt = "+proj=utm +zone=32 +datum=WGS84" in
  let crs =
    Proj.Transformation.of_string ~src tgt
    |> Proj.Transformation.normalize_for_visualization
  in
  let copenhagen = Proj.Coord.make ~x:12. ~y:55. ~z:0. ~t:0. in
  let utm = Proj.Transformation.transform crs copenhagen in
  let back = Proj.Transformation.transform ~direction:Inverse crs utm in
  let easting_and_northing = (Proj.Coord.x utm, Proj.Coord.y utm) in
  let lon_and_lat = (Proj.Coord.x back, Proj.Coord.y back) in
  (easting_and_northing, lon_and_lat)
