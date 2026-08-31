let () =
  let (east, north), (lon, lat) = Example.example () in
  Printf.printf "Easting: %.3f, Northing: %.3f\n" east north;
  Printf.printf "Longitude: %g, Latitude: %g\n" lon lat
