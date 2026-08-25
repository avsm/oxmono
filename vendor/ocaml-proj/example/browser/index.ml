open Brr

let () =
  let (east, north), (lon, lat) = Example.example () in
  let utm = Printf.sprintf "Easting: %.3f, Northing: %.3f\n" east north in
  let wgs = Printf.sprintf "Longitude: %g, Latitude: %g\n" lon lat in
  let data = El.div [ El.p [ El.txt' utm ]; El.p [ El.txt' wgs ] ] in
  let body = Document.body G.document in
  El.append_children body [ data ]
