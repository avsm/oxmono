type t = { lat : float; lon : float }

let v ~lat ~lon =
  if
    (not (Float.is_finite lat))
    || lat < -90. || lat > 90.
    || (not (Float.is_finite lon))
    || lon < -180. || lon > 180.
  then invalid_arg "Coordinates must be finite degrees in geographic range";
  { lat; lon }

let jsont =
  Jsont.Object.map (fun lat lon -> Codec.checked (fun () -> v ~lat ~lon))
  |> Jsont.Object.mem "lat" Jsont.number ~enc:(fun p -> p.lat)
  |> Jsont.Object.mem "lon" Jsont.number ~enc:(fun p -> p.lon)
  |> Jsont.Object.finish
