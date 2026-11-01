type t = { south : float; west : float; north : float; east : float }

let v ~south ~west ~north ~east =
  ignore (Point.v ~lat:south ~lon:west);
  ignore (Point.v ~lat:north ~lon:east);
  if south > north then invalid_arg "south must not exceed north";
  { south; west; north; east }

let jsont =
  Jsont.Object.map (fun south west north east ->
      Codec.checked (fun () -> v ~south ~west ~north ~east))
  |> Jsont.Object.mem "minlat" Jsont.number ~enc:(fun b -> b.south)
  |> Jsont.Object.mem "minlon" Jsont.number ~enc:(fun b -> b.west)
  |> Jsont.Object.mem "maxlat" Jsont.number ~enc:(fun b -> b.north)
  |> Jsont.Object.mem "maxlon" Jsont.number ~enc:(fun b -> b.east)
  |> Jsont.Object.finish

let nominatim_jsont =
  Jsont.map
    (Jsont.list Codec.number_string)
    ~dec:(function
      | [ south; north; west; east ] ->
          Codec.checked (fun () -> v ~south ~west ~north ~east)
      | _ ->
          Jsont.Error.msgf Jsont.Meta.none "Expected four bounding coordinates")
    ~enc:(fun b -> [ b.south; b.north; b.west; b.east ])
