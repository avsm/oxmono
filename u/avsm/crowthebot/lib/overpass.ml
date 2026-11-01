type request = {
  latitude : float;
  longitude : float;
  radius : int;
  tag : string option;
  value : string option;
}

type result = { features : Jsont.json list; limited : bool }
type t = request -> result

let coordinates latitude longitude =
  Float.is_finite latitude && Float.is_finite longitude && latitude >= -90.
  && latitude <= 90. && longitude >= -180. && longitude <= 180.

let validate r =
  if not (coordinates r.latitude r.longitude) then
    invalid_arg "Map coordinates must be valid latitude/longitude.";
  if r.radius < 1 || r.radius > 2000 then
    invalid_arg "Map radius must be between 1 and 2000 metres.";
  Option.iter
    (fun tag ->
      if
        tag = ""
        || String.length tag > 64
        || not
             (String.for_all
                (function
                  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | ':' | '-' ->
                      true
                  | _ -> false)
                tag)
      then invalid_arg "Invalid OpenStreetMap tag key.")
    r.tag;
  if r.value <> None && r.tag = None then
    invalid_arg "A map tag value requires a tag key.";
  Option.iter
    (fun value ->
      if
        value = ""
        || String.length value > 128
        || String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value
      then
        invalid_arg
          "Map tag values must contain 1 to 128 bytes without controls.")
    r.value

let quoted s = Result.get_ok (Jsont_bytesrw.encode_string Jsont.string s)

let query r =
  validate r;
  let around =
    Printf.sprintf "(around:%d,%.7f,%.7f)" r.radius r.latitude r.longitude
  in
  let nearby =
    match r.tag with
    | None ->
        Printf.sprintf "(nwr%s[\"name\"];nwr%s[\"addr:street\"];);" around
          around
    | Some tag ->
        Printf.sprintf "nwr%s[%s%s];" around (quoted tag)
          (Option.fold ~none:"" ~some:(fun value -> "=" ^ quoted value) r.value)
  in
  Printf.sprintf
    "[out:json][timeout:15][maxsize:16777216];is_in(%.7f,%.7f);area._[\"boundary\"=\"administrative\"][\"name\"];out \
     tags 20;%sout center tags 100;"
    r.latitude r.longitude nearby

let member key = function
  | Jsont.Object (fields, _) ->
      List.find_map
        (fun ((k, _), value) -> if k = key then Some value else None)
        fields
  | _ -> None

let string = function Some (Jsont.String (s, _)) -> Some s | _ -> None

let number = function
  | Some (Jsont.Number (n, _)) when Float.is_finite n -> Some n
  | _ -> None

let object_ fields =
  Jsont.Json.object'
    (List.map (fun (key, value) -> ((key, Jsont.Meta.none), value)) fields)

let distance r lat lon =
  let radians x = x *. Float.pi /. 180. in
  let dlat = radians (lat -. r.latitude)
  and dlon = radians (lon -. r.longitude) in
  let a =
    (sin (dlat /. 2.) ** 2.)
    +. cos (radians r.latitude)
       *. cos (radians lat)
       *. (sin (dlon /. 2.) ** 2.)
  in
  6371000. *. 2. *. asin (sqrt (max 0. (min 1. a)))

let decode r body =
  let json =
    match Jsont_bytesrw.decode_string Jsont.json body with
    | Ok json -> json
    | Error _ -> invalid_arg "Overpass returned invalid JSON."
  in
  if member "remark" json <> None then
    invalid_arg
      "Overpass could not complete the query. Try a smaller radius later.";
  let elements =
    match member "elements" json with
    | Some (Jsont.Array (values, _)) when List.length values <= 120 -> values
    | _ -> invalid_arg "Overpass returned an invalid or oversized result."
  in
  let feature element =
    match (string (member "type" element), number (member "id" element)) with
    | Some kind, Some id
      when List.mem kind [ "node"; "way"; "relation"; "area" ]
           && id > 0. && id <= 9007199254740991.
           && floor id = id ->
        let tags = member "tags" element in
        let tag key = Option.bind tags (fun tags -> string (member key tags)) in
        let keys =
          [
            "name";
            "addr:housenumber";
            "addr:street";
            "addr:city";
            "addr:postcode";
            "addr:country";
            "amenity";
            "shop";
            "tourism";
            "place";
            "highway";
            "natural";
            "building";
            "historic";
            "leisure";
            "railway";
            "admin_level";
            "boundary";
          ]
        in
        let tags =
          List.filter_map
            (fun key ->
              Option.map
                (fun value ->
                  (key, Jsont.Json.string (Plugin.clip ~bytes:80 value)))
                (tag key))
            keys
        in
        let position =
          if kind = "node" then Some element else member "center" element
        in
        let position =
          Option.bind position (fun p ->
              match (number (member "lat" p), number (member "lon" p)) with
              | Some lat, Some lon when coordinates lat lon -> Some (lat, lon)
              | _ -> None)
        in
        if tags = [] || (kind <> "area" && position = None) then None
        else
          let rank =
            Option.fold ~none:Float.infinity
              ~some:(fun (lat, lon) -> distance r lat lon)
              position
          in
          let rank =
            if kind = "area" then
              let level =
                Option.value ~default:0
                  (Option.bind (tag "admin_level") int_of_string_opt)
              in
              -.float_of_int (max 0 (min 12 level)) -. 1.
            else rank
          in
          let url =
            if kind = "area" then
              if id > 3600000000. then
                Some
                  (Printf.sprintf "https://www.openstreetmap.org/relation/%.0f"
                     (id -. 3600000000.))
              else None
            else
              Some
                (Printf.sprintf "https://www.openstreetmap.org/%s/%.0f" kind id)
          in
          Some
            ( rank,
              kind,
              id,
              object_
                ([
                   ("type", Jsont.Json.string kind);
                   ("id", Jsont.Json.number id);
                   ("tags", object_ tags);
                   ("contains_query_point", Jsont.Json.bool (kind = "area"));
                 ]
                @ Option.fold ~none:[]
                    ~some:(fun url -> [ ("url", Jsont.Json.string url) ])
                    url
                @ Option.fold ~none:[]
                    ~some:(fun (lat, lon) ->
                      [
                        ("latitude", Jsont.Json.number lat);
                        ("longitude", Jsont.Json.number lon);
                        ( "centre_distance_metres",
                          Jsont.Json.number (floor (distance r lat lon)) );
                      ])
                    position) )
    | _ -> None
  in
  let features =
    List.filter_map feature elements
    |> List.sort_uniq (fun (_, k, id, _) (_, k', id', _) ->
        compare (k, id) (k', id'))
    |> List.sort (fun (a, k, id, _) (b, k', id', _) ->
        compare (a, k, id) (b, k', id'))
    |> List.map (fun (_, _, _, json) -> json)
  in
  let count kind =
    List.length
      (List.filter (fun e -> string (member "type" e) = Some kind) elements)
  in
  {
    features;
    limited = count "area" >= 20 || List.length elements - count "area" >= 100;
  }

let create ~(config : Owntracks_config.overpass) ~fetch ~clock =
  let endpoint =
    Tool_config.endpoint ~allow_http:config.allow_http config.url
  in
  let uri = Uri.of_string endpoint in
  let path = match Uri.path uri with "" -> "/" | path -> path in
  let fetch =
    Fetch.restrict ~methods:[ `POST ] ~under:[ endpoint ]
      ~filter:(fun request ->
        if Fetch.Middleware.Url.path_and_query request.url = path then `Allow
        else `Reject "Only the configured Overpass interpreter is allowed.")
      fetch
    |> Fetch.with_limits ~clock ~min_interval:(Duration.of_sec 1)
         ~max_concurrent:1
  in
  let enabled = config.enabled in
  fun r ->
    if not enabled then
      invalid_arg "Overpass lookups are disabled in the OwnTracks TOML.";
    let query = query r in
    let response =
      try
        Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 20.)
        @@ fun () ->
        Fetch.with_response ~redirects:0
          ~headers:
            Fetch.Header.
              [
                (content_type, media "application/x-www-form-urlencoded");
                (user_agent, "crowthebot/0.1");
              ]
          ~body:(Fetch.String (Uri.encoded_of_query [ ("data", [ query ]) ]))
          fetch `POST endpoint
          (fun response ->
            let status = Fetch.status response in
            if status <> 200 then
              Error (Printf.sprintf "Overpass returned HTTP %d." status)
            else
              Ok
                (Eio.Buf_read.of_flow ~max_size:(1024 * 1024)
                   (Fetch.body response)
                |> Eio.Buf_read.take_all))
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> Error "Overpass request failed, timed out or exceeded 1 MiB."
    in
    match response with
    | Ok body -> decode r body
    | Error message -> invalid_arg message

let resolve t request = t request
