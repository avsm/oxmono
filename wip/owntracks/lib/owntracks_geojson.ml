(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type geometry = Point of float array | Line_string of float array array

type properties = {
  name : string;
  timestamp : int option;
  time : string option;
  accuracy : float option;
  speed : float option;
  battery : int option;
  tracker_id : string option;
  points : int option;
  start_time : string option;
  end_time : string option;
}

type feature = { geometry : geometry; properties : properties }
type t = Feature of feature | Feature_collection of feature list

let position_jsont =
  let check p =
    if Array.length p < 2 || Array.length p > 3 then
      Jsont.Error.msgf Jsont.Meta.none "A position needs 2 or 3 coordinates"
  in
  Jsont.iter ~dec:check ~enc:check (Jsont.array Owntracks_codec.number)

let line_jsont =
  let check ps =
    if Array.length ps < 2 then
      Jsont.Error.msgf Jsont.Meta.none "A LineString needs at least 2 positions"
  in
  Jsont.iter ~dec:check ~enc:check (Jsont.array position_jsont)

let geometry_jsont =
  let body kind coordinates =
    Jsont.Object.map ~kind Fun.id
    |> Jsont.Object.mem "coordinates" coordinates ~enc:Fun.id
    |> Jsont.Object.finish
  in
  let point =
    Jsont.Object.Case.map "Point" (body "Point" position_jsont) ~dec:(fun p ->
        Point p)
  in
  let line =
    Jsont.Object.Case.map "LineString" (body "LineString" line_jsont)
      ~dec:(fun ps -> Line_string ps)
  in
  let enc_case = function
    | Point p -> Jsont.Object.Case.value point p
    | Line_string ps -> Jsont.Object.Case.value line ps
  in
  Jsont.Object.map ~kind:"Geometry" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case
       Jsont.Object.Case.[ make point; make line ]
  |> Jsont.Object.finish

let properties_jsont =
  let make name timestamp time accuracy speed battery tracker_id points
      start_time end_time =
    {
      name;
      timestamp;
      time;
      accuracy;
      speed;
      battery;
      tracker_id;
      points;
      start_time;
      end_time;
    }
  in
  Jsont.Object.map ~kind:"OwnTracks properties" make
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun p -> p.name)
  |> Jsont.Object.opt_mem "timestamp" Owntracks_codec.integer ~enc:(fun p ->
      p.timestamp)
  |> Jsont.Object.opt_mem "time" Jsont.string ~enc:(fun p -> p.time)
  |> Jsont.Object.opt_mem "accuracy" Owntracks_codec.number ~enc:(fun p ->
      p.accuracy)
  |> Jsont.Object.opt_mem "speed" Owntracks_codec.number ~enc:(fun p -> p.speed)
  |> Jsont.Object.opt_mem "battery" Owntracks_codec.integer ~enc:(fun p ->
      p.battery)
  |> Jsont.Object.opt_mem "tracker_id" Jsont.string ~enc:(fun p -> p.tracker_id)
  |> Jsont.Object.opt_mem "points" Owntracks_codec.integer ~enc:(fun p ->
      p.points)
  |> Jsont.Object.opt_mem "start_time" Jsont.string ~enc:(fun p -> p.start_time)
  |> Jsont.Object.opt_mem "end_time" Jsont.string ~enc:(fun p -> p.end_time)
  |> Jsont.Object.finish

let feature_body =
  Jsont.Object.map ~kind:"Feature" (fun geometry properties ->
      { geometry; properties })
  |> Jsont.Object.mem "geometry" geometry_jsont ~enc:(fun f -> f.geometry)
  |> Jsont.Object.mem "properties" properties_jsont ~enc:(fun f -> f.properties)
  |> Jsont.Object.finish

let tagged_type tag body =
  let case = Jsont.Object.Case.map tag body ~dec:Fun.id in
  Jsont.Object.map ~kind:tag Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id
       ~enc_case:(fun v -> Jsont.Object.Case.value case v)
       [ Jsont.Object.Case.make case ]
  |> Jsont.Object.finish

let jsont =
  let feature =
    Jsont.Object.Case.map "Feature" feature_body ~dec:(fun f -> Feature f)
  in
  let collection_body =
    Jsont.Object.map ~kind:"FeatureCollection" Fun.id
    |> Jsont.Object.mem "features"
         (Jsont.list (tagged_type "Feature" feature_body))
         ~enc:Fun.id
    |> Jsont.Object.finish
  in
  let collection =
    Jsont.Object.Case.map "FeatureCollection" collection_body ~dec:(fun fs ->
        Feature_collection fs)
  in
  let enc_case = function
    | Feature f -> Jsont.Object.Case.value feature f
    | Feature_collection fs -> Jsont.Object.Case.value collection fs
  in
  Jsont.Object.map ~kind:"OwnTracks GeoJSON" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case
       Jsont.Object.Case.[ make feature; make collection ]
  |> Jsont.Object.finish

let position loc =
  let lon = Owntracks_location.lon loc and lat = Owntracks_location.lat loc in
  match Owntracks_location.alt loc with
  | None -> [| lon; lat |]
  | Some alt -> [| lon; lat; alt |]

let empty_properties name =
  {
    name;
    timestamp = None;
    time = None;
    accuracy = None;
    speed = None;
    battery = None;
    tracker_id = None;
    points = None;
    start_time = None;
    end_time = None;
  }

let point_feature ~device_name loc =
  let timestamp = Owntracks_location.tst loc in
  let properties =
    {
      (empty_properties device_name) with
      timestamp = Some timestamp;
      time = Some (Owntracks_location.format_timestamp timestamp);
      accuracy = Owntracks_location.acc loc;
      speed = Owntracks_location.vel loc;
      battery = Owntracks_location.batt loc;
      tracker_id = Owntracks_location.tid loc;
    }
  in
  Feature { geometry = Point (position loc); properties }

let linestring_feature ~device_name locations =
  let locations = Array.of_list locations in
  let count = Array.length locations in
  if count < 2 then invalid_arg "A LineString needs at least 2 locations";
  Array.stable_sort
    (fun a b ->
      Int.compare (Owntracks_location.tst a) (Owntracks_location.tst b))
    locations;
  let topic = Owntracks_location.topic locations.(0) in
  if Array.exists (fun loc -> Owntracks_location.topic loc <> topic) locations
  then invalid_arg "A LineString must contain one device's locations";
  let properties =
    {
      (empty_properties device_name) with
      points = Some count;
      start_time =
        Some
          (Owntracks_location.format_timestamp
             (Owntracks_location.tst locations.(0)));
      end_time =
        Some
          (Owntracks_location.format_timestamp
             (Owntracks_location.tst locations.(count - 1)));
    }
  in
  Feature { geometry = Line_string (Array.map position locations); properties }

let collection features =
  let add acc = function
    | Feature f -> f :: acc
    | Feature_collection fs -> List.rev_append fs acc
  in
  Feature_collection (List.rev (List.fold_left add [] features))

let to_string value =
  Jsont_bytesrw.encode_string ~buf:(Bytes.create 4096) jsont value
