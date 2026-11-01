module Point = Point
module Bounds = Bounds
module Tags = Tags
module Reference = Reference
module Address = Address
module Place = Place
module Element = Element
module Dataset = Dataset
module Query = Query

type error =
  | Http_status of { status : int; body : string }
  | Invalid_response of string
  | Api_error of string
  | Response_too_large of int

type t = {
  http : Fetch.plain;
  overpass : Uri.t;
  nominatim : Uri.t;
  max_response : int;
}

let base url =
  let uri = Uri.of_string url in
  if
    (not (List.mem (Uri.scheme uri) [ Some "http"; Some "https" ]))
    || Uri.host uri = None
    || Uri.host uri = Some ""
    || Uri.userinfo uri <> None
    || Uri.query uri <> []
    || Uri.fragment uri <> None
  then invalid_arg "Expected an HTTP(S) base URL without userinfo or query";
  let path = Uri.path uri in
  Uri.with_path uri
    (if String.ends_with ~suffix:"/" path then path else path ^ "/")

let v ?(overpass_url = "http://marpe:8888/overpass/api/")
    ?(nominatim_url = "http://marpe:8888/nominatim/")
    ?(max_response = 16 * 1024 * 1024) http =
  if max_response < 1 || max_response = max_int then
    invalid_arg "max_response must be positive and below max_int";
  {
    http = Fetch.restrict http;
    overpass = base overpass_url;
    nominatim = base nominatim_url;
    max_response;
  }

let endpoint base path params =
  Uri.with_path base (Uri.path base ^ path) |> fun uri ->
  Uri.with_query uri (List.map (fun (k, v) -> (k, [ v ])) params)
  |> Uri.to_string

let request t ?headers ?(body = Fetch.Empty) meth url =
  Eio.Switch.run @@ fun sw ->
  let response = Fetch.fetch ~sw ?headers ~body t.http meth url in
  try
    let body =
      Eio.Buf_read.of_flow ~max_size:(t.max_response + 1) (Fetch.body response)
      |> Eio.Buf_read.take_all
    in
    let status = Fetch.status response in
    if status < 200 || status >= 300 then Error (Http_status { status; body })
    else Ok body
  with Eio.Buf_read.Buffer_limit_exceeded ->
    Error (Response_too_large t.max_response)

let ( let* ) = Result.bind
let invalid e = Invalid_response e

let decode codec body =
  Result.map_error invalid (Jsont_bytesrw.decode_string codec body)

let member name = function
  | Jsont.Object (members, _) ->
      List.find_map
        (fun ((key, _), value) -> if key = name then Some value else None)
        members
  | _ -> None

let api_json body =
  let* json = decode Jsont.json body in
  match member "error" json with
  | Some (Jsont.String (message, _)) -> Error (Api_error message)
  | Some _ -> Error (Api_error body)
  | None -> Ok json

let pp_error ppf = function
  | Http_status { status; body } ->
      Format.fprintf ppf "OSM HTTP %d: %s" status body
  | Invalid_response message -> Format.fprintf ppf "OSM response: %s" message
  | Api_error message -> Format.pp_print_string ppf message
  | Response_too_large limit ->
      Format.fprintf ppf "OSM response exceeds %d bytes" limit

module Nominatim = struct
  let place json = Result.map_error invalid (Jsont.Json.decode Place.jsont json)

  type structured = {
    amenity : string option;
    street : string option;
    city : string option;
    county : string option;
    state : string option;
    country : string option;
    postalcode : string option;
  }

  type search_query = Free_form of string | Structured of structured

  let structured ?amenity ?street ?city ?county ?state ?country ?postalcode () =
    Structured { amenity; street; city; county; state; country; postalcode }

  let search_params = function
    | Free_form q ->
        if String.trim q = "" then invalid_arg "q must not be empty";
        [ ("q", q) ]
    | Structured s ->
        let params =
          List.filter_map
            (fun (key, value) ->
              Option.bind value (fun value ->
                  if String.trim value = "" then None else Some (key, value)))
            [
              ("amenity", s.amenity);
              ("street", s.street);
              ("city", s.city);
              ("county", s.county);
              ("state", s.state);
              ("country", s.country);
              ("postalcode", s.postalcode);
            ]
        in
        if params = [] then invalid_arg "Structured search must have a field";
        params

  let bool b = if b then "1" else "0"
  let language = function None -> [] | Some s -> [ ("accept-language", s) ]

  let search_query t ?(limit = 10) ?(addressdetails = true) ?language:lang
      ?(countrycodes = []) ?viewbox ?(bounded = false) query =
    if limit < 1 || limit > 40 then invalid_arg "limit must be in 1..40";
    if bounded && viewbox = None then invalid_arg "bounded requires viewbox";
    let params =
      [
        ("format", "jsonv2");
        ("limit", string_of_int limit);
        ("addressdetails", bool addressdetails);
      ]
      @ search_params query @ language lang
      @ (match viewbox with
        | None -> []
        | Some b ->
            [
              ( "viewbox",
                Printf.sprintf "%.17g,%.17g,%.17g,%.17g" b.Bounds.west b.north
                  b.east b.south );
              ("bounded", bool bounded);
            ])
      @
      if countrycodes = [] then []
      else [ ("countrycodes", String.concat "," countrycodes) ]
    in
    let* body = request t `GET (endpoint t.nominatim "search" params) in
    let* json = api_json body in
    Result.map_error invalid (Jsont.Json.decode (Jsont.list Place.jsont) json)

  let search t ?limit ?addressdetails ?language ?countrycodes ?viewbox ?bounded
      ~q () =
    search_query t ?limit ?addressdetails ?language ?countrycodes ?viewbox
      ?bounded (Free_form q)

  let reverse_at t ?(zoom = 18) ?(addressdetails = true) ?language:lang
      (point : Point.t) =
    if zoom < 0 || zoom > 18 then invalid_arg "zoom must be in 0..18";
    let params =
      [
        ("format", "jsonv2");
        ("lat", Printf.sprintf "%.17g" point.lat);
        ("lon", Printf.sprintf "%.17g" point.lon);
        ("zoom", string_of_int zoom);
        ("addressdetails", bool addressdetails);
      ]
      @ language lang
    in
    let* body = request t `GET (endpoint t.nominatim "reverse" params) in
    let* json = api_json body in
    place json

  let reverse t ?zoom ?addressdetails ?language ~lat ~lon () =
    reverse_at t ?zoom ?addressdetails ?language (Point.v ~lat ~lon)
end

module Overpass = struct
  let query_raw t query =
    if String.trim query = "" then invalid_arg "query must not be empty";
    let headers, body = Fetch.Form.urlencoded [ ("data", query) ] in
    request t ~headers ~body `POST (endpoint t.overpass "interpreter" [])

  let decode_as codec body =
    let* json = api_json body in
    match member "remark" json with
    | Some (Jsont.String (message, _)) when message <> "" ->
        Error (Api_error message)
    | Some (Jsont.String _) | None ->
        Result.map_error invalid (Jsont.Json.decode codec json)
    | Some _ -> Error (Invalid_response "Expected a string remark")

  let decode body = decode_as Dataset.jsont body

  let query t query =
    let* body = query_raw t query in
    decode body

  let run t query =
    let* body = query_raw t (Query.to_ql query) in
    decode_as (Query.jsont query) body

  let status t = request t `GET (endpoint t.overpass "status" [])
end
