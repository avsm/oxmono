(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
let checks = ref 0
let check name b = incr checks; if not b then failwith name
let get = function Ok v -> v | Error e -> failwith e
let decode_links s = match Fetch.Header.decode_links s with
  | Some links -> Ok links | None -> Error "Invalid Link syntax"
let date s = get (Memento.Datetime.of_json s)
let a : Memento.capture =
  { uri = "https://archive.example/1"; datetime = date "2020-01-01T00:00:00Z" }
let b : Memento.capture =
  { uri = "https://archive.example/2"; datetime = date "2020-01-03T00:00:00Z" }
let original = "https://original.example/broken"
let basic : Memento.Timemap.t = {
  original_uri = original; timegate_uri = Some "https://archive.example/gate";
  timemap_uri = Some { json_format = Some "https://archive.example/map.json";
                      link_format = Some "https://archive.example/map" };
  mementos = Some { list = [a; b]; first = Some a; last = Some b; closest = None };
  pages = None; timemap_index = None;
}
let encode t = get (Jsont_bytesrw.encode_string Memento.Timemap.jsont t)
let decode s = Jsont_bytesrw.decode_string Memento.Timemap.jsont s
let headers pairs = Http.Header.of_list pairs
let response_headers response =
  List.map (fun (f : Proffer.Headers.field) -> f.spelling, f.value)
    (Proffer_mock.headers response)
let serve_json : (Proffer.Resp.respond @ local -> unit) @ portable =
  fun respond -> Memento_proffer.json_timemap respond basic
let serve_gate : (Proffer.Resp.respond @ local -> unit) @ portable =
  fun respond -> Memento_proffer.timegate respond ~original a
let () =
  check "portable JSON server captures module value"
    (Proffer_mock.status (Proffer_mock.describe serve_json) = Httpz.Res.Success);
  check "portable TimeGate captures module value"
    (Proffer_mock.status (Proffer_mock.describe serve_gate) = Httpz.Res.Found);
  let req = Proffer.Req.v ~meth:Httpz.Method.Get ~target:"/"
      ~headers:(Proffer.Headers.of_list
        ["Accept-Datetime", Memento.Datetime.to_http a.datetime]) () in
  check "server parses Accept-Datetime"
    (Memento_proffer.accept_datetime req = Ok (Some a.datetime));
  let req = Proffer.Req.v ~meth:Httpz.Method.Get ~target:"/"
      ~headers:(Proffer.Headers.of_list ["Accept-Datetime", "bad"]) () in
  check "server rejects invalid Accept-Datetime"
    (Result.is_error (Memento_proffer.accept_datetime req));
  let req = Proffer.Req.v ~meth:Httpz.Method.Get ~target:"/"
      ~headers:(Proffer.Headers.of_list
        ["Accept-Datetime", Memento.Datetime.to_http a.datetime;
         "accept-datetime", Memento.Datetime.to_http a.datetime]) () in
  check "server rejects repeated Accept-Datetime"
    (Result.is_error (Memento_proffer.accept_datetime req));
  check "JSON roundtrip" (get (decode (encode basic)) = basic);
  let fractional = date "2020-01-01T02:00:00.123400+02:00" in
  check "UTC fractions retained"
    (Memento.Datetime.to_json fractional = "2020-01-01T00:00:00.1234Z");
  check "HTTP dates omit fractions"
    (Memento.Datetime.to_http fractional = "Wed, 01 Jan 2020 00:00:00 GMT");
  check "HTTP date truncates picoseconds without rounding"
    (Memento.Datetime.to_http (date "2020-01-01T00:00:00.999999999999Z")
      = "Wed, 01 Jan 2020 00:00:00 GMT");
  check "HTTP date roundtrip"
    (get (Memento.Datetime.of_http (Memento.Datetime.to_http a.datetime)) = a.datetime);
  check "obsolete HTTP date"
    (Memento.Datetime.of_http "Sunday, 06-Nov-94 08:49:37 GMT" |> Result.is_ok);
  List.iter (fun s -> check "bad datetime" (Result.is_error (Memento.Datetime.of_json s)))
    ["2020-02-30T00:00:00Z"; "2020-01-01"; "invalid"];
  check "oversized HTTP date rejected"
    (Result.is_error (Memento.Datetime.of_http (String.make 40000 'x')));
  List.iter (fun s -> check "invalid JSON map" (Result.is_error (decode s)))
    [ {|{}|}; {|{"original_uri":"relative","mementos":{"list":[]}}|};
      {|{"original_uri":"https://example/%zz","mementos":{"list":[]}}|};
      {|{"original_uri":"https://example/","mementos":{}}|};
      {|{"original_uri":"https://example/","mementos":{"list":[]},"timemap_index":[]}|};
      {|{"original_uri":"https://example/","timemap_index":[],"pages":{}}|};
      {|{"original_uri":"https://example/","mementos":{"list":[{"uri":"https://archive/","datetime":"bad"}]}}|};
      {|{"original_uri":"https://example/","timemap_index":[{"uri":"https://archive/","from":"2021-01-01T00:00:00Z","until":"2020-01-01T00:00:00Z"}]}|};
      {|{"original_uri":"https://example/","timemap_index":[{"uri":"https://archive/","memento_compliant":"maybe"}]}|} ];
  check "URI schemes beyond HTTP"
    (Result.is_ok (decode {|{"original_uri":"urn:example:original","mementos":{"list":[]}}|}));
  check "unknown extensions allowed"
    (Result.is_ok (decode {|{"original_uri":"https://example/","mementos":{"list":[],"extra":true},"extra":{}}|}));
  let r : Memento.Timemap.reference = { uri = "https://archive.example/page/2";
    from = Some a.datetime; until = Some b.datetime;
    memento_compliant = Some true; archive_id = Some "test" } in
  let index = { basic with mementos = None; timemap_index = Some [r] } in
  check "index roundtrip" (get (decode (encode index)) = index);
  check "index references" (Memento.Timemap.references index = [r]);
  let paged = { basic with pages = Some { prev = None; next = Some r };
      mementos = Some { list = [a]; first = Some a; last = Some b; closest = Some a } } in
  check "paged global endpoints" (get (decode (encode paged)) = paged);
  check "index children may omit page links"
    (Result.is_ok (decode (encode { paged with pages = None })));
  check "null page boundary"
    (Result.is_ok (decode {|{"original_uri":"https://example/","mementos":{"list":[]},"pages":{"prev":null}}|}));
  check "invalid value rejected during encoding"
    (Result.is_error (Jsont_bytesrw.encode_string Memento.Timemap.jsont
      { basic with timemap_index = Some [] }));
  let links = [Fetch.Header.link ~rel:"original" original;
    Fetch.Header.link ~rel:"timegate" "https://archive.example/gate";
    Memento.Link.memento ~rels:["first"; "prev"] a;
    Memento.Link.memento ~rels:["last"; "next"] b;
    Memento.Link.timemap ~from:a.datetime ~until:b.datetime
      ~media_type:"application/json" "https://archive.example/map.json"] in
  let wire = Fetch.Header.encode_links links in
  let parsed = get (decode_links wire) in
  check "multiple relation tokens"
    (Fetch.Header.link_rel "prev" parsed <> None && Fetch.Header.link_rel "memento" parsed <> None);
  check "Link date comma handling" (get (Memento.Link.captures parsed) = [a; b]);
  check "unknown Link attributes"
    (let l = get (decode_links {|<https://archive.example/1>; rel="memento"; datetime="Wed, 01 Jan 2020 00:00:00 GMT"; license="urn:license:test"|}) in
     List.assoc_opt "license" (List.hd l).Fetch.Header.params = Some "urn:license:test");
  List.iter (fun s -> check "bad capture link"
      (match decode_links s with Error _ -> true
       | Ok l -> Result.is_error (Memento.Link.captures l)))
    [ {|<https://archive.example/1>; rel="memento"|};
      {|<https://archive.example/1>; rel="memento"; datetime="bad"|};
      {|<https://archive.example/1>; rel="memento"; datetime="Wed, 01 Jan 2020 00:00:00 GMT"; datetime="Wed, 01 Jan 2020 00:00:00 GMT"|} ];
  let md = get (Memento.Headers.read (headers
    ["Link", Fetch.Header.encode_links [Fetch.Header.link ~rel:"original" original];
     "Link", Fetch.Header.encode_links [Fetch.Header.link ~rel:"timegate" "https://archive.example/gate"];
     "Memento-Datetime", Memento.Datetime.to_http a.datetime;
     "Vary", "Accept, ACCEPT-DATETIME"])) in
  check "repeated Link fields" (List.length md.links = 2);
  check "combined resource roles" (md.is_timegate && md.is_memento);
  check "malformed response date"
    (Result.is_error (Memento.Headers.read (headers ["Memento-Datetime", "bad"])));
  check "repeated response date"
    (Result.is_error (Memento.Headers.read (headers
      ["Memento-Datetime", Memento.Datetime.to_http a.datetime;
       "Memento-Datetime", Memento.Datetime.to_http b.datetime])));
  check "Last-Modified is not capture datetime"
    (not (get (Memento.Headers.read (headers
      ["Last-Modified", Memento.Datetime.to_http a.datetime]))).is_memento);
  check "nearest ties prefer earlier"
    (Memento.nearest (date "2020-01-02T00:00:00Z") [b; a] = Some a);
  check "empty capture list" (Memento.nearest a.datetime [] = None);
  let response = Proffer_mock.describe (fun respond ->
    Memento_proffer.timegate respond ~original a) in
  check "Proffer TimeGate status" (Proffer_mock.status response = Httpz.Res.Found);
  let md = get (Memento.Headers.read (headers (response_headers response))) in
  check "TimeGate no capture datetime" (md.is_timegate && md.datetime = None);
  let response = Proffer_mock.describe (fun respond ->
    Memento_proffer.json_timemap respond paged) in
  check "Proffer JSON roundtrip" (get (decode (Proffer_mock.body response)) = paged);
  let response = Proffer_mock.describe (fun respond ->
    Memento_proffer.link_timemap respond links) in
  check "Proffer link-format"
    (response_headers response
      |> List.exists (fun (n, v) -> String.lowercase_ascii n = "content-type" && v = "application/link-format"));
  check "Proffer Link roundtrip"
    (get (Memento.Link.captures (get (decode_links (Proffer_mock.body response)))) = [a; b]);
  Eio_mock.Backend.run @@ fun () ->
  let calls = ref [] in
  let client = Fetch_mock.client (fun req ->
    let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
    calls := url :: !calls;
    match url with
    | "https://original.example/broken" ->
        Fetch_mock.respond ~status:404
          ~headers:(headers ["Link", "</gate>; rel=\"timegate\"; anchor=\"https://original.example/broken\""]) "" req
    | "https://original.example/gate" | "https://archive.example/gate" ->
        check "Accept-Datetime encoded"
          (Http.Header.get req.headers "accept-datetime" = Some (Memento.Datetime.to_http a.datetime));
        check "negotiation uses HEAD" (req.meth = `HEAD);
        Fetch_mock.respond ~status:302
          ~headers:(headers (Memento.Headers.timegate ~original a)) "" req
    | "https://archive.example/1" ->
        check "datetime survives cross-origin redirect"
          (Http.Header.get req.headers "accept-datetime" <> None);
        Fetch_mock.respond ~headers:(headers (Memento.Headers.memento ~original a)) "" req
    | "https://archive.example/map.json" ->
        Fetch_mock.respond ~headers:(headers ["Content-Type", "application/json; charset=utf-8"])
          (encode paged) req
    | "https://archive.example/map" ->
        Fetch_mock.respond ~headers:(headers ["Content-Type", "application/link-format"]) wire req
    | "https://archive.example/direct" ->
        Fetch_mock.respond ~headers:(headers
          (Memento.Headers.memento ~original a @
           ["Vary", "Accept-Datetime"; "Content-Location", "/1"])) "" req
    | "https://archive.example/no-distinct-uri" ->
        Fetch_mock.respond ~headers:(headers
          (Memento.Headers.memento ~original a @ ["Vary", "accept-datetime"])) "" req
    | "https://archive.example/excluded" ->
        Fetch_mock.respond ~headers:(headers
          ["Link", "<http://mementoweb.org/terms/donotnegotiate>; rel=type"]) "" req
    | "https://archive.example/deep.json" ->
        Fetch_mock.respond ~headers:(headers ["Content-Type", "application/json"])
          ("{\"extra\":" ^ String.make 130 '[' ^ "0" ^ String.make 130 ']' ^ "}") req
    | "https://archive.example/not-memento" -> Fetch_mock.respond "" req
    | "https://archive.example/cycle" -> Fetch_mock.respond ~status:302
        ~headers:(headers ["Location", "/cycle"]) "" req
    | _ -> Fetch_mock.respond ~status:404 "" req) in
  let found = get (Memento_fetch.find client ~datetime:a.datetime original) in
  check "discovery through broken original" (found.url = a.uri && found.metadata.is_memento);
  calls := [];
  ignore (get (Memento_fetch.find ~timegate:"https://archive.example/gate"
    client ~datetime:a.datetime original));
  check "explicit archive skips original" (List.length !calls = 2 && not (List.mem original !calls));
  let direct = get (Memento_fetch.negotiate client ~datetime:a.datetime
    "https://archive.example/direct") in
  check "direct TimeGate selects Content-Location" (direct.capture = Some a);
  let no_distinct_uri = get (Memento_fetch.negotiate client ~datetime:a.datetime
    "https://archive.example/no-distinct-uri") in
  check "direct TimeGate without Memento URI" (no_distinct_uri.capture = None);
  check "negotiation exclusion honoured"
    (Result.is_error (Memento_fetch.find client ~datetime:a.datetime
      "https://archive.example/excluded"));
  check "JSON nesting bounded"
    (Result.is_error (Memento_fetch.timemap client "https://archive.example/deep.json"));
  let map = get (Memento_fetch.timemap client "https://archive.example/map.json") in
  check "client JSON map" (get (Memento_fetch.captures map) = [a]);
  check "client link-format map"
    (get (Memento_fetch.captures (get (Memento_fetch.timemap client "https://archive.example/map"))) = [a; b]);
  check "body bound enforced"
    (Result.is_error (Memento_fetch.timemap ~limit:20 client "https://archive.example/map.json"));
  check "HTTP error reported" (Result.is_error (Memento_fetch.timemap client "https://archive.example/missing"));
  check "ordinary response rejected"
    (Result.is_error (Memento_fetch.negotiate client ~datetime:a.datetime "https://archive.example/not-memento"));
  let denied = Fetch.restrict ~filter:(fun _ -> `Reject "denied") client in
  let before = List.length !calls in
  check "caller URL policy preserved"
    (try ignore (Memento_fetch.timemap denied "https://archive.example/map"); false with _ -> List.length !calls = before);
  check "redirect loop bounded"
    (try ignore (Memento_fetch.negotiate ~redirects:2 client ~datetime:a.datetime
        "https://archive.example/cycle"); false with _ -> true);
  Printf.printf "Memento: %d checks passed\n" !checks
