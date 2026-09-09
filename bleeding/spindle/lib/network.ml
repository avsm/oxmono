(* SPDX-License-Identifier: ISC *)
open Json
module U = Httpz_uri

type t = { fetch : Fetch_httpz.t; plc : string; allow_http : bool }

let origin ~allow_http value =
  let uri = U.of_string_exn value in
  let scheme =
    match U.scheme uri with
    | This "https" -> "https"
    | This "http" when allow_http -> "http"
    | _ -> invalid "HTTPS is required for remote services"
  in
  if U.has_userinfo uri || U.has_query uri || U.has_fragment uri then
    invalid "service URL has credentials, query or fragment";
  let host =
    match U.encoded_host uri with
    | This host when host <> "" -> host
    | _ -> invalid "service URL has no host"
  in
  let host = if String.contains host ':' then "[" ^ host ^ "]" else host in
  let port =
    match U.port uri with
    | Null -> ""
    | This p when p > 0 && p <= 65535 ->
        if (scheme = "https" && p = 443) || (scheme = "http" && p = 80) then ""
        else ":" ^ string_of_int p
    | _ -> invalid "service port is out of range"
  in
  scheme ^ "://" ^ host ^ port

let v ~allow_http ~plc system =
  let plc = origin ~allow_http plc in
  let fetch =
    Fetch_httpz.v system#net ~clock:system#mono_clock ~https:Httpz_tls.system
      ~max_response:(16 * 1024 * 1024)
      ()
  in
  { fetch; plc; allow_http }

let read ?(limit = 1048576) t url =
  let uri = U.of_string_exn url in
  let base = U.with_encoded_query uri Null |> fun uri -> U.to_string uri in
  let host = origin ~allow_http:t.allow_http base in
  let fetch = Fetch.restrict t.fetch ~under:[ host ] in
  Fetch.read ~limit fetch url

let json t url = decode (read t url)

let query url fields =
  U.of_string_exn url |> fun uri -> U.set_query_params uri fields |> U.to_string

let knot t name =
  let value =
    if String.contains name '/' then name
    else (if t.allow_http then "http://" else "https://") ^ name
  in
  origin ~allow_http:t.allow_http value

let resolve t value =
  ignore (did value);
  let url =
    if String.starts_with ~prefix:"did:plc:" value then t.plc ^ "/" ^ value
    else if String.starts_with ~prefix:"did:web:" value then (
      let host = String.sub value 8 (String.length value - 8) in
      if String.contains host ':' then invalid "path-based did:web unsupported";
      let host =
        match U.percent_decode host with
        | This host -> host
        | Null -> invalid "invalid did:web encoding"
      in
      let local =
        host = "localhost" || String.starts_with ~prefix:"localhost:" host
      in
      if local then (
        if not t.allow_http then
          invalid "localhost DID requires development mode")
      else if not (Atp.Handle.is_valid host) then invalid "invalid did:web host";
      knot t host ^ "/.well-known/did.json")
    else invalid "unsupported DID method"
  in
  let raw = read ~limit:65536 t url in
  if get "id" (decode raw) <> value then invalid "DID document mismatch";
  raw

let service document name kind =
  let services =
    match field "service" document with None -> [] | Some value -> list value
  in
  let id = get "id" document in
  List.find_opt
    (fun value ->
      let key = get "id" value in
      (key = "#" ^ name || key = id ^ "#" ^ name) && get "type" value = kind)
    services
  |> Option.map (get "serviceEndpoint")

let pds t owner =
  let document = decode (resolve t owner) in
  match service document "atproto_pds" "AtprotoPersonalDataServer" with
  | Some url ->
      let path = U.encoded_path (U.of_string_exn url) in
      if path <> "" && path <> "/" then invalid "PDS endpoint has a path prefix";
      origin ~allow_http:t.allow_http url
  | None -> invalid "identity has no PDS"

let records t owner collection =
  let url = pds t owner ^ "/xrpc/com.atproto.repo.listRecords" in
  let rec pages cursor count acc =
    if count >= 100 then invalid "record pagination exceeds 100 pages";
    let result =
      json t
        (query url
           ([ ("repo", owner); ("collection", collection); ("limit", "100") ]
           @ match cursor with None -> [] | Some c -> [ ("cursor", c) ]))
    in
    let values = list (required "records" result) in
    let acc = List.rev_append values acc in
    match field "cursor" result with
    | Some (Jsont.String (next, _)) when next <> "" ->
        if cursor = Some next then invalid "repeated record cursor";
        pages (Some next) (count + 1) acc
    | _ -> List.rev acc
  in
  pages None 0 []
