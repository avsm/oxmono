module Url = Fetch.Middleware.Url
type t = { client : Fetch.plain; root_url : Url.t; limits : Httpz_dav.limits }
exception Protocol_error of string
type http_error = { status : int; headers : Http.Header.t; body : string;
                    truncated : bool; dav_errors : Httpz_dav.element list }
exception Http_error of http_error
let protocol = function Ok v -> v | Error s -> raise (Protocol_error s)
let url_exn = function Ok v -> v | Error s -> invalid_arg ("Fetch_dav: " ^ s)
let root t = Url.to_string t.root_url
let v ?(limits = Httpz_dav.default_limits) ~root client =
  Httpz_dav.validate_limits limits;
  let root_url = url_exn (Url.of_string root) in
  let u = Httpz_uri.of_string_exn root in
  if Httpz_uri.encoded_query u <> Null || Httpz_uri.encoded_fragment u <> Null ||
    not (String.ends_with ~suffix:"/" (Httpz_uri.encoded_path u)) then
    invalid_arg "Fetch_dav: root must be a collection URL without query or fragment";
  { client = Fetch.restrict ~under:[Url.to_string root_url] client; root_url; limits }
let resolve t reference =
  let u = Httpz_uri.of_string_exn reference in
  if Httpz_uri.encoded_fragment u <> Null then invalid_arg "Fetch_dav: fragments are not resource paths";
  let url = url_exn (Url.resolve ~base:t.root_url reference) in
  if not (Url.under ~prefix:t.root_url url) then invalid_arg "Fetch_dav: URL outside root";
  Url.to_string url
let child t ~collection name =
  if name = "" || name = "." || name = ".." || String.contains name '/' || String.contains name '\\' then
    invalid_arg "Fetch_dav.child: invalid segment";
  let collection = resolve t collection in
  let u = Httpz_uri.of_string_exn collection in
  if Httpz_uri.encoded_query u <> Null || not (String.ends_with ~suffix:"/" (Httpz_uri.encoded_path u)) then
    invalid_arg "Fetch_dav.child: collection must end in / and have no query";
  resolve t (collection ^ Httpz_uri.percent_encode ~component:`Unreserved name)

module Header = struct
  let depth = Fetch.Header.v "Depth" ~encode:Httpz_dav.encode_depth ~decode:Httpz_dav.decode_depth
  let overwrite = Fetch.Header.v "Overwrite" ~encode:(function true -> "T" | false -> "F")
    ~decode:(fun s -> match String.trim s with "T" -> Some true | "F" -> Some false | _ -> None)
  let lock_token = Fetch.Header.v "Lock-Token" ~encode:Httpz_dav.Token.encode ~decode:Httpz_dav.Token.decode
  let split_dav s =
    let rec scan start pos coded acc =
      if pos = String.length s then
        if coded then None else Some (List.rev (String.trim (String.sub s start (pos-start)) :: acc))
      else match s.[pos] with
        | '<' when coded -> None
        | '>' when not coded -> None
        | '<' -> scan start (pos+1) true acc
        | '>' -> scan start (pos+1) false acc
        | ',' when not coded -> scan (pos+1) (pos+1) false
            (String.trim (String.sub s start (pos-start)) :: acc)
        | _ -> scan start (pos+1) coded acc
    in
    Option.map (List.filter ((<>) "")) (scan 0 0 false [])
  let dav = Fetch.Header.v ~list_valued:true "DAV" ~encode:(String.concat ", ") ~decode:split_dav
end

let read_bounded limit response =
  let flow = Fetch.body response in
  let scratch = Cstruct.create (min limit 16384) in
  let b = Buffer.create (min limit 4096) in
  let rec loop remaining =
    if remaining = 0 then
      (try ignore (Eio.Flow.single_read flow (Cstruct.sub scratch 0 1)); true with End_of_file -> false)
    else try
      let n = Eio.Flow.single_read flow (Cstruct.sub scratch 0 (min remaining (Cstruct.length scratch))) in
      Buffer.add_string b (Cstruct.to_string (Cstruct.sub scratch 0 n));
      loop (remaining-n)
    with End_of_file -> false
  in
  let truncated = loop limit in
  Buffer.contents b, truncated
let encoding response body =
  match Fetch.header Fetch.Header.content_type response with
  | None -> raise (Protocol_error "missing or malformed XML Content-Type")
  | Some media ->
      if media.media <> "application/xml" && media.media <> "text/xml" then
        raise (Protocol_error "expected application/xml or text/xml");
      if List.exists (fun prefix -> String.starts_with ~prefix body)
        ["\239\187\191"; "\254\255"; "\255\254"] then None else
      let charset = List.filter (fun (k, _) -> k = "charset") media.params in
      match charset with
      | [] -> None
      | [_, s] -> Some (match String.lowercase_ascii s with
          | "utf-8" | "utf8" -> `UTF_8 | "utf-16" -> `UTF_16
          | "utf-16be" -> `UTF_16BE | "utf-16le" -> `UTF_16LE
          | "iso-8859-1" -> `ISO_8859_1 | "iso-8859-15" -> `ISO_8859_15
          | "us-ascii" -> `US_ASCII
          | _ -> raise (Protocol_error "unsupported XML charset"))
      | _ -> raise (Protocol_error "duplicate XML charset")
let xml t response =
  let body, truncated = read_bounded t.limits.max_bytes response in
  if truncated then raise (Protocol_error "XML byte limit exceeded");
  let encoding = encoding response body in
  protocol (Httpz_dav.parse_xml ~limits:t.limits ?encoding body)
let multi t response = protocol (Httpz_dav.multistatus (xml t response))
let reject t response =
  let body, truncated = read_bounded t.limits.max_bytes response in
  let dav_errors =
    if truncated then [] else
    try match Httpz_dav.parse_xml ~limits:t.limits ?encoding:(encoding response body) body with
      | Ok e when e.name = Httpz_dav.dav "error" -> List.filter_map (function
          | Httpz_dav.Element e -> Some e | _ -> None) e.children
      | _ -> []
    with Protocol_error _ -> [] in
  raise (Http_error { status = Fetch.status response; headers = Fetch.headers response;
                      body; truncated; dav_errors })
let request ?(headers = Fetch.Header.[]) ?(body = Fetch.String "") t meth target f =
  let url = resolve t target in
  Fetch.with_response ~headers ~body ~redirects:0 ~sensitive:["if"; "lock-token"]
    t.client (Http.Method.of_string meth) url f
let xml_headers = Fetch.Header.[raw "Content-Type" "application/xml; charset=utf-8"]
type capabilities = { dav : string list; allow : string list }
let options t target = request t "OPTIONS" target (fun r ->
  if Fetch.status r <> 200 then reject t r;
  let dav = match Fetch.header Header.dav r with Some xs -> xs | None -> [] in
  let allow = Http.Header.get_multi (Fetch.headers r) "allow"
    |> List.concat_map (String.split_on_char ',') |> List.map String.trim
    |> List.filter ((<>) "") in
  { dav; allow })
let propfind ?(depth = `Zero) t target query =
  let headers = Fetch.Header.append Fetch.Header.[Header.depth, depth] xml_headers in
  request ~headers ~body:(Fetch.String (Httpz_dav.propfind query)) t "PROPFIND" target (fun r ->
    if Fetch.status r <> 207 then reject t r;
    multi t r)
type condition = Unconditional | If_match of Fetch.Header.etag | If_absent
let conditions ?(condition = Unconditional) ?if_ () =
  let hs = match condition with
    | Unconditional -> Fetch.Header.[]
    | If_match validator ->
        if validator.weak then invalid_arg "Fetch_dav: If-Match requires a strong ETag";
        Fetch.Header.[if_match, `Etags [validator]]
    | If_absent -> Fetch.Header.[if_none_match, `Any] in
  match if_ with None -> hs | Some value ->
    Fetch.Header.append hs Fetch.Header.[raw "If" (Httpz_dav.encode_if value)]
type mutation = Complete of int | Multi of Httpz_dav.multistatus
let mutation t allowed r =
  let s = Fetch.status r in
  if s = 207 then Multi (multi t r)
  else if List.mem s allowed then Complete s else reject t r
let proppatch ?condition ?if_ t target updates =
  let headers = Fetch.Header.append (conditions ?condition ?if_ ()) xml_headers in
  request ~headers ~body:(Fetch.String (Httpz_dav.proppatch updates)) t "PROPPATCH" target (fun r ->
    if Fetch.status r <> 207 then reject t r;
    multi t r)
let mkcol ?if_ t target = request ~headers:(conditions ?if_ ()) t "MKCOL" target (fun r ->
  if Fetch.status r <> 201 then reject t r)
let put ?condition ?if_ ?content_type t target body =
  let headers = conditions ?condition ?if_ () in
  let headers = match content_type with None -> headers | Some ty ->
    Fetch.Header.append headers Fetch.Header.[raw "Content-Type" ty] in
  request ~headers ~body t "PUT" target (fun r ->
    let s = Fetch.status r in
    if List.mem s [200; 201; 204] then s else reject t r)
let delete ?condition ?if_ t target =
  request ~headers:(conditions ?condition ?if_ ()) t "DELETE" target (mutation t [200; 204])
let transfer ?(overwrite = false) ?if_ ~depth t meth ~src ~dst () =
  let destination = resolve t dst in
  let headers = Fetch.Header.append (conditions ?if_ ())
    Fetch.Header.[raw "Destination" destination; Header.overwrite, overwrite; Header.depth, depth] in
  request ~headers t meth src (mutation t [201; 204])
let copy ?(depth = `Infinity) ?overwrite ?if_ t ~src ~dst () =
  transfer ?overwrite ?if_ ~depth:(depth : Httpz_dav.tree_depth :> Httpz_dav.depth) t "COPY" ~src ~dst ()
let move ?overwrite ?if_ t ~src ~dst () =
  transfer ?overwrite ?if_ ~depth:`Infinity t "MOVE" ~src ~dst ()
let with_download ?headers t target f = request ?headers ~body:Fetch.Empty t "GET" target (fun r ->
  if List.mem (Fetch.status r) [200; 206; 304] then f r else reject t r)
type lease = { url : string; token : Httpz_dav.Token.t; granted : Httpz_dav.lock }
let lock_condition lease = Httpz_dav.Tagged [lease.url, [[Httpz_dav.Is (Httpz_dav.Token lease.token)]]]
let timeout_headers timeout = Fetch.Header.[raw "Timeout" (Httpz_dav.encode_timeout timeout)]
let granted t r token =
  let locks = protocol (Httpz_dav.locks (xml t r)) in
  match List.filter (fun (l : Httpz_dav.lock) -> l.token = Some token) locks with
  | [granted] -> { url = Fetch.url r; token; granted }
  | _ -> raise (Protocol_error "lock response must contain the granted token exactly once")
let lock ?(scope = Httpz_dav.Exclusive) ?(depth = `Zero) ?(timeout = Httpz_dav.Seconds 600L) ?owner t target =
  let headers = Fetch.Header.append xml_headers
    (Fetch.Header.append Fetch.Header.[Header.depth, (depth : Httpz_dav.tree_depth :> Httpz_dav.depth)] (timeout_headers timeout)) in
  request ~headers ~body:(Fetch.String (Httpz_dav.lockinfo ?owner scope)) t "LOCK" target (fun r ->
    if not (List.mem (Fetch.status r) [200; 201]) then reject t r;
    match Fetch.header Header.lock_token r with
    | Some token -> granted t r token
    | None -> raise (Protocol_error "missing or malformed Lock-Token"))
let refresh_lock ?(timeout = Httpz_dav.Seconds 600L) t lease =
  let if_ = Httpz_dav.Untagged [[Httpz_dav.Is (Httpz_dav.Token lease.token)]] in
  let headers = Fetch.Header.append (conditions ~if_ ()) (timeout_headers timeout) in
  request ~headers t "LOCK" lease.url (fun r ->
    if Fetch.status r <> 200 then reject t r;
    granted t r lease.token)
let unlock t lease =
  request ~headers:Fetch.Header.[Header.lock_token, lease.token] t "UNLOCK" lease.url (fun r ->
    if Fetch.status r <> 204 then reject t r)
