module Url = Fetch.Middleware.Url
type t = { client : Fetch.plain; root_url : Url.t; limits : Httpz_dav.limits; lenient : bool }
exception Protocol_error of string
type http_error = { status : int; headers : Http.Header.t; body : string;
                    truncated : bool; dav_errors : Httpz_dav.element list }
exception Http_error of http_error
let protocol = function Ok v -> v | Error s -> raise (Protocol_error s)
let url_exn = function Ok v -> v | Error s -> invalid_arg ("Fetch_dav: " ^ s)
let root t = Url.to_string t.root_url
let v ?(limits = Httpz_dav.default_limits) ?(lenient_hrefs = false) ~root client =
  Httpz_dav.validate_limits limits;
  let root_url = url_exn (Url.of_string root) in
  let u = Httpz_uri.of_string_exn root in
  if Httpz_uri.encoded_query u <> Null || Httpz_uri.encoded_fragment u <> Null ||
    not (String.ends_with ~suffix:"/" (Httpz_uri.encoded_path u)) then
    invalid_arg "Fetch_dav: root must be a collection URL without query or fragment";
  { client = Fetch.restrict ~under:[Url.to_string root_url] client; root_url; limits; lenient = lenient_hrefs }
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
let multi t response = protocol (Httpz_dav.multistatus ~lenient:t.lenient (xml t response))
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
let mkcol ?if_ ?props t target =
  let headers, body = match props with
    | None -> conditions ?if_ (), Fetch.String ""
    | Some props -> Fetch.Header.append (conditions ?if_ ()) xml_headers, Fetch.String (Httpz_dav.mkcol props) in
  request ~headers ~body t "MKCOL" target (fun r -> if Fetch.status r <> 201 then reject t r)
let mkcalendar ?if_ ?props t target =
  let headers, body = match props with
    | None -> conditions ?if_ (), Fetch.String ""
    | Some props -> Fetch.Header.append (conditions ?if_ ()) xml_headers, Fetch.String (Httpz_dav.mkcalendar props) in
  request ~headers ~body t "MKCALENDAR" target (fun r -> if Fetch.status r <> 201 then reject t r)
type written = { status : int; etag : string option }
let etag_of r = Http.Header.get (Fetch.headers r) "etag"
let put ?condition ?if_ ?content_type t target body =
  let headers = conditions ?condition ?if_ () in
  let headers = match content_type with None -> headers | Some ty ->
    Fetch.Header.append headers Fetch.Header.[raw "Content-Type" ty] in
  request ~headers ~body t "PUT" target (fun r ->
    let status = Fetch.status r in
    if List.mem status [200; 201; 204] then { status; etag = etag_of r } else reject t r)
let bounded t r =
  let body, truncated = read_bounded t.limits.max_bytes r in
  if truncated then raise (Protocol_error "body byte limit exceeded");
  body
let get ?headers t target = request ?headers ~body:Fetch.Empty t "GET" target (fun r ->
  if Fetch.status r <> 200 then reject t r;
  bounded t r, etag_of r)
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

let report_request ?(depth = `Zero) t target body f =
  let headers = Fetch.Header.append Fetch.Header.[Header.depth, depth] xml_headers in
  request ~headers ~body:(Fetch.String body) t "REPORT" target f
let report ?depth t target body = report_request ?depth t target body (fun r ->
  if Fetch.status r <> 207 then reject t r;
  multi t r)
let report_body ?depth t target body = report_request ?depth t target body (fun r ->
  if Fetch.status r <> 200 then reject t r;
  bounded t r)
let sync ?token ?level ?limit ?(props = [Httpz_dav.Prop.getetag]) t target =
  let base = resolve t target in
  report_request t target (Httpz_dav.Sync.request ?token ?level ?limit props) (fun r ->
    if Fetch.status r <> 207 then reject t r;
    protocol (Httpz_dav.Sync.decode ~lenient:t.lenient ~base (xml t r)))

(* The well-known path answers with a redirect, RFC 6764 Section 5, which no
   DAV operation follows, so its Location is read and resolved here. *)
let context_path t service =
  let target = Httpz_dav.Discovery.well_known service in
  request ~body:Fetch.Empty t "GET" target (fun r ->
    let redirect = List.mem (Fetch.status r) [301; 302; 303; 307; 308] in
    match Http.Header.get (Fetch.headers r) "location" with
    | Some location when redirect ->
        if List.length (Http.Header.get_multi (Fetch.headers r) "location") <> 1 then
          raise (Protocol_error "duplicate discovery Location");
        let base = url_exn (Url.of_string (Fetch.url r)) in
        resolve t (Url.to_string (url_exn (Url.resolve ~base location)))
    | None when Fetch.status r = 404 -> root t
    | _ when redirect -> raise (Protocol_error "missing discovery Location")
    | _ -> reject t r)
let response_for m url =
  let base = protocol (Url.of_string url) in
  let matches href =
    let resolved = protocol (Httpz_dav.resolve_href ~base:url href) in
    let candidate = protocol (Url.of_string resolved) in
    Url.same_origin base candidate &&
    Url.path_segments base = Url.path_segments candidate &&
    Url.has_query base = Url.has_query candidate &&
    (not (Url.has_query base) ||
      Url.path_and_query base = Url.path_and_query candidate) in
  match List.filter (fun r -> List.exists matches r.Httpz_dav.hrefs)
    m.Httpz_dav.responses with
  | [response] -> response
  | [] -> raise (Protocol_error "requested resource absent from multistatus")
  | _ -> raise (Protocol_error "duplicate resource in multistatus")

let principal t target =
  let url = resolve t target in
  let r = response_for (propfind t url Httpz_dav.Discovery.principal_query) url in
  let resolved h = protocol (Httpz_dav.resolve_href ~base:url h) in
  match Option.bind (Httpz_dav.find_property Httpz_dav.Prop.current_user_principal r) Httpz_dav.Prop.principal with
  | Some (`Href h) -> resolved h
  | Some `Unauthenticated -> raise (Protocol_error "the server reports the user unauthenticated")
  | None -> match Option.map Httpz_dav.Prop.hrefs (Httpz_dav.find_property Httpz_dav.Prop.principal_url r) with
    | Some (h :: _) -> resolved h
    | _ -> raise (Protocol_error "no current-user-principal")
let home_set t name target =
  let url = resolve t target in
  let r = response_for (propfind t url (Httpz_dav.Prop [name])) url in
  match Httpz_dav.find_property name r with
  | Some p -> List.map (fun h -> protocol (Httpz_dav.resolve_href ~base:url h)) (Httpz_dav.Prop.hrefs p)
  | None -> []

let read_only t =
  { t with client = Fetch.restrict
      ~methods:[`GET; `HEAD; `OPTIONS; Http.Method.of_string "PROPFIND"] t.client }

module Mirror = struct
  type action = Initial | Restart of string | Polling | Fetched of string * string
    | Skipped of string | Removed of string * string | Pruned of string | Truncated
    | Token of string | Unchanged
  let pp_action ppf = function
    | Initial -> Format.fprintf ppf "initial synchronization, no token stored"
    | Restart why -> Format.fprintf ppf "token refused (%s), rebuilding" why
    | Polling -> Format.fprintf ppf "no sync-collection report, polling entity tags"
    | Fetched (href, file) -> Format.fprintf ppf "fetched %s into %s" href file
    | Skipped href -> Format.fprintf ppf "unchanged %s" href
    | Removed (href, file) -> Format.fprintf ppf "removed %s for %s" file href
    | Pruned file -> Format.fprintf ppf "pruned %s" file
    | Truncated -> Format.fprintf ppf "page truncated, continuing"
    | Token token -> Format.fprintf ppf "stored token %s" token
    | Unchanged -> Format.fprintf ppf "no changes"
  type summary = { fetched : int; removed : int; token : string option }
  let index_file = ".davsync"
  let bad message = raise (Protocol_error ("DAV mirror: " ^ message))
  let field s =
    not (String.exists (fun c -> Char.code c < 32 || c = '\127') s)
  let valid_file s =
    s <> "" && s <> "." && s <> ".." && s <> index_file &&
    not (String.starts_with ~prefix:".davsync.tmp-" s) && field s &&
    not (String.contains s '/') && not (String.contains s '\\')
  let file_of_href href =
    let name = Httpz_dav.basename href in
    if not (valid_file name) then bad "unsafe member filename";
    name
  type index = {
    token : string option;
    members : (string * (string * string)) list;
  }
  let same = String.equal
  let canonical t collection href =
    let href = resolve t href in
    let url = url_exn (Url.of_string href) in
    let prefix = url_exn (Url.of_string collection) in
    let uri = Httpz_uri.of_string_exn href in
    if not (Url.under ~prefix url) || Url.path_segments prefix = Url.path_segments url
       || Httpz_uri.encoded_query uri <> Null then
      bad "member is outside the collection";
    href
  let collision members href file =
    if List.exists (fun (h, (_, f)) -> f = file && h <> href) members then
      bad "members have colliding filenames"
  let load t collection dir =
    let path = Eio.Path.(dir / index_file) in
    match Eio.Path.kind ~follow:false path with
    | `Not_found -> { token = None; members = [] }
    | `Regular_file ->
        let text = Eio.Path.with_open_in path (fun flow ->
          Eio.Buf_read.take_all (Eio.Buf_read.of_flow flow ~max_size:16_777_216)) in
        let lines = String.split_on_char '\n' text in
        let token, lines = match lines with
          | line :: rest when String.starts_with ~prefix:"token\t" line ->
              let value = String.sub line 6 (String.length line - 6) in
              if not (field value) then bad "invalid index token";
              (if value = "" then None else Some value), rest
          | _ -> bad "invalid index header" in
        let members = List.fold_left (fun members line ->
          if line = "" then members else
          match String.split_on_char '\t' line with
          | [href; etag; file] when field etag && valid_file file ->
              let href = canonical t collection href in
              if file <> file_of_href href || List.mem_assoc href members then
                bad "invalid index member";
              collision members href file;
              (href, (etag, file)) :: members
          | _ -> bad "invalid index record") [] lines in
        { token; members }
    | _ -> bad "index is not a regular file"
  let temporary = Atomic.make 0
  let atomic_write dir file write =
    let rec open_temp attempts =
      if attempts = 0 then bad "temporary filenames exhausted";
      let name = ".davsync.tmp-" ^ string_of_int (Atomic.fetch_and_add temporary 1) in
      let path = Eio.Path.(dir / name) in
      let created = ref false in
      try
        Eio.Path.with_open_out ~create:(`Exclusive 0o600) path
          (fun sink -> created := true; write sink);
        path
      with
      | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) when not !created ->
          open_temp (attempts - 1)
      | exn ->
          if !created then Eio.Cancel.protect (fun () ->
            Eio.Path.unlink ~missing_ok:true path);
          raise exn in
    let path = open_temp 64 in
    match Eio.Path.rename path Eio.Path.(dir / file) with
    | () -> ()
    | exception exn ->
        Eio.Cancel.protect (fun () -> Eio.Path.unlink ~missing_ok:true path);
        raise exn
  let store dir index =
    let b = Buffer.create 256 in
    let token = Option.value ~default:"" index.token in
    if not (field token) then bad "invalid sync token";
    Buffer.add_string b ("token\t" ^ token ^ "\n");
    List.iter (fun (href, (etag, file)) ->
      if not (field href && field etag && valid_file file) then
        bad "invalid index data";
      Buffer.add_string b (String.concat "\t" [href; etag; file] ^ "\n"))
      index.members;
    if Buffer.length b > 16_777_216 then bad "index byte limit exceeded";
    atomic_write dir index_file (fun sink ->
      Eio.Flow.copy_string (Buffer.contents b) sink)
  let remember index href etag file =
    { index with members = (href, (etag, file)) ::
      List.filter (fun (h, _) -> not (same h href)) index.members }
  let forget index href =
    { index with members = List.filter (fun (h, _) -> not (same h href)) index.members }
  let fetch ~log t dir index href reported =
    let file = file_of_href href in
    collision index.members href file;
    let held = List.find_opt (fun (h, _) -> same h href) index.members in
    match held, reported with
    | Some (_, (etag, file')), Some tag when etag = tag && file' = file &&
        Eio.Path.kind ~follow:false Eio.Path.(dir / file) = `Regular_file ->
        log (Skipped href); index, false
    | _ ->
        let etag = with_download t href (fun r ->
          atomic_write dir file (fun sink -> Eio.Flow.copy (Fetch.body r) sink);
          etag_of r) in
        log (Fetched (href, file));
        remember index href (Option.value ~default:"" etag) file, true
  let remove ~log dir index href =
    match List.find_opt (fun (h, _) -> same h href) index.members with
    | Some (_, (_, file)) ->
        Eio.Path.unlink ~missing_ok:true Eio.Path.(dir / file);
        log (Removed (href, file));
        forget index href, true
    | None -> index, false
  let check_response r =
    match r.Httpz_dav.outcome with
    | Httpz_dav.Status status when status >= 200 && status < 300 -> ()
    | Httpz_dav.Properties groups when List.for_all (fun g ->
        g.Httpz_dav.status >= 200 && g.status < 300 || g.status = 404) groups -> ()
    | _ -> bad "incomplete member listing"
  let rec pages ~log ?limit ~level ~rebuilding ~tokens t ~collection ~dir
      index token seen fetched removed =
    if List.length tokens >= 1024 then bad "page limit exceeded";
    let page = sync ?token ~level ?limit t collection in
    if page.truncated && (page.token = None || page.token = token ||
        List.mem page.token tokens) then bad "sync token did not advance";
    let index, seen, fetched, removed = List.fold_left
      (fun (index, seen, fetched, removed) -> function
      | Httpz_dav.Sync.Changed r ->
          check_response r;
          let href = canonical t collection (Httpz_dav.href r) in
          if Httpz_dav.is_collection r then index, seen, fetched, removed else
          let index, did = fetch ~log t dir index href (Httpz_dav.etag r) in
          index, href :: seen, (if did then fetched + 1 else fetched), removed
      | Httpz_dav.Sync.Unsupported _ -> bad "unsupported member in sync report"
      | Httpz_dav.Sync.Removed href ->
          let href = canonical t collection href in
          let index, did = remove ~log dir index href in
          index, seen, fetched, if did then removed + 1 else removed)
      (index, seen, fetched, removed) page.changes in
    let index = { index with token = page.token } in
    store dir (if rebuilding then { index with token = None } else index);
    if not rebuilding then Option.iter (fun t -> log (Token t)) page.token;
    if page.truncated then (
      log Truncated;
      pages ~log ?limit ~level ~rebuilding ~tokens:(page.token :: tokens)
        t ~collection ~dir index page.token seen fetched removed)
    else index, seen, { fetched; removed; token = page.token }
  let poll ~log t ~collection ~dir index =
    log Polling;
    let listing = propfind ~depth:`One t collection
      (Httpz_dav.Prop [Httpz_dav.Prop.getetag; Httpz_dav.Prop.resourcetype]) in
    let present = List.filter_map (fun r ->
      check_response r;
      let href = resolve t (Httpz_dav.href r) in
      if same href collection then None else
      let href = canonical t collection href in
      if Httpz_dav.is_collection r then None else Some (href, Httpz_dav.etag r))
      listing.responses in
    let index = { index with token = None } in
    store dir index;
    let index, fetched = List.fold_left (fun (index, n) (href, etag) ->
      let index, did = fetch ~log t dir index href etag in
      index, if did then n + 1 else n) (index, 0) present in
    let gone = List.filter (fun (h, _) ->
      not (List.exists (fun (p, _) -> same p h) present)) index.members in
    let index, removed = List.fold_left (fun (index, n) (href, _) ->
      let index, did = remove ~log dir index href in
      index, if did then n + 1 else n) (index, 0) gone in
    store dir index;
    { fetched; removed; token = None }
  let supports_sync t collection =
    let listing = propfind ~depth:`Zero t collection
      (Httpz_dav.Prop [Httpz_dav.Prop.supported_report_set]) in
    let r = response_for listing collection in
    match Httpz_dav.find_property Httpz_dav.Prop.supported_report_set r with
    | Some p -> List.mem (Httpz_dav.dav "sync-collection") (Httpz_dav.Prop.reports p)
    | None -> false
  let rebuild ~log ?limit ~level t ~collection ~dir index =
    let index = { index with token = None } in
    store dir index;
    let index, seen, summary = pages ~log ?limit ~level ~rebuilding:true
      ~tokens:[] t ~collection ~dir index None [] 0 0 in
    let stale = List.filter (fun (h, _) -> not (List.exists (same h) seen)) index.members in
    List.iter (fun (_, (_, file)) ->
      Eio.Path.unlink ~missing_ok:true Eio.Path.(dir / file); log (Pruned file)) stale;
    store dir { index with
      members = List.filter (fun (h, _) -> List.exists (same h) seen) index.members };
    Option.iter (fun token -> log (Token token)) index.token;
    summary
  let run ?(log = fun _ -> ()) ?limit ?(level = `One) t ~collection ~dir =
    let collection = resolve t collection in
    ignore (child t ~collection "validation");
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
    Eio.Path.with_subtree dir (fun dir ->
      let index = load t collection dir in
      if not (supports_sync t collection) then poll ~log t ~collection ~dir index else
      match index.token with
      | None -> log Initial; rebuild ~log ?limit ~level t ~collection ~dir index
      | Some token ->
          match pages ~log ?limit ~level ~rebuilding:false ~tokens:[] t
            ~collection ~dir index (Some token) [] 0 0 with
          | _, _, summary ->
              if summary.fetched = 0 && summary.removed = 0 then log Unchanged;
              summary
          | exception Http_error e when List.mem e.status [400; 403; 409] ->
              let why =
                if Httpz_dav.Condition.has Httpz_dav.Condition.valid_sync_token e.dav_errors
                then "valid-sync-token" else "HTTP " ^ string_of_int e.status in
              log (Restart why);
              (* Earlier pages may have committed their progress. *)
              rebuild ~log ?limit ~level t ~collection ~dir (load t collection dir))
end

module Session = struct
  type dav = t
  let scoped = resolve
  type t = { dav : dav; sw : Eio.Switch.t; principal : string; home_sets : string list }
  type error =
    | Http of int * string
    | Dav of int * Httpz_dav.element list
    | Precondition_failed of string
    | Not_found of string
    | Xml of string
    | Data of string
    | Discovery of string
    | Transport of Fetch.error * string
  let first_line s =
    let s = match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s in
    let s = String.trim s in
    if String.length s > 120 then String.sub s 0 120 ^ "..." else s
  let pp_error ?(describe = snd) ppf = function
    | Http (code, body) -> Format.fprintf ppf "HTTP %d: %s" code (first_line body)
    | Dav (code, conditions) ->
        Format.fprintf ppf "HTTP %d: %s" code
          (String.concat ", " (List.map (fun (e : Httpz_dav.element) -> describe e.name) conditions))
    | Precondition_failed target -> Format.fprintf ppf "precondition failed for %s" target
    | Not_found target -> Format.fprintf ppf "%s does not exist" target
    | Xml msg -> Format.fprintf ppf "XML: %s" msg
    | Data msg -> Format.fprintf ppf "data: %s" msg
    | Discovery msg -> Format.fprintf ppf "discovery: %s" msg
    | Transport (_, msg) -> Format.fprintf ppf "transport: %s" (first_line msg)
  let error_to_string ?describe e = Format.asprintf "%a" (pp_error ?describe) e
  let guard ~target f =
    try Ok (f ()) with
    | Eio.Io (Fetch.E e, _) as exn -> Error (Transport (e, Format.asprintf "%a" Eio.Exn.pp exn))
    | Protocol_error msg -> Error (Xml msg)
    | Invalid_argument msg -> Error (Discovery msg)
    | Http_error e -> (match e.status with
        | 404 -> Error (Not_found target)
        | 412 -> Error (Precondition_failed target)
        | code when e.dav_errors <> [] -> Error (Dav (code, e.dav_errors))
        | code -> Error (Http (code, e.body)))
  let ( let* ) = Result.bind
  let connect ~sw ?(credentials = []) ?(allow_insecure = false) ?limits ?lenient_hrefs
      ~service ~home_set:home fetch url =
    let* parsed = Result.map_error (fun m -> Discovery m) (Url.of_string url) in
    let origin = Url.origin parsed in
    let fetch = match credentials with
      | [] -> Fetch.Middleware.(of_handler (handler fetch))
      | credentials -> Fetch.with_credentials ~scope:[origin] ~allow_insecure credentials fetch in
    let dav = v ?limits ?lenient_hrefs ~root:(origin ^ "/") fetch in
    let url = Url.to_string parsed in
    let well_known = Httpz_dav.Discovery.well_known service in
    let path = Httpz_dav.href_path url in
    let* context =
      if String.ends_with ~suffix:well_known path || String.ends_with ~suffix:(well_known ^ "/") path
      then guard ~target:url (fun () -> context_path dav service)
      else Ok url in
    let* principal = guard ~target:context (fun () -> principal dav context) in
    let* home_sets = guard ~target:principal (fun () -> home_set dav home principal) in
    if home_sets = [] then Error (Discovery (principal ^ " names no " ^ snd home))
    else Ok { dav; sw; principal; home_sets }
  let principal t = t.principal
  let home_sets t = t.home_sets
  let client t = t.dav
  let switch t = t.sw
  let resolve t href =
    match Httpz_dav.resolve_href ~base:t.principal href with Ok r -> r | Error _ -> href
  let propfind t ?(depth = `Zero) url query =
    let url = resolve t url in
    guard ~target:url (fun () -> propfind ~depth t.dav url query)
  let report t ?(depth = `Zero) url body =
    let url = resolve t url in
    guard ~target:url (fun () -> report ~depth t.dav url (Httpz_dav.encode_xml body))
  let report_body t ?(depth = `Zero) url body =
    let url = resolve t url in
    guard ~target:url (fun () -> report_body ~depth t.dav url (Httpz_dav.encode_xml body))
  let mkcol t ?props url =
    let url = resolve t url in
    guard ~target:url (fun () -> mkcol ?props t.dav url)
  let mkcalendar t ?props url =
    let url = resolve t url in
    guard ~target:url (fun () -> mkcalendar ?props t.dav url)
  let proppatch t url updates =
    let url = resolve t url in
    let* m = guard ~target:url (fun () -> proppatch t.dav url updates) in
    match Httpz_dav.failures m with
    | [] -> Ok ()
    | (code, conditions, names) :: _ -> Error (Dav (code, conditions @ List.map Httpz_dav.empty names))
  let condition ?etag ?(create = false) () =
    match etag with
    | Some s -> (match Fetch.Header.decode Fetch.Header.etag s with
        | Some v -> Ok (If_match v)
        | None -> Error (Data (Printf.sprintf "%S is not an entity tag" s)))
    | None -> Ok (if create then If_absent else Unconditional)
  let delete t ?etag url =
    let url = resolve t url in
    let* condition = condition ?etag () in
    let* outcome = guard ~target:url (fun () -> delete ~condition t.dav url) in
    match outcome with
    | Complete _ -> Ok ()
    | Multi m -> (match Httpz_dav.failures m with
        | [] -> Ok ()
        | (code, conditions, _) :: _ -> Error (Dav (code, conditions)))
  type member = { href : string; etag : string option; content_type : string option }
  let members t url =
    let url = resolve t url in
    let query = Httpz_dav.Prop Httpz_dav.Prop.[resourcetype; getetag; getcontenttype] in
    let* m = propfind t ~depth:`One url query in
    Ok (List.filter_map (fun (r : Httpz_dav.response) ->
      let href = Httpz_dav.href r and status = Httpz_dav.response_status r in
      if Httpz_dav.same_href href url || Httpz_dav.is_collection r || status < 200 || status > 299
      then None
      else Some {
        href = (match Httpz_dav.resolve_href ~base:url href with Ok h -> h | Error _ -> href);
        etag = Httpz_dav.etag r;
        content_type = Option.map Httpz_dav.content
          (Httpz_dav.find_property Httpz_dav.Prop.getcontenttype r) }) m.responses)
  let get t ?accept url =
    let url = resolve t url in
    let headers = match accept with Some a -> Fetch.Header.[raw "Accept" a] | None -> Fetch.Header.[] in
    guard ~target:url (fun () -> get ~headers t.dav url)
  let put t ?etag ?create ~content_type url body =
    let url = resolve t url in
    let* condition = condition ?etag ?create () in
    let* written = guard ~target:url (fun () -> put ~condition ~content_type t.dav url (Fetch.String body)) in
    Ok written.etag
  let download t ?(headers = Fetch.Header.[]) url =
    let url = resolve t url in
    guard ~target:url (fun () ->
      let url = scoped t.dav url in
      let r = Fetch.fetch ~sw:t.sw ~headers ~body:Fetch.Empty ~redirects:0
        ~sensitive:["if"; "lock-token"] t.dav.client `GET url in
      if List.mem (Fetch.status r) [200; 206; 304] then r
      else Fun.protect ~finally:(fun () -> Fetch.close r)
        (fun () -> reject t.dav r))
  let sync t ?token ?limit url =
    let url = resolve t url in
    guard ~target:url (fun () -> sync ?token ?limit t.dav url)
  let sync_token t url =
    let* m = propfind t ~depth:`Zero url (Httpz_dav.Prop [Httpz_dav.Prop.sync_token]) in
    Ok (match m.responses with
      | r :: _ -> Option.map Httpz_dav.content (Httpz_dav.find_property Httpz_dav.Prop.sync_token r)
      | [] -> None)
  let safe_segment s =
    s <> "" && String.for_all (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '@' -> true | _ -> false) s
  let member_name uid ext =
    match uid with
    | Some u when safe_segment u -> u ^ ext
    | _ -> String.concat "" (List.init 16 (fun _ -> Printf.sprintf "%x" (Random.int 16))) ^ ext
end
