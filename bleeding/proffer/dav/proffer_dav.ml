module D = Httpz_dav
module P = Proffer
module U = Httpz_uri

exception Error of int * D.name list
let fail ?(conditions = []) status = raise (Error (status, conditions))
let decoded = function This x -> x | Null -> fail 400
let protocol = function Ok x -> x | Error _ -> fail 400

module Path = struct
  type t = string list
  let valid s =
    s <> "" && s <> "." && s <> ".." && String.length s <= 255
    && String.is_valid_utf_8 s
    && not (String.exists (fun c -> c = '/' || c = '\\'
      || Char.code c < 32 || Char.code c = 127) s)
  let of_segments segments =
    if List.length segments > 64 || not (List.for_all valid segments) then
      invalid_arg "Proffer_dav.Path: invalid resource path";
    segments
  let segments t = t
  let child t name = of_segments (t @ [name])
  let parent = function [] -> None | t -> Some (List.rev (List.tl (List.rev t)))
  let rec under ~prefix t = match prefix, t with
    | [], _ -> true
    | a :: aa, b :: bb when a = b -> under ~prefix:aa bb
    | _ -> false
  let equal = (=)
end

type kind = File | Collection
type entry = { kind : kind; length : int64; etag : string option;
  modified : float; properties : D.element list }
module Reader = struct
  type t = {
    stat : Path.t -> entry option;
    list : Path.t -> (string * entry) list;
    read : Path.t -> (entry -> (P.Body.Sink.t -> unit) -> unit) @ local -> unit;
  }
  let v ~stat ~list ~read = { stat; list; read }
end

type lease = { path : Path.t; token : D.Token.t; principal : string;
  depth : D.tree_depth; expires : float; owner : D.element option }
type operation =
  | Put of Path.t * P.Req.Input.t
  | Mkcol of Path.t
  | Delete of Path.t
  | Copy of { src : Path.t; dst : Path.t; overwrite : bool; depth : D.tree_depth }
  | Move of { src : Path.t; dst : Path.t; overwrite : bool }
  | Proppatch of Path.t * D.update list
  | Check of Path.t
  | Lock of Path.t * string * D.tree_depth * int * D.element option
  | Refresh of Path.t * string * D.Token.t * int
  | Unlock of Path.t * string * D.Token.t

type result = Completed of int | Written of int * entry | Locked of int * lease
module Writer = struct
  type t = { reader : Reader.t; now : unit -> float;
    leases : unit -> lease list;
    mutate : operation -> guard:(Reader.t -> lease list -> unit) -> result }
  let v ~reader ~now ~leases ~mutate = { reader; now; leases; mutate }
  let reader t = t.reader
end

type access = Read_only | Read_write
module Security = struct
  type t = Public | Authenticated of {
    realm : string;
    authenticate : string -> string option;
    authorize : string -> access option;
  }
  let authenticated ~realm ~authenticate ~authorize =
    if String.exists (fun c -> c = '"' || c = '\\'
      || Char.code c < 32 || Char.code c = 127) realm then
      invalid_arg "Proffer_dav.Security: invalid realm";
    Authenticated { realm; authenticate; authorize }
  let public_read_only = Public
end

type limits = { max_xml_bytes : int; max_xml_nodes : int; max_xml_depth : int;
  max_resources : int; max_response_bytes : int; max_file_bytes : int64;
  max_active : int }
let default_limits = { max_xml_bytes = 1024 * 1024; max_xml_nodes = 20_000;
  max_xml_depth = 32; max_resources = 10_000; max_response_bytes = 8 * 1024 * 1024;
  max_file_bytes = 16_777_216L; max_active = 16 }

type t = { reader : Reader.t; writer : Writer.t option; security : Security.t;
  origin : U.t; at : Path.t; limits : limits; mutable active : int }
let port uri = match U.port uri with
  | This p -> p | Null -> if U.scheme uri = This "https" then 443 else 80
let same_origin a b = U.scheme a = U.scheme b && U.encoded_host a = U.encoded_host b
  && port a = port b
let make ?(limits = default_limits) ?(allow_insecure_loopback = false)
    ~origin ~at ~security reader writer =
  if limits.max_xml_bytes < 1 || limits.max_xml_nodes < 1
    || limits.max_xml_depth < 1 || limits.max_resources < 1
    || limits.max_response_bytes < 1 || limits.max_file_bytes < 0L
    || limits.max_active < 1 then invalid_arg "Proffer_dav: invalid limits";
  let origin = U.of_string_exn origin in
  let secure = U.scheme origin = This "https" in
  let local_http = allow_insecure_loopback && U.scheme origin = This "http"
    && List.mem (U.encoded_host origin)
      [This "localhost"; This "127.0.0.1"; This "::1"] in
  if not (secure || local_http) || not (U.has_authority origin)
    || List.mem (U.encoded_host origin) [Null; This ""]
    || U.has_userinfo origin || U.has_query origin || U.has_fragment origin
    || not (List.mem (U.encoded_path origin) [""; "/"])
    || port origin < 1 || port origin > 65535 then
    invalid_arg "Proffer_dav: expected HTTPS origin (or explicit local HTTP)";
  (match security, writer with Security.Public, Some _ ->
    invalid_arg "Proffer_dav: public exports must be read-only" | _ -> ());
  { reader; writer; security; origin; at = Path.of_segments at; limits; active = 0 }
let read_only ?limits ?allow_insecure_loopback ~origin ~at ~security reader =
  make ?limits ?allow_insecure_loopback ~origin ~at ~security reader None
let read_write ~limits ?allow_insecure_loopback ~origin ~at ~security writer =
  make ~limits ?allow_insecure_loopback ~origin ~at ~security
    (Writer.reader writer) (Some writer)

let fields (req : P.Req.t @ local) =
  let rec copy (fields : (string * string) list @ local) =
    match fields with
    | [] -> []
    | (k, v) :: rest ->
        (String.lowercase_ascii (P.Req.globalize k), P.Req.globalize v)
        :: copy rest in
  let result = copy (P.Headers.to_list (P.Req.headers req)) in
  result
let values headers name = List.filter_map (fun (k, v) ->
  if k = name then Some v else None) headers
let single headers name = match values headers name with
  | [] -> None | [v] -> Some v | _ -> fail 400
let required headers name = match single headers name with
  | Some v -> v | None -> fail 400
let weak_tag s = if String.starts_with ~prefix:"W/" s then
  String.sub s 2 (String.length s - 2) else s
let etag_matches ~strong ~exists etag field =
  let field = String.trim field in
  if field = "*" then exists
  else
    let n = String.length field in
    let rec scan i quoted start acc =
      if i = n then
        if quoted then fail 400
        else List.rev (String.trim (String.sub field start (n - start)) :: acc)
      else if field.[i] = '"' then scan (i+1) (not quoted) start acc
      else if field.[i] = ',' && not quoted then
        scan (i+1) false (i+1) (String.trim (String.sub field start (i-start)) :: acc)
      else scan (i+1) quoted start acc in
    let tags = scan 0 false 0 [] in
    if not (List.for_all D.valid_etag tags) then fail 400;
    List.exists (fun tag -> match etag with
      | None -> false
      | Some current -> if strong then D.strong_etag tag && tag = current
        else weak_tag tag = weak_tag current) tags
let status code = match P.Status.of_code code with Some s -> s | None -> P.Status.of_code 500 |> Option.get
let answer ?(headers = []) ?(content_type = "application/xml; charset=utf-8")
    respond code body =
  P.Resp.v respond ~status:(status code)
    ~headers:(P.Headers.of_list (("Cache-Control", "no-store") ::
      ("X-Content-Type-Options", "nosniff") :: headers))
    ~content_type:(This content_type) (P.Body.String body)
let reject ?(allow = "OPTIONS, PROPFIND") respond code conditions =
  let headers = if code = 405 then ["Allow", allow] else [] in
  answer ~headers respond code (D.Server.error conditions)
let path t reference =
  if String.length reference > 4096 then fail 414;
  let uri = decoded (U.of_string reference) in
  if U.has_query uri || U.has_fragment uri || U.has_userinfo uri then fail 400;
  if U.has_authority uri && not (same_origin uri t.origin) then fail 403;
  if U.scheme uri <> Null && not (same_origin uri t.origin) then fail 403;
  let encoded = U.encoded_path uri in
  if not (String.starts_with ~prefix:"/" encoded) then fail 400;
  let raw = String.split_on_char '/' encoded in
  let raw = List.tl raw in
  let raw = if List.length raw > 0 && List.hd (List.rev raw) = "" then
    List.rev (List.tl (List.rev raw)) else raw in
  let segments = List.map (fun s -> decoded (U.percent_decode s)) raw in
  let segments = try Path.of_segments segments with Invalid_argument _ -> fail 400 in
  if not (Path.under ~prefix:t.at segments) then fail 403;
  let rec drop prefix xs = match prefix, xs with
    | [], xs -> xs | _ :: ps, _ :: xs -> drop ps xs | _ -> assert false in
  drop t.at segments
let href t path kind =
  "/" ^ String.concat "/" (List.map (U.percent_encode ~component:`Unreserved)
    (t.at @ Path.segments path)) ^
  (if kind = Collection && t.at @ path <> [] then "/" else "")
let body t (req : P.Req.t @ local) =
  let input = P.Req.input req in
  let buffer = Buffer.create 256 and scratch = Bytes.create 16384 in
  let rec loop () =
    let n = P.Req.Input.read input scratch ~off:0 ~len:(Bytes.length scratch) in
    if n > 0 then begin
      if Buffer.length buffer > t.limits.max_xml_bytes - n then fail 413;
      Buffer.add_subbytes buffer scratch 0 n;
      loop ()
    end in
  loop ();
  Buffer.contents buffer
let xml t headers source =
  let fields = match single headers "content-type" with
    | None -> fail 415
    | Some value -> String.split_on_char ';' value |> List.map String.trim in
  let ty = String.lowercase_ascii (List.hd fields) in
  if ty <> "application/xml" && ty <> "text/xml" then fail 415;
  let params = List.map (fun field ->
    match String.index_opt field '=' with
    | None -> fail 415
    | Some i ->
        let key = String.lowercase_ascii (String.trim (String.sub field 0 i)) in
        let value = String.trim
          (String.sub field (i+1) (String.length field-i-1)) in
        let value = if String.length value >= 2 && value.[0] = '"'
          && value.[String.length value-1] = '"' then
            String.sub value 1 (String.length value-2) else value in
        key, String.lowercase_ascii value) (List.tl fields) in
  let charset = List.filter (fun (k, _) -> k = "charset") params in
  let encoding = match charset with
    | [] -> None
    | [_, value] -> Some (match value with
        | "utf-8" | "utf8" -> `UTF_8 | "utf-16" -> `UTF_16
        | "utf-16be" -> `UTF_16BE | "utf-16le" -> `UTF_16LE
        | "iso-8859-1" -> `ISO_8859_1 | "iso-8859-15" -> `ISO_8859_15
        | "us-ascii" -> `US_ASCII | _ -> fail 415)
    | _ -> fail 415 in
  let encoding = if List.exists (fun prefix ->
    String.starts_with ~prefix source) ["\239\187\191"; "\254\255"; "\255\254"]
    then None else encoding in
  protocol (D.parse_xml ~limits:{max_bytes=t.limits.max_xml_bytes;
    max_nodes=t.limits.max_xml_nodes; max_depth=t.limits.max_xml_depth}
    ?encoding source)

let existing reader path = match reader.Reader.stat path with
  | Some e -> e | None -> fail 404
let xml_result t respond code value =
  let body = try D.Server.multistatus
    ~max_bytes:t.limits.max_response_bytes value
    with D.Output_too_large -> fail 507 in
  answer respond code body

let date s =
  if String.length s > 32767 then None else
  let bytes = Bytes.of_string s in
  let #(valid, time) = Httpz.Date.parse bytes
    (Httpz.Span.make ~off:(Stdlib_stable.Int16_u.of_int 0)
      ~len:(Stdlib_stable.Int16_u.of_int (String.length s))) in
  match valid with Httpz.Date.Valid -> Some
    (Stdlib_upstream_compatible.Float_u.to_float time) | _ -> None
let http_conditions ~read headers entry =
  let exists = entry <> None in
  let etag = Option.bind entry (fun e -> e.etag) in
  let field name = match values headers name with
    | [] -> None | xs -> Some (String.concat "," xs) in
  let modified_after value = match entry, value with
    | Some e, Some s -> (match date s with
      | Some time -> floor e.modified > floor time | None -> false)
    | _ -> false in
  (match field "if-match" with
  | Some value -> if not (etag_matches ~strong:true ~exists etag value) then fail 412
  | None -> if modified_after (single headers "if-unmodified-since") then fail 412);
  match field "if-none-match" with
  | Some value ->
      if etag_matches ~strong:false ~exists etag value then
        fail (if read then 304 else 412)
  | None when read ->
      (match entry, Option.bind (single headers "if-modified-since") date with
      | Some e, Some time when floor e.modified <= floor time -> fail 304
      | _ -> ())
  | None -> ()
let covers lease path = Path.equal lease.path path
  || lease.depth = `Infinity && Path.under ~prefix:lease.path path
let leases t = match t.writer with None -> [] | Some w ->
  List.filter (fun lease -> lease.expires > w.now ()) (w.leases ())
let lock_value t now lease = {
  D.scope = D.Exclusive; depth = lease.depth;
  timeout = Some (D.Seconds (Int64.of_int (max 1 (int_of_float (ceil (lease.expires -. now))))));
  token = Some lease.token; root = Some (href t lease.path
    (match t.reader.stat lease.path with
     | Some e -> e.kind | None -> File));
  owner = lease.owner }
let now t = match t.writer with None -> 0. | Some w -> w.now ()
let prop name children = D.element (D.dav name) children
let text_prop name value = prop name [D.Text value]
let properties t path entry =
  let base = [prop "resourcetype" (if entry.kind = Collection then
      [D.Element (prop "collection" [])] else []);
    text_prop "displayname" (match List.rev (Path.segments path) with
      | [] -> "" | name :: _ -> name);
    text_prop "getlastmodified" (Httpz.Date.format
      (Stdlib_upstream_compatible.Float_u.of_float entry.modified))]
    @ (match entry.etag with None -> [] | Some e -> [text_prop "getetag" e])
    @ (if entry.kind = Collection then [] else
      [text_prop "getcontentlength" (Int64.to_string entry.length);
       text_prop "getcontenttype" (P.Mime.of_path (String.concat "/" path))]) in
  let locking = match t.writer with
    | None -> []
    | Some _ -> [D.Server.lockdiscovery (List.filter (fun l -> covers l path)
        (leases t) |> List.map (lock_value t (now t)));
      prop "supportedlock" [D.Element (prop "lockentry"
        [D.Element (prop "lockscope" [D.Element (prop "exclusive" [])]);
         D.Element (prop "locktype" [D.Element (prop "write" [])])])]] in
  base @ locking @ entry.properties
let response t path entry query =
  let available = properties t path entry in
  let names = match query with
    | D.Prop names -> names
    | D.Allprop extra -> List.sort_uniq compare
        (List.map (fun e -> e.D.name) available @ extra)
    | D.Propname -> List.map (fun e -> e.D.name) available in
  if List.length names > 128 then fail 413;
  let found, missing = List.fold_left (fun (found, missing) name ->
    match List.find_opt (fun e -> e.D.name = name) available with
    | Some value -> ((if query = D.Propname then D.element name [] else value) :: found, missing)
    | None -> found, D.element name [] :: missing) ([], []) names in
  let group status properties = {D.status; properties = List.rev properties;
    errors = []; description = None} in
  let groups = (if found = [] then [] else [group 200 found])
    @ (if missing = [] then [] else [group 404 missing]) in
  let groups = if groups = [] then [group 200 []] else groups in
  {D.hrefs = [href t path entry.kind]; outcome = D.Properties groups;
   errors = []; description = None; location = None}
let multistatus responses = {D.responses; description = None}
let read_methods = ["OPTIONS"; "GET"; "HEAD"; "PROPFIND"]
let write_methods = ["PUT"; "MKCOL"; "DELETE"; "COPY"; "MOVE";
  "PROPPATCH"; "LOCK"; "UNLOCK"]
let can_write t access = access = Read_write && Option.is_some t.writer
let callback f value = try f value with _ -> fail 503
let authorize t principal = match t.security with
  | Security.Public -> Some Read_only
  | Security.Authenticated security -> callback security.authorize principal
let authenticate t headers = match t.security with
  | Security.Public -> "anonymous", Read_only
  | Security.Authenticated security ->
      let credential = match values headers "authorization" with
        | [value] -> value | _ -> fail 401 in
      let principal = match callback security.authenticate credential with
        | Some principal when principal <> "" -> principal | _ -> fail 401 in
      match callback security.authorize principal with
      | Some access -> principal, access | None -> fail 403
let no_body t req = if body t req <> "" then fail 415
let required_writer t = match t.writer with Some w -> w | None -> fail 403
let no_root path = if path = [] then fail 403
let destination t headers src =
  let dst = path t (required headers "destination") in
  no_root dst;
  if Path.under ~prefix:src dst || Path.under ~prefix:dst src then fail 403;
  dst
let overwrite headers = match single headers "overwrite" with
  | None | Some "T" -> true | Some "F" -> false | _ -> fail 400
let depth headers default = match single headers "depth" with
  | None -> default
  | Some value -> (match D.decode_depth value with Some d -> d | None -> fail 400)
let timeout headers = match single headers "timeout" with
  | None -> 600
  | Some value ->
      let values = String.split_on_char ',' value in
      let rec choose = function
        | [] -> fail 400
        | value :: rest -> match D.decode_timeout (String.trim value) with
          | Some D.Infinite -> 3600
          | Some (D.Seconds seconds) when seconds > 0L ->
              Int64.to_int (min 3600L seconds)
          | _ -> choose rest in
      choose values
let if_header headers = Option.map (fun value ->
  protocol (D.Server.if_condition value)) (single headers "if")
let if_conditions t headers source reader active =
  let condition = if_header headers in
  let tagged = match condition with
    | None -> []
    | Some (D.Untagged groups) -> [source, groups]
    | Some (D.Tagged values) -> List.map (fun (tag, groups) -> path t tag, groups) values in
  let token_exists path token = List.exists (fun lease ->
    covers lease path && lease.token = token) active in
  let term path = function
    | D.Token token -> token_exists path token
    | D.Etag tag -> match reader.Reader.stat path with
        | Some {etag=Some current; _} -> weak_tag current = weak_tag tag
        | _ -> false in
  if condition <> None && not (List.exists (fun (path, groups) ->
    List.exists (List.for_all (function
      | D.Is value -> term path value
      | D.Not value -> not (term path value))) groups) tagged) then fail 412

let guard t principal headers source operation reader active =
  (match authorize t principal with Some Read_write -> () | _ -> fail 403);
  http_conditions ~read:false headers (reader.Reader.stat source);
  if_conditions t headers source reader active;
  let tagged = match if_header headers with
    | None -> []
    | Some (D.Untagged groups) -> [source, groups]
    | Some (D.Tagged values) -> List.map (fun (tag, groups) ->
        path t tag, groups) values in
  let direct, recursive = match operation with
    | Put (p, _) ->
        let paths = if reader.Reader.stat p = None then
          p :: Option.to_list (Path.parent p) else [p] in paths, []
    | Mkcol p -> p :: Option.to_list (Path.parent p), []
    | Proppatch (p, _) | Check p -> [p], []
    | Delete p -> p :: Option.to_list (Path.parent p), [p]
    | Copy {dst; _} -> dst :: Option.to_list (Path.parent dst), [dst]
    | Move {src; dst; _} -> src :: dst ::
        (Option.to_list (Path.parent src) @ Option.to_list (Path.parent dst)), [src; dst]
    | Lock (p, _, _, _, _) when reader.Reader.stat p = None ->
        Option.to_list (Path.parent p), []
    | Lock _ | Refresh _ | Unlock _ -> [], [] in
  List.iter (fun lease ->
    if List.exists (covers lease) direct ||
      List.exists (fun p -> Path.under ~prefix:p lease.path) recursive then begin
      let submitted = List.exists (fun (_resource, groups) ->
        List.exists (List.exists (function
          | D.Is (D.Token token) | D.Not (D.Token token) -> token = lease.token
          | _ -> false)) groups) tagged in
      if lease.principal <> principal || not submitted then
        fail ~conditions:[D.dav "lock-token-submitted"] 423
    end) active
let locked_response t ~refresh respond code lease =
  let body = D.encode_xml (prop "prop" [D.Element
    (D.Server.lockdiscovery [lock_value t (now t) lease])]) in
  answer respond code body ~headers:((if refresh then [] else
    ["Lock-Token", D.Token.encode lease.token]) @
    ["Timeout", D.encode_timeout (Option.get (lock_value t (now t) lease).timeout)])
let perform t principal headers source operation respond =
  let writer = required_writer t in
  match writer.mutate operation ~guard:(guard t principal headers source operation) with
  | Completed code -> answer respond code ""
  | Written (code, entry) ->
      answer respond code "" ~headers:(match entry.etag with
        | None -> [] | Some tag -> ["ETag", tag])
  | Locked (code, lease) -> locked_response t
      ~refresh:(match operation with Refresh _ -> true | _ -> false)
      respond code lease

let handle t principal access headers source meth (req : P.Req.t @ local)
    (respond : P.Resp.respond @ local) =
  if List.mem meth read_methods then
    if_conditions t headers source t.reader (leases t);
  match meth with
  | "OPTIONS" ->
      no_body t req;
      answer respond 200 "" ~headers:["Allow", String.concat ", "
        (read_methods @ if can_write t access then write_methods else []);
        "DAV", (if t.writer <> None then "1, 2" else "1")]
  | "PROPFIND" ->
      let level = depth headers `Infinity in
      if level = `Infinity then fail ~conditions:[D.dav "propfind-finite-depth"] 403;
      let data = body t req in
      let query = if data = "" then D.Allprop []
        else protocol (D.Server.propfind (xml t headers data)) in
      let entry = existing t.reader source in
      http_conditions ~read:false headers (Some entry);
      let children = if level = `One && entry.kind = Collection then
        t.reader.list source else [] in
      if List.length children >= t.limits.max_resources then fail 507;
      (* Charge each member before retaining the complete multistatus tree.
         This bounds repeated lockdiscovery and dead-property expansion too. *)
      let remaining = ref t.limits.max_response_bytes in
      let bounded path entry =
        let r = response t path entry query in
        let encoded = try D.Server.multistatus ~max_bytes:!remaining
          (multistatus [r]) with D.Output_too_large -> fail 507 in
        remaining := !remaining - String.length encoded;
        r in
      let responses = bounded source entry ::
        List.map (fun (name, entry) -> bounded (Path.child source name) entry) children in
      xml_result t respond 207 (multistatus responses)
  | "GET" | "HEAD" ->
      no_body t req;
      let entry = existing t.reader source in
      if entry.kind = Collection then begin
        if meth <> "HEAD" then fail 405;
        http_conditions ~read:true headers (Some entry);
        answer respond 200 ""
      end else begin
      let request_headers = headers in
      let () = t.reader.read source (fun entry write ->
        let headers = ["Cache-Control", "private, no-cache";
          "X-Content-Type-Options", "nosniff";
          "Content-Disposition", "attachment"] @
          (match entry.etag with None -> [] | Some value -> ["ETag", value]) in
        match http_conditions ~read:true request_headers (Some entry) with
        | () ->
            P.Resp.v respond ~headers:(P.Headers.of_list headers)
              ~last_modified:entry.modified
              ~content_type:(This (P.Mime.of_path (String.concat "/" source)))
              (P.Body.Stream {length=Some entry.length; write; trailers=P.Headers.empty})
        | exception Error (304, _) ->
            answer respond 304 "" ~headers:(List.filter (fun (k, _) -> k = "ETag") headers)) in ()
      end
  | "PUT" ->
      no_root source;
      perform t principal headers source (Put (source, P.Req.input req)) respond
  | "MKCOL" -> no_root source; no_body t req;
      perform t principal headers source (Mkcol source) respond
  | "DELETE" -> no_root source; no_body t req;
      perform t principal headers source (Delete source) respond
  | "COPY" | "MOVE" ->
      no_root source; no_body t req;
      let dst = destination t headers source and overwrite = overwrite headers in
      let level = depth headers `Infinity in
      let operation = if meth = "MOVE" then begin
        if level <> `Infinity then fail 400;
        Move {src=source; dst; overwrite}
      end else
        let depth = match level with `Zero -> `Zero | `Infinity -> `Infinity | _ -> fail 400 in
        Copy {src=source; dst; overwrite; depth} in
      perform t principal headers source operation respond
  | "PROPPATCH" ->
      let updates = protocol (D.Server.proppatch (xml t headers (body t req))) in
      let names = List.concat_map (function D.Set xs -> List.map (fun p -> p.D.name) xs
        | D.Remove xs -> xs) updates in
      if List.length names > 128 then fail 413;
      let protected = List.exists (fun (ns, _) -> ns = "DAV:") names in
      if protected then begin
        let w = required_writer t in
        let operation = Check source in
        ignore (w.mutate operation
          ~guard:(guard t principal headers source operation))
      end;
      let groups = if protected then List.map (fun name ->
        let code = if fst name = "DAV:" then 403 else 424 in
        {D.status=code; properties=[D.element name []];
         errors=(if code = 403 then [D.element (D.dav "cannot-modify-protected-property") []] else []);
         description=None}) names
      else begin
        let w = required_writer t in
        let operation = Proppatch (source, updates) in
        ignore (w.mutate operation ~guard:(guard t principal headers source operation));
        [{D.status=200; properties=List.map (fun n -> D.element n []) names;
          errors=[]; description=None}]
      end in
      xml_result t respond 207 (multistatus [{D.hrefs=[href t source (existing t.reader source).kind];
        outcome=D.Properties groups; errors=[]; description=None; location=None}])
  | "LOCK" ->
      let data = body t req in
      let operation = if data = "" then
        let token = match if_header headers with
          | Some (D.Untagged [[D.Is (D.Token token)]]) -> token
          | Some (D.Tagged [tag, [[D.Is (D.Token token)]]])
            when Path.equal (path t tag) source -> token
          | _ -> fail 400 in
        Refresh (source, principal, token, timeout headers)
      else
        let scope, owner = protocol (D.Server.lockinfo (xml t headers data)) in
        if scope <> D.Exclusive then fail 501;
        (match owner with None -> () | Some owner ->
          try ignore (D.encode_xml ~max_bytes:4096 owner)
          with D.Output_too_large -> fail 413);
        let level = match depth headers `Infinity with
          | `Zero -> `Zero | `Infinity -> `Infinity | _ -> fail 400 in
        Lock (source, principal, level, timeout headers, owner) in
      perform t principal headers source operation respond
  | "UNLOCK" ->
      no_body t req;
      let token = match D.Token.decode (required headers "lock-token") with
        | Some token -> token | None -> fail 400 in
      perform t principal headers source (Unlock (source, principal, token)) respond
  | _ -> answer respond 405 "" ~headers:["Allow", String.concat ", " read_methods]

let admit t (req : P.Req.t @ local) (respond : P.Resp.respond @ local) =
  let headers = fields req in
  try
    (match P.Req.transport req, U.scheme t.origin with
     | P.Req.Secure, This "https" | P.Req.Loopback, This "http" -> ()
     | _ -> fail 403);
    let principal, access = authenticate t headers in
    let host = required headers "host" in
    let scheme = decoded (U.scheme t.origin) in
    let authority = decoded (U.of_string (scheme ^ "://" ^ host ^ "/")) in
    if not (same_origin authority t.origin) then fail 400;
    let source = path t (P.Req.globalize (P.Req.target req)) in
    let meth = P.Method.to_string (P.Req.meth req) in
    if List.mem meth write_methods && not (can_write t access) then fail 403;
    if not (List.mem meth (read_methods @ write_methods)) then fail 405;
    if meth = "COPY" || meth = "MOVE" then begin
      ignore (destination t headers source);
      ignore (overwrite headers)
    end;
    if single headers "content-encoding" <> None then fail 415;
    ignore (single headers "content-type");
    ignore (single headers "depth");
    ignore (if_header headers);
    if t.active >= t.limits.max_active then fail 503;
    let max_body = if meth = "PUT" then t.limits.max_file_bytes
      else Int64.of_int t.limits.max_xml_bytes in
    Some (P.Site.accept ~max_body (fun req respond ->
      if t.active >= t.limits.max_active then reject respond 503 []
      else begin
      t.active <- t.active + 1;
      (match
        try handle t principal access headers source meth req respond with
        | Error (code, conditions) ->
            reject ~allow:(String.concat ", " (read_methods @
              if can_write t access then write_methods else []))
              respond code conditions
        | P.Req.Input.Rejected code -> reject respond (P.Status.code code) []
       with
       | () -> t.active <- t.active - 1
       | exception exn -> t.active <- t.active - 1; raise exn)
      end))
  with Error (code, conditions) ->
    if code = 401 then begin
      let realm = match t.security with Security.Public -> "DAV"
        | Security.Authenticated s -> s.realm in
      answer respond 401 "" ~headers:["WWW-Authenticate", "Basic realm=\"" ^ realm ^ "\""]
    end else if code = 405 then
      answer respond 405 "" ~headers:["Allow", String.concat ", " read_methods]
    else reject respond code conditions;
    None

let mount ~at (export @ portable) site =
  P.Site.with_endpoint ~at ~admit:(fun env req respond ->
    let t = export env in
    if t.at <> at then invalid_arg "Proffer_dav.mount: export prefix mismatch";
    let result = admit t req respond in result) site
