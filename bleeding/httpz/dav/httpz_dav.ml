type encoding = Xmlm.encoding
let ns_xml = Xmlm.ns_xml
let ns_xmlns = Xmlm.ns_xmlns
type name = string * string
let dav local = "DAV:", local
type xml = Text of string | Element of element
and element = { name : name; attrs : (name * string) list; children : xml list }
type limits = { max_bytes : int; max_depth : int; max_nodes : int }
let default_limits = { max_bytes = 8 * 1024 * 1024; max_depth = 64; max_nodes = 100_000 }
let validate_limits l =
  if l.max_bytes < 1 || l.max_depth < 1 || l.max_nodes < 1 then
    invalid_arg "Httpz_dav: limits must be positive"
let element ?(attrs = []) name children = { name; attrs; children }
let children name e = List.filter_map (function
  | Element e when e.name = name -> Some e | _ -> None) e.children
exception Invalid of string
let invalid s = raise (Invalid s)
let protect f = try Ok (f ()) with Invalid s -> Error s
let text_exn e =
  let b = Buffer.create 32 in
  List.iter (function Text s -> Buffer.add_string b s
    | Element _ -> invalid "expected text-only element") e.children;
  Buffer.contents b
let text e = protect (fun () -> text_exn e)
let elems e = List.filter_map (function
  | Element e -> Some e
  | Text s when String.trim s = "" -> None
  | Text _ -> invalid "unexpected text in DAV structure") e.children
let optional name e = match children name e with
  | [] -> None | [v] -> Some v | _ -> invalid ("duplicate " ^ snd name)
let required name e = match optional name e with
  | Some v -> v | None -> invalid ("missing " ^ snd name)
let description e = Option.map text_exn (optional (dav "responsedescription") e)

let parse_xml ?(limits = default_limits) ?encoding source =
  validate_limits limits;
  protect (fun () ->
    if String.length source > limits.max_bytes then invalid "XML byte limit exceeded";
    (* A BOM takes precedence over the transport's charset, RFC 7303 §3.2. *)
    let bom = List.exists (fun p -> String.starts_with ~prefix:p source)
      ["\239\187\191"; "\254\255"; "\255\254"] in
    let input = Xmlm.make_input ~strip:false ~enc:(if bom then None else encoding)
      (`String (0, source)) in
    let nodes = ref 0 in
    let charge count =
      if count > limits.max_nodes - !nodes then invalid "XML node limit exceeded";
      nodes := !nodes + count
    in
    let rec read depth inherited =
      if depth > limits.max_depth then invalid "XML depth limit exceeded";
      match Xmlm.input input with
      | `El_start (name, attrs) ->
          charge (1 + List.length attrs);
          let seen = Hashtbl.create (List.length attrs) in
          List.iter (fun (n, _) ->
            if Hashtbl.mem seen n then invalid "duplicate XML attribute";
            Hashtbl.add seen n ()) attrs;
          let contextual ((ns, n), _) = ns = Xmlm.ns_xmlns || (ns = Xmlm.ns_xml && n = "lang") in
          (* Retain the scope for a property later detached from its response.
             Charge synthesized attributes too, bounding context amplification. *)
          let extra = List.filter (fun (n, _) -> not (Hashtbl.mem seen n)) inherited in
          charge (List.length extra);
          let context = List.filter contextual attrs @ extra in
          let rec body acc = match Xmlm.peek input with
            | `El_end -> ignore (Xmlm.input input); List.rev acc
            | `El_start _ -> body (Element (read (depth + 1) context) :: acc)
            | `Data s -> ignore (Xmlm.input input); charge 1; body (Text s :: acc)
            | `Dtd _ -> invalid "unexpected DTD"
          in
          { name; attrs = attrs @ extra; children = body [] }
      | _ -> invalid "expected XML element"
    in
    try
      (match Xmlm.input input with `Dtd None -> () | _ -> invalid "DTDs are not accepted");
      let root = read 1 [] in
      if not (Xmlm.eoi input) then invalid "trailing XML document";
      root
    with Xmlm.Error ((line, col), err) ->
      invalid (Printf.sprintf "XML %d:%d: %s" line col (Xmlm.error_message err)))

exception Output_too_large
let encode_xml ?(max_bytes = max_int) root =
  if max_bytes < 0 then invalid_arg "Httpz_dav.encode_xml: negative bound";
  let minimum = ref 0 in
  let count n =
    if n > max_bytes - !minimum then raise Output_too_large;
    minimum := !minimum + n in
  (* Reserve every caller-declared prefix, including bindings on ancestors of
     newly constructed children. Generated prefixes then cannot change the
     meaning of a QName in text or attributes anywhere in the document. *)
  let reserved = Hashtbl.create 16 in
  let rec reserve e =
    count 1;
    let seen = Hashtbl.create (List.length e.attrs) in
    List.iter (fun ((ns, local) as name, value) ->
      count (String.length value);
      if Hashtbl.mem seen name then invalid_arg "Httpz_dav.encode_xml: duplicate attribute";
      Hashtbl.add seen name ();
      if ns = Xmlm.ns_xmlns then Hashtbl.replace reserved local ()) e.attrs;
    List.iter (function Element e -> reserve e
      | Text value -> count (String.length value)) e.children
  in
  reserve root;
  let b = Buffer.create 256 in
  let output = Xmlm.make_output (`Fun (fun byte ->
    if Buffer.length b >= max_bytes then raise Output_too_large;
    Buffer.add_char b (Char.chr byte))) in
  let counter = ref 0 in
  let generated = Hashtbl.create 16 in
  let generated_prefix ns = match Hashtbl.find_opt generated ns with
    | Some p -> p
    | None ->
        let rec fresh () =
          incr counter;
          let p = "davz" ^ string_of_int !counter in
          if Hashtbl.mem reserved p then fresh () else p
        in
        let p = fresh () in
        Hashtbl.add generated ns p;
        p
  in
  let valid_text s =
    let rec loop i =
      if i < String.length s then begin
        let d = String.get_utf_8_uchar s i in
        let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if not (Uchar.utf_decode_is_valid d) ||
          not (c = 9 || c = 10 || c = 13 || c >= 0x20 && c <= 0xd7ff ||
               c >= 0xe000 && c <= 0xfffd || c >= 0x10000 && c <= 0x10ffff) then
          invalid_arg "Httpz_dav.encode_xml: invalid XML character";
        loop (i + Uchar.utf_decode_length d)
      end
    in loop 0
  in
  let rec write e =
    let available = Hashtbl.create 8 in
    List.iter (fun ((ns, p), uri) ->
      if ns = Xmlm.ns_xmlns && p <> "xmlns" then
        Hashtbl.replace available uri p) e.attrs;
    let bound = Hashtbl.create 8 in
    let declarations = ref [] in
    let selected = Hashtbl.create 8 in
    let bind ns =
      if ns <> "" && ns <> Xmlm.ns_xml && ns <> Xmlm.ns_xmlns &&
         not (Hashtbl.mem bound ns)
      then begin
        let p = match Hashtbl.find_opt available ns with
          | Some p -> p | None -> generated_prefix ns in
        let name = Xmlm.ns_xmlns, p in
        declarations := (name, ns) :: !declarations;
        Hashtbl.add selected name ();
        Hashtbl.add bound ns ()
      end
    in
    bind (fst e.name);
    List.iter (fun ((ns, _), v) -> valid_text v; bind ns) e.attrs;
    (* Avoid an inherited default namespace changing a no-namespace element. *)
    if fst e.name = "" then begin
      declarations := ((Xmlm.ns_xmlns, "xmlns"), "") :: !declarations;
      Hashtbl.add selected (Xmlm.ns_xmlns, "xmlns") ()
    end;
    (* Xmlm indexes output bindings by URI. Put the selected non-default
       binding last, so a default namespace or a shadowed ancestor prefix
       cannot cause it to emit an attribute or element with the wrong name. *)
    let attrs = List.filter (fun (name, _) -> not (Hashtbl.mem selected name)) e.attrs
      @ List.rev !declarations in
    Xmlm.output output (`El_start (e.name, attrs));
    List.iter (function Text s -> valid_text s; Xmlm.output output (`Data s) | Element e -> write e) e.children;
    Xmlm.output output `El_end
  in
  Xmlm.output output (`Dtd None);
  write root;
  let source = Buffer.contents b in
  (* Xmlm's writer assumes names and Unicode are valid. Verify that assumption
     for application-supplied fragments before handing bytes to a transport. *)
  let l = { max_bytes = max 1 (String.length source); max_depth = max_int; max_nodes = max_int } in
  let rec names_match a b =
    let attrs = Hashtbl.create (List.length b.attrs) in
    List.iter (fun (name, value) -> Hashtbl.add attrs name value) b.attrs;
    a.name = b.name &&
    List.for_all (fun (name, value) -> Hashtbl.find_opt attrs name = Some value) a.attrs &&
    let elements e = List.filter_map (function Element e -> Some e | _ -> None) e.children in
    let ac = elements a and bc = elements b in
    List.length ac = List.length bc && List.for_all2 names_match ac bc in
  (match parse_xml ~limits:l source with
  | Ok e when names_match root e -> ()
  | Ok _ -> invalid_arg "Httpz_dav.encode_xml: invalid expanded name or namespace binding"
  | Error s -> invalid_arg ("Httpz_dav.encode_xml: " ^ s));
  source

type propstat = { properties : element list; status : int; errors : element list; description : string option }
type outcome = Status of int | Properties of propstat list
type response = { hrefs : string list; outcome : outcome; errors : element list;
                  description : string option; location : string option }
type multistatus = { responses : response list; description : string option }
let status e =
  let s = String.trim (text_exn e) in
  if String.length s < 12 || not (String.starts_with ~prefix:"HTTP/1." s) ||
    s.[7] < '0' || s.[7] > '9' || s.[8] <> ' ' ||
    (String.length s > 12 && s.[12] <> ' ') ||
    not (List.for_all (fun n -> s.[n] >= '0' && s.[n] <= '9') [9; 10; 11])
  then invalid "invalid DAV status line";
  let code = int_of_string (String.sub s 9 3) in
  if code < 200 || code > 599 then invalid "invalid DAV status code";
  code
let error_elements e = match optional (dav "error") e with
  | None -> [] | Some e -> elems e
let href_uri s = match Httpz_uri.of_string s with
  | Null -> invalid "invalid DAV href"
  | This u ->
      if Httpz_uri.encoded_fragment u <> Null || Httpz_uri.encoded_userinfo u <> Null then invalid "DAV href has fragment or userinfo";
      (match Httpz_uri.scheme u with
      | This ("http" | "https") when (match Httpz_uri.encoded_host u with This h -> h <> "" | Null -> false) -> ()
      | Null when not (Httpz_uri.has_authority u) && String.starts_with ~prefix:"/" (Httpz_uri.encoded_path u) -> ()
      | _ -> invalid "DAV href must be an HTTP(S) URL or absolute path");
      u
(* A server that writes a member name into an href without encoding it,
   such as Fastmail's file store, is read by encoding the characters an
   absolute path may not hold. Only the shape is repaired: a slash or a
   percent stays as it is. *)
let repair_href s =
  let b = Buffer.create (String.length s + 8) in
  String.iter (fun c -> match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' | '_' | '~' | '/' | ':' | '@' | '!' | '$'
    | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '%' | '?' | '#' -> Buffer.add_char b c
    | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c))) s;
  Buffer.contents b
let href ?(lenient = false) e =
  let s = String.trim (text_exn e) in
  match protect (fun () -> href_uri s) with
  | Ok _ -> s
  | Error _ when lenient -> let r = repair_href s in ignore (href_uri r); r
  | Error e -> invalid e
let resolve_href ~base s = protect (fun () ->
  let u = href_uri s in
  let b = href_uri base in
  if Httpz_uri.scheme b = Null then invalid "href base must be absolute";
  Httpz_uri.to_string (Httpz_uri.resolve ~base:b u))
let multistatus ?lenient root = protect (fun () ->
  if root.name <> dav "multistatus" then invalid "expected DAV:multistatus";
  ignore (elems root);
  let form = ref None in
  let check_href e =
    let s = href ?lenient e in
    let absolute = Httpz_uri.scheme (href_uri s) <> Null in
    (match !form with None -> form := Some absolute
      | Some previous when previous = absolute -> ()
      | _ -> invalid "mixed absolute and path hrefs");
    s
  in
  let response e =
    ignore (elems e);
    let hrefs = List.map check_href (children (dav "href") e) in
    if hrefs = [] then invalid "response has no href";
    let groups = children (dav "propstat") e in
    let outcome = match optional (dav "status") e, groups with
      | Some s, [] -> Status (status s)
      | None, (_ :: _ as groups) when List.length hrefs = 1 ->
          Properties (List.map (fun e ->
            ignore (elems e);
            let properties = elems (required (dav "prop") e) in
            { properties; status = status (required (dav "status") e);
              errors = error_elements e; description = description e }) groups)
      | _ -> invalid "response must contain status or propstat, with one href for propstat"
    in
    let location = Option.map (fun e -> href ?lenient (required (dav "href") e)) (optional (dav "location") e) in
    { hrefs; outcome; errors = error_elements e; description = description e; location }
  in
  { responses = List.map response (children (dav "response") root); description = description root })
let property_results name response = match response.outcome with
  | Status s when s >= 200 && s < 300 -> []
  | Status s -> [Error s]
  | Properties groups -> List.concat_map (fun group ->
      List.filter_map (fun p -> if p.name <> name then None else
        Some (if group.status >= 200 && group.status < 300 then Ok p else Error group.status))
        group.properties) groups
let property name response = match property_results name response with
  | [] -> None
  | [result] -> Some result
  | _ -> invalid_arg "Httpz_dav.property: multiple results; use property_results"

type propfind = Allprop of name list | Propname | Prop of name list
type update = Set of element list | Remove of name list
let el n xs = Element (element (dav n) xs)
let names xs = List.map (fun n -> Element (element n [])) xs
let propfind query =
  let body = match query with
    | Prop ps -> [el "prop" (names ps)]
    | Propname -> [el "propname" []]
    | Allprop [] -> [el "allprop" []]
    | Allprop ps -> [el "allprop" []; el "include" (names ps)] in
  encode_xml (element (dav "propfind") body)
let proppatch updates =
  if updates = [] then invalid_arg "Httpz_dav.proppatch: empty update";
  let body = List.map (function
    | Set [] | Remove [] -> invalid_arg "Httpz_dav.proppatch: empty operation"
    | Set ps -> el "set" [el "prop" (List.map (fun e -> Element e) ps)]
    | Remove ps -> el "remove" [el "prop" (names ps)]) updates in
  encode_xml (element (dav "propertyupdate") body)

type depth = [ `Zero | `One | `Infinity ]
type tree_depth = [ `Zero | `Infinity ]
let encode_depth = function `Zero -> "0" | `One -> "1" | `Infinity -> "infinity"
let decode_depth s = match String.trim s with
  | "0" -> Some `Zero | "1" -> Some `One | "infinity" -> Some `Infinity | _ -> None
type timeout = Infinite | Seconds of int64
let encode_timeout = function
  | Infinite -> "Infinite"
  | Seconds n ->
      if n < 0L || n > 0xffff_ffffL then invalid_arg "Httpz_dav: timeout outside uint32";
      "Second-" ^ Int64.to_string n
let decode_timeout s =
  let s = String.trim s in
  if s = "Infinite" then Some Infinite
  else if not (String.starts_with ~prefix:"Second-" s) then None else
  let n = String.sub s 7 (String.length s - 7) in
  if n = "" || not (String.for_all (function '0'..'9' -> true | _ -> false) n) then None else
  match Int64.of_string_opt n with Some n when n <= 0xffff_ffffL -> Some (Seconds n) | _ -> None

module Token = struct
  type t = string
  let of_string s = match Httpz_uri.of_string s with
    | This u when Httpz_uri.scheme u <> Null && Httpz_uri.encoded_fragment u = Null -> Ok s
    | _ -> Error "lock token must be an absolute URI"
  let to_string s = s
  let encode t = "<" ^ t ^ ">"
  let decode s =
    let s = String.trim s in
    let n = String.length s in
    if n < 3 || s.[0] <> '<' || s.[n-1] <> '>' then None else
    match of_string (String.sub s 1 (n-2)) with Ok t -> Some t | Error _ -> None
end
type term = Token of Token.t | Etag of string
type condition = Is of term | Not of term
type if_condition = Untagged of condition list list | Tagged of (string * condition list list) list
let valid_etag s =
  let n = String.length s in
  let start = if String.starts_with ~prefix:"W/" s then 2 else 0 in
  n >= start + 2 && s.[start] = '"' && s.[n-1] = '"' &&
  let rec loop i = i = n-1 || (let c = Char.code s.[i] in
    (c = 0x21 || c >= 0x23 && c <> 0x7f) && loop (i+1)) in
  loop (start+1)
let strong_etag s = valid_etag s && not (String.starts_with ~prefix:"W/" s)
let encode_if condition =
  let term = function
    | Token t -> Token.encode t
    | Etag s -> if not (valid_etag s) then
        invalid_arg "Httpz_dav.If: invalid entity tag" else "[" ^ s ^ "]" in
  let group xs =
    if xs = [] then invalid_arg "Httpz_dav.If: empty condition list";
    "(" ^ String.concat " " (List.map (function Is t -> term t | Not t -> "Not " ^ term t) xs) ^ ")" in
  let groups xs =
    if xs = [] then invalid_arg "Httpz_dav.If: empty lists";
    String.concat " " (List.map group xs) in
  match condition with
  | Untagged xs -> groups xs
  | Tagged [] -> invalid_arg "Httpz_dav.If: empty tagged lists"
  | Tagged xs -> String.concat " " (List.map (fun (uri, xs) ->
      (match protect (fun () -> href_uri uri) with
      | Ok u when Httpz_uri.scheme u <> Null -> () | _ -> invalid_arg "Httpz_dav.If: invalid resource tag");
      "<" ^ uri ^ "> " ^ groups xs) xs)

type scope = Exclusive | Shared
type lock = { scope : scope; depth : tree_depth; timeout : timeout option;
              token : Token.t option; root : string option; owner : element option }
let lockinfo ?owner scope =
  encode_xml (element (dav "lockinfo")
    ([el "lockscope" [el (match scope with Exclusive -> "exclusive" | Shared -> "shared") []];
      el "locktype" [el "write" []]] @
     match owner with None -> [] | Some xs -> [el "owner" xs]))
let locks root = protect (fun () ->
  if root.name <> dav "prop" then invalid "expected DAV:prop";
  let discovery = required (dav "lockdiscovery") root in
  List.map (fun e ->
    let scope_e = required (dav "lockscope") e in
    let scope = match children (dav "exclusive") scope_e, children (dav "shared") scope_e with
      | [_], [] -> Exclusive | [], [_] -> Shared | _ -> invalid "invalid lock scope" in
    ignore (required (dav "write") (required (dav "locktype") e));
    let depth = match decode_depth (text_exn (required (dav "depth") e)) with
      | Some (`Zero | `Infinity as d) -> d | _ -> invalid "invalid lock depth" in
    let timeout = Option.map (fun e -> match decode_timeout (text_exn e) with
      | Some t -> t | None -> invalid "invalid lock timeout") (optional (dav "timeout") e) in
    let token = Option.map (fun e ->
      let s = String.trim (text_exn (required (dav "href") e)) in
      match Token.of_string s with Ok t -> t | Error s -> invalid s) (optional (dav "locktoken") e) in
    let root = Option.map (fun e -> href (required (dav "href") e)) (optional (dav "lockroot") e) in
    { scope; depth; timeout; token; root; owner = optional (dav "owner") e }
  ) (children (dav "activelock") discovery))

let attr name e = List.assoc_opt name e.attrs
let elements e = List.filter_map (function Element e -> Some e | Text _ -> None) e.children
let find name e = List.find_opt (fun e -> e.name = name) (elements e)
let rec content e = String.trim (String.concat "" (List.map (function
  | Text s -> s | Element e -> content e) e.children))
let leaf name s = element name [Text s]
let empty name = element name []
(* Escapes decode for display, except %2F, which must not become a separator. *)
let href_path s =
  let path = match Httpz_uri.of_string s with This u -> Httpz_uri.encoded_path u | Null -> s in
  let path = match String.index_opt path '?' with Some i -> String.sub path 0 i | None -> path in
  let b = Buffer.create (String.length path) in
  let hex c = match c with '0'..'9' -> Some (Char.code c - 48)
    | 'a'..'f' -> Some (Char.code c - 87) | 'A'..'F' -> Some (Char.code c - 55) | _ -> None in
  let n = String.length path in
  let rec loop i = if i < n then
    match path.[i], (if i + 2 < n then hex path.[i+1] else None), (if i + 2 < n then hex path.[i+2] else None) with
    | '%', Some h, Some l when h * 16 + l <> 0x2f -> Buffer.add_char b (Char.chr (h * 16 + l)); loop (i + 3)
    | c, _, _ -> Buffer.add_char b c; loop (i + 1) in
  loop 0;
  Buffer.contents b
let without_slash s =
  let n = String.length s in
  if n > 1 && s.[n-1] = '/' then String.sub s 0 (n-1) else s
let same_href a b = without_slash (href_path a) = without_slash (href_path b)
let basename s =
  let p = without_slash (href_path s) in
  match String.rindex_opt p '/' with
  | Some i -> String.sub p (i+1) (String.length p - i - 1) | None -> p

module Prop = struct
  let creationdate = dav "creationdate"
  let displayname = dav "displayname"
  let getcontentlanguage = dav "getcontentlanguage"
  let getcontentlength = dav "getcontentlength"
  let getcontenttype = dav "getcontenttype"
  let getetag = dav "getetag"
  let getlastmodified = dav "getlastmodified"
  let resourcetype = dav "resourcetype"
  let lockdiscovery = dav "lockdiscovery"
  let supportedlock = dav "supportedlock"
  let supported_report_set = dav "supported-report-set"
  let principal_url = dav "principal-URL"
  let alternate_uri_set = dav "alternate-URI-set"
  let group_membership = dav "group-membership"
  let owner = dav "owner"
  let current_user_privilege_set = dav "current-user-privilege-set"
  let principal_collection_set = dav "principal-collection-set"
  let current_user_principal = dav "current-user-principal"
  let add_member = dav "add-member"
  let sync_token = dav "sync-token"
  let hrefs e = List.map content (children (dav "href") e)
  let etag e = if e.name <> getetag then None else
    match content e with "" -> None | s -> Some s
  let resource_types e = List.map (fun e -> e.name) (elements e)
  let is_collection e = e.name = resourcetype && List.mem (dav "collection") (resource_types e)
  let reports e = if e.name <> supported_report_set then [] else
    List.concat_map (fun sr -> List.concat_map (fun r -> resource_types r)
      (children (dav "report") sr)) (children (dav "supported-report") e)
  let privileges e = if e.name <> current_user_privilege_set then [] else
    List.concat_map resource_types (children (dav "privilege") e)
  let principal e = if e.name <> current_user_principal then None else
    if find (dav "unauthenticated") e <> None then Some `Unauthenticated
    else match hrefs e with h :: _ -> Some (`Href h) | [] -> None
end

let href r = match r.hrefs with h :: _ -> h | [] -> ""
let find_response m s = List.find_opt (fun r -> same_href (href r) s) m.responses
let success s = s >= 200 && s < 300
let response_status r = match r.outcome with
  | Status s -> s | Properties (g :: _) -> g.status | Properties [] -> 200
let succeeded r = match r.outcome with
  | Status _ -> []
  | Properties groups -> List.concat_map (fun g -> if success g.status then g.properties else []) groups
let find_property name r = List.find_opt (fun e -> e.name = name) (succeeded r)
let property_status name r = match r.outcome with
  | Status _ -> None
  | Properties groups -> List.find_map (fun g ->
      if List.exists (fun e -> e.name = name) g.properties then Some g.status else None) groups
let is_collection r = match find_property Prop.resourcetype r with
  | Some e -> Prop.is_collection e | None -> false
let etag r = Option.bind (find_property Prop.getetag r) Prop.etag
let failures m = List.concat_map (fun r -> match r.outcome with
  | Status s when not (success s) -> [s, r.errors, []]
  | Status _ -> []
  | Properties groups -> List.filter_map (fun g ->
      if success g.status || g.status = 424 then None
      else Some (g.status, g.errors, List.map (fun e -> e.name) g.properties)) groups) m.responses

let mkcol props =
  encode_xml (element (dav "mkcol") [el "set" [el "prop" (List.map (fun e -> Element e) props)]])
let mkcalendar props =
  encode_xml (element ("urn:ietf:params:xml:ns:caldav", "mkcalendar")
    [el "set" [el "prop" (List.map (fun e -> Element e) props)]])
let mkcol_response root = protect (fun () ->
  if root.name <> dav "mkcol-response" && root.name <> ("urn:ietf:params:xml:ns:caldav", "mkcalendar-response")
  then invalid "expected DAV:mkcol-response";
  let synthetic = element (dav "multistatus")
    [el "response" (Element (leaf (dav "href") "/") :: List.map (fun e -> Element e) (elems root))] in
  match multistatus synthetic with
  | Ok { responses = [{ outcome = Properties groups; _ }]; _ } -> groups
  | Ok _ -> invalid "mkcol-response has no propstat"
  | Error s -> invalid s)

module Sync = struct
  type level = [ `One | `Infinite ]
  let request ?(token = "") ?(level = `One) ?limit props =
    encode_xml (element (dav "sync-collection")
      ([el "sync-token" [Text token];
        el "sync-level" [Text (match level with `One -> "1" | `Infinite -> "infinite")]] @
       (match limit with None -> [] | Some n ->
         if n < 0 then invalid_arg "Httpz_dav.Sync.request: negative limit";
         [el "limit" [el "nresults" [Text (string_of_int n)]]]) @
       [el "prop" (names props)]))
  type change = Changed of response | Removed of string | Unsupported of string * element list
  type t = { token : string option; changes : change list; truncated : bool }
  let decode ?lenient ~base root = protect (fun () ->
    let m = match multistatus ?lenient root with Ok m -> m | Error s -> invalid s in
    let token = Option.map content (optional (dav "sync-token") root) in
    let truncated = ref false in
    let changes = List.filter_map (fun r -> match r.outcome with
      | Properties _ -> Some (Changed r)
      | Status 404 -> Some (Removed (href r))
      | Status 507 when same_href (href r) base -> truncated := true; None
      | Status 403 -> Some (Unsupported (href r, r.errors))
      | Status _ -> invalid "unexpected sync response status") m.responses in
    { token; changes; truncated = !truncated })
end

module Condition = struct
  let propfind_finite_depth = dav "propfind-finite-depth"
  let cannot_modify_protected_property = dav "cannot-modify-protected-property"
  let preserved_live_properties = dav "preserved-live-properties"
  let no_external_entities = dav "no-external-entities"
  let lock_token_submitted = dav "lock-token-submitted"
  let no_conflicting_lock = dav "no-conflicting-lock"
  let allow_client_defined_uri = dav "allow-client-defined-uri"
  let valid_sync_token = dav "valid-sync-token"
  let number_of_matches_within_limits = dav "number-of-matches-within-limits"
  let supported_report = dav "supported-report"
  let sync_traversal_supported = dav "sync-traversal-supported"
  let need_privileges = dav "need-privileges"
  let has name errors = List.exists (fun e -> e.name = name) errors
  let hrefs name errors = List.concat_map (fun e -> if e.name = name then Prop.hrefs e else []) errors
end

module Discovery = struct
  type service = [ `Caldav | `Carddav ]
  let label = function `Caldav -> "caldav" | `Carddav -> "carddav"
  let well_known s = "/.well-known/" ^ label s
  let srv_name ~secure s domain = Printf.sprintf "_%s%s._tcp.%s" (label s) (if secure then "s" else "") domain
  let txt_path record = List.find_map (fun kv -> match String.index_opt kv '=' with
    | Some i when String.lowercase_ascii (String.trim (String.sub kv 0 i)) = "path" ->
        Some (String.trim (String.sub kv (i+1) (String.length kv - i - 1)))
    | _ -> None) (String.split_on_char ' ' (String.trim record))
  let mailbox address = match String.rindex_opt address '@' with
    | Some i when i > 0 && i < String.length address - 1 ->
        Some (String.sub address 0 i, String.sub address (i+1) (String.length address - i - 1))
    | _ -> None
  let principal_query = Prop [Prop.current_user_principal; Prop.principal_url]
end

module Server = struct
  let propfind root = protect (fun () ->
    if root.name <> dav "propfind" then invalid "expected DAV:propfind";
    let selected = List.filter (fun e -> List.mem e.name
      [dav "allprop"; dav "propname"; dav "prop"]) (elems root) in
    match selected with
    | [e] when e.name = dav "allprop" ->
        if elems e <> [] then invalid "allprop must be empty";
        let names = match optional (dav "include") root with
          | None -> [] | Some e -> List.map (fun e -> e.name) (elems e) in
        Allprop names
    | [e] when e.name = dav "propname" ->
        if elems e <> [] || optional (dav "include") root <> None then
          invalid "invalid propname";
        Propname
    | [e] ->
        if optional (dav "include") root <> None then invalid "unexpected include";
        Prop (List.map (fun e -> e.name) (elems e))
    | _ -> invalid "expected exactly one property selection")

  let proppatch root = protect (fun () ->
    if root.name <> dav "propertyupdate" then invalid "expected propertyupdate";
    let updates = List.filter_map (fun e ->
      if e.name = dav "set" || e.name = dav "remove" then
        let props = elems (required (dav "prop") e) in
        if props = [] then invalid "empty property update";
        Some (if e.name = dav "set" then Set props
          else Remove (List.map (fun p -> p.name) props))
      else None) (elems root) in
    if updates = [] then invalid "empty propertyupdate";
    updates)

  let lockinfo root = protect (fun () ->
    if root.name <> dav "lockinfo" then invalid "expected lockinfo";
    let scope = match elems (required (dav "lockscope") root) with
      | [e] when e.name = dav "exclusive" -> Exclusive
      | [e] when e.name = dav "shared" -> Shared
      | _ -> invalid "invalid lockscope" in
    (match elems (required (dav "locktype") root) with
    | [e] when e.name = dav "write" -> ()
    | _ -> invalid "unsupported locktype");
    scope, optional (dav "owner") root)

  let if_condition source = protect (fun () ->
    let n = String.length source in
    if n = 0 || n > 16384 then invalid "invalid If length";
    let pos = ref 0 and terms = ref 0 and lists = ref 0 and tags = ref 0 in
    let space () =
      while !pos < n && (source.[!pos] = ' ' || source.[!pos] = '\t') do
        incr pos
      done in
    let delimited left right =
      if !pos >= n || source.[!pos] <> left then invalid "invalid If syntax";
      incr pos;
      let first = !pos in
      while !pos < n && source.[!pos] <> right do incr pos done;
      if !pos = n then invalid "unterminated If term";
      let value = String.sub source first (!pos - first) in
      incr pos;
      value in
    let group () =
      incr lists;
      if !lists > 128 then invalid "too many If lists";
      incr pos;
      let rec loop acc =
        space ();
        if !pos >= n then invalid "unterminated If list";
        if source.[!pos] = ')' then begin
          incr pos;
          if acc = [] then invalid "empty If list";
          List.rev acc
        end else begin
          incr terms;
          if !terms > 512 then invalid "too many If terms";
          let negated = !pos + 3 < n && String.sub source !pos 3 = "Not" in
          if negated then begin
            pos := !pos + 3;
            if source.[!pos] <> ' ' && source.[!pos] <> '\t' then
              invalid "Not requires whitespace";
            space ()
          end;
          if !pos >= n then invalid "missing If term";
          let term = match source.[!pos] with
            | '<' ->
                let token = delimited '<' '>' in
                (match Token.of_string token with
                | Ok t -> Token t | Error _ -> invalid "invalid state token")
            | '[' ->
                let tag = delimited '[' ']' in
                if not (valid_etag tag) then invalid "invalid entity tag";
                Etag tag
            | _ -> invalid "invalid If term" in
          loop ((if negated then Not term else Is term) :: acc)
        end in
      loop [] in
    let groups () =
      let rec loop acc =
        space ();
        if !pos < n && source.[!pos] = '(' then loop (group () :: acc)
        else if acc = [] then invalid "missing If lists"
        else List.rev acc in
      loop [] in
    space ();
    if !pos < n && source.[!pos] = '(' then begin
      let value = Untagged (groups ()) in
      space ();
      if !pos <> n then invalid "mixed If forms";
      value
    end else
      let rec loop acc =
        space ();
        if !pos = n then
          if acc = [] then invalid "empty If" else Tagged (List.rev acc)
        else begin
          incr tags;
          if !tags > 64 then invalid "too many resource tags";
          let uri = delimited '<' '>' in
          ignore (href_uri uri);
          let conditions = groups () in
          loop ((uri, conditions) :: acc)
        end in
      loop [])

  let text name value = el name [Text value]
  let status code =
    if code < 100 || code > 599 then invalid_arg "invalid DAV status";
    text "status" (Printf.sprintf "HTTP/1.1 %d Status" code)
  let errors xs = if xs = [] then [] else [el "error" (List.map (fun x -> Element x) xs)]
  let description = function
    | None -> [] | Some s -> [text "responsedescription" s]
  let propstat (p : propstat) =
    el "propstat" ([el "prop" (List.map (fun p -> Element p) p.properties);
      status p.status] @ errors p.errors @ description p.description)
  let multistatus ?max_bytes value =
    let response (r : response) =
      if r.hrefs = [] then invalid_arg "response needs href";
      List.iter (fun h -> ignore (href_uri h)) r.hrefs;
      let outcome = match r.outcome with
        | Status code -> [status code]
        | Properties ps -> List.map propstat ps in
      el "response" (List.map (text "href") r.hrefs @ outcome @ errors r.errors
        @ description r.description @ match r.location with
        | None -> [] | Some h -> [el "location" [text "href" h]]) in
    encode_xml ?max_bytes (element (dav "multistatus")
      (List.map response value.responses @ description value.description))
  let error names = encode_xml (element (dav "error")
    (List.map (fun name -> Element (element name [])) names))
  let lockdiscovery locks =
    let active l =
      el "activelock" ([el "lockscope" [el (match l.scope with
        | Exclusive -> "exclusive" | Shared -> "shared") []];
        el "locktype" [el "write" []];
        text "depth" (encode_depth (l.depth :> depth))]
        @ (match l.owner with None -> [] | Some e -> [Element e])
        @ (match l.timeout with None -> [] | Some t -> [text "timeout" (encode_timeout t)])
        @ (match l.token with None -> [] | Some t -> [el "locktoken" [text "href" (Token.to_string t)]])
        @ (match l.root with None -> [] | Some h -> [el "lockroot" [text "href" h]])) in
    element (dav "lockdiscovery") (List.map active locks)
end
