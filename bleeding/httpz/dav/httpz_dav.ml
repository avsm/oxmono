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

let encode_xml root =
  (* Reserve every caller-declared prefix, including bindings on ancestors of
     newly constructed children. Generated prefixes then cannot change the
     meaning of a QName in text or attributes anywhere in the document. *)
  let reserved = Hashtbl.create 16 in
  let rec reserve e =
    let seen = Hashtbl.create (List.length e.attrs) in
    List.iter (fun ((ns, local) as name, _) ->
      if Hashtbl.mem seen name then invalid_arg "Httpz_dav.encode_xml: duplicate attribute";
      Hashtbl.add seen name ();
      if ns = Xmlm.ns_xmlns then Hashtbl.replace reserved local ()) e.attrs;
    List.iter (function Element e -> reserve e | Text _ -> ()) e.children
  in
  reserve root;
  let b = Buffer.create 256 in
  let output = Xmlm.make_output (`Buffer b) in
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
let href e = let s = String.trim (text_exn e) in ignore (href_uri s); s
let resolve_href ~base s = protect (fun () ->
  let u = href_uri s in
  let b = href_uri base in
  if Httpz_uri.scheme b = Null then invalid "href base must be absolute";
  Httpz_uri.to_string (Httpz_uri.resolve ~base:b u))
let multistatus root = protect (fun () ->
  if root.name <> dav "multistatus" then invalid "expected DAV:multistatus";
  ignore (elems root);
  let form = ref None in
  let check_href e =
    let s = href e in
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
    let location = Option.map (fun e -> href (required (dav "href") e)) (optional (dav "location") e) in
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
