(* webdavz_xml.ml - XML tree types and xmlm codec
   RFC 4918 Section 14 — XML element definitions *)

type fqname = string * string
type attribute = fqname * string

type tree =
  | Pcdata of string
  | Node of string * string * attribute list * tree list

let dav_ns = "DAV:"
let carddav_ns = "urn:ietf:params:xml:ns:carddav"

(* RFC 7231 Section 7.1.1.1 — HTTP-date (IMF-fixdate).
   Used by getlastmodified (RFC 4918 Section 15.7). *)
let http_date_of_unix_time t =
  let open Unix in
  let tm = gmtime t in
  let weekday = [|"Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"|] in
  let month = [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|] in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    weekday.(tm.tm_wday) tm.tm_mday month.(tm.tm_mon)
    (tm.tm_year + 1900) tm.tm_hour tm.tm_min tm.tm_sec

(* RFC 3339 / ISO 8601 — used by creationdate (RFC 4918 Section 15.1). *)
let iso8601_of_unix_time t =
  let open Unix in
  let tm = gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let[@inline] dav_node name children = Node (dav_ns, name, [], children)
let[@inline] pcdata s = Pcdata s

(* RFC 4918 Section 16 — precondition/postcondition error XML body.
   e.g. error_xml "resource-must-be-null" produces:
   <D:error xmlns:D="DAV:"><D:resource-must-be-null/></D:error> *)
let error_xml element = dav_node "error" [dav_node element []]

let find_children ns name children =
  List.filter (fun t ->
    match t with
    | Node (ns', name', _, _) -> String.equal ns ns' && String.equal name name'
    | Pcdata _ -> false)
    children

let[@inline] node_name = function
  | Node (ns, name, _, _) -> Some (ns, name)
  | Pcdata _ -> None

(* xmlm-based parsing — RFC 4918 requires well-formed XML *)
let parse xml_string =
  let input = Xmlm.make_input (`String (0, xml_string)) in
  let rec parse_node () =
    match Xmlm.input input with
    | `El_start ((ns, name), attrs) ->
        let attrs = List.map (fun ((ans, aname), value) ->
          ((ans, aname), value)) attrs
        in
        let children = parse_children [] in
        Some (Node (ns, name, attrs, children))
    | `Data s -> Some (Pcdata s)
    | `El_end -> None
    | `Dtd _ -> parse_node ()
  and parse_children acc =
    match parse_node () with
    | Some node -> parse_children (node :: acc)
    | None -> List.rev acc
  in
  try
    match parse_node () with
    | Some tree -> Some tree
    | None -> None
  with Xmlm.Error _ -> None

(* Shared namespace prefix mapping for xmlm output *)
let ns_prefix ns =
  if String.equal ns dav_ns then Some "D"
  else if String.equal ns carddav_ns then Some "C"
  else None

(* xmlm-based serialization with XML declaration.
   RFC 4918 responses should include the XML declaration. *)
let serialize tree =
  let buf = Buffer.create 1024 in
  let output = Xmlm.make_output ~ns_prefix (`Buffer buf) in
  let rec write_tree = function
    | Pcdata s -> Xmlm.output output (`Data s)
    | Node (ns, name, attrs, children) ->
        Xmlm.output output (`El_start (((ns, name), attrs)));
        List.iter write_tree children;
        Xmlm.output output `El_end
  in
  Xmlm.output output (`Dtd None);
  write_tree tree;
  Buffer.contents buf

(* Compact serialization without XML declaration — for embedding in
   larger documents or tests. *)
let serialize_compact tree =
  let buf = Buffer.create 1024 in
  let output = Xmlm.make_output ~decl:false ~ns_prefix (`Buffer buf) in
  let rec write_tree = function
    | Pcdata s -> Xmlm.output output (`Data s)
    | Node (ns, name, attrs, children) ->
        Xmlm.output output (`El_start (((ns, name), attrs)));
        List.iter write_tree children;
        Xmlm.output output `El_end
  in
  Xmlm.output output (`Dtd None);
  write_tree tree;
  Buffer.contents buf
