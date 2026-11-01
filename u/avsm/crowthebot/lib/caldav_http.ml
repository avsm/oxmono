module D = Httpz_dav
module M = Fetch.Middleware

let xml_limits = { D.max_bytes = 65536; max_depth = 8; max_nodes = 2048 }

let elements (e : D.element) =
  List.filter_map (function D.Element e -> Some e | Text _ -> None) e.children

let rec attributes (e : D.element) =
  List.for_all (fun ((ns, _), _) -> ns = D.ns_xmlns) e.attrs
  && List.for_all attributes (elements e)

let container (e : D.element) =
  List.for_all
    (function D.Text s -> String.trim s = "" | Element _ -> true)
    e.children

let empty e = container e && elements e = []
let named name (e : D.element) = e.name = D.dav name
let property_names e = container e && List.for_all empty (elements e)

let propfind root =
  named "propfind" root && container root
  &&
  match elements root with
  | [ p ] when named "prop" p -> property_names p
  | [ p ] when named "allprop" p || named "propname" p -> empty p
  | [ all; include_ ] when named "allprop" all && named "include" include_ ->
      empty all && property_names include_
  | _ -> false

let text e = match D.text e with Ok s -> Some s | Error _ -> None

let sync root =
  named "sync-collection" root
  && container root
  &&
  match elements root with
  | [ token; level; limit; props ] -> (
      named "sync-token" token
      && elements token = []
      && named "sync-level" level
      && text level = Some "1"
      && named "limit" limit && container limit
      && (match elements limit with
        | [ n ] -> named "nresults" n && text n = Some "20"
        | _ -> false)
      && named "prop" props && property_names props
      &&
      match elements props with
      | [ etag ] -> named "getetag" etag
      | _ -> false)
  | _ -> false

let request_body check = function
  | M.String body -> (
      match D.parse_xml ~limits:xml_limits body with
      | Ok root -> check root
      | Error _ -> false)
  | Empty | Stream _ -> false

let rec same_xml a b =
  let attrs e =
    List.filter (fun ((ns, _), _) -> ns <> D.ns_xmlns) e.D.attrs
    |> List.sort compare
  in
  let children e =
    List.filter
      (function D.Text s -> String.trim s <> "" | _ -> true)
      e.D.children
  in
  a.D.name = b.D.name
  && attrs a = attrs b
  &&
  let a = children a and b = children b in
  List.length a = List.length b
  && List.for_all2
       (fun a b ->
         match (a, b) with
         | D.Text a, D.Text b -> a = b
         | D.Element a, D.Element b -> same_xml a b
         | _ -> false)
       a b

let agenda root =
  try
    let q =
      match Caldav.Report.query_of_xml root with
      | Ok q -> q
      | Error _ -> raise Exit
    in
    let range =
      match q.props with
      | Caldav.Report.Prop (_, Some { expand = Some r; _ }) -> r
      | _ -> raise Exit
    in
    let instant = function
      | Some d when d.Ical.Date.time.utc -> (
          match Ical.Date.to_ptime d with Some p -> p | None -> raise Exit)
      | _ -> raise Exit
    in
    let w =
      Caldav_agenda.window ~start:(instant range.start)
        ~finish:(instant range.finish)
    in
    same_xml root (Caldav.Report.query_to_xml (Caldav_agenda.query w))
  with _ -> false

let filter (req : M.request) =
  let reject () =
    `Reject
      "CalDAV mirror permits only discovery, sync and bounded agenda reads"
  in
  (* No method overrides, lock tokens, scheduling headers or URL parameters
     can give a nominally read-only request a second meaning. Credentials are
     attached by the private client before this check. *)
  let headers =
    Http.Header.to_list req.headers
    |> List.for_all (fun (name, _) ->
        List.mem
          (String.lowercase_ascii name)
          [ "accept"; "authorization"; "content-type"; "depth" ])
  in
  if (not headers) || M.Url.has_query req.url || M.Url.has_fragment req.url then
    reject ()
  else
    let allowed =
      match Http.Method.to_string req.meth with
      | "GET" -> req.body = M.Empty
      | "PROPFIND" ->
          List.mem
            (Http.Header.get_multi req.headers "depth")
            [ [ "0" ]; [ "1" ] ]
          && request_body
               (fun root -> attributes root && propfind root)
               req.body
      | "REPORT" ->
          Http.Header.get_multi req.headers "depth" = [ "0" ]
          && request_body (fun root -> attributes root && sync root) req.body
          || Http.Header.get_multi req.headers "depth" = [ "1" ]
             && request_body agenda req.body
      | _ -> false
    in
    if allowed then `Allow else reject ()

let read_only ~url fetch =
  let url = Tool_config.endpoint ~allow_http:false url in
  let uri = Uri.of_string url in
  let origin = Uri.with_path uri "/" |> Uri.to_string in
  Fetch.restrict ~under:[ origin ]
    ~methods:
      [ `GET; Http.Method.of_string "PROPFIND"; Http.Method.of_string "REPORT" ]
    ~filter fetch
