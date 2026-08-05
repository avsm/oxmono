(* webdavz_response.ml - 207 Multi-Status response generation
   RFC 4918 Section 13 — multi-status response
   RFC 4918 Section 14.16 — multistatus XML element *)

open Webdavz_xml

type propstat = {
  status : Httpz.Res.status;
  props : tree list;
}

type response = {
  href : string;
  propstats : propstat list;
}

let[@inline] propstat_ok props = { status = Httpz.Res.Success; props }
let[@inline] propstat_not_found props = { status = Httpz.Res.Not_found; props }

let[@inline] prop_node (ns, name) values = Node (ns, name, [], values)
let[@inline] empty_prop_node (ns, name) = Node (ns, name, [], [])

(* RFC 4918 Section 14.28 — status element uses full HTTP status line *)
let status_line status =
  Printf.sprintf "HTTP/1.1 %d %s"
    (Httpz.Res.status_code status)
    (Httpz.Res.status_reason status)

(* RFC 4918 Section 14.22 — propstat *)
let propstat_to_tree ps =
  let status_node = dav_node "status" [pcdata (status_line ps.status)] in
  let prop = dav_node "prop" ps.props in
  dav_node "propstat" [prop; status_node]

(* RFC 4918 Section 14.24 — response *)
let response_to_tree r =
  let href = dav_node "href" [pcdata r.href] in
  let propstats = List.map propstat_to_tree r.propstats in
  dav_node "response" (href :: propstats)

(* RFC 4918 Section 14.16 — multistatus *)
let multistatus responses =
  let children = List.map response_to_tree responses in
  let root = Node (dav_ns, "multistatus", [], children) in
  serialize root
