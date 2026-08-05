(* webdavz_prop.ml - WebDAV property types
   RFC 4918 Section 15 — DAV properties *)

type t = (Webdavz_xml.fqname * Webdavz_xml.tree list) list

let dav = Webdavz_xml.dav_ns

(* RFC 4918 Section 15 property names *)
let resourcetype    = (dav, "resourcetype")
let displayname     = (dav, "displayname")
let getcontenttype  = (dav, "getcontenttype")
let getcontentlength = (dav, "getcontentlength")
let getetag         = (dav, "getetag")
let getlastmodified = (dav, "getlastmodified")
let creationdate    = (dav, "creationdate")
let supportedlock   = (dav, "supportedlock")
let lockdiscovery   = (dav, "lockdiscovery")

let find name props =
  match List.find_opt (fun ((ns, ln), _) ->
    let (qns, qln) = name in
    String.equal ns qns && String.equal ln qln) props
  with
  | Some (_, v) -> Some v
  | None -> None

let is_collection props =
  match find resourcetype props with
  | Some trees ->
    List.exists (fun t ->
      match Webdavz_xml.node_name t with
      | Some (ns, name) -> String.equal ns dav && String.equal name "collection"
      | None -> false)
      trees
  | None -> false
