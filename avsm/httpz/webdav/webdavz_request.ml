(* webdavz_request.ml - PROPFIND/PROPPATCH request body parsing *)

open Webdavz_xml

type propfind =
  | Allprop of fqname list
  | Propname
  | Props of fqname list

type propupdate =
  | Set of fqname * tree list
  | Remove of fqname

type depth = Zero | One | Infinity

(* RFC 4918 Section 10.2: Depth header values are "0", "1", or "infinity".
   Missing header defaults to infinity per Section 9.1. *)
let parse_depth = function
  | None -> Ok Infinity
  | Some "0" -> Ok Zero
  | Some "1" -> Ok One
  | Some "infinity" -> Ok Infinity
  | Some _ -> Error `Bad_request

(* Extract property names from DAV:prop children *)
let extract_prop_names children =
  let prop_nodes = find_children dav_ns "prop" children in
  match prop_nodes with
  | [] -> []
  | Node (_, _, _, prop_children) :: _ ->
    List.filter_map (fun t ->
      match t with
      | Node (ns, name, _, _) -> Some (ns, name)
      | Pcdata _ -> None)
      prop_children
  | Pcdata _ :: _ -> []

let parse_propfind xml =
  match parse xml with
  | None -> None
  | Some (Node (_, "propfind", _, children)) ->
    let allprop = find_children dav_ns "allprop" children in
    let propname = find_children dav_ns "propname" children in
    if propname <> [] then
      Some Propname
    else if allprop <> [] then begin
      let include_nodes = find_children dav_ns "include" children in
      let includes = match include_nodes with
        | Node (_, _, _, inc_children) :: _ ->
          List.filter_map (fun t ->
            match t with
            | Node (ns, name, _, _) -> Some (ns, name)
            | Pcdata _ -> None)
            inc_children
        | _ -> []
      in
      Some (Allprop includes)
    end else begin
      let props = extract_prop_names children in
      Some (Props props)
    end
  | Some _ -> None

let propfind_of_body = function
  | None -> Allprop []
  | Some s when String.length s = 0 -> Allprop []
  | Some s ->
    match parse_propfind s with
    | Some pf -> pf
    | None -> Allprop []

let parse_proppatch xml =
  match parse xml with
  | None -> None
  | Some (Node (_, "propertyupdate", _, children)) ->
    let updates = List.filter_map (fun child ->
      match child with
      | Node (ns, "set", _, set_children) when String.equal ns dav_ns ->
        let prop_nodes = find_children dav_ns "prop" set_children in
        begin match prop_nodes with
        | Node (_, _, _, prop_children) :: _ ->
          begin match prop_children with
          | Node (pns, pname, _, value) :: _ ->
            Some (Set ((pns, pname), value))
          | _ -> None
          end
        | _ -> None
        end
      | Node (ns, "remove", _, rm_children) when String.equal ns dav_ns ->
        let prop_nodes = find_children dav_ns "prop" rm_children in
        begin match prop_nodes with
        | Node (_, _, _, prop_children) :: _ ->
          begin match prop_children with
          | Node (pns, pname, _, _) :: _ ->
            Some (Remove (pns, pname))
          | _ -> None
          end
        | _ -> None
        end
      | _ -> None)
      children
    in
    Some updates
  | Some _ -> None
