(* carddavz_report.ml - CardDAV REPORT handling *)

open Webdavz.Xml

type text_match = {
  collation : string;
  match_type : string;
  value : string;
}

type prop_filter = {
  name : string;
  text_match : text_match option;
}

type address_filter = {
  prop_filters : prop_filter list;
}

type report =
  | Addressbook_query of {
      filter : address_filter;
      props : fqname list;
    }
  | Addressbook_multiget of {
      hrefs : string list;
      props : fqname list;
    }

let carddav_ns = Webdavz.Xml.carddav_ns

(* Extract property names from DAV:prop children *)
let extract_props children =
  let prop_nodes = find_children dav_ns "prop" children in
  match prop_nodes with
  | Node (_, _, _, prop_children) :: _ ->
    List.filter_map (fun t ->
      match t with
      | Node (ns, name, _, _) -> Some (ns, name)
      | Pcdata _ -> None)
      prop_children
  | _ -> []

(* Parse text-match element *)
let parse_text_match children =
  match find_children carddav_ns "text-match" children with
  | Node (_, _, attrs, text_children) :: _ ->
    let match_type = match List.assoc_opt ("", "match-type") attrs with
      | Some s -> s
      | None -> "contains"
    in
    let collation = match List.assoc_opt ("", "collation") attrs with
      | Some s -> s
      | None -> "i;unicode-casemap"
    in
    let value = match text_children with
      | Pcdata s :: _ -> s
      | _ -> ""
    in
    Some { collation; match_type; value }
  | _ -> None

(* Parse prop-filter elements *)
let parse_prop_filters children =
  let filter_nodes = find_children carddav_ns "prop-filter" children in
  List.filter_map (fun node ->
    match node with
    | Node (_, _, attrs, filter_children) ->
      begin match List.assoc_opt ("", "name") attrs with
      | Some name ->
        let text_match = parse_text_match filter_children in
        Some { name; text_match }
      | None -> None
      end
    | Pcdata _ -> None)
    filter_nodes

let parse_report xml =
  match parse xml with
  | None -> None
  | Some (Node (ns, "addressbook-query", _, children)) when String.equal ns carddav_ns ->
    let props = extract_props children in
    let filter_nodes = find_children carddav_ns "filter" children in
    let prop_filters = match filter_nodes with
      | Node (_, _, _, filter_children) :: _ -> parse_prop_filters filter_children
      | _ -> []
    in
    Some (Addressbook_query {
      filter = { prop_filters };
      props;
    })
  | Some (Node (ns, "addressbook-multiget", _, children)) when String.equal ns carddav_ns ->
    let props = extract_props children in
    let href_nodes = find_children dav_ns "href" children in
    let hrefs = List.filter_map (fun node ->
      match node with
      | Node (_, _, _, [Pcdata s]) -> Some s
      | _ -> None)
      href_nodes
    in
    Some (Addressbook_multiget { hrefs; props })
  | Some _ -> None

(* Text matching per RFC 6352 *)
let text_contains ~value ~pattern =
  let vlen = String.length value in
  let plen = String.length pattern in
  if plen > vlen then false
  else begin
    let found = ref false in
    for i = 0 to vlen - plen do
      if not !found then begin
        let matches = ref true in
        for j = 0 to plen - 1 do
          let vc = Char.lowercase_ascii (String.get value (i + j)) in
          let pc = Char.lowercase_ascii (String.get pattern j) in
          if not (Char.equal vc pc) then matches := false
        done;
        if !matches then found := true
      end
    done;
    !found
  end

let text_equals ~value ~pattern =
  String.equal (String.lowercase_ascii value) (String.lowercase_ascii pattern)

let text_starts_with ~value ~pattern =
  let vlen = String.length value in
  let plen = String.length pattern in
  plen <= vlen &&
  String.equal
    (String.lowercase_ascii (String.sub value 0 plen))
    (String.lowercase_ascii pattern)

let text_ends_with ~value ~pattern =
  let vlen = String.length value in
  let plen = String.length pattern in
  plen <= vlen &&
  String.equal
    (String.lowercase_ascii (String.sub value (vlen - plen) plen))
    (String.lowercase_ascii pattern)

let text_match_applies tm value =
  let pattern = tm.value in
  match tm.match_type with
  | "equals" -> text_equals ~value ~pattern
  | "starts-with" -> text_starts_with ~value ~pattern
  | "ends-with" -> text_ends_with ~value ~pattern
  | _ -> text_contains ~value ~pattern

let prop_filter_matches pf vcard =
  let name = String.uppercase_ascii pf.name in
  let matching_props = List.filter (fun p ->
    String.equal p.Carddavz_vcard.name name)
    vcard.Carddavz_vcard.properties
  in
  match pf.text_match with
  | None ->
    (* "is defined" test — at least one matching property *)
    matching_props <> []
  | Some tm ->
    List.exists (fun p ->
      text_match_applies tm p.Carddavz_vcard.value)
      matching_props

let vcard_matches_filter filter vcard =
  List.for_all (fun pf -> prop_filter_matches pf vcard)
    filter.prop_filters
