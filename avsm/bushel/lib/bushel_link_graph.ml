(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Link graph for tracking relationships between Bushel entries *)

module StringSet = Set.Make(String)

type entry_type = [ `Paper | `Project | `Note | `Idea | `Video | `Contact ]

type internal_link = {
  source : string;
  target : string;
  target_type : entry_type;
}

type external_link = {
  source : string;
  domain : string;
  url : string;
}

type t = {
  mutable internal_links : internal_link list;
  mutable external_links : external_link list;
  outbound : (string, StringSet.t) Hashtbl.t;
  backlinks : (string, StringSet.t) Hashtbl.t;
  external_by_entry : (string, StringSet.t) Hashtbl.t;
  external_by_domain : (string, StringSet.t) Hashtbl.t;
}

(** {1 Constructors} *)

let empty () = {
  internal_links = [];
  external_links = [];
  outbound = Hashtbl.create 256;
  backlinks = Hashtbl.create 256;
  external_by_entry = Hashtbl.create 256;
  external_by_domain = Hashtbl.create 64;
}

(** {1 Global Storage} *)

let current_graph : t option ref = ref None

let set_graph graph = current_graph := Some graph
let get_graph () = !current_graph

(** {1 Utility Functions} *)

let entry_type_to_string = function
  | `Paper -> "paper"
  | `Project -> "project"
  | `Note -> "note"
  | `Idea -> "idea"
  | `Video -> "video"
  | `Contact -> "contact"

let entry_type_of_entry = function
  | `Paper _ -> `Paper
  | `Project _ -> `Project
  | `Note _ -> `Note
  | `Idea _ -> `Idea
  | `Video _ -> `Video

let add_to_set_hashtbl tbl key value =
  let current =
    try Hashtbl.find tbl key
    with Not_found -> StringSet.empty
  in
  Hashtbl.replace tbl key (StringSet.add value current)

(** {1 Query Functions} *)

let get_outbound graph slug =
  try StringSet.elements (Hashtbl.find graph.outbound slug)
  with Not_found -> []

let get_backlinks graph slug =
  try StringSet.elements (Hashtbl.find graph.backlinks slug)
  with Not_found -> []

let get_external_links graph slug =
  try StringSet.elements (Hashtbl.find graph.external_by_entry slug)
  with Not_found -> []

let get_entries_linking_to_domain graph domain =
  try StringSet.elements (Hashtbl.find graph.external_by_domain domain)
  with Not_found -> []

(** Query functions using global graph *)

let get_backlinks_for_slug slug =
  match !current_graph with
  | None -> []
  | Some graph -> get_backlinks graph slug

let get_outbound_for_slug slug =
  match !current_graph with
  | None -> []
  | Some graph -> get_outbound graph slug

let get_external_links_for_slug slug =
  match !current_graph with
  | None -> []
  | Some graph -> get_external_links graph slug

(** {1 Pretty Printing} *)

let pp_internal_link ppf (link : internal_link) =
  Fmt.pf ppf "%s -> %s (%s)"
    link.source
    link.target
    (entry_type_to_string link.target_type)

let pp_external_link ppf (link : external_link) =
  Fmt.pf ppf "%s -> %s (%s)"
    link.source
    link.domain
    link.url

let pp ppf graph =
  Fmt.pf ppf "@[<v>Internal links: %d@,External links: %d@,Entries with outbound: %d@,Entries with backlinks: %d@]"
    (List.length graph.internal_links)
    (List.length graph.external_links)
    (Hashtbl.length graph.outbound)
    (Hashtbl.length graph.backlinks)

(** {1 JSON Export} *)

let to_json graph entries =
  let entry_nodes = List.map (fun entry ->
    let slug = Bushel_entry.slug entry in
    let title = Bushel_entry.title entry in
    let entry_type = entry_type_of_entry entry in
    `O [
      ("id", `String slug);
      ("title", `String title);
      ("type", `String (entry_type_to_string entry_type));
      ("group", `String "entry");
    ]
  ) (Bushel_entry.all_entries entries) in

  let contact_nodes = List.map (fun contact ->
    let handle = Sortal_schema.Contact.handle contact in
    let name = Sortal_schema.Contact.name contact in
    `O [
      ("id", `String handle);
      ("title", `String name);
      ("type", `String "contact");
      ("group", `String "entry");
    ]
  ) (Bushel_entry.contacts entries) in

  let domain_map = Hashtbl.create 64 in
  List.iter (fun link ->
    if not (Hashtbl.mem domain_map link.domain) then
      Hashtbl.add domain_map link.domain ()
  ) graph.external_links;

  let domain_nodes = Hashtbl.fold (fun domain () acc ->
    (`O [
      ("id", `String ("domain:" ^ domain));
      ("title", `String domain);
      ("type", `String "domain");
      ("group", `String "domain");
    ]) :: acc
  ) domain_map [] in

  let all_nodes = entry_nodes @ contact_nodes @ domain_nodes in

  let internal_links_json = List.map (fun (link : internal_link) ->
    `O [
      ("source", `String link.source);
      ("target", `String link.target);
      ("type", `String "internal");
    ]
  ) graph.internal_links in

  let external_links_json = List.map (fun (link : external_link) ->
    `O [
      ("source", `String link.source);
      ("target", `String ("domain:" ^ link.domain));
      ("type", `String "external");
    ]
  ) graph.external_links in

  let all_links = internal_links_json @ external_links_json in

  `O [
    ("nodes", `A all_nodes);
    ("links", `A all_links);
  ]
