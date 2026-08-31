(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

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
  internal_links : internal_link list;
  external_links : external_link list;
  outbound : string list Bushel_smap.t;
  backlinks : string list Bushel_smap.t;
  external_by_entry : string list Bushel_smap.t;
}

let empty =
  {
    internal_links = [];
    external_links = [];
    outbound = Bushel_smap.empty;
    backlinks = Bushel_smap.empty;
    external_by_entry = Bushel_smap.empty;
  }

let group pairs =
  let m =
    List.fold_left
      (fun m (k, v) ->
        let cur =
          match StringMap.find_opt k m with
          | Some s -> s
          | None -> StringSet.empty
        in
        StringMap.add k (StringSet.add v cur) m)
      StringMap.empty pairs
  in
  Bushel_smap.of_list
    (List.map (fun (k, s) -> (k, StringSet.elements s)) (StringMap.bindings m))

let v ~internal_links ~external_links =
  {
    internal_links;
    external_links;
    outbound =
      group (List.map (fun (l : internal_link) -> (l.source, l.target)) internal_links);
    backlinks =
      group (List.map (fun (l : internal_link) -> (l.target, l.source)) internal_links);
    external_by_entry =
      group (List.map (fun (l : external_link) -> (l.source, l.url)) external_links);
  }

let find tbl slug =
  match Bushel_smap.find_opt slug tbl with Some l -> l | None -> []

let backlinks g slug = find g.backlinks slug
let outbound g slug = find g.outbound slug
let external_urls g slug = find g.external_by_entry slug
let all_external_links g = g.external_links

let entry_type_of_entry = function
  | `Paper _ -> `Paper
  | `Project _ -> `Project
  | `Note _ -> `Note
  | `Idea _ -> `Idea
  | `Video _ -> `Video

let entry_type_to_string = function
  | `Paper -> "paper"
  | `Project -> "project"
  | `Note -> "note"
  | `Idea -> "idea"
  | `Video -> "video"
  | `Contact -> "contact"

let pp ppf g =
  Fmt.pf ppf
    "@[<v>Internal links: %d@,External links: %d@,Entries with outbound: %d@,Entries with backlinks: %d@]"
    (List.length g.internal_links)
    (List.length g.external_links)
    (List.length (Bushel_smap.bindings g.outbound))
    (List.length (Bushel_smap.bindings g.backlinks))
