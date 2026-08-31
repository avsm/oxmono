(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type entry =
  [ `Paper of Bushel_paper.t
  | `Project of Bushel_project.t
  | `Idea of Bushel_idea.t
  | `Video of Bushel_video.t
  | `Note of Bushel_note.t
  ]

type slugs = entry Bushel_smap.t

type t = {
  slugs : slugs;
  papers : Bushel_paper.ts;
  old_papers : Bushel_paper.ts;
  notes : Bushel_note.ts;
  projects : Bushel_project.ts;
  ideas : Bushel_idea.ts;
  videos : Bushel_video.ts;
  contacts : Sortal_schema.Contact.t list;
  images : Srcsetter.t list;
  image_index : Srcsetter.t Bushel_smap.t;
  data_dir : string;
  doi_entries : Bushel_doi_entry.ts;
  graph : Bushel_link_graph.t;
}

let slugged_entries ~notes ~projects ~ideas ~videos ~papers =
  List.map (fun n -> (n.Bushel_note.slug, `Note n)) notes
  @ List.map (fun p -> (p.Bushel_project.slug, `Project p)) projects
  @ List.map (fun i -> (i.Bushel_idea.slug, `Idea i)) ideas
  @ List.map (fun v -> (v.Bushel_video.slug, `Video v)) videos
  @ List.map (fun p -> (p.Bushel_paper.slug, `Paper p)) papers

let v ~papers ~notes ~projects ~ideas ~videos ~contacts ?(images=[]) ?(doi_entries=[]) ~data_dir () =
  let papers, old_papers = List.partition (fun p -> p.Bushel_paper.latest) papers in
  let slugs : slugs =
    Bushel_smap.of_list (slugged_entries ~notes ~projects ~ideas ~videos ~papers)
  in
  let image_index =
    Bushel_smap.of_list (List.map (fun img -> (Srcsetter.slug img, img)) images)
  in
  { slugs; papers; old_papers; notes; projects; ideas; videos; contacts; images; image_index; data_dir; doi_entries;
    graph = Bushel_link_graph.empty }

let with_graph t graph = { t with graph }

let contacts { contacts; _ } = contacts
let videos { videos; _ } = videos
let ideas { ideas; _ } = ideas
let papers { papers; _ } = papers
let notes { notes; _ } = notes
let projects { projects; _ } = projects
let old_papers { old_papers; _ } = old_papers
let images { images; _ } = images
let data_dir { data_dir; _ } = data_dir
let doi_entries { doi_entries; _ } = doi_entries
let graph { graph; _ } = graph
let backlinks { graph; _ } slug = Bushel_link_graph.backlinks graph slug
let outbound { graph; _ } slug = Bushel_link_graph.outbound graph slug
let external_urls { graph; _ } slug = Bushel_link_graph.external_urls graph slug
let all_external_links { graph; _ } = Bushel_link_graph.all_external_links graph

let lookup_image { image_index; _ } slug =
  Bushel_smap.find_opt slug image_index

let lookup { slugs; _ } slug = Bushel_smap.find_opt slug slugs
let lookup_exn { slugs; _ } slug = Bushel_smap.find slug slugs

let to_type_string = function
  | `Paper _ -> "paper"
  | `Note _ -> "note"
  | `Project _ -> "project"
  | `Idea _ -> "idea"
  | `Video _ -> "video"

let slug = function
  | `Paper p -> Bushel_paper.slug p
  | `Note n -> Bushel_note.slug n
  | `Project p -> Bushel_project.slug p
  | `Idea i -> Bushel_idea.slug i
  | `Video v -> Bushel_video.slug v

let title = function
  | `Paper p -> Bushel_paper.title p
  | `Note n -> Bushel_note.title n
  | `Project p -> Bushel_project.title p
  | `Idea i -> Bushel_idea.title i
  | `Video v -> Bushel_video.title v

let body = function
  | `Paper p -> Bushel_paper.abstract p
  | `Note n -> Bushel_note.body n
  | `Project p -> Bushel_project.body p
  | `Idea i -> Bushel_idea.body i
  | `Video v -> Bushel_video.description v

let social = function
  | `Note n -> Bushel_note.social n
  | `Paper p -> Bushel_paper.social p
  | `Idea i -> Bushel_idea.social i
  | `Video v -> Bushel_video.social v
  | `Project p -> Bushel_project.social p

let sidebar = function
  | `Note { Bushel_note.sidebar = Some s; _ } -> Some s
  | _ -> None

let synopsis = function
  | `Note n -> Bushel_note.synopsis n
  | _ -> None

let site_url = function
  | `Paper p -> "/papers/" ^ Bushel_paper.slug p
  | `Note n -> "/notes/" ^ Bushel_note.slug n
  | `Project p -> "/projects/" ^ Bushel_project.slug p
  | `Idea i -> "/ideas/" ^ Bushel_idea.slug i
  | `Video v -> "/videos/" ^ Bushel_video.slug v

let date (x : entry) =
  match x with
  | `Paper p -> Bushel_paper.date p
  | `Note n -> Bushel_note.date n
  | `Project p -> (Bushel_project.start p, 1, 1)
  | `Idea i -> (Bushel_idea.year i, Bushel_idea.month i, 1)
  | `Video v -> Bushel_video.date v

let datetime v = Bushel_types.ptime_of_date_exn (date v)

let year x =
  let (y, _, _) = date x in y

let is_index_entry = function
  | `Note n -> n.Bushel_note.index_page
  | _ -> false

let lookup_site_url t slug =
  match lookup t slug with
  | Some ent -> site_url ent
  | None -> ""

let lookup_title t slug =
  match lookup t slug with
  | Some ent -> title ent
  | None -> ""

let notes_for_slug { notes; _ } slug =
  List.filter (fun n ->
    match Bushel_note.slug_ent n with
    | Some s -> s = slug
    | None -> false
  ) notes

let all_entries { notes; projects; ideas; videos; papers; _ } =
  List.map snd (slugged_entries ~notes ~projects ~ideas ~videos ~papers)

let all_papers { papers; old_papers; _ } =
  List.map (fun x -> `Paper x) (papers @ old_papers)

let compare a b =
  let da = datetime a in
  let db = datetime b in
  if Ptime.equal da db then String.compare (title a) (title b)
  else Ptime.compare da db

let lookup_by_name { contacts; _ } n =
  let name_lower = String.lowercase_ascii n in
  let matches = List.filter (fun c ->
    List.exists (fun name -> String.lowercase_ascii name = name_lower)
      (Sortal_schema.Contact.names c)
  ) contacts in
  match matches with
  | [contact] -> Some contact
  | _ -> None

let tags_of_ent ent : Bushel_tags.t list =
  match ent with
  | `Paper p -> Bushel_tags.of_string_list @@ Bushel_paper.tags p
  | `Video v -> Bushel_tags.of_string_list @@ Bushel_video.tags v
  | `Project p -> Bushel_tags.of_string_list @@ Bushel_project.tags p
  | `Note n -> Bushel_tags.of_string_list @@ Bushel_note.tags n
  | `Idea i -> Bushel_tags.of_string_list @@ Bushel_idea.tags i

let mention_entries entries tags =
  let lk t =
    try Some (lookup_exn entries t)
    with Not_found ->
      Printf.eprintf "mention_entries not found: %s\n%!" t;
      None
  in
  List.filter_map (function
    | `Slug t -> lk t
    | _ -> None
  ) tags

let smallest_webp_variant img =
  let variants = Srcsetter.variants img in
  let webp_variants =
    Srcsetter.MS.bindings variants
    |> List.filter (fun (name, _) -> String.ends_with ~suffix:".webp" name)
  in
  match webp_variants with
  | [] -> "/images/" ^ Srcsetter.name img
  | variants ->
    let large_variants = List.filter (fun (_, (w, _)) -> w > 480) variants in
    let candidates = if large_variants = [] then variants else large_variants in
    let name, _ =
      match candidates with
      | [] -> assert false
      | first :: rest ->
        List.fold_left
          (fun ((_, (best, _)) as current) ((_, (width, _)) as candidate) ->
            if width < best then candidate else current)
          first rest
    in
    "/images/" ^ name

let thumbnail_for_slug entries slug =
  Option.map smallest_webp_variant (lookup_image entries slug)

let contact_thumbnail entries contact =
  thumbnail_for_slug entries (Sortal_schema.Contact.handle contact)

let inline_destination link =
  let open Cmarkit in
  match Inline.Link.reference link with
  | `Inline (definition, _) -> Option.map fst (Link_definition.dest definition)
  | `Ref _ -> None

let first_inline_value select md =
  let open Cmarkit in
  let doc = Doc.of_string md in
  let found = ref None in
  let visit _ inline =
    if Option.is_none !found then found := select inline;
    `Default
  in
  ignore (Mapper.map_doc (Mapper.make ~inline:visit ()) doc);
  !found

let extract_first_image md =
  let open Cmarkit in
  first_inline_value
    (function Inline.Image (image, _) -> inline_destination image | _ -> None)
    md

let extract_first_video entries md =
  let open Cmarkit in
  first_inline_value
    (function
      | Inline.Link (link, _) -> (
        match inline_destination link with
        | Some url when String.starts_with ~prefix:":" url -> (
          let slug = String.sub url 1 (String.length url - 1) in
          match lookup entries slug with
          | Some (`Video video) -> Some (Bushel_video.uuid video)
          | _ -> None)
        | _ -> None)
      | _ -> None)
    md

let rec thumbnail_slug entries = function
  | `Paper p -> Some (Bushel_paper.slug p)
  | `Video v -> Some (Bushel_video.uuid v)
  | `Project p -> Some (Printf.sprintf "project-%s" (Bushel_project.slug p))
  | `Idea i ->
    (match extract_first_image (Bushel_idea.body i) with
     | Some url when String.starts_with ~prefix:":" url ->
       Some (String.sub url 1 (String.length url - 1))
     | _ ->
       let project_slug = Bushel_idea.project i in
       match lookup entries project_slug with
       | Some p -> thumbnail_slug entries p
       | None ->
         match Bushel_idea.supervisors i with
         | c :: _ -> Some (Sortal_schema.Contact.handle c)
         | [] -> None)
  | `Note n ->
    (match Bushel_note.titleimage n with
     | Some slug -> Some slug
     | None ->
       match extract_first_image (Bushel_note.body n) with
       | Some url when String.starts_with ~prefix:":" url ->
         Some (String.sub url 1 (String.length url - 1))
       | Some _ -> None
       | None ->
         match extract_first_video entries (Bushel_note.body n) with
         | Some video_uuid -> Some video_uuid
         | None ->
           match Bushel_note.slug_ent n with
           | Some slug_ent ->
             (match lookup entries slug_ent with
              | Some entry -> thumbnail_slug entries entry
              | None -> None)
           | None -> None)

let thumbnail entries entry =
  match thumbnail_slug entries entry with
  | None -> None
  | Some thumb_slug -> (
    match thumbnail_for_slug entries thumb_slug with
    | Some _ as thumbnail -> thumbnail
    | None ->
      (match entry with
       | `Project p ->
         let project_ideas = List.filter (fun idea ->
           Bushel_idea.project idea = ":" ^ Bushel_project.slug p
         ) (ideas entries) in
         let all_supervisors =
           List.fold_left (fun acc idea ->
             List.fold_left (fun acc2 c ->
               if List.exists (fun c2 ->
                 Sortal_schema.Contact.handle c2 = Sortal_schema.Contact.handle c
               ) acc2 then acc2 else c :: acc2
             ) acc (Bushel_idea.supervisors idea)
           ) [] project_ideas
         in
         let (others, avsm) = List.partition (fun c ->
           Sortal_schema.Contact.handle c <> "avsm"
         ) all_supervisors in
         List.find_map
           (fun c ->
             thumbnail_for_slug entries (Sortal_schema.Contact.handle c))
           (others @ avsm)
       | _ -> None))
