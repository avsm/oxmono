(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Union entry type for all Bushel content *)

type entry =
  [ `Paper of Bushel_paper.t
  | `Project of Bushel_project.t
  | `Idea of Bushel_idea.t
  | `Video of Bushel_video.t
  | `Note of Bushel_note.t
  ]

type slugs = (string, entry) Hashtbl.t

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
  image_index : (string, Srcsetter.t) Hashtbl.t;
  data_dir : string;
  doi_entries : Bushel_doi_entry.ts;
}

(** {1 Constructors} *)

let v ~papers ~notes ~projects ~ideas ~videos ~contacts ?(images=[]) ?(doi_entries=[]) ~data_dir () =
  let slugs : slugs = Hashtbl.create 42 in
  let papers, old_papers = List.partition (fun p -> p.Bushel_paper.latest) papers in
  List.iter (fun n -> Hashtbl.add slugs n.Bushel_note.slug (`Note n)) notes;
  List.iter (fun p -> Hashtbl.add slugs p.Bushel_project.slug (`Project p)) projects;
  List.iter (fun i -> Hashtbl.add slugs i.Bushel_idea.slug (`Idea i)) ideas;
  List.iter (fun v -> Hashtbl.add slugs v.Bushel_video.slug (`Video v)) videos;
  List.iter (fun p -> Hashtbl.add slugs p.Bushel_paper.slug (`Paper p)) papers;
  (* Build image index *)
  let image_index = Hashtbl.create (List.length images) in
  List.iter (fun img -> Hashtbl.add image_index (Srcsetter.slug img) img) images;
  { slugs; papers; old_papers; notes; projects; ideas; videos; contacts; images; image_index; data_dir; doi_entries }

(** {1 Accessors} *)

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

(** {1 Image Lookup} *)

let lookup_image { image_index; _ } slug =
  Hashtbl.find_opt image_index slug

(** {1 Lookup Functions} *)

let lookup { slugs; _ } slug = Hashtbl.find_opt slugs slug
let lookup_exn { slugs; _ } slug = Hashtbl.find slugs slug

(** {1 Entry Properties} *)

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
  | `Paper _ -> ""
  | `Note n -> Bushel_note.body n
  | `Project p -> Bushel_project.body p
  | `Idea i -> Bushel_idea.body i
  | `Video _ -> ""

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

(** {1 Derived Lookups} *)

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

let all_entries { slugs; _ } =
  Hashtbl.fold (fun _ v acc -> v :: acc) slugs []

let all_papers { papers; old_papers; _ } =
  List.map (fun x -> `Paper x) (papers @ old_papers)

(** {1 Comparison} *)

let compare a b =
  let da = datetime a in
  let db = datetime b in
  if Ptime.equal da db then String.compare (title a) (title b)
  else Ptime.compare da db

(** {1 Contact Lookups} *)

let lookup_by_name { contacts; _ } n =
  let name_lower = String.lowercase_ascii n in
  let matches = List.filter (fun c ->
    List.exists (fun name -> String.lowercase_ascii name = name_lower)
      (Sortal_schema.Contact.names c)
  ) contacts in
  match matches with
  | [contact] -> Some contact
  | _ -> None

(** {1 Tag Functions} *)

let tags_of_ent _entries ent : Bushel_tags.t list =
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

(** {1 Thumbnail Functions} *)

(** Get the smallest webp variant from a srcsetter image - prefers size just above 480px *)
let smallest_webp_variant img =
  let variants = Srcsetter.variants img in
  let webp_variants =
    Srcsetter.MS.bindings variants
    |> List.filter (fun (name, _) -> String.ends_with ~suffix:".webp" name)
  in
  match webp_variants with
  | [] ->
    (* No webp variants - use the name field which is always webp *)
    "/images/" ^ Srcsetter.name img
  | variants ->
    (* Prefer variants with width > 480px, choosing the smallest one above 480 *)
    let large_variants = List.filter (fun (_, (w, _)) -> w > 480) variants in
    let candidates = if large_variants = [] then variants else large_variants in
    (* Find the smallest variant from candidates *)
    let smallest = List.fold_left (fun acc (name, (w, h)) ->
      match acc with
      | None -> Some (name, w, h)
      | Some (_, min_w, _) when w < min_w -> Some (name, w, h)
      | _ -> acc
    ) None candidates in
    match smallest with
    | Some (name, _, _) -> "/images/" ^ name
    | None -> "/images/" ^ Srcsetter.name img

(** Get thumbnail slug for a contact *)
let contact_thumbnail_slug contact =
  (* Contact images use just the handle as slug *)
  Some (Sortal_schema.Contact.handle contact)

(** Get thumbnail URL for a contact - resolved through srcsetter *)
let contact_thumbnail entries contact =
  match contact_thumbnail_slug contact with
  | None -> None
  | Some thumb_slug ->
    match lookup_image entries thumb_slug with
    | Some img -> Some (smallest_webp_variant img)
    | None -> None

(** Extract the first image URL from markdown text *)
let extract_first_image md =
  let open Cmarkit in
  let doc = Doc.of_string md in
  let found_image = ref None in
  let find_image_in_inline _mapper = function
    | Inline.Image (img, _) ->
      (match Inline.Link.reference img with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (url, _) when !found_image = None ->
            found_image := Some url;
            Mapper.default
          | _ -> Mapper.default)
       | _ -> Mapper.default)
    | _ -> Mapper.default
  in
  let mapper = Mapper.make ~inline:find_image_in_inline () in
  let _ = Mapper.map_doc mapper doc in
  !found_image

(** Extract the first video slug from markdown text by looking for bushel video links *)
let extract_first_video entries md =
  let open Cmarkit in
  let doc = Doc.of_string md in
  let found_video = ref None in
  let find_video_in_inline _mapper = function
    | Inline.Link (link, _) ->
      (match Inline.Link.reference link with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (url, _) when !found_video = None && String.starts_with ~prefix:":" url ->
            let slug = String.sub url 1 (String.length url - 1) in
            (match lookup entries slug with
             | Some (`Video v) ->
               found_video := Some (Bushel_video.uuid v);
               Mapper.default
             | _ -> Mapper.default)
          | _ -> Mapper.default)
       | _ -> Mapper.default)
    | _ -> Mapper.default
  in
  let mapper = Mapper.make ~inline:find_video_in_inline () in
  let _ = Mapper.map_doc mapper doc in
  !found_video

(** Get thumbnail slug for an entry with fallbacks *)
let rec thumbnail_slug entries = function
  | `Paper p -> Some (Bushel_paper.slug p)
  | `Video v -> Some (Bushel_video.uuid v)
  | `Project p -> Some (Printf.sprintf "project-%s" (Bushel_project.slug p))
  | `Idea i ->
    let is_active = match Bushel_idea.status i with
      | Bushel_idea.Available | Bushel_idea.Discussion | Bushel_idea.Ongoing -> true
      | Bushel_idea.Completed | Bushel_idea.Expired -> false
    in
    if is_active then
      (* Use first supervisor's face image *)
      let supervisors = Bushel_idea.supervisors i in
      match supervisors with
      | sup :: _ ->
        let handle = if String.length sup > 0 && sup.[0] = '@'
          then String.sub sup 1 (String.length sup - 1)
          else sup
        in
        (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = handle) (contacts entries) with
         | Some c ->
           Some (Sortal_schema.Contact.handle c)
         | None ->
           (* Fallback to project thumbnail *)
           let project_slug = Bushel_idea.project i in
           (match lookup entries project_slug with
            | Some p -> thumbnail_slug entries p
            | None -> None))
      | [] ->
        (* No supervisors, use project thumbnail *)
        let project_slug = Bushel_idea.project i in
        (match lookup entries project_slug with
         | Some p -> thumbnail_slug entries p
         | None -> None)
    else
      (* Use project thumbnail for completed/expired ideas *)
      let project_slug = Bushel_idea.project i in
      (match lookup entries project_slug with
       | Some p -> thumbnail_slug entries p
       | None -> None)
  | `Note n ->
    (* Use titleimage if set, otherwise extract first image from body,
       then try video, otherwise use slug_ent's thumbnail *)
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
           (* Fallback to slug_ent's thumbnail if present *)
           match Bushel_note.slug_ent n with
           | Some slug_ent ->
             (match lookup entries slug_ent with
              | Some entry -> thumbnail_slug entries entry
              | None -> None)
           | None -> None)

(** Get thumbnail URL for an entry with fallbacks - resolved through srcsetter *)
let thumbnail entries entry =
  match thumbnail_slug entries entry with
  | None -> None
  | Some thumb_slug ->
    match lookup_image entries thumb_slug with
    | Some img -> Some (smallest_webp_variant img)
    | None ->
      (* For projects, fallback to supervisor faces if project image doesn't exist *)
      (match entry with
       | `Project p ->
         (* Find ideas for this project *)
         let project_ideas = List.filter (fun idea ->
           Bushel_idea.project idea = ":" ^ Bushel_project.slug p
         ) (ideas entries) in
         (* Collect all unique supervisors from these ideas *)
         let all_supervisors =
           List.fold_left (fun acc idea ->
             List.fold_left (fun acc2 sup ->
               if List.mem sup acc2 then acc2 else sup :: acc2
             ) acc (Bushel_idea.supervisors idea)
           ) [] project_ideas
         in
         (* Split into avsm and others, preferring others first *)
         let (others, avsm) = List.partition (fun sup ->
           let handle = if String.length sup > 0 && sup.[0] = '@'
             then String.sub sup 1 (String.length sup - 1)
             else sup
           in
           handle <> "avsm"
         ) all_supervisors in
         let ordered_supervisors = others @ avsm in
         let rec try_supervisors = function
           | [] -> None
           | sup :: rest ->
             let handle = if String.length sup > 0 && sup.[0] = '@'
               then String.sub sup 1 (String.length sup - 1)
               else sup
             in
             (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = handle) (contacts entries) with
              | Some c ->
                (match lookup_image entries (Sortal_schema.Contact.handle c) with
                 | Some img -> Some (smallest_webp_variant img)
                 | None -> try_supervisors rest)
              | None -> try_supervisors rest)
         in
         try_supervisors ordered_supervisors
       | _ -> None)
