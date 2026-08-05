(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Links page component.

    Shows all outbound external links ordered by date (newest first),
    with enriched display: GitHub shortnames, arxiv IDs, contact
    associations, karakeep titles, and favicons. Paginated via
    infinite scrolling. *)

open Htmlit

module Entry = Bushel.Entry
module Contact = Sortal_schema.Contact
module I = Arod.Icons

(** {1 Helpers} *)

(** Format a URL as "domain /path" with truncated path. *)
let domain_and_path url =
  let u = Uri.of_string url in
  let domain = Option.value ~default:"" (Uri.host u) in
  let path = match Uri.path u with "" | "/" -> "" | p -> p in
  let path =
    if String.length path > 50 then String.sub path 0 50 ^ "\xe2\x80\xa6"
    else path
  in
  (domain, path)

(** {1 URL Classification} *)

type link_kind =
  | Github | Code | Arxiv | Doi | Rfc | Contact | Paper | Web | Untitled

let string_of_kind = function
  | Github -> "github" | Code -> "code" | Arxiv -> "arxiv"
  | Doi -> "doi" | Rfc -> "rfc" | Contact -> "contact"
  | Paper -> "paper" | Web -> "web" | Untitled -> "untitled"

type link_display = {
  label : string;
  secondary : string option;
  kind : link_kind;
  favicon : string option;
  contact : Contact.t option;
}

(** Extract path segments from a URL. *)
let path_segments url =
  let u = Uri.of_string url in
  match Uri.path u with
  | "" | "/" -> []
  | path ->
    String.split_on_char '/' path
    |> List.filter (fun s -> s <> "")

(** Try to extract an RFC number from a URL path. *)
let extract_rfc_number url =
  let segs = path_segments url in
  let rec find = function
    | [] -> None
    | seg :: rest ->
      let s = String.lowercase_ascii seg in
      if String.length s > 3 && String.sub s 0 3 = "rfc" then
        let num = String.sub s 3 (String.length s - 3) in
        if String.length num > 0 && num.[0] >= '0' && num.[0] <= '9' then
          Some num
        else find rest
      else find rest
  in
  find segs

(** Shared platforms that should not be contact-matched. *)
let shared_platforms = [
  "github.com"; "gitlab.com"; "codeberg.org"; "tangled.org"; "bitbucket.org";
  "twitter.com"; "x.com"; "bsky.app"; "bsky.social";
  "mastodon.social"; "mastodon.online"; "hachyderm.io";
  "linkedin.com"; "youtube.com"; "youtu.be";
  "arxiv.org"; "doi.org"; "orcid.org";
  "reddit.com"; "news.ycombinator.com";
  "threads.com";
  "medium.com"; "substack.com"; "wordpress.com";
  "en.wikipedia.org"; "wikipedia.org";
  "scholar.google.com"; "google.com";
  "stackoverflow.com"; "stackexchange.com";
  "researchgate.net"; "academia.edu";
  "amazon.com"; "goodreads.com";
]

let strip_www = Common.strip_www

(** Build a domain-to-contact hashtable with path prefixes.
    Each domain maps to a list of (path_prefix, contact) pairs so that
    e.g. cl.cam.ac.uk/~sv440/ does not match cl.cam.ac.uk/~avsm2/. *)
let build_contact_by_domain contacts =
  let tbl : (string, (string * Contact.t) list) Hashtbl.t = Hashtbl.create 64 in
  let is_shared h = List.mem h shared_platforms in
  let add_url c url_str =
    let uri = Uri.of_string url_str in
    match Uri.host uri with
    | Some h ->
      let bare = strip_www (String.lowercase_ascii h) in
      if not (is_shared bare) then begin
        let path = match Uri.path uri with "" | "/" -> "/" | p -> p in
        let cur = try Hashtbl.find tbl bare with Not_found -> [] in
        if not (List.exists (fun (p, _) -> p = path) cur) then
          Hashtbl.replace tbl bare ((path, c) :: cur)
      end
    | None -> ()
  in
  List.iter (fun c ->
    List.iter (fun (u : Contact.url_entry) -> add_url c u.url) (Contact.urls c);
    List.iter (fun (s : Contact.service) -> add_url c s.url) (Contact.current_services c)
  ) contacts;
  tbl

(** Find a contact for a URL by matching domain and longest path prefix. *)
let find_contact_for_url contact_by_domain bare_host url_path =
  match Hashtbl.find_opt contact_by_domain bare_host with
  | None -> None
  | Some entries ->
    (* Find the entry with the longest path prefix that matches *)
    let matches = List.filter (fun (prefix, _) ->
      prefix = "/" || String.length url_path >= String.length prefix
        && String.sub url_path 0 (String.length prefix) = prefix
    ) entries in
    match matches with
    | [] -> None
    | _ ->
      (* Pick the longest prefix match; "/" is weakest *)
      let best = List.fold_left (fun acc (p, c) ->
        match acc with
        | None -> Some (p, c)
        | Some (bp, _) -> if String.length p > String.length bp then Some (p, c) else acc
      ) None matches in
      Option.map snd best

(** Classify a URL into a structured display. *)
let classify_url ~contact_by_domain ~doi_entries ~ctx url =
  let u = Uri.of_string url in
  let host = match Uri.host u with Some h -> h | None -> "" in
  let host_lc = String.lowercase_ascii host in
  let bare_host = strip_www host_lc in
  (* Get favicon from links.yml if available *)
  let favicon = match Arod.Ctx.link_for_url ctx url with
    | Some l ->
      let meta = match l.karakeep with Some k -> k.metadata | None -> [] in
      (match List.assoc_opt "favicon" meta with
       | Some f when f <> "" -> Some f
       | _ -> None)
    | None -> None
  in
  (* Get title from links.yml *)
  let karakeep_title = match Arod.Ctx.link_for_url ctx url with
    | Some l ->
      let meta = match l.karakeep with Some k -> k.metadata | None -> [] in
      (match List.assoc_opt "title" meta with
       | Some t when t <> "" -> Some t
       | _ -> None)
    | None -> None
  in
  let segs = path_segments url in
  let mk ?secondary kind label =
    { label; secondary; kind; favicon; contact = None }
  in
  (* 1. Contact match — requires path prefix match *)
  let url_path = match Uri.path u with "" -> "/" | p -> p in
  match find_contact_for_url contact_by_domain bare_host url_path with
  | Some contact ->
    let name = Contact.name contact in
    let label = match karakeep_title with
      | Some t -> t
      | None -> name
    in
    { label; secondary = None; kind = Contact; favicon;
      contact = Some contact }
  | None ->
  (* 2. GitHub — intelligent URL breakdown *)
  if bare_host = "github.com" then begin
    match segs with
    | user :: repo :: "issues" :: num :: _ ->
      mk ~secondary:(Printf.sprintf "%s/%s" user repo)
        Github (Printf.sprintf "Issue #%s" num)
    | user :: repo :: "pull" :: num :: _ ->
      mk ~secondary:(Printf.sprintf "%s/%s" user repo)
        Github (Printf.sprintf "PR #%s" num)
    | user :: repo :: "releases" :: "tag" :: tag :: _ ->
      mk ~secondary:(Printf.sprintf "%s/%s" user repo)
        Github tag
    | user :: repo :: "tree" :: branch :: _ ->
      mk ~secondary:(Printf.sprintf "%s/%s" user repo)
        Github branch
    | user :: repo :: "blob" :: _branch :: rest ->
      let file = match rest with
        | [] -> ""
        | parts -> List.nth parts (List.length parts - 1)
      in
      mk ~secondary:(Printf.sprintf "%s/%s" user repo)
        Github (if file <> "" then file else repo)
    | user :: repo :: "wiki" :: page :: _ ->
      mk ~secondary:(Printf.sprintf "%s/%s" user repo)
        Github (Printf.sprintf "Wiki: %s" page)
    | user :: repo :: "actions" :: _ ->
      mk ~secondary:(Printf.sprintf "%s/%s" user repo)
        Github "Actions"
    | user :: repo :: "commit" :: sha :: _ ->
      let short_sha = if String.length sha > 7 then String.sub sha 0 7 else sha in
      mk ~secondary:(Printf.sprintf "%s/%s" user repo)
        Github short_sha
    | user :: repo :: _ ->
      mk Github (Printf.sprintf "%s/%s" user repo)
    | [user] -> mk Github user
    | [] -> mk Github "github.com"
  end
  (* 2b. Other code hosts — gitlab, codeberg, tangled *)
  else if bare_host = "gitlab.com" || bare_host = "codeberg.org"
          || bare_host = "tangled.org" then begin
    let label = match segs with
      | user :: repo :: _ -> Printf.sprintf "%s/%s" user repo
      | [user] -> user
      | [] -> bare_host
    in
    let label = match karakeep_title with
      | Some t -> t
      | None -> label
    in
    mk Code label
  end
  (* 3. ArXiv — title from doi.yml or karakeep, fallback to arXiv ID *)
  else if bare_host = "arxiv.org" then begin
    let arxiv_id = match segs with
      | ("abs" | "pdf") :: id :: _ -> Some id
      | _ -> None
    in
    let doi_title = match Bushel.Doi_entry.find_by_url doi_entries url with
      | Some e when e.status = Resolved && e.title <> "" -> Some e.title
      | _ -> None
    in
    match doi_title with
    | Some t -> mk Arxiv t
    | None ->
      match karakeep_title with
      | Some t -> mk Arxiv t
      | None ->
        let label = match arxiv_id with
          | Some id -> "arXiv:" ^ id
          | None -> "arxiv.org"
        in
        mk Arxiv label
  end
  (* 4. DOI — look up title from doi.yml first, then karakeep *)
  else if bare_host = "doi.org" then begin
    let path = Uri.path u in
    let doi_id =
      if String.length path > 1 then String.sub path 1 (String.length path - 1)
      else ""
    in
    let doi_title =
      if doi_id <> "" then
        match Bushel.Doi_entry.find_by_doi doi_entries doi_id with
        | Some e when e.status = Resolved && e.title <> "" -> Some e.title
        | _ -> None
      else None
    in
    match doi_title with
    | Some t -> mk Doi t
    | None ->
      match karakeep_title with
      | Some t -> mk Doi t
      | None ->
        let label = if doi_id <> "" then "doi:" ^ doi_id else "doi.org" in
        mk Doi label
  end
  (* 5. IETF / RFC *)
  else if bare_host = "datatracker.ietf.org" || bare_host = "rfc-editor.org"
          || bare_host = "www.rfc-editor.org" then begin
    let label = match extract_rfc_number url with
      | Some n -> "RFC " ^ n
      | None -> host
    in
    mk ?secondary:karakeep_title Rfc label
  end
  (* 6. Paper URL — look up title from doi.yml, then karakeep *)
  else if Bushel.Link.is_paper_url url then begin
    let doi_title = match Bushel.Doi_entry.find_by_url doi_entries url with
      | Some e when e.status = Resolved && e.title <> "" -> Some e.title
      | _ -> None
    in
    match doi_title with
    | Some t -> mk Paper t
    | None ->
      match karakeep_title with
      | Some t -> mk Paper t
      | None ->
        let (domain, path) = domain_and_path url in
        let label = if path = "" then domain else domain ^ " " ^ path in
        mk Paper label
  end
  (* 7. Title from links.yml *)
  else match karakeep_title with
  | Some title ->
    mk Web title
  (* 8. Fallback — untitled *)
  | None ->
    let (domain, path) = domain_and_path url in
    let label = if path = "" then domain else domain ^ " " ^ path in
    mk Untitled label

(** Code hosting platforms for filter classification. *)
let code_hosts = [
  "github.com"; "gitlab.com"; "codeberg.org"; "tangled.org"
]

(** Filter categories for the sidebar infobox. *)
type filter_kind = Fp_paper | Fp_contact | Fp_code | Fp_titled | Fp_untitled

let string_of_filter_kind = function
  | Fp_paper -> "paper" | Fp_contact -> "contact" | Fp_code -> "code"
  | Fp_titled -> "titled" | Fp_untitled -> "untitled"

(** Map fine-grained display kind to filter category. *)
let filter_of_kind = function
  | Arxiv | Doi | Paper -> Fp_paper
  | Contact -> Fp_contact
  | Github | Code -> Fp_code
  | Rfc | Web -> Fp_titled
  | Untitled -> Fp_untitled

(** {1 Kind Badge} *)

let kind_badge ~entries display =
  match display.kind with
  | Github ->
    El.span ~at:[At.class' "link-kind-badge link-kind-github";
                 At.v "title" "GitHub"]
      [El.unsafe_raw (I.brand ~cl:"" ~size:12 I.github_brand)]
  | Code ->
    El.span ~at:[At.class' "link-kind-badge link-kind-github";
                 At.v "title" "Code"]
      [El.unsafe_raw (I.outline ~cl:"" ~size:12 I.code_o)]
  | Arxiv ->
    El.span ~at:[At.class' "link-kind-badge link-kind-arxiv";
                 At.v "title" "arXiv"]
      [El.unsafe_raw (I.brand ~cl:"" ~size:12 I.arxiv_brand)]
  | Doi ->
    El.span ~at:[At.class' "link-kind-badge link-kind-doi";
                 At.v "title" "DOI"]
      [El.unsafe_raw (I.brand ~cl:"" ~size:12 I.doi_brand)]
  | Rfc ->
    El.span ~at:[At.class' "link-kind-badge link-kind-rfc";
                 At.v "title" "RFC"]
      [El.unsafe_raw (I.outline ~cl:"" ~size:12 I.file_certificate_o)]
  | Contact ->
    let thumb = match display.contact with
      | Some c -> Entry.contact_thumbnail entries c
      | None -> None
    in
    (match thumb with
     | Some src ->
       El.img ~at:[At.src src;
                   At.class' "link-contact-thumb";
                   At.v "width" "14"; At.v "height" "14";
                   At.v "loading" "lazy";
                   At.v "alt" ""] ()
     | None ->
       match display.favicon with
       | Some favicon_url ->
         El.img ~at:[At.src favicon_url;
                     At.class' "link-favicon";
                     At.v "width" "14"; At.v "height" "14";
                     At.v "loading" "lazy";
                     At.v "alt" ""] ()
       | None ->
         El.span ~at:[At.class' "link-kind-badge link-kind-contact";
                      At.v "title" "Contact"]
           [El.unsafe_raw (I.outline ~cl:"" ~size:12 I.user_o)])
  | Paper | Web | Untitled ->
    match display.favicon with
    | Some favicon_url ->
      El.img ~at:[At.src favicon_url;
                  At.class' "link-favicon";
                  At.v "width" "14"; At.v "height" "14";
                  At.v "loading" "lazy";
                  At.v "alt" ""] ()
    | None -> El.span ~at:[At.class' "link-kind-badge"] [El.txt "\xc2\xb7"]

(** {1 Group Computation} *)

type link_group = {
  ent : Entry.entry;
  links : Bushel.Link_graph.external_link list;
}

(** Compute all link groups sorted by entry date descending. *)
let compute_groups ~ctx =
  let entries = Arod.Ctx.entries ctx in
  let all_links = Bushel.Link_graph.all_external_links () in
  let by_source : (string, Bushel.Link_graph.external_link list) Hashtbl.t =
    Hashtbl.create 128 in
  List.iter (fun (link : Bushel.Link_graph.external_link) ->
    let cur = try Hashtbl.find by_source link.source with Not_found -> [] in
    if List.exists (fun (l : Bushel.Link_graph.external_link) -> l.url = link.url) cur then ()
    else Hashtbl.replace by_source link.source (link :: cur)
  ) all_links;
  let groups = Hashtbl.fold (fun slug links acc ->
    match Entry.lookup entries slug with
    | Some ent -> { ent; links } :: acc
    | None -> acc
  ) by_source [] in
  List.sort (fun a b ->
    compare (Entry.date b.ent) (Entry.date a.ent)
  ) groups

(** {1 Group Rendering} *)

(** Render a single link group with a data-month-id for scroll tracking. *)
let render_group ~contact_by_domain ~doi_entries ~entries ~ctx group =
  let (y, m, d) = Entry.date group.ent in
  let date_str = Printf.sprintf "%s %d" (Common.month_name m) y in
  let month_id = Printf.sprintf "%04d-%02d" y m in
  let day_str = string_of_int d in
  let type_icon = Sidebar.entry_type_icon ~size:12 group.ent in
  let header =
    El.div ~at:[At.class' "link-group-header"] [
      El.span ~at:[At.class' "hidden md:inline-flex shrink-0 opacity-50"]
        [El.unsafe_raw type_icon];
      El.a ~at:[At.href (Entry.site_url group.ent);
                At.class' "link-group-title no-underline"]
        [El.txt (Entry.title group.ent)];
      El.span ~at:[At.class' "note-compact-meta"] [El.txt date_str]]
  in
  let link_rows = List.map (fun (link : Bushel.Link_graph.external_link) ->
    let display = classify_url ~contact_by_domain ~doi_entries ~ctx link.url in
    let badge = kind_badge ~entries display in
    let label_children =
      let primary = El.txt display.label in
      match display.secondary with
      | Some sec ->
        [primary;
         El.span ~at:[At.class' "link-label-secondary"]
           [El.txt (" " ^ sec)]]
      | None -> [primary]
    in
    let label_el =
        El.a ~at:[At.href link.url;
                  At.class' "link-label no-underline";
                  At.v "rel" "noopener"]
          label_children
    in
    let show_hint = (display.kind <> Web && display.kind <> Untitled) || display.favicon <> None in
    let domain_hint =
      if show_hint then
        El.span ~at:[At.class' "link-domain-hint"]
          [El.txt link.domain]
      else El.void
    in
    El.div ~at:[At.class' "link-row pl-0 md:pl-5";
                At.v "data-link-kind" (string_of_kind display.kind);
                At.v "data-link-filter" (string_of_filter_kind (filter_of_kind display.kind))]
      [badge; label_el; domain_hint]
  ) group.links in
  El.div ~at:[At.class' "link-group";
              At.v "data-month-id" month_id;
              At.v "data-day" day_str]
    (header :: link_rows)

(** Render a slice of link groups as an HTML string for the pagination API. *)
let render_groups_html ~ctx groups =
  let contacts = Arod.Ctx.contacts ctx in
  let entries = Arod.Ctx.entries ctx in
  let contact_by_domain = build_contact_by_domain contacts in
  let doi_entries = Entry.doi_entries entries in
  let els = List.map (render_group ~contact_by_domain ~doi_entries ~entries ~ctx) groups in
  El.to_string ~doctype:false (El.div els)

(** Return all computed groups for use by the pagination API. *)
let all_groups ~ctx = compute_groups ~ctx

(** {1 Links List Page} *)

let page_size = 25

let links_list ~ctx =
  let groups = compute_groups ~ctx in
  let entries = Arod.Ctx.entries ctx in
  let contacts = Arod.Ctx.contacts ctx in
  let contact_by_domain = build_contact_by_domain contacts in

  (* Domain stats for sidebar (computed over all groups) *)
  let url_set = Hashtbl.create 256 in
  let domain_tbl : (string, int) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun group ->
    List.iter (fun (link : Bushel.Link_graph.external_link) ->
      if not (Hashtbl.mem url_set link.url) then begin
        Hashtbl.add url_set link.url ();
        let cur = try Hashtbl.find domain_tbl link.domain with Not_found -> 0 in
        Hashtbl.replace domain_tbl link.domain (cur + 1)
      end
    ) group.links
  ) groups;
  let domain_counts = Hashtbl.fold (fun d c acc -> (d, c) :: acc) domain_tbl [] in
  let domain_counts = List.sort (fun (_, a) (_, b) -> compare b a) domain_counts in

  (* Filter-kind counts for sidebar (quick classification per unique URL) *)
  let filter_counts : (filter_kind, int) Hashtbl.t = Hashtbl.create 8 in
  let bump_filter k =
    let cur = try Hashtbl.find filter_counts k with Not_found -> 0 in
    Hashtbl.replace filter_counts k (cur + 1)
  in
  Hashtbl.iter (fun url () ->
    let u = Uri.of_string url in
    let host = match Uri.host u with Some h -> h | None -> "" in
    let bare = strip_www (String.lowercase_ascii host) in
    if bare = "arxiv.org" || bare = "doi.org" || Bushel.Link.is_paper_url url then
      bump_filter Fp_paper
    else if find_contact_for_url contact_by_domain bare
              (match Uri.path u with "" -> "/" | p -> p) <> None then
      bump_filter Fp_contact
    else if List.mem bare code_hosts then
      bump_filter Fp_code
    else if bare = "datatracker.ietf.org" || bare = "rfc-editor.org" then
      bump_filter Fp_titled
    else match Arod.Ctx.link_for_url ctx url with
      | Some l ->
        let meta = match l.karakeep with Some k -> k.metadata | None -> [] in
        (match List.assoc_opt "title" meta with
         | Some t when t <> "" -> bump_filter Fp_titled
         | _ -> bump_filter Fp_untitled)
      | None -> bump_filter Fp_untitled
  ) url_set;

  let total_urls = Hashtbl.length url_set in
  let total_domains = List.length domain_counts in
  let total_groups = List.length groups in

  (* Build calendar data: { "YYYY-MM": [link_count_day1, ...], ... } *)
  let month_links : (string, int list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun group ->
    let (y, m, d) = Entry.date group.ent in
    let key = Printf.sprintf "%04d-%02d" y m in
    let cur = try Hashtbl.find month_links key with Not_found -> [] in
    Hashtbl.replace month_links key (d :: cur)
  ) groups;
  let calendar_months =
    Hashtbl.fold (fun k _ acc -> k :: acc) month_links []
    |> List.sort (fun a b -> compare b a)
  in
  let calendar_json =
    let entries = List.map (fun key ->
      let days = Hashtbl.find month_links key in
      let days = List.sort_uniq compare days in
      let day_strs = List.map string_of_int days in
      Printf.sprintf {|"%s":[%s]|} key (String.concat "," day_strs)
    ) calendar_months in
    "{" ^ String.concat "," entries ^ "}"
  in
  let first_month = match calendar_months with
    | m :: _ -> m | [] -> ""
  in

  (* Render only first page of groups *)
  let visible_groups =
    if List.length groups > page_size then Common.take page_size groups
    else groups
  in
  let doi_entries = Entry.doi_entries entries in
  let group_els = List.map (render_group ~contact_by_domain ~doi_entries ~entries ~ctx) visible_groups in

  let intro =
    El.p ~at:[At.class' "text-sm text-gray-600 dark:text-gray-400 mb-6"] [
      El.txt "These are all the outbound links from my site, categorised here for convenient search. They are archived for offline use through ";
      El.a ~at:[At.href "https://karakeep.app";
                At.class' "text-accent hover:underline"] [
        El.txt "Karakeep"];
      El.txt "."]
  in

  let article =
    El.div ~at:[
      At.v "data-pagination" "true";
      At.v "data-collection-type" "links";
      At.v "data-total-count" (string_of_int total_groups);
      At.v "data-current-count" (string_of_int (List.length visible_groups));
      At.v "data-types" ""] [
      intro;
      El.div ~at:[At.class' "link-list"] group_els]
  in

  (* Sidebar *)
  let calendar_box =
    Common.meta_box ~id:"links-calendar"
      ~body_cls:"sidebar-meta-body notes-calendar"
      ~data_attrs:["data-calendar-months", calendar_json;
                   "data-current-month", first_month]
      ~header:[El.txt (Printf.sprintf " %d links \xC2\xB7 %d domains"
                 total_urls total_domains)]
      [El.div ~at:[At.class' "cal-header"] [];
       El.div ~at:[At.class' "heatmap-strip"] [];
       El.div ~at:[At.class' "cal-divider"] [];
       El.div ~at:[At.class' "cal-grid"] []]
  in
  let max_domains_shown = 8 in
  let max_domains_modal = 50 in
  let top_domains =
    let visible = List.filteri (fun i _ -> i < max_domains_shown) domain_counts in
    let domain_row (domain, count) =
      El.p ~at:[At.class' "sidebar-meta-line"] [
        El.txt domain;
        El.span ~at:[At.class' "text-muted ml-auto"]
          [El.txt (string_of_int count)]]
    in
    let visible_els = List.map domain_row visible in
    let expand_btn =
      if total_domains > max_domains_shown then
        El.button ~at:[At.class' "sidebar-meta-expand";
                       At.v "data-modal-target" "domains-modal-overlay"]
          [El.txt (Printf.sprintf "+ %d more" (total_domains - max_domains_shown))]
      else El.void
    in
    Common.meta_box ~header:[El.txt " top domains"]
      (visible_els @ [expand_btn])
  in
  let domains_modal =
    if total_domains > max_domains_shown then
      let modal_domains = List.filteri (fun i _ -> i < max_domains_modal) domain_counts in
      let modal_rows = List.map (fun (domain, count) ->
        El.div ~at:[At.class' "links-modal-row"] [
          El.span ~at:[At.class' "flex-1 truncate"] [El.txt domain];
          El.span ~at:[At.class' "text-muted ml-auto"]
            [El.txt (string_of_int count)]]
      ) modal_domains in
      El.div ~at:[At.id "domains-modal-overlay";
                  At.class' "links-modal-overlay"] [
        El.div ~at:[At.class' "links-modal"] [
          El.div ~at:[At.class' "links-modal-header"] [
            El.span [El.txt (Printf.sprintf "Top Domains (%d)" total_domains)];
            El.button ~at:[At.class' "links-modal-close-btn"]
              [El.txt "\xC3\x97"]];
          El.div ~at:[At.class' "links-modal-body"] modal_rows]]
    else El.void
  in
  let link_filter_box =
    let filter_icon kind =
      let svg_inner = match kind with
        | Fp_paper ->
          (* heroicons/16/solid/academic-cap *)
          {|<path d="M7.702 1.368a.75.75 0 0 1 .597 0c2.098.91 4.105 1.99 6.004 3.223a.75.75 0 0 1-.194 1.348A34.27 34.27 0 0 0 8.341 8.25a.75.75 0 0 1-.682 0c-.625-.32-1.262-.62-1.909-.901v-.542a36.878 36.878 0 0 1 2.568-1.33.75.75 0 0 0-.636-1.357 38.39 38.39 0 0 0-3.06 1.605.75.75 0 0 0-.372.648v.365c-.773-.294-1.56-.56-2.359-.8a.75.75 0 0 1-.194-1.347 40.901 40.901 0 0 1 6.005-3.223ZM4.25 8.348c-.53-.212-1.067-.411-1.611-.596a40.973 40.973 0 0 0-.418 2.97.75.75 0 0 0 .474.776c.175.068.35.138.524.21a5.544 5.544 0 0 1-.58.681.75.75 0 1 0 1.06 1.06c.35-.349.655-.726.915-1.124a29.282 29.282 0 0 0-1.395-.617A5.483 5.483 0 0 0 4.25 8.5v-.152Z"/><path d="M7.603 13.96c-.96-.6-1.958-1.147-2.989-1.635a6.981 6.981 0 0 0 1.12-3.341c.419.192.834.393 1.244.602a2.25 2.25 0 0 0 2.045 0 32.787 32.787 0 0 1 4.338-1.834c.175.978.315 1.969.419 2.97a.75.75 0 0 1-.474.776 29.385 29.385 0 0 0-4.909 2.461.75.75 0 0 1-.794 0Z"/>|}
        | Fp_contact ->
          (* heroicons/16/solid/user *)
          {|<path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6ZM12.735 14c.618 0 1.093-.561.872-1.139a6.002 6.002 0 0 0-11.215 0c-.22.578.254 1.139.872 1.139h9.47Z"/>|}
        | Fp_code ->
          (* heroicons/16/solid/code-bracket *)
          {|<path fill-rule="evenodd" d="M4.78 4.97a.75.75 0 0 1 0 1.06L2.81 8l1.97 1.97a.75.75 0 1 1-1.06 1.06l-2.5-2.5a.75.75 0 0 1 0-1.06l2.5-2.5a.75.75 0 0 1 1.06 0ZM11.22 4.97a.75.75 0 0 0 0 1.06L13.19 8l-1.97 1.97a.75.75 0 1 0 1.06 1.06l2.5-2.5a.75.75 0 0 0 0-1.06l-2.5-2.5a.75.75 0 0 0-1.06 0ZM8.856 2.008a.75.75 0 0 1 .636.848l-1.5 10.5a.75.75 0 0 1-1.484-.212l1.5-10.5a.75.75 0 0 1 .848-.636Z" clip-rule="evenodd"/>|}
        | Fp_titled ->
          (* heroicons/16/solid/link *)
          {|<path fill-rule="evenodd" d="M8.914 6.025a.75.75 0 0 1 1.06 0 3.5 3.5 0 0 1 0 4.95l-2 2a3.5 3.5 0 0 1-5.396-4.402.75.75 0 0 1 1.251.827 2 2 0 0 0 3.085 2.514l2-2a2 2 0 0 0 0-2.828.75.75 0 0 1 0-1.06Z" clip-rule="evenodd"/><path fill-rule="evenodd" d="M7.086 9.975a.75.75 0 0 1-1.06 0 3.5 3.5 0 0 1 0-4.95l2-2a3.5 3.5 0 0 1 5.396 4.402.75.75 0 0 1-1.251-.827 2 2 0 0 0-3.085-2.514l-2 2a2 2 0 0 0 0 2.828.75.75 0 0 1 0 1.06Z" clip-rule="evenodd"/>|}
        | Fp_untitled ->
          (* heroicons/16/solid/ellipsis-horizontal *)
          {|<path d="M2 8a1.5 1.5 0 1 1 3 0 1.5 1.5 0 0 1-3 0ZM6.5 8a1.5 1.5 0 1 1 3 0 1.5 1.5 0 0 1-3 0ZM12.5 6.5a1.5 1.5 0 1 0 0 3 1.5 1.5 0 0 0 0-3Z"/>|}
      in
      El.unsafe_raw (Printf.sprintf
        {|<svg class="inline-block shrink-0" width="12" height="12" viewBox="0 0 16 16" fill="currentColor">%s</svg>|}
        svg_inner)
    in
    let categories = [
      (Fp_paper, "paper");
      (Fp_contact, "contact");
      (Fp_code, "code");
      (Fp_titled, "titled");
      (Fp_untitled, "untitled");
    ] in
    let rows = List.filter_map (fun (kind, label) ->
      let count = try Hashtbl.find filter_counts kind with Not_found -> 0 in
      if count = 0 then None
      else
        let kind_str = string_of_filter_kind kind in
        let checked_at = if kind = Fp_untitled then [] else [At.checked] in
        Some (El.label ~at:[At.class' "paper-filter-row"] [
          El.input ~at:([At.type' "checkbox";
                        At.v "data-link-filter" kind_str;
                        At.class' "link-filter-checkbox sr-only"] @ checked_at) ();
          filter_icon kind;
          El.span ~at:[At.class' "paper-filter-label"] [El.txt label];
          El.span ~at:[At.class' "paper-stat-count"] [El.txt (string_of_int count)]])
    ) categories in
    Common.meta_box
      ~header:[El.txt " ";
               El.span [El.txt (Printf.sprintf "filter: %d links" total_urls)]]
      rows
  in
  let sidebar =
    El.aside ~at:[At.class' "hidden lg:block lg:w-72 shrink-0"]
      [El.div ~at:[At.class' "sticky top-20"]
        [calendar_box; link_filter_box; top_domains];
       domains_modal]
  in
  (article, sidebar)
