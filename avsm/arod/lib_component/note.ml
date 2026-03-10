(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Note component rendering using htmlit. *)

open Htmlit

module Note = Bushel.Note
module I = Arod.Icons

(** {1 Helpers} *)

(** Render a heading for an entry with date, via link, and DOI. *)
let heading ~ctx ent =
  let via, via_url = match ent with
    | `Note n ->
      (match n.Note.via with
       | None -> None, None
       | Some (t, u) -> Some t, Some u)
    | _ -> None, None
  in
  let via_el = match via, via_url with
    | Some t, Some u when t <> "" ->
      El.a ~at:[At.href u; At.class' "text-sm text-secondary"]
        [El.txt (Printf.sprintf "(via %s)" t)]
    | _, Some u ->
      El.a ~at:[At.href u; At.class' "text-sm text-secondary"]
        [El.txt "(via)"]
    | _ -> El.void
  in
  match ent with
  | `Note { index_page = true; _ } -> El.void
  | _ ->
    let doi_el = match ent with
      | `Note n when Note.perma n ->
        (match Note.doi n with
         | Some doi_str ->
           El.span ~at:[At.class' "text-sm text-secondary"] [
             El.txt " / ";
             El.a ~at:[At.href ("https://doi.org/" ^ doi_str)] [El.txt "DOI"]]
         | None -> El.void)
      | _ -> El.void
    in
    let display_title = Bushel.Entry.title ent in
    El.h2 ~at:[At.class' "text-xl font-semibold mb-2"] [
      El.a ~at:[At.href (Bushel.Entry.site_url ent)] [
        El.txt display_title];
      El.txt " "; via_el;
      El.span ~at:[At.class' "text-sm text-secondary"] [
        El.txt " / ";
        (let (y, m, d) = Bushel.Entry.date ent in
         El.time ~at:[At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
           [El.txt (Common.ptime_date_short (y, m, d))])];
      doi_el]

(** Brief note for lists with truncated body. *)
let brief ~ctx n =
  let body_html, word_count_info = Common.truncated_body ~ctx (`Note n) in
  let children = [heading ~ctx (`Note n); body_html] in
  (El.div children, word_count_info)

(** Full note rendering with parent reference link. *)
let full ~ctx n =
  let body = Note.body n in
  let body_with_ref = match Note.slug_ent n with
    | None -> body
    | Some slug_ent ->
      let parent_ent = Arod.Ctx.lookup_exn ctx slug_ent in
      let parent_title = Bushel.Entry.title parent_ent in
      body ^ "\n\nRead more about [" ^ parent_title ^ "](:" ^ slug_ent ^ ")."
  in
  let html, sidenotes = Arod.Md.to_html ~ctx body_with_ref in
  (El.div ~at:[At.class' "mb-4"] [
    heading ~ctx (`Note n);
    El.unsafe_raw html], sidenotes)

(** Full note page with proper header and article structure. *)
let full_page ~ctx n =
  let (y, m, d) = Bushel.Entry.date (`Note n) in
  let date_str = Common.ptime_date_full (y, m, d) in
  let datetime_str = Printf.sprintf "%04d-%02d-%02d" y m d in
  let all_tags = Arod.Ctx.tags_of_ent ctx (`Note n) in
  let doi_el = match Note.doi n with
    | Some doi_str ->
      [El.txt " \xC2\xB7 ";
       El.txt "DOI: ";
       El.a ~at:[At.href ("https://doi.org/" ^ doi_str)] [El.txt doi_str]]
    | None -> []
  in
  (* Meta row — hidden on desktop where sidebar shows this info *)
  let meta_row =
    El.p ~at:[At.class' "text-sm text-secondary mb-2 lg:hidden"]
      ([El.time ~at:[At.v "datetime" datetime_str; At.class' "dt-published"] [El.txt date_str]] @ doi_el)
  in
  (* H1 title (no self-link) *)
  let display_title = Note.title n in
  let title_el =
    Common.page_title ~cls:"page-title text-xl font-semibold tracking-tight mb-1 p-name"
      display_title
  in
  (* Tags below title, like papers *)
  let tags_el = Common.detail_tags (List.map Bushel.Tags.to_raw_string all_tags) in
  (* Synopsis — hidden on desktop where sidebar shows it *)
  let synopsis_el = match Note.synopsis n with
    | Some syn ->
      [El.p ~at:[At.class' "text-base leading-snug text-secondary lg:hidden p-summary"] [El.txt syn]]
    | None -> []
  in
  (* Header *)
  let header_el =
    El.header ~at:[At.id "intro"; At.class' "mb-6"]
      ([meta_row; title_el; tags_el] @ synopsis_el)
  in
  (* Body with parent reference *)
  let body = Note.body n in
  let body_with_ref = match Note.slug_ent n with
    | None -> body
    | Some slug_ent ->
      let parent_ent = Arod.Ctx.lookup_exn ctx slug_ent in
      let parent_title = Bushel.Entry.title parent_ent in
      body ^ "\n\nRead more about [" ^ parent_title ^ "](:" ^ slug_ent ^ ")."
  in
  let body_html, sidenotes = Arod.Md.to_html ~ctx body_with_ref in
  let headings = Arod.Md.extract_headings body_with_ref in
  (* Social discussion icon links at the end of the post *)
  let discuss_el = match Note.social n with
    | None -> El.void
    | Some soc ->
      match Sidebar.social_icon_links ~size:16 soc with
      | [] -> El.void
      | icons ->
        El.div ~at:[At.class' "flex items-center gap-3 mt-8"]
          icons
  in
  let hidden_author = Common.hidden_author_hcard ~ctx in
  let article_el =
    El.article ~at:[At.class' "e-content"] [El.unsafe_raw body_html; discuss_el]
  in
  (El.div ~at:[At.class' "h-entry"] [header_el; hidden_author; article_el], sidenotes, headings)

(** Format a number with comma thousands separators. *)
let format_number n =
  let s = string_of_int n in
  let len = String.length s in
  if len <= 3 then s
  else
    let buf = Buffer.create (len + len / 3) in
    let rem = len mod 3 in
    for i = 0 to len - 1 do
      if i > 0 && (i - rem) mod 3 = 0 then Buffer.add_char buf ',';
      Buffer.add_char buf s.[i]
    done;
    Buffer.contents buf

(** Compact note card for list view. *)
let compact ~ctx note =
  let (y, m, d) = Bushel.Entry.date (`Note note) in
  let date_str =
    if Note.weeknote note then
      let (_,_,_, sun_y, sun_m, sun_d) = Note.week_date_range note in
      let today = Ptime_clock.now () |> Ptime.to_date in
      if today <= (sun_y, sun_m, sun_d) then "ongoing"
      else Printf.sprintf "%d %s %d" d (Common.month_name m) y
    else Printf.sprintf "%d %s %d" d (Common.month_name m) y
  in
  let url = Bushel.Entry.site_url (`Note note) in
  let all_tags = Arod.Ctx.tags_of_ent ctx (`Note note) in
  let tag_strs = List.map Bushel.Tags.to_raw_string all_tags in
  let tags_data = String.concat "," tag_strs in
  let month_data = Printf.sprintf "%04d-%02d" y m in
  let note_id = "note-" ^ Bushel.Entry.slug (`Note note) in
  let synopsis = match Note.synopsis note with
    | Some s -> s
    | None -> ""
  in
  let tag_chips = match tag_strs with
    | [] -> El.void
    | tags ->
      El.div ~at:[At.class' "note-compact-tags"] (
        List.map (fun t ->
          El.a ~at:[At.class' "note-tag-chip"; At.v "data-tag" t;
                    At.href ("#tag=" ^ t)]
            [El.txt ("#" ^ t)]
        ) tags)
  in
  let is_weeknote = Note.weeknote note in
  let is_perma = Note.perma note in
  let card_cls = "note-compact hover:bg-surface note-item h-entry px-1 py-1 md:px-2 md:py-1 md:pl-6"
    ^ (if is_weeknote then " note-weeknote"
       else if is_perma then " note-perma"
       else "") in
  let display_title = Note.title note in
  let ref_el = match Note.slug_ent note with
    | Some slug ->
      (match Arod.Ctx.lookup ctx slug with
       | Some parent_ent ->
         let type_icon = Sidebar.entry_type_icon ~opacity:"opacity-60" ~size:10 parent_ent in
         El.div ~at:[At.class' "note-compact-ref"] [
           El.a ~at:[At.href (Bushel.Entry.site_url parent_ent);
                     At.class' "link-backlink-chip no-underline"]
             [El.unsafe_raw type_icon;
              El.span ~at:[At.class' "note-compact-ref-text"]
                [El.txt (Bushel.Entry.title parent_ent)]]]
       | None -> El.void)
    | None -> El.void
  in
  let type_icon_el, icon_tooltip =
    if is_weeknote then
      I.outline ~cl:"opacity-40" ~size:14 I.calendar_o, "Weekly"
    else if is_perma then
      I.outline ~cl:"opacity-40" ~size:14 I.bookmark_o, "Full post"
    else
      I.outline ~cl:"opacity-40" ~size:14 I.writing_o, "Quick post"
  in
  El.div ~at:[At.id note_id;
              At.class' card_cls;
              At.v "data-tags" tags_data;
              At.v "data-month" month_data] [
    El.span ~at:[At.class' "note-type-icon"; At.v "title" icon_tooltip]
      [El.unsafe_raw type_icon_el];
    (* Row 1: title + meta *)
    El.div ~at:[At.class' "note-compact-row"] [
      El.a ~at:[At.href url; At.class' "note-compact-title flex-1 min-w-0 font-medium !text-text !no-underline p-name u-url"]
        [El.txt display_title];
      El.time ~at:[At.class' "note-compact-meta shrink-0 text-[0.82rem] text-secondary whitespace-nowrap tabular-nums dt-published";
                   At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
        [El.txt date_str]];
    (* Row 2: synopsis — weeknotes get smaller text since their titles are more descriptive *)
    (if synopsis <> "" then
       let synopsis_cls = if is_weeknote
         then "note-compact-synopsis text-[0.74rem] text-secondary leading-[1.3] mt-[0.1rem] p-summary"
         else "note-compact-synopsis text-[0.85rem] text-secondary leading-[1.4] mt-[0.1rem] p-summary"
       in
       El.div ~at:[At.class' synopsis_cls] [El.txt synopsis]
     else El.void);
    (* Row 3: slug_ent reference *)
    ref_el;
    (* Row 4: tags *)
    tag_chips]

(** Notes list page grouped by month with calendar sidebar.
    Returns [(article, sidebar)]. *)
let notes_list ~ctx =
  let all_notes =
    Arod.Ctx.notes ctx
    |> List.sort (fun a b -> Bushel.Entry.compare (`Note a) (`Note b))
    |> List.rev
  in
  let total_notes = List.length all_notes in
  let total_words = List.fold_left (fun acc n -> acc + Note.words n) 0 all_notes in
  (* Group by (year, month) *)
  let by_month = Hashtbl.create 32 in
  List.iter (fun n ->
    let (y, m, _d) = Bushel.Entry.date (`Note n) in
    let key = (y, m) in
    let cur = try Hashtbl.find by_month key with Not_found -> [] in
    Hashtbl.replace by_month key (n :: cur)
  ) all_notes;
  let months =
    Hashtbl.fold (fun k _ acc -> k :: acc) by_month []
    |> List.sort (fun (y1, m1) (y2, m2) ->
      let c = compare y2 y1 in if c <> 0 then c else compare m2 m1)
  in
  (* Build tag frequency map *)
  let tag_counts = Hashtbl.create 64 in
  List.iter (fun n ->
    let tags = Arod.Ctx.tags_of_ent ctx (`Note n) in
    List.iter (fun tag ->
      let t = Bushel.Tags.to_raw_string tag in
      let cur = try Hashtbl.find tag_counts t with Not_found -> 0 in
      Hashtbl.replace tag_counts t (cur + 1)
    ) tags
  ) all_notes;
  (* Sort tags by frequency, take top 20 *)
  let sorted_tags =
    Hashtbl.fold (fun t c acc -> (t, c) :: acc) tag_counts []
    |> List.sort (fun (_, a) (_, b) -> compare b a)
  in
  let top_tags = List.filteri (fun i _ -> i < 20) sorted_tags in
  (* Build calendar data JSON: { "YYYY-MM": [day1, day2, ...], ... } *)
  let calendar_json =
    let month_entries = List.map (fun (y, m) ->
      let notes = Hashtbl.find by_month (y, m) in
      let days = List.map (fun n ->
        let (_, _, d) = Bushel.Entry.date (`Note n) in d
      ) notes in
      let days = List.sort_uniq compare days in
      let key = Printf.sprintf "%04d-%02d" y m in
      let day_strs = List.map string_of_int days in
      Printf.sprintf {|"%s":[%s]|} key (String.concat "," day_strs)
    ) months in
    "{" ^ String.concat "," month_entries ^ "}"
  in
  (* Render month sections *)
  let month_sections = List.map (fun (y, m) ->
    let notes = List.rev (Hashtbl.find by_month (y, m)) in
    let section_id = Printf.sprintf "month-%04d-%02d" y m in
    let month_id = Printf.sprintf "%04d-%02d" y m in
    let note_cards = List.map (fun n -> compact ~ctx n) notes in
    El.div ~at:[At.id section_id;
                At.v "data-month-id" month_id;
                At.class' "mb-6"] [
      El.div ~at:[At.class' "paper-year-header sticky top-0 bg-bg z-10 py-0.5"] [
        El.txt (Printf.sprintf "%s %d" (Common.month_name_full m) y)];
      El.div ~at:[At.class' "note-month-list"] note_cards]
  ) months in
  (* Article *)
  let article = El.article ~at:[At.class' "h-feed"] [El.div month_sections]
  in
  (* Sidebar: calendar box — stats in header + heatmap + per-month calendar *)
  let first_month = match months with
    | (y, m) :: _ -> Printf.sprintf "%04d-%02d" y m
    | [] -> ""
  in
  let calendar_box =
    Common.meta_box ~id:"notes-calendar"
      ~body_cls:"sidebar-meta-body notes-calendar"
      ~data_attrs:["data-calendar-months", calendar_json;
                   "data-current-month", first_month]
      ~header:[El.txt (Printf.sprintf " %s notes \xC2\xB7 %s words"
                 (format_number total_notes) (format_number total_words))]
      [El.div ~at:[At.class' "cal-header"] [];
       El.div ~at:[At.class' "heatmap-strip"] [];
       El.div ~at:[At.class' "cal-divider"] [];
       El.div ~at:[At.class' "cal-grid"] []]
  in
  (* Sidebar: tag cloud box *)
  let tag_cloud_box = match top_tags with
    | [] -> El.void
    | _ ->
      let tag_btns = List.map (fun (tag, count) ->
        El.button ~at:[At.class' "tag-cloud-btn";
                       At.v "data-tag" tag] [
          El.txt tag;
          El.span ~at:[At.class' "tag-count inline-flex items-center justify-center min-w-[0.95rem] h-[0.95rem] text-[0.5rem] font-semibold text-muted bg-surface-alt rounded-full leading-none tabular-nums"] [
            El.txt (string_of_int count)]]
      ) top_tags in
      Common.meta_box ~body_cls:"sidebar-meta-body tag-cloud"
        ~header:[El.txt " tags"] tag_btns
  in
  let sidebar =
    El.aside ~at:[At.class' "hidden lg:block lg:w-72 shrink-0"]
      [El.div ~at:[At.class' "sticky top-16"]
         [calendar_box; tag_cloud_box]]
  in
  (article, sidebar)

(** Truncated note for feeds. *)
let for_feed ~ctx n = Common.truncated_body ~ctx (`Note n)

(** Citation references section for notes with DOI-bearing links. *)
let references ~ctx n =
  let cfg = Arod.Ctx.config ctx in
    let me = Arod.Ctx.lookup_by_handle ctx cfg.site.author_handle in
    match me with
    | None -> El.void
    | Some author_contact ->
      let entries = Arod.Ctx.entries ctx in
      let refs = Bushel.Md.note_references entries author_contact n in
      match refs with
      | [] -> El.void
      | _ ->
        let ref_items = List.mapi (fun i (doi, citation, is_paper) ->
          let num = i + 1 in
          let doi_url = Printf.sprintf "https://doi.org/%s" doi in
          let cite_id = Arod.Md.doi_to_id doi in
          let icon = match is_paper with
            | Bushel.Md.Paper -> Arod.Icons.(outline ~cl:"opacity-40" ~size:12 paper_o)
            | Bushel.Md.Note -> Arod.Icons.(outline ~cl:"opacity-40" ~size:12 note_o)
            | Bushel.Md.External -> Arod.Icons.(outline ~cl:"opacity-40" ~size:12 external_link_o)
          in
          El.div ~at:[At.id (Printf.sprintf "ref-%d" num);
                      At.class' "ref-item h-cite"] [
            El.span ~at:[At.class' "ref-num"] [
              El.a ~at:[At.href ("#" ^ cite_id); At.class' "ref-backlink no-underline";
                        At.v "title" "Jump to citation"]
                [El.txt (Printf.sprintf "[%d]" num)]];
            El.unsafe_raw icon;
            El.span ~at:[At.class' "ref-body"] [
              El.span ~at:[At.class' "p-name"] [El.txt (citation ^ " ")];
              El.a ~at:[At.href doi_url; At.v "target" "_blank";
                        At.v "rel" "noopener";
                        At.class' "ref-doi u-url"] [El.txt doi]]]
        ) refs in
        El.div ~at:[At.class' "references-block mt-8"] [
          El.h3 ~at:[At.class' "text-sm font-semibold text-secondary uppercase tracking-wide mb-2"]
            [El.txt "References"];
          El.div ~at:[At.class' "ref-list"] ref_items]
