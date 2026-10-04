(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Note components. *)

open Htmlit

module Note = Bushel.Note
module I = Arod.Icons

(** [heading ~ctx ent] is the heading for [ent]. *)
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
      El.a ~at:[At.href (Bushel.Entry.site_url ent); At.class' "p-name u-url"] [
        El.txt display_title];
      El.txt " "; via_el;
      El.span ~at:[At.class' "text-sm text-secondary"] [
        El.txt " / ";
        (let (y, m, d) = Bushel.Entry.date ent in
         El.time ~at:[At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d);
                      At.class' "dt-published"]
           [El.txt (Common.ptime_date_short (y, m, d))])];
      doi_el]

(** [brief ~ctx n] is a truncated rendering of [n]. *)
let brief ~ctx n =
  let body_html, word_count_info = Common.truncated_body ~ctx (`Note n) in
  let children = [heading ~ctx (`Note n); body_html] in
  (El.div children, word_count_info)

(** [full ~ctx n] is the full rendering of [n]. *)
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

(** [full_page ~ctx n] is the article page for [n]. *)
let full_page ~ctx n =
  let (y, m, d) = Bushel.Entry.date (`Note n) in
  let date_str = Common.ptime_date_full (y, m, d) in
  let datetime_str = Printf.sprintf "%04d-%02d-%02d" y m d in
  let all_tags = Bushel.Entry.tags_of_ent (`Note n) in
  let display_title = Note.title n in
  let title_el =
    Common.page_title ~cls:"page-title text-xl font-semibold tracking-tight mb-2 p-name"
      display_title
  in
  let tags_el = Common.detail_tags ~date:(y, m, d) ?doi:(Note.doi n)
    (List.map Bushel.Tags.to_raw_string all_tags) in
  let synopsis_el = match Note.synopsis n with
    | Some syn ->
      [El.p ~at:[At.class' "detail-synopsis lg:hidden p-summary"] [El.txt syn]]
    | None -> []
  in
  let dt_el = El.time ~at:[At.v "datetime" datetime_str; At.class' "dt-published hidden"] [El.txt date_str] in
  let dt_upd_el = match n.Note.updated with
    | Some d -> [Common.hidden_dt_updated d]
    | None -> []
  in
  let header_el =
    El.header ~at:[At.id "intro"; At.class' "mb-6"]
      ([title_el; tags_el; dt_el] @ dt_upd_el @ synopsis_el)
  in
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
  let hidden_meta = Common.hidden_entry_meta ~ctx ?doi:(Note.doi n) (`Note n) in
  let article_el =
    El.article ~at:[At.class' "e-content"] [El.unsafe_raw body_html; discuss_el]
  in
  (El.div ~at:[At.class' "h-entry"] [header_el; hidden_author; hidden_meta; article_el], sidenotes, headings)

(** [format_number n] is [n] with comma thousands separators. *)
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

(** [compact ~ctx note] is a compact journal card for [note]. *)
(** [short_date (_, m, d)] is a date as ["28 Sep"]. *)
let short_date (_, m, d) = Printf.sprintf "%d %s" d (Common.month_name m)

(** [tl_bullet ~ctx ~url ~kind ~icon entry] is the circle on the timeline that
    marks [entry]. It is the image of [entry], and without one an icon on a
    tinted disc. [kind] picks its size and colour. *)
let tl_bullet ~ctx ~url ~kind ~icon entry =
  let cls = "tl-bullet tl-bullet-" ^ kind in
  match Bushel.Entry.thumbnail (Arod.Ctx.entries ctx) entry with
  | Some src ->
    El.a ~at:[At.href url; At.class' cls;
              At.v "tabindex" "-1"; At.v "aria-hidden" "true"]
      [El.img ~at:[At.src src; At.v "alt" ""; At.v "loading" "lazy";
                   At.class' "tl-bullet-img"] ()]
  | None ->
    El.span ~at:[At.class' (cls ^ " tl-bullet-icon");
                 At.v "aria-hidden" "true"]
      [El.unsafe_raw
         (Arod.Icons.outline ~size:(if kind = "note" then 15 else 11) icon)]

let compact ?(cls="") ?(timeline=false) ~ctx note =
  let (y, m, d) = Bushel.Entry.date (`Note note) in
  let date_str = Printf.sprintf "%d %s %d" d (Common.month_name m) y in
  let url = Bushel.Entry.site_url (`Note note) in
  let all_tags = Bushel.Entry.tags_of_ent (`Note note) in
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
          El.a ~at:[At.class' "note-tag-chip p-category"; At.v "data-tag" t;
                    At.href ("#tag=" ^ t)]
            [El.txt ("#" ^ t)]
        ) tags)
  in
  let is_perma = Note.perma note in
  let card_cls = "note-compact hover:bg-surface note-item h-entry px-1 py-1 md:px-2 md:py-1"
    ^ (if timeline then " tl-item tl-note" else "")
    ^ (if is_perma then " note-perma" else "")
    ^ (if cls = "" then "" else " " ^ cls) in
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
  let body = [
    El.div ~at:[At.class' "note-compact-row"] [
      El.a ~at:[At.href url; At.class' "note-compact-title flex-1 min-w-0 font-medium !text-text !no-underline p-name u-url"]
        [El.txt display_title];
      El.time ~at:[At.class' "note-compact-meta shrink-0 text-[0.82rem] text-secondary whitespace-nowrap tabular-nums dt-published";
                   At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
        [El.txt date_str]];
    (if synopsis <> "" then
       El.div ~at:[At.class' "note-compact-synopsis text-[0.85rem] text-secondary leading-[1.4] mt-[0.1rem] p-summary"]
         [El.txt synopsis]
     else El.void);
    ref_el;
    tag_chips]
  in
  (* On the timeline a note is a bullet on the line and its text beside it, with
     its date as a small caption above the title and not in a column. *)
  let children =
    if timeline then
      [tl_bullet ~ctx ~url ~kind:"note" ~icon:Arod.Icons.note_o (`Note note);
       El.div ~at:[At.class' "tl-body min-w-0 flex-1"] [
         El.time ~at:[At.class' "tl-meta dt-published";
                      At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
           [El.txt (short_date (y, m, d))];
         El.a ~at:[At.href url; At.class' "tl-title p-name u-url"]
           [El.txt display_title];
         (if synopsis <> "" then
            El.div ~at:[At.class' "tl-synopsis p-summary"] [El.txt synopsis]
          else El.void);
         ref_el;
         tag_chips]]
    else body
  in
  El.div ~at:[At.id note_id;
              At.class' card_cls;
              At.v "data-tags" tags_data;
              At.v "data-month" month_data] children

(** [strip_weeknote_prefix t] is [t] without its weeknote prefix. *)
let strip_weeknote_prefix t =
  if String.length t >= 6 && String.sub t 0 6 = ".plan-" then
    match String.index_opt t ':' with
    | Some i when i + 2 < String.length t ->
      String.sub t (i + 2) (String.length t - i - 2)
    | _ -> t
  else t

let week_step pt =
  Option.get (Ptime.sub_span pt (Ptime.Span.of_int_s (7 * 86400)))

(** [week_key date] is the ISO week of [date]. *)
let week_key date = Note.iso_week_number date

(** [quiet_between ~newer ~older] is the number of weeks between the weeks of
    two dates, which have nothing in them. *)
let quiet_between ~newer ~older =
  let target = week_key older in
  if week_key newer = target then 0
  else
    match Ptime.of_date newer with
    | None -> 0
    | Some t ->
      let rec go pt steps =
        if steps > 520 then 0
        else if week_key (Ptime.to_date pt) = target then steps - 1
        else go (week_step pt) (steps + 1)
      in
      max 0 (go (week_step t) 1)

(** [week_range date] is the Monday to Sunday of the week of [date], as
    ["11\xE2\x80\x9317 Aug"]. *)
let week_range date =
  match Ptime.of_date date with
  | None -> ""
  | Some t ->
    let back =
      match Ptime.weekday t with
      | `Mon -> 0 | `Tue -> 1 | `Wed -> 2 | `Thu -> 3
      | `Fri -> 4 | `Sat -> 5 | `Sun -> 6
    in
    let day k =
      Ptime.to_date
        (Option.get (Ptime.add_span t (Ptime.Span.of_int_s (k * 86400))))
    in
    let (_, m1, d1) = day (-back) and (_, m2, d2) = day (6 - back) in
    if m1 = m2 then
      Printf.sprintf "%d\xE2\x80\x93%d %s" d1 d2 (Common.month_name m1)
    else
      Printf.sprintf "%d %s \xE2\x80\x93 %d %s" d1 (Common.month_name m1) d2
        (Common.month_name m2)

(** [quiet_marker n] is the mark on the line for [n] weeks with nothing in
    them. *)
let quiet_marker n =
  El.div ~at:[At.class' "tl-quiet"]
    [El.txt (if n = 1 then "1 quiet week"
             else Printf.sprintf "%d quiet weeks" n)]

(** [week_head ~ctx n] is weeknote [n] as the head of its week on the
    timeline. *)
let week_head ~ctx n =
  let (y, m, d) = Note.date n in
  let (_, wk) = Note.week_number n in
  let url = Bushel.Entry.site_url (`Note n) in
  let tags_data =
    String.concat ","
      (List.map Bushel.Tags.to_raw_string (Bushel.Entry.tags_of_ent (`Note n)))
  in
  El.div ~at:[At.class' "tl-item tl-week note-item h-entry";
              At.v "data-tags" tags_data;
              At.v "data-month" (Printf.sprintf "%04d-%02d" y m);
              At.v "title" (Printf.sprintf "%s words" (format_number (Note.words n)))] [
    tl_bullet ~ctx ~url ~kind:"week" ~icon:Arod.Icons.calendar_week_o
      (`Note n);
    El.div ~at:[At.class' "tl-body min-w-0 flex-1"] [
      El.div ~at:[At.class' "tl-meta"] [
        El.txt (Printf.sprintf "W%02d" wk);
        El.txt " \xC2\xB7 ";
        El.time ~at:[At.class' "dt-published";
                     At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
          [El.txt (week_range (y, m, d))]];
      El.a ~at:[At.href url; At.class' "tl-title tl-week-title p-name u-url"]
        [El.txt (strip_weeknote_prefix (Note.title n))]]]

(** [release_item t rs] is the line for the releases [rs] of repository [t],
    newest first, made in one month. It is the smallest entry on the
    timeline: a rocket on the line, the name and version, the date, and one
    line of summary. A month's releases of one repository are one line, the
    newest named and the rest counted. Each registry that carries the newest is
    an icon linking to its ecosyste.ms metadata. A release is a [note-item]
    with no tags, so a tag filter hides it. *)
let release_item (t : Bushel.Release.t) (rs : Bushel.Release.release list) =
  let r = List.hd rs in
  let earlier = List.tl rs in
  let (y, m, d) = r.date in
  let name =
    match String.rindex_opt t.repo '/' with
    | Some i -> String.sub t.repo (i + 1) (String.length t.repo - i - 1)
    | None -> t.repo
  in
  let registry reg =
    let label = reg.Bushel.Release.name ^ " on ecosyste.ms" in
    El.a ~at:[At.href (Bushel.Release.metadata_url reg r);
              At.class' "release-registry";
              At.v "title" label; At.v "aria-label" label]
      [El.unsafe_raw (Arod.Icons.registry_icon ~size:11 reg.Bushel.Release.name)]
  in
  El.div ~at:[At.class' "tl-item tl-release note-item";
              At.v "data-kind" "release";
              At.v "data-tags" "";
              At.v "data-month" (Printf.sprintf "%04d-%02d" y m)] [
    El.span ~at:[At.class' "tl-bullet tl-bullet-release release-mark";
                 At.v "role" "img"; At.v "aria-label" "Code release";
                 At.v "title" "Code release"]
      [El.unsafe_raw (Arod.Icons.outline ~size:9 Arod.Icons.rocket_o)];
    El.div ~at:[At.class' "tl-body release-line min-w-0 flex-1"] [
      El.a ~at:[At.href r.url;
                At.class' "release-name !text-text !no-underline"]
        [El.txt (name ^ " " ^ r.version)];
      El.time ~at:[At.class' "release-date";
                   At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
        [El.txt (short_date (y, m, d))];
      (match earlier with
       | [] -> El.void
       | _ ->
         El.span ~at:[At.class' "release-earlier";
                      At.v "title"
                        (String.concat ", "
                           (List.rev_map (fun (e : Bushel.Release.release) ->
                              e.version) earlier))]
           [El.txt (Printf.sprintf "+%d earlier" (List.length earlier))]);
      El.span ~at:[At.class' "release-summary"] [El.txt r.summary];
      (match r.registries with
       | [] -> El.void
       | regs ->
         El.span ~at:[At.class' "release-registries"] (List.map registry regs))]]

(** [group_releases rs] is the [(repository, releases)] of [rs], one per
    repository, with each repository's releases newest first. *)
let group_releases rs =
  let repos =
    List.sort_uniq String.compare
      (List.map (fun ((t : Bushel.Release.t), _) -> t.repo) rs)
  in
  List.map (fun repo ->
    let mine = List.filter (fun ((t : Bushel.Release.t), _) -> t.repo = repo) rs in
    (fst (List.hd mine),
     List.sort Bushel.Release.compare_release (List.map snd mine))) repos

(** [by_week items] is [items], which are newest first, split into runs of one
    ISO week, each with the key of its week. *)
let by_week items =
  List.fold_left (fun acc ((date, _) as item) ->
    let key = week_key date in
    match acc with
    | (k, run) :: rest when k = key -> (k, item :: run) :: rest
    | _ -> (key, [item]) :: acc) [] items
  |> List.rev_map (fun (k, run) -> (k, List.rev run))

(** [notes_list ~ctx] is the journal article and its sidebar. *)
let notes_list ~ctx =
  let all_notes =
    Arod.Ctx.notes ctx
    |> List.sort (fun a b -> Bushel.Entry.compare (`Note a) (`Note b))
    |> List.rev
  in
  let weeknotes, journal_notes = List.partition Note.weeknote all_notes in
  let by_month = Hashtbl.create 32 in
  List.iter (fun n ->
    let (y, m, _d) = Bushel.Entry.date (`Note n) in
    let key = (y, m) in
    let cur = try Hashtbl.find by_month key with Not_found -> [] in
    Hashtbl.replace by_month key (n :: cur)
  ) all_notes;
  let releases_by_month = Hashtbl.create 32 in
  List.iter (fun (t : Bushel.Release.t) ->
    List.iter (fun (r : Bushel.Release.release) ->
      let (y, m, _) = r.date in
      let cur =
        try Hashtbl.find releases_by_month (y, m) with Not_found -> [] in
      Hashtbl.replace releases_by_month (y, m) ((t, r) :: cur)) t.releases)
    (Arod.Ctx.releases ctx);
  (* The weeks with nothing in them, marked after the last week with something.
     A release or a note of any kind makes a week. *)
  let active_weeks = Hashtbl.create 64 in
  let touch date =
    let key = week_key date in
    match Hashtbl.find_opt active_weeks key with
    | Some seen when compare seen date >= 0 -> ()
    | _ -> Hashtbl.replace active_weeks key date
  in
  List.iter (fun n -> touch (Bushel.Entry.date (`Note n))) all_notes;
  Hashtbl.iter (fun _ rs ->
    List.iter (fun (_, (r : Bushel.Release.release)) -> touch r.date) rs)
    releases_by_month;
  let quiet_after = Hashtbl.create 64 in
  let rec note_gaps = function
    | (newer_key, newer) :: ((_, older) :: _ as rest) ->
      Hashtbl.replace quiet_after newer_key (quiet_between ~newer ~older);
      note_gaps rest
    | _ -> ()
  in
  note_gaps
    (List.sort (fun (_, a) (_, b) -> compare b a)
       (Hashtbl.fold (fun k d acc -> (k, d) :: acc) active_weeks []));
  let months =
    let keys tbl = Hashtbl.fold (fun k _ acc -> k :: acc) tbl [] in
    List.sort_uniq (fun (y1, m1) (y2, m2) ->
      let c = compare y2 y1 in if c <> 0 then c else compare m2 m1)
      (keys by_month @ keys releases_by_month)
  in
  let week_group (key, items) =
    (* The weeknote heads its week. A week without one has a plain label. *)
    let weeknote =
      List.find_map (function (_, `Week n) -> Some n | _ -> None) items in
    let rest = List.filter (function (_, `Week _) -> false | _ -> true) items in
    let head =
      match weeknote with
      | Some n -> week_head ~ctx n
      | None ->
        El.div ~at:[At.class' "tl-wk-label"]
          [El.txt (week_range (fst (List.hd items)))]
    in
    let entries = List.map (fun (_, item) ->
      match item with
      | `Note n -> compact ~timeline:true ~ctx n
      | `Release (t, rs) -> release_item t rs
      | `Week _ -> El.void) rest in
    El.div ~at:[At.class' "tl-wk"] (head :: entries)
    :: (match Hashtbl.find_opt quiet_after key with
        | Some q when q > 0 -> [quiet_marker q]
        | _ -> [])
  in
  let month_sections = List.map (fun (y, m) ->
    let notes =
      List.rev (try Hashtbl.find by_month (y, m) with Not_found -> []) in
    let releases =
      try Hashtbl.find releases_by_month (y, m) with Not_found -> [] in
    let section_id = Printf.sprintf "month-%04d-%02d" y m in
    let month_id = Printf.sprintf "%04d-%02d" y m in
    (* Notes, weeknotes and releases run together, newest first. On one day a
       note comes before a release. *)
    let items =
      List.map (fun n ->
        (Bushel.Entry.date (`Note n),
         if Note.weeknote n then `Week n else `Note n)) notes
      @ List.map (fun (t, rs) ->
          ((List.hd rs).Bushel.Release.date, `Release (t, rs)))
          (group_releases releases)
      |> List.stable_sort (fun (d1, _) (d2, _) -> compare d2 d1)
    in
    El.div ~at:[At.id section_id;
                At.v "data-month-id" month_id;
                At.class' "tl-month"] [
      El.div ~at:[At.class' "tl-month-head paper-year-header sticky top-0 bg-bg z-10 py-0.5"] [
        El.txt (Printf.sprintf "%s %d" (Common.month_name_full m) y)];
      El.div ~at:[At.class' "tl-weeks"]
        (List.concat_map week_group (by_week items))]
  ) months in
  let article =
    El.article ~at:[At.class' "h-feed"] [
      Common.hidden_feed_meta ~ctx "Notes";
      El.div ~at:[At.class' "timeline notes-journal min-w-0"] month_sections]
  in
  let featured_rail =
    let featured =
      match List.filter Note.featured journal_notes with
      | [] -> List.filter Note.perma journal_notes |> Common.take 5
      | marked -> marked
    in
    match featured with
    | [] -> El.void
    | _ ->
      let feat_card n =
        let url = Bushel.Entry.site_url (`Note n) in
        let (y, m, d) = Bushel.Entry.date (`Note n) in
        let slice = match Bushel.Entry.thumbnail (Arod.Ctx.entries ctx) (`Note n) with
          | Some src ->
            El.a ~at:[At.href url; At.class' "feat-slice-link";
                      At.v "tabindex" "-1"; At.v "aria-hidden" "true"]
              [El.img ~at:[At.src src; At.v "alt" "";
                           At.v "loading" "lazy";
                           At.class' "week-slice"] ()]
          | None -> El.void
        in
        let doi_el = match Note.doi n with
          | Some doi ->
            El.span [El.txt " \xC2\xB7 ";
                     El.a ~at:[At.href ("https://doi.org/" ^ doi);
                               At.class' "feat-doi"] [El.txt "DOI"]]
          | None -> El.void
        in
        El.div ~at:[At.class' "feat-card"] [
          El.div ~at:[At.class' "week-row-body min-w-0"] [
            El.div ~at:[At.class' "week-meta"] [
              El.time ~at:[At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
                [El.txt (Printf.sprintf "%d %s %d" d (Common.month_name m) y)];
              doi_el];
            El.a ~at:[At.href url; At.class' "week-title"]
              [El.txt (Note.title n)]];
          slice]
      in
      El.div ~at:[At.class' "notes-feat"] [
        El.div ~at:[At.class' "paper-year-header"] [El.txt "Featured"];
        El.div ~at:[At.class' "feat-list"] (List.map feat_card featured)]
  in
  let sidebar =
    El.aside ~at:[At.class' "hidden lg:block lg:w-52 shrink-0"]
      [featured_rail]
  in
  (article, sidebar)

(** [for_feed ~ctx n] is the truncated feed rendering of [n]. *)
let for_feed ~ctx n = Common.truncated_body ~ctx (`Note n)

(** [references ~ctx n] is the citation list for [n]. *)
let references ~ctx n =
  match Arod.Ctx.note_references ctx (Bushel.Note.slug n) with
  | [] -> El.void
  | refs ->
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
