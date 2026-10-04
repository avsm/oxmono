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

(** [short_date (_, m, d)] is a date as ["28 Sep"]. *)
let short_date (_, m, d) = Printf.sprintf "%d %s" d (Common.month_name m)

(** [compact ~ctx note] is a compact journal card for [note]. *)
let compact ?(cls="") ~ctx note =
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
  El.div ~at:[At.id note_id;
              At.class' card_cls;
              At.v "data-tags" tags_data;
              At.v "data-month" month_data] [
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

(* The timeline. Every entry is a row of fixed height, positioned by its [top]
   and [height] in em, and the spine is drawn from those positions (see
   [Snake]). A row holds the exit curve from the spine, the node on it, and the
   text beside it. *)

let pos_style ~top ~height =
  Printf.sprintf "top:%.3fem;height:%.3fem" top height

(** [exit_svg kind ~y_abs] is the svg of the exit curve of a row of [kind] that
    begins [y_abs] down the timeline. It starts on the spine, above the row. *)
let exit_svg ~plain kind ~y_abs =
  let e = Snake.exit_ ~plain ~kind ~y_abs in
  let top = -5.0 in
  let w = Float.max Snake.svg_width (Snake.arrive ~plain kind +. 0.5) in
  let h = Snake.height kind -. top in
  Printf.sprintf
    {|<svg class="sn-exit%s" viewBox="0 %.2f %.2f %.2f" style="top:%.2fem;width:%.2fem;height:%.2fem" aria-hidden="true" focusable="false"><path class="sn-lane" d="%s"/><path class="sn-exit-path" d="%s"/><path class="sn-flow" d="%s"/></svg>|}
    (if plain then " sn-exit-fade" else "")
    top w h top w h e.Snake.lane e.Snake.path
    e.Snake.path

(** [node_style kind] is the position and size of the node of a row of
    [kind]. *)
let node_style kind =
  let h = Snake.node_height kind in
  Printf.sprintf "left:%.3fem;top:%.3fem;width:%.3fem;height:%.3fem"
    (Snake.node_left kind) (Snake.center kind -. (h /. 2.))
    (Snake.node_width kind) h

(** [sn_node ~url ~kind ~label src] is the thumbnail that begins a row, linking
    to [url]. *)
let sn_node ~url ~kind ~label src =
  El.a ~at:[At.href url; At.class' ("sn-node sn-node-" ^ label);
            At.v "style" (node_style kind);
            At.v "tabindex" "-1"; At.v "aria-hidden" "true"]
    [El.img ~at:[At.src src; At.v "alt" ""; At.v "loading" "lazy";
                 At.class' "sn-node-img"] ()]

(** [thumbnail ~ctx n] is the image of note [n], if it has one. *)
let thumbnail ~ctx n =
  Bushel.Entry.thumbnail (Arod.Ctx.entries ctx) (`Note n)

let text_style kind =
  Printf.sprintf "left:%.3fem" (Snake.text_left kind)

(** [title_stop title] is the full stop that ends [title] before the synopsis
    runs on after it, or nothing when [title] already ends in punctuation. *)
let title_stop title =
  match title.[String.length title - 1] with
  | '.' | '!' | '?' | ':' -> ""
  | _ -> "."
  | exception Invalid_argument _ -> ""

(** [sn_tags ?limit n] is the column at the right of the row of [n]. It holds
    its plain and set tags, at most [limit] (default four), each linking to a
    search for it, above the icons for its DOI, StandardSite page and social
    discussions. *)
let sn_tags ?(limit = 4) n =
  let tags =
    List.filter_map (function
      | (`Text _ | `Set _) as t -> Some (Bushel.Tags.to_raw_string t)
      | _ -> None) (Bushel.Entry.tags_of_ent (`Note n))
  in
  let tags = List.filteri (fun i _ -> i < limit) tags in
  let link ~icon ~label ?(cl = "") href =
    El.a ~at:[At.href href; At.class' ("sn-link " ^ cl); At.v "title" label;
              At.v "aria-label" label; At.v "rel" "noopener"]
      [El.unsafe_raw (I.outline ~size:11 icon)]
  in
  let doi =
    match Note.doi n with
    | Some d ->
      [link ~icon:I.fingerprint_o ~label:("DOI " ^ d) ("https://doi.org/" ^ d)]
    | None -> []
  in
  let standardsite =
    match Note.standardsite n with
    | Some uri ->
      [link ~icon:I.world_o ~label:"StandardSite" ("https://pdsls.dev/" ^ uri)]
    | None -> []
  in
  let social =
    match Note.social n with
    | Some soc -> Sidebar.social_icon_links ~size:11 soc
    | None -> []
  in
  let links = doi @ standardsite @ social in
  El.div ~at:[At.class' "sn-tags"]
    ((if tags = [] then []
      else
        [El.div ~at:[At.class' "sn-tag-list"]
           (List.map (fun t ->
              El.a ~at:[At.href ("#tag=" ^ t); At.v "data-tag" t;
                        At.class' "sn-tag"] [El.txt ("#" ^ t)]) tags)])
    @ (if links = [] then []
       else [El.div ~at:[At.class' "sn-links"] links]))

(** [sn_note ~ctx ~y_rel ~y_abs n] is journal note [n] as a row. *)
let sn_note ~ctx ~y_rel ~y_abs n =
  let (y, m, d) = Bushel.Entry.date (`Note n) in
  let url = Bushel.Entry.site_url (`Note n) in
  let tags_data =
    String.concat ","
      (List.map Bushel.Tags.to_raw_string (Bushel.Entry.tags_of_ent (`Note n)))
  in
  let synopsis = Option.value (Note.synopsis n) ~default:"" in
  let title = Note.title n in
  let image = thumbnail ~ctx n in
  El.div ~at:[At.id ("note-" ^ Bushel.Entry.slug (`Note n));
              At.class' "sn-item sn-note note-item h-entry";
              At.v "data-tags" tags_data;
              At.v "data-month" (Printf.sprintf "%04d-%02d" y m);
              At.v "style"
                (pos_style ~top:y_rel ~height:(Snake.height Snake.Note))] [
    El.unsafe_raw (exit_svg ~plain:(image = None) Snake.Note ~y_abs);
    (match image with
     | Some src -> sn_node ~url ~kind:Snake.Note ~label:"note" src
     | None -> El.void);
    El.div ~at:[At.class' "sn-text"; At.v "style" (text_style Snake.Note)] [
      El.div ~at:[At.class' "sn-body"] [
        El.time ~at:[At.class' "sn-meta dt-published";
                     At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
          [El.txt (short_date (y, m, d))];
        El.p ~at:[At.class' "sn-line"] [
          El.a ~at:[At.href url; At.class' "sn-title p-name u-url"]
            [El.txt (if synopsis = "" then title else title ^ title_stop title)];
          (if synopsis <> "" then
             El.span ~at:[At.class' "sn-synopsis p-summary"]
               [El.txt synopsis]
           else El.void)]];
      sn_tags n]]

(** [sn_week ~ctx ~y_rel ~y_abs n] is weeknote [n] as a row. Its node is a
    rounded square and its row is tinted, so that it is not mistaken for a
    note. *)
let sn_week ~ctx ~y_rel ~y_abs n =
  let (y, m, d) = Note.date n in
  let (_, wk) = Note.week_number n in
  let synopsis = Option.value (Note.synopsis n) ~default:"" in
  let title = strip_weeknote_prefix (Note.title n) in
  let image = thumbnail ~ctx n in
  let url = Bushel.Entry.site_url (`Note n) in
  let tags_data =
    String.concat ","
      (List.map Bushel.Tags.to_raw_string (Bushel.Entry.tags_of_ent (`Note n)))
  in
  El.div ~at:[At.class' "sn-item sn-week note-item h-entry";
              At.v "data-tags" tags_data;
              At.v "data-month" (Printf.sprintf "%04d-%02d" y m);
              At.v "title" (Printf.sprintf "%s words" (format_number (Note.words n)));
              At.v "style"
                (pos_style ~top:y_rel ~height:(Snake.height Snake.Week))] [
    El.unsafe_raw (exit_svg ~plain:(image = None) Snake.Week ~y_abs);
    (match image with
     | Some src -> sn_node ~url ~kind:Snake.Week ~label:"week" src
     | None -> El.void);
    El.div ~at:[At.class' "sn-text"; At.v "style" (text_style Snake.Week)] [
      El.div ~at:[At.class' "sn-body"] [
        El.div ~at:[At.class' "sn-meta"] [
          El.txt (Printf.sprintf "Week %d" wk);
          El.txt " \xC2\xB7 ";
          El.time ~at:[At.class' "dt-published";
                       At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" y m d)]
            [El.txt (week_range (y, m, d))]];
        El.p ~at:[At.class' "sn-line"] [
          El.a ~at:[At.href url; At.class' "sn-title p-name u-url"]
            [El.txt (if synopsis = "" then title else title ^ title_stop title)];
          (if synopsis <> "" then
             El.span ~at:[At.class' "sn-synopsis p-summary"]
               [El.txt synopsis]
           else El.void)]];
      sn_tags n]]

(** [sn_release ~y_rel ~y_abs t rs] is the row for the releases [rs] of
    repository [t], newest first, made in one month. It is the smallest row: a
    rocket on a small node, the name and version, the date, and one line of
    summary. A month's releases of one repository are one row, the newest named
    and the rest counted. Each registry that carries the newest is an icon
    linking to its ecosyste.ms metadata. A release is a [note-item] with no
    tags, so a tag filter hides it. *)
let sn_release ~y_rel ~y_abs (t : Bushel.Release.t)
    (rs : Bushel.Release.release list) =
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
      [El.unsafe_raw
         (Arod.Icons.registry_icon ~size:12 reg.Bushel.Release.name)]
  in
  El.div ~at:[At.class' "sn-item sn-release note-item";
              At.v "data-kind" "release";
              At.v "data-tags" "";
              At.v "data-month" (Printf.sprintf "%04d-%02d" y m);
              At.v "style"
                (pos_style ~top:y_rel ~height:(Snake.height Snake.Release))] [
    El.unsafe_raw (exit_svg ~plain:true Snake.Release ~y_abs);
    El.div ~at:[At.class' "sn-text release-line";
                At.v "style" (text_style Snake.Release)] [
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

(** [sn_quiet ~y_rel n] is the row that says [n] weeks had nothing in them. *)
let sn_quiet ~y_rel n =
  El.div ~at:[At.class' "sn-quiet";
              At.v "style"
                (pos_style ~top:y_rel ~height:(Snake.height Snake.Quiet)
                 ^ ";" ^ text_style Snake.Release)]
    [El.span ~at:[At.class' "sn-quiet-text"]
       [El.txt (if n = 1 then "1 quiet week"
                else Printf.sprintf "%d quiet weeks" n)]]

(** [sn_season ~year ~month ~y0 ~height] is the strip of seasonal motifs beside
    the spine for a month that begins [y0] down the timeline. *)
let sn_season ~year ~month ~y0 ~height =
  let season = Snake.season_of_month month in
  let paths =
    Snake.season_paths ~month ~seed:((year * 12) + month) ~y0 ~height
    |> List.map (fun (s, filled, d) ->
         Printf.sprintf {|<path class="sn-s-%s %s" d="%s"/>|}
           (Snake.season_name s) (if filled then "sn-fl" else "sn-st") d)
  in
  El.unsafe_raw
    (Printf.sprintf
       {|<svg class="sn-season sn-season-%s" viewBox="0 0 %.2f %.2f" style="width:%.2fem;height:%.3fem" aria-hidden="true" focusable="false">%s</svg>|}
       (Snake.season_name season) Snake.season_width height Snake.season_width
       height (String.concat "" paths))

(** [sn_month_pill label] is the marker for a month, which sits on the spine. *)
let sn_month_pill label =
  El.h2 ~at:[At.class' "sn-pill";
             At.v "style"
               (Printf.sprintf "top:%.3fem" ((Snake.month_height /. 2.) -. 0.9))]
    [El.span [El.txt label]]

(** [group_releases rs] is the [(repository, releases)] of [rs], one per
    repository, with each repository's releases newest first. *)
let group_releases rs =
  let repos =
    List.sort_uniq String.compare
      (List.map (fun ((t : Bushel.Release.t), _) -> t.repo) rs)
  in
  List.map (fun repo ->
    let mine =
      List.filter (fun ((t : Bushel.Release.t), _) -> t.repo = repo) rs
    in
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
  (* Months run down the page, each a block of rows. [y] is how far down the
     timeline the next block begins. *)
  let y = ref 0. in
  let month_sections = List.map (fun (yr, mo) ->
    let notes =
      List.rev (try Hashtbl.find by_month (yr, mo) with Not_found -> []) in
    let releases =
      try Hashtbl.find releases_by_month (yr, mo) with Not_found -> [] in
    let section_id = Printf.sprintf "month-%04d-%02d" yr mo in
    let month_id = Printf.sprintf "%04d-%02d" yr mo in
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
    let month_top = !y in
    let cursor = ref Snake.month_height in
    let rows =
      List.concat_map (fun (key, group) ->
        let rows = List.map (fun (_, item) ->
          let kind, build =
            match item with
            | `Note n ->
              (Snake.Note,
               fun ~y_rel ~y_abs -> sn_note ~ctx ~y_rel ~y_abs n)
            | `Week n ->
              (Snake.Week,
               fun ~y_rel ~y_abs -> sn_week ~ctx ~y_rel ~y_abs n)
            | `Release (t, rs) ->
              (Snake.Release,
               fun ~y_rel ~y_abs -> sn_release ~y_rel ~y_abs t rs)
          in
          let y_rel = !cursor in
          cursor := !cursor +. Snake.height kind;
          build ~y_rel ~y_abs:(month_top +. y_rel)) group
        in
        match Hashtbl.find_opt quiet_after key with
        | Some q when q > 0 ->
          let y_rel = !cursor in
          cursor := !cursor +. Snake.height Snake.Quiet;
          rows @ [sn_quiet ~y_rel q]
        | _ -> rows) (by_week items)
    in
    let month_h = !cursor +. Snake.month_gap in
    y := month_top +. month_h;
    El.div ~at:[At.id section_id;
                At.v "data-month-id" month_id;
                At.class' "sn-month";
                At.v "style" (pos_style ~top:month_top ~height:month_h)]
      (sn_season ~year:yr ~month:mo ~y0:month_top ~height:month_h
       :: sn_month_pill (Printf.sprintf "%s %d" (Common.month_name_full mo) yr)
       :: rows)
  ) months in
  let total = !y in
  let spine =
    let d = Snake.spine_path ~height:total in
    El.unsafe_raw
      (Printf.sprintf
         {|<svg class="snake-spine" viewBox="0 0 %.2f %.2f" style="width:%.2fem;height:%.3fem" aria-hidden="true" focusable="false"><path class="snake-line" d="%s"/></svg>|}
         Snake.svg_width total Snake.svg_width total d)
  in
  let article =
    El.article ~at:[At.class' "h-feed"] [
      Common.hidden_feed_meta ~ctx "Notes";
      El.div ~at:[At.class' "snake notes-journal";
                  At.v "style" (Printf.sprintf "height:%.3fem" total)]
        (spine :: month_sections)]
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
