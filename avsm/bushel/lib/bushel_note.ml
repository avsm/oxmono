(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Note entry type for Bushel *)

type social = Bushel_types.social

type t = {
  title : string;
  date : Ptime.date;
  slug : string;
  body : string;
  tags : string list;
  draft : bool;
  updated : Ptime.date option;
  sidebar : string option;
  index_page : bool;
  perma : bool;              (** Permanent article that will receive a DOI *)
  weeknote : bool;           (** Regular small update with ISO week numbering *)
  doi : string option;       (** DOI identifier for permanent articles *)
  synopsis : string option;
  titleimage : string option;
  via : (string * string) option;  (** (label, url) for link-style notes *)
  slug_ent : string option;  (** Reference to another entry *)
  source : string option;    (** Source for news-style notes *)
  url : string option;       (** External URL for news-style notes *)
  author : string option;    (** Author for news-style notes *)
  category : string option;  (** Category for news-style notes *)
  standardsite : string option;  (** Standards body site reference *)
  social : social option;    (** Discussion links on social platforms *)
  source_file : string option;  (** Original source file path (set by loader, not frontmatter) *)
}

type ts = t list

(** {1 Accessors} *)

let title { title; _ } = title
let slug { slug; _ } = slug
let body { body; _ } = body
let tags { tags; _ } = tags
let draft { draft; _ } = draft
let sidebar { sidebar; _ } = sidebar
let synopsis { synopsis; _ } = synopsis
let perma { perma; _ } = perma
let weeknote { weeknote; _ } = weeknote
let doi { doi; _ } = doi
let titleimage { titleimage; _ } = titleimage
let slug_ent { slug_ent; _ } = slug_ent
let source { source; _ } = source
let url { url; _ } = url
let author { author; _ } = author
let category { category; _ } = category
let standardsite { standardsite; _ } = standardsite
let social { social; _ } = social
let source_file { source_file; _ } = source_file

let origdate { date; _ } = Bushel_types.ptime_of_date_exn date

let date { date; updated; _ } =
  match updated with
  | None -> date
  | Some v -> v

let datetime v = Bushel_types.ptime_of_date_exn (date v)

let link { body; via; slug; _ } =
  match body, via with
  | "", Some (l, u) -> `Ext (l, u)
  | "", None -> failwith (slug ^ ": note external without via, via-url")
  | _, _ -> `Local slug

let words { body; _ } = Bushel_util.count_words body

(** {1 ISO 8601 Week Numbers} *)

(** ISO 8601 week number. Returns [(iso_year, week_number)] where
    [iso_year] may differ from calendar year at year boundaries. *)
let iso_week_number (y, m, d) =
  let is_leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0 in
  let month_days = [|0; 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|] in
  let doy = ref d in
  for i = 1 to m - 1 do
    doy := !doy + month_days.(i) + (if i = 2 && is_leap y then 1 else 0)
  done;
  let pt = Bushel_types.ptime_of_date_exn (y, m, d) in
  let wd = Ptime.weekday_num pt in
  let iso_wd = if wd = 0 then 7 else wd in
  let wk = (!doy - iso_wd + 10) / 7 in
  if wk < 1 then
    let prev_dec31 = Bushel_types.ptime_of_date_exn (y - 1, 12, 31) in
    let prev_wd = let w = Ptime.weekday_num prev_dec31 in if w = 0 then 7 else w in
    let prev_doy = if is_leap (y - 1) then 366 else 365 in
    let prev_wk = (prev_doy - prev_wd + 10) / 7 in
    (y - 1, prev_wk)
  else if wk > 52 then
    let total_days = if is_leap y then 366 else 365 in
    let last_wk = (total_days - iso_wd + 10) / 7 in
    if wk > last_wk then (y + 1, 1)
    else (y, wk)
  else (y, wk)

let week_number n = iso_week_number (date n)

(** [week_date_range n] returns the Monday-Sunday date range for this weeknote
    as [(mon_month, mon_day, sun_month, sun_day, sun_year)]. The weeknote date
    is assumed to be the end (Friday/Saturday/Sunday) of the week. We compute
    ISO week Monday from the note's date. *)
let week_date_range n =
  let (y, m, d) = date n in
  let pt = Bushel_types.ptime_of_date_exn (y, m, d) in
  (* Find Monday of this ISO week: weekday_num gives 0=Sun..6=Sat *)
  let wd = Ptime.weekday_num pt in
  let days_since_monday = match wd with
    | 0 -> 6 | n -> n - 1
  in
  let monday = Ptime.Span.of_int_s (- days_since_monday * 86400) in
  let mon_pt = Option.get (Ptime.add_span pt monday) in
  let sunday = Ptime.Span.of_int_s ((6 - days_since_monday) * 86400) in
  let sun_pt = Option.get (Ptime.add_span pt sunday) in
  let (mon_y, mon_m, mon_d) = Ptime.to_date mon_pt in
  let (sun_y, sun_m, sun_d) = Ptime.to_date sun_pt in
  (mon_y, mon_m, mon_d, sun_y, sun_m, sun_d)

let ordinal_suffix d = match d mod 10 with
  | 1 when d <> 11 -> "st"
  | 2 when d <> 12 -> "nd"
  | 3 when d <> 13 -> "rd"
  | _ -> "th"

let short_month = function
  | 1 -> "Jan" | 2 -> "Feb" | 3 -> "Mar" | 4 -> "Apr"
  | 5 -> "May" | 6 -> "Jun" | 7 -> "Jul" | 8 -> "Aug"
  | 9 -> "Sep" | 10 -> "Oct" | 11 -> "Nov" | 12 -> "Dec"
  | _ -> ""

(** Human-readable date range string for weeknotes.
    "Feb 3rd\xe2\x80\x937th" if same month, "Mar 28th\xe2\x80\x93Apr 4th" if straddling. *)
let week_date_range_string n =
  let (_, mon_m, mon_d, _, sun_m, sun_d) = week_date_range n in
  if mon_m = sun_m then
    Printf.sprintf "%s %d%s\xe2\x80\x93%d%s"
      (short_month mon_m) mon_d (ordinal_suffix mon_d) sun_d (ordinal_suffix sun_d)
  else
    Printf.sprintf "%s %d%s\xe2\x80\x93%s %d%s"
      (short_month mon_m) mon_d (ordinal_suffix mon_d)
      (short_month sun_m) sun_d (ordinal_suffix sun_d)

(** Find the previous and next weeknotes by week number in a sorted notes list.
    Returns [(prev_option, next_option)]. *)
let adjacent_weeknotes notes n =
  let weeknotes =
    List.filter (fun wn -> wn.weeknote) notes
    |> List.sort (fun a b ->
      let (ya, wa) = week_number a in
      let (yb, wb) = week_number b in
      compare (ya, wa) (yb, wb))
  in
  let (cur_y, cur_w) = week_number n in
  let rec find_adj prev = function
    | [] -> (prev, None)
    | wn :: rest ->
      let (wy, ww) = week_number wn in
      if wy = cur_y && ww = cur_w then
        let next = match rest with [] -> None | h :: _ -> Some h in
        (prev, next)
      else find_adj (Some wn) rest
  in
  find_adj None weeknotes

let weeknote_title n =
  let (yr, wk) = week_number n in
  Printf.sprintf ".plan-%02d-%02d: %s" (yr mod 100) wk (title n)

(** {1 Comparison} *)

let compare a b = Ptime.compare (datetime b) (datetime a)

(** {1 Lookup} *)

let lookup slug notes = List.find_opt (fun n -> n.slug = slug) notes

(** {1 Jsont Codec} *)

let via_jsont : (string * string) option Jsont.t =
  (* via is encoded as two separate fields: via and via-url *)
  Jsont.null None  (* Handled specially in of_frontmatter *)

let jsont ~default_date ~default_slug : t Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let make title date slug tags draft updated index_page perma weeknote doi synopsis titleimage
           slug_ent source url author category standardsite social =
    { title; date; slug; body = ""; tags; draft; updated; sidebar = None;
      index_page; perma; weeknote; doi; synopsis; titleimage; via = None;
      slug_ent; source; url; author; category; standardsite; social;
      source_file = None }
  in
  map ~kind:"Note" make
  |> mem "title" string ~enc:(fun n -> n.title)
  |> mem "date" Bushel_types.ptime_date_jsont ~dec_absent:default_date ~enc:(fun n -> n.date)
  |> mem "slug" string ~dec_absent:default_slug ~enc:(fun n -> n.slug)
  |> mem "tags" (list string) ~dec_absent:[] ~enc:(fun n -> n.tags)
  |> mem "draft" bool ~dec_absent:false ~enc:(fun n -> n.draft)
  |> mem "updated" (option Bushel_types.ptime_date_jsont) ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.updated)
  |> mem "index_page" bool ~dec_absent:false ~enc:(fun n -> n.index_page)
  |> mem "perma" bool ~dec_absent:false ~enc:(fun n -> n.perma)
  |> mem "weeknote" bool ~dec_absent:false ~enc:(fun n -> n.weeknote)
  |> mem "doi" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.doi)
  |> mem "synopsis" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.synopsis)
  |> mem "titleimage" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.titleimage)
  |> mem "slug_ent" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.slug_ent)
  |> mem "source" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.source)
  |> mem "url" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.url)
  |> mem "author" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.author)
  |> mem "category" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.category)
  |> mem "standardsite" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.standardsite)
  |> mem "social" (option Bushel_types.social_jsont) ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.social)
  |> finish

(** {1 Parsing} *)

let of_frontmatter (fm : Frontmatter.t) : (t, string) result =
  (* Extract slug and date from filename to use as defaults *)
  let default_slug, default_date =
    match Frontmatter.fname fm with
    | Some fname ->
      (match Frontmatter.slug_of_fname fname with
       | Ok (s, d) -> (s, Option.fold ~none:(1, 1, 1) ~some:Ptime.to_date d)
       | Error _ -> ("", (1, 1, 1)))
    | None -> ("", (1, 1, 1))
  in
  (* Get via fields manually since they're two separate fields *)
  let via =
    match Frontmatter.find_string "via" fm, Frontmatter.find_string "via-url" fm with
    | Some a, Some b -> Some (a, b)
    | None, Some b -> Some ("", b)
    | _ -> None
  in
  match Frontmatter.decode (jsont ~default_date ~default_slug) fm with
  | Error e -> Error e
  | Ok n ->
    let n = { n with body = Frontmatter.body fm; via; source_file = Frontmatter.fname fm } in
    if n.weeknote then
      (* Set date to Sunday of the week, and prepend weeknote prefix to title *)
      let (_, _, _, sun_y, sun_m, sun_d) = week_date_range n in
      Ok { n with date = (sun_y, sun_m, sun_d); title = weeknote_title n }
    else Ok n

(** {1 Pretty Printing} *)

let pp ppf n =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %a@," (styled `Bold string) "Type" (styled `Cyan string) "Note";
  pf ppf "%a: %a@," (styled `Bold string) "Slug" string (slug n);
  pf ppf "%a: %a@," (styled `Bold string) "Title" string (title n);
  let (year, month, day) = date n in
  pf ppf "%a: %04d-%02d-%02d@," (styled `Bold string) "Date" year month day;
  (match n.updated with
   | Some (y, m, d) -> pf ppf "%a: %04d-%02d-%02d@," (styled `Bold string) "Updated" y m d
   | None -> ());
  pf ppf "%a: %b@," (styled `Bold string) "Draft" (draft n);
  pf ppf "%a: %b@," (styled `Bold string) "Index Page" n.index_page;
  pf ppf "%a: %b@," (styled `Bold string) "Perma" (perma n);
  pf ppf "%a: %b@," (styled `Bold string) "Weeknote" (weeknote n);
  (match doi n with
   | Some d -> pf ppf "%a: %a@," (styled `Bold string) "DOI" string d
   | None -> ());
  (match synopsis n with
   | Some syn -> pf ppf "%a: %a@," (styled `Bold string) "Synopsis" string syn
   | None -> ());
  (match titleimage n with
   | Some img -> pf ppf "%a: %a@," (styled `Bold string) "Title Image" string img
   | None -> ());
  (match n.via with
   | Some (label, url) ->
     if label <> "" then
       pf ppf "%a: %a (%a)@," (styled `Bold string) "Via" string label string url
     else
       pf ppf "%a: %a@," (styled `Bold string) "Via" string url
   | None -> ());
  (match standardsite n with
   | Some site -> pf ppf "%a: %a@," (styled `Bold string) "Standard Site" string site
   | None -> ());
  let t = tags n in
  if t <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Tags" (list ~sep:comma string) t;
  (match sidebar n with
   | Some sb ->
     pf ppf "@,";
     pf ppf "%a:@," (styled `Bold string) "Sidebar";
     pf ppf "%a@," string sb
   | None -> ());
  let bd = body n in
  if bd <> "" then begin
    pf ppf "@,";
    pf ppf "%a:@," (styled `Bold string) "Body";
    pf ppf "%a@," string bd;
  end;
  pf ppf "@]"
