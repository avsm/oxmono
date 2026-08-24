(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The search page and its results fragment.

    The fragment is what [search.js] swaps in as the reader types, so it
    holds both columns under one id. The page wraps it in the form a
    browser without JavaScript submits. *)

open Htmlit
module I = Arod.Icons
module S = Arod_search

let kind_paths = function
  | "paper" -> I.paper_o
  | "note" -> I.note_o
  | "weekly" -> I.calendar_o
  | "video" -> I.video_o
  | "project" -> I.folder_o
  | "idea" -> I.bulb_o
  | "link" -> I.link_o
  | _ -> I.tag_o

let kind_icon ?(size = 14) kind =
  El.unsafe_raw (I.outline ~size (kind_paths kind))

let favicon_for ~ctx url =
  match Arod.Ctx.link_for_url ctx url with
  | Some (l : Bushel.Link.t) -> (
    let meta = match l.karakeep with Some k -> k.metadata | None -> [] in
    match List.assoc_opt "favicon" meta with
    | Some f when f <> "" -> Some f
    | _ -> None)
  | None -> None

(* Words are split on spaces only, so punctuation stays attached and the
   title reads as written. A word is marked when its lowercase form starts
   with a term, which is the prefix match the index performs. *)
let mark ~terms title =
  let words = String.split_on_char ' ' title in
  let marked w =
    let lw = String.lowercase_ascii w in
    List.exists (fun t -> t <> "" && String.starts_with ~prefix:t lw) terms
  in
  let rec go acc = function
    | [] -> List.rev acc
    | [ w ] -> go ((if marked w then El.b [ El.txt w ] else El.txt w) :: acc) []
    | w :: rest ->
      let el = if marked w then El.b [ El.txt w ] else El.txt w in
      go (El.txt " " :: el :: acc) rest
  in
  El.splice (go [] words)

(* "26th Oct 2026" rather than an ISO stamp: a result list is read, not
   collated. *)
let ordinal d =
  if d mod 100 >= 11 && d mod 100 <= 13 then "th"
  else match d mod 10 with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"

let pretty_date date =
  match String.split_on_char '-' date with
  | y :: m :: d :: _ -> (
    match int_of_string_opt m, int_of_string_opt d with
    | Some m, Some d when m >= 1 && m <= 12 ->
      Printf.sprintf "%d%s %s %s" d (ordinal d) (Common.month_name m) y
    | _ -> date)
  | _ -> date

let pretty_month date =
  match String.split_on_char '-' date with
  | y :: m :: _ -> (
    match int_of_string_opt m with
    | Some m when m >= 1 && m <= 12 ->
      Printf.sprintf "%s %s" (Common.month_name m) y
    | _ -> date)
  | _ -> date

(* Every section shares one header anatomy so the tiers read as one
   system: the label names the tier, the note says how it is ordered,
   and the badge carries the total. *)
let section_head ?note ?mid ?total label =
  let note = match note with
    | Some n -> [El.span ~at:[At.class' "sp-note"] [El.txt n]]
    | None -> []
  in
  let mid = Option.value ~default:[] mid in
  let badge = match total with
    | Some n -> [El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int n)]]
    | None -> []
  in
  El.div ~at:[At.class' "sp-sec-h"]
    ([ El.span ~at:[At.class' "sp-eyebrow"] [El.txt label] ]
     @ note @ mid @ badge)

let more ~shown ~total ~param =
  if total > shown then
    El.button ~at:[At.class' "sp-more"; At.v "data-more" param]
      [ El.txt (Printf.sprintf "Show %d more" (total - shown)) ]
  else El.void

let goto_chip (g : S.goto) =
  let icon = match g.goto_kind with
    | `Section -> El.unsafe_raw (I.outline ~size:14 I.home_o)
    | `Project -> kind_icon "project"
    | `Tag -> El.unsafe_raw (I.outline ~size:14 I.tag_o)
  in
  let label = match g.goto_kind with
    | `Tag -> "#" ^ g.label
    | _ -> g.label
  in
  El.a ~at:[At.href g.url; At.class' "sp-hit sp-goto"]
    [ El.span ~at:[At.class' "sp-ic"] [icon];
      El.span ~at:[At.class' "sp-t"] [El.txt label];
      El.span ~at:[At.class' "sp-sub"] [El.txt g.detail] ]

let goto_section (r : S.results) =
  match r.goto with
  | [] -> El.void
  | gs ->
    El.div ~at:[At.class' "sp-sec"]
      [ section_head ~note:"pages this query names" "Go to";
        El.div ~at:[At.class' "sp-gotos"] (List.map goto_chip gs) ]

let tags_el tags =
  match tags with
  | [] -> El.void
  | ts ->
    El.span ~at:[At.class' "sp-tags"]
      (List.map
         (fun t ->
           El.span ~at:[At.class' "sp-tag"; At.v "data-tag" t]
             [El.txt ("#" ^ t)])
         (Common.take 5 ts))

let work_row ~ctx ~terms (h : S.hit) =
  (* One media block on the left: the entry's image washed into the
     background with the kind icon as a corner badge, or the icon alone,
     larger, when the entry has no image. Hovering the row brings the
     image to full colour. *)
  let thumb = match Arod.Ctx.lookup ctx h.slug with
    | None -> None
    | Some ent -> Bushel.Entry.thumbnail (Arod.Ctx.entries ctx) ent
  in
  let media = match thumb with
    | Some src ->
      El.span ~at:[At.class' ("sp-media sp-ic-" ^ h.kind)]
        [ El.img ~at:[At.src src; At.alt ""; At.v "loading" "lazy"] ();
          El.span ~at:[At.class' "sp-media-badge"]
            [kind_icon ~size:12 h.kind] ]
    | None ->
      El.span ~at:[At.class' ("sp-media sp-media-solo sp-ic-" ^ h.kind)]
        [ kind_icon ~size:22 h.kind ]
  in
  El.a ~at:[At.href h.url; At.class' "sp-hit sp-work"]
    [ media;
      El.span ~at:[At.class' "sp-body"]
        [ El.span ~at:[At.class' "sp-line"]
            [ El.span ~at:[At.class' "sp-t"] [mark ~terms h.title];
              El.span ~at:[At.class' "sp-d"] [El.txt (pretty_date h.date)] ];
          (if h.snippet = "" then El.void
           else El.span ~at:[At.class' "sp-snip"] [El.unsafe_raw h.snippet]);
          tags_el h.tags ] ]

let link_row ~ctx ~terms (h : S.hit) =
  let fav = match favicon_for ~ctx h.url with
    | Some src ->
      El.img ~at:[At.src src; At.alt ""; At.width 16; At.v "height" "16";
                  At.v "loading" "lazy"] ()
    | None -> kind_icon "link"
  in
  let via = match h.parent_slugs with
    | [] -> El.void
    | slug :: rest -> (
      (* An unresolved slug renders no via at all rather than a dangling
         "in ". A resolved one links to the citing entry, so the row
         offers both ends of the citation. *)
      match Arod.Ctx.lookup ctx slug with
      | None -> El.void
      | Some ent ->
        let title = Bushel.Entry.title ent in
        if title = "" then El.void
        else
          let kind = match ent with
            | `Note n when Bushel.Note.weeknote n -> "weekly"
            | _ -> Bushel.Entry.to_type_string ent
          in
          let extra = if rest = [] then "" else
              Printf.sprintf " +%d" (List.length rest) in
          El.splice
            [ El.span ~at:[At.class' "sp-via-in"] [El.txt " in "];
              El.a ~at:[At.href (Bushel.Entry.site_url ent);
                        At.class' ("sp-via sp-ic-" ^ kind)]
                [ kind_icon ~size:11 kind;
                  El.txt (" " ^ title ^ extra) ] ])
  in
  (* The row holds two destinations, the link and the entry citing it, so
     it is a div rather than one anchor: HTML forbids nesting them. The
     script treats [data-href] as the row's own destination. *)
  El.div ~at:[At.class' "sp-hit sp-link"; At.v "data-href" h.url]
    [ El.span ~at:[At.class' "sp-fav"] [fav];
      El.span ~at:[At.class' "sp-body"]
        [ El.span ~at:[At.class' "sp-line"]
            [ El.a ~at:[At.href h.url; At.class' "sp-t";
                        At.title h.title; At.v "rel" "noopener"]
                [mark ~terms h.title];
              El.span ~at:[At.class' "sp-d"]
                [El.txt (pretty_month h.date)] ];
          (* One sentence: "host in <icon> title", wrapping freely. *)
          El.span ~at:[At.class' "sp-meta"]
            [ El.span ~at:[At.class' "sp-dom"] [El.txt (S.host_of_url h.url)];
              via ] ] ]

(* The toggle doubles as the ordering statement in the header: the active
   side says how the tiers below are sorted. Real links, so the choice
   works without the script, which intercepts them via [data-sort]. *)
let sort_toggle ~q ~(order : S.order) =
  let opt this label =
    let sort = match this with `Relevance -> "relevance" | `Date -> "date" in
    let cls = "sp-sort-opt" ^ (if order = this then " on" else "") in
    El.a ~at:[At.href ("/search?q="
                        ^ Uriz.pct_encode ~component:`Query_value q
                        ^ "&sort=" ^ sort);
              At.class' cls; At.v "data-sort" sort]
      [El.txt label]
  in
  El.span ~at:[At.class' "sp-sort"]
    [ opt `Relevance "relevance"; opt `Date "date" ]

let kind_label = function
  | "paper" -> "Papers" | "note" -> "Notes" | "weekly" -> "Weeklies"
  | "project" -> "Projects" | "idea" -> "Ideas" | "video" -> "Talks"
  | "link" -> "Links" | k -> k

let has_filter ~words prefix v =
  List.mem (prefix ^ v) words

let facets ~q (r : S.results) =
  let words = String.split_on_char ' ' q in
  let kind_chip (k, n) =
    let on = if has_filter ~words "kind:" k then " on" else "" in
    El.button ~at:[At.class' ("sp-f" ^ on); At.v "data-kind" k]
      [ El.txt (kind_label k); El.txt " ";
        El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int n)] ]
  in
  let tag_chip (t, n) =
    let on = if has_filter ~words "#" t then " on" else "" in
    El.button ~at:[At.class' ("sp-f" ^ on); At.v "data-tag" t]
      [ El.txt ("#" ^ t); El.txt " ";
        El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int n)] ]
  in
  let row chip = function
    | [] -> []
    | xs -> [ El.div ~at:[At.class' "sp-facets"] (List.map chip xs) ]
  in
  row kind_chip r.kinds @ row tag_chip r.tags

(* A corrupt year outside this range would otherwise size the bar list, and
   a bad one such as 1 would render thousands of empty divs. [years] is
   already filtered to that range by the caller. *)
let histogram years =
  match years with
  | [] -> El.void
  | years ->
    let hi = fst (List.hd (List.rev years)) in
    (* A one-year match would otherwise draw one huge bar. Padding the
       span to a decade keeps a sparse result readable as a timeline. *)
    let lo = min (fst (List.hd years)) (hi - 9) in
    let max_n = List.fold_left (fun m (_, n) -> max m n) 1 years in
    let bars = List.init (hi - lo + 1) (fun i ->
      let y = lo + i in
      let n = Option.value ~default:0 (List.assoc_opt y years) in
      let cls = if n = max_n then "sp-year hot" else "sp-year" in
      let label = if y = lo || y = hi then [El.span [El.txt (string_of_int y)]]
        else [] in
      let pct = max 4 (100 * n / max_n) in
      El.div ~at:[At.class' cls;
                  At.style (Printf.sprintf "height:%d%%" pct);
                  At.title (Printf.sprintf "%d: %d" y n)] label)
    in
    El.div ~at:[At.class' "sp-years"] bars

let rail ~ctx ~q (r : S.results) =
  let chips = facets ~q r in
  let years = List.filter (fun (y, _) -> y >= 1970 && y <= 2100) r.years in
  let narrow =
    if chips = [] && years = [] then El.void
    else
      El.div ~at:[At.class' "sp-sec"]
        ([ section_head ~note:"filter these results" "Narrow" ]
         @ chips @ [ histogram years ])
  in
  let links = match r.links with
    | [] -> El.void
    | ls ->
      El.div ~at:[At.class' "sp-sec sp-links"]
        ([ section_head ~note:"cited from entries here"
             ~total:r.links_total "Links" ]
         @ [ El.div ~at:[At.class' "sp-rows"]
               (List.map (link_row ~ctx ~terms:r.terms) ls) ]
         @ [ more ~shown:(List.length ls) ~total:r.links_total
               ~param:"link_limit" ])
  in
  El.aside ~at:[At.class' "sp-rail"] [ narrow; links ]

let main_column ~ctx ~q ~order (r : S.results) =
  let work = match r.work with
    | [] -> El.void
    | ws ->
      let head =
        section_head ~mid:[ sort_toggle ~q ~order ] ~total:r.work_total
          "On this site"
      in
      El.div ~at:[At.class' "sp-sec"]
        ([ head ]
         @ [ El.div ~at:[At.class' "sp-rows"]
               (List.map (work_row ~ctx ~terms:r.terms) ws) ]
         @ [ more ~shown:(List.length ws) ~total:r.work_total ~param:"limit" ])
  in
  El.div ~at:[At.class' "sp-main"] [ goto_section r; work ]

let empty_state ~q =
  let msg =
    if q = "" then
      "Type to search. Results group by how close they are to this site: \
       pages and tags first, then papers, notes, projects, ideas and talks, \
       then the links they cite."
    else Printf.sprintf "Nothing matches \"%s\"." q
  in
  El.div ~at:[At.class' "sp-empty"] [El.txt msg]

let fragment ~ctx ~q ~order (r : S.results) =
  let count =
    El.div ~at:[At.class' "sp-count"]
      [ El.txt (Printf.sprintf "%d on this site · %d links"
                  r.work_total r.links_total) ]
  in
  let body =
    if r.goto = [] && r.work = [] && r.links = [] then [ empty_state ~q ]
    else [ count; main_column ~ctx ~q ~order r; rail ~ctx ~q r ]
  in
  El.div ~at:[At.id "search-results"; At.class' "sp-grid"] body

let page_body ~ctx ~q ~order r =
  let placeholder = "Search papers, notes, projects, links" in
  let form =
    El.form ~at:[At.action "/search"; At.method' "get"; At.class' "sp-form";
                 At.v "role" "search"]
      [ El.span ~at:[At.class' "sp-prompt"] [El.txt ">_"];
        El.input ~at:([ At.id "search-page-input"; At.type' "search";
                        At.name "q"; At.value q; At.autocomplete "off";
                        At.v "placeholder" placeholder ]
                      @ (if q = "" then [At.autofocus] else [])) ();
        El.span ~at:[At.id "search-spinner"; At.class' "sp-spin";
                     At.v "aria-hidden" "true"] [] ]
  in
  El.div ~at:[At.class' "sp-page"] [ form; fragment ~ctx ~q ~order r ]
