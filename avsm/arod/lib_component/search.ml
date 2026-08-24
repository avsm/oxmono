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

let kind_icon kind = Nav.filter_icon_for kind

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

let host url = S.host_of_url url

(* Every section shares one header anatomy so the tiers read as one
   system: the label names the tier, the note says how it is ordered,
   and the badge carries the total. *)
let section_head ?note ?total label =
  let note = match note with
    | Some n -> [El.span ~at:[At.class' "sp-note"] [El.txt n]]
    | None -> []
  in
  let badge = match total with
    | Some n -> [El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int n)]]
    | None -> []
  in
  El.div ~at:[At.class' "sp-sec-h"]
    ([ El.span ~at:[At.class' "sp-eyebrow"] [El.txt label] ] @ note @ badge)

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
      (List.map (fun t -> El.span [El.txt ("#" ^ t)]) (Common.take 5 ts))

let work_row ~terms (h : S.hit) =
  El.a ~at:[At.href h.url; At.class' ("sp-hit sp-work sp-k-" ^ h.kind)]
    [ El.span ~at:[At.class' ("sp-ic sp-ic-" ^ h.kind)] [kind_icon h.kind];
      El.span ~at:[At.class' "sp-body"]
        [ El.span ~at:[At.class' "sp-line"]
            [ El.span ~at:[At.class' "sp-t"] [mark ~terms h.title];
              El.span ~at:[At.class' "sp-d"] [El.txt h.date] ];
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
          let extra = if rest = [] then "" else
              Printf.sprintf " +%d" (List.length rest) in
          El.a ~at:[At.href (Bushel.Entry.site_url ent);
                    At.class' "sp-via"]
            [ El.span ~at:[At.class' "sp-via-in"] [El.txt "in "];
              El.txt (title ^ extra) ])
  in
  (* The row holds two destinations, the link and the entry citing it, so
     it is a div rather than one anchor: HTML forbids nesting them. The
     script treats [data-href] as the row's own destination. *)
  El.div ~at:[At.class' "sp-hit sp-link"; At.v "data-href" h.url]
    [ El.span ~at:[At.class' "sp-fav"] [fav];
      El.span ~at:[At.class' "sp-body"]
        [ El.span ~at:[At.class' "sp-line"]
            [ El.a ~at:[At.href h.url; At.class' "sp-t";
                        At.v "rel" "noopener"] [mark ~terms h.title];
              El.span ~at:[At.class' "sp-d"]
                [El.txt (String.sub h.date 0 (min 7 (String.length h.date)))] ];
          El.span ~at:[At.class' "sp-meta"]
            [ El.span ~at:[At.class' "sp-dom"] [El.txt (host h.url)]; via ] ] ]

(* Enough encoding for a query-string value: the characters that would
   end or split it. Tags and search words never need more. *)
let pct_query s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> match c with
    | '#' -> Buffer.add_string b "%23"
    | '&' -> Buffer.add_string b "%26"
    | '+' -> Buffer.add_string b "%2B"
    | '%' -> Buffer.add_string b "%25"
    | ' ' -> Buffer.add_string b "%20"
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

(* The toggle doubles as the ordering statement in the header: the active
   side says how the tiers below are sorted. Real links, so the choice
   works without the script, which intercepts them via [data-sort]. *)
let sort_toggle ~q ~(order : S.order) =
  let opt this label =
    let sort = match this with `Relevance -> "relevance" | `Date -> "date" in
    let cls = "sp-sort-opt" ^ (if order = this then " on" else "") in
    El.a ~at:[At.href ("/search?q=" ^ pct_query q ^ "&sort=" ^ sort);
              At.class' cls; At.v "data-sort" sort]
      [El.txt label]
  in
  El.span ~at:[At.class' "sp-sort"]
    [ opt `Relevance "relevance"; opt `Date "date" ]

let kind_label = function
  | "paper" -> "Papers" | "note" -> "Notes" | "project" -> "Projects"
  | "idea" -> "Ideas" | "video" -> "Talks" | "link" -> "Links"
  | k -> k

let has_filter ~q prefix v =
  List.mem (prefix ^ v) (String.split_on_char ' ' q)

let facets ~q (r : S.results) =
  let kind_chip (k, n) =
    let on = if has_filter ~q "kind:" k then " on" else "" in
    El.button ~at:[At.class' ("sp-f" ^ on); At.v "data-kind" k]
      [ El.txt (kind_label k); El.txt " ";
        El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int n)] ]
  in
  let tag_chip (t, n) =
    let on = if has_filter ~q "#" t then " on" else "" in
    El.button ~at:[At.class' ("sp-f" ^ on); At.v "data-tag" t]
      [ El.txt ("#" ^ t); El.txt " ";
        El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int n)] ]
  in
  if r.kinds = [] && r.tags = [] then []
  else
    [ El.div ~at:[At.class' "sp-facets"] (List.map kind_chip r.kinds);
      El.div ~at:[At.class' "sp-facets"] (List.map tag_chip r.tags) ]

(* A corrupt year outside this range would otherwise size the bar list, and
   a bad one such as 1 would render thousands of empty divs. *)
let histogram (r : S.results) =
  match List.filter (fun (y, _) -> y >= 1970 && y <= 2100) r.years with
  | [] -> El.void
  | years ->
    let lo = fst (List.hd years) and hi = fst (List.hd (List.rev years)) in
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
  let has_years =
    List.exists (fun (y, _) -> y >= 1970 && y <= 2100) r.years
  in
  let narrow =
    if chips = [] && not has_years then El.void
    else
      El.div ~at:[At.class' "sp-sec"]
        ([ section_head ~note:"filter these results" "Narrow" ]
         @ chips @ [ histogram r ])
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

let main_column ~q ~order (r : S.results) =
  let work = match r.work with
    | [] -> El.void
    | ws ->
      let head =
        El.div ~at:[At.class' "sp-sec-h"]
          [ El.span ~at:[At.class' "sp-eyebrow"] [El.txt "On this site"];
            sort_toggle ~q ~order;
            El.span ~at:[At.class' "sp-n"]
              [El.txt (string_of_int r.work_total)] ]
      in
      El.div ~at:[At.class' "sp-sec"]
        ([ head ]
         @ [ El.div ~at:[At.class' "sp-rows"]
               (List.map (work_row ~terms:r.terms) ws) ]
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
    else [ count; main_column ~q ~order r; rail ~ctx ~q r ]
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
  (El.div ~at:[At.class' "sp-page"] [ form; fragment ~ctx ~q ~order r ],
   El.void)
