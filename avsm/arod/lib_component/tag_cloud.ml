(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The tag cloud. A tag has an icon if one was drawn for it. Some have a
    coloured illustration, shown in tones of the text colour until it is
    pointed at. Others have a line icon, and the rest are words alone. *)

open Htmlit

(** [line_icon tag] is the inside of the 24 pixel line icon of [tag], if it has
    one. They share one grid and one stroke, so they sit together. *)
let line_icon = function
  | "academia" -> Some {|<path d="M2 9.5L12 5l10 4.5L12 14z"/><path d="M6 11.7V16c0 1.4 2.7 3 6 3s6-1.6 6-3v-4.3"/><path d="M22 9.5V15"/>|}
  | "ai" -> Some {|<path d="M10 4l1.6 4.4L16 10l-4.4 1.6L10 16l-1.6-4.4L4 10l4.4-1.6z"/><path d="M18 14l.8 2.2L21 17l-2.2.8L18 20l-.8-2.2L15 17l2.2-.8z"/>|}
  | "biodiversity" -> Some {|<path d="M12 8c-1.6-3.6-5.4-4.4-6.8-2.6S5.6 10 9 11.2c-3 .9-3.9 3.6-2.4 5.2s4.2.2 5.4-3.2"/><path d="M12 8c1.6-3.6 5.4-4.4 6.8-2.6S18.4 10 15 11.2c3 .9 3.9 3.6 2.4 5.2s-4.2.2-5.4-3.2"/><path d="M12 7v12"/>|}
  | "brain" -> Some {|<path d="M12 5.5a3 3 0 00-5.5-1.2A3.5 3.5 0 004 9.5a3.5 3.5 0 001 5.5A3 3 0 009 19a3 3 0 003 1.5z"/><path d="M12 5.5a3 3 0 015.5-1.2A3.5 3.5 0 0120 9.5a3.5 3.5 0 01-1 5.5A3 3 0 0115 19a3 3 0 01-3 1.5z"/><path d="M12 5.5v15"/>|}
  | "cambridge" -> Some {|<path d="M2.5 9.5h19"/><path d="M5 9.5v10M12 9.5v10M19 9.5v10"/><path d="M5 14.5a3.5 3.5 0 007 0M12 14.5a3.5 3.5 0 007 0"/><path d="M2.5 19.5h19"/><path d="M12 5.5v4M10 5.5h4"/>|}
  | "climate" -> Some {|<path d="M10 14.2V5.5a2 2 0 014 0v8.7a4 4 0 11-4 0z"/><path d="M12 9v7.5"/><path d="M17.5 7h3M17.5 10h2"/>|}
  | "compsci" -> Some {|<rect x="3" y="4" width="18" height="16" rx="2"/><path d="M7.5 9.5l3 2.5-3 2.5M12.5 15h4"/>|}
  | "conference" -> Some {|<rect x="9" y="3" width="6" height="11" rx="3"/><path d="M5.5 11a6.5 6.5 0 0013 0M12 17.5V21M9 21h6"/>|}
  | "conservation" -> Some {|<path d="M6 18c0-7.5 5-12.5 13-12.5 0 8-4.5 12.5-11 12.5z"/><path d="M5 20l8-8"/>|}
  | "eio" -> Some {|<path d="M4 8h12M12.5 4.5L16 8l-3.5 3.5"/><path d="M20 16H8M11.5 12.5L8 16l3.5 3.5"/>|}
  | "embedded" -> Some {|<rect x="6.5" y="6.5" width="11" height="11" rx="1.6"/><path d="M9.5 3v3.5M14.5 3v3.5M9.5 17.5V21M14.5 17.5V21M3 9.5h3.5M3 14.5h3.5M17.5 9.5H21M17.5 14.5H21"/><circle cx="12" cy="12" r="1.6"/>|}
  | "esp32" -> Some {|<path d="M2.5 9a14 14 0 0119 0"/><path d="M5.5 12.5a9.5 9.5 0 0113 0"/><path d="M8.5 16a5 5 0 017 0"/><path d="M12 19.5h.01"/>|}
  | "evidence" -> Some {|<path d="M7 3h7l5 5v13H7z"/><path d="M14 3v5h5"/><path d="M10 14.5l2 2 3.5-3.8"/>|}
  | "functional" -> Some {|<path d="M6 4h2.5c1.5 0 2.3.8 2.9 2.2L17.5 20"/><path d="M11.5 12.5L6 20"/>|}
  | "landuse" -> Some {|<rect x="3" y="4.5" width="18" height="15" rx="1.6"/><path d="M3 10h18M11 10v9.5M3 15h8M16 10v9.5"/>|}
  | "life" -> Some {|<path d="M12 21v-9"/><path d="M12 13c0-4 3-6.5 7.5-6.5 0 4.2-3 6.5-7.5 6.5z"/><path d="M12 15.5c0-3-2.2-5-6.2-5 0 3 2.2 5 6.2 5z"/>|}
  | "llms" -> Some {|<path d="M4 5h16v11h-9.5L6.5 20v-4H4z"/><path d="M8.5 10.5h.01M12 10.5h.01M15.5 10.5h.01"/>|}
  | "nature" -> Some {|<path d="M12 21v-5"/><path d="M12 16c-4 0-6.2-2.3-6.2-5 0-1.9 1.4-3.1 3.2-3.2.1-3 1.5-5 3-5s2.9 2 3 5c1.8.1 3.2 1.3 3.2 3.2 0 2.7-2.2 5-6.2 5z"/>|}
  | "ocaml" -> Some {|<path d="M3 16c0-3.2 1.4-5.5 3.5-5.5 1.8 0 2.4 1.7 4 1.7s2.3-3.2 4.5-3.2c2.4 0 3.5 2.3 3.5 4.8"/><path d="M18 11.2c.4-2.2 1.4-3.7 3-4.2"/><path d="M6 16v4M10 16v4M15 16v4M18.5 15v5"/>|}
  | "oxcaml" -> Some {|<path d="M3.5 5.5c0 3.8 2 6 4.5 6h8c2.5 0 4.5-2.2 4.5-6"/><path d="M8 11.5V15a4 4 0 008 0v-3.5"/><path d="M10.5 15.8h.01M13.5 15.8h.01"/>|}
  | "packaging" -> Some {|<path d="M12 3l8 4.5v9L12 21l-8-4.5v-9z"/><path d="M4 7.5l8 4.5 8-4.5M12 12v9"/>|}
  | "policy" -> Some {|<path d="M3 9l9-5 9 5z"/><path d="M5.5 9.5v8M9.5 9.5v8M14.5 9.5v8M18.5 9.5v8"/><path d="M3 20.5h18"/>|}
  | "programming" -> Some {|<path d="M8 7L3 12l5 5M16 7l5 5-5 5M14 4.5l-4 15"/>|}
  | "research" -> Some {|<circle cx="10.5" cy="10.5" r="6.5"/><path d="M15.5 15.5L21 21"/><path d="M8 10.5h5M10.5 8v5"/>|}
  | "satellite" -> Some {|<path d="M3.5 8.5l4-4 3 3-4 4zM13.5 18.5l4-4 3 3-4 4z"/><path d="M9.5 9.5l5 5"/><path d="M8.5 16a4.5 4.5 0 004 4M5.5 15.5a7.5 7.5 0 007 7"/>|}
  | "scotland" -> Some {|<rect x="3.5" y="5" width="17" height="14" rx="1.6"/><path d="M3.5 5L20.5 19M20.5 5L3.5 19"/>|}
  | "sdms" -> Some {|<path d="M3.5 6.5l5.5-2.5 6 2.5 5.5-2.5v13.5l-5.5 2.5-6-2.5-5.5 2.5z"/><path d="M9 4v13.5M15 6.5V20"/>|}
  | "security" -> Some {|<path d="M12 3l7 3v5.5c0 4.3-3 7.8-7 9.5-4-1.7-7-5.2-7-9.5V6z"/><path d="M9 12l2.2 2.2L15.2 10"/>|}
  | "selfhosting" -> Some {|<rect x="4" y="4" width="16" height="6.5" rx="1.6"/><rect x="4" y="13.5" width="16" height="6.5" rx="1.6"/><path d="M7.5 7.2h.01M7.5 16.8h.01"/><path d="M11.5 7.2h5M11.5 16.8h5"/>|}
  | "sensing" -> Some {|<circle cx="12" cy="12" r="1.5"/><path d="M8.6 8.6a4.8 4.8 0 000 6.8M15.4 8.6a4.8 4.8 0 010 6.8"/><path d="M5.8 5.8a8.8 8.8 0 000 12.4M18.2 5.8a8.8 8.8 0 010 12.4"/>|}
  | "spatial" -> Some {|<path d="M12 21s7-6.1 7-11.2a7 7 0 10-14 0C5 14.9 12 21 12 21z"/><circle cx="12" cy="10" r="2.4"/>|}
  | "systems" -> Some {|<rect x="5" y="3" width="14" height="18" rx="2"/><rect x="7.5" y="5.5" width="9" height="7" rx="1"/><path d="M8 17h5M16 17h.01"/>|}
  | "teaching" -> Some {|<rect x="3" y="4" width="18" height="12" rx="1.6"/><path d="M8 20h8M12 16v4"/><path d="M7 8.5h6M7 11.5h4"/>|}
  | "tessera" -> Some {|<rect x="4" y="4" width="7" height="7" rx="1.2"/><rect x="13" y="4" width="7" height="7" rx="1.2"/><rect x="4" y="13" width="7" height="7" rx="1.2"/><path d="M16.5 13l3.5 3.5-3.5 3.5-3.5-3.5z"/>|}
  | "weather" -> Some {|<path d="M7.5 18.5a4.5 4.5 0 01-.6-8.960A6 6 0 0118.5 10.5a4 4 0 01-.5 8z"/><path d="M9 21.5v-1M13 21.5v-1M17 21.5v-1"/>|}
  | "zarr" -> Some {|<rect x="3.5" y="3.5" width="17" height="17" rx="2"/><path d="M3.5 12h17M12 3.5v17"/><path d="M7.2 7.8h.01M16.5 16.5h.01"/>|}
  | _ -> None

(** [counts ~ctx ~months] is the plain and set tags of the notes of the last
    [months] months, with how many notes carry each, most common first and then
    alphabetically. The months are counted back from the newest note. All the
    notes count if [months] is 0. *)
let counts ~ctx ~months =
  let notes = Arod.Ctx.notes ctx in
  let index n =
    let (y, m, _) = Bushel.Entry.date (`Note n) in
    (y * 12) + m
  in
  let newest = List.fold_left (fun acc n -> max acc (index n)) 0 notes in
  let recent n = months = 0 || index n > newest - months in
  let table = Hashtbl.create 64 in
  List.iter (fun n ->
    if recent n then
      List.iter (function
        | (`Text _ | `Set _) as t ->
          let k = Bushel.Tags.to_raw_string t in
          Hashtbl.replace table k
            (1 + Option.value (Hashtbl.find_opt table k) ~default:0)
        | _ -> ()) (Bushel.Entry.tags_of_ent (`Note n))) notes;
  Hashtbl.fold (fun k c acc -> (k, c) :: acc) table []
  |> List.sort (fun (a, ca) (b, cb) ->
       let c = compare cb ca in if c <> 0 then c else String.compare a b)

(** [search_url ?kind tag] is the search page for the entries tagged [tag],
    only those of [kind] if it is given, such as ["paper"]. *)
let search_url ?kind tag =
  let q = "#" ^ tag ^ (match kind with Some k -> " kind:" ^ k | None -> "") in
  "/search?q=" ^ Uriz.pct_encode ~component:`Query_value q

(** [symbol_id tag] is the id of the sprite symbol of [tag]. *)
let symbol_id tag =
  "ta-" ^ String.map (fun c ->
    match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> c | _ -> '_') tag

(* [between s a b] is the text of [s] after the first [a] and before the next
   [b] after it. *)
let between s a b =
  let find sub from =
    let n = String.length sub in
    let rec go i =
      if i + n > String.length s then None
      else if String.sub s i n = sub then Some i
      else go (i + 1)
    in
    go from
  in
  match find a 0 with
  | None -> None
  | Some i ->
    let start = i + String.length a in
    Option.map (fun k -> String.sub s start (k - start)) (find b start)

(** [alias tag] is the tag whose picture [tag] shares. A few tags are another
    spelling of one. *)
let alias = function
  | "llm" -> "llms"
  | "carbon-credits" | "carbon" -> "carboncredits"
  | "forest" -> "forests"
  | "network" | "networks" -> "networking"
  | "packages" -> "packaging"
  | "distributed-systems" -> "distributed"
  | "programming-languages" -> "programming"
  | "effects" -> "effect-handlers"
  | "aoh" -> "aoah"
  | tag -> tag

(** The address of the sprite that holds every illustration. It is one file
    for the whole site, so that a picture used a thousand times is sent once
    and cached. *)
let sprite_url = "/tag-art.svg"

(** [sprite_doc files] is the sprite of [files], which are each the name of a
    tag and its illustration. A layer takes its fill from a custom property
    that the page sets, because the sprite is a file of its own and the style
    sheet of the page does not reach into it. *)
let sprite_doc files =
  let layer = function
    | "ink" -> "currentColor"
    | c -> Printf.sprintf "var(--tf-%s)" c
  in
  let symbol (tag, svg) =
    match (between svg {|viewBox="|} {|"|}, between svg ">" "</svg>") with
    | Some vb, Some inner ->
      let inner =
        List.fold_left (fun acc c ->
          let cls = Printf.sprintf {|class="c-%s"|} c in
          let b = Buffer.create (String.length acc) in
          let n = String.length cls in
          let i = ref 0 in
          while !i < String.length acc do
            if !i + n <= String.length acc && String.sub acc !i n = cls then (
              Buffer.add_string b
                (Printf.sprintf {|style="fill:%s"|} (layer c));
              i := !i + n)
            else (
              Buffer.add_char b acc.[!i];
              incr i)
          done;
          Buffer.contents b) inner [ "ink"; "green"; "amber"; "blue"; "coral"; "tan" ]
      in
      Some (Printf.sprintf {|<symbol id="%s" viewBox="%s">%s</symbol>|}
              (symbol_id tag) vb inner)
    | _ -> None
  in
  {|<svg xmlns="http://www.w3.org/2000/svg">|}
  ^ String.concat "" (List.filter_map symbol files)
  ^ "</svg>"

(** [icon ~art tag] is the icon of [tag]: its coloured illustration if it has
    one, which is drawn from the sprite, else its line icon, else its initial
    in a circle. They are all one square, sized by the page that shows them,
    and drawn with the same outline weight as the illustrations. *)
let icon ~art tag =
  match art tag with
  | Some _ ->
    El.unsafe_raw
      (Printf.sprintf
         {|<svg class="ti col" viewBox="0 0 200 200" aria-hidden="true" focusable="false"><use href="%s#%s"/></svg>|}
         sprite_url (symbol_id (alias tag)))
  | None ->
    (match line_icon tag with
     | Some inner ->
       El.unsafe_raw
         ({|<svg class="ti line" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="0.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">|}
          ^ inner ^ "</svg>")
     | None ->
       El.span ~at:[At.class' "ti mono"; At.v "aria-hidden" "true"]
         [El.txt (String.uppercase_ascii (String.sub tag 0 1))])

(** [icon_link ~art ~noun ~count tag] is the small icon of [tag] as a link to a
    search for it. Its tooltip and label give the tag and how many of the
    things called [noun] carry it. *)
let icon_link ?kind ~art ~noun ~count tag =
  let tip =
    Printf.sprintf "%s, %d %s%s" tag count noun (if count = 1 then "" else "s")
  in
  El.a ~at:[At.href (search_url ?kind tag); At.v "data-tag" tag;
            At.class' "sn-ico no-underline"; At.v "title" tip;
            At.v "aria-label" tip]
    [icon ~art tag]

(** [tile ~art (tag, count)] is the tile of [tag]. Every tile is the same
    size, so the icons sit evenly. *)
let tile ~art (tag, count) =
  El.a ~at:[At.href (search_url tag); At.class' "tc-tag no-underline";
            At.v "title"
              (Printf.sprintf "%d note%s" count
                 (if count = 1 then "" else "s"))]
    [icon ~art tag;
     El.span ~at:[At.class' "tc-name"] [El.txt tag];
     El.span ~at:[At.class' "tc-count"] [El.txt (string_of_int count)]]

(** [page ~ctx ~art ~months] is the tag page for the last [months] months of
    notes. [art tag] is the coloured illustration of [tag], as an svg, if it
    has one. The tags are in a grid of equal tiles, most common first, each
    with how many notes carry it. *)
let page ~ctx ~art ~months =
  let tags = counts ~ctx ~months in
  El.article [
    El.h1 ~at:[At.class' "page-title text-xl font-semibold mb-2"]
      [El.txt "Tags"];
    El.p ~at:[At.class' "text-sm text-secondary mb-6"]
      [El.txt
         (if months = 0 then "The tags of every note."
          else Printf.sprintf "The tags of the notes of the last %d months." months);
       El.txt " Point at one to see its colours."];
    El.div ~at:[At.class' "tag-tiles"] (List.map (tile ~art) tags)]
