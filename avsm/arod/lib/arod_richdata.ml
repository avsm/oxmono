(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON-LD rich data for SEO *)

let jsonld j =
  Printf.sprintf {|<script type="application/ld+json">%s</script>|}
    (Ezjsonm.to_string (`O j))

let jsonlds j =
  Printf.sprintf {|<script type="application/ld+json">%s</script>|}
    (Ezjsonm.to_string (`A j))

type els = (string * string) list

let breadcrumbs_ld (els:els) =
  let elsj =
    List.mapi (fun i (name, item) ->
      let last = i = List.length els - 1 in
      `O ([
        "@type", `String "ListItem";
        "position", `String (string_of_int (i+1));
        "name", `String name ] @ (if last then [] else ["item", `String item]))
    ) els in
  [
    "@context", `String "https://schema.org";
    "@type", `String "BreadcrumbList";
    "itemListElement", `A elsj
  ]

let breadcrumbs els = jsonld @@ breadcrumbs_ld els

module MC = Sortal_schema.Contact
module MN = Bushel.Note
module MP = Bushel.Paper

let json_of_contact (c:MC.t) =
  `O ([
    "@type", `String "Person";
    "name", `String (MC.name c);
  ] @ (match MC.best_url c with None -> [] | Some c -> ["url", `String c]))

let date p = Ptime.to_rfc3339 p

let note_ld ~author ?(images=[]) (c:MN.t) =
  let x = [
    "@context", `String "https://schema.org";
    "@type", `String "NewsArticle";
    "headline", `String c.MN.title;
    "image", `A (List.map (fun i -> `String i) images);
    "datePublished", `String (date @@ MN.origdate c);
    "dateModified", `String (date @@ MN.datetime c);
    "abstract", `String (Option.value ~default:"" @@ MN.synopsis c);
    "author", `A [json_of_contact author]
  ] in
  match c.MN.via with
  | None -> x
  | Some (_,u) -> ("significantLink", `String u) :: x

let paper_ld ~ctx (p:MP.t) =
  let authors = MP.authors p |> List.filter_map (Arod_ctx.lookup_by_name ctx) in
  [
    "@context", `String "https://schema.org";
    "@type", `String "ScholarlyArticle";
    "pagination", `String (MP.pages p);
    "abstract", `String (MP.abstract p);
    "datePublished", `String (date @@ MP.datetime p);
    "publisher", `String (MP.publisher p);
    "url", `String (Option.value ~default:"" @@ MP.url p);
    "headline", `String (MP.title p);
    "author", `A (List.map json_of_contact authors)
  ]

let generic_ld ~ctx cfg e =
  let me = match Arod_ctx.lookup_by_handle ctx cfg.Arod_config.site.author_handle with
    | Some c -> c
    | None -> failwith "Author not found"
  in
  [
    "@context", `String "https://schema.org";
    "@type", `String "WebPage";
    "datePublished", `String (date @@ Bushel.Entry.datetime e);
    "author", `A [json_of_contact me];
    "abstract", `String (Option.value ~default:"" @@ Bushel.Entry.synopsis e)
  ]

let entry_ld ~ctx cfg e =
  let me = match Arod_ctx.lookup_by_handle ctx cfg.Arod_config.site.author_handle with
    | Some c -> c
    | None -> failwith "Author not found"
  in
  match e with
  | `Note n -> note_ld ~author:me n
  | `Paper p -> paper_ld ~ctx p
  | _ -> generic_ld ~ctx cfg e

let breadcrumb_of_ent cfg ent =
  ("Home", cfg.Arod_config.site.base_url ^ "/") ::
  ( match ent with
    | `Paper _ -> "Papers", (cfg.site.base_url ^ "/papers")
    | `Video _ -> "Videos", (cfg.site.base_url ^ "/videos")
    | `Idea _ -> "Ideas", (cfg.site.base_url ^ "/ideas")
    | `Project _ -> "Projects", (cfg.site.base_url ^ "/projects")
    | `Note _ -> "Notes", (cfg.site.base_url ^ "/notes")
  ) ::
  [Bushel.Entry.title ent, ""]

let json_of_entry ~ctx cfg ent =
  jsonld @@ entry_ld ~ctx cfg ent

let json_of_feed ~ctx cfg feed =
  match feed with
  | `Note (n, e) ->
    let me = match Arod_ctx.lookup_by_handle ctx cfg.Arod_config.site.author_handle with
      | Some c -> c
      | None -> failwith "Author not found"
    in
    let note_with_ent_ld = [
      "@context", `String "https://schema.org";
      "@type", `String "NewsArticle";
      "headline", `String (MN.title n);
      "image", `A [];
      "datePublished", `String (date @@ MN.datetime n);
      "author", `A [json_of_contact me];
      "mainEntity", `O (entry_ld ~ctx cfg e)
    ] in
    jsonld note_with_ent_ld
  | `Entry e -> jsonld @@ entry_ld ~ctx cfg e
