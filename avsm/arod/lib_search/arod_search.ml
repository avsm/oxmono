(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** FTS5 full-text search index for Arod content.

    Uses one FTS5 table per entry kind (paper, note, project, idea, video,
    link) so that kind filtering is a simple matter of which tables to
    query. A search ranks matches into a work tier and a links tier, each
    sorted, cut to a limit and counted before the cut. *)

module StringSet = Set.Make(String)

type t = {
  db : Sqlite3_eio.t;
  mutable own_host : string;
  mutable tag_counts : (string * int) list;
  mutable projects : (string * string * string * string) list;
}

type goto_kind = [ `Section | `Project | `Tag ]

type goto = {
  label : string;
  url : string;
  detail : string;
  goto_kind : goto_kind;
}

type hit = {
  slug : string;
  kind : string;
  url : string;
  title : string;
  snippet : string;
  date : string;
  tags : string list;
  parent_slugs : string list;
  score : float;
}

type order = [ `Relevance | `Date ]

type results = {
  terms : string list;
  goto : goto list;
  work : hit list;
  work_total : int;
  links : hit list;
  links_total : int;
  kinds : (string * int) list;
  years : (int * int) list;
  tags : (string * int) list;
}

let empty = {
  terms = []; goto = []; work = []; work_total = 0; links = [];
  links_total = 0; kinds = []; years = []; tags = [];
}

(** {1 Kinds} *)

(* A weeknote is filed apart from other notes so the two can be searched
   and filtered separately, though both live under /notes. It shares the
   note's ranking prior: it is a diary of the same writing. *)
let kinds = ["paper"; "note"; "weekly"; "project"; "idea"; "video"; "link"]

let table_for kind = "search_" ^ kind

(** {1 Scoring} *)

(* A project page is the landing point for its topic. An idea is a proposal
   rather than a result. *)
let kind_prior = function
  | "project" -> 1.15
  | "video" -> 0.9
  | "idea" -> 0.85
  | _ -> 1.0

let age_years ~today:(ty, tm, _) date =
  match String.split_on_char '-' date with
  | y :: m :: _ -> (
    match int_of_string_opt y, int_of_string_opt m with
    | Some y, Some m ->
      float_of_int (ty - y) +. (float_of_int (tm - m) /. 12.)
    | _ -> 100.)
  | _ -> 100.

(* A date after [today] gives a negative age. Clamp it at zero first so a
   future-dated entry gets the maximum boost and no more. *)
let freshness ~today date =
  let age = Float.max 0. (age_years ~today date) in
  1. +. (0.25 *. Float.max 0. (1. -. (age /. 8.)))

let citation_bonus n = 1. +. (0.3 *. log (float_of_int (max 1 n)))

let normalise_url url =
  let strip prefix s =
    if String.starts_with ~prefix s then
      String.sub s (String.length prefix)
        (String.length s - String.length prefix)
    else s
  in
  let u = String.lowercase_ascii url |> strip "https://" |> strip "http://"
          |> strip "www." in
  let n = ref (String.length u) in
  while !n > 0 && (u.[!n - 1] = '/' || u.[!n - 1] = '#') do decr n done;
  String.sub u 0 !n

let host_of_url url =
  let u = normalise_url url in
  match String.index_opt u '/' with
  | Some i -> String.sub u 0 i
  | None -> u

(** {1 Schema — one FTS5 table per kind} *)

let create_table_sql kind =
  Printf.sprintf
    {|CREATE VIRTUAL TABLE IF NOT EXISTS %s USING fts5(
        slug UNINDEXED,
        url UNINDEXED,
        date UNINDEXED,
        parent_slugs UNINDEXED,
        title,
        body,
        tags,
        tokenize='porter unicode61'
      )|}
    (table_for kind)

let create_entry_tags_sql =
  {|CREATE TABLE IF NOT EXISTS entry_tags (
      tag TEXT NOT NULL,
      kind TEXT NOT NULL,
      slug TEXT NOT NULL,
      url TEXT NOT NULL,
      title TEXT NOT NULL,
      date TEXT NOT NULL
    )|}

let create_entry_tags_index_sql =
  {|CREATE INDEX IF NOT EXISTS idx_entry_tags_tag ON entry_tags(tag)|}

(* own_host does not fit a per-kind table or entry_tags, and a read-only
   handle has no other way to learn it: it never calls index, so it must
   read back whatever the last index wrote here. *)
let create_search_meta_sql =
  {|CREATE TABLE IF NOT EXISTS search_meta (
      key TEXT PRIMARY KEY,
      value TEXT
    )|}

let create_all_tables db =
  List.iter (fun kind ->
    Sqlite3.Rc.check (Sqlite3_eio.exec db (create_table_sql kind))
  ) kinds;
  Sqlite3.Rc.check (Sqlite3_eio.exec db create_entry_tags_sql);
  Sqlite3.Rc.check (Sqlite3_eio.exec db create_entry_tags_index_sql);
  Sqlite3.Rc.check (Sqlite3_eio.exec db create_search_meta_sql)

let create ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path in
  create_all_tables db;
  { db; own_host = ""; tag_counts = []; projects = [] }

let create_memory ~sw () =
  let db = Sqlite3_eio.open_memory ~sw () in
  create_all_tables db;
  { db; own_host = ""; tag_counts = []; projects = [] }

(** {1 Date formatting} *)

let date_string_of_triple (y, m, d) =
  Fmt.str "%04d-%02d-%02d" y m d

(** {1 Indexing} *)

let insert_sql kind =
  Printf.sprintf
    {|INSERT INTO %s (slug, url, date, parent_slugs, title, body, tags)
      VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)|}
    (table_for kind)

let insert_row t ~kind ~slug ~url ~date ~parent_slugs ~title ~body ~tags =
  let stmt = Sqlite3_eio.prepare t.db (insert_sql kind) in
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 slug);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 2 url);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 3 date);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 4 parent_slugs);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 5 title);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 6 body);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 7 tags);
  let rc = Sqlite3_eio.step t.db stmt in
  ignore (Sqlite3_eio.finalize t.db stmt);
  match rc with
  | Sqlite3.Rc.DONE -> ()
  | rc -> Sqlite3.Rc.check rc

let insert_tag_row t ~tag ~kind ~slug ~url ~title ~date =
  let stmt = Sqlite3_eio.prepare t.db
    {|INSERT INTO entry_tags (tag, kind, slug, url, title, date)
      VALUES (?1, ?2, ?3, ?4, ?5, ?6)|} in
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 tag);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 2 kind);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 3 slug);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 4 url);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 5 title);
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 6 date);
  let rc = Sqlite3_eio.step t.db stmt in
  ignore (Sqlite3_eio.finalize t.db stmt);
  match rc with
  | Sqlite3.Rc.DONE -> ()
  | rc -> Sqlite3.Rc.check rc

(* The markdown source of an entry, before [body_text] turns it into the
   prose a reader sees. *)
let entry_markdown (ent : Bushel.Entry.entry) = match ent with
  | `Paper p -> Bushel.Paper.abstract p
  | `Note n -> Bushel.Note.body n
  | `Project p -> Bushel.Project.body p
  | `Idea i -> Bushel.Idea.body i
  | `Video v -> Bushel.Video.description v

let plain_body ~contact_name ent =
  Bushel.Md.plain_text_of_markdown ~contact_name (entry_markdown ent)

let entry_tags (ent : Bushel.Entry.entry) = match ent with
  | `Paper p -> Bushel.Paper.tags p
  | `Note n -> Bushel.Note.tags n
  | `Project p -> Bushel.Project.tags p
  | `Idea i -> Bushel.Idea.tags i
  | `Video v -> Bushel.Video.tags v

let index_entry t ~body_text (ent : Bushel.Entry.entry) =
  let slug = Bushel.Entry.slug ent in
  let kind = match ent with
    | `Note n when n.Bushel.Note.weeknote -> "weekly"
    | _ -> Bushel.Entry.to_type_string ent
  in
  let url = Bushel.Entry.site_url ent in
  let date = date_string_of_triple (Bushel.Entry.date ent) in
  let title = Bushel.Entry.title ent in
  let tags_list = entry_tags ent in
  let tags = String.concat " " tags_list in
  let body = body_text ent in
  insert_row t ~kind ~slug ~url ~date ~parent_slugs:"" ~title ~body ~tags;
  List.iter (fun tag ->
    insert_tag_row t ~tag ~kind ~slug ~url ~title ~date
  ) tags_list

let strip_scheme url =
  let prefixes = ["https://"; "http://"] in
  match List.find_opt (fun p -> String.starts_with ~prefix:p url) prefixes with
  | Some p -> String.sub url (String.length p) (String.length url - String.length p)
  | None -> url

let index_link t ~entry_meta (link : Bushel.Link.t) =
  let url = Bushel.Link.url link in
  let slug = url in
  let kind = "link" in
  let date = date_string_of_triple (Bushel.Link.date link) in
  let karakeep_meta = match link.karakeep with
    | Some k -> k.metadata
    | None -> []
  in
  let title = match List.assoc_opt "title" karakeep_meta with
    | Some t when t <> "" -> t
    | _ -> url
  in
  let karakeep_summary = match List.assoc_opt "summary" karakeep_meta with
    | Some s when s <> "" -> s
    | _ -> ""
  in
  (* The titles of the citing entries are part of what a link is about:
     a search for the entry's subject should surface the links it cites,
     not only the entry itself. *)
  let parents = match link.bushel with
    | None -> []
    | Some b -> List.filter_map entry_meta b.slugs
  in
  let parent_titles = List.map fst parents in
  let body =
    let desc = Bushel.Link.description link in
    let parts =
      (if karakeep_summary <> "" then [karakeep_summary] else [])
      @ (if desc <> "" then [desc] else [])
      @ [strip_scheme url]
      @ parent_titles
    in
    String.concat "\n" parts
  in
  let karakeep_tags = match link.karakeep with
    | Some k -> k.tags
    | None -> []
  in
  let bushel_tags = match link.bushel with
    | Some b -> b.tags
    | None -> []
  in
  let all_tags = karakeep_tags @ bushel_tags in
  let tags = String.concat " " all_tags in
  (* Tag filtering matches exactly, and Karakeep writes phrases such as
     "Open Source Software" that no #tag can name. The rows a #tag query
     reads are therefore the lowercased union of the link's own tags and
     its citing entries' tags, so a link filters under the tags of the
     entries that cite it. The FTS tags column keeps only the link's own
     tags, so tag words do not over-boost ranked text queries. *)
  let filter_tags =
    List.sort_uniq compare
      (List.map String.lowercase_ascii
         (all_tags @ List.concat_map snd parents))
  in
  let parent_slugs = match link.bushel with
    | Some b -> String.concat "," b.slugs
    | None -> ""
  in
  insert_row t ~kind ~slug ~url ~date ~parent_slugs ~title ~body ~tags;
  List.iter (fun tag ->
    insert_tag_row t ~tag ~kind ~slug ~url ~title ~date
  ) filter_tags

(* An index written before search_meta existed has no such table, so
   prepare raises: treat that as no stored host rather than crash. *)
let load_own_host t =
  try
    let stmt = Sqlite3_eio.prepare t.db
      {|SELECT value FROM search_meta WHERE key = 'own_host'|} in
    let _rc, host = Sqlite3_eio.fold t.db stmt ~init:"" ~f:(fun _acc row ->
      match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> ""
    ) in
    ignore (Sqlite3_eio.finalize t.db stmt);
    host
  with Eio.Exn.Io _ -> ""

(* Both lists are read on every query and never change between rebuilds,
   so they are computed once here rather than queried each time. *)
let load_tag_counts t =
  let stmt = Sqlite3_eio.prepare t.db
    {|SELECT tag, COUNT(*) AS cnt FROM entry_tags
      WHERE kind <> 'link' GROUP BY tag ORDER BY cnt DESC, tag|} in
  let _rc, tags = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
    match row.(0), row.(1) with
    | Sqlite3.Data.TEXT tag, Sqlite3.Data.INT n -> (tag, Int64.to_int n) :: acc
    | _ -> acc
  ) in
  ignore (Sqlite3_eio.finalize t.db stmt);
  List.rev tags

let load_projects t =
  let stmt = Sqlite3_eio.prepare t.db
    {|SELECT slug, title, url, date FROM search_project ORDER BY date DESC|} in
  let text = function Sqlite3.Data.TEXT s -> s | _ -> "" in
  let _rc, ps = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
    (text row.(0), text row.(1), text row.(2), text row.(3)) :: acc
  ) in
  ignore (Sqlite3_eio.finalize t.db stmt);
  List.rev ps

(* A read-only handle never calls [index], so it learns [own_host],
   [tag_counts] and [projects] by reading back what the last [index]
   left in the database rather than by computing them itself. *)
let open_readonly ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path in
  let t = { db; own_host = ""; tag_counts = []; projects = [] } in
  t.own_host <- load_own_host t;
  t.tag_counts <- load_tag_counts t;
  t.projects <- load_projects t;
  t

let save_own_host t own_host =
  let stmt = Sqlite3_eio.prepare t.db
    {|INSERT OR REPLACE INTO search_meta (key, value)
      VALUES ('own_host', ?1)|} in
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 own_host);
  let rc = Sqlite3_eio.step t.db stmt in
  ignore (Sqlite3_eio.finalize t.db stmt);
  match rc with
  | Sqlite3.Rc.DONE -> ()
  | rc -> Sqlite3.Rc.check rc

let index t ~own_host ~body_text ~entries ~links =
  t.own_host <- own_host;
  Sqlite3.Rc.check (Sqlite3_eio.exec t.db "BEGIN");
  List.iter (fun kind ->
    Sqlite3.Rc.check (Sqlite3_eio.exec t.db
      (Printf.sprintf "DELETE FROM %s" (table_for kind)))
  ) kinds;
  Sqlite3.Rc.check (Sqlite3_eio.exec t.db "DELETE FROM entry_tags");
  save_own_host t own_host;
  List.iter (fun ent -> index_entry t ~body_text ent) entries;
  let meta = Hashtbl.create (List.length entries) in
  List.iter (fun ent ->
    Hashtbl.replace meta (Bushel.Entry.slug ent)
      (Bushel.Entry.title ent, entry_tags ent)
  ) entries;
  let entry_meta slug = Hashtbl.find_opt meta slug in
  List.iter (fun link -> index_link t ~entry_meta link) links;
  Sqlite3.Rc.check (Sqlite3_eio.exec t.db "COMMIT");
  (* Log per-table counts *)
  List.iter (fun kind ->
    let tbl = table_for kind in
    let sql = Printf.sprintf "SELECT count(*) FROM %s" tbl in
    let stmt = Sqlite3_eio.prepare t.db sql in
    let _rc, count = Sqlite3_eio.fold t.db stmt ~init:0 ~f:(fun _acc row ->
      match row.(0) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0
    ) in
    ignore (Sqlite3_eio.finalize t.db stmt);
    Logs.info (fun m -> m "Search index: %s has %d rows" tbl count)
  ) kinds;
  t.tag_counts <- load_tag_counts t;
  t.projects <- load_projects t

(* What the renderer escaped comes back as text once the tags are gone,
   or a snippet would show &amp;quot; where the page shows a quote. The
   ampersand is decoded last so an escaped entity stays escaped. *)
let html_unescape s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let ate entity by =
      let l = String.length entity in
      if !i + l <= n && String.sub s !i l = entity then begin
        Buffer.add_string buf by; i := !i + l; true
      end else false
    in
    if not (ate "&lt;" "<" || ate "&gt;" ">" || ate "&quot;" "\""
            || ate "&#39;" "'" || ate "&apos;" "'" || ate "&amp;" "&")
    then begin Buffer.add_char buf s.[!i]; incr i end
  done;
  Buffer.contents buf

let rebuild t ctx =
  (* Index the prose a reader sees, not its markup: the body renders
     through the site's own HTML pipeline, the tags are stripped and the
     entities decoded, so a search for "http" matches text about HTTP
     rather than every [foo](http://...) target, and a snippet shows
     resolved references rather than bushel syntax. *)
  let body_text ent =
    fst (Arod.Md.to_html ~ctx (entry_markdown ent))
    |> Arod.Text.strip_html |> html_unescape
    |> Arod.Text.collapse_whitespace
  in
  let own_host = host_of_url (Arod.Ctx.base_url ctx) in
  index t ~own_host ~body_text
    ~entries:(Arod.Ctx.all_entries ctx) ~links:(Arod.Ctx.all_links ctx)

(** {1 Querying} *)

let parse_parent_slugs s =
  if s = "" then []
  else String.split_on_char ',' s |> List.filter (fun s -> s <> "")

(* Per-kind fetch depth. The facets count over these, so they are the
   upper bound on a total. *)
let fetch_depth kind = if kind = "link" then 500 else 200

(* A browse (no query text) or a tags-only query fetches this many rows
   per kind, in place of the per-query fetch_depth above. *)
let browse_depth = 1000

let split_tags s =
  String.split_on_char ' ' s |> List.filter (fun t -> t <> "")

(* FTS5's snippet() wraps a match in whatever bytes it is given, and a body
   can hold real angle brackets ("<repo>") or third-party text, so wrapping
   in "<b>" directly would let either forge markup in a rendered page. The
   snippet is instead wrapped in these control bytes, which cannot occur in
   indexed text, HTML-escaped as plain text, and only then turned into the
   real tags the match wrapper needs. *)
let escape_snippet raw =
  let escaped = Arod.Md.html_escape_attr raw in
  let buf = Buffer.create (String.length escaped) in
  String.iter (fun c ->
    if c = '\002' then Buffer.add_string buf "<b>"
    else if c = '\003' then Buffer.add_string buf "</b>"
    else Buffer.add_char buf c
  ) escaped;
  Buffer.contents buf

(** Query one per-kind table ordered by relevance. [score] is the negated
    bm25, so larger is better. *)
let query_table t ~kind q =
  let tbl = table_for kind in
  let sql = Printf.sprintf
    {|SELECT slug, url, date, parent_slugs, title,
           snippet(%s, 5, char(2), char(3), '...', 32),
           bm25(%s, 0.0, 0.0, 0.0, 0.0, 10.0, 1.0, 5.0),
           tags
      FROM %s
      WHERE %s MATCH ?1
      ORDER BY bm25(%s, 0.0, 0.0, 0.0, 0.0, 10.0, 1.0, 5.0)
      LIMIT ?2|}
    tbl tbl tbl tbl tbl
  in
  (* An index written before a kind existed has no table for it. Reading
     it as empty keeps an old database searchable until the next index. *)
  match Sqlite3_eio.prepare t.db sql with
  | exception Eio.Exn.Io _ -> []
  | stmt ->
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 q);
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 2 (fetch_depth kind));
  let text i row = match row.(i) with Sqlite3.Data.TEXT s -> s | _ -> "" in
  let _rc, results = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
    let rank = match row.(6) with Sqlite3.Data.FLOAT f -> f | _ -> 0.0 in
    { slug = text 0 row; kind; url = text 1 row; title = text 4 row;
      snippet = escape_snippet (text 5 row); date = text 2 row;
      tags = split_tags (text 7 row);
      parent_slugs = parse_parent_slugs (text 3 row);
      score = -. rank } :: acc
  ) in
  ignore (Sqlite3_eio.finalize t.db stmt);
  List.rev results

let by_score a b =
  match compare b.score a.score with
  | 0 -> compare b.date a.date
  | c -> c

let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let rank_work ~today hits =
  List.map (fun h ->
    { h with score = h.score *. kind_prior h.kind *. freshness ~today h.date })
    hits
  |> List.sort by_score

(* Two URLs that differ only in scheme, www, a trailing slash or hash are
   one page. So are two links on one host with the same title, which is how
   a redirect and its target both end up cited. The higher-scoring copy
   survives, so this runs after the sort. *)
let dedupe_links ~own_host hits =
  let seen = Hashtbl.create 64 in
  List.filter (fun h ->
    let url_key = normalise_url h.url in
    let host = match String.index_opt url_key '/' with
      | Some i -> String.sub url_key 0 i | None -> url_key in
    let title_key = host ^ "|" ^ String.lowercase_ascii h.title in
    if host = own_host && own_host <> "" then false
    else if Hashtbl.mem seen url_key || Hashtbl.mem seen title_key then false
    else begin
      Hashtbl.replace seen url_key ();
      Hashtbl.replace seen title_key ();
      true
    end
  ) hits

let rank_links ~today ~own_host hits =
  List.map (fun h ->
    { h with score = h.score *. freshness ~today h.date
                     *. citation_bonus (List.length h.parent_slugs) })
    hits
  |> List.sort by_score
  |> dedupe_links ~own_host

(** {1 Tag queries} *)

(** Query entries matching ALL given tags exactly. *)
let search_tags t ~kinds ~limit tags =
  if tags = [] then []
  else
    let n_tags = List.length tags in
    let tag_placeholders = List.mapi (fun i _ ->
      Printf.sprintf "?%d" (i + 1)
    ) tags |> String.concat ", " in
    let kind_clause = match kinds with
      | [] -> ""
      | ks ->
        let kind_phs = List.mapi (fun i _ ->
          Printf.sprintf "?%d" (n_tags + i + 1)
        ) ks |> String.concat ", " in
        Printf.sprintf " AND kind IN (%s)" kind_phs
    in
    let sql = Printf.sprintf
      {|SELECT slug, kind, url, title, date
        FROM entry_tags
        WHERE tag IN (%s)%s
        GROUP BY slug
        HAVING COUNT(DISTINCT tag) = ?%d
        ORDER BY date DESC
        LIMIT ?%d|}
      tag_placeholders kind_clause
      (n_tags + List.length kinds + 1)
      (n_tags + List.length kinds + 2)
    in
    let stmt = Sqlite3_eio.prepare t.db sql in
    List.iteri (fun i tag ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt (i + 1) tag)
    ) tags;
    List.iteri (fun i k ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt (n_tags + i + 1) k)
    ) kinds;
    Sqlite3.Rc.check (Sqlite3.bind_int stmt (n_tags + List.length kinds + 1) n_tags);
    Sqlite3.Rc.check (Sqlite3.bind_int stmt (n_tags + List.length kinds + 2) limit);
    let _rc, results = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
      let slug = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      let kind = match row.(1) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      let url = match row.(2) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      let title = match row.(3) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      let date = match row.(4) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      { slug; kind; url; title; snippet = ""; date; score = 0.0;
        parent_slugs = []; tags = [] } :: acc
    ) in
    ignore (Sqlite3_eio.finalize t.db stmt);
    List.rev results

(** [tags_for_slug t slug] is every tag on the entry [slug], sorted. A
    row from {!search_tags} carries no tags column of its own, so a hit
    that is to show or be counted by them needs this looked up. *)
let tags_for_slug t slug =
  let stmt = Sqlite3_eio.prepare t.db
    {|SELECT DISTINCT tag FROM entry_tags WHERE slug = ?1 ORDER BY tag|} in
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 slug);
  let _rc, tags = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
    match row.(0) with Sqlite3.Data.TEXT s -> s :: acc | _ -> acc
  ) in
  ignore (Sqlite3_eio.finalize t.db stmt);
  List.rev tags

(** {1 Search syntax} *)

let parse_search_input input =
  let words = String.split_on_char ' ' input in
  let found_kinds = ref [] in
  let found_tags = ref [] in
  let terms = List.filter_map (fun w ->
    match String.split_on_char ':' w with
    | ["kind"; k] when List.mem k kinds ->
      found_kinds := k :: !found_kinds; None
    | _ ->
      if w = "" then None
      else if String.length w > 1 && w.[0] = '#' then begin
        (* Tag rows are stored lowercased, so the filter matches any case
           the reader types. *)
        found_tags :=
          String.lowercase_ascii (String.sub w 1 (String.length w - 1))
          :: !found_tags;
        None
      end
      else Some w
  ) words in
  (* Append * to the last term for prefix matching (works-as-you-type)
     unless it already ends with * or is a quoted phrase *)
  let terms = match List.rev terms with
    | [] -> []
    | last :: rest ->
      let last' =
        if String.ends_with ~suffix:"*" last then last
        else if String.starts_with ~prefix:"\"" last then last
        else last ^ "*"
      in
      List.rev (last' :: rest)
  in
  let fts_query = String.concat " " terms in
  let plain =
    List.map (fun w ->
      let w = String.lowercase_ascii w in
      let w = if String.ends_with ~suffix:"*" w
        then String.sub w 0 (String.length w - 1) else w in
      String.concat "" (String.split_on_char '"' w)) terms
    |> List.filter (fun w -> w <> "")
  in
  (List.rev !found_kinds, List.rev !found_tags, fts_query, plain)

(* entry_tags carries a row per (entry, tag), so an untagged entry has no
   row there and a browse over it would silently drop that entry. The
   per-kind table holds every entry of that kind regardless of tags. *)
let browse_kinds t ~kinds:target_kinds =
  List.concat_map (fun kind ->
    let tbl = table_for kind in
    let sql = Printf.sprintf
      {|SELECT slug, url, date, parent_slugs, title, tags
        FROM %s
        ORDER BY date DESC
        LIMIT %d|}
      tbl browse_depth
    in
    match Sqlite3_eio.prepare t.db sql with
    | exception Eio.Exn.Io _ -> []
    | stmt ->
    let text i row = match row.(i) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let _rc, results = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
      { slug = text 0 row; kind; url = text 1 row; title = text 4 row;
        snippet = ""; date = text 2 row; score = 0.0;
        tags = split_tags (text 5 row);
        parent_slugs = parse_parent_slugs (text 3 row) } :: acc
    ) in
    ignore (Sqlite3_eio.finalize t.db stmt);
    List.rev results
  ) target_kinds

let split_tiers hits =
  List.partition (fun h -> h.kind <> "link") hits

(* Both a ranked query and a plain browse cut work and links to the
   caller's limits and count the matches before the cut, so the record
   is built once here. *)
let make_results ~terms ~limit ~link_limit work links =
  { empty with terms; work = take limit work;
    work_total = List.length work; links = take link_limit links;
    links_total = List.length links }

(** {1 Go-to tier} *)

let sections = [
  "Papers", "/papers"; "Notes", "/notes"; "Projects", "/projects";
  "Ideas", "/ideas"; "Talks", "/talks"; "Links", "/links";
  "Network", "/network";
]

let name_words s =
  String.lowercase_ascii s
  |> String.split_on_char ' '
  |> List.concat_map (String.split_on_char '-')
  |> List.filter (fun w -> w <> "")

let is_prefix_of_name ~term name =
  List.exists (String.starts_with ~prefix:term) (name_words name)

(* site.js redirects a "/#tag=" hash straight to "/search?q=%23<tag>", so a
   go-to chip that used the hash form cost a full home-page load before that
   redirect ran. Pointing it at the search URL directly skips that hop. *)
let tag_goto_url tag =
  "/search?q=%23" ^ Uriz.pct_encode ~component:`Query_value tag

(* Every query word must match something in a hit for it to be offered as a
   go-to, so a two-word query only jumps to a project or tag whose name
   accounts for both words. *)
let goto_hits t terms =
  let terms = List.filter (fun w -> String.length w >= 2) terms in
  if terms = [] then []
  else
    let every_term f = List.for_all f terms in
    let sections =
      List.filter_map (fun (name, url) ->
        if List.exists (fun term ->
             String.starts_with ~prefix:term (String.lowercase_ascii name))
             terms
        then Some { label = name; url; detail = "section";
                    goto_kind = `Section }
        else None) sections
    in
    let projects =
      List.filter_map (fun (slug, title, url, date) ->
        if every_term (fun term ->
             String.starts_with ~prefix:term (String.lowercase_ascii slug)
             || is_prefix_of_name ~term title)
        then Some { label = title; url;
                    detail = String.sub date 0 4 ^ " project";
                    goto_kind = `Project }
        else None) t.projects
    in
    let tags =
      List.filter_map (fun (tag, n) ->
        if every_term (fun term ->
             String.starts_with ~prefix:term tag
             || is_prefix_of_name ~term tag)
        then Some { label = tag; url = tag_goto_url tag;
                    detail = Printf.sprintf "%d %s" n
                        (if n = 1 then "entry" else "entries");
                    goto_kind = `Tag }
        else None) t.tag_counts
    in
    take 7 (sections @ projects @ take 5 tags)

(** {1 Facets} *)

let count_by key hits =
  let tbl = Hashtbl.create 16 in
  List.iter (fun h ->
    List.iter (fun k ->
      Hashtbl.replace tbl k
        (1 + Option.value ~default:0 (Hashtbl.find_opt tbl k)))
      (key h)) hits;
  Hashtbl.fold (fun k n acc -> (k, n) :: acc) tbl []

let tag_facet (work : hit list) =
  count_by (fun (h : hit) -> h.tags) work
  |> List.sort (fun (a, n) (b, m) ->
       match compare m n with 0 -> compare a b | c -> c)
  |> take 8

(* Every field counts over the work tier, never the links tier, so a facet
   click narrows the same set the results came from. *)
let facets (work : hit list) =
  let kinds = count_by (fun (h : hit) -> [h.kind]) work
              |> List.sort (fun (a, _) (b, _) -> compare a b) in
  let years = count_by (fun (h : hit) ->
      match int_of_string_opt
              (String.sub h.date 0 (min 4 (String.length h.date)))
      with Some y -> [y] | None -> []) work
    |> List.sort (fun (a, _) (b, _) -> compare a b) in
  (kinds, years, tag_facet work)

let search t ?today ?(limit = 20) ?(link_limit = 12) ?(order = `Relevance)
    input =
  let today = match today with
    | Some d -> d
    | None -> let (d, _) = Ptime.to_date_time (Ptime_clock.now ()) in d
  in
  let found_kinds, found_tags, fts_query, terms = parse_search_input input in
  Logs.info (fun m -> m "Search: input=%S kinds=[%s] tags=[%s] fts_query=%S"
    input (String.concat "," found_kinds) (String.concat "," found_tags)
    fts_query);
  let target_kinds = match found_kinds with [] -> kinds | ks -> ks in
  (* Date order re-sorts the matched set, so an old but relevant hit gives
     way to every newer match, not just to the newer ones in the shown
     slice. Ranking still runs first: dedupe keeps the better-scoring copy
     of a link whichever order is shown. *)
  let reorder hits = match order with
    | `Relevance -> hits
    | `Date -> List.sort (fun a b -> compare b.date a.date) hits
  in
  let finish hits =
    let work, links = split_tiers hits in
    let work = rank_work ~today work |> reorder in
    let links =
      rank_links ~today ~own_host:t.own_host links |> reorder in
    let kinds, years, tags = facets work in
    { (make_results ~terms ~limit ~link_limit work links) with
      goto = goto_hits t terms; kinds; years; tags }
  in
  let browse hits =
    let work, links = split_tiers hits in
    let by_date = List.sort (fun a b -> compare b.date a.date) in
    let work = by_date work in
    let links = by_date links |> dedupe_links ~own_host:t.own_host in
    let kinds, years, tags = facets work in
    { (make_results ~terms ~limit ~link_limit work links) with
      kinds; years; tags }
  in
  match found_tags, fts_query with
  | [], "" when found_kinds = [] -> empty
  | [], "" -> browse (browse_kinds t ~kinds:target_kinds)
  | [], _ ->
    finish (List.concat_map (fun kind -> query_table t ~kind fts_query)
              target_kinds)
  | tags, "" ->
    (* search_tags rows carry no tags of their own, so the work tier
       and the tag facet are enriched after the browse limit cuts them
       down to size: at most [limit] small lookups, not one per match. *)
    let r =
      browse (search_tags t ~kinds:target_kinds ~limit:browse_depth tags)
    in
    let work = List.map
        (fun (h : hit) -> { h with tags = tags_for_slug t h.slug }) r.work in
    { r with work; tags = tag_facet work }
  | tags, _ ->
    let tag_slugs =
      List.fold_left (fun s r -> StringSet.add r.slug s) StringSet.empty
        (search_tags t ~kinds:target_kinds ~limit:browse_depth tags)
    in
    List.concat_map (fun kind -> query_table t ~kind fts_query) target_kinds
    |> List.filter (fun r -> StringSet.mem r.slug tag_slugs)
    |> finish

let pp_hit ppf h =
  let snippet = html_unescape (Arod.Text.strip_html h.snippet) in
  let tags = match h.tags with
    | [] -> "" | ts -> " #" ^ String.concat " #" ts in
  let parents = match h.parent_slugs with
    | [] -> "" | ps -> " in " ^ String.concat ", " ps in
  Fmt.pf ppf "@[<v>%s [%s] %s %6.1f%s%s@,  %s@,  %s@]"
    h.title h.kind h.date h.score tags parents h.url snippet

let pp_results ppf r =
  let tier name total hits =
    if hits <> [] then begin
      Fmt.pf ppf "@[<v>== %s (%d)@,@]" name total;
      List.iter (fun h -> Fmt.pf ppf "%a@.@." pp_hit h) hits
    end
  in
  if r.goto <> [] then begin
    Fmt.pf ppf "== go to@.";
    List.iter (fun g -> Fmt.pf ppf "  %s  %s  %s@." g.label g.detail g.url)
      r.goto;
    Fmt.pf ppf "@."
  end;
  tier "on this site" r.work_total r.work;
  tier "links" r.links_total r.links
