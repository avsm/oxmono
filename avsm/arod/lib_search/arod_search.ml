(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** FTS5 full-text search index for Arod content.

    Uses one FTS5 table per entry kind (paper, note, project, idea, video,
    link) so that kind filtering is a simple matter of which tables to query.
    Results from each table are merged and sorted by date. *)

module StringSet = Set.Make(String)

type t = { db : Sqlite3_eio.t }

type result = {
  slug : string;
  kind : string;
  url : string;
  title : string;
  snippet : string;
  date : string;
  rank : float;
  parent_slugs : string list;
  tags : string list;
}

(** {1 Kinds} *)

let kinds = ["paper"; "note"; "project"; "idea"; "video"; "link"]

let table_for kind = "search_" ^ kind

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

let create_all_tables db =
  List.iter (fun kind ->
    Sqlite3.Rc.check (Sqlite3_eio.exec db (create_table_sql kind))
  ) kinds;
  Sqlite3.Rc.check (Sqlite3_eio.exec db create_entry_tags_sql);
  Sqlite3.Rc.check (Sqlite3_eio.exec db create_entry_tags_index_sql)

let create ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path in
  create_all_tables db;
  { db }

let create_memory ~sw () =
  let db = Sqlite3_eio.open_memory ~sw () in
  create_all_tables db;
  { db }

let open_readonly ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path in
  { db }

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

let index_entry t ~contact_name (ent : Bushel.Entry.entry) =
  let slug = Bushel.Entry.slug ent in
  let kind = Bushel.Entry.to_type_string ent in
  let url = Bushel.Entry.site_url ent in
  let date = date_string_of_triple (Bushel.Entry.date ent) in
  let title = Bushel.Entry.title ent in
  let tags_list = match ent with
    | `Paper p -> Bushel.Paper.tags p
    | `Note n -> Bushel.Note.tags n
    | `Project p -> Bushel.Project.tags p
    | `Idea i -> Bushel.Idea.tags i
    | `Video v -> Bushel.Video.tags v
  in
  let tags = String.concat " " tags_list in
  let plain = Bushel.Md.plain_text_of_markdown ~contact_name in
  let body = match ent with
    | `Paper p -> Bushel.Paper.abstract p
    | `Note n -> plain (Bushel.Note.body n)
    | `Project p -> plain (Bushel.Project.body p)
    | `Idea i -> plain (Bushel.Idea.body i)
    | `Video v -> Bushel.Video.description v
  in
  insert_row t ~kind ~slug ~url ~date ~parent_slugs:"" ~title ~body ~tags;
  List.iter (fun tag ->
    insert_tag_row t ~tag ~kind ~slug ~url ~title ~date
  ) tags_list

let strip_scheme url =
  let prefixes = ["https://"; "http://"] in
  match List.find_opt (fun p -> String.starts_with ~prefix:p url) prefixes with
  | Some p -> String.sub url (String.length p) (String.length url - String.length p)
  | None -> url

let index_link t (link : Bushel.Link.t) =
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
  let body =
    let desc = Bushel.Link.description link in
    let parts =
      (if karakeep_summary <> "" then [karakeep_summary] else [])
      @ (if desc <> "" then [desc] else [])
      @ [strip_scheme url]
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
  let parent_slugs = match link.bushel with
    | Some b -> String.concat "," b.slugs
    | None -> ""
  in
  insert_row t ~kind ~slug ~url ~date ~parent_slugs ~title ~body ~tags;
  List.iter (fun tag ->
    insert_tag_row t ~tag ~kind ~slug ~url ~title ~date
  ) all_tags

let rebuild t ctx =
  Sqlite3.Rc.check (Sqlite3_eio.exec t.db "BEGIN");
  List.iter (fun kind ->
    Sqlite3.Rc.check (Sqlite3_eio.exec t.db
      (Printf.sprintf "DELETE FROM %s" (table_for kind)))
  ) kinds;
  Sqlite3.Rc.check (Sqlite3_eio.exec t.db "DELETE FROM entry_tags");
  let contacts = Arod.Ctx.contacts ctx in
  let contact_name handle =
    List.find_map (fun c ->
      if Sortal_schema.Contact.handle c = handle
      then Some (Sortal_schema.Contact.name c)
      else None
    ) contacts
  in
  let entries = Arod.Ctx.all_entries ctx in
  List.iter (fun ent -> index_entry t ~contact_name ent) entries;
  let links = Arod.Ctx.all_links ctx in
  List.iter (fun link -> index_link t link) links;
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
  ) kinds

(** {1 Querying} *)

let parse_parent_slugs s =
  if s = "" then []
  else String.split_on_char ',' s |> List.filter (fun s -> s <> "")

(** Query a single per-kind FTS5 table. *)
let query_table t ~kind ~limit q =
  let tbl = table_for kind in
  let sql = Printf.sprintf
    {|SELECT slug, url, date, parent_slugs, title,
           snippet(%s, 5, '<b>', '</b>', '...', 32),
           bm25(%s, 0.0, 0.0, 0.0, 0.0, 10.0, 1.0, 5.0)
      FROM %s
      WHERE %s MATCH ?1
      ORDER BY date DESC
      LIMIT ?2|}
    tbl tbl tbl tbl
  in
  let stmt = Sqlite3_eio.prepare t.db sql in
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 q);
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 2 limit);
  let _rc, results = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
    let slug = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let url = match row.(1) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let date = match row.(2) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let parent_slugs_str = match row.(3) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let title = match row.(4) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let snippet = match row.(5) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let rank = match row.(6) with Sqlite3.Data.FLOAT f -> f | _ -> 0.0 in
    let parent_slugs = parse_parent_slugs parent_slugs_str in
    { slug; kind; url; title; snippet; date; rank; parent_slugs; tags = [] } :: acc
  ) in
  ignore (Sqlite3_eio.finalize t.db stmt);
  List.rev results

(** Merge results from multiple tables, sorted by date descending, take [limit]. *)
let merge_results ~limit results_per_kind =
  let all = List.concat results_per_kind in
  let sorted = List.sort (fun a b -> String.compare b.date a.date) all in
  let rec take acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> take (x :: acc) (n - 1) xs
  in
  take [] limit sorted

(** {1 Tag queries} *)

(** Look up tags for a given slug from the entry_tags table. *)
let tags_for_slug t slug =
  let stmt = Sqlite3_eio.prepare t.db
    {|SELECT DISTINCT tag FROM entry_tags WHERE slug = ?1 ORDER BY tag|} in
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 slug);
  let _rc, tags = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
    match row.(0) with Sqlite3.Data.TEXT s -> s :: acc | _ -> acc
  ) in
  ignore (Sqlite3_eio.finalize t.db stmt);
  List.rev tags

(** Query entries matching ALL given tags exactly. *)
let search_tags t ?(kinds=[]) ?(limit=20) tags =
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
      { slug; kind; url; title; snippet = ""; date; rank = 0.0;
        parent_slugs = []; tags = [] } :: acc
    ) in
    ignore (Sqlite3_eio.finalize t.db stmt);
    List.rev results

(** Return all unique tags with counts. *)
let all_tags t =
  let stmt = Sqlite3_eio.prepare t.db
    {|SELECT tag, COUNT(*) as cnt FROM entry_tags
      GROUP BY tag ORDER BY cnt DESC|} in
  let _rc, tags = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
    let tag = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
    let count = match row.(1) with Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    (tag, count) :: acc
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
        found_tags := String.sub w 1 (String.length w - 1) :: !found_tags;
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
  (List.rev !found_kinds, List.rev !found_tags, fts_query)

let enrich_tags t results =
  List.map (fun r ->
    { r with tags = tags_for_slug t r.slug }
  ) results

let search t ?(limit = 20) input =
  let found_kinds, found_tags, fts_query = parse_search_input input in
  Logs.info (fun m -> m "Search: input=%S kinds=[%s] tags=[%s] fts_query=%S"
    input (String.concat "," found_kinds) (String.concat "," found_tags) fts_query);
  let target_kinds = match found_kinds with
    | [] -> kinds
    | ks -> ks
  in
  match found_tags, fts_query with
  | [], "" when found_kinds = [] -> []
  | [], "" ->
    (* Kind-only browse: return recent entries of the specified kinds *)
    let kind_phs = List.mapi (fun i _ ->
      Printf.sprintf "?%d" (i + 1)
    ) target_kinds |> String.concat ", " in
    let sql = Printf.sprintf
      {|SELECT DISTINCT slug, kind, url, title, date
        FROM entry_tags
        WHERE kind IN (%s)
        ORDER BY date DESC
        LIMIT ?%d|}
      kind_phs (List.length target_kinds + 1)
    in
    let stmt = Sqlite3_eio.prepare t.db sql in
    List.iteri (fun i k ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt (i + 1) k)
    ) target_kinds;
    Sqlite3.Rc.check (Sqlite3.bind_int stmt (List.length target_kinds + 1) limit);
    let _rc, results = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
      let slug = match row.(0) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      let kind = match row.(1) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      let url = match row.(2) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      let title = match row.(3) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      let date = match row.(4) with Sqlite3.Data.TEXT s -> s | _ -> "" in
      { slug; kind; url; title; snippet = ""; date; rank = 0.0;
        parent_slugs = []; tags = [] } :: acc
    ) in
    ignore (Sqlite3_eio.finalize t.db stmt);
    enrich_tags t (List.rev results)
  | [], _ ->
    (* Pure FTS search, same as before *)
    let per_kind = List.map (fun kind ->
      let results = query_table t ~kind ~limit fts_query in
      Logs.info (fun m -> m "Search: table=%s query=%S -> %d results"
        (table_for kind) fts_query (List.length results));
      results
    ) target_kinds in
    enrich_tags t (merge_results ~limit per_kind)
  | tags, "" ->
    (* Pure tag search *)
    let results = search_tags t ~kinds:target_kinds ~limit tags in
    enrich_tags t results
  | tags, _ ->
    (* Mixed: tag filter + FTS — intersect results *)
    let tag_results = search_tags t ~kinds:target_kinds ~limit:1000 tags in
    let tag_slugs = List.fold_left (fun s r ->
      StringSet.add r.slug s
    ) StringSet.empty tag_results in
    let per_kind = List.map (fun kind ->
      query_table t ~kind ~limit fts_query
    ) target_kinds in
    let fts_results = merge_results ~limit:1000 per_kind in
    let filtered = List.filter (fun r ->
      StringSet.mem r.slug tag_slugs
    ) fts_results in
    let rec take acc n = function
      | _ when n <= 0 -> List.rev acc
      | [] -> List.rev acc
      | x :: xs -> take (x :: acc) (n - 1) xs
    in
    enrich_tags t (take [] limit filtered)

let pp_result ppf r =
  let snippet = Arod.Text.strip_html r.snippet in
  let tags_str = match r.tags with [] -> "" | ts -> " #" ^ String.concat " #" ts in
  Fmt.pf ppf "@[<v>%s [%s] %s%s@,  %s@,  %s@]"
    r.title r.kind r.date tags_str r.url snippet
