# Arod Search Rerank Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rank search results in strict tiers (go to, work, links) and serve them on a `/search` page with a main column and a rail, replacing the date-ordered modal.

**Architecture:** `Arod_search` (lib_search) gains a tiered `results` record and does all ranking, deduplication and facet counting in OCaml over per-kind FTS5 queries ordered by `bm25`. A new `Arod_component.Search` renders that record as the page body and as a fragment. `Arod_render` assembles the page and the JSON, both reached from handlers through closures in `Arod_env` because the search handle is domain-bound. The modal and its JS are deleted.

**Tech Stack:** OCaml 5.2.0+ox (OxCaml), dune, SQLite FTS5 via `sqlite3-eio`, Htmlit, Jsont, proffer (routes and mock tests), Tailwind v3 prebuilt CSS.

**Spec:** `docs/superpowers/specs/2026-08-23-arod-search-rerank-design.md`

## Global Constraints

- Build, test and format must be clean before each commit, scoped to arod: `dune build @avsm/arod/all @avsm/arod/runtest --force`. `ocamlformat` is not installed in the `5.2.0+ox` switch, so match surrounding formatting by hand and keep lines within 80 columns.
- Handlers are `@ portable`. The search handle is domain-bound, so anything that reads it is reached through a closure field in `Arod_env.t`, never captured by a handler.
- Prose and docstrings: `[foo x y] is ...`, no em-dashes, no semicolons joining clauses. Comments explain what the code cannot.
- Commit messages: one line, imperative, no trailers. Mechanical changes (the Tailwind regen) get their own commit.
- Never regenerate a golden to make a test pass. The goldens under `avsm/arod/test/fixtures/golden` cover markdown renders, not the nav, so none should change here.
- New Tailwind utility classes in OCaml markup require `avsm/arod/tailwind/regen.sh`. Prefer custom `sp-*` classes defined in `Theme.custom_css`, which is inlined into every page, so the regen is only needed if a utility class not already in `assets/tw.css` is introduced.
- Work in `avsm/arod`. All paths below are relative to it unless they start with `docs/`.

---

## File map

| file | responsibility |
|---|---|
| `lib_search/arod_search.ml`, `.mli` | index, tiered ranking, dedupe, go-to matching, facets. Modify. |
| `lib_search/dune` | add `ptime.clock.os` for the default `today`. Modify. |
| `test/test_search.ml` | ranking tests over an in-memory index built from synthetic entries and links. Create. |
| `lib_component/search.ml` | HTML for the results fragment and the page body. Create. |
| `lib_component/dune` | add `arod.search`. Modify. |
| `lib_component/theme.ml` | `sp-*` CSS. Delete the `search-*` modal CSS. Modify. |
| `lib_component/scripts.ml` | add `search_js` page script, replace modal JS with a shortcut. Modify. |
| `lib_component/layout.ml` | add `Search` to `page_script`. Modify. |
| `lib_component/nav.ml` | search button becomes a link, delete `search_modal`. Modify. |
| `lib_handlers/arod_render.ml`, `.mli` | JSON codec for `results`, `search_page` render. Modify. |
| `lib_handlers/arod_env.ml`, `.mli` | `search` and `search_page` closures. Modify. |
| `lib_handlers/arod_handlers.ml`, `.mli` | `search_api` parameters, new `search_page` handler. Modify. |
| `lib/server/arod_site.ml` | `/search` route. Modify. |
| `bin/main.ml` | CLI output and server wiring. Modify. |
| `test/test_json.ml`, `test/test_routes.ml`, `test/dune` | update to the new shapes, register `test_search`. Modify. |
| `CHANGES.md` | user-visible entries. Modify. |

---

### Task 1: `index` entry point so tests can build an index without a data directory

**Files:**
- Modify: `lib_search/arod_search.ml:217-243` (the `rebuild` function)
- Modify: `lib_search/arod_search.mli`
- Create: `test/test_search.ml`
- Modify: `test/dune`

**Interfaces:**
- Produces: `val index : t -> own_host:string -> contact_name:(string -> string option) -> entries:Bushel.Entry.entry list -> links:Bushel.Link.t list -> unit`. `rebuild t ctx` becomes a call to it. `own_host` is stored on `t` for Task 2, and is the host of the site's base URL with any leading `www.` removed.

- [ ] **Step 1: Write the failing test**

Create `test/test_search.ml`:

```ocaml
(* The ranking in Arod_search is pinned here over an in-memory index built
   from synthetic entries and links, so each check names the one property
   of the model it holds. The real corpus is not in the repository. *)

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let note ?(tags = []) ?(date = (2024, 2, 3)) ~slug ~title body : Bushel.Note.t =
  {
    Bushel.Note.title;
    date;
    slug;
    body;
    tags;
    draft = false;
    updated = None;
    sidebar = None;
    index_page = false;
    perma = false;
    weeknote = false;
    featured = false;
    doi = None;
    synopsis = None;
    titleimage = None;
    via = None;
    slug_ent = None;
    source = None;
    url = None;
    author = None;
    category = None;
    standardsite = None;
    social = None;
    source_file = None;
  }

let link ?(title = "") ?(slugs = []) ?(date = (2024, 1, 1)) url : Bushel.Link.t =
  {
    Bushel.Link.url;
    date;
    description = "";
    karakeep =
      (if title = "" then None
       else
         Some
           {
             Bushel.Link.remote_url = url;
             id = "k";
             tags = [];
             metadata = [ ("title", title) ];
           });
    bushel = Some { Bushel.Link.slugs; tags = [] };
  }

let index ?(own_host = "") ~notes ~links () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t = Arod_search.create_memory ~sw () in
  Arod_search.index t ~own_host
    ~contact_name:(fun _ -> None)
    ~entries:(List.map (fun n -> `Note n) notes)
    ~links;
  t

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t = Arod_search.create_memory ~sw () in
  Arod_search.index t
    ~contact_name:(fun _ -> None)
    ~entries:[ `Note (note ~slug:"a" ~title:"Unikernels" "A body.") ]
    ~links:[ link ~title:"Unikernel blog" ~slugs:[ "a" ] "https://x.org/u" ];
  let results = Arod_search.search t "unikernel" in
  check "an indexed note is found"
    (List.exists (fun (r : Arod_search.result) -> r.slug = "a") results);
  check "so is an indexed link"
    (List.exists
       (fun (r : Arod_search.result) -> r.slug = "https://x.org/u")
       results)

let () = Printf.printf "test_search: %d checks ok\n" !checks
```

The `index` helper at the top is unused until Task 2. Leave it in, since Task 2's checks are written against it.

Add to `test/dune`:

```
(test
 (name test_search)
 (modules test_search)
 (libraries arod.search bushel eio eio_main))
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `dune build @avsm/arod/test/runtest --force 2>&1 | head -20`
Expected: a compile error, `Unbound value Arod_search.index`.

- [ ] **Step 3: Implement `index` and route `rebuild` through it**

In `lib_search/arod_search.ml`, change the handle type and `create` functions:

```ocaml
type t = {
  db : Sqlite3_eio.t;
  mutable own_host : string;
}

let create ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path in
  create_all_tables db;
  { db; own_host = "" }

let create_memory ~sw () =
  let db = Sqlite3_eio.open_memory ~sw () in
  create_all_tables db;
  { db; own_host = "" }

let open_readonly ~sw path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path in
  { db; own_host = "" }
```

Replace `rebuild` with:

```ocaml
let host_of_url url =
  let strip prefix s =
    if String.starts_with ~prefix s then
      String.sub s (String.length prefix)
        (String.length s - String.length prefix)
    else s
  in
  let u = String.lowercase_ascii url |> strip "https://" |> strip "http://"
          |> strip "www." in
  match String.index_opt u '/' with
  | Some i -> String.sub u 0 i
  | None -> u

let index t ~own_host ~contact_name ~entries ~links =
  t.own_host <- own_host;
  Sqlite3.Rc.check (Sqlite3_eio.exec t.db "BEGIN");
  List.iter (fun kind ->
    Sqlite3.Rc.check (Sqlite3_eio.exec t.db
      (Printf.sprintf "DELETE FROM %s" (table_for kind)))
  ) kinds;
  Sqlite3.Rc.check (Sqlite3_eio.exec t.db "DELETE FROM entry_tags");
  List.iter (fun ent -> index_entry t ~contact_name ent) entries;
  List.iter (fun link -> index_link t link) links;
  Sqlite3.Rc.check (Sqlite3_eio.exec t.db "COMMIT");
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

let rebuild t ctx =
  let contacts = Arod.Ctx.contacts ctx in
  let contact_name handle =
    List.find_map (fun c ->
      if Sortal_schema.Contact.handle c = handle
      then Some (Sortal_schema.Contact.name c)
      else None
    ) contacts
  in
  let own_host = host_of_url (Arod.Ctx.base_url ctx) in
  index t ~own_host ~contact_name
    ~entries:(Arod.Ctx.all_entries ctx) ~links:(Arod.Ctx.all_links ctx)
```

In `lib_search/arod_search.mli`, after `rebuild`:

```ocaml
val index :
  t ->
  own_host:string ->
  contact_name:(string -> string option) ->
  entries:Bushel.Entry.entry list ->
  links:Bushel.Link.t list ->
  unit
(** [index t ?own_host ~contact_name ~entries ~links] drops every table and
    indexes [entries] and [links]. [contact_name handle] is the display name
    a body mention of [handle] expands to. [own_host] is the host of the
    site's own base URL, and links on it are left out of search results. It
    is what {!rebuild} calls with a context's contents. *)
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `dune build @avsm/arod/all @avsm/arod/runtest --force 2>&1 | tail -5`
Expected: `test_search: 2 checks ok` and no errors.

- [ ] **Step 5: Commit**

```bash
git add avsm/arod/lib_search avsm/arod/test/test_search.ml avsm/arod/test/dune
git commit -m "Expose the search indexer so tests can build an index"
```

---

### Task 2: Tiered `results` with work and link ranking, dedupe and own-host exclusion

This task changes the type of `search`, which breaks `Arod_render`, `Arod_env`, `bin/main.ml` and `test_json`. Task 3 repairs them. Build the library and test alone here with `dune build @avsm/arod/lib_search/all @avsm/arod/test/runtest --force 2>&1 | grep -v arod_render` and commit at the end of Task 3, so the tree is never committed broken.

**Files:**
- Modify: `lib_search/arod_search.ml`, `lib_search/arod_search.mli`, `lib_search/dune`
- Modify: `test/test_search.ml`

**Interfaces:**
- Produces:

```ocaml
type goto_kind = [ `Section | `Project | `Tag ]
type goto = { label : string; url : string; detail : string;
              goto_kind : goto_kind }
type hit = { slug : string; kind : string; url : string; title : string;
             snippet : string; date : string; tags : string list;
             parent_slugs : string list; score : float }
type results = {
  terms : string list;        (* query words, lowercased, for marking *)
  goto : goto list;
  work : hit list; work_total : int;
  links : hit list; links_total : int;
  kinds : (string * int) list;
  years : (int * int) list;
  tags : (string * int) list;
}
val empty : results
val search :
  t -> ?today:int * int * int -> ?limit:int -> ?link_limit:int ->
  string -> results
val kind_prior : string -> float
val freshness : today:int * int * int -> string -> float
val citation_bonus : int -> float
val normalise_url : string -> string
val host_of_url : string -> string
val pp_results : Format.formatter -> results -> unit
```

`goto`, `kinds`, `years` and `tags` are empty in this task. Task 4 fills them. `result` and `pp_result` are deleted.

- [ ] **Step 1: Write the failing tests**

Replace the second `let () =` block in `test/test_search.ml` (keep `checks`, `check`, `note`, `link`, `index` and the final print) with:

```ocaml
let today = (2026, 8, 23)

let slugs (hits : Arod_search.hit list) =
  List.map (fun (h : Arod_search.hit) -> h.slug) hits

let () =
  let t =
    index
      ~notes:
        [
          note ~slug:"old" ~date:(2010, 1, 1) ~title:"Unikernels" "Body.";
          note ~slug:"new" ~date:(2026, 1, 1) ~title:"Unikernels" "Body.";
          note ~slug:"body-only" ~title:"Other" "About unikernels here.";
        ]
      ~links:
        [
          link ~title:"Unikernel blog" ~slugs:[ "old" ]
            "https://x.org/unikernel";
          link ~title:"Unikernel blog" ~slugs:[ "old"; "new" ]
            "https://www.x.org/unikernel/";
          link ~title:"Unikernel mirror" ~slugs:[ "old" ]
            "https://X.org/unikernel#";
          link ~title:"Unikernels local" ~slugs:[ "old" ]
            "https://example.com/papers/u.pdf";
          link ~title:"Unikernels twice" ~slugs:[ "old"; "new" ]
            "https://y.org/a";
          link ~title:"Unikernels once" ~slugs:[ "old" ] "https://z.org/a";
        ]
      ~own_host:"example.com" ()
  in
  let r = Arod_search.search t ~today "unikernel" in
  check "a title match outranks a body match"
    (List.mem "body-only" (slugs r.work)
    && List.nth (slugs r.work) 2 = "body-only");
  check "freshness breaks the tie between equal title matches"
    (slugs r.work |> List.filteri (fun i _ -> i < 2) = [ "new"; "old" ]);
  check "links never appear in the work tier"
    (List.for_all (fun (h : Arod_search.hit) -> h.kind <> "link") r.work);
  check "URLs differing by scheme, www, trailing slash or hash are one link"
    (List.length
       (List.filter
          (fun (h : Arod_search.hit) ->
            Arod_search.normalise_url h.url = "x.org/unikernel")
          r.links)
    = 1);
  check "a link on the site's own host is dropped"
    (not (List.exists (fun (h : Arod_search.hit) ->
              h.url = "https://example.com/papers/u.pdf") r.links));
  check "a link cited twice outranks the same title cited once"
    (let ys = List.filter (fun (h : Arod_search.hit) ->
         String.starts_with ~prefix:"https://y.org" h.url
         || String.starts_with ~prefix:"https://z.org" h.url) r.links in
     List.map (fun (h : Arod_search.hit) -> h.url) ys
     = [ "https://y.org/a"; "https://z.org/a" ]);
  check "totals count matches before the limit"
    (r.work_total = 3 && r.links_total = 3);
  check "the query words are returned lowercased without the prefix star"
    (r.terms = [ "unikernel" ])

let () =
  let t =
    index
      ~notes:
        [
          note ~slug:"a" ~date:(2020, 1, 1) ~title:"A" "x";
          note ~slug:"b" ~date:(2021, 1, 1) ~title:"B" "x";
        ]
      ~links:[] ()
  in
  let r = Arod_search.search t ~today ~limit:1 "x" in
  check "limit caps the work list" (List.length r.work = 1);
  check "but not the total" (r.work_total = 2);
  let r = Arod_search.search t ~today "kind:note" in
  check "a filter-only query browses by date"
    (slugs r.work = [ "b"; "a" ]);
  check "an empty query is empty"
    (Arod_search.search t ~today "" = Arod_search.empty)

let () =
  check "kind priors favour projects over ideas"
    (Arod_search.kind_prior "project" > Arod_search.kind_prior "paper"
    && Arod_search.kind_prior "paper" > Arod_search.kind_prior "idea");
  check "freshness is 1.25 this month and 1.0 after eight years"
    (Arod_search.freshness ~today "2026-08-01" = 1.25
    && Arod_search.freshness ~today "2018-01-01" = 1.0);
  check "the citation bonus is 1 for a single citation"
    (Arod_search.citation_bonus 1 = 1.0
    && Arod_search.citation_bonus 2 > 1.0);
  check "host_of_url drops scheme and www"
    (Arod_search.host_of_url "https://www.Example.com/a/b" = "example.com")
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `dune build @avsm/arod/test/runtest --force 2>&1 | head -10`
Expected: compile errors naming `Arod_search.hit` and `Arod_search.search`.

- [ ] **Step 3: Implement the types and the ranking**

Add `ptime.clock.os` to `lib_search/dune`:

```
(library
 (name arod_search)
 (public_name arod.search)
 (libraries arod bushel sqlite3-eio ptime ptime.clock.os fmt))
```

In `lib_search/arod_search.ml`, replace the `result` type with:

```ocaml
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
```

Add the scoring functions after `{1 Kinds}`:

```ocaml
(** {1 Scoring} *)

let local_kinds = ["paper"; "note"; "project"; "idea"; "video"]

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

let freshness ~today date =
  1. +. (0.25 *. Float.max 0. (1. -. (age_years ~today date /. 8.)))

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
```

Delete the `host_of_url` written in Task 1 (this one replaces it). Replace `query_table` and `merge_results` with:

```ocaml
(* Per-kind fetch depth. The facets count over these, so they are the
   upper bound on a total. *)
let fetch_depth kind = if kind = "link" then 500 else 200

let split_tags s =
  String.split_on_char ' ' s |> List.filter (fun t -> t <> "")

(** Query one per-kind table ordered by relevance. [score] is the negated
    bm25, so larger is better. *)
let query_table t ~kind q =
  let tbl = table_for kind in
  let sql = Printf.sprintf
    {|SELECT slug, url, date, parent_slugs, title,
           snippet(%s, 5, '<b>', '</b>', '...', 32),
           bm25(%s, 0.0, 0.0, 0.0, 0.0, 10.0, 1.0, 5.0),
           tags
      FROM %s
      WHERE %s MATCH ?1
      ORDER BY bm25(%s, 0.0, 0.0, 0.0, 0.0, 10.0, 1.0, 5.0)
      LIMIT ?2|}
    tbl tbl tbl tbl tbl
  in
  let stmt = Sqlite3_eio.prepare t.db sql in
  Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 q);
  Sqlite3.Rc.check (Sqlite3.bind_int stmt 2 (fetch_depth kind));
  let text i row = match row.(i) with Sqlite3.Data.TEXT s -> s | _ -> "" in
  let _rc, results = Sqlite3_eio.fold t.db stmt ~init:[] ~f:(fun acc row ->
    let rank = match row.(6) with Sqlite3.Data.FLOAT f -> f | _ -> 0.0 in
    { slug = text 0 row; kind; url = text 1 row; title = text 4 row;
      snippet = text 5 row; date = text 2 row;
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
    let title_key = host_of_url h.url ^ "|" ^ String.lowercase_ascii h.title in
    if host_of_url h.url = own_host && own_host <> "" then false
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
```

`search_tags` and the browse query return rows too. Change both to build `hit` values: in `search_tags` and in the kind-only browse branch, replace each `{ slug; kind; url; title; snippet = ""; date; rank = 0.0; parent_slugs = []; tags = [] }` with `{ slug; kind; url; title; snippet = ""; date; score = 0.0; parent_slugs = []; tags = [] }`. Delete `enrich_tags` and `tags_for_slug`, and instead select the tags with a second query per slug is not needed: `entry_tags` rows carry one tag each, so for those two paths leave `tags = []`. The page shows tags only for FTS hits.

Change `parse_search_input` to also return the lowercased terms without the trailing `*`:

```ocaml
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
```

Replace `search` with:

```ocaml
let split_tiers hits =
  List.partition (fun h -> h.kind <> "link") hits

let search t ?today ?(limit = 20) ?(link_limit = 12) input =
  let today = match today with
    | Some d -> d
    | None -> let (d, _) = Ptime.to_date_time (Ptime_clock.now ()) in d
  in
  let found_kinds, found_tags, fts_query, terms = parse_search_input input in
  Logs.info (fun m -> m "Search: input=%S kinds=[%s] tags=[%s] fts_query=%S"
    input (String.concat "," found_kinds) (String.concat "," found_tags)
    fts_query);
  let target_kinds = match found_kinds with [] -> kinds | ks -> ks in
  let finish hits =
    let work, links = split_tiers hits in
    let work = rank_work ~today work in
    let links = rank_links ~today ~own_host:t.own_host links in
    { empty with terms; work = take limit work;
      work_total = List.length work; links = take link_limit links;
      links_total = List.length links }
  in
  let browse hits =
    let work, links = split_tiers hits in
    let by_date = List.sort (fun a b -> compare b.date a.date) in
    let work = by_date work and links = by_date links in
    { empty with terms; work = take limit work;
      work_total = List.length work; links = take link_limit links;
      links_total = List.length links }
  in
  match found_tags, fts_query with
  | [], "" when found_kinds = [] -> empty
  | [], "" -> browse (browse_kinds t ~kinds:target_kinds)
  | [], _ ->
    finish (List.concat_map (fun kind -> query_table t ~kind fts_query)
              target_kinds)
  | tags, "" -> browse (search_tags t ~kinds:target_kinds ~limit:1000 tags)
  | tags, _ ->
    let tag_slugs =
      List.fold_left (fun s r -> StringSet.add r.slug s) StringSet.empty
        (search_tags t ~kinds:target_kinds ~limit:1000 tags)
    in
    List.concat_map (fun kind -> query_table t ~kind fts_query) target_kinds
    |> List.filter (fun r -> StringSet.mem r.slug tag_slugs)
    |> finish
```

Lift the kind-only browse SQL out of the old `search` into `browse_kinds t ~kinds` returning `hit list` (the same SELECT over `entry_tags` with `LIMIT 1000`, since `take` applies the caller's limit).

Replace `pp_result` with:

```ocaml
let pp_hit ppf h =
  let snippet = Arod.Text.strip_html h.snippet in
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
```

Update `lib_search/arod_search.mli`: delete `result` and `pp_result`, add the types from the Interfaces block above with these docs:

```ocaml
type hit = { ... }
(** One ranked hit. [score] is the tier's combined score, larger is better.
    For a link, [parent_slugs] names the entries that cite it. *)

type results = { ... }
(** The tiers of one search. [work] and [links] are ranked and cut to the
    caller's limits, [work_total] and [links_total] count the matches before
    the cut, and [kinds], [years] and [tags] count over every work match.
    [terms] is the query's words, lowercased, for marking matches. *)

val empty : results
(** [empty] is the result of a query that asked for nothing. *)

val search :
  t -> ?today:int * int * int -> ?limit:int -> ?link_limit:int ->
  string -> results
(** [search t ?today ?limit ?link_limit input] ranks what matches [input]
    in three strict tiers. Papers, notes, projects, ideas and videos are
    ordered by [bm25 × kind prior × freshness]. Links are ordered by
    [bm25 × freshness × citation bonus], deduplicated by normalised URL and
    by host and title, and never include the site's own host. [today]
    defaults to the current date and fixes freshness for tests. [limit]
    defaults to 20 and [link_limit] to 12. The syntax is as before: words,
    ["exact phrase"], [prefix*], [kind:paper] and [#tag]. A query with only
    filters browses the filtered set by date. *)

val kind_prior : string -> float
(** [kind_prior kind] is the multiplier the work tier applies to [kind]. *)

val freshness : today:int * int * int -> string -> float
(** [freshness ~today date] is between 1.0 and 1.25, largest for [today]
    and 1.0 from eight years before it. *)

val citation_bonus : int -> float
(** [citation_bonus n] is the multiplier for a link cited by [n] entries. *)

val normalise_url : string -> string
(** [normalise_url u] is [u] lowercased without scheme, leading [www.] or
    trailing [/] and [#]. Two links with one normalised URL are one page. *)

val host_of_url : string -> string
(** [host_of_url u] is the host of {!normalise_url}[ u]. *)

val pp_results : Format.formatter -> results -> unit
(** [pp_results ppf r] prints each tier under a heading. *)
```

- [ ] **Step 4: Run the library test to verify it passes**

Run: `dune build @avsm/arod/lib_search/all @avsm/arod/test/runtest --force 2>&1 | grep -v "arod_render\|arod_env\|main.ml\|test_json" | tail -20`
Expected: `test_search: 16 checks ok`. Other targets fail to compile, which Task 3 fixes. If the freshness check fails on rounding, compare with `Float.abs (x -. 1.25) < 1e-9`.

- [ ] **Step 5: Do not commit yet**

The tree does not build. Continue to Task 3.

---

### Task 3: JSON API, environment, handler parameters and CLI for the new record

**Files:**
- Modify: `lib_handlers/arod_render.ml:469-551`, `lib_handlers/arod_render.mli:109-112`
- Modify: `lib_handlers/arod_env.ml`, `lib_handlers/arod_env.mli`
- Modify: `lib_handlers/arod_handlers.ml:225-232`
- Modify: `bin/main.ml:83`, `bin/main.ml:144-180`
- Modify: `test/test_json.ml`, `test/test_routes.ml:105-140,300-310`

**Interfaces:**
- Consumes: `Arod_search.results`, `Arod_search.hit`, `Arod_search.goto` from Task 2.
- Produces: `Arod_render.search : ctx:Arod.Ctx.t -> Arod_search.results -> (Proffer.Body.Sink.t -> unit)`. `Arod_env.t.search : q:string -> limit:int -> link_limit:int -> (Proffer.Body.Sink.t -> unit) * int`. `Arod_env.create ~search:(limit:int -> link_limit:int -> string -> Arod_search.results)`.

- [ ] **Step 1: Update the JSON tests**

In `test/test_json.ml` replace the `result` helper and the first `let () =` block of search checks with:

```ocaml
let hit ?(kind = "note") ?(snippet = "s") ?(tags = []) ?(parent_slugs = [])
    slug : Arod_search.hit =
  {
    Arod_search.slug;
    kind;
    url = "/notes/" ^ slug;
    title = "T";
    snippet;
    date = "2024-02-03";
    tags;
    parent_slugs;
    score = 1.0;
  }

let results ?(goto = []) ?(links = []) ?(kinds = []) ?(years = [])
    ?(tags = []) work : Arod_search.results =
  {
    Arod_search.terms = [];
    goto;
    work;
    work_total = List.length work;
    links;
    links_total = List.length links;
    kinds;
    years;
    tags;
  }

let search r = render (Arod_handlers.Render.search ~ctx r)

let empty_tail = {|,"work_total":0,"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}|}

let () =
  eq "an empty result set is every member, empty"
    (search Arod_search.empty)
    ({|{"goto":[],"work":[]|} ^ empty_tail);
  eq "a bare hit omits tags, thumbnail and parents"
    (search (results [ hit "a-note" ]))
    ({|{"goto":[],"work":[{"slug":"a-note","kind":"note","url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03"}],"work_total":1,|}
     ^ {|"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}|});
  eq "tags are emitted when the entry has them"
    (search (results [ hit ~tags:[ "ocaml"; "mirage" ] "a-note" ]))
    ({|{"goto":[],"work":[{"slug":"a-note","kind":"note","url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03",|}
     ^ {|"tags":["ocaml","mirage"]}],"work_total":1,|}
     ^ {|"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}|});
  eq "a parent the context knows is expanded, one it does not is dropped"
    (search (results [ hit ~parent_slugs:[ "a-note"; "absent" ] "a-note" ]))
    ({|{"goto":[],"work":[{"slug":"a-note","kind":"note","url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03",|}
     ^ {|"parents":[{"slug":"a-note","title":"A Note",|}
     ^ {|"url":"/notes/a-note","kind":"note"}]}],"work_total":1,|}
     ^ {|"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}|});
  eq "go-to hits and facets are objects"
    (search
       (results
          ~goto:[ { Arod_search.label = "Papers"; url = "/papers";
                    detail = "section"; goto_kind = `Section } ]
          ~kinds:[ ("note", 2) ] ~years:[ (2024, 2) ] ~tags:[ ("ocaml", 1) ]
          []))
    ({|{"goto":[{"label":"Papers","url":"/papers","detail":"section",|}
     ^ {|"kind":"section"}],"work":[],"work_total":0,"links":[],|}
     ^ {|"links_total":0,"kinds":[{"kind":"note","count":2}],|}
     ^ {|"years":[{"year":2024,"count":2}],|}
     ^ {|"tags":[{"tag":"ocaml","count":1}]}|})
```

Update the snippet escaping check to wrap with `results [ hit ~snippet "a-note" ]` and expect the `{"goto":[],"work":[` prefix and the `],"work_total":1,"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}` suffix around the existing hit object.

- [ ] **Step 2: Update the route test stubs**

In `test/test_routes.ml` change the `search` stub to:

```ocaml
    search =
      (fun ~q ~limit ~link_limit ->
        let s = Printf.sprintf "%s/%d/%d" q limit link_limit in
        ((fun sink -> Proffer.Body.Sink.write sink s), String.length q));
```

and the checks near line 300 to:

```ocaml
  check "the search API reads its query"
    (body (get "/api/search?q=ocaml&limit=3&link_limit=2") = "ocaml/3/2");
  check "and logs before and after"
    (List.rev !searches = [ "ocaml/3/?"; "ocaml/3/5" ]);
  searches := [];
  check "an absent query is the empty string"
    (body (get "/api/search") = "/20/12");
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `dune build @avsm/arod/all 2>&1 | head -20`
Expected: compile errors in `arod_render.ml` about `Arod_search.result`.

- [ ] **Step 4: Update `Arod_render.search`**

In `lib_handlers/arod_render.ml`, after `Search_hit`, add:

```ocaml
module Search_goto = struct
  type t = { label : string; url : string; detail : string; kind : string }

  let codec =
    Jsont.Object.map ~kind:"goto" (fun label url detail kind ->
      { label; url; detail; kind })
    |> Jsont.Object.mem "label" Jsont.string ~enc:(fun g -> g.label)
    |> Jsont.Object.mem "url" Jsont.string ~enc:(fun g -> g.url)
    |> Jsont.Object.mem "detail" Jsont.string ~enc:(fun g -> g.detail)
    |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun g -> g.kind)
    |> Jsont.Object.finish
end

(* A facet is a name and a count. The member naming the thing counted
   differs per facet, so one codec is built per member name. *)
let count_codec ~kind name =
  Jsont.Object.map ~kind (fun k n -> (k, n))
  |> Jsont.Object.mem name Jsont.string ~enc:fst
  |> Jsont.Object.mem "count" Jsont.int ~enc:snd
  |> Jsont.Object.finish

let year_codec =
  Jsont.Object.map ~kind:"year" (fun y n -> (y, n))
  |> Jsont.Object.mem "year" Jsont.int ~enc:fst
  |> Jsont.Object.mem "count" Jsont.int ~enc:snd
  |> Jsont.Object.finish

module Search_response = struct
  type t = {
    goto : Search_goto.t list;
    work : Search_hit.t list;
    work_total : int;
    links : Search_hit.t list;
    links_total : int;
    kinds : (string * int) list;
    years : (int * int) list;
    tags : (string * int) list;
  }

  let codec =
    Jsont.Object.map ~kind:"results"
      (fun goto work work_total links links_total kinds years tags ->
        { goto; work; work_total; links; links_total; kinds; years; tags })
    |> Jsont.Object.mem "goto" (Jsont.list Search_goto.codec)
         ~enc:(fun r -> r.goto)
    |> Jsont.Object.mem "work" (Jsont.list Search_hit.codec)
         ~enc:(fun r -> r.work)
    |> Jsont.Object.mem "work_total" Jsont.int ~enc:(fun r -> r.work_total)
    |> Jsont.Object.mem "links" (Jsont.list Search_hit.codec)
         ~enc:(fun r -> r.links)
    |> Jsont.Object.mem "links_total" Jsont.int
         ~enc:(fun r -> r.links_total)
    |> Jsont.Object.mem "kinds" (Jsont.list (count_codec ~kind:"kind" "kind"))
         ~enc:(fun r -> r.kinds)
    |> Jsont.Object.mem "years" (Jsont.list year_codec) ~enc:(fun r -> r.years)
    |> Jsont.Object.mem "tags" (Jsont.list (count_codec ~kind:"tag" "tag"))
         ~enc:(fun r -> r.tags)
    |> Jsont.Object.finish
end
```

Delete `search_codec`. Rewrite `search`:

```ocaml
let search_hit ~ctx (r : Arod_search.hit) =
  let entries = Arod.Ctx.entries ctx in
  let parents = List.filter_map (fun slug ->
    match Arod.Ctx.lookup ctx slug with
    | Some ent ->
      Some {
        Search_parent.slug;
        title = Bushel.Entry.title ent;
        url = Bushel.Entry.site_url ent;
        kind = Bushel.Entry.to_type_string ent;
      }
    | None -> None
  ) r.parent_slugs in
  let thumbnail = match r.kind with
    | "link" ->
      (match Arod.Ctx.link_for_url ctx r.url with
       | Some link ->
         let meta = match link.karakeep with Some k -> k.metadata | None -> [] in
         (match List.assoc_opt "favicon" meta with
          | Some f when f <> "" -> Some f
          | _ -> None)
       | None -> None)
    | _ ->
      (match Arod.Ctx.lookup ctx r.slug with
       | Some ent -> Bushel.Entry.thumbnail entries ent
       | None -> None)
  in
  { Search_hit.slug = r.slug; kind = r.kind; url = r.url; title = r.title;
    snippet = r.snippet; date = r.date; tags = r.tags; thumbnail; parents }

let goto_kind_string = function
  | `Section -> "section" | `Project -> "project" | `Tag -> "tag"

let search ~ctx (r : Arod_search.results) =
  let goto = List.map (fun (g : Arod_search.goto) ->
    { Search_goto.label = g.label; url = g.url; detail = g.detail;
      kind = goto_kind_string g.goto_kind }) r.goto in
  Arod_json.stream Search_response.codec
    { Search_response.goto; work = List.map (search_hit ~ctx) r.work;
      work_total = r.work_total; links = List.map (search_hit ~ctx) r.links;
      links_total = r.links_total; kinds = r.kinds; years = r.years;
      tags = r.tags }
```

In `lib_handlers/arod_render.mli`:

```ocaml
val search :
  ctx:Arod.Ctx.t -> Arod_search.results -> (Proffer.Body.Sink.t -> unit)
(** [search ~ctx results] writes [results] as the JSON the search page
    reads, streamed as {!pagination} is. *)
```

- [ ] **Step 5: Update `Arod_env`**

In both `arod_env.ml` and `arod_env.mli`, the field becomes:

```ocaml
  search :
    q:string -> limit:int -> link_limit:int ->
    (Proffer.Body.Sink.t -> unit) * int;
```

with the `.mli` doc:

```ocaml
      (** [search ~q ~limit ~link_limit] is the tiers for [q], at most
          [limit] work hits and [link_limit] links, as the JSON the search
          page reads, paired with the number of hits in both tiers. An empty
          [q] is an empty result set and queries nothing. *)
```

`create`'s `search` argument becomes `search:(limit:int -> link_limit:int -> string -> Arod_search.results)` in both files, and the body:

```ocaml
    search =
      (fun ~q ~limit ~link_limit ->
        if String.equal q "" then
          (Arod_render.search ~ctx Arod_search.empty, 0)
        else
          let r = search ~limit ~link_limit q in
          (Arod_render.search ~ctx r,
           List.length r.work + List.length r.links));
```

- [ ] **Step 6: Update the handler and the CLI**

In `lib_handlers/arod_handlers.ml`:

```ocaml
let search_api env req respond =
  let q = match Req.query_param req "q" with Some q -> q | None -> "" in
  let limit = int_param req "limit" ~default:20 ~lo:1 ~hi:100 in
  let link_limit = int_param req "link_limit" ~default:12 ~lo:1 ~hi:100 in
  env.E.log_search ~query:q ~limit ~results:None;
  let write, results = env.E.search ~q ~limit ~link_limit in
  env.E.log_search ~query:q ~limit ~results:(Some results);
  Resp.stream respond json_type write
```

In `bin/main.ml` line 83:

```ocaml
        ~search:(fun ~limit ~link_limit q ->
          Arod_search.search search ~limit ~link_limit q)
```

and in `search_cmd` replace the block from `let results = ...` to the end of the `else` branch with:

```ocaml
      let results = Arod_search.search search ?limit input in
      if results = Arod_search.empty then begin
        Printf.printf "No results.\n";
        0
      end else begin
        Fmt.pr "%a@." Arod_search.pp_results results;
        0
      end
```

- [ ] **Step 7: Build and run every arod test**

Run: `dune build @avsm/arod/all @avsm/arod/runtest --force 2>&1 | tail -8`
Expected: clean, with `test_search: 16 checks ok`, `test_json: N checks ok` and `test_routes` passing.

- [ ] **Step 8: Try it on the real index**

Run: `dune exec -- avsm/arod/bin/main.exe index && dune exec -- avsm/arod/bin/main.exe search ocaml 2>/dev/null | head -30`
Expected: a `== on this site` heading, notes and papers first, then `== links`. The index command rebuilds `~/.cache/arod/search.db` so the CLI sees the new row shape.

- [ ] **Step 9: Commit Tasks 2 and 3 together**

```bash
git add avsm/arod/lib_search avsm/arod/lib_handlers avsm/arod/bin/main.ml avsm/arod/test
git commit -m "Rank search results in tiers of local work then cited links"
```

---

### Task 4: Go-to tier and facets

**Files:**
- Modify: `lib_search/arod_search.ml`, `lib_search/arod_search.mli`
- Modify: `test/test_search.ml`

**Interfaces:**
- Consumes: `results` from Task 2.
- Produces: `goto`, `kinds`, `years`, `tags` populated. `t` gains `mutable tag_counts : (string * int) list` and `mutable projects : (string * string * string * string) list` (slug, title, url, date), both set by `index`.

- [ ] **Step 1: Write the failing tests**

Append to `test/test_search.ml` before the final print:

```ocaml
let project ~slug ~title ~start : Bushel.Project.t =
  { Bushel.Project.slug; title; start; finish = None; tags = []; ideas = "";
    body = "Body."; social = None }

let () =
  let t =
    Eio_main.run @@ fun _env ->
    Eio.Switch.run @@ fun sw ->
    let t = Arod_search.create_memory ~sw () in
    Arod_search.index t ~own_host:""
      ~contact_name:(fun _ -> None)
      ~entries:
        [
          `Project (project ~slug:"ocamllabs" ~title:"OCaml Labs" ~start:2012);
          `Note (note ~slug:"n1" ~date:(2020, 1, 1) ~tags:[ "ocaml"; "eio" ]
                   ~title:"OCaml one" "x");
          `Note (note ~slug:"n2" ~date:(2021, 1, 1) ~tags:[ "ocaml" ]
                   ~title:"OCaml two" "x");
          `Note (note ~slug:"n3" ~date:(2022, 6, 1) ~tags:[ "ocaml-labs" ]
                   ~title:"Other" "x");
        ]
      ~links:[ link ~title:"OCaml site" ~slugs:[ "n1" ] "https://ocaml.org" ];
    t
  in
  let r = Arod_search.search t ~today "ocaml" in
  let gotos = List.map (fun (g : Arod_search.goto) ->
      (g.goto_kind, g.label, g.url, g.detail)) r.goto in
  check "a project whose title starts with the word is a go-to hit"
    (List.mem (`Project, "OCaml Labs", "/projects/ocamllabs", "2012 project")
       gotos);
  check "a tag is a go-to hit with its entry count, most used first"
    (List.filter (fun (k, _, _, _) -> k = `Tag) gotos
     = [ (`Tag, "ocaml", "/#tag=ocaml", "2 entries");
         (`Tag, "ocaml-labs", "/#tag=ocaml-labs", "1 entry") ]);
  check "projects come before tags"
    (match gotos with (`Project, _, _, _) :: _ -> true | _ -> false);
  (* n3 matches through its tag, since the tags column tokenises
     "ocaml-labs" into two words. A project indexes at its start year. *)
  check "kinds count work matches per kind, by name"
    (r.kinds = [ ("note", 3); ("project", 1) ]);
  check "years count work matches ascending"
    (r.years = [ (2012, 1); (2020, 1); (2021, 1); (2022, 1) ]);
  check "tags are the most used among work matches, then by name"
    (r.tags = [ ("ocaml", 2); ("eio", 1); ("ocaml-labs", 1) ]);
  let r = Arod_search.search t ~today "pap" in
  check "a section matches on a prefix of its name"
    (List.exists (fun (g : Arod_search.goto) ->
         g.goto_kind = `Section && g.url = "/papers") r.goto);
  let r = Arod_search.search t ~today "lab" in
  check "a project matches on a prefix of any title word"
    (List.exists (fun (g : Arod_search.goto) ->
         g.goto_kind = `Project) r.goto)
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `dune build @avsm/arod/test/runtest --force 2>&1 | grep FAIL`
Expected: `FAIL: a project whose title starts with the word is a go-to hit`.

- [ ] **Step 3: Implement go-to matching and facets**

In `lib_search/arod_search.ml`, extend `t`:

```ocaml
type t = {
  db : Sqlite3_eio.t;
  mutable own_host : string;
  mutable tag_counts : (string * int) list;
  mutable projects : (string * string * string * string) list;
}
```

and initialise `tag_counts = []; projects = []` in the three constructors. At the end of `index`, after the row-count logging:

```ocaml
  t.tag_counts <- load_tag_counts t;
  t.projects <- load_projects t
```

with, placed before `index`:

```ocaml
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
```

Add the go-to and facet logic before `search`:

```ocaml
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
             String.starts_with ~prefix:term slug
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
        then Some { label = tag; url = "/#tag=" ^ tag;
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
      Hashtbl.replace tbl k (1 + Option.value ~default:0 (Hashtbl.find_opt tbl k)))
      (key h)) hits;
  Hashtbl.fold (fun k n acc -> (k, n) :: acc) tbl []

let facets work =
  let kinds = count_by (fun h -> [h.kind]) work
              |> List.sort (fun (a, _) (b, _) -> compare a b) in
  let years = count_by (fun h ->
      match int_of_string_opt (String.sub h.date 0 (min 4 (String.length h.date)))
      with Some y -> [y] | None -> []) work
    |> List.sort (fun (a, _) (b, _) -> compare a b) in
  let tags = count_by (fun h -> h.tags) work
    |> List.sort (fun (a, n) (b, m) ->
         match compare m n with 0 -> compare a b | c -> c)
    |> take 8 in
  (kinds, years, tags)
```

In `search`, change `finish` and `browse` to fill the fields:

```ocaml
  let finish hits =
    let work, links = split_tiers hits in
    let work = rank_work ~today work in
    let links = rank_links ~today ~own_host:t.own_host links in
    let kinds, years, tags = facets work in
    { terms; goto = goto_hits t terms; work = take limit work;
      work_total = List.length work; links = take link_limit links;
      links_total = List.length links; kinds; years; tags }
```

`browse` keeps `goto = []` and fills `kinds, years, tags` the same way.

In the `.mli`, document `goto`:

```ocaml
type goto = { ... }
(** A page the query names rather than describes: a section, a project or a
    tag whose name starts with a query word. [detail] is the short line
    shown beside it, such as ["2012 project"] or ["257 entries"]. *)
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `dune build @avsm/arod/all @avsm/arod/runtest --force 2>&1 | tail -5`
Expected: `test_search: 24 checks ok`, everything else clean.

- [ ] **Step 5: Commit**

```bash
git add avsm/arod/lib_search avsm/arod/test/test_search.ml
git commit -m "Add go-to hits and facets to search results"
```

---

### Task 5: The search page component

**Files:**
- Create: `lib_component/search.ml`
- Modify: `lib_component/dune`, `lib_component/arod_component.ml`
- Modify: `lib_component/theme.ml`
- Create: `test/test_search_page.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: `Arod_search.results`, `Arod_search.hit`, `Arod_search.goto`.
- Produces:

```ocaml
(* lib_component/search.ml *)
val favicon_for : ctx:Arod.Ctx.t -> string -> string option
val mark : terms:string list -> string -> Htmlit.El.html
val fragment :
  ctx:Arod.Ctx.t -> q:string -> Arod_search.results -> Htmlit.El.html
val page_body :
  ctx:Arod.Ctx.t -> q:string -> Arod_search.results ->
  Htmlit.El.html * Htmlit.El.html
```

`fragment` is the `<div id="search-results">` holding the count line, go-to chips, the work list, and the rail as a second child, so one fetch replaces both columns. `page_body` is `(article, sidebar)`: the article holds the input form and the main column of the fragment, the sidebar holds the rail. Since a fragment must replace both columns in one swap, the page body nests as follows: `article = form; div#search-results[main, rail]`, and the sidebar is `El.void`. The rail is laid out beside the main column by `sp-grid` CSS, not by the layout's flex.

- [ ] **Step 1: Write the failing test**

Create `test/test_search_page.ml`:

```ocaml
(* The search page markup is pinned by shape, not byte: each check names one
   element the JavaScript or the reader relies on. *)

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let cfg : Arod.Config.t =
  { Arod.Config.default with
    site = { Arod.Config.default.site with base_url = "https://example.com" } }

let note : Bushel.Note.t =
  { Bushel.Note.title = "Xen Hypervisor"; date = (2024, 2, 3); slug = "xen";
    body = "Body."; tags = [ "xen" ]; draft = false; updated = None;
    sidebar = None; index_page = false; perma = false; weeknote = false;
    featured = false; doi = None; synopsis = None; titleimage = None;
    via = None; slug_ent = None; source = None; url = None; author = None;
    category = None; standardsite = None; social = None; source_file = None }

let ctx =
  Arod.Ctx.of_entries ~config:cfg
    (Bushel.Entry.v ~papers:[] ~notes:[ note ] ~projects:[] ~ideas:[]
       ~videos:[] ~contacts:[] ~data_dir:"." ())

let hit ?(kind = "note") ?(url = "/notes/xen") ?(parent_slugs = [])
    ?(tags = []) title : Arod_search.hit =
  { Arod_search.slug = "xen"; kind; url; title; snippet = "<b>Xen</b> body";
    date = "2024-02-03"; tags; parent_slugs; score = 3.0 }

let results : Arod_search.results =
  { Arod_search.terms = [ "xen" ];
    goto = [ { Arod_search.label = "xen"; url = "/#tag=xen";
               detail = "1 entry"; goto_kind = `Tag } ];
    work = [ hit ~tags:[ "xen"; "systems" ] "Xen Hypervisor" ];
    work_total = 30;
    links = [ hit ~kind:"link" ~url:"https://wiki.xen.org/XenStore"
                ~parent_slugs:[ "xen" ] "XenStore - Xen" ];
    links_total = 5;
    kinds = [ ("note", 30) ]; years = [ (2024, 30) ];
    tags = [ ("xen", 29) ] }

let html = Htmlit.El.to_string ~doctype:false

let () =
  let f = html (Arod_component.Search.fragment ~ctx ~q:"xen" results) in
  check "the fragment has the id the script swaps"
    (contains f {|id="search-results"|});
  check "the count line states both totals"
    (contains f "30 on this site" && contains f "5 links");
  check "a go-to chip links to the tag filter"
    (contains f {|href="/#tag=xen"|} && contains f "1 entry");
  check "a work row marks the matched title word"
    (contains f "<b>Xen</b> Hypervisor");
  check "a work row shows its tags"
    (contains f "#systems");
  check "a link row falls back to the glyph when there is no favicon"
    (contains f {|class="sp-fav"|} && not (contains f "<img"));
  check "a link row names the citing entry"
    (contains f {|<span class="sp-via-in">in </span>Xen Hypervisor</span>|});
  check "the link row shows the host"
    (contains f "wiki.xen.org");
  check "a truncated tier offers more"
    (contains f "Show 29 more" && contains f "Show 4 more");
  check "facets carry the filter to add"
    (contains f {|data-kind="note"|} && contains f {|data-tag="xen"|});
  check "the histogram has a bar per year"
    (contains f {|class="sp-year hot"|})

let () =
  let article, _ = Arod_component.Search.page_body ~ctx ~q:"xen" results in
  let a = html article in
  check "the page has a form that submits q to /search"
    (contains a {|action="/search"|} && contains a {|name="q"|}
    && contains a {|value="xen"|});
  check "and contains the fragment" (contains a {|id="search-results"|});
  let article, _ =
    Arod_component.Search.page_body ~ctx ~q:"" Arod_search.empty in
  check "an empty query shows the prompt and autofocuses"
    (contains (html article) "autofocus"
    && contains (html article) "Type to search")

let () =
  check "mark wraps a word starting with a term"
    (html (Arod_component.Search.mark ~terms:[ "uni" ] "The Unikernel way")
     = "The <b>Unikernel</b> way");
  check "mark escapes what it does not wrap"
    (html (Arod_component.Search.mark ~terms:[] "a < b") = "a &lt; b")

let () = Printf.printf "test_search_page: %d checks ok\n" !checks
```

Add to `test/dune`:

```
(test
 (name test_search_page)
 (modules test_search_page)
 (libraries arod arod.component arod.search bushel htmlit))
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `dune build @avsm/arod/test/runtest --force 2>&1 | head -5`
Expected: `Unbound module Arod_component.Search`.

- [ ] **Step 3: Implement the component**

Add `arod.search` to `lib_component/dune`'s libraries. Check `lib_component/arod_component.ml` for how modules are re-exported (it aliases each component module) and add `module Search = Search`.

Create `lib_component/search.ml`:

```ocaml
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

let plural n one many = Printf.sprintf "%d %s" n (if n = 1 then one else many)

let section_head label total =
  El.div ~at:[At.class' "sp-sec-h"]
    [ El.span ~at:[At.class' "sp-eyebrow"] [El.txt label];
      El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int total)] ]

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
      [ El.div ~at:[At.class' "sp-sec-h"]
          [El.span ~at:[At.class' "sp-eyebrow"] [El.txt "Go to"]];
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
    | slug :: rest ->
      let title = Bushel.Entry.lookup_title (Arod.Ctx.entries ctx) slug in
      let extra = if rest = [] then "" else
          Printf.sprintf " +%d" (List.length rest) in
      El.span ~at:[At.class' "sp-via"]
        [ El.span ~at:[At.class' "sp-via-in"] [El.txt "in "];
          El.txt (title ^ extra) ]
  in
  El.a ~at:[At.href h.url; At.class' "sp-hit sp-link"; At.v "rel" "noopener"]
    [ El.span ~at:[At.class' "sp-fav"] [fav];
      El.span ~at:[At.class' "sp-body"]
        [ El.span ~at:[At.class' "sp-line"]
            [ El.span ~at:[At.class' "sp-t"] [mark ~terms h.title];
              El.span ~at:[At.class' "sp-d"]
                [El.txt (String.sub h.date 0 (min 7 (String.length h.date)))] ];
          El.span ~at:[At.class' "sp-meta"]
            [ El.span ~at:[At.class' "sp-dom"] [El.txt (host h.url)]; via ] ] ]

let kind_label = function
  | "paper" -> "Papers" | "note" -> "Notes" | "project" -> "Projects"
  | "idea" -> "Ideas" | "video" -> "Talks" | "link" -> "Links"
  | k -> k

let has_filter ~q prefix v =
  List.mem (prefix ^ v) (String.split_on_char ' ' q)

let facets ~q (r : S.results) =
  let kind_chip (k, n) =
    El.button ~at:[At.class' ("sp-f" ^ (if has_filter ~q "kind:" k then " on" else ""));
                   At.v "data-kind" k]
      [ El.txt (kind_label k); El.txt " ";
        El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int n)] ]
  in
  let tag_chip (t, n) =
    El.button ~at:[At.class' ("sp-f" ^ (if has_filter ~q "#" t then " on" else ""));
                   At.v "data-tag" t]
      [ El.txt ("#" ^ t); El.txt " ";
        El.span ~at:[At.class' "sp-n"] [El.txt (string_of_int n)] ]
  in
  if r.kinds = [] && r.tags = [] then El.void
  else
    El.div ~at:[At.class' "sp-sec"]
      [ El.div ~at:[At.class' "sp-sec-h"]
          [El.span ~at:[At.class' "sp-eyebrow"] [El.txt "Narrow"]];
        El.div ~at:[At.class' "sp-facets"] (List.map kind_chip r.kinds);
        El.div ~at:[At.class' "sp-facets"] (List.map tag_chip r.tags) ]

let histogram (r : S.results) =
  match r.years with
  | [] | [ _ ] -> El.void
  | years ->
    let lo = fst (List.hd years) and hi = fst (List.hd (List.rev years)) in
    let max_n = List.fold_left (fun m (_, n) -> max m n) 1 years in
    let bars = List.init (hi - lo + 1) (fun i ->
      let y = lo + i in
      let n = Option.value ~default:0 (List.assoc_opt y years) in
      let cls = if n = max_n then "sp-year hot" else "sp-year" in
      let label = if y = lo || y = hi then [El.span [El.txt (string_of_int y)]]
        else [] in
      El.div ~at:[At.class' cls;
                  At.style (Printf.sprintf "height:%d%%" (max 4 (100 * n / max_n)));
                  At.title (Printf.sprintf "%d: %d" y n)] label)
    in
    El.div ~at:[At.class' "sp-years"] bars

let rail ~ctx ~q (r : S.results) =
  let links = match r.links with
    | [] -> El.void
    | ls ->
      El.div ~at:[At.class' "sp-sec"]
        ([ section_head "Links cited on this site" r.links_total ]
         @ List.map (link_row ~ctx ~terms:r.terms) ls
         @ [ more ~shown:(List.length ls) ~total:r.links_total
               ~param:"link_limit" ])
  in
  El.aside ~at:[At.class' "sp-rail"] [ facets ~q r; histogram r; links ]

let main_column ~q (r : S.results) =
  let work = match r.work with
    | [] -> El.void
    | ws ->
      El.div ~at:[At.class' "sp-sec"]
        ([ section_head "On this site" r.work_total ]
         @ List.map (work_row ~terms:r.terms) ws
         @ [ more ~shown:(List.length ws) ~total:r.work_total ~param:"limit" ])
  in
  let count =
    El.div ~at:[At.class' "sp-count"]
      [ El.txt (Printf.sprintf "%d on this site · %d links"
                  r.work_total r.links_total) ]
  in
  El.div ~at:[At.class' "sp-main"] [ count; goto_section r; work ]

let empty_state ~q =
  let msg =
    if q = "" then
      "Type to search. Results group by how close they are to this site: \
       pages and tags first, then papers, notes, projects, ideas and talks, \
       then the links they cite."
    else Printf.sprintf "Nothing matches \"%s\"." q
  in
  El.div ~at:[At.class' "sp-empty"] [El.txt msg]

let fragment ~ctx ~q (r : S.results) =
  let body =
    if r.goto = [] && r.work = [] && r.links = [] then [ empty_state ~q ]
    else [ main_column ~q r; rail ~ctx ~q r ]
  in
  El.div ~at:[At.id "search-results"; At.class' "sp-grid"] body

let page_body ~ctx ~q r =
  let form =
    El.form ~at:[At.action "/search"; At.method' "get"; At.class' "sp-form";
                 At.v "role" "search"]
      [ El.span ~at:[At.class' "sp-prompt"] [El.txt ">_"];
        El.input ~at:([ At.id "search-input"; At.type' "search"; At.name "q";
                        At.value q; At.autocomplete "off";
                        At.v "placeholder" "Search papers, notes, projects, links" ]
                      @ (if q = "" then [At.autofocus] else [])) () ]
  in
  (El.div ~at:[At.class' "sp-page"] [ form; fragment ~ctx ~q r ], El.void)
```

These exist and were checked against the sources: `Common.take` (`common.ml:41`), `Bushel.Entry.lookup_title`, and in the vendored Htmlit `At.method'`, `At.alt`, `At.width`, `At.title`, `At.style`, `At.autofocus`, `El.splice`, `El.form`, `El.button`, `El.aside`, `El.b`, `El.img`, `El.input`. `At.v name value` builds any other attribute.

- [ ] **Step 4: Add the CSS**

In `lib_component/theme.ml`, inside `custom_css`, after the `.search-modal` block, add:

```css
  /* Search page */
  .sp-page { max-width: 72rem; margin: 0 auto; }
  .sp-form { display: flex; align-items: center; gap: 0.5rem; border: 1px solid var(--color-border-faint); border-radius: 6px; padding: 0.4rem 0.7rem; margin-bottom: 0.9rem; }
  .sp-form:focus-within { border-color: var(--color-accent); }
  .sp-prompt { color: var(--color-accent); font-family: ui-monospace, 'SF Mono', monospace; font-weight: 600; font-size: 0.85rem; }
  .sp-form input { flex: 1; border: 0; outline: 0; background: transparent; color: var(--color-text); font: inherit; font-size: 1rem; }
  .sp-grid { display: grid; grid-template-columns: minmax(0, 1fr) 20rem; gap: 2rem; }
  @media (max-width: 56rem) { .sp-grid { grid-template-columns: 1fr; } }
  .sp-count { font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.78rem; color: var(--color-secondary); margin-bottom: 0.6rem; }
  .sp-sec { margin-bottom: 1.1rem; }
  .sp-sec-h { display: flex; justify-content: space-between; align-items: baseline; padding: 0.2rem 0; }
  .sp-eyebrow { font-size: 0.66rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--color-muted); }
  .sp-n { font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.68rem; color: var(--color-faint); }
  .sp-gotos { display: flex; flex-wrap: wrap; gap: 0.4rem; padding: 0.25rem 0 0.45rem; }
  .sp-goto { display: inline-flex; align-items: center; gap: 0.35rem; padding: 0.25rem 0.6rem; border: 1px solid var(--color-border); border-radius: 6px; font-size: 0.82rem; text-decoration: none !important; color: var(--color-text) !important; }
  .sp-goto .sp-t { font-weight: 500; }
  .sp-goto .sp-sub { font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.68rem; color: var(--color-muted); }
  .sp-goto:hover, .sp-goto.selected { border-color: var(--color-accent); background: var(--color-surface); }
  .sp-hit { text-decoration: none !important; color: inherit !important; }
  .sp-work, .sp-link { display: flex; gap: 0.55rem; padding: 0.4rem 0.5rem; margin-left: -0.5rem; border-left: 2px solid transparent; border-radius: 4px; }
  .sp-work:hover, .sp-work.selected, .sp-link:hover, .sp-link.selected { background: var(--color-surface); border-left-color: var(--color-accent); }
  .sp-ic, .sp-fav { width: 1.3rem; height: 1.3rem; display: inline-flex; align-items: center; justify-content: center; flex-shrink: 0; margin-top: 0.1rem; color: var(--color-secondary); }
  .sp-fav img { width: 16px; height: 16px; border-radius: 3px; }
  .sp-ic-paper { color: #3b82f6; } .sp-ic-note { color: #10b981; } .sp-ic-project { color: #8b5cf6; }
  .sp-ic-idea { color: #f59e0b; } .sp-ic-video { color: #ef4444; }
  .sp-body { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 0.1rem; }
  .sp-line { display: flex; align-items: baseline; gap: 0.5rem; min-width: 0; }
  .sp-line .sp-t { font-weight: 500; font-size: 0.88rem; flex: 1; min-width: 0; }
  .sp-link .sp-t { font-size: 0.82rem; }
  .sp-d { font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.7rem; color: var(--color-muted); flex-shrink: 0; }
  .sp-snip { font-size: 0.78rem; color: var(--color-secondary); line-height: 1.4; overflow: hidden; display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; }
  .sp-snip b, .sp-t b { background: var(--color-highlight); font-weight: 600; border-radius: 2px; }
  .sp-tags { display: flex; flex-wrap: wrap; gap: 0.25rem; }
  .sp-tags span { font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.64rem; color: var(--color-muted); }
  .sp-meta { display: flex; gap: 0.6rem; align-items: baseline; min-width: 0; font-size: 0.7rem; }
  .sp-dom { font-family: ui-monospace, 'SF Mono', monospace; color: var(--color-muted); flex-shrink: 0; }
  .sp-via { color: var(--color-secondary); min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  .sp-via-in { color: var(--color-muted); }
  .sp-more { display: block; padding: 0.3rem 0; font: inherit; font-size: 0.76rem; color: var(--color-link); background: transparent; border: 0; cursor: pointer; }
  .sp-more:hover { text-decoration: underline; }
  .sp-rail { border-left: 1px solid var(--color-border); padding-left: 1.25rem; font-size: 0.82rem; }
  @media (max-width: 56rem) { .sp-rail { border-left: 0; padding-left: 0; border-top: 1px solid var(--color-border); padding-top: 1rem; } }
  .sp-facets { display: flex; flex-wrap: wrap; gap: 0.3rem; padding: 0.2rem 0 0.5rem; }
  .sp-f { display: inline-flex; gap: 0.3rem; align-items: baseline; font: inherit; font-size: 0.74rem; padding: 0.1rem 0.5rem; border: 1px solid var(--color-border); border-radius: 999px; color: var(--color-secondary); cursor: pointer; background: var(--color-bg); }
  .sp-f:hover, .sp-f.on { border-color: var(--color-accent); color: var(--color-text); }
  .sp-years { display: flex; align-items: flex-end; gap: 2px; height: 2.2rem; margin: 0.2rem 0 1.2rem; }
  .sp-year { flex: 1; background: var(--color-border-faint); border-radius: 1px 1px 0 0; position: relative; min-height: 2px; }
  .sp-year.hot { background: var(--color-accent); }
  .sp-year span { position: absolute; bottom: -0.95rem; left: 0; font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.58rem; color: var(--color-faint); }
  .sp-empty { padding: 2rem 1rem; text-align: center; color: var(--color-secondary); font-size: 0.85rem; grid-column: 1 / -1; }
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `dune build @avsm/arod/all @avsm/arod/runtest --force 2>&1 | tail -5`
Expected: `test_search_page: 16 checks ok`. Adjust the `in </span>Xen Hypervisor` expectation to what Htmlit emits for the `sp-via` span, but only its serialisation, not the content.

- [ ] **Step 6: Commit**

```bash
git add avsm/arod/lib_component avsm/arod/test/test_search_page.ml avsm/arod/test/dune
git commit -m "Add the search page component"
```

---

### Task 6: The `/search` route, fragment endpoint and page script

**Files:**
- Modify: `lib_handlers/arod_render.ml`, `lib_handlers/arod_render.mli`
- Modify: `lib_handlers/arod_env.ml`, `lib_handlers/arod_env.mli`
- Modify: `lib_handlers/arod_handlers.ml`, `lib_handlers/arod_handlers.mli`
- Modify: `lib/server/arod_site.ml:96-106`
- Modify: `lib_component/layout.ml:332-347`, `lib_component/scripts.ml`
- Modify: `bin/main.ml`
- Modify: `test/test_routes.ml`

**Interfaces:**
- Consumes: `Arod_component.Search.page_body`, `.fragment` from Task 5.
- Produces: `Arod_render.search_page : ctx:Arod.Ctx.t -> q:string -> fragment:bool -> Arod_search.results -> string`. `Arod_env.t.search_page : q:string -> limit:int -> link_limit:int -> fragment:bool -> string`. `Arod_handlers.search_page : handler`. `Layout.page_script` gains `Search`, served as `search.js`.

- [ ] **Step 1: Write the failing route tests**

In `test/test_routes.ml`, add to the `env` record after `search`:

```ocaml
    search_page =
      (fun ~q ~limit ~link_limit ~fragment ->
        Printf.sprintf "page:%s/%d/%d/%b" q limit link_limit fragment);
```

and after the search API checks:

```ocaml
  check "the search page reads its query and limits"
    (body (get "/search?q=xen&limit=5&link_limit=3") = "page:xen/5/3/false");
  check "a fragment request is marked"
    (body (get "/search?q=xen&fragment=1") = "page:xen/20/12/true");
  check "the search page is HTML"
    (header (get "/search") H.Content_type
     = Some "text/html; charset=utf-8");
  check "the page script is served"
    (contains (body (get "/js/search.js")) "search-results")
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `dune build @avsm/arod/test/runtest --force 2>&1 | head -5`
Expected: a compile error, `search_page` is not a field of `Arod_handlers.Env.t`.

- [ ] **Step 3: Add the render**

In `lib_handlers/arod_render.ml` after `search`:

```ocaml
let search_page ~ctx ~q ~fragment (r : Arod_search.results) =
  if fragment then
    Htmlit.El.to_string ~doctype:false
      (C.Search.fragment ~ctx ~q r)
  else
    let article, _sidebar = C.Search.page_body ~ctx ~q r in
    let title = if q = "" then "Search" else "Search: " ^ q in
    C.Layout.page ~ctx ~title ~description:"Search this site" ~url:"/search"
      ~current_page:"Search" ~page_scripts:[Search] ~main_cls:"max-w-6xl"
      ~article ()
```

`page_body` returns `El.void` as its sidebar, which is not passed: the rail is laid out by the fragment's own grid, and `max-w-6xl` is the utility the notes list already uses to widen `main.prose`, so no new Tailwind class is introduced. In the `.mli`:

```ocaml
val search_page :
  ctx:Arod.Ctx.t -> q:string -> fragment:bool -> Arod_search.results -> string
(** [search_page ~ctx ~q ~fragment r] is the search page for [q] showing
    [r], or with [fragment] only the results region the page script swaps
    in. *)
```

- [ ] **Step 4: Add the page script**

In `lib_component/layout.ml`, add `| Search` to `page_script` and `| Search -> "search.js"` to `script_file_of`. In `lib_component/scripts.ml`, add before `by_name`:

```ocaml
let search_js = {|
// Search page: live results as you type, facets, keyboard selection
(function() {
  var input = document.getElementById('search-input');
  var form = input && input.closest('form');
  if (!input) return;
  var limits = { limit: 20, link_limit: 12 };
  var timer = null, sel = -1, controller = null;

  function results() { return document.getElementById('search-results'); }
  function hits() { return results().querySelectorAll('.sp-hit'); }

  function url(q) {
    return '/search?q=' + encodeURIComponent(q)
      + '&limit=' + limits.limit + '&link_limit=' + limits.link_limit;
  }

  function load(q) {
    if (controller) controller.abort();
    controller = new AbortController();
    fetch(url(q) + '&fragment=1', { signal: controller.signal })
      .then(function(r) { return r.text(); })
      .then(function(html) {
        var box = results();
        box.outerHTML = html;
        sel = -1;
        history.replaceState(null, '', q ? '/search?q=' + encodeURIComponent(q) : '/search');
      })
      .catch(function() {});
  }

  function search() {
    limits = { limit: 20, link_limit: 12 };
    load(input.value.trim());
  }

  input.addEventListener('input', function() {
    clearTimeout(timer);
    timer = setTimeout(search, 120);
  });
  if (form) form.addEventListener('submit', function(e) { e.preventDefault(); search(); });

  function select(i) {
    var hs = hits();
    if (!hs.length) return;
    if (sel >= 0 && sel < hs.length) hs[sel].classList.remove('selected');
    sel = (i + hs.length) % hs.length;
    hs[sel].classList.add('selected');
    hs[sel].scrollIntoView({ block: 'nearest' });
  }

  input.addEventListener('keydown', function(e) {
    if (e.key === 'ArrowDown') { e.preventDefault(); select(sel + 1); }
    else if (e.key === 'ArrowUp') { e.preventDefault(); select(sel - 1); }
    else if (e.key === 'Enter' && sel >= 0) {
      e.preventDefault();
      var h = hits()[sel];
      if (h) window.location.href = h.getAttribute('href');
    }
    else if (e.key === 'Escape') { input.value = ''; search(); }
  });

  function toggleWord(word) {
    var words = input.value.trim().split(/\s+/).filter(Boolean);
    var i = words.indexOf(word);
    if (i >= 0) words.splice(i, 1); else words.push(word);
    input.value = words.join(' ');
    search();
    input.focus();
  }

  document.addEventListener('click', function(e) {
    var more = e.target.closest('[data-more]');
    if (more) {
      var p = more.getAttribute('data-more');
      limits[p] = limits[p] * 2;
      load(input.value.trim());
      return;
    }
    var k = e.target.closest('[data-kind]');
    if (k) { toggleWord('kind:' + k.getAttribute('data-kind')); return; }
    var t = e.target.closest('[data-tag]');
    if (t) { toggleWord('#' + t.getAttribute('data-tag')); }
  });
})();
|}
```

and register `"search.js", search_js;` in `by_name`. Leave the modal's `search_js` in `site.js` for now, Task 7 removes it. Rename the new one `search_page_js` if the name collides, and use that name in `by_name`.

- [ ] **Step 5: Add the environment closure, handler and route**

In `arod_env.mli` and `arod_env.ml` add the field after `search`:

```ocaml
  search_page :
    q:string -> limit:int -> link_limit:int -> fragment:bool -> string;
      (** [search_page ~q ~limit ~link_limit ~fragment] is the search page
          for [q], or with [fragment] only its results region. It is a
          closure because the search handle is bound to the domain that
          built this record. *)
```

In `create`:

```ocaml
    search_page =
      (fun ~q ~limit ~link_limit ~fragment ->
        let r =
          if String.equal q "" then Arod_search.empty
          else search ~limit ~link_limit q
        in
        Arod_render.search_page ~ctx ~q ~fragment r);
```

In `arod_handlers.ml` after `search_api`:

```ocaml
let search_page env req respond =
  let q = match Req.query_param req "q" with Some q -> q | None -> "" in
  let limit = int_param req "limit" ~default:20 ~lo:1 ~hi:100 in
  let link_limit = int_param req "link_limit" ~default:12 ~lo:1 ~hi:100 in
  let fragment = Req.query_param req "fragment" = Some "1" in
  Resp.html respond (env.E.search_page ~q ~limit ~link_limit ~fragment)
```

and in `arod_handlers.mli` next to `search_api`:

```ocaml
val search_page : handler
(** The search page at [/search], or its results fragment when the query
    string carries [fragment=1]. *)
```

In `lib/server/arod_site.ml` before the `api` routes:

```ocaml
    get (s "search" /? nil) H.search_page;
```

- [ ] **Step 6: Build, run the tests, and try the page**

Run: `dune build @avsm/arod/all @avsm/arod/runtest --force 2>&1 | tail -5`
Expected: clean, `test_routes` passes with the four new checks.

Then serve and look: `dune exec -- avsm/arod/bin/main.exe serve` in the background, open `http://localhost:8080/search?q=xen`, type, click a facet, press the arrow keys, click `Show N more`. Check the layout at a narrow width. Stop the server.

- [ ] **Step 7: Commit**

```bash
git add avsm/arod
git commit -m "Serve a /search page with live tiered results"
```

---

### Task 7: Remove the modal and point the nav at `/search`

**Files:**
- Modify: `lib_component/nav.ml:179-260,344-351,389`
- Modify: `lib_component/scripts.ml:343-640,1431`
- Modify: `lib_component/theme.ml` (the `.search-modal-overlay` through `.search-result .sr-parent:hover` rules, and the unlayered `.search-filter-pill` rules below `@layer`)
- Modify: `test/test_routes.ml`

- [ ] **Step 1: Write the failing tests**

In `test/test_routes.ml` after the front page checks:

```ocaml
  check "the nav search button is a link to the search page"
    (contains front {|href="/search"|});
  check "and the modal is gone"
    (not (contains front "search-modal-overlay"));
  check "the site script navigates on the keyboard shortcut"
    (contains (body (get "/js/site.js")) "location.href = '/search'")
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `dune build @avsm/arod/test/runtest --force 2>&1 | grep FAIL`
Expected: `FAIL: the nav search button is a link to the search page`.

- [ ] **Step 3: Replace the button and delete the modal**

In `lib_component/nav.ml`, replace the search button element with:

```ocaml
                  (* Search link *)
                  El.a
                    ~at:[
                      At.href "/search";
                      At.v "aria-label" "Search";
                      At.class' "shrink-0 ml-auto p-1.5 rounded-md text-secondary hover:text-link hover:bg-surface transition-all";
                    ]
                    [ search_icon ];
```

Delete `search_filter_pill`, `search_modal` and the `search_modal;` entry in the header's child list. If `filter_icon_for` is now unused inside `nav.ml` but used by `Search`, keep it.

In `lib_component/scripts.ml`, replace the whole modal `search_js` (the one starting `// Search — live FTS5 search with debounce`) with:

```ocaml
let search_shortcut_js = {|
// Cmd-K or Ctrl-K opens the search page
(function() {
  document.addEventListener('keydown', function(e) {
    if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
      e.preventDefault();
      window.location.href = '/search';
    }
  });
})();
|}
```

and in `by_name`'s `site.js` list replace `search_js` with `search_shortcut_js`. If Task 6 named the page script `search_page_js`, rename it to `search_js` now.

In `lib_component/theme.ml`, delete the modal rules: `.search-modal-overlay`, `.search-modal`, `.search-results-area`, `.search-empty-state`, `.search-no-results`, every `.search-result` rule, the `.sr-icon-*` rules, and the `.search-filter-pill` rules below `@layer`. Keep `.tag-search-link` and `.kind-search-link`, which other markup uses. Grep for each deleted class across `lib_component` to confirm nothing else emits it:

Run: `grep -rn "search-result\|search-filter-pill\|search-modal\|sr-icon" avsm/arod/lib_component avsm/arod/lib_handlers avsm/arod/lib`
Expected: no output.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `dune build @avsm/arod/all @avsm/arod/runtest --force 2>&1 | tail -5`
Expected: clean.

- [ ] **Step 5: Commit**

```bash
git add avsm/arod
git commit -m "Replace the search modal with a link to the search page"
```

---

### Task 8: Stylesheet regen, changelog, and the serving-path comparison

**Files:**
- Possibly modify: `assets/tw.css`
- Modify: `CHANGES.md` (repository root or `avsm/arod/CHANGES.md`, whichever exists)

- [ ] **Step 1: Regenerate Tailwind and see whether it changed**

Run: `cd avsm/arod/tailwind && ./regen.sh && cd - && git status --short avsm/arod/assets/tw.css`
Expected: either no change, or a modified `tw.css`. The new markup uses `sp-*` classes from `custom_css`, so a change means a utility class slipped in. Either way, if it changed, commit it alone:

```bash
git add avsm/arod/assets/tw.css
git commit -m "Regenerate tw.css"
```

- [ ] **Step 2: Capture the serving path and compare**

`render_capture.sh` needs a real data directory and a free port, and its header says how to run it. Run it from `main` (stash or use a worktree at `main`) into one directory, and from this branch into another, then diff. Expected differences: every page's nav (the modal markup is gone, the button is a link), `site.js`, and the new `/search` route if the script covers it. Anything else is a regression to fix before continuing.

- [ ] **Step 3: Changelog**

Add under the unreleased heading in `CHANGES.md`:

```
- Search ranks results in tiers: pages and tags the query names, then papers,
  notes, projects, ideas and talks by relevance, then the links they cite.
  Links are deduplicated and links to this site are left out.
- Search moves from a modal to a `/search` page with facets, a year
  histogram and a links rail. Search URLs can be shared. `Cmd-K` opens it.
- `arod search` prints results by tier with scores.
```

- [ ] **Step 4: Final verification and commit**

Run: `dune build @avsm/arod/all @avsm/arod/runtest --force 2>&1 | tail -3 && dune build @fmt 2>&1 | grep -v ocamlformat | head`
Expected: clean.

```bash
git add CHANGES.md
git commit -m "Record the search rework in the changelog"
```
