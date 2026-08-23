# Arod search reranked

Replace date-ordered search with a tiered ranking that puts the site's own
pages and writing ahead of the links it cites, and replace the search modal
with a `/search` page that lays the tiers out as a main column and a rail.

Prototype: https://claude.ai/code/artifact/70c32985-7193-48b4-81e8-4ea942f0843e
(variant B). The ranking below is the one the prototype runs.

## Problem

`Arod_search.search` queries one FTS5 table per kind, orders each table by
`date DESC`, applies the caller's limit per table, then merges by date again.
The `bm25()` value it selects is never read. The index holds 4,550 links
against 771 local entries (147 papers, 404 notes, 16 projects, 84 ideas, 120
talks), so any broad query returns one or two recent notes followed by a wall
of GitHub pull requests. `arod search ocaml` returns one note and eleven links
in its first twelve rows.

Measured against `~/.cache/arod/search.db`:

| observation | count |
|---|---|
| links with no citing entry (`parent_slugs = ''`) | 0 |
| links cited by two or more entries | 585 |
| links whose URL differs from another only by scheme, `www.`, trailing `/` or `#` | present (`ssir.org/...offsets` and `...offsets#`) |
| links pointing at the site's own host | present (`anil.recoil.org/papers/...pdf`) |
| distinct tags on local entries | 422 |

Every link is cited by something, so "bookmark with no parent" is not a class
that exists. Citation count is the curation signal links carry.

## Ranking model

A result set has three tiers. Tiers are strict: nothing in a later tier is
ever shown above anything in an earlier one.

| tier | contents | order within tier |
|---|---|---|
| go to | section pages, projects and tags whose name starts with a query word | sections, then projects, then tags by entry count |
| work | papers, notes, projects, ideas, videos | `bm25 × kind prior × freshness` |
| links | external links, deduplicated | `bm25 × freshness × citation bonus` |

`bm25` is `-bm25(table, 0, 0, 0, 0, 10, 1, 5)`, the weights the index already
uses (title 10, tags 5, body 1), negated so that larger is better.

`kind prior` is project 1.15, paper 1.0, note 1.0, video 0.9, idea 0.85. A
project page is the canonical landing point for its topic. Ideas are proposals
rather than results.

`freshness` is `1 + 0.25 × max 0 (1 - age_in_years / 8)`. It separates ties
and lifts this year's work by a quarter at most. Date is never the primary
key.

`citation bonus` is `1 + 0.3 × ln(number of citing entries)`.

The go-to tier matches names, not text. A section matches when a query word
is a prefix of its name (Papers, Notes, Projects, Ideas, Talks, Links,
Network). A project matches when every query word is a prefix of its slug or
of a word in its title. A tag matches when every query word is a prefix of the
tag or of a hyphen-separated part of it. Tags link to `/#tag=<tag>`, which is
where `/tags/<tag>` already redirects. At most 7 go-to hits are returned:
sections and projects first, then tags by count.

Links are deduplicated before ranking. Two links are the same when their URLs
agree after lowercasing, dropping the scheme and a leading `www.`, and
trimming trailing `/` and `#`, or when they share a host and a title. The
higher-scoring copy survives. Links whose host is the site's own base URL are
dropped from the links tier, since the content they point at is local and
already indexed as work.

Kind filters (`kind:paper`, or the facets on the page) restrict the work and
links tiers. `#tag` filters restrict to entries carrying the tag. A query with
only filters and no words browses the restricted set by date, as today.

## Search library

`Arod_search.search` returns a record instead of a flat list.

```ocaml
type goto = {
  label : string;            (* "OCaml Labs", "ocaml", "Papers" *)
  url : string;
  detail : string;           (* "2012 project", "257 entries", "section" *)
  goto_kind : [ `Section | `Project | `Tag ];
}

type hit = {
  slug : string; kind : string; url : string; title : string;
  snippet : string; date : string; tags : string list;
  parent_slugs : string list;
  score : float;             (* the tier's combined score, for the CLI *)
}

type results = {
  goto : goto list;
  work : hit list;           (* ranked, at most [limit] *)
  work_total : int;          (* matches before the limit *)
  links : hit list;          (* ranked, deduplicated, at most [link_limit] *)
  links_total : int;
  kinds : (string * int) list;    (* work matches per kind *)
  years : (int * int) list;       (* work matches per year, ascending *)
  tags : (string * int) list;     (* top 8 tags among work matches *)
}

val search : t -> ?limit:int -> ?link_limit:int -> string -> results
```

`limit` defaults to 20 and `link_limit` to 12. The facet fields are computed
over every match, not the limited slice.

Each per-kind query becomes `ORDER BY bm25(...) LIMIT n` with `n` large enough
to feed the facets (200 per local kind, 500 for links). The `tags` column is
selected from the FTS row and split on spaces, which removes the per-result
`entry_tags` lookup that `enrich_tags` does today.

Project names for the go-to tier come from the `search_project` table. Tag
counts come from `entry_tags` restricted to non-link kinds, computed once at
`rebuild` into a sorted immutable list held in `t`, since the handler reads it
from a portable closure.

`rebuild` gains a sibling `index : t -> contact_name:(string -> string option)
-> entries:Bushel.Entry.entry list -> links:Bushel.Link.t list -> unit` that
`rebuild` calls, so tests can build an index over synthetic entries and links
without a data directory.

## JSON API

`/api/search?q=&limit=&link_limit=` returns the record above:

```json
{
  "goto": [{"label": "OCaml Labs", "url": "/projects/ocamllabs",
            "detail": "2012 project", "kind": "project"}],
  "work": [{"slug": "...", "kind": "note", "url": "...", "title": "...",
            "snippet": "...", "date": "2022-04-19", "tags": ["ocaml"],
            "thumbnail": "...", "parents": []}],
  "work_total": 302,
  "links": [{"slug": "https://...", "kind": "link", "url": "https://...",
             "title": "...", "snippet": "...", "date": "2025-01-01",
             "thumbnail": "https://github.com/fluidicon.png",
             "parents": [{"slug": "xen", "title": "Xen Hypervisor",
                          "url": "/projects/xen", "kind": "project"}]}],
  "links_total": 958,
  "kinds": [{"kind": "note", "count": 230}, {"kind": "paper", "count": 40}],
  "years": [{"year": 2003, "count": 2}, {"year": 2004, "count": 5}],
  "tags": [{"tag": "ocaml", "count": 180}, {"tag": "mirageos", "count": 40}]
}
```

The hit shape is the existing `Search_hit` with `score` omitted. `thumbnail`
for a link is the Karakeep favicon, as now. `Arod_render.search` keeps its
parent and thumbnail lookups and maps the new record.

## The page

`GET /search` renders a page with a search input at the top of the main
column, the go-to chips, the work list, and a rail. `GET /search?q=...`
renders the same page with results filled in on the server, so a search URL
is shareable and works without JavaScript.

Main column, top to bottom:

- the input, prefilled from `q`, focused when `q` is empty
- a count line: `302 on this site · 958 links`
- go-to chips, each with its kind icon, label and detail
- the work list. Each row is the kind icon, title with matches marked, date,
  a two-line snippet with matches marked, and up to five tags. A `Show N
  more` button after the list fetches with a doubled `limit`.

Rail, top to bottom:

- `Narrow`: a chip per kind with its count, then the top tags with counts.
  Clicking a kind adds or removes `kind:<kind>` in the query. Clicking a tag
  appends `#<tag>`. Active chips are marked.
- a year histogram over work matches, busiest year in the accent colour, with
  the first and last year labelled.
- `Links cited on this site`: each row is the favicon (or the link glyph when
  the link has none), title with matches marked, date, then the host and `in
  <citing entry>` with `+N` when there are several. A `Show N more` button
  fetches with a doubled `link_limit`.

On viewports under 56rem the rail stacks below the main column.

Client behaviour, in a new `search.js` page script:

- typing debounces 120ms, then fetches `/search?q=...&fragment=1`, which
  returns only the results region (count line, chips, work list and rail) as
  HTML, and swaps it into place. One renderer, in OCaml, produces both the
  full page and the fragment.
- each fetch calls `history.replaceState` so the address bar tracks the query
- `↑` `↓` move a selection through go-to chips, work rows and link rows in
  document order, `Enter` opens the selection, `Escape` clears the input
- `Tab` from the input moves to the first chip as normal focus order

Snippet and title highlighting come from the FTS5 `snippet()` output, which
already wraps matches in `<b>`. Title highlighting is done in OCaml by
wrapping each query word's prefix occurrences.

## Navigation

The modal goes. `Nav.search_modal` and `Scripts.search_js` are deleted. The
nav search button becomes a link to `/search`. The `Cmd-K` and `Ctrl-K`
shortcut, the only one the modal binds today, navigates to `/search`. This removes the second renderer and the duplicated
keyboard handling, and every page's HTML shrinks by the modal markup.

## CLI

`arod search` prints the three tiers with a heading each, the score in a
column after the date, and the citing entry for links. Counts follow each
tier heading when the limit truncated it.

## Testing

- `test/test_search.ml`, new: builds an in-memory index over synthetic
  entries and links through `index` and checks tier membership, that a link
  never precedes a work hit, kind prior and freshness ordering, link
  deduplication, own-host exclusion, go-to matching for sections, projects
  and tags, facet counts, and the filter-only browse path.
- `test/test_json.ml`: update the `Arod_search.result` construction to the
  new `hit` and add a case for the full record.
- `test/test_routes.ml`: `/search` renders the page, `/search?q=x` renders
  with results, `/search?q=x&fragment=1` returns the fragment without the
  layout, `/api/search` returns the new shape.
- `render_capture.sh` before and after. Every page changes because the nav
  loses the modal, so the comparison is for unintended differences beyond
  the nav and the new route.

## Out of scope

- changing what is indexed, or the FTS5 tokenizer
- the home page's `#tag=` filter
- search over contacts or feed items
- favicon fetching or caching for links that have none
