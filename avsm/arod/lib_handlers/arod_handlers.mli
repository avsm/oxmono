(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Route handlers for arod, described as proffer responses.

    Each handler is a {!Proffer.Route.handler} over {!Env.t}, so it reads
    every capability it needs from that record and captures nothing. The site
    builder assembles them into the route table.

    A page that has both an HTML and a markdown rendering is a
    {!Proffer.Negotiate.v} handler over two cached renders. A cached response
    carries a weak entity-tag, which the backend answers a conditional request
    from, and an [X-Cache] field of ["hit"] or ["miss"], which the stats
    dashboard breaks traffic down by. *)

module Env = Arod_env
(** The capabilities every handler here reads. It is re-exported so that a
    caller assembling the site names one module. *)

module Render = Arod_render
(** The rendering {!Env.create} closes over. A caller needs it only to name
    the types {!Env}'s fields are written in. *)

type handler = Env.t Proffer.Route.handler
(** What every route runs. *)

(** {1 Content pages}

    Each of these negotiates between HTML and markdown on the request's Accept
    field, and caches both renderings. *)

val index : handler @@ portable
(** [index] is the front page. *)

val papers_list : handler @@ portable
(** [papers_list] is the list of papers. *)

val notes_list : handler @@ portable
(** [notes_list] is the list of notes. *)

val ideas_list : handler @@ portable
(** [ideas_list] is the list of research ideas. *)

val projects_list : handler @@ portable
(** [projects_list] is the list of projects. *)

val videos_list : handler @@ portable
(** [videos_list] is the list of talks. *)

val links_list : handler @@ portable
(** [links_list] is the list of outbound links. *)

val network_page : handler @@ portable
(** [network_page] is the network activity page. *)

val paper : string -> handler @@ portable
(** [paper slug] is the paper named by [slug]. A [slug] ending in [".pdf"] is
    the paper's PDF read from the served paper directory, one ending in
    [".bib"] is its BibTeX entry, and one ending in [".md"] is the entry as
    markdown. Anything else is the negotiated page. *)

val note : string -> handler @@ portable
(** [note slug] is the note named by [slug], or that entry as markdown when
    [slug] ends in [".md"]. *)

val idea : string -> handler @@ portable
(** [idea slug] is the idea named by [slug], or that entry as markdown when
    [slug] ends in [".md"]. *)

val project : string -> handler @@ portable
(** [project slug] is the project named by [slug], or that entry as markdown
    when [slug] ends in [".md"]. *)

val video : string -> handler @@ portable
(** [video slug] is the talk named by [slug], or that entry as markdown when
    [slug] ends in [".md"]. *)

(** {1 Markdown pages}

    A ["<page>.md"] URL asks for a list page as markdown without negotiating.
    Each one shares its cache entry with the markdown variant of the page it
    names. *)

val index_markdown : handler @@ portable
val papers_markdown : handler @@ portable
val notes_markdown : handler @@ portable
val ideas_markdown : handler @@ portable
val projects_markdown : handler @@ portable
val videos_markdown : handler @@ portable
val links_markdown : handler @@ portable
val network_markdown : handler @@ portable

(** {1 Feeds} *)

val atom_feed : string -> handler @ portable @@ portable
(** [atom_feed path] is the Atom feed of every entry, naming itself [path].
    Two routes serve it, so [path] is the one the route table gives and never
    the request target, which a client controls and could use to fill the
    cache. The response is cached under [path]. *)

val json_feed : handler @@ portable
(** [json_feed] is the JSON feed of every entry. *)

val perma_atom : handler @@ portable
(** [perma_atom] is the Atom feed of permanent entries. *)

val perma_json : handler @@ portable
(** [perma_json] is the JSON feed of permanent entries. *)

(** {1 Redirect targets} *)

val encode_segment : string -> string @@ portable
(** [encode_segment s] is [s] with every byte outside the unreserved set of
    RFC 3986 section 2.3 percent-encoded, sub-delimiters included. That is
    stricter than what the RFC allows in a path segment, which also admits a
    sub-delimiter, [':'] and ['@']. A route capture arrives decoded, so a
    redirect built from one passes it through this first. Without it a segment
    holding a space redirects to the wrong place, and one holding a CR makes
    {!Proffer.Resp.redirect} refuse the field and the request answer 500. An
    ordinary slug is unchanged. *)

(** {1 Machine-readable pages} *)

val sitemap : handler @@ portable
(** [sitemap] is the XML sitemap of every entry. *)

val blogroll_opml : handler @@ portable
(** [blogroll_opml] is the OPML blogroll of every contact that has a feed. *)

val robots_txt : handler @@ portable
(** [robots_txt] allows every crawler and points at the sitemap. *)

val well_known : string -> handler @@ portable
(** [well_known key] is the configured value under [key], and a 404 when the
    configuration names no such key. *)

(** {1 JSON APIs} *)

val pagination_api : handler @@ portable
(** [pagination_api] is one page of a collection as rendered HTML with the
    counts a client needs to ask for the next. It reads [collection],
    [offset], [limit] and any number of [type] query parameters. *)

val search_api : handler @@ portable
(** [search_api] is the full-text search results for the [q] query parameter,
    at most [limit] of them. *)

val search_page : handler @@ portable
(** The search page at [/search], or its results fragment when the query
    string carries [fragment=1]. *)

(** {1 Files} *)

val image_file : string list -> handler @@ portable
(** [image_file segs] is the file named by [segs] under the served image
    directory. The read is confined to that directory, so a [segs] that would
    leave it is a 404, as is a file that is missing. *)

val embedded_file : string -> handler @ portable @@ portable
(** [embedded_file path] is the asset [path] compiled into the binary, with a
    Content-Type from its extension. *)

val embedded_file_immutable : string -> handler @ portable @@ portable
(** [embedded_file_immutable path] is {!embedded_file} with a one year
    immutable cache policy. Such an asset is fetched with a content hash in
    its query string, so a change to the file changes the URL. *)

val js_file : string -> handler @@ portable
(** [js_file name] is the compiled script [name], cached as
    {!embedded_file_immutable} is. *)

(** {1 Stats dashboard}

    These are gated by {!Proffer.Site.with_auth} at the site level rather than
    one at a time, so none of them checks credentials itself. *)

val stats_auth : password:string -> string option -> bool @@ portable
(** [stats_auth ~password auth] is whether the Authorization field [auth]
    carries HTTP Basic credentials whose password is [password]. The user name
    is ignored. It is [false] when [auth] is absent, is not Basic, or does not
    decode. *)

val stats_dashboard : handler @@ portable
(** [stats_dashboard] is the access log dashboard over the range named by the
    [range] query parameter, defaulting to seven days. *)

val stats_overview : handler @@ portable
(** [stats_overview] is the dashboard's summary figures as JSON. *)

val stats_traffic : handler @@ portable
(** [stats_traffic] is the dashboard's traffic series as JSON. *)

val stats_recent : handler @@ portable
(** [stats_recent] is the most recent requests as JSON. *)
