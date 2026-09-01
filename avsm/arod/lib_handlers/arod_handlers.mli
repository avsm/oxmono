(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Proffer route handlers for Arod. *)

module Env = Arod_env
(** Handler capabilities. *)

module Render = Arod_render
(** Response rendering. *)

type handler = Env.t Proffer.Route.handler
(** What every route runs. *)

(** {1 Content pages} *)

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
(** [paper slug] is the paper named by [slug]. The suffixes [".pdf"], [".bib"]
    and [".md"] select its alternate representations. *)

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

(** {1 Markdown pages} *)

val index_markdown : handler @@ portable
(** [index_markdown] is the front page as markdown. *)
val papers_markdown : handler @@ portable
(** [papers_markdown] is the paper list as markdown. *)
val notes_markdown : handler @@ portable
(** [notes_markdown] is the note list as markdown. *)
val ideas_markdown : handler @@ portable
(** [ideas_markdown] is the idea list as markdown. *)
val projects_markdown : handler @@ portable
(** [projects_markdown] is the project list as markdown. *)
val videos_markdown : handler @@ portable
(** [videos_markdown] is the video list as markdown. *)
val links_markdown : handler @@ portable
(** [links_markdown] is the outbound-link list as markdown. *)
val network_markdown : handler @@ portable
(** [network_markdown] is the network page as markdown. *)

(** {1 Feeds} *)

val atom_feed : string -> handler @ portable @@ portable
(** [atom_feed path] is the Atom feed of every entry, named and cached as
    [path]. *)

val json_feed : handler @@ portable
(** [json_feed] is the JSON feed of every entry. *)

val perma_atom : handler @@ portable
(** [perma_atom] is the Atom feed of permanent entries. *)

val perma_json : handler @@ portable
(** [perma_json] is the JSON feed of permanent entries. *)

(** {1 Redirect targets} *)

val encode_segment : string -> string @@ portable
(** [encode_segment s] is [s] percent-encoded as an RFC 3986 path segment. *)

(** {1 Machine-readable pages} *)

val sitemap : handler @@ portable
(** [sitemap] is the XML sitemap of every entry. *)

val blogroll_opml : handler @@ portable
(** [blogroll_opml] is the OPML blogroll of every contact that has a feed. *)

val robots_txt : handler @@ portable
(** [robots_txt] allows every crawler and points at the sitemap. *)

val llms_txt : handler @@ portable
(** [llms_txt] is the llms.txt index of every entry's Markdown twin. *)

val well_known : string -> handler @@ portable
(** [well_known key] is the configured value under [key], and a 404 when the
    configuration names no such key. *)

(** {1 JSON APIs} *)

val pagination_api : handler @@ portable
(** [pagination_api] is one page of a collection as rendered HTML with the
    counts a client needs to ask for the next. It reads [collection],
    [offset], [limit] and any number of [type] query parameters. *)

val search_api : handler @@ portable
(** [search_api] is the full-text result set selected by [q], [limit],
    [link_limit] and [sort]. *)

val search_page : handler @@ portable
(** [search_page] is [/search], or its results fragment for [fragment=1]. *)

(** {1 Files} *)

val image_file : string list @ local -> handler @ local @@ portable
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

(** {1 Statistics} *)

val stats_auth : password:string -> string option -> bool @@ portable
(** [stats_auth ~password auth] is [true] if [auth] is valid Basic
    authentication with [password]. *)

val stats_dashboard : handler @@ portable
(** [stats_dashboard] is the access log dashboard over the range named by the
    [range] query parameter, defaulting to seven days. *)

val stats_overview : handler @@ portable
(** [stats_overview] is the dashboard's summary figures as JSON. *)

val stats_traffic : handler @@ portable
(** [stats_traffic] is the dashboard's traffic series as JSON. *)

val stats_recent : handler @@ portable
(** [stats_recent] is the most recent requests as JSON. *)
