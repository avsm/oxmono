(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure route handlers for arod

    This module contains all the HTTP route handlers as pure functions that
    operate on the framework-agnostic [Arod_route.Request] and
    [Arod_route.Response] types. The handlers are designed to be reusable
    across different HTTP server implementations. *)

(** {1 Query Information}

    Query parameters extracted from requests for filtering and pagination. *)

type query_info = {
  tags : Arod_model.Tags.t list;  (** Tag filters from ?t= parameters *)
  min : int;  (** Minimum items to show, from ?min= parameter (default 25) *)
  show_all : bool;  (** Whether to show all items, from ?all parameter *)
}
(** Query information extracted from a request. *)

val query_info_of_request : Arod_route.Request.t -> query_info
(** [query_info_of_request req] extracts tag filters, min count, and show_all
    flag from request query parameters. *)

(** {1 Content Handlers}

    Handlers for individual content pages and listings. *)

val index : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the index/home page. *)

val papers : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the papers listing page. *)

val paper : Arod_config.t -> (unit * string) -> Arod_route.Request.t -> Arod_route.Response.t
(** [paper cfg ((), slug) req] handles individual paper pages or PDF/bib downloads. *)

val notes : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the notes listing page. *)

val note : (unit * string) -> Arod_route.Request.t -> Arod_route.Response.t
(** [note ((), slug) req] handles individual note pages. *)

val ideas : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the ideas listing page. *)

val idea : (unit * string) -> Arod_route.Request.t -> Arod_route.Response.t
(** [idea ((), slug) req] handles individual idea pages. *)

val projects : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the projects listing page. *)

val project : (unit * string) -> Arod_route.Request.t -> Arod_route.Response.t
(** [project ((), slug) req] handles individual project pages. *)

val videos : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the videos/talks listing page. *)

val video : (unit * string) -> Arod_route.Request.t -> Arod_route.Response.t
(** [video ((), slug) req] handles individual video pages. *)

val content : (unit * string) -> Arod_route.Request.t -> Arod_route.Response.t
(** [content ((), slug) req] generic content handler that looks up any entry by slug. *)

(** {1 Legacy Handlers} *)

val news_redirect : (unit * string) -> Arod_route.Request.t -> Arod_route.Response.t
(** [news_redirect ((), slug) req] redirects /news/slug to /notes/slug. *)

val wiki : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for legacy /wiki endpoint. *)

val news : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for legacy /news endpoint. *)

(** {1 Feed Handlers} *)

val atom_feed : Arod_config.t -> Arod_route.Request.t -> Arod_route.Response.t
(** Handler for Atom feed generation. *)

val json_feed : Arod_config.t -> Arod_route.Request.t -> Arod_route.Response.t
(** Handler for JSON feed generation. *)

val perma_atom : Arod_config.t -> Arod_route.Request.t -> Arod_route.Response.t
(** Handler for permanent/archival Atom feed. *)

val perma_json : Arod_config.t -> Arod_route.Request.t -> Arod_route.Response.t
(** Handler for permanent/archival JSON feed. *)

(** {1 Utility Handlers} *)

val sitemap : Arod_config.t -> Arod_route.Request.t -> Arod_route.Response.t
(** Handler for sitemap.xml generation. *)

val bushel_graph : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the Bushel link graph visualization page. *)

val bushel_graph_data : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the Bushel link graph JSON data. *)

val pagination_api : Arod_route.Request.t -> Arod_route.Response.t
(** Handler for the pagination API endpoint. *)

val well_known : Arod_config.t -> (unit * string) -> Arod_route.Request.t -> Arod_route.Response.t
(** [well_known cfg ((), key) req] handles .well-known/[key] endpoints. *)

val robots_txt : Arod_config.t -> Arod_route.Request.t -> Arod_route.Response.t
(** Handler for robots.txt. *)

(** {1 Static File Serving} *)

val static_file : dir:string -> string -> Arod_route.Request.t -> Arod_route.Response.t
(** [static_file ~dir path req] serves a file from [dir]/[path] with
    appropriate MIME type. Returns 404 if file not found. *)

(** {1 Route Collection} *)

val all_routes : Arod_config.t -> Arod_route.Routes.t
(** [all_routes cfg] returns all routes for the arod application. *)
