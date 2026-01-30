(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure route handlers for arod

    This module contains all the HTTP route handlers as pure functions that
    use the CPS-style [Httpz.Route.respond] function to write responses
    directly. The handlers are designed to be reusable across different
    HTTP server implementations with zero response record allocation. *)

(** {1 Query Information}

    Query parameters extracted from requests for filtering and pagination. *)

type query_info = {
  tags : Arod_model.Tags.t list;  (** Tag filters from ?t= parameters *)
  min : int;  (** Minimum items to show, from ?min= parameter (default 25) *)
  show_all : bool;  (** Whether to show all items, from ?all parameter *)
}
(** Query information extracted from a request. *)

val query_info_of_ctx : Httpz.Route.ctx -> query_info
(** [query_info_of_ctx ctx] extracts tag filters, min count, and show_all
    flag from context query parameters. *)

(** {1 Content Handlers}

    Handlers for individual content pages and listings.
    All handlers take a [respond] function for direct response writing. *)

val index : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the index/home page. *)

val papers : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the papers listing page. *)

val paper : Arod_config.t -> string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [paper cfg slug ctx respond] handles individual paper pages or PDF/bib downloads. *)

val notes : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the notes listing page. *)

val note : string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [note slug ctx respond] handles individual note pages. *)

val ideas : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the ideas listing page. *)

val idea : string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [idea slug ctx respond] handles individual idea pages. *)

val projects : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the projects listing page. *)

val project : string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [project slug ctx respond] handles individual project pages. *)

val videos : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the videos/talks listing page. *)

val video : string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [video slug ctx respond] handles individual video pages. *)

val content : string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [content slug ctx respond] generic content handler that looks up any entry by slug. *)

(** {1 Legacy Handlers} *)

val news_redirect : string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [news_redirect slug ctx respond] redirects /news/slug to /notes/slug. *)

val wiki : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for legacy /wiki endpoint. *)

val news : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for legacy /news endpoint. *)

(** {1 Feed Handlers} *)

val atom_feed : Arod_config.t -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for Atom feed generation. *)

val json_feed : Arod_config.t -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for JSON feed generation. *)

val perma_atom : Arod_config.t -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for permanent/archival Atom feed. *)

val perma_json : Arod_config.t -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for permanent/archival JSON feed. *)

(** {1 Utility Handlers} *)

val sitemap : Arod_config.t -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for sitemap.xml generation. *)

val bushel_graph : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the Bushel link graph visualization page. *)

val bushel_graph_data : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the Bushel link graph JSON data. *)

val pagination_api : Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for the pagination API endpoint. *)

val well_known : Arod_config.t -> string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [well_known cfg key ctx respond] handles .well-known/[key] endpoints. *)

val robots_txt : Arod_config.t -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** Handler for robots.txt. *)

(** {1 Static File Serving} *)

val static_file : dir:string -> string -> Httpz.Route.ctx -> Httpz.Route.respond -> unit
(** [static_file ~dir path ctx respond] serves a file from [dir]/[path] with
    appropriate MIME type. Calls [respond] with 404 if file not found. *)

(** {1 Route Collection} *)

val all_routes : Arod_config.t -> Httpz.Route.t
(** [all_routes cfg] returns all routes for the arod application. *)
