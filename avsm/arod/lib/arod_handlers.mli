(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure route handlers for arod using context-based state.

    This module contains all the HTTP route handlers. Handlers take a context
    ([Arod_ctx.t]) and cache ([Arod_cache.t]) and use the CPS-style
    [Httpz_server.Route.respond] function to write responses directly.

    Content routes are cached for performance. Static file routes and
    dynamic query-dependent routes bypass the cache. *)

(** {1 Route Collection} *)

val all_routes : ctx:Arod_ctx.t -> cache:Arod_cache.t -> Httpz_server.Route.t
(** [all_routes ~ctx ~cache] returns all routes for the arod application.
    Content routes use the cache for memoization. *)

(** {1 Static File Serving} *)

val static_file : dir:string -> string -> Httpz_server.Route.ctx ->
  Httpz_server.Route.respond -> unit
(** [static_file ~dir path ctx respond] serves a file from [dir]/[path] with
    appropriate MIME type. Calls [respond] with 404 if file not found. *)
