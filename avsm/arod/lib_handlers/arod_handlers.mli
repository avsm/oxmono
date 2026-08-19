(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure route handlers for arod using context-based state.

    This module contains all the HTTP route handlers. Handlers take a context
    ([Arod_ctx.t]) and cache ([Arod_cache.t]) and use the CPS-style
    [Httpz_route.respond] function to write responses directly.

    Content routes are cached for performance. Static file routes and
    dynamic query-dependent routes bypass the cache. *)

(** {1 Route Collection} *)

val all_routes : ctx:Arod.Ctx.t -> cache:Arod.Cache.t -> search:Arod_search.t ->
  log:Arod_log.t -> fs:Eio.Fs.dir_ty Eio.Path.t -> Httpz_route.t
(** [all_routes ~ctx ~cache ~search ~log ~fs] returns all routes for the arod application.
    Content routes use the cache for memoization. The [search] handle is used
    for the [/api/search] JSON endpoint. [log] provides the access log database
    for the stats dashboard. [fs] is the filesystem capability the static file
    routes read through, confined to their subtree by {!static_file}. *)

(** {1 Static File Serving} *)

val confined_path : string list -> string option
(** [confined_path segs] is [segs] joined with ['/'] when every segment names
    something directly under the serving directory, and [None] otherwise. A
    segment that is empty, ["."] or [".."], or that holds a ['/'] or a NUL, is
    refused, so a joined path can never leave the subtree. *)

val static_file : dir:_ Eio.Path.t -> string list -> Httpz_route.ctx @ local ->
  local_ Httpz_route.respond -> unit
(** [static_file ~dir segs ctx respond] serves the file named by [segs] under
    [dir], with a MIME type taken from its extension. It answers 404 when the
    file is missing.

    [segs] comes from the client, so it is confined to the subtree here: a
    segment that is empty, ["."] or [".."], or that holds a ['/'] or a NUL,
    gives a 404. [Eio.Path] does not do this. A [dir] rooted at
    {!Eio.Stdenv.fs} follows [".."] out of the subtree, so without this check
    a request reads any file the process can. *)
