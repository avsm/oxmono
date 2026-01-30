(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** tiny_httpd server adapter for arod routes

    This module adapts the framework-agnostic {!Arod.Route} abstraction to
    work with tiny_httpd. It provides conversion functions between the
    abstract request/response types and tiny_httpd's types, as well as
    server lifecycle management. *)

(** {1 Request/Response Conversion} *)

val request_of_tiny : 'body Tiny_httpd.Request.t -> Arod.Route.Request.t
(** [request_of_tiny req] converts a tiny_httpd request to an arod request. *)

val response_to_tiny : Arod.Route.Response.t -> Tiny_httpd.Response.t
(** [response_to_tiny resp] converts an arod response to a tiny_httpd response. *)

(** {1 Route Registration} *)

val register_routes : Tiny_httpd.t -> Arod.Route.Routes.t -> unit
(** [register_routes server routes] registers all routes with the tiny_httpd
    server. Each route is converted to a tiny_httpd route handler. *)

(** {1 Server Lifecycle} *)

val create_server : config:Arod.Config.t -> Tiny_httpd.t
(** [create_server ~config] creates a tiny_httpd server with host and port
    from [config]. *)

val run :
  config:Arod.Config.t ->
  ?memo_cache:Arod.Route.Response.t Arod.Memo.t ->
  ?memoized_paths:string list ->
  Arod.Route.Routes.t ->
  (unit, exn) result
(** [run ~config ?memo_cache ?memoized_paths routes] creates and starts a
    tiny_httpd server with the given routes.

    @param config Server configuration with host, port, and paths.
    @param memo_cache Optional memoization cache for caching responses.
    @param memoized_paths List of path prefixes to memoize (default:
      ["/feeds/"; "/sitemap"; "/perma."; "/bushel/graph.json"]).
    @return [Ok ()] on clean shutdown, [Error exn] on failure. *)
