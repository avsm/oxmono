(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** tiny_httpd server adapter for arod routes *)

let src = Logs.Src.create "arod.server" ~doc:"Arod server adapter"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Request/Response Conversion} *)

let request_of_tiny req =
  let meth =
    match Tiny_httpd.Request.meth req with
    | `GET -> `GET
    | `POST -> `POST
    | `PUT -> `PUT
    | `DELETE -> `DELETE
    | `HEAD -> `HEAD
    | `OPTIONS -> `OPTIONS
  in
  let path = Tiny_httpd.Request.path req in
  let query = Tiny_httpd.Request.query req in
  (* Headers.t is already (string * string) list *)
  let headers = Tiny_httpd.Request.headers req in
  Arod.Route.Request.create ~meth ~path ~query ~headers

let response_to_tiny resp =
  let status = Arod.Route.Response.status resp in
  let headers = Arod.Route.Response.headers resp in
  let body = Arod.Route.Response.body resp in
  if status >= 200 && status < 300 then
    Tiny_httpd.Response.make_string ~headers (Ok body)
  else if status >= 300 && status < 400 then
    (* Redirect *)
    Tiny_httpd.Response.make_raw ~code:status ~headers body
  else
    Tiny_httpd.Response.fail ~code:status "%s" body

(** {1 Route Registration} *)

let register_routes server routes =
  (* tiny_httpd uses a different routing model, so we use a catch-all handler
     that dispatches through our route collection *)
  Tiny_httpd.Server.add_route_handler ~meth:`GET server
    Tiny_httpd.Route.(rest_of_path)
    (fun path req ->
      let arod_req = request_of_tiny req in
      (* Reconstruct the full path including leading slash *)
      let full_path = "/" ^ path in
      let arod_req =
        Arod.Route.Request.create
          ~meth:(Arod.Route.Request.meth arod_req)
          ~path:full_path
          ~query:(Arod.Route.Request.query arod_req)
          ~headers:(Arod.Route.Request.headers arod_req)
      in
      match Arod.Route.Routes.dispatch routes arod_req with
      | Some resp -> response_to_tiny resp
      | None -> Tiny_httpd.Response.fail ~code:404 "Not Found");
  (* Also handle the root path *)
  Tiny_httpd.Server.add_route_handler ~meth:`GET server
    Tiny_httpd.Route.(exact_path "/" return)
    (fun req ->
      let arod_req = request_of_tiny req in
      match Arod.Route.Routes.dispatch routes arod_req with
      | Some resp -> response_to_tiny resp
      | None -> Tiny_httpd.Response.fail ~code:404 "Not Found")

(** {1 Server Lifecycle} *)

let create_server ~config =
  Tiny_httpd.create ~addr:config.Arod.Config.server.host
    ~port:config.Arod.Config.server.port ()

let default_memoized_paths =
  [ "/feeds/"; "/sitemap"; "/perma."; "/bushel/graph.json" ]

let should_memoize path memoized_paths =
  List.exists (fun prefix -> String.length path >= String.length prefix &&
    String.sub path 0 (String.length prefix) = prefix) memoized_paths

let run ~config ?memo_cache ?(memoized_paths = default_memoized_paths) routes =
  let server = create_server ~config in
  (* Add logging middleware *)
  Tiny_httpd.add_middleware server ~stage:(`Stage 1) (fun h req ->
      let start_time = Unix.gettimeofday () in
      let resp = h req in
      let elapsed = Unix.gettimeofday () -. start_time in
      Log.info (fun m ->
          m "%s %s - %.3fs"
            (Tiny_httpd.Meth.to_string (Tiny_httpd.Request.meth req))
            (Tiny_httpd.Request.path req) elapsed);
      resp);
  (* Register all routes with optional memoization *)
  let routes_with_memo =
    match memo_cache with
    | None -> routes
    | Some _cache ->
        (* Wrap memoized paths *)
        Arod.Route.Routes.fold
          (fun route acc ->
            (* For now we just use the routes as-is; memoization could be added
               by wrapping individual handlers, but that requires more intrusive
               changes to the route structure. Instead, we apply memoization at
               dispatch time. *)
            Arod.Route.Routes.add route acc)
          routes Arod.Route.Routes.empty
  in
  (* Use a dispatch wrapper that applies memoization *)
  Tiny_httpd.Server.add_route_handler ~meth:`GET server
    Tiny_httpd.Route.rest_of_path
    (fun path req ->
      let arod_req = request_of_tiny req in
      let full_path = "/" ^ path in
      let arod_req =
        Arod.Route.Request.create
          ~meth:(Arod.Route.Request.meth arod_req)
          ~path:full_path
          ~query:(Arod.Route.Request.query arod_req)
          ~headers:(Arod.Route.Request.headers arod_req)
      in
      let dispatch_fn =
        match memo_cache with
        | Some cache when should_memoize full_path memoized_paths ->
            fun req ->
              Arod.Memo.memoize cache
                (fun r ->
                  match Arod.Route.Routes.dispatch routes_with_memo r with
                  | Some resp -> resp
                  | None -> Arod.Route.Response.not_found)
                req
        | _ -> fun req ->
            match Arod.Route.Routes.dispatch routes_with_memo req with
            | Some resp -> resp
            | None -> Arod.Route.Response.not_found
      in
      response_to_tiny (dispatch_fn arod_req));
  (* Handle root path *)
  Tiny_httpd.Server.add_route_handler ~meth:`GET server
    Tiny_httpd.Route.(exact_path "/" return)
    (fun req ->
      let arod_req = request_of_tiny req in
      let dispatch_fn =
        match memo_cache with
        | Some cache when should_memoize "/" memoized_paths ->
            fun req ->
              Arod.Memo.memoize cache
                (fun r ->
                  match Arod.Route.Routes.dispatch routes_with_memo r with
                  | Some resp -> resp
                  | None -> Arod.Route.Response.not_found)
                req
        | _ -> fun req ->
            match Arod.Route.Routes.dispatch routes_with_memo req with
            | Some resp -> resp
            | None -> Arod.Route.Response.not_found
      in
      response_to_tiny (dispatch_fn arod_req));
  Log.app (fun m ->
      m "Listening on http://%s:%d" config.server.host config.server.port);
  Tiny_httpd.run server
