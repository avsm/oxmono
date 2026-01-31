(** Zero-allocation HTTP routing with type-safe path patterns.

    This module provides a routing DSL with the following features:
    - Type-safe path patterns using GADTs
    - Zero-copy path matching using spans
    - Per-route header requirements (headers extracted only when needed)
    - CPS-style handlers for direct response writing

    {2 Quick Start}

    {[
      open Httpz_server.Route

      (* Simple routes without captured parameters *)
      let routes = of_list [
        get_ [] (fun _ctx respond -> html respond "Welcome!");
        get_ ["api"; "status"] (fun _ctx respond -> json respond {|{"ok":true}|});
      ]
    ]}

    {2 Path Patterns}

    Use pattern combinators to capture path segments:

    {[
      (* Capture a single segment *)
      let user_route = get ("users" **> seg) (fun user_id _ctx respond ->
        html respond (Printf.sprintf "User: %s" user_id))

      (* Capture multiple segments *)
      let file_route = get (lit "files" ** seg ** seg)
        (fun (dir, file) _ctx respond ->
          html respond (Printf.sprintf "File: %s/%s" dir file))

      (* Capture remaining path *)
      let static_route = get ("static" **> tail) (fun path _ctx respond ->
        serve_file path respond)
    ]}

    {2 Header Requirements}

    Declare which headers a route needs. Headers are extracted during
    dispatch, avoiding unnecessary copying:

    {[
      (* Route that needs Content-Type *)
      let upload = post_h ("upload" **> seg)
        (Httpz.Header_name.Content_type +> h0)
        (fun filename (content_type, ()) _ctx respond ->
          match content_type with
          | Some ct -> handle_upload filename ct respond
          | None -> respond_string respond ~status:Bad_request "Missing Content-Type")

      (* Route that needs multiple headers *)
      let api = post_h (lit "api" ** seg)
        (Httpz.Header_name.Authorization +> Httpz.Header_name.Content_type +> h0)
        (fun (_api_name) (auth, (ct, ())) _ctx respond ->
          match auth with
          | None -> respond_string respond ~status:Unauthorized "Auth required"
          | Some token -> process_request token ct respond)
    ]} *)

(** {1 Response Body} *)

type body =
  | Empty
  | String of string
  | Bigstring of { buf : Base_bigstring.t; off : int; len : int }
  | Stream of { length : int option; iter : (string -> unit) -> unit }
(** Response body variants.

    - [Empty]: No body (e.g., 204 No Content, redirects)
    - [String]: Immediate string body
    - [Bigstring]: Zero-copy slice from bigstring (for static files)
    - [Stream]: Streaming body with optional known length *)

(** {1 Response Headers} *)

type resp_header = Httpz.Header_name.t * string
(** Response header: typed name and string value.

    {[
      let headers = [
        (Httpz.Header_name.Content_type, "text/html");
        (Httpz.Header_name.Cache_control, "max-age=3600");
      ]
    ]} *)

(** {1 Response Writing} *)

type respond = status:Httpz.Res.status -> headers:local_ resp_header list -> body -> unit
(** Response writer function passed to handlers.

    Handlers call [respond] to write the HTTP response directly:
    {[
      fun _ctx respond ->
        respond ~status:Success ~headers:[] (String "Hello!")
    ]}

    The [headers] list is local for stack allocation. *)

(** {2 Response Helpers}

    Convenience functions that call [respond] with common patterns. *)

val html : local_ respond -> string -> unit
(** [html respond body] sends [body] as [text/html] with status 200. *)

val json : local_ respond -> string -> unit
(** [json respond body] sends [body] as [application/json] with status 200. *)

val xml : local_ respond -> string -> unit
(** [xml respond body] sends [body] as [application/xml] with status 200. *)

val atom : local_ respond -> string -> unit
(** [atom respond body] sends [body] as [application/atom+xml] with status 200. *)

val plain : local_ respond -> string -> unit
(** [plain respond body] sends [body] as [text/plain] with status 200. *)

val redirect : local_ respond -> status:Httpz.Res.status -> location:string -> unit
(** [redirect respond ~status ~location] sends a redirect response.

    {[
      redirect respond ~status:Moved_permanently ~location:"/new-path"
    ]} *)

val not_found : local_ respond -> unit
(** [not_found respond] sends a 404 response with "Not Found" body. *)

val respond_string : local_ respond -> status:Httpz.Res.status -> ?headers:local_ resp_header list -> string -> unit
(** [respond_string respond ~status ?headers body] sends a string response.

    {[
      respond_string respond ~status:Created
        ~headers:[(Location, "/users/123")]
        {|{"id": 123}|}
    ]} *)

(** {2 Streaming Responses} *)

val stream :
  local_ respond ->
  status:Httpz.Res.status ->
  ?headers:local_ resp_header list ->
  ?length:int ->
  ((string -> unit) -> unit) ->
  unit
(** [stream respond ~status ?headers ?length iter] sends a streaming response.

    The [iter] function receives a [write_chunk] callback to produce body data:
    {[
      stream respond ~status:Success (fun write ->
        write "chunk 1";
        write "chunk 2";
        write "final chunk")
    ]}

    Use [~length] if the total size is known (enables Content-Length). *)

val respond_bigstring :
  local_ respond ->
  status:Httpz.Res.status ->
  ?headers:local_ resp_header list ->
  Base_bigstring.t ->
  off:int ->
  len:int ->
  unit
(** [respond_bigstring respond ~status ?headers buf ~off ~len] sends a
    zero-copy response from a bigstring slice.

    Useful for serving static file content without copying. *)

(** {1 Request Context} *)

type ctx
(** Handler context providing access to path and query parameters. *)

val path : ctx -> string
(** [path ctx] returns the request path as a string. *)

val query_param : ctx -> string -> string option
(** [query_param ctx name] finds the first query parameter with [name].

    {[
      match query_param ctx "page" with
      | Some p -> int_of_string p
      | None -> 1
    ]} *)

val query_params : ctx -> string -> string list
(** [query_params ctx name] returns all query parameters with [name].

    Useful for multi-value parameters like [?tag=a&tag=b]. *)

val query : ctx -> (string * string) list
(** [query ctx] returns all query parameters as key-value pairs. *)

val cache_key : ctx -> string
(** [cache_key ctx] generates a cache key from path and query.

    Useful for implementing response caching. *)

(** {1 Header Requirements}

    Declare which headers a route needs. The type system ensures handlers
    receive the correct header tuple structure. *)

type _ hdr
(** Header requirement specification. Type parameter encodes the tuple structure. *)

val h0 : unit hdr
(** [h0] indicates no headers needed. Handler receives [()]. *)

val ( +> ) : Httpz.Header_name.t -> 'a hdr -> (string option * 'a) hdr
(** [name +> rest] adds a header requirement.

    {[
      (* Require Content-Type and Accept *)
      let my_headers = Httpz.Header_name.Content_type +> Httpz.Header_name.Accept +> h0
      (* Type: (string option * (string option * unit)) hdr *)

      (* Handler receives the tuple *)
      fun (content_type, (accept, ())) ctx respond -> ...
    ]} *)

(** {1 Path Patterns}

    Type-safe path patterns using a GADT. The type parameter encodes
    what values are captured. *)

type _ pat
(** Path pattern that captures values of type ['a]. *)

val root : unit pat
(** [root] matches the root path [/] exactly. Captures [()]. *)

val lit : string -> unit pat
(** [lit s] matches a literal segment [s]. Captures [()].

    {[
      lit "api"  (* matches /api *)
    ]} *)

val seg : string pat
(** [seg] captures any single path segment as a string.

    {[
      "users" **> seg  (* matches /users/alice, captures "alice" *)
    ]} *)

val tail : string pat
(** [tail] captures the remaining path as a string.

    {[
      "static" **> tail  (* matches /static/css/style.css, captures "css/style.css" *)
    ]} *)

val ( ** ) : 'a pat -> 'b pat -> ('a * 'b) pat
(** [p1 ** p2] composes two patterns, capturing both values as a tuple.

    {[
      lit "users" ** seg ** lit "posts" ** seg
      (* matches /users/alice/posts/123, captures ((((), "alice"), ()), "123") *)
    ]} *)

val ( **> ) : string -> 'a pat -> 'a pat
(** [prefix **> pat] matches literal [prefix] then [pat], keeping only
    [pat]'s captures.

    {[
      "users" **> seg  (* matches /users/alice, captures "alice" *)
      "api" **> "v1" **> seg  (* matches /api/v1/resource, captures "resource" *)
    ]} *)

val ( <** ) : 'a pat -> string -> 'a pat
(** [pat <** suffix] matches [pat] then literal [suffix], keeping only
    [pat]'s captures.

    {[
      seg <** ".html"  (* matches /about.html, captures "about" *)
    ]} *)

(** {1 Handlers} *)

type ('a, 'h) handler = 'a -> 'h -> ctx -> respond -> unit
(** Handler function type.

    - ['a]: Captured path values
    - ['h]: Extracted header values
    - [ctx]: Request context
    - [respond]: Response writer (call this to send the response) *)

(** {1 Routes} *)

type route
(** A single route binding method, pattern, headers, and handler. *)

(** {2 Routes Without Header Requirements} *)

val get : 'a pat -> ('a -> ctx -> respond -> unit) -> route
(** [get pat handler] creates a GET route.

    {[
      get ("users" **> seg) (fun user_id ctx respond ->
        html respond (Printf.sprintf "User: %s" user_id))
    ]} *)

val post : 'a pat -> ('a -> ctx -> respond -> unit) -> route
(** [post pat handler] creates a POST route. *)

val put : 'a pat -> ('a -> ctx -> respond -> unit) -> route
(** [put pat handler] creates a PUT route. *)

val delete : 'a pat -> ('a -> ctx -> respond -> unit) -> route
(** [delete pat handler] creates a DELETE route. *)

val get_ : string list -> (ctx -> respond -> unit) -> route
(** [get_ segments handler] creates a GET route matching exact path segments.

    {[
      get_ ["api"; "health"] (fun _ctx respond -> json respond {|{"ok":true}|})
    ]} *)

val post_ : string list -> (ctx -> respond -> unit) -> route
(** [post_ segments handler] creates a POST route matching exact segments. *)

(** {2 Routes With Header Requirements} *)

val get_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
(** [get_h pat headers handler] creates a GET route that extracts headers.

    {[
      get_h ("api" **> seg)
        (Httpz.Header_name.Authorization +> h0)
        (fun resource (auth, ()) ctx respond ->
          match auth with
          | Some token -> serve_resource resource token respond
          | None -> respond_string respond ~status:Unauthorized "Auth required")
    ]} *)

val post_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val put_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val delete_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route

val route_h : Httpz.Method.t -> 'a pat -> 'h hdr -> ('a, 'h) handler -> route
(** [route_h method_ pat headers handler] creates a route for any method. *)

(** {1 Route Collections} *)

type t
(** Collection of routes for dispatch. *)

val empty : t
(** Empty route collection. *)

val add : route -> t -> t
(** [add route routes] adds a route to the collection. *)

val of_list : route list -> t
(** [of_list routes] creates a collection from a list.

    {[
      let routes = of_list [
        get_ [] home_handler;
        get ("users" **> seg) user_handler;
        post_ ["api"; "login"] login_handler;
      ]
    ]} *)

(** {1 Dispatch} *)

val dispatch :
  bytes ->
  meth:Httpz.Method.t ->
  target:Httpz.Span.t ->
  headers:local_ Httpz.Header.t list ->
  t ->
  respond:respond ->
  bool
(** [dispatch buf ~meth ~target ~headers routes ~respond] dispatches a request.

    Tries each route in order until one matches. On match, calls the handler
    with the [respond] function for direct response writing.

    Returns [true] if a route matched, [false] otherwise (caller should
    handle 404).

    {[
      let matched = Route.dispatch buf ~meth ~target ~headers routes ~respond in
      if not matched then
        Route.not_found respond
    ]} *)
