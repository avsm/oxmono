(** Zero-copy HTTP routing for httpz.

    Routes use a GADT pattern DSL for type-safe path matching.
    Path matching is zero-copy using spans. String capture only on match.
    Headers are declared per-route and extracted only when needed.

    {2 Example}

    {[
      open Httpz.Route

      let hello name () ctx = html ("Hello, " ^ name ^ "!")

      (* Route needing Content-Type header *)
      let upload data (content_type, ()) ctx =
        match content_type with
        | Some ct -> html ("Got " ^ ct)
        | None -> respond ~status:Bad_request "Missing Content-Type"

      let routes = of_list [
        get ("hello" **> seg) hello;
        post_h ("upload" **> seg) (Header_name.Content_type +> h0) upload;
        get_ [] (fun ctx -> html "Welcome!");
      ]
    ]} *)

(** {1 Response} *)

type body =
  | Empty
  | String of string
  | Bigstring of { buf : Base_bigstring.t; off : int; len : int }
  | Stream of { length : int option; iter : (string -> unit) -> unit }
(** Response body - immediate or streaming. Bigstring variant uses Base_bigstring.t for zero-copy file serving. *)

(** {1 Response Headers}

    Typed response headers using Header_name.t for type safety.
    Headers are local for stack allocation. *)

type resp_header = Header_name.t * string
(** A response header: typed name and string value. *)

(** {1 Response Writer}

    Handlers receive a local [respond] function for direct response writing.
    No intermediate record - fully CPS with stack-allocated closures and headers. *)

type respond = status:Res.status -> headers:local_ resp_header list -> body -> unit
(** Response writer function. Headers list is local for stack allocation. *)

(** {2 Response helpers}

    Convenience functions that call the respond function with common patterns.
    All use local respond and local typed headers for zero allocation. *)

val html : local_ respond -> string -> unit
val json : local_ respond -> string -> unit
val xml : local_ respond -> string -> unit
val atom : local_ respond -> string -> unit
val plain : local_ respond -> string -> unit
val redirect : local_ respond -> status:Res.status -> location:string -> unit
val not_found : local_ respond -> unit
val respond_string : local_ respond -> status:Res.status -> ?headers:local_ resp_header list -> string -> unit

(** {2 Streaming responses} *)

val stream :
  local_ respond ->
  status:Res.status ->
  ?headers:local_ resp_header list ->
  ?length:int ->
  ((string -> unit) -> unit) ->
  unit
(** [stream respond ~status iter] writes a streaming response.
    [iter write_chunk] is called to produce the body - call [write_chunk]
    for each chunk. Optional [length] sets Content-Length if known. *)

val respond_bigstring :
  local_ respond ->
  status:Res.status ->
  ?headers:local_ resp_header list ->
  Base_bigstring.t ->
  off:int ->
  len:int ->
  unit
(** Zero-copy response from bigstring slice (e.g., static file content). *)

(** {1 Context}

    Passed to handlers for path/query access. *)

type ctx
(** Handler context with buffer, path, and query. *)

val path : ctx -> string
(** Get request path as string. *)

val query_param : ctx -> string -> string option
(** Find query parameter by name. *)

val query_params : ctx -> string -> string list
(** Find all query parameters with given name. *)

val query : ctx -> (string * string) list
(** Get all query parameters as pairs. *)

val cache_key : ctx -> string
(** Generate cache key from path and query. *)

(** {1 Header Requirements}

    Declare which headers a route needs. Headers are extracted during
    dispatch, avoiding closure overhead and unnecessary copying. *)

type _ hdr
(** Header requirement specification. *)

val h0 : unit hdr
(** No headers needed. *)

val ( +> ) : Header_name.t -> 'a hdr -> (string option * 'a) hdr
(** [name +> rest] requires header [name] plus [rest].
    {[
      (* Require Content-Type and Accept *)
      let my_headers = Header_name.Content_type +> Header_name.Accept +> h0
      (* Type: (string option * (string option * unit)) hdr *)
    ]} *)

(** {1 Patterns}

    Type-safe path patterns using a GADT. *)

type _ pat
(** Path pattern that captures values of type ['a]. *)

val root : unit pat
(** Match root path "/". *)

val lit : string -> unit pat
(** Match literal segment. *)

val seg : string pat
(** Capture segment as string. *)

val tail : string pat
(** Capture remaining path as string. *)

val ( ** ) : 'a pat -> 'b pat -> ('a * 'b) pat
(** Compose patterns. *)

val ( **> ) : string -> 'a pat -> 'a pat
(** [prefix **> pattern] matches literal [prefix] then [pattern], returning
    only the pattern's capture. ["users" **> seg] captures the username. *)

val ( <** ) : 'a pat -> string -> 'a pat
(** [pattern <** suffix] matches [pattern] then literal [suffix], returning
    only the pattern's capture. [seg <** ""] handles trailing slash. *)

(** {1 Routes} *)

type ('a, 'h) handler = 'a -> 'h -> ctx -> respond -> unit
(** Handler receiving path captures, extracted headers, context, and respond function.
    The handler calls [respond] to write the response - no return value needed. *)

type route
(** A route binding method + pattern + headers to handler. *)

(** {2 Routes without headers} *)

val get : 'a pat -> ('a -> ctx -> respond -> unit) -> route
val post : 'a pat -> ('a -> ctx -> respond -> unit) -> route
val put : 'a pat -> ('a -> ctx -> respond -> unit) -> route
val delete : 'a pat -> ('a -> ctx -> respond -> unit) -> route

val get_ : string list -> (ctx -> respond -> unit) -> route
(** [get_ ["api"; "users"] handler] matches GET /api/users exactly. *)

val post_ : string list -> (ctx -> respond -> unit) -> route
(** [post_ ["api"; "users"] handler] matches POST /api/users exactly. *)

(** {2 Routes with headers} *)

val get_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val post_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val put_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val delete_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route
val route_h : Method.t -> 'a pat -> 'h hdr -> ('a, 'h) handler -> route

type t
(** Route collection. *)

val empty : t
val add : route -> t -> t
val of_list : route list -> t

(** {1 Dispatch} *)

val dispatch :
  bytes ->
  meth:Method.t ->
  target:Span.t ->
  headers:local_ Header.t list ->
  t ->
  respond:respond ->
  bool
(** [dispatch buf ~meth ~target ~headers routes ~respond] dispatches the
    request to matching route. If a route matches, calls the handler with
    the [respond] function for direct response writing. Returns [true] if
    matched, [false] otherwise. Zero allocation in the hot path. *)
