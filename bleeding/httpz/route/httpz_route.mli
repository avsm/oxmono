(** Segment-based HTTP routing.

    Routes match literal path segments against the parse buffer in place. A
    segment is copied into a string only when a {!seg} or {!tail} captures it,
    so a route of literals allocates nothing.

    Handlers receive [ctx @ local] and [respond @ local]. Both borrow the
    connection's parse buffer, which the next request reuses; neither may
    outlive the handler.

    {2 Quick Start}

    {[
      open Httpz_route

      let routes = of_list [
        get_ [] (fun _ctx respond -> html respond "Welcome!");
        get_ ["api"; "status"] (fun _ctx respond -> json respond {|{"ok":true}|});

        get ("users" / seg root) (fun (user_id, ()) _ctx respond ->
          html respond (sprintf "User: %s" user_id));

        get ("static" / tail) (fun path _ctx respond ->
          plain respond (String.concat "/" path));
      ]
    ]}

    {2 Pattern Syntax}

    [( / )] is left-associative, so chained literals need parentheses:

    {[
      "users" / seg root            (* /users/:id *)
      "api" / ("v1" / seg root)     (* /api/v1/:resource *)
      "static" / tail               (* /static/** *)
      root                          (* / *)
    ]}

    {!get_} takes a plain list instead: [get_ ["api"; "v1"; "health"] h]. *)

(** {1 Response Body} *)

type body =
  | Empty
  | String of string
  | Bigstring of { buf : Base_bigstring.t; off : int; len : int }
  | Stream of { length : int option; iter : (string -> unit) -> unit }
  | Stream_bigstring of
      { length : int option
      ; iter : (Base_bigstring.t -> off:int -> len:int -> unit) -> unit
      }
      (** Like {!Stream}, but emits each chunk from a bigstring instead of
          copying it into a string. The buffer passed to the callback is valid
          only for that call and may be reused afterwards. *)

(** {1 Response Headers} *)

type resp_header = Httpz.Header_name.t * string

(** {1 Response Writing} *)

type respond = status:Httpz.Res.status -> headers:local_ resp_header list -> body -> unit
(** Response writer. Headers are [local_] for stack allocation. *)

(** {2 Response Helpers}

    All helpers take [local_ respond] to enable stack-allocated closures. *)

val html : local_ respond -> string -> unit @@ portable
val json : local_ respond -> string -> unit @@ portable
val xml : local_ respond -> string -> unit @@ portable
val atom : local_ respond -> string -> unit @@ portable
val plain : local_ respond -> string -> unit @@ portable
val redirect : local_ respond -> status:Httpz.Res.status -> location:string -> unit @@ portable
val not_found : local_ respond -> unit @@ portable
val respond_string :
  local_ respond -> status:Httpz.Res.status -> ?headers:local_ resp_header list
  -> string -> unit
  @@ portable
val stream : local_ respond -> status:Httpz.Res.status -> ?headers:local_ resp_header list -> ?length:int ->
  ((string -> unit) -> unit) -> unit @@ portable

(** {1 Request Context} *)

type ctx
(** Request context providing access to path, query, and HTTP method. *)

val meth : ctx @ local -> Httpz.Method.t @@ portable
(** Get the HTTP method of the request. *)

val is_head : ctx @ local -> bool @@ portable
(** Returns [true] if this is a HEAD request. Use this to skip expensive
    body generation - the routing layer automatically matches HEAD requests
    to GET routes. *)

val path : ctx @ local -> string @@ portable
(** Get request path as string (e.g., "/users/123"). *)

val query_param : ctx @ local -> string -> string option @@ portable
(** Find first query parameter by name. *)

val query_params : ctx @ local -> string -> string list @@ portable
(** Find all query parameters by name. *)

val query : ctx @ local -> (string * string) list @@ portable
(** Get all query parameters. *)

val body : ctx @ local -> Httpz.Span.t @@ portable
(** [body ctx] returns the request body as an unboxed span into the parse buffer.
    Zero-copy — no allocation until you call {!body_string}.
    Returns a span with [len = 0] for no-body requests.
    Returns a span with [len = -1] for chunked encoding. *)

val body_string : ctx @ local -> string option @@ portable
(** [body_string ctx] materializes the body as a string, or [None] if empty. *)

val content_length : ctx @ local -> int64# @@ portable
(** [content_length ctx] returns the Content-Length value, or [-1L] if absent. *)

(** {2 Response Helpers - WebDAV} *)

val xml_multistatus : local_ respond -> string -> unit @@ portable
(** [xml_multistatus respond xml] sends a 207 Multi-Status response
    with [Content-Type: application/xml]. *)

(** {2 Lazy Response Helpers}

    These variants skip body generation for HEAD requests. The thunk is only
    called for non-HEAD requests. Use these when body generation is expensive. *)

val html_gen : ctx @ local -> local_ respond -> (unit -> string) -> unit @@ portable
val json_gen : ctx @ local -> local_ respond -> (unit -> string) -> unit @@ portable
val xml_gen : ctx @ local -> local_ respond -> (unit -> string) -> unit @@ portable
val atom_gen : ctx @ local -> local_ respond -> (unit -> string) -> unit @@ portable
val plain_gen : ctx @ local -> local_ respond -> (unit -> string) -> unit @@ portable

(** {1 Path Patterns} *)

type _ pat : immutable_data
(** Path pattern. Type parameter encodes captured values. A pattern holds no
    mutable state, so one may be built on any domain. *)

val root : unit pat @@ portable
(** Match root path [/]. *)

val ( / ) : string -> 'a pat -> 'a pat @@ portable
(** [prefix / rest] matches literal segment, then continues. *)

val lit : string -> 'a pat -> 'a pat @@ portable
(** Same as [( / )]. *)

val seg : 'a pat -> (string * 'a) pat @@ portable
(** Capture any segment, then continue. *)

val tail : string list pat @@ portable
(** Capture all remaining segments. *)

(** {1 Header Requirements} *)

type _ hdr : immutable_data

val h0 : unit hdr @@ portable
val ( +> ) : Httpz.Header_name.t -> 'a hdr -> (string option * 'a) hdr @@ portable

(** {1 Handlers} *)

type ('a, 'h) handler = 'a -> 'h -> ctx @ local -> local_ respond -> unit
(** Handler function with local respond for stack allocation. *)

(** {1 Routes} *)

type route

val get : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val post : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val put : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val delete : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable

val get_ : string list -> (ctx @ local -> local_ respond -> unit) -> route @@ portable
val post_ : string list -> (ctx @ local -> local_ respond -> unit) -> route @@ portable

val get_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val post_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val put_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val delete_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable

val get_h1 :
  'a pat -> Httpz.Header_name.t
  -> ('a -> string option -> ctx @ local -> local_ respond -> unit)
  -> route
  @@ portable
val post_h1 :
  'a pat -> Httpz.Header_name.t
  -> ('a -> string option -> ctx @ local -> local_ respond -> unit)
  -> route
  @@ portable

val route : Httpz.Method.t -> 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable

(** {2 WebDAV Route Constructors} *)

val propfind : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val proppatch : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val mkcol : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val report : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val copy_ : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val move_ : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val lock : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable
val unlock : 'a pat -> ('a -> ctx @ local -> local_ respond -> unit) -> route @@ portable

val propfind_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val proppatch_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val mkcol_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val report_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val copy_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val move_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val lock_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable
val unlock_h : 'a pat -> 'h hdr -> ('a, 'h) handler -> route @@ portable

(** {1 Route Collections} *)

type t

val empty : t @@ portable
val of_list : route list -> t @@ portable
val add : route -> t -> t @@ portable

(** {1 Dispatch} *)

val dispatch :
  bytes ->
  meth:Httpz.Method.t ->
  path:Httpz.Span.t ->
  query:Httpz.Span.t ->
  body:Httpz.Span.t ->
  content_length:int64# ->
  headers:local_ Httpz.Header.t list ->
  t ->
  respond:local_ respond ->
  bool @@ portable
(** [dispatch buf ~meth ~path ~query ~body ~content_length ~headers routes
    ~respond] runs the first matching route and returns whether one matched.

    [path], [query] and [body] are spans into [buf]; pass [req.#path] and
    [req.#query] from {!Httpz.parse} rather than splitting the target again.
    [content_length] is [-1L] when the header is absent. *)
