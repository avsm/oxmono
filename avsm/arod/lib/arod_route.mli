(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Framework-agnostic HTTP routing

    This module provides a pure routing abstraction that is independent of any
    specific HTTP server implementation. Routes are defined using a typed
    pattern DSL and pure handler functions that take requests and return
    responses. *)

(** HTTP methods. *)
type meth = [ `GET | `POST | `PUT | `DELETE | `HEAD | `OPTIONS ]

val meth_to_string : meth -> string
(** [meth_to_string m] returns the string representation of method [m]. *)

val meth_of_string : string -> meth option
(** [meth_of_string s] parses an HTTP method from string [s]. *)

(** HTTP requests. *)
module Request : sig
  type t
  (** The type of HTTP requests. *)

  val create :
    meth:meth ->
    path:string ->
    query:(string * string) list ->
    headers:(string * string) list ->
    t
  (** [create ~meth ~path ~query ~headers] constructs a request. *)

  val meth : t -> meth
  (** [meth req] returns the HTTP method of the request. *)

  val path : t -> string
  (** [path req] returns the URL path of the request. *)

  val query : t -> (string * string) list
  (** [query req] returns all query parameters. *)

  val query_param : t -> string -> string option
  (** [query_param req name] returns the first value for query parameter [name]. *)

  val query_params : t -> string -> string list
  (** [query_params req name] returns all values for query parameter [name]. *)

  val headers : t -> (string * string) list
  (** [headers req] returns all request headers. *)

  val header : t -> string -> string option
  (** [header req name] returns the value of header [name]. *)

  val cache_key : t -> string
  (** [cache_key req] returns a string suitable for use as a memoization key,
      combining path and sorted query parameters. *)
end

(** HTTP responses. *)
module Response : sig
  type t
  (** The type of HTTP responses. *)

  val html : string -> t
  (** [html content] creates an HTML response with content-type text/html. *)

  val json : string -> t
  (** [json content] creates a JSON response with content-type application/json. *)

  val xml : string -> t
  (** [xml content] creates an XML response with content-type application/xml. *)

  val atom : string -> t
  (** [atom content] creates an Atom feed response with content-type
      application/atom+xml. *)

  val plain : string -> t
  (** [plain content] creates a plain text response. *)

  val redirect : code:int -> location:string -> t
  (** [redirect ~code ~location] creates a redirect response. *)

  val not_found : t
  (** [not_found] is a 404 Not Found response. *)

  val raw : status:int -> headers:(string * string) list -> string -> t
  (** [raw ~status ~headers body] creates a response with explicit status,
      headers, and body. *)

  val status : t -> int
  (** [status resp] returns the HTTP status code. *)

  val headers : t -> (string * string) list
  (** [headers resp] returns the response headers. *)

  val body : t -> string
  (** [body resp] returns the response body. *)
end

(** {1 Path Pattern DSL}

    Patterns match URL paths and can capture path segments as typed values. *)

type 'a pattern
(** The type of path patterns that capture a value of type ['a]. *)

val root : unit pattern
(** [root] matches the root path "/". *)

val exact : string -> unit pattern
(** [exact s] matches the exact path segment [s]. *)

val param : string pattern
(** [param] matches any path segment and captures it as a string. *)

val rest : string pattern
(** [rest] matches and captures the rest of the path (everything after
    this point). *)

val ( @/ ) : 'a pattern -> 'b pattern -> ('a * 'b) pattern
(** [p1 @/ p2] composes patterns [p1] and [p2], matching [p1] followed by [p2]
    and combining their captured values into a tuple. *)

(** {1 Route Definitions} *)

type handler = Request.t -> Response.t
(** A handler is a function from request to response. *)

type 'a param_handler = 'a -> Request.t -> Response.t
(** A parameterized handler receives captured path parameters. *)

type t
(** The type of a single route. *)

val get : 'a pattern -> 'a param_handler -> t
(** [get pattern handler] creates a GET route. *)

val post : 'a pattern -> 'a param_handler -> t
(** [post pattern handler] creates a POST route. *)

val route : meth -> 'a pattern -> 'a param_handler -> t
(** [route meth pattern handler] creates a route for method [meth]. *)

val get_ : string list -> handler -> t
(** [get_ segments handler] creates a GET route matching exact path segments
    (e.g., [get_ ["api"; "users"] handler] matches "/api/users"). *)

val post_ : string list -> handler -> t
(** [post_ segments handler] creates a POST route matching exact path segments. *)

(** {1 Route Collection} *)

module Routes : sig
  type route = t
  (** Alias for route type. *)

  type t
  (** A collection of routes. *)

  val empty : t
  (** The empty route collection. *)

  val add : route -> t -> t
  (** [add route routes] adds a route to the collection. *)

  val of_list : route list -> t
  (** [of_list routes] creates a collection from a list of routes. *)

  val dispatch : t -> Request.t -> Response.t option
  (** [dispatch routes req] attempts to find a matching route and invoke its
      handler. Returns [None] if no route matches. *)

  val fold : (route -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f routes acc] folds over all routes in the collection. *)
end
