(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Response Caching per RFC 9111

    This module provides an in-memory cache for HTTP responses following
    RFC 9111 (HTTP Caching). It handles cache storage, validation, and
    freshness calculations.

    {2 Cache Keys}

    Cache entries are keyed by method and effective request URI per RFC 9111.
    Only safe methods (GET, HEAD) are cached by default.

    {2 Examples}

    {[
      (* Create an in-memory cache *)
      let cache = Cache.Memory.create ~max_entries:1000 () in

      (* Store a response *)
      Cache.Memory.store cache ~key ~response ~request_time ~response_time;

      (* Lookup a cached response *)
      match Cache.Memory.lookup cache ~key ~now with
      | Some (entry, status) ->
        Printf.printf "Cache %s: %s\n"
          (match status with `Fresh -> "hit" | `Stale -> "stale")
          entry.url
      | None -> Printf.printf "Cache miss\n"
    ]}
*)

(** Log source for cache operations *)
val src : Logs.Src.t

(** {1 Cache Entry} *)

(** A cached response entry *)
type entry = {
  url : string;
  (** The effective request URI *)

  method_ : Method.t;
  (** HTTP method used for the request *)

  status : int;
  (** Response status code *)

  headers : Headers.t;
  (** Response headers *)

  body : string;
  (** Response body *)

  request_time : Ptime.t;
  (** When the request was initiated *)

  response_time : Ptime.t;
  (** When the response was received *)

  date_value : Ptime.t option;
  (** Parsed Date header value *)

  age_value : int;
  (** Age header value (0 if not present) *)

  cache_control : Cache_control.response;
  (** Parsed Cache-Control header *)

  etag : string option;
  (** ETag header for validation *)

  last_modified : string option;
  (** Last-Modified header for validation *)

  vary_headers : (string * string) list;
  (** Request header values for Vary matching *)

  freshness_lifetime : int option;
  (** Calculated freshness lifetime in seconds *)
}

(** Cache lookup result status *)
type lookup_status =
  | Fresh
  (** Entry is fresh and can be served directly *)
  | Stale
  (** Entry is stale but might be served with revalidation or max-stale *)

(** {1 Cache Key} *)

(** A cache key for lookups *)
type key = {
  method_key : Method.t;
  uri : string;
  vary_values : (string * string) list;
  (** Values of Vary headers from request *)
}

val make_key :
  method_:Method.t ->
  uri:string ->
  ?request_headers:Headers.t ->
  ?vary:string list ->
  unit ->
  key
(** Create a cache key.
    @param method_ The HTTP method
    @param uri The effective request URI
    @param request_headers Request headers for Vary matching
    @param vary List of header names from Vary response header *)

(** {1 In-Memory Cache} *)

module Memory : sig
  (** In-memory HTTP response cache using a Hashtbl.
      Thread-safe using Eio.Mutex. *)

  type t
  (** The cache type *)

  val create : ?max_entries:int -> unit -> t
  (** Create a new in-memory cache.
      @param max_entries Maximum number of entries (default 10000) *)

  val store :
    t ->
    url:string ->
    method_:Method.t ->
    status:int ->
    headers:Headers.t ->
    body:string ->
    request_time:Ptime.t ->
    response_time:Ptime.t ->
    ?request_headers:Headers.t ->
    unit ->
    bool
  (** Store a response in the cache.
      Returns true if stored, false if not cacheable.

      @param url The effective request URI
      @param method_ The HTTP method
      @param status Response status code
      @param headers Response headers
      @param body Response body
      @param request_time When the request was initiated
      @param response_time When the response was received
      @param request_headers Request headers for Vary matching *)

  val lookup :
    t ->
    method_:Method.t ->
    uri:string ->
    ?request_headers:Headers.t ->
    now:Ptime.t ->
    unit ->
    (entry * lookup_status) option
  (** Look up a cached response.

      @param method_ The HTTP method
      @param uri The effective request URI
      @param request_headers Request headers for Vary matching
      @param now Current time for freshness check
      @return Some (entry, status) if found, None if not in cache *)

  val invalidate : t -> uri:string -> unit
  (** Remove all entries for a URI (used after unsafe methods). *)

  val clear : t -> unit
  (** Clear all entries from the cache. *)

  val size : t -> int
  (** Return the number of entries in the cache. *)

  val stats : t -> int * int * int
  (** Return cache statistics: (hits, misses, stores). *)
end

(** {1 Cache Validation} *)

val needs_validation : entry -> bool
(** Check if a cached entry needs revalidation with the origin server.
    True if must-revalidate or no-cache is set. *)

val validation_headers : entry -> Headers.t
(** Get headers to send for a conditional request.
    Includes If-None-Match (from ETag) and/or If-Modified-Since (from Last-Modified). *)

val is_not_modified : status:int -> bool
(** Check if a response indicates the cached version is still valid (304). *)

(** {1 Vary Header Support} *)

val parse_vary : string -> string list
(** Parse a Vary header value into a list of header names. *)

val vary_matches :
  cached_vary:(string * string) list ->
  request_headers:Headers.t ->
  bool
(** Check if request headers match the cached Vary values. *)
