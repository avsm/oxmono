(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** TTL-based cache for rendered HTML responses.

    Used to memoize expensive rendering operations for content routes.
    Static file routes and dynamic (query-dependent) routes bypass the cache. *)

type t
(** The cache type. *)

val create : ttl:float -> t
(** [create ~ttl] creates a cache with the given TTL in seconds. *)

val cache_key : path:string -> query:(string * string) list -> string
(** [cache_key ~path ~query] computes a cache key from request path and
    query parameters. Query parameters are sorted for consistent keys. *)

val get : t -> string -> string option
(** [get t key] returns the cached value if present and not expired. *)

val set : t -> string -> string -> unit
(** [set t key value] stores a value in the cache. *)

val clear : t -> unit
(** [clear t] removes all entries and resets statistics. *)

val stats : t -> int * int
(** [stats t] returns [(hits, misses)]. *)

val size : t -> int
(** [size t] returns the number of entries in the cache. *)

val memoize : t -> string -> (unit -> string) -> string
(** [memoize t key f] returns the cached value for [key] if present,
    otherwise calls [f ()], caches the result, and returns it. *)
