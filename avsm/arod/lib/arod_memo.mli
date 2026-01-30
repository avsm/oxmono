(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Memoization cache with TTL

    This module provides a simple in-memory cache with time-to-live (TTL)
    expiration. It is useful for caching expensive computations like feed
    generation where the result can be reused for a period of time. *)

type 'a t
(** The type of a cache storing values of type ['a]. *)

val create : ttl:float -> unit -> 'a t
(** [create ~ttl ()] creates a cache where entries expire after [ttl] seconds. *)

val get : 'a t -> key:string -> 'a option
(** [get cache ~key] returns the cached value if present and not expired. *)

val set : 'a t -> key:string -> 'a -> unit
(** [set cache ~key value] stores [value] in the cache with the current
    timestamp. *)

val clear : 'a t -> unit
(** [clear cache] removes all entries from the cache. *)

val stats : 'a t -> int * int
(** [stats cache] returns [(hits, misses)] statistics. *)

val memoize : 'a t -> string -> (unit -> 'a) -> 'a
(** [memoize cache key f] returns a cached value for [key] if present and not
    expired, otherwise calls [f ()] and caches the result. *)
