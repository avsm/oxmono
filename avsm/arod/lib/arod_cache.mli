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

val get : t -> string -> string option
(** [get t key] returns the cached value if present and not expired. *)

val set : t -> string -> string -> unit
(** [set t key value] stores a value in the cache. *)
