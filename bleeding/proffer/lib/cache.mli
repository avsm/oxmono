(** This module provides a concurrent memoization cache of rendered bodies and
    entity-tags. *)

type t : value mod portable contended
(** A [t] is a cache that may be created once at startup and shared by handlers.
    The kind is declared so it stays reachable from a portable handler. An
    abstract type without one reads as contended there, and a cache that names
    only [portable] is unusable from the handlers it exists to serve. *)

val create : ?max_entries:int -> ttl:float -> unit -> t @@ portable
(** [create ~ttl ()] is an empty cache whose entries live [ttl] seconds and
    which holds at most [max_entries] of them, 1024 by default. It raises
    [Invalid_argument] unless [ttl] is finite and nonnegative and
    [max_entries] is positive. *)

val memoize :
  t -> now:float -> key:string -> (unit -> string) -> string * Etag.t
  @@ portable
(** [memoize t ~now ~key gen] is the cached body under [key] and its entity-tag.
    It calls [gen] when the key is absent or expired at [now], measured in
    seconds from a clock used consistently for every call. [gen] runs on the
    calling domain and is not stored, so it may capture domain-bound state.
    Concurrent misses may call [gen] more than once, and one generated value is
    retained. A miss also removes all expired entries, and evicts the least
    recently used entry when the cache is at [max_entries]. It raises
    [Invalid_argument] unless [now] is finite. *)

val stats : t -> int * int @@ portable
(** [stats t] is the hit and miss counts since [t] was created. *)
