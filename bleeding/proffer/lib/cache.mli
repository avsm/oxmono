(** This module provides a concurrent memoization cache of rendered bodies and
    entity-tags. *)

type t : value mod portable contended
(** A cache shared between fibers and domains. *)

val create : ?max_entries:int -> ttl:Duration.t -> unit -> t @@ portable
(** [create ~ttl ()] is an empty cache holding at most [max_entries] entries,
    1024 by default. [ttl] is each entry's lifetime.

    @raise Invalid_argument if [max_entries] is not positive. *)

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
