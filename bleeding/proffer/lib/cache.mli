(** A memoization cache keyed by string, holding a rendered body and its
    entity-tag. It crosses domains, so a policy built once at startup is
    reachable from every domain's handlers. *)

type t : value mod portable contended
(** A cache. The kind is declared so a cache created once at startup stays
    reachable from a portable handler. An abstract type without one reads as
    contended there, and a cache that names only [portable] is unusable from
    the handlers it exists to serve. *)

val create : ttl:float -> t @@ portable
(** [create ~ttl] is an empty cache whose entries live [ttl] seconds. *)

val memoize :
  t -> now:float -> key:string -> (unit -> string) -> string * Etag.t
  @@ portable
(** [memoize t ~now ~key gen] is the body under [key] and an entity-tag over
    it. It runs [gen] and stores the result when [key] is absent or its entry
    is older than the cache's [ttl] at [now], and returns the stored body
    otherwise. [now] is seconds since the epoch, passed in so the core reads no
    clock. [gen] runs on the calling domain and is not stored, so it may
    capture domain-bound state. Two domains racing on a miss both run [gen] and
    one result wins, which is the right trade for memoization. *)

val stats : t -> int * int @@ portable
(** [stats t] is the hit and miss counts since [t] was created. *)
