(** Whole-object HTTP cache for immutable public stores, including Zarr. *)

module Cache = Cache
module Logger = Logger

type mapping
type t

val mapping : prefix:string -> upstream:string -> mapping
(** [mapping ~prefix ~upstream] maps an absolute local path to an HTTP root.
    Trailing slashes are forbidden except for the local root ["/"]. *)

val upstream : mapping -> string

val create : cache:Cache.t -> ?allow_query:bool -> mapping list -> t
(** [create ~cache mappings] selects the longest matching prefix. Query strings
    are rejected unless [allow_query] is true (default false). Configure the
    cache's upstream allowlist to include these roots and approved redirects. *)

val site : t Proffer.Site.t
(** [site] serves GET, HEAD and CORS OPTIONS. A cold GET or HEAD downloads and
    publishes the complete object before responding. Explicit, open-ended and
    suffix ranges are sliced from immutable local bytes. Multi-range requests
    return 400. Hits and negative entries are permanent. No Zarr codecs are
    interpreted and client credentials are not forwarded. *)

val object_key : t -> string -> string option
(** [object_key t target] hashes the mapped upstream URL for telemetry joins. *)
