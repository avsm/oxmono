(** Whole-object cache with a transactional URL index and immutable SHA-256
    blobs. URLs must identify immutable public objects. *)

type t
type entry
type selection = Whole | From of int * int option | Suffix of int
type range = { start : int; stop : int }
type outcome = Hit | Miss | Shared
type result = Found of entry | Missing of int

type limits = {
  max_object_bytes : int;
  reserve_bytes : int64;
  downloads : int;
  timeout : Duration.t;
  max_entries : int;
}

val default_limits : limits
(** Defaults: 8 GiB per object, 5 GiB filesystem reserve, four concurrent
    downloads, 30 minutes per active fetch, and one million URL entries. *)

exception Unsatisfiable of int
exception Upstream of string
exception Limit of string

val with_open :
  dir:Eio.Fs.dir_ty Eio.Path.t ->
  clock:_ Eio.Time.Mono.t ->
  wall_clock:_ Eio.Time.clock ->
  random:Eio.Flow.source_ty Eio.Resource.t ->
  ?client:Fetch.plain ->
  ?limits:limits ->
  ?upstreams:string list ->
  (t -> 'a) ->
  'a
(** [with_open ... f] owns the cache directory for [f], including all readers.
    It refuses another owner, symlinked storage and legacy JSON/sparse caches.
    Resources close before the ownership lock is released. [client] is required
    only for misses. [upstreams] must then explicitly allow the roots used by
    initial requests and redirects. Join all child fibers before returning from
    [f]. *)

val get : t -> url:string -> result * outcome
(** [get t ~url] returns the complete cached object, downloading a whole GET on
    a miss. It sends no Range, cookies or client credentials. Concurrent misses
    share a download. A cancelled owner allows remaining callers to retry. 404
    and 410 results are cached permanently. *)

val size : entry -> int
val content_type : entry -> string
val etag : entry -> string option
val modified : entry -> string option
val digest : entry -> string
val resolve : int -> selection -> range

val write : entry -> range -> Proffer.Body.Sink.t -> unit
(** [write entry range sink] streams the selected bytes with a 64 KiB buffer.
    The entry and its cache owner must remain live throughout the call. *)

val logger : t -> Logger.t

val key : string -> string
(** [key url] is a SHA-256 identity for joining telemetry to the URL index. *)

val forget : t -> url:string -> unit
(** [forget t ~url] removes its index reference. Existing blobs remain until GC.
    This is an offline maintenance operation, before serving any requests. *)

type collected = { blobs : int; bytes : int64; temporary : int }

val gc : t -> collected
(** [gc t] removes orphan blobs and interrupted temporary downloads. Call only
    during offline maintenance, before starting any requests on [t]. The
    process-wide ownership lock excludes a running server or another GC. *)

val summary : sw:Eio.Switch.t -> _ Eio.Path.t -> string
(** [summary ~sw dir] reads cache counts and deduplication totals as JSON
    without acquiring the writer lock, including while a server is running. *)

type progress = Logger.transfer = {
  key : string;
  path : string;
  bytes : int;
  total : int option;
  age : float;
}

val activity : t -> int * progress list
(** [activity t] is the number of downloads waiting for a slot and active
    transfers, with consumed bytes, optional length and monotonic age in
    seconds. Followers sharing a transfer do not count as queued downloads. *)
