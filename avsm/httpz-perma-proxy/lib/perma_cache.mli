(** Permanent cache library for HTTP proxy.

    Provides range tracking, metadata serialization, and cache file I/O. *)

(** {1 Range tracking} *)

type range = { start : int; stop : int }
(** A byte range [\[start, stop)], exclusive of [stop]. *)

val merge_range : range list -> range -> range list
(** [merge_range ranges r] merges range [r] into [ranges], a sorted
    non-overlapping range list. Returns a new sorted, non-overlapping list
    with [r] coalesced into the existing ranges. *)

val is_range_cached : range list -> start:int -> len:int -> bool
(** [is_range_cached ranges ~start ~len] returns [true] if the byte range
    [\[start, start+len)] is fully covered by [ranges]. *)

val is_complete : range list -> total_len:int -> bool
(** [is_complete ranges ~total_len] returns [true] if [ranges] covers
    [\[0, total_len)]. *)

(** {1 Metadata} *)

type meta = {
  content_length : int;
  content_type : string;
  headers : (string * string) list;
  ranges : range list;
  complete : bool;
  status_code : int;
}
(** Cache entry metadata, persisted as JSON alongside cached data files. *)

val meta_jsont : meta Jsont.t
(** JSON type descriptor for {!meta}. *)

val read_meta : Eio.Fs.dir_ty Eio.Path.t -> string -> meta option
(** [read_meta fs key] loads the [.meta] JSON file for cache key [key]
    from the filesystem rooted at [fs]. Returns [None] if the file does
    not exist or cannot be parsed. *)

val write_meta : Eio.Fs.dir_ty Eio.Path.t -> string -> meta -> unit
(** [write_meta fs key meta] saves [meta] as a [.meta] JSON file for
    cache key [key] under [fs]. Creates parent directories as needed. *)

(** {1 Cache file I/O} *)

val cache_path : host:string -> url_path:string -> string
(** [cache_path ~host ~url_path] computes the relative filesystem path
    for a cached resource. *)

val write_data : sw:Eio.Switch.t -> Eio.Fs.dir_ty Eio.Path.t -> string -> off:int -> string -> unit
(** [write_data ~sw fs key ~off data] writes [data] at byte offset [off]
    in the data file for [key] using Eio random-access I/O. *)

val read_data : sw:Eio.Switch.t -> Eio.Fs.dir_ty Eio.Path.t -> string -> off:int -> len:int -> string
(** [read_data ~sw fs key ~off ~len] reads [len] bytes starting at byte
    offset [off] from the data file for [key]. *)

(** {1 Range header parsing} *)

val parse_range_header : string -> (int * int option) option
(** [parse_range_header s] parses an HTTP Range header value of the form
    ["bytes=START-END"] or ["bytes=START-"]. Returns
    [Some (start, Some end_inclusive)] or [Some (start, None)],
    or [None] if the header cannot be parsed. *)

(** {1 URL mapping} *)

type url_map = {
  prefix : string;
  upstream : string;
  upstream_host : string;
}
(** URL mapping from a local path prefix to an upstream URL. *)

val parse_map : string -> url_map option
(** [parse_map s] parses a string of the form ["PREFIX=UPSTREAM"] into a
    {!url_map}. Returns [None] if the string contains no ['=']. *)

val find_map : url_map list -> string -> (url_map * string) option
(** [find_map maps path] finds the first map whose prefix matches [path].
    Returns [Some (map, suffix)] where [suffix] is the remaining path after
    the prefix, or [None] if no map matches. *)

(** {1 Proxy handler} *)

type response = {
  status : Httpz.Res.status;
  resp_headers : Httpz_server.Route.resp_header list;
  body : Httpz_server.Route.body;
}
(** Response data returned by {!handle_request}. *)

val handle_request :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_dir:string ->
  session:Requests.t ->
  maps:url_map list ->
  verbose:bool ->
  path:string ->
  is_head:bool ->
  range_header:string option ->
  response
(** [handle_request ~sw ~net ~fs ~cache_dir ~session ~maps ~verbose ~path
    ~is_head ~range_header] handles an HTTP request by looking up the
    appropriate URL map, checking the cache, and fetching from upstream
    as needed.  Large fetches are streamed incrementally through the
    proxy (tee-ing to cache while forwarding to the client).
    Returns a {!response} with CORS headers included. *)

val cors_preflight_response : response
(** Response for OPTIONS preflight requests with CORS headers. *)
