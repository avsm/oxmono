(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Permanent sparse-object cache.

    Cached bytes are never revalidated on hits. URLs must name immutable
    objects. Clear the cache while the server is stopped to refresh objects
    changed in place. Validators are checked on misses to prevent mixing
    revisions. One server process must own a cache directory. *)
type t
type entry
type selection = Whole | From of int * int option | Suffix of int
exception Unsatisfiable of int
(** [Unsatisfiable size] reports a range outside a [size]-byte object. *)
exception Upstream of string
(** [Upstream message] reports an invalid response or corrupt cache. *)
val create :
  dir:Eio.Fs.dir_ty Eio.Path.t -> client:Fetch.plain ->
  random:Eio.Flow.source_ty Eio.Resource.t -> t
(** [create ~dir ~client ~random] creates the cache directory and initializes the
    per-object fiber locks. [random] supplies secure random bytes for data
    generation IDs, normally [Eio.Stdenv.secure_random env]. Existing
    manifests and data are reused. *)
val ensure :
  t -> url:string -> head_only:bool -> selection ->
  (entry * Coverage.t) option
(** [ensure t ~url ~head_only selection] ensures the selected bytes are on
    disk. Only missing intervals are fetched. HEAD persists object metadata
    without fetching data. Missing upstream objects return [None]. Failed or
    cancelled transfers never publish their coverage. Upstream 404 and 410
    responses are also cached permanently. Data is synced before
    an atomic manifest replacement. *)
val cached : t -> url:string -> entry option
(** [cached t ~url] loads an existing entry without network traffic. *)
val size : entry -> int
(** [size entry] is the full object length, including uncovered bytes. *)
val content_type : entry -> string
(** [content_type entry] is the upstream media type. *)
val etag : entry -> string option
(** [etag entry] is the upstream entity tag. *)
val modified : entry -> string option
(** [modified entry] is the upstream Last-Modified field. *)
val covered : entry -> Coverage.t -> bool
(** [covered entry range] says whether every requested byte is cached. *)
val revision : entry -> string
(** [revision entry] identifies the local data generation. A changed upstream
    object gets a different generation without overwriting active readers. *)
val complete : entry -> bool
(** [complete entry] says whether the entire object is cached. Sparse holes
    never count as stored data. *)
val read : entry -> Coverage.t -> string
(** [read entry range] reads covered bytes into memory. *)
val write : entry -> Coverage.t -> Proffer.Body.Sink.t -> unit
(** [write entry range sink] streams covered bytes to [sink] with bounded
    buffers. Empty ranges do not open a data file. *)
val resolve : int -> selection -> Coverage.t
(** [resolve size selection] clamps the selection to an object of [size]
    bytes. Invalid selections raise [Invalid_argument]. A selection past
    the object raises [Unsatisfiable size]. *)
